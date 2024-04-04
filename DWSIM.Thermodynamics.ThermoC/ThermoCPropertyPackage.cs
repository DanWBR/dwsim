using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;
using System.IO;
using DWSIM.Interfaces;
using System.Threading.Tasks;
using DWSIM.Thermodynamics.PropertyPackages;
using System.Runtime.InteropServices;
using s = DWSIM.UI.Shared.Common;
using System.Diagnostics;
using DWSIM.Interfaces.Enums;
using DWSIM.ExtensionMethods.Eto;

namespace DWSIM.Thermodynamics.ThermoC
{

    public enum ThermoProperty
    {
        FugacityCoeff,
        CompressibilityCoeff,
        Density,
        Enthalpy,
        Entropy,
        HeatCapacityCp,
        HeatCapacityCv,
    }

    public class ThermoCPropertyPackage : PropertyPackages.PropertyPackage
    {

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        static extern bool SetDllDirectory(string lpPathName);

        libthermoc thermoc = null;

        PropertyPackages.Auxiliary.LeeKesler lk = new PropertyPackages.Auxiliary.LeeKesler();

        public override bool Popular => true;

        private string model = "vdW";

        public string Model
        {
            get
            {
                return model;
            }

            set
            {
                model = value;
            }
        }

        private string mixrule = "1F";

        public string MixRule
        {
            get
            {
                return mixrule;
            }

            set
            {
                mixrule = value;
            }
        }

        private bool useLeeKeslerEnthalpy = true;

        public bool UseLeeKeslerEnthalpy
        {
            get
            {
                return useLeeKeslerEnthalpy;
            }

            set
            {
                useLeeKeslerEnthalpy = value;
            }
        }

        private Model1 singlemodel;
        private ModelN mixturemodel;

        private string comphash = "";

        public ThermoCPropertyPackage()
            : base()
        {
            _packagetype = PackageType.EOS;

            //check if the correct platform files are in place
            bool is64 = true;

            var platfile = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),"ThermoCS","platform.txt");

            if (File.Exists(platfile)) if (int.Parse(File.ReadAllText(platfile)) == 32) is64 = false;

            if (Environment.Is64BitProcess == false && is64 == true)
            {
                throw new Exception("Found wrong library versions for the currently executing platform. Copy the correct files form 'ThermoCS/32-bit' or 'ThermoCS/64-bit' back to 'ThermoCS/' folder and try again.");
            }
            else if (Environment.Is64BitProcess == true && is64 == false)
            {
                throw new Exception("Found wrong library versions for the currently executing platform. Copy the correct files form 'ThermoCS/32-bit' or 'ThermoCS/64-bit' back to 'ThermoCS/' folder and try again.");
            }
            
            Init();
        }

        private void Init()
        {
            if (ThermoCS.PlatformCheck.RunningPlatform() == ThermoCS.PlatformCheck.Platform.Mac)
            {
                Environment.CurrentDirectory = Directory.GetParent(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)).Parent.FullName;
            }
            else if (ThermoCS.PlatformCheck.RunningPlatform() == ThermoCS.PlatformCheck.Platform.Linux)
            {
                Environment.CurrentDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
            }

            ComponentName = "ThermoC Bridge";
            ComponentDescription = "Bridge Library for the ThermoC software package";
            IsConfigurable = true;

        }

        public override void DisplayGroupedEditingForm()
        {
            DisplayEditingForm();
        }

        public override object ReturnInstance(string typename)
        {
            Type t = Type.GetType(typename, false);
            return Activator.CreateInstance(t);
        }

        public override void DisplayEditingForm()
        {
            var f = GetForm();
            f.Show();
            f.Center();
        }

        public Eto.Forms.Form GetForm()
        {

            if (GlobalSettings.Settings.CAPEOPENMode) new Eto.Forms.Application(Eto.Platforms.WinForms).Attach();

            var container = s.GetDefaultContainer();
            container.Tag = "Settings";

            var models = ThermoCS.Helpers.GetModelNames();

            Eto.Forms.DropDown modelselector = null;
            Eto.Forms.DropDown mixruleselector = null;

            Eto.Forms.TextArea modelfiles = null;
            Eto.Forms.TextArea mixrulefiles = null;

            var mixrules = ThermoCS.Helpers.GetModelSupportedMixingRules(Model);

            bool adding = false;

            s.CreateAndAddLabelRow(container, "Parameters");

            s.CreateAndAddCheckBoxRow(container, "Use Lee-Kesler model for caloric properties", UseLeeKeslerEnthalpy, (sender, e) => UseLeeKeslerEnthalpy = sender.Checked.GetValueOrDefault());

            s.CreateAndAddDescriptionRow(container, "Enables or disables usage of the Lee-Kesler model to calculate Enthalpy, Entropy and Heat Capacities.");

            s.CreateAndAddLabelRow(container, "Model Selection");

            modelselector = s.CreateAndAddDropDownRow(container, "Model", models, models.IndexOf(Model), (sender, e) =>
            {
                comphash = "";
                Model = models[sender.SelectedIndex];
                mixrules = ThermoCS.Helpers.GetModelSupportedMixingRules(Model);
                mixrules.Insert(0, "");
                adding = true;
                mixruleselector.Items.Clear();
                mixruleselector.Items.AddRange(mixrules.Select(x => new Eto.Forms.ListItem() { Key = x, Text = x }));
                if (mixrules.Contains(MixRule))
                {
                    mixruleselector.SelectedKey = MixRule;
                }
                else
                {
                    mixruleselector.SelectedIndex = 0;
                }
                adding = false;
                modelfiles.Text = String.Join("\n", ThermoCS.Helpers.GetModelSupportedCompounds(Model));
            });

            s.CreateAndAddLabelRow2(container, "Supported Compounds");

            modelfiles = s.CreateAndAddMultilineTextBoxRow(container, String.Join("\n", ThermoCS.Helpers.GetModelSupportedCompounds(Model)), true, false, null);
            modelfiles.Height = 200;

            s.CreateAndAddLabelAndButtonRow(container, "Model Description", "View", null, (sender, e) =>
            {
                var fwv = s.GetDefaultEditorForm("ThermoC: " + Model + " EOS", 800, 600, new Eto.Forms.WebView { Url = new Uri(Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "ThermoCS", "html", "eos", Model + ".html"), UriKind.Absolute) }, true);
                fwv.Show();
            });

            s.CreateAndAddLabelRow(container, "Mixing Rule Selection");

            mixruleselector = s.CreateAndAddDropDownRow(container, "Mixing Rule", mixrules, mixrules.IndexOf(MixRule), (sender, e) =>
            {
                comphash = "";
                if (!adding)
                {
                    var mxrl = ThermoCS.Helpers.GetModelSupportedMixingRules(Model);
                    mxrl.Insert(0, "");
                    MixRule = mxrl[sender.SelectedIndex];
                    mixrulefiles.Text = String.Join("\n", ThermoCS.Helpers.GetMixRuleParameterFiles(Model, MixRule));
                }
            });

            s.CreateAndAddLabelRow2(container, "Available Binary Parameter Files");

            mixrulefiles = s.CreateAndAddMultilineTextBoxRow(container, "", true, false, null);
            mixrulefiles.Text = String.Join("\n", ThermoCS.Helpers.GetMixRuleParameterFiles(Model, MixRule));
            mixrulefiles.Height = 200;

            var container2 = s.GetDefaultContainer();
            container2.Tag = "About";

            s.CreateAndAddLabelRow(container2, "About the ThermoC Bridge");
            s.CreateAndAddLabelRow2(container2, "The ThermoC Bridge is a software which connects to the ThermoC software package and exposes its models to DWSIM as a Property Package.");
            s.CreateAndAddLabelRow2(container2, "ThermoC Bridge is Copyright (c) 2017-2020 Daniel Wagner.");
            s.CreateAndAddLabelRow2(container2, "ThermoC Software Package is Copyright (c) Ulrich K. Deiters.");
            s.CreateAndAddLabelRow(container2, "Literature");
            s.CreateAndAddLabelRow2(container2, "1. U. K. Deiters, “A modular program for the calculation of thermodynamic properties of fluids”, Chem. Eng. Technol. 23 (2000) 581–584.\n2. U. K. Deiters and Th. Kraska, High-Pressure Fluid Phase Equilibria—Phenomenology and Computation, Elsevier, Amsterdam 2012.");
            s.CreateAndAddLabelRow(container2, "Contact Information");
            s.CreateAndAddLabelRow2(container2, "Prof. Dr. Ulrich K. Deiters\nInstitute of Physical Chemistry, University of Cologne\nLuxemburger Str. 116, D-50939 Köln\nTel. +49 (0)221 470-4543, Fax +49 (0)221 470-4900");
            s.CreateAndAddLabelAndButtonRow(container2, "Send e-mail to Prof. Ulrich", "ulrich.deiters@uni-koeln.de", null, (sender, e) => Process.Start("mailto:ulrich.deiters@uni-koeln.de"));
            s.CreateAndAddLabelAndButtonRow(container2, "Send e-mail to Daniel Wagner", "dwsim@inforside.com.br", null, (sender, e) => Process.Start("mailto:dwsim@inforside.com.br"));
            s.CreateAndAddButtonRow(container2, "Visit the ThermoC website for more information", null, (sender, e) => Process.Start("http://thermoc.uni-koeln.de/index2.html"));
            s.CreateAndAddButtonRow(container2, "View ThermoC README file", null, (sender, e) =>
            {
                var file = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "ThermoCS", "README.txt");
                if (ThermoCS.PlatformCheck.RunningPlatform() == ThermoCS.PlatformCheck.Platform.Windows)
                {
                    Process.Start(file);
                }
                else if (ThermoCS.PlatformCheck.RunningPlatform() == ThermoCS.PlatformCheck.Platform.Linux)
                {
                    Process.Start("xdg-open", file);
                }
                else
                {
                    Process.Start("open", file);
                }
            });

            var container3 = s.GetDefaultContainer();
            container3.Tag = "Utilities";

            s.CreateAndAddLabelRow(container3, "ThermoC Programs");
            s.CreateAndAddLabelRow2(container3, "These are the ThermoC main programs, which you can use to perform additional calculations and regress model parameters.");

            var applist = new Dictionary<string, string>();

            applist.Add("charact1", "Brown's characteristic curves (Joule inversion, Boyle, Joule-Thomson inversion curves)");
            applist.Add("check1", "Consistency test for user-supplied EOS modules");
            applist.Add("checkN", "Consistency test for mixture modules");
            applist.Add("crit2", "Critical curves of binary mixtures");
            applist.Add("difflimit1", "Binary diffusion coefficient at zero concentration");
            applist.Add("expandN", "Adiabatic expansion curves");
            applist.Add("ffe1", "Vapour pressure curve of a pure fluid");
            applist.Add("ffe2", "VLE, LLE for binary mixtures (obsolete)");
            applist.Add("ffeN", "VLE, LLE for multi-component mixtures");
            applist.Add("mixN", "Temperature and volume change upon isenthalpic-isobaric or isenthalpic-isentropic mixing of pure fluids");
            applist.Add("phase2", "VLE, LLE, SLE, SGE of binary mixtures (combines ffe2 and sfe2, but uses another search algorithm)");
            applist.Add("reduc1", "Calculation of pure-fluid EOS parameters from exp. data");
            applist.Add("reduc2", "EOS cross parameter estimation for binary mixtures");
            applist.Add("reduc3", "EOS pure-fluid parameter estimation from mixture data");
            applist.Add("rsfe1", "Dehydration of a solid compound (and analogous reactions)");
            applist.Add("sfe1", "Sublimation pressure curve of a pure fluid");
            applist.Add("sfe2", "SLE, SGE for binary mixtures");
            applist.Add("sle1", "Melting pressure curve of a pure compound");
            applist.Add("spinodal1", "Spinodal curve of a pure compound");
            applist.Add("spinodal2", "Spinodal curves of binary mixtures");
            applist.Add("surf1", "Surface tension (from viscosity)");
            applist.Add("transit2", "Solid/fluid flash of binary mixture; transitiometer simulation");
            applist.Add("virN", "Virial coefficients of pure fluids or mixtures");
            applist.Add("visco1", "Viscosity of pure fluids (friction theory)");
            applist.Add("viscofit1", "Fitting of friction theory parameters");
            applist.Add("xthN", "Single phase properties of fluids (including excess properties)");
            applist.Add("xth1s", "Thermodynamic properties of a single solid phase");
            applist.Add("xth1id", "Single phase properties of impure solids");

            foreach (var item in applist)
            {
                s.CreateAndAddBoldLabelAndButtonRow(container3, item.Key, "Open", null, (sender, e) =>
                {
                    var startInfo = new ProcessStartInfo();
                    switch (ThermoCS.PlatformCheck.RunningPlatform())
                    {
                        case ThermoCS.PlatformCheck.Platform.Windows:
                            startInfo.WorkingDirectory = Environment.CurrentDirectory;
                            startInfo.FileName = Environment.CurrentDirectory + "\\ThermoCS\\" + item.Key + ".exe";
                            if (item.Key.Contains("1"))
                            {
                                startInfo.Arguments = Model;
                            }
                            else
                            {
                                startInfo.Arguments = Model + " " + MixRule;
                            }
                            Eto.Forms.MessageBox.Show("Model: " + Model + "\n" + "Mixing Rule: " + MixRule, "Starting utility '" + item.Key + "'...", Eto.Forms.MessageBoxButtons.OK, Eto.Forms.MessageBoxType.Information);
                            break;
                        case ThermoCS.PlatformCheck.Platform.Linux:
                            var currdir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
                            var ldc = "LD_LIBRARY_PATH=" + currdir + "/ThermoCS/; export LD_LIBRARY_PATH";
                            var scriptl = new StringBuilder();
                            scriptl.AppendLine("#!/bin/bash");
                            scriptl.AppendLine("cd '" + currdir + "'");
                            scriptl.AppendLine(ldc);
                            //scriptl.AppendLine("chmod +x ThermoCS/" + item.Key);
                            if (item.Key.Contains("1"))
                            {
                                scriptl.AppendLine("echo 'model: " + Model + "'");
                                scriptl.AppendLine("./ThermoCS/" + item.Key + " " + Model);
                            }
                            else
                            {
                                scriptl.AppendLine("echo 'model: " + Model + "'");
                                scriptl.AppendLine("echo 'mixing rule: " + MixRule + "'");
                                scriptl.AppendLine("./ThermoCS/" + item.Key + " " + Model + " " + MixRule);
                            }
                            var filepathl = Path.GetTempFileName();
                            File.WriteAllText(filepathl, scriptl.ToString());
                            Process.Start("/bin/bash", "-c \" chmod +x " + filepathl + " \"");
                            startInfo.WindowStyle = ProcessWindowStyle.Normal;
                            startInfo.FileName = "xterm";
                            startInfo.Arguments = "-e '" + filepathl + "'";
                            break;
                        case ThermoCS.PlatformCheck.Platform.Mac:
                            var basedir = Directory.GetParent(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)).Parent.FullName;
                            var ldcosx = "export DYLD_LIBRARY_PATH=" + basedir + "/Contents/MonoBundle/ThermoCS/";
                            var script = new StringBuilder();
                            script.AppendLine("#!/bin/bash");
                            script.AppendLine("cd '" + basedir + "'");
                            script.AppendLine(ldcosx);
                            script.AppendLine("chmod +x Contents/MonoBundle/ThermoCS/" + item.Key);
                            if (item.Key.Contains("1"))
                            {
                                script.AppendLine("echo 'model: " + Model + "'");
                                script.AppendLine("./Contents/MonoBundle/ThermoCS/" + item.Key + " " + Model);
                            }
                            else
                            {
                                script.AppendLine("echo 'model: " + Model + "'");
                                script.AppendLine("echo 'mixing rule: " + MixRule + "'");
                                script.AppendLine("./Contents/MonoBundle/ThermoCS/" + item.Key + " " + Model + " " + MixRule);
                            }
                            var filepath = Path.GetTempFileName();
                            File.WriteAllText(filepath, script.ToString());
                            Process.Start("/bin/bash", "-c \" chmod +x " + filepath + " \"");
                            startInfo.WindowStyle = ProcessWindowStyle.Normal;
                            startInfo.FileName = "open";
                            startInfo.Arguments = "-a Terminal.app " + filepath;
                            break;
                    }
                    Process proc = Process.Start(startInfo);
                });
                s.CreateAndAddLabelRow2(container3, item.Value);
            }

            var sf = GlobalSettings.Settings.DpiScale;

            var f = s.GetDefaultTabbedForm("Edit ThermoC Property Package", (int)(500 * sf), (int)(650 * sf), 
                new Eto.Forms.DynamicLayout[] { container, container3, container2 });
            if (GlobalSettings.Settings.CAPEOPENMode) f.Topmost = true;
            f.SetFontAndPadding();
            return f;
            
        }

        [System.Runtime.ExceptionServices.HandleProcessCorruptedStateExceptionsAttribute]
        public Object CalculateProperty(ThermoProperty prop, double[] vx, string state, Double param1, Double param2, Double param3, Double param4)
        {

            if (GlobalSettings.Settings.ExcelMode || GlobalSettings.Settings.CAPEOPENMode) Environment.CurrentDirectory = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location));

            if (vx.Sum() == 0.0)
            {
                switch (prop)
                {
                    case ThermoProperty.FugacityCoeff:
                        return RET_NullVector();
                    default:
                        return 0.0;
                }
            }

            if (ThermoCS.PlatformCheck.RunningPlatform() == ThermoCS.PlatformCheck.Platform.Windows) SetDllDirectory(Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "ThermoCS"));

            // PARAM order: T, P, H, S

            double T = param1;
            double P = param2 * 1E-6;
            double H = param3;
            double S = param4;

            Calculator.CheckParallelPInvoke();

            int n = vx.Length;
            int i, j;

            try
            {
                if (thermoc == null) thermoc = new libthermoc();
            }
            catch (Exception ex)
            {
                Console.WriteLine("Error initializing ThermoC: " + ex.ToString());
                throw ex;
            }

            var cnames = RET_VNAMES();
            var compounds = cnames.Select(x => x.ToLower()).ToArray();

            for (i = 0; i < n; i++)
            {
                if (compounds[i].StartsWith("n-")) compounds[i] = compounds[i].Remove(0, 2);
            }

            var newcomphash = String.Join(",", compounds);

            var supported = ThermoCS.Helpers.GetModelSupportedCompounds(Model);

            foreach (var comp in compounds)
            {
                if (!supported.Contains(comp)) throw new ArgumentException("ThermoC error: unsupported compound - " + comp);
            }

            doubleV molfracs = null;

            try
            {
                molfracs = new doubleV(n);
            }
            catch (Exception ex)
            {
                Console.WriteLine("ThermoC error: " + ex.ToString());
                throw ex;
            }

            for (i = 0; i < n; i++)
            {
                molfracs.setItemValue(i, vx[i]);
            }

            phase_preference ph = phase_preference.th_STABLE;

            switch (state)
            {
                case "V":
                    ph = phase_preference.th_GAS;
                    break;
                case "L":
                    ph = phase_preference.th_LIQUID;
                    break;
                default:
                    ph = phase_preference.th_STABLE;
                    break;
            }

            if (n == 1)
            {

                //single compound

                if (comphash != newcomphash || singlemodel == null)
                {

                    comphash = newcomphash;

                    singlemodel = libthermoc.attach1(Model);

                    singlemodel.name(compounds[0]);
                    singlemodel.id(1);
                    singlemodel.load(Model);

                }

                double dens = 0.0, dens2 = 0.0;

                Console.WriteLine("Calculating single-compound density ('vcalc' call on libthermoc native library)");

                var ndens = singlemodel.vcalc(P, T, (int)ph, ref dens, ref dens2);

                Console.WriteLine(String.Format("Calculated mixture density (1): {0}", dens));

                if (ndens == -1 || ndens < -1.0) throw new Exception("ThermoC/" + Model + "/" + MixRule + " error: could not calculate density at the given conditions.");

                double value = 0;

                switch (prop)
                {
                    case ThermoProperty.CompressibilityCoeff:
                        Console.WriteLine("Calculating single-compound compressibility factor ('Z' call on libthermoc native library)");
                        value = singlemodel.Z(dens, T);
                        if (value + 1E20 <= double.Epsilon) throw new Exception("ThermoC/" + Model + "/" + MixRule + " error: could not calculate compressibility factor at the given conditions.");
                        return value;
                    case ThermoProperty.Density:
                        return dens * 1E3 * AUX_MMM(vx);
                    case ThermoProperty.Enthalpy:
                        Console.WriteLine("Calculating single-compound enthalpy ('Hr' call on libthermoc native library)");
                        value = singlemodel.Hr(dens, T);
                        if (value + 1E20 <= double.Epsilon) throw new Exception("ThermoC/" + Model + "/" + MixRule + " error: could not calculate enthalpy at the given conditions.");
                        return value * 8.314 * T / AUX_MMM(vx) + RET_Hid(298.15, T, vx);
                    case ThermoProperty.Entropy:
                        Console.WriteLine("Calculating single-compound entropy ('Sr' call on libthermoc native library)");
                        value = singlemodel.Sr(dens, T);
                        if (value + 1E20 <= double.Epsilon) throw new Exception("ThermoC/" + Model + "/" + MixRule + " error: could not calculate entropy at the given conditions.");
                        return value * 8.314 / AUX_MMM(vx) + RET_Sid(298.15, T, param2, vx);
                    case ThermoProperty.FugacityCoeff:
                        return new double[] { 1.0d };
                    case ThermoProperty.HeatCapacityCp:
                        Console.WriteLine("Calculating single-compound heat capacity Cp ('Cpr' call on libthermoc native library)");
                        value = singlemodel.Cpr(dens, T);
                        if (value + 1E20 <= double.Epsilon) throw new Exception("ThermoC/" + Model + "/" + MixRule + " error: could not calculate heat capacity at the given conditions.");
                        return value * 8.314 / AUX_MMM(vx) + AUX_CPi(RET_VNAMES()[0], T);
                    case ThermoProperty.HeatCapacityCv:
                        Console.WriteLine("Calculating single-compound heat capacity Cv ('Cvr' call on libthermoc native library)");
                        value = singlemodel.CVr(dens, T);
                        if (value + 1E20 <= double.Epsilon) throw new Exception("ThermoC/" + Model + "/" + MixRule + " error: could not calculate heat capacity at the given conditions.");
                        return value * 8.314 / AUX_MMM(vx) + AUX_CPi(RET_VNAMES()[0], T);
                    default:
                        return 0.0;
                }

            }
            else
            {

                //mixture

                if (comphash != newcomphash || mixturemodel == null)
                {

                    comphash = newcomphash;

                    var id = ThermoCS.Helpers.GetCompoundParameterFileIndexes(Model, ThermoCS.Helpers.GetModelSupportedCompounds(Model)[0])[0];

                    try
                    {
                        mixturemodel = libthermoc.attachN(Model, MixRule);
                        Console.WriteLine("[ThermoC] Mixture model attached successfully (Model: " + Model + " / Mixing rule: " + MixRule + ")");
                    }
                    catch (Exception ex)
                    {
                        Console.WriteLine("ThermoC error: " + ex.ToString());
                        throw ex;
                    }

                    mixturemodel.alloc(n, Model);

                    for (i = 0; i < n; i++)
                    {
                        var mdl1 = mixturemodel.getModel1i(i);
                        mdl1.name(compounds[i]);
                        mdl1.id(id);
                        mdl1.load(Model);
                    }

                    for (i = 0; i < n; i++)
                    {
                        for (j = 0; j < n; j++)
                        {
                            if (i != j)
                            {
                                if (ThermoCS.Helpers.IsMixRuleParameterFileAvailable(Model, MixRule, compounds[i], compounds[j]))
                                {
                                    try
                                    {
                                        mixturemodel.id(i, j, ThermoCS.Helpers.GetMixRuleParameterFileIndexes(Model, MixRule, compounds[i], compounds[j])[0]);
                                        mixturemodel.load(Model, MixRule);
                                    }
                                    catch (Exception ex)
                                    {
                                        if (Flowsheet != null) Flowsheet.ShowMessage("ThermoC error: could not read mixing rule parameter data for " + compounds[i] + "/" + compounds[j] + ": " + ex.Message, IFlowsheet.MessageType.GeneralError);
                                    }
                                }
                            }
                        }
                    }

                }

                double dens = 0.0, dens2 = 0.0;

                Console.WriteLine("Calculating mixture density ('vcalc' call on libthermoc native library)");

                var ndens = mixturemodel.vcalc(P, T, molfracs, (int)ph, ref dens, ref dens2);

                if (ndens == -1 || ndens < -1.0) throw new Exception("ThermoC/" + Model + "/" + MixRule + " error: could not calculate density at the given conditions.");

                if (ph == phase_preference.th_LIQUID && dens2 > dens) dens = dens2;
                if (ph == phase_preference.th_GAS && dens2 < dens && dens2 != 0.0) dens = dens2;

                Console.WriteLine(String.Format("Calculated mixture density: {0}", dens));

                double value = 0;

                switch (prop)
                {
                    case ThermoProperty.CompressibilityCoeff:
                        Console.WriteLine("Calculating mixture compressibility factor ('Z' call on libthermoc native library)");
                        value = mixturemodel.Z(dens, T, molfracs);
                        Console.WriteLine(String.Format("Calculated value: {0}", value));
                        if (value + 1E20 <= double.Epsilon) throw new Exception("ThermoC/" + Model + "/" + MixRule + " error: could not calculate compressibility factor at the given conditions.");
                        return value;
                    case ThermoProperty.Density:
                        return dens * 1E3 * AUX_MMM(vx);
                    case ThermoProperty.Enthalpy:
                        Console.WriteLine("Calculating mixture enthalpy ('Hr' call on libthermoc native library)");
                        value = mixturemodel.Hr(dens, T, molfracs);
                        Console.WriteLine(String.Format("Calculated value: {0}", value));
                        if (value + 1E20 <= double.Epsilon) throw new Exception("ThermoC/" + Model + "/" + MixRule + " error: could not calculate enthalpy at the given conditions.");
                        return value * 8.314 * T / AUX_MMM(vx) + RET_Hid(298.15, T, vx);
                    case ThermoProperty.Entropy:
                        Console.WriteLine("Calculating mixture entropy ('Sr' call on libthermoc native library)");
                        value = mixturemodel.Sr(dens, T, molfracs);
                        Console.WriteLine(String.Format("Calculated value: {0}", value));
                        if (value + 1E20 <= double.Epsilon) throw new Exception("ThermoC/" + Model + "/" + MixRule + " error: could not calculate entropy at the given conditions.");
                        return value * 8.314 / AUX_MMM(vx) + RET_Sid(298.15, T, param2, vx);
                    case ThermoProperty.FugacityCoeff:
                        var cp = new List<double>();
                        var phi = new List<double>();
                        Console.WriteLine("Calculating mixture compressibility factor ('Z' call on libthermoc native library)");
                        var Z = mixturemodel.Z(dens, T, molfracs);
                        Console.WriteLine(String.Format("Calculated value: {0}", Z));
                        Console.WriteLine("Calculating fugacity coefficients ('myr' calls on libthermoc native library)");
                        for (i = 0; i < n; i++)
                        {
                            cp.Add(mixturemodel.myr(dens, T, molfracs, i));
                            phi.Add(Math.Exp((cp[i]) - Math.Log(Z)));
                            Console.WriteLine(String.Format("Calculated value (compound {0}): {1}", i+1, phi[phi.Count -1]));
                        }
                        return phi.ToArray();
                    case ThermoProperty.HeatCapacityCp:
                        Console.WriteLine("Calculating mixture heat capacity Cp ('Cpr' call on libthermoc native library)");
                        value = mixturemodel.Cpr(dens, T, molfracs);
                        Console.WriteLine(String.Format("Calculated value: {0}", value));
                        if (value + 1E20 <= double.Epsilon) throw new Exception("ThermoC/" + Model + "/" + MixRule + " error: could not calculate heat capacity at the given conditions.");
                        double cpm = 0;
                        for (i = 0; i < n; i++)
                        {
                            cpm += vx[i] * AUX_CPi(cnames[i], T);
                        }
                        return value * 8.314 / AUX_MMM(vx) + cpm;
                    case ThermoProperty.HeatCapacityCv:
                        Console.WriteLine("Calculating mixture heat capacity Cv ('Cvr' call on libthermoc native library)");
                        value = mixturemodel.CVr(dens, T, molfracs);
                        Console.WriteLine(String.Format("Calculated value: {0}", value));
                        if (value + 1E20 <= double.Epsilon) throw new Exception("ThermoC/" + Model + "/" + MixRule + " error: could not calculate heat capacity at the given conditions.");
                        double cvm = 0;
                        for (i = 0; i < n; i++)
                        {
                            cvm += vx[i] * AUX_CPi(cnames[i], T);
                        }
                        return value * 8.314 / AUX_MMM(vx) + (cvm * AUX_MMM(vx) - 8.314) / AUX_MMM(vx);
                    default:
                        return 0.0;
                }

            }

        }

        public override bool MobileCompatible
        {
            get { return false; }
        }

        public override bool SupportsComponent(Interfaces.ICompoundConstantProperties comp)
        {
            return true;
        }

        #region legacy

        public override double DW_CalcEnergyFlowMistura_ISOL(double T, double P)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcCp_ISOL(PropertyPackages.Phase Phase1, double T, double P)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcCv_ISOL(PropertyPackages.Phase Phase1, double T, double P)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcK_ISOL(PropertyPackages.Phase Phase1, double T, double P)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcMM_ISOL(PropertyPackages.Phase Phase1, double T, double P)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcMassaEspecifica_ISOL(PropertyPackages.Phase Phase1, double T, double P, double Pvp = 0)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcPVAP_ISOL(double T)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcTensaoSuperficial_ISOL(PropertyPackages.Phase Phase1, double T, double P)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcViscosidadeDinamica_ISOL(PropertyPackages.Phase Phase1, double T, double P)
        {
            throw new NotImplementedException();
        }


        #endregion

        public double CalcCp(double[] Vx, double T, double P, State st)
        {
            if (UseLeeKeslerEnthalpy)
            {
                switch (st)
                {
                    case PropertyPackages.State.Liquid:
                        return ((double[])lk.CpCvR_LK("L", T, P, Vx, RET_VKij(), AUX_CONVERT_MOL_TO_MASS(Vx), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa()))[1];
                    case PropertyPackages.State.Vapor:
                        return ((double[])lk.CpCvR_LK("V", T, P, Vx, RET_VKij(), AUX_CONVERT_MOL_TO_MASS(Vx), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa()))[1];
                    default:
                        return 0.0d;
                }
            }
            else
            {
                switch (st)
                {
                    case PropertyPackages.State.Liquid:
                        return (double)CalculateProperty(ThermoProperty.HeatCapacityCp, Vx, "L", T, P, 0, 0);
                    case PropertyPackages.State.Vapor:
                        return (double)CalculateProperty(ThermoProperty.HeatCapacityCp, Vx, "V", T, P, 0, 0);
                    default:
                        return 0.0d;
                }
            }

        }

        public double CalcCv(double[] Vx, double T, double P, State st)
        {
            if (UseLeeKeslerEnthalpy)
            {
                switch (st)
                {
                    case PropertyPackages.State.Liquid:
                        return ((double[])lk.CpCvR_LK("L", T, P, Vx, RET_VKij(), AUX_CONVERT_MOL_TO_MASS(Vx), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa()))[2];
                    case PropertyPackages.State.Vapor:
                        return ((double[])lk.CpCvR_LK("V", T, P, Vx, RET_VKij(), AUX_CONVERT_MOL_TO_MASS(Vx), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa()))[2];
                    default:
                        return 0.0d;
                }

            }
            else
            {
                switch (st)
                {
                    case PropertyPackages.State.Liquid:
                        return (double)CalculateProperty(ThermoProperty.HeatCapacityCv, Vx, "L", T, P, 0, 0);
                    case PropertyPackages.State.Vapor:
                        return (double)CalculateProperty(ThermoProperty.HeatCapacityCv, Vx, "V", T, P, 0, 0);
                    default:
                        return 0.0d;
                }
            }

        }

        public override double AUX_VAPDENS(double T, double P)
        {
            return (double)CalculateProperty(ThermoProperty.Density, RET_VMOL(PropertyPackages.Phase.Vapor), "V", T, P, 0, 0);
        }

        public override double AUX_LIQDENS(double T, Array Vx, double P = 0, double Pvp = 0, bool FORCE_EOS = false)
        {
            return (double)CalculateProperty(ThermoProperty.Density, Vx.Cast<double>().ToArray(), "L", T, P, 0, 0);
        }

        public override double[] DW_CalcFugCoeff(Array Vx, double T, double P, PropertyPackages.State st)
        {
            double[] vz = Vx.Cast<double>().ToArray();
            switch (st)
            {
                case PropertyPackages.State.Liquid:
                    return (double[])CalculateProperty(ThermoProperty.FugacityCoeff, vz, "L", T, P, 0, 0);
                case PropertyPackages.State.Vapor:
                    return (double[])CalculateProperty(ThermoProperty.FugacityCoeff, vz, "V", T, P, 0, 0);
                default:
                    return RET_NullVector();
            }
        }

        public override double DW_CalcEnthalpy(Array Vx, double T, double P, PropertyPackages.State st)
        {
            double[] vz = Vx.Cast<double>().ToArray();
            if (UseLeeKeslerEnthalpy)
            {
                switch (st)
                {
                    case PropertyPackages.State.Liquid:
                        return lk.H_LK_MIX("L", T, P, vz, RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), RET_Hid(298.15, T, vz));
                    case PropertyPackages.State.Vapor:
                        return lk.H_LK_MIX("V", T, P, vz, RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), RET_Hid(298.15, T, vz));
                    case PropertyPackages.State.Solid:
                        return lk.H_LK_MIX("L", T, P, vz, RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), RET_Hid(298.15, T, vz)) - RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(vz), T);
                    default:
                        return 0d;
                }
            }
            else
            {
                switch (st)
                {
                    case PropertyPackages.State.Liquid:
                        return (double)CalculateProperty(ThermoProperty.Enthalpy, vz, "L", T, P, 0, 0);
                    case PropertyPackages.State.Vapor:
                        return (double)CalculateProperty(ThermoProperty.Enthalpy, vz, "V", T, P, 0, 0);
                    case PropertyPackages.State.Solid:
                        return (double)CalculateProperty(ThermoProperty.Enthalpy, vz, "L", T, P, 0, 0) - RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(vz), T);
                    default:
                        return 0d;
                }
            }
        }

        public override double DW_CalcEnthalpyDeparture(Array Vx, double T, double P, PropertyPackages.State st)
        {
            throw new NotImplementedException();
        }

        public override double DW_CalcEntropy(Array Vx, double T, double P, PropertyPackages.State st)
        {
            double[] vz = Vx.Cast<double>().ToArray();
            if (UseLeeKeslerEnthalpy)
            {
                switch (st)
                {
                    case PropertyPackages.State.Liquid:
                        return lk.S_LK_MIX("L", T, P, vz, RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), RET_Sid(298.15, T, P, vz));
                    case PropertyPackages.State.Vapor:
                        return lk.S_LK_MIX("V", T, P, vz, RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), RET_Sid(298.15, T, P, vz));
                    case PropertyPackages.State.Solid:
                        return lk.S_LK_MIX("L", T, P, vz, RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), RET_Sid(298.15, T, P, vz)) - RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(vz), T) / T;
                    default:
                        return 0d;
                }
            }
            else
            {
                switch (st)
                {
                    case PropertyPackages.State.Liquid:
                        return (double)CalculateProperty(ThermoProperty.Entropy, vz, "L", T, P, 0, 0);
                    case PropertyPackages.State.Vapor:
                        return (double)CalculateProperty(ThermoProperty.Entropy, vz, "V", T, P, 0, 0);
                    case PropertyPackages.State.Solid:
                        return (double)CalculateProperty(ThermoProperty.Entropy, vz, "L", T, P, 0, 0) - RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(vz), T) / T;
                    default:
                        return 0d;
                }
            }
        }

        public override double DW_CalcEntropyDeparture(Array Vx, double T, double P, PropertyPackages.State st)
        {
            throw new NotImplementedException();
        }

        public override void DW_CalcPhaseProps(PropertyPackages.Phase Phase)
        {
            double result = 0d;
            Phase dwpl = default(Phase);

            double T = 0d;
            double P = 0d;
            double phasemolarfrac = 0d;
            double overallmolarflow = 0d;

            int phaseID = 0;
            T = this.CurrentMaterialStream.Phases[0].Properties.temperature.GetValueOrDefault();
            P = this.CurrentMaterialStream.Phases[0].Properties.pressure.GetValueOrDefault();

            switch (Phase)
            {
                case PropertyPackages.Phase.Mixture:
                    phaseID = 0;
                    dwpl = PropertyPackages.Phase.Mixture;
                    break;
                case PropertyPackages.Phase.Vapor:
                    phaseID = 2;
                    dwpl = PropertyPackages.Phase.Vapor;
                    break;
                case PropertyPackages.Phase.Liquid1:
                    phaseID = 3;
                    dwpl = PropertyPackages.Phase.Liquid1;
                    break;
                case PropertyPackages.Phase.Liquid2:
                    phaseID = 4;
                    dwpl = PropertyPackages.Phase.Liquid2;
                    break;
                case PropertyPackages.Phase.Liquid3:
                    phaseID = 5;
                    dwpl = PropertyPackages.Phase.Liquid3;
                    break;
                case PropertyPackages.Phase.Liquid:
                    phaseID = 1;
                    dwpl = PropertyPackages.Phase.Liquid;
                    break;
                case PropertyPackages.Phase.Aqueous:
                    phaseID = 6;
                    dwpl = PropertyPackages.Phase.Aqueous;
                    break;
                case PropertyPackages.Phase.Solid:
                    phaseID = 7;
                    dwpl = PropertyPackages.Phase.Solid;
                    break;
            }

            if (phaseID > 0)
            {
                overallmolarflow = this.CurrentMaterialStream.Phases[0].Properties.molarflow.GetValueOrDefault();
                phasemolarfrac = this.CurrentMaterialStream.Phases[phaseID].Properties.molarfraction.GetValueOrDefault();
                result = overallmolarflow * phasemolarfrac;
                this.CurrentMaterialStream.Phases[phaseID].Properties.molarflow = result;
                result = result * this.AUX_MMM(Phase) / 1000;
                this.CurrentMaterialStream.Phases[phaseID].Properties.massflow = result;
                if (this.CurrentMaterialStream.Phases[0].Properties.massflow.GetValueOrDefault() > 0)
                {
                    result = phasemolarfrac * overallmolarflow * this.AUX_MMM(Phase) / 1000 / this.CurrentMaterialStream.Phases[0].Properties.massflow.GetValueOrDefault();
                }
                else
                {
                    result = 0;
                }
                this.CurrentMaterialStream.Phases[phaseID].Properties.massfraction = result;
                this.DW_CalcCompVolFlow(phaseID);
                this.DW_CalcCompFugCoeff(Phase);
            }


            if (phaseID == 3 | phaseID == 4 | phaseID == 5 | phaseID == 6)
            {

                result = this.AUX_LIQDENS(T, RET_VMOL(dwpl), P);
                this.CurrentMaterialStream.Phases[phaseID].Properties.density = result;
                this.CurrentMaterialStream.Phases[phaseID].Properties.compressibilityFactor = P / result * AUX_MMM(dwpl) / 1000 / 8.314 / T;

                this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy = this.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid);
                this.CurrentMaterialStream.Phases[phaseID].Properties.entropy = this.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Liquid);

                this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCp = this.CalcCp(RET_VMOL(dwpl), T, P, State.Liquid);
                this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCv = this.CalcCv(RET_VMOL(dwpl), T, P, State.Liquid);

                result = this.AUX_MMM(Phase);
                this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight = result;

                result = this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                this.CurrentMaterialStream.Phases[phaseID].Properties.molar_enthalpy = result;

                result = this.CurrentMaterialStream.Phases[phaseID].Properties.entropy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                this.CurrentMaterialStream.Phases[phaseID].Properties.molar_entropy = result;

                result = this.AUX_CONDTL(T);
                this.CurrentMaterialStream.Phases[phaseID].Properties.thermalConductivity = result;

                result = this.AUX_LIQVISCm(T,P);
                this.CurrentMaterialStream.Phases[phaseID].Properties.viscosity = result;
                this.CurrentMaterialStream.Phases[phaseID].Properties.kinematic_viscosity = result / this.CurrentMaterialStream.Phases[phaseID].Properties.density.Value;

            }
            else if (phaseID == 2)
            {


                result = this.AUX_VAPDENS(T, P);
                this.CurrentMaterialStream.Phases[phaseID].Properties.density = result;
                this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy = this.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor);
                this.CurrentMaterialStream.Phases[phaseID].Properties.entropy = this.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Vapor);
                this.CurrentMaterialStream.Phases[phaseID].Properties.compressibilityFactor = P / result * AUX_MMM(dwpl) / 1000 / 8.314 / T;
                this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCp = this.CalcCp(RET_VMOL(dwpl), T, P, State.Vapor);
                this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCv = this.CalcCv(RET_VMOL(dwpl), T, P, State.Vapor);
                result = this.AUX_MMM(Phase);
                this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight = result;
                result = this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                this.CurrentMaterialStream.Phases[phaseID].Properties.molar_enthalpy = result;
                result = this.CurrentMaterialStream.Phases[phaseID].Properties.entropy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                this.CurrentMaterialStream.Phases[phaseID].Properties.molar_entropy = result;
                result = this.AUX_CONDTG(T, P);
                this.CurrentMaterialStream.Phases[phaseID].Properties.thermalConductivity = result;
                result = this.AUX_VAPVISCm(T, this.CurrentMaterialStream.Phases[phaseID].Properties.density.GetValueOrDefault(), this.AUX_MMM(Phase));
                this.CurrentMaterialStream.Phases[phaseID].Properties.viscosity = result;
                this.CurrentMaterialStream.Phases[phaseID].Properties.kinematic_viscosity = result / this.CurrentMaterialStream.Phases[phaseID].Properties.density.Value;

            }
            else if (phaseID == 7)
            {
                result = this.AUX_SOLIDDENS();
                this.CurrentMaterialStream.Phases[phaseID].Properties.density = result;
                List<Interfaces.ICompoundConstantProperties> constprops = new List<Interfaces.ICompoundConstantProperties>();
                foreach (Interfaces.ICompound su in this.CurrentMaterialStream.Phases[0].Compounds.Values)
                {
                    constprops.Add(su.ConstantProperties);
                }
                this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy = this.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Solid);
                this.CurrentMaterialStream.Phases[phaseID].Properties.entropy = this.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Solid);
                this.CurrentMaterialStream.Phases[phaseID].Properties.compressibilityFactor = 0.0;
                //result
                result = this.DW_CalcSolidHeatCapacityCp(T, RET_VMOL(PropertyPackages.Phase.Solid), constprops);
                this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCp = result;
                this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCv = result;
                result = this.AUX_MMM(Phase);
                this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight = result;
                result = this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                this.CurrentMaterialStream.Phases[phaseID].Properties.molar_enthalpy = result;
                result = this.CurrentMaterialStream.Phases[phaseID].Properties.entropy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                this.CurrentMaterialStream.Phases[phaseID].Properties.molar_entropy = result;
                result = this.AUX_CONDTG(T, P);
                this.CurrentMaterialStream.Phases[phaseID].Properties.thermalConductivity = 0.0;
                //result
                this.CurrentMaterialStream.Phases[phaseID].Properties.viscosity = 1E+20;
                this.CurrentMaterialStream.Phases[phaseID].Properties.kinematic_viscosity = 1E+20;

            }
            else if (phaseID == 1)
            {
                DW_CalcLiqMixtureProps();


            }
            else
            {
                DW_CalcOverallProps();


            }

            if (phaseID > 0)
            {
                if (this.CurrentMaterialStream.Phases[phaseID].Properties.density.GetValueOrDefault() > 0 & overallmolarflow > 0)
                {
                    result = overallmolarflow * phasemolarfrac * this.AUX_MMM(Phase) / 1000 / this.CurrentMaterialStream.Phases[phaseID].Properties.density.GetValueOrDefault();
                }
                else
                {
                    result = 0;
                }
                this.CurrentMaterialStream.Phases[phaseID].Properties.volumetric_flow = result;
            }
        }

        public override void DW_CalcProp(string property, PropertyPackages.Phase phase)
        {

            double result = 0.0d;
            int phaseID = -1;
            string state = "";
            State pstate = default(State);

            double T = 0d;
            double P = 0d;
            T = this.CurrentMaterialStream.Phases[0].Properties.temperature.GetValueOrDefault();
            P = this.CurrentMaterialStream.Phases[0].Properties.pressure.GetValueOrDefault();

            switch (phase)
            {
                case Phase.Vapor:
                    state = "V";
                    pstate = PropertyPackages.State.Vapor;
                    break;
                case Phase.Liquid:
                case Phase.Liquid1:
                case Phase.Liquid2:
                case Phase.Liquid3:
                case Phase.Aqueous:
                    state = "L";
                    pstate = PropertyPackages.State.Liquid;
                    break;
                case Phase.Solid:
                    state = "S";
                    pstate = PropertyPackages.State.Solid;
                    break;
            }

            switch (phase)
            {
                case PropertyPackages.Phase.Mixture:
                    phaseID = 0;
                    break;
                case PropertyPackages.Phase.Vapor:
                    phaseID = 2;
                    break;
                case PropertyPackages.Phase.Liquid1:
                    phaseID = 3;
                    break;
                case PropertyPackages.Phase.Liquid2:
                    phaseID = 4;
                    break;
                case PropertyPackages.Phase.Liquid3:
                    phaseID = 5;
                    break;
                case PropertyPackages.Phase.Liquid:
                    phaseID = 1;
                    break;
                case PropertyPackages.Phase.Aqueous:
                    phaseID = 6;
                    break;
                case PropertyPackages.Phase.Solid:
                    phaseID = 7;
                    break;
            }

            this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight = this.AUX_MMM(phase);

            switch (property.ToLower())
            {
                case "compressibilityfactor":
                    result = (double)CalculateProperty(ThermoProperty.CompressibilityCoeff, RET_VMOL(phase), state, T, P, 0, 0);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.compressibilityFactor = result;
                    break;
                case "heatcapacity":
                case "heatcapacitycp":
                    if (state == "V")
                    {
                        result = this.CalcCp(RET_VMOL(phase), T, P, State.Vapor);
                    }
                    else
                    {
                        result = this.CalcCp(RET_VMOL(phase), T, P, State.Liquid);
                    }
                    this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCp = result;
                    break;
                case "heatcapacitycv":
                    if (state == "V")
                    {
                        result = this.CalcCv(RET_VMOL(phase), T, P, State.Vapor);
                    }
                    else
                    {
                        result = this.CalcCv(RET_VMOL(phase), T, P, State.Liquid);
                    }
                    this.CurrentMaterialStream.Phases[phaseID].Properties.heatCapacityCv = result;
                    break;
                case "enthalpy":
                case "enthalpynf":
                    result = this.DW_CalcEnthalpy(RET_VMOL(phase), T, P, pstate);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy = result;
                    result = this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                    this.CurrentMaterialStream.Phases[phaseID].Properties.molar_enthalpy = result;
                    break;
                case "entropy":
                case "entropynf":
                    result = this.DW_CalcEntropy(RET_VMOL(phase), T, P, pstate);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.entropy = result;
                    result = this.CurrentMaterialStream.Phases[phaseID].Properties.entropy.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                    this.CurrentMaterialStream.Phases[phaseID].Properties.molar_entropy = result;
                    break;
                case "excessenthalpy":
                    result = this.DW_CalcEnthalpyDeparture(RET_VMOL(phase), T, P, pstate);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.excessEnthalpy = result;
                    break;
                case "excessentropy":
                    result = this.DW_CalcEntropyDeparture(RET_VMOL(phase), T, P, pstate);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.excessEntropy = result;
                    break;
                case "enthalpyf":
                    double entF = this.AUX_HFm25(phase);
                    result = this.DW_CalcEnthalpy(RET_VMOL(phase), T, P, pstate);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpyF = result + entF;
                    result = this.CurrentMaterialStream.Phases[phaseID].Properties.enthalpyF.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                    this.CurrentMaterialStream.Phases[phaseID].Properties.molar_enthalpyF = result;
                    break;
                case "entropyf":
                    entF = this.AUX_SFm25(phase);
                    result = this.DW_CalcEntropy(RET_VMOL(phase), T, P, pstate);
                    this.CurrentMaterialStream.Phases[phaseID].Properties.entropyF = result + entF;
                    result = this.CurrentMaterialStream.Phases[phaseID].Properties.entropyF.GetValueOrDefault() * this.CurrentMaterialStream.Phases[phaseID].Properties.molecularWeight.GetValueOrDefault();
                    this.CurrentMaterialStream.Phases[phaseID].Properties.molar_entropyF = result;
                    break;
                case "viscosity":
                    if (state == "L")
                    {
                        result = this.AUX_LIQVISCm(T,P);
                    }
                    else
                    {
                        result = this.AUX_VAPVISCm(T, this.CurrentMaterialStream.Phases[phaseID].Properties.density.GetValueOrDefault(), this.AUX_MMM(phase));
                    }
                    this.CurrentMaterialStream.Phases[phaseID].Properties.viscosity = result;
                    break;
                case "thermalconductivity":
                    if (state == "L")
                    {
                        result = this.AUX_CONDTL(T);
                    }
                    else
                    {
                        result = this.AUX_CONDTG(T, P);
                    }
                    this.CurrentMaterialStream.Phases[phaseID].Properties.thermalConductivity = result;
                    break;
                case "fugacity":
                case "fugacitycoefficient":
                case "logfugacitycoefficient":
                case "activity":
                case "activitycoefficient":
                    this.DW_CalcCompFugCoeff(phase);
                    break;
                case "volume":
                case "density":
                    if (state == "L")
                    {
                        result = this.AUX_LIQDENS(T, P, 0.0, phaseID, false);
                    }
                    else
                    {
                        result = this.AUX_VAPDENS(T, P);
                    }
                    this.CurrentMaterialStream.Phases[phaseID].Properties.density = result;
                    break;
                case "surfacetension":
                    this.CurrentMaterialStream.Phases[0].Properties.surfaceTension = this.AUX_SURFTM(T);
                    break;
                default:
                    throw new CapeOpen.CapeThrmPropertyNotAvailableException();
            }

        }

        public override void DW_CalcCompPartialVolume(PropertyPackages.Phase phase, double T, double P)
        {
            //do nothing
        }

        public override bool LoadData(List<System.Xml.Linq.XElement> data)
        {
            base.LoadData(data);
            return XMLSerializer.XMLSerializer.Deserialize(this, data);
        }

        public override List<System.Xml.Linq.XElement> SaveData()
        {
            return XMLSerializer.XMLSerializer.Serialize(this);
        }

        public override double AUX_Z(double[] Vx, double T, double P, PhaseName state)
        {
            return 1.0f;
        }
    }
}
