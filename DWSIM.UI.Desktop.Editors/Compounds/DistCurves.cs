using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.Thermodynamics.BaseClasses;
using Eto.Drawing;
using Eto.Forms;
using s = DWSIM.UI.Shared.Common;

using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;
using DWSIM.Thermodynamics.Streams;
using DWSIM.UI.Desktop.Shared;

using DWSIM.ExtensionMethods;
using System.IO;
using DWSIM.Interfaces.Enums;
using DWSIM.UI.Desktop.Shared.Controls;
using DWSIM.Thermodynamics.Utilities.PetroleumCharacterization;
using DWSIM.Thermodynamics.Utilities.PetroleumCharacterization.Methods;
using DWSIM.MathOps.MathEx.Interpolation;
using DWSIM.Thermodynamics.PropertyPackages;

namespace DWSIM.UI.Desktop.Editors
{
    public class DistCurvePCharacterization : DynamicLayout
    {

        private class tmpcomp
        {
            public double tbpm;
            public double tbp0;
            public double tbpf;
            public double fv0;
            public double fvf;
            public double fvm;
        }

        List<tmpcomp> tccol = new List<tmpcomp>();

        public Flowsheet flowsheet;

        private String assayname;

        int ncomps = 10;
        string Tccorr = "Riazi-Daubert (1985)", Pccorr = "Riazi-Daubert (1985)", AFcorr = "Lee-Kesler (1976)", MWcorr = "Winn (1956)";

        List<double> cuttemps = new List<double>(), cb = new List<double>(), tbp = new List<double>(), mwc = new List<double>(), sgc = new List<double>(), visc100 = new List<double>(), visc210 = new List<double>();
        int pseudocuts = 0;
        double mwb, sgb;
        int tbpcurvetype = 0, curvebasis = 0, pseudomode = 0;
        bool hasmwc = false, hassgc = false, hasvisc100c = false, hasvisc210c = false;
        bool adjustAf = true, adjustZR = true;

        string decsep1, decsep2;

        public DistCurvePCharacterization(Flowsheet fs)
            : base()
        {
            flowsheet = fs;
            Init();
        }

        void Init()
        {

            Padding = new Padding(10);

            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;
            var nf = flowsheet.FlowsheetOptions.NumberFormat;

            assayname = "MyAssay" + new Random().Next(1, 100).ToString("000");

            s.CreateAndAddLabelRow(this, "Assay Information");
            s.CreateAndAddStringEditorRow(this, "Assay Name", assayname, (arg3, arg2) =>
            {
                assayname = arg3.Text;
            });
            s.CreateAndAddDescriptionRow(this, "Enter the name of the assay. It will be used to identify the Material Stream on the flowsheet and the associated compounds as well.");

            s.CreateAndAddLabelRow(this, "Property Methods");

            s.CreateAndAddDescriptionRow(this, "Select the methods to calculate compound properties.");

            s.CreateAndAddDropDownRow(this, "Molecular Weight", new List<string>() { "Winn (1956)", "Riazi (1986)", "Lee-Kesler (1974)" }, 0, (arg3, arg2) =>
            {
                MWcorr = arg3.SelectedValue.ToString();
            });
            s.CreateAndAddDropDownRow(this, "Critical Temperature", new List<string>() { "Riazi-Daubert (1985)", "Riazi (2005)", "Lee-Kesler (1976)", "Farah (2006)" }, 0, (arg3, arg2) =>
            {
                Tccorr = arg3.SelectedValue.ToString();
            });
            s.CreateAndAddDropDownRow(this, "Critical Pressure", new List<string>() { "Riazi-Daubert (1985)", "Lee-Kesler (1976)", "Farah (2006)" }, 0, (arg3, arg2) =>
            {
                Pccorr = arg3.SelectedValue.ToString();
            });
            s.CreateAndAddDropDownRow(this, "Acentric Factor", new List<string>() { "Lee-Kesler (1976)", "Korsten (2000)" }, 0, (arg3, arg2) =>
            {
                AFcorr = arg3.SelectedValue.ToString();
            });
            s.CreateAndAddCheckBoxRow(this, "Adjust Acentric Factors to match Normal Boiling Temperatures", adjustAf, (arg1, arg2) =>
            {
                adjustAf = arg1.Checked.GetValueOrDefault();
            }, null);
            s.CreateAndAddCheckBoxRow(this, "Adjust Rackett Parameters to match Specific Gravities", adjustZR, (arg1, arg2) =>
            {
                adjustZR = arg1.Checked.GetValueOrDefault();
            }, null);

            s.CreateAndAddLabelRow(this, "Setup Curves");

            string[] curvetypes = { "TBP, ASTM D2892", "ASTM D86", "ASTM D1160", "ASTM D2887" };

            var curvetypeselector = s.CreateAndAddDropDownRow(this, "Boiling Point Curve Type", curvetypes.ToList(), 0, (arg3, arg2) => { });

            var chk_mwcurve = s.CreateAndAddCheckBoxRow(this, "Molar Weight", false, (arg3, arg2) => { });

            var chk_sgcurve = s.CreateAndAddCheckBoxRow(this, "Specific Gravity", false, (arg3, arg2) => { });

            var chk_visc100 = s.CreateAndAddCheckBoxRow(this, "Kinematic Viscosity @ 100 F", false, (arg3, arg2) => { });

            var chk_visc210 = s.CreateAndAddCheckBoxRow(this, "Kinematic Viscosity @ 210 F", false, (arg3, arg2) => { });

            string[] curvebasistypes = { "Liquid Volume (%)", "Molar (%)", "Mass (%)" };

            var curvebasisselector = s.CreateAndAddDropDownRow(this, "Curve Basis", curvebasistypes.ToList(), 0, (arg3, arg2) => { });

            s.CreateAndAddLabelRow(this, "Bulk Sample Data");

            var sgtbox = s.CreateAndAddTextBoxRow(this, nf, "Specific Gravity", 0.0d, (arg3, arg2) => { });

            s.CreateAndAddDescriptionRow(this, "Leave it unchanged if not available.");

            var mwtbox = s.CreateAndAddTextBoxRow(this, nf, "Molar Weight", 0.0d, (arg3, arg2) => { });

            s.CreateAndAddDescriptionRow(this, "Leave it unchanged if not available.");

            s.CreateAndAddLabelRow(this, "Curve Data");

            s.CreateAndAddDescriptionRow(this, "Enter curve data in the field below, separating the column values with spaces. Values should be input without thousands separator. First column is the curve basis data, following columns should contain the data according the the curve selection above.");

            s.CreateAndAddDescriptionRow(this, "Current Temperature units: " + su.temperature);
            s.CreateAndAddDescriptionRow(this, "Current Kinematic Viscosity units: " + su.cinematic_viscosity);

            var decsepsel1 = s.CreateAndAddDropDownRow(this, "Decimal Separator", new[] { "Dot (.)", "Comma (,)" }.ToList(), 0, (arg3, arg2) => { });

            var txtcurvedata = s.CreateAndAddMultilineMonoSpaceTextBoxRow(this, "", 200, false, (arg3, arg2) => { });

            s.CreateAndAddLabelRow(this, "Pseudo Compounds");
            s.CreateAndAddDescriptionRow(this, "Select the method to be used for generation of pseudo compounds (petroleum fractions).");

            string[] pseudotypes = { "Defined Number", "Defined Cut Temperatures" };

            var pseudomodeselector = s.CreateAndAddDropDownRow(this, "Pseudo Cut Type", pseudotypes.ToList(), 0, (arg3, arg2) => { });

            s.CreateAndAddDescriptionRow(this, "Enter the number of pseudo compounds in the field below if you selected the first option in the selector above, or the cut temperatures in the current temperature units, separated by spaces and without thousands separator. Do not include the maximum and minimum temperatures on the list.");

            var decsepsel2 = s.CreateAndAddDropDownRow(this, "Decimal Separator", new[] { "Dot (.)", "Comma (,)" }.ToList(), 0, (arg3, arg2) => { });

            var txtpseudomodedata = s.CreateAndAddFullTextBoxRow(this, "10", (arg3, arg2) => { });

            s.CreateAndAddButtonRow(this, "Characterize Assay and Create Compounds", null, (arg3, arg2) =>
            {
                var comps = new Dictionary<string, Compound>();

                //get data

                tbpcurvetype = curvetypeselector.SelectedIndex;
                hasmwc = chk_mwcurve.Checked.GetValueOrDefault();
                hassgc = chk_sgcurve.Checked.GetValueOrDefault();
                hasvisc100c = chk_visc100.Checked.GetValueOrDefault();
                hasvisc210c = chk_visc210.Checked.GetValueOrDefault();
                curvebasis = curvebasisselector.SelectedIndex;
                Double.TryParse(sgtbox.Text, out sgb);
                Double.TryParse(mwtbox.Text, out mwb);
                pseudomode = pseudomodeselector.SelectedIndex;
                decsep1 = decsepsel1.SelectedIndex == 0 ? "." : ",";
                decsep2 = decsepsel2.SelectedIndex == 0 ? "." : ",";

                var datalines = txtcurvedata.Text.Split('\n');
                try
                {
                    ParseCurveData(datalines, su);
                }
                catch (Exception ex)
                {
                    flowsheet.ShowMessage("Error parsing curve data: " + ex.Message, IFlowsheet.MessageType.GeneralError);
                    return;
                }

                try
                {
                    if (pseudomode == 0)
                    {
                        pseudocuts = Int32.Parse(txtpseudomodedata.Text);
                    }
                    else
                    {
                        cuttemps = txtpseudomodedata.Text.Trim().Split(' ').Select(t => t.ToDoubleWithSeparator(decsep2)).ToList();
                    }
                }
                catch (Exception ex)
                {
                    flowsheet.ShowMessage("Error parsing pseudo compound cuts: " + ex.Message, IFlowsheet.MessageType.GeneralError);
                    return;
                }

                var dialog = ProgressDialog.Show(this, "Petroleum Characterization", "Generating compounds, please wait...", false);

                Task.Factory.StartNew(() =>
                {
                    comps = GenerateCompounds(su);
                }).ContinueWith((t) =>
                {
                    Application.Instance.Invoke(() => { dialog.Close(); });
                    if (t.Exception == null)
                    {
                        var api = 141.5 / sgb - 131.5;
                        if (sgb == 0.0) api = 0.0;
                        var assay = new DWSIM.SharedClasses.Utilities.PetroleumCharacterization.Assay.Assay(12.0, mwb, api, 310.928, 372.039, tbpcurvetype, "",
                            new System.Collections.ArrayList(cb), new System.Collections.ArrayList(tbp), new System.Collections.ArrayList(mwc),
                            new System.Collections.ArrayList(sgc), new System.Collections.ArrayList(visc100), new System.Collections.ArrayList(visc210));
                        var ms2 = new MaterialStream("", "");
                        ms2.SetFlowsheet(flowsheet);
                        if (flowsheet.PropertyPackages.Count > 0)
                        {
                            ms2.SetPropertyPackage(flowsheet.PropertyPackages.Values.First());
                        }
                        else
                        {
                            ms2.SetPropertyPackage(new Thermodynamics.PropertyPackages.PengRobinsonPropertyPackage());
                        }
                        foreach (var subst in comps.Values)
                        {
                            ms2.Phases[0].Compounds.Add(subst.Name, subst);
                            ms2.Phases[1].Compounds.Add(subst.Name, new Compound(subst.Name, "") { ConstantProperties = subst.ConstantProperties });
                            ms2.Phases[2].Compounds.Add(subst.Name, new Compound(subst.Name, "") { ConstantProperties = subst.ConstantProperties });
                            ms2.Phases[3].Compounds.Add(subst.Name, new Compound(subst.Name, "") { ConstantProperties = subst.ConstantProperties });
                            ms2.Phases[4].Compounds.Add(subst.Name, new Compound(subst.Name, "") { ConstantProperties = subst.ConstantProperties });
                            ms2.Phases[5].Compounds.Add(subst.Name, new Compound(subst.Name, "") { ConstantProperties = subst.ConstantProperties });
                            ms2.Phases[6].Compounds.Add(subst.Name, new Compound(subst.Name, "") { ConstantProperties = subst.ConstantProperties });
                            ms2.Phases[7].Compounds.Add(subst.Name, new Compound(subst.Name, "") { ConstantProperties = subst.ConstantProperties });
                        }
                        var qc = new Thermodynamics.QualityCheck(assay, ms2);
                        qc.DoQualityCheck();
                        Application.Instance.Invoke(() =>
                        {
                            qc.DisplayForm((c) =>
                            {
                                Application.Instance.Invoke(() =>
                                {
                                    var form = s.GetDefaultEditorForm("Compound Properties: " + c.Name, 800, 600, new CompoundViewer((Flowsheet)flowsheet, c), false);
                                    form.Show();
                                });
                            }, () =>
                            {
                                foreach (var comp in comps.Values)
                                {
                                    if (!flowsheet.AvailableCompounds.ContainsKey(comp.Name))
                                    {
                                        flowsheet.AvailableCompounds.Add(comp.Name, comp.ConstantProperties);
                                    }
                                    flowsheet.SelectedCompounds.Add(comp.Name, flowsheet.AvailableCompounds[comp.Name]);
                                    foreach (MaterialStream obj in flowsheet.SimulationObjects.Values.Where((x) => x.GraphicObject.ObjectType == ObjectType.MaterialStream))
                                    {
                                        foreach (var phase in obj.Phases.Values)
                                        {
                                            phase.Compounds.Add(comp.Name, new Thermodynamics.BaseClasses.Compound(comp.Name, ""));
                                            phase.Compounds[comp.Name].ConstantProperties = flowsheet.SelectedCompounds[comp.Name];
                                        }
                                    }
                                }
                                var ms = (MaterialStream)flowsheet.AddObject(ObjectType.MaterialStream, 100, 100, assayname);
                                double wtotal = comps.Values.Select((x) => x.MoleFraction.GetValueOrDefault() * x.ConstantProperties.Molar_Weight).Sum();
                                foreach (var c in ms.Phases[0].Compounds.Values)
                                {
                                    c.MassFraction = 0.0f;
                                    c.MoleFraction = 0.0f;
                                }
                                foreach (var c in comps.Values)
                                {
                                    c.MassFraction = c.MoleFraction.GetValueOrDefault() * c.ConstantProperties.Molar_Weight / wtotal;
                                    ms.Phases[0].Compounds[c.Name].MassFraction = c.MassFraction.GetValueOrDefault();
                                    ms.Phases[0].Compounds[c.Name].MoleFraction = c.MoleFraction.GetValueOrDefault();
                                }
                                Application.Instance.Invoke(() =>
                                {
                                    flowsheet.UpdateInterface();
                                    flowsheet.UpdateEditorPanels.Invoke();
                                    flowsheet.ShowMessage("Material Stream '" + assayname + "' added successfully. " + ncomps.ToString() + " compounds created.", IFlowsheet.MessageType.Information);

                                    if (MessageBox.Show("Do you want to export the created compounds to a XML database?", "Petroleum C7+ Characterization", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.Yes) == DialogResult.Yes)
                                    {
                                        try
                                        {
                                            var compstoexport = comps.Values.Select((x) => x.ConstantProperties).ToArray();
                                            var savedialog = new SaveFileDialog();
                                            savedialog.Title = "Save Compounds to XML Database";
                                            savedialog.Filters.Add(new FileFilter("XML File", new[] { ".xml" }));
                                            savedialog.CurrentFilterIndex = 0;
                                            if (savedialog.ShowDialog(this) == DialogResult.Ok)
                                            {
                                                try
                                                {
                                                    if (!File.Exists(savedialog.FileName))
                                                    {
                                                        File.WriteAllText(savedialog.FileName, "");
                                                        Thermodynamics.Databases.UserDB.CreateNew(savedialog.FileName, "compounds");
                                                    }
                                                    using (var stream = new FileStream(savedialog.FileName, FileMode.OpenOrCreate))
                                                    {
                                                        Thermodynamics.Databases.UserDB.AddCompounds(compstoexport, stream, true);
                                                    }
                                                    flowsheet.ShowMessage("Compounds successfully saved to XML file.", IFlowsheet.MessageType.Information);
                                                }
                                                catch (Exception ex)
                                                {
                                                    flowsheet.ShowMessage("Error saving compound to JSON file: " + ex.ToString(), IFlowsheet.MessageType.GeneralError);
                                                }
                                            }
                                        }
                                        catch (Exception ex)
                                        {
                                            flowsheet.ShowMessage("Error saving data: " + ex.ToString(), IFlowsheet.MessageType.GeneralError);
                                        }
                                    }
                                });
                            });
                        });
                    }
                    else
                    {
                        Application.Instance.Invoke(() =>
                        {
                            flowsheet.ShowMessage("Error saving data: " + t.Exception.GetBaseException().Message, IFlowsheet.MessageType.GeneralError);
                        });
                    }
                });
            });

        }

        Dictionary<string, Compound> GenerateCompounds(IUnitsOfMeasure su)
        {

            var id = new Random().Next(1000, 9999);

            //generate pseudos from number or temperature cuts

            int i = 0;
            int method = pseudomode;
            double[] tbp2 = null;
            double[] tbpx = null;

            var fittedt = new List<double>();
            double[] coeff;
            object[] obj = null;
            double Tmin, Tmax;

            //generate polynomial from input data

            if (tbpcurvetype == 0)
            {
                //tbp
                tbp2 = tbp.ToArray();
                tbpx = cb.ToArray();
            }
            else if (tbpcurvetype == 1)
            {
                //d86
                double T0 = 0;
                double T10 = 0;
                double T30 = 0;
                double T50 = 0;
                double T70 = 0;
                double T90 = 0;
                double T100 = 0;
                //interpolate to obtain points
                double[] w = null;
                ratinterpolation.buildfloaterhormannrationalinterpolant(cb.ToArray(), cb.Count, 1, ref w);
                T0 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0.0d);
                T10 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0.1d);
                T30 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0.3d);
                T50 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0.5d);
                T70 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0.7d);
                T90 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0.9d);
                T100 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 1.0d);
                //tbp
                tbp2 = DistillationCurveConversion.ASTMD86ToPEV_Riazi(new double[] { T0, T10, T30, T50, T70, T90, T100 });
                tbpx = new double[] { 1E-06, 0.1, 0.3, 0.5, 0.7, 0.9, 1.0 };
            }
            else if (tbpcurvetype == 2)
            {
                //vacuum
                double T0 = 0;
                double T10 = 0;
                double T30 = 0;
                double T50 = 0;
                double T70 = 0;
                double T90 = 0;
                double T100 = 0;
                //interpolate to obtain points
                double[] w = null;
                ratinterpolation.buildfloaterhormannrationalinterpolant(cb.ToArray(), cb.Count, 1, ref w);
                T0 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0);
                T10 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0.1);
                T30 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0.3);
                T50 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0.5);
                T70 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0.7);
                T90 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0.9);
                T100 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 1.0);
                //tbp
                tbp2 = DistillationCurveConversion.ASTMD1160ToPEVsub_Wauquier(new double[] { T0, T10, T30, T50, T70, T90, T100 });
                double K = 12.0;
                tbp2[0] = DistillationCurveConversion.PEVsubToPEV_MaxwellBonnel(tbp2[0], 1333, K);
                tbp2[1] = DistillationCurveConversion.PEVsubToPEV_MaxwellBonnel(tbp2[1], 1333, K);
                tbp2[2] = DistillationCurveConversion.PEVsubToPEV_MaxwellBonnel(tbp2[2], 1333, K);
                tbp2[3] = DistillationCurveConversion.PEVsubToPEV_MaxwellBonnel(tbp2[3], 1333, K);
                tbp2[4] = DistillationCurveConversion.PEVsubToPEV_MaxwellBonnel(tbp2[4], 1333, K);
                tbp2[5] = DistillationCurveConversion.PEVsubToPEV_MaxwellBonnel(tbp2[5], 1333, K);
                tbp2[6] = DistillationCurveConversion.PEVsubToPEV_MaxwellBonnel(tbp2[6], 1333, K);
                tbpx = new double[] { 1E-06, 0.1, 0.3, 0.5, 0.7, 0.9, 1.0 };
            }
            else if (tbpcurvetype == 3)
            {
                //simulated
                double T5 = 0;
                double T10 = 0;
                double T30 = 0;
                double T50 = 0;
                double T70 = 0;
                double T90 = 0;
                double T95 = 0;
                double T100 = 0;
                //interpolate to obtain points
                double[] w = null;
                ratinterpolation.buildfloaterhormannrationalinterpolant(cb.ToArray(), cb.Count, 1, ref w);
                T5 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0.05);
                T10 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0.1);
                T30 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0.3);
                T50 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0.5);
                T70 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0.7);
                T90 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0.9);
                T95 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 0.95);
                T100 = polinterpolation.barycentricinterpolation(cb.ToArray(), tbp.ToArray(), w, cb.Count, 1.0);
                //tbp
                tbp2 = DistillationCurveConversion.ASTMD2887ToPEV_Daubert(new double[] { T5, T10, T30, T50, T70, T90, T95, T100 });
                tbpx = new double[] { 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 1.0 };
            }

            Tmin = tbp2.Min();
            Tmax = tbp2.Max();

            //y = 10358x5 - 15934x4 + 11822x3 - 4720,2x2 + 1398,2x + 269,23
            //R² = 1

            double[] inest = new double[7];

            if (tbpcurvetype == 1)
            {
                double[] w2 = null;
                ratinterpolation.buildfloaterhormannrationalinterpolant(tbpx, tbpx.Length, 1, ref w2);
                inest[0] = polinterpolation.barycentricinterpolation(tbpx, tbp2, w2, tbpx.Length, 0);
            }
            else
            {
                inest[0] = Tmin;
            }
            inest[1] = 1398;
            inest[2] = 4720;
            inest[3] = 11821;
            inest[4] = 15933;
            inest[5] = 10358;
            inest[6] = -3000;

            DistillationCurveConversion.TBPFit lmfit = new DistillationCurveConversion.TBPFit();
            obj = (object[])lmfit.GetCoeffs(tbpx, tbp2, inest, 1E-10, 1E-08, 1E-08, 1000);
            coeff = (double[])obj[0];

            //TBP(K) = aa + bb*fv + cc*fv^2 + dd*fv^3 + ee*fv^4 + ff*fv^5 (fv 0 ~ 1)

            fittedt.Clear();
            for (i = 0; i <= tbp2.Length - 1; i++)
            {
                fittedt.Add(cv.ConvertFromSI(su.temperature, coeff[0] + coeff[1] * tbpx[i] + coeff[2] * Math.Pow(tbpx[i], 2) + coeff[3] * Math.Pow(tbpx[i], 3) + coeff[4] * Math.Pow(tbpx[i], 4) + coeff[5] * Math.Pow(tbpx[i], 5) + coeff[6] * Math.Pow(tbpx[i], 6)));
            }

            //create pseudos

            if (method == 0)
            {
                int np = Convert.ToInt32(pseudocuts);
                double deltaT = (Tmax - Tmin) / (np);
                double t0 = 0;
                double fv0 = 0;
                t0 = Tmin;
                fv0 = tbpx.Min();
                tccol.Clear();
                for (i = 0; i <= np - 1; i++)
                {
                    tmpcomp tc = new tmpcomp();
                    tc.tbp0 = t0;
                    tc.tbpf = t0 + deltaT;
                    tc.fv0 = GetFV(coeff, fv0, tc.tbp0);
                    tc.fvf = GetFV(coeff, fv0, tc.tbpf);
                    tc.fvm = tc.fv0 + (tc.fvf - tc.fv0) / 2;
                    tc.tbpm = GetT(coeff, tc.fvm);
                    tccol.Add(tc);
                    t0 = t0 + deltaT;
                    fv0 = tc.fvf;
                }
            }
            else
            {
                int np = cuttemps.Count + 1;
                double t0 = 0;
                double fv0 = 0;
                t0 = Tmin;
                fv0 = tbpx.Min();
                tccol.Clear();
                for (i = 0; i <= np - 1; i++)
                {
                    tmpcomp tc = new tmpcomp();
                    tc.tbp0 = t0;
                    if (i == np - 1)
                    {
                        tc.tbpf = Tmax;
                    }
                    else
                    {
                        tc.tbpf = cv.ConvertToSI(su.temperature, cuttemps[i]);
                    }
                    tc.fv0 = GetFV(coeff, fv0, tc.tbp0);
                    tc.fvf = GetFV(coeff, fv0, tc.tbpf);
                    tc.fvm = tc.fv0 + (tc.fvf - tc.fv0) / 2;
                    tc.tbpm = GetT(coeff, tc.fvm);
                    tccol.Add(tc);
                    fv0 = tc.fvf;
                    if (i < np - 1)
                        t0 = cv.ConvertToSI(su.temperature, cuttemps[i]);
                }
            }

            DWSIM.Thermodynamics.Utilities.PetroleumCharacterization.Methods.GL methods2 = new DWSIM.Thermodynamics.Utilities.PetroleumCharacterization.Methods.GL();

            Dictionary<string, Compound> ccol = new Dictionary<string, Compound>();

            i = 0;

            foreach (tmpcomp tc in tccol)
            {
                ConstantProperties cprops = new ConstantProperties();

                cprops.NBP = tc.tbpm;
                cprops.OriginalDB = "Petroleum Assay: " + assayname;
                cprops.CurrentDB = "Petroleum Assay: " + assayname;

                //SG
                if (!hassgc)
                {
                    if (Math.Abs(cprops.PF_MM.GetValueOrDefault()) < 1e-10)
                    {
                        if (cprops.NBP.GetValueOrDefault() < 1080)
                        {
                            cprops.PF_MM = Math.Pow((1.0 / 0.01964 * (6.97996 - Math.Log(1080.0 - cprops.NBP.GetValueOrDefault()))), 1.5);
                        }
                        else
                        {
                            cprops.PF_MM = Math.Pow((1.0 / 0.01964 * (6.97996 + Math.Log(-1080.0 + cprops.NBP.GetValueOrDefault()))), 1.5);
                        }
                    }
                    cprops.PF_SG = PropertyMethods.d15_Riazi(cprops.PF_MM.GetValueOrDefault());
                }
                else
                {
                    double[] w = null;
                    ratinterpolation.buildfloaterhormannrationalinterpolant(this.cb.ToArray(), cb.Count, 1, ref w);
                    cprops.PF_SG = polinterpolation.barycentricinterpolation(cb.ToArray(), sgc.ToArray(), w, cb.Count(), tc.fvm);
                }

                //MW
                if (!hasmwc)
                {
                    switch (MWcorr)
                    {
                        case "Winn (1956)":
                            cprops.PF_MM = PropertyMethods.MW_Winn(cprops.NBP.GetValueOrDefault(), cprops.PF_SG.GetValueOrDefault());
                            break;
                        case "Riazi (1986)":
                            cprops.PF_MM = PropertyMethods.MW_Riazi(cprops.NBP.GetValueOrDefault(), cprops.PF_SG.GetValueOrDefault());
                            break;
                        case "Lee-Kesler (1974)":
                            cprops.PF_MM = PropertyMethods.MW_LeeKesler(cprops.NBP.GetValueOrDefault(), cprops.PF_SG.GetValueOrDefault());
                            break;
                    }
                }
                else
                {
                    double[] w = null;
                    ratinterpolation.buildfloaterhormannrationalinterpolant(this.cb.ToArray(), cb.Count(), 1, ref w);
                    cprops.PF_MM = polinterpolation.barycentricinterpolation(this.cb.ToArray(), mwc.ToArray(), w, cb.Count(), tc.fvm);
                }

                cprops.Molar_Weight = cprops.PF_MM.GetValueOrDefault();

                char[] trimchars = new char[] { ' ', '_', ',', ';', ':' };

                if (Double.IsNaN(cprops.NBP.GetValueOrDefault()))
                {
                    cprops.Name = "C" + assayname.Trim(trimchars).ToString() + "_NBP_" + i.ToString();
                    cprops.CAS_Number = assayname.Trim(trimchars) + "-" + i.ToString();
                }
                else
                {
                    cprops.Name = "C" + assayname.Trim(trimchars).ToString() + "_NBP_" + Convert.ToInt32(cprops.NBP.GetValueOrDefault() - 273.15).ToString();
                    cprops.CAS_Number = assayname.Trim(trimchars) + "-" + Convert.ToInt32(cprops.NBP.GetValueOrDefault()).ToString();
                }

                i += 1;

                Compound subst = new Compound(cprops.Name, "");

                subst.ConstantProperties = cprops;
                subst.Name = cprops.Name;
                subst.PetroleumFraction = true;

                ccol.Add(cprops.Name, subst);

            }

            CalculateMolarFractions(ccol);

            if (mwb > 1E-10)
            {
                double mixtMW = 0;
                foreach (var c in ccol.Values)
                {
                    mixtMW += c.MoleFraction.GetValueOrDefault() * c.ConstantProperties.Molar_Weight;
                }
                double facm = mwb / mixtMW;
                foreach (var c in ccol.Values)
                {
                    c.ConstantProperties.Molar_Weight *= facm;
                }
            }

            if (sgb > 1E-10)
            {
                double mixtD = 0;
                foreach (var c in ccol.Values)
                {
                    mixtD += c.MassFraction.GetValueOrDefault() * c.ConstantProperties.PF_SG.GetValueOrDefault();
                }
                double facd = 141.5 / (131.5 + sgb) / mixtD;
                foreach (var c in ccol.Values)
                {
                    c.ConstantProperties.PF_SG *= facd;
                }
            }

            i = 0;

            foreach (var subst in ccol.Values)
            {
                ConstantProperties cprops = (ConstantProperties)subst.ConstantProperties;

                tmpcomp tc = tccol[i];

                //VISC
                if (!hasvisc100c)
                {
                    cprops.PF_Tv1 = 311;
                    cprops.PF_Tv2 = 372;
                    cprops.PF_v1 = PropertyMethods.Visc37_Abbott(cprops.NBP.GetValueOrDefault(), cprops.PF_SG.GetValueOrDefault());
                    cprops.PF_v2 = PropertyMethods.Visc98_Abbott(cprops.NBP.GetValueOrDefault(), cprops.PF_SG.GetValueOrDefault());
                }
                else
                {
                    double[] w = null;
                    ratinterpolation.buildfloaterhormannrationalinterpolant(cb.ToArray(), visc100.Count, 1, ref w);
                    cprops.PF_v1 = polinterpolation.barycentricinterpolation(cb.ToArray(), visc100.ToArray(), w, cb.Count, tc.fvm);
                    ratinterpolation.buildfloaterhormannrationalinterpolant(cb.ToArray(), visc210.Count, 1, ref w);
                    cprops.PF_v2 = polinterpolation.barycentricinterpolation(cb.ToArray(), visc210.ToArray(), w, cb.Count, tc.fvm);
                    cprops.PF_Tv1 = (100 - 32) / 9 * 5 + 273.15;
                    cprops.PF_Tv2 = (210 - 32) / 9 * 5 + 273.15;
                }

                cprops.PF_vA = PropertyMethods.ViscWaltherASTM_A(cprops.PF_Tv1.GetValueOrDefault(), cprops.PF_v1.GetValueOrDefault(), cprops.PF_Tv2.GetValueOrDefault(), cprops.PF_v2.GetValueOrDefault());
                cprops.PF_vB = PropertyMethods.ViscWaltherASTM_B(cprops.PF_Tv1.GetValueOrDefault(), cprops.PF_v1.GetValueOrDefault(), cprops.PF_Tv2.GetValueOrDefault(), cprops.PF_v2.GetValueOrDefault());

                //Tc
                switch (Tccorr)
                {
                    case "Riazi-Daubert (1985)":
                        cprops.Critical_Temperature = PropertyMethods.Tc_RiaziDaubert(cprops.NBP.GetValueOrDefault(), cprops.PF_SG.GetValueOrDefault());
                        break;
                    case "Lee-Kesler (1976)":
                        cprops.Critical_Temperature = PropertyMethods.Tc_LeeKesler(cprops.NBP.GetValueOrDefault(), cprops.PF_SG.GetValueOrDefault());
                        break;
                    case "Farah (2006)":
                        cprops.Critical_Temperature = PropertyMethods.Tc_Farah(cprops.PF_vA.GetValueOrDefault(), cprops.PF_vB.GetValueOrDefault(), cprops.NBP.GetValueOrDefault(), cprops.PF_SG.GetValueOrDefault());
                        break;
                    case "Riazi (2005)":
                        cprops.Critical_Temperature = PropertyMethods.Tc_Riazi(cprops.NBP.GetValueOrDefault(), cprops.PF_SG.GetValueOrDefault());
                        break;
                }

                //Pc
                switch (Pccorr)
                {
                    case "Riazi-Daubert (1985)":
                        cprops.Critical_Pressure = PropertyMethods.Pc_RiaziDaubert(cprops.NBP.GetValueOrDefault(), cprops.PF_SG.GetValueOrDefault());
                        break;
                    case "Lee-Kesler (1976)":
                        cprops.Critical_Pressure = PropertyMethods.Pc_LeeKesler(cprops.NBP.GetValueOrDefault(), cprops.PF_SG.GetValueOrDefault());
                        break;
                    case "Farah (2006)":
                        cprops.Critical_Pressure = PropertyMethods.Pc_Farah(cprops.PF_vA.GetValueOrDefault(), cprops.PF_vB.GetValueOrDefault(), cprops.NBP.GetValueOrDefault(), cprops.PF_SG.GetValueOrDefault());
                        break;
                }

                //Af
                switch (AFcorr)
                {
                    case "Lee-Kesler (1976)":
                        cprops.Acentric_Factor = PropertyMethods.AcentricFactor_LeeKesler(cprops.Critical_Temperature, cprops.Critical_Pressure, cprops.NBP.GetValueOrDefault());
                        break;
                    case "Korsten (2000)":
                        cprops.Acentric_Factor = PropertyMethods.AcentricFactor_Korsten(cprops.Critical_Temperature, cprops.Critical_Pressure, cprops.NBP.GetValueOrDefault());
                        break;
                }

                cprops.Normal_Boiling_Point = cprops.NBP.GetValueOrDefault();

                cprops.IsPF = 1;
                cprops.PF_Watson_K = Math.Pow((1.8 * cprops.NBP.GetValueOrDefault()), 0.33333) / cprops.PF_SG.GetValueOrDefault();

                var tmp = (double[])methods2.calculate_Hf_Sf(cprops.PF_SG.GetValueOrDefault(), cprops.Molar_Weight, cprops.NBP.GetValueOrDefault());

                cprops.IG_Enthalpy_of_Formation_25C = tmp[0];
                cprops.IG_Entropy_of_Formation_25C = tmp[1];
                cprops.IG_Gibbs_Energy_of_Formation_25C = tmp[0] - 298.15 * tmp[1];

                cprops.Formula = "C" + Convert.ToDouble(tmp[2]).ToString("N2") + "H" + Convert.ToDouble(tmp[3]).ToString("N2");

                DWSIM.Thermodynamics.Utilities.Hypos.Methods.HYP methods = new DWSIM.Thermodynamics.Utilities.Hypos.Methods.HYP();

                cprops.HVap_A = methods.DHvb_Vetere(cprops.Critical_Temperature, cprops.Critical_Pressure, cprops.Normal_Boiling_Point) / cprops.Molar_Weight;

                cprops.Critical_Compressibility = DWSIM.Thermodynamics.PropertyPackages.Auxiliary.PROPS.Zc1(cprops.Acentric_Factor);
                cprops.Critical_Volume = 8314 * cprops.Critical_Compressibility * cprops.Critical_Temperature / cprops.Critical_Pressure;
                cprops.Z_Rackett = DWSIM.Thermodynamics.PropertyPackages.Auxiliary.PROPS.Zc1(cprops.Acentric_Factor);
                if (cprops.Z_Rackett < 0)
                {
                    cprops.Z_Rackett = 0.2;
                }

                cprops.Chao_Seader_Acentricity = cprops.Acentric_Factor;
                cprops.Chao_Seader_Solubility_Parameter = Math.Pow(((cprops.HVap_A * cprops.Molar_Weight - 8.314 * cprops.Normal_Boiling_Point) * 238.846 * DWSIM.Thermodynamics.PropertyPackages.Auxiliary.PROPS.liq_dens_rackett(cprops.Normal_Boiling_Point, cprops.Critical_Temperature, cprops.Critical_Pressure, cprops.Acentric_Factor, cprops.Molar_Weight) / cprops.Molar_Weight / 1000000.0), 0.5);
                cprops.Chao_Seader_Liquid_Molar_Volume = 1 / DWSIM.Thermodynamics.PropertyPackages.Auxiliary.PROPS.liq_dens_rackett(cprops.Normal_Boiling_Point, cprops.Critical_Temperature, cprops.Critical_Pressure, cprops.Acentric_Factor, cprops.Molar_Weight) * cprops.Molar_Weight / 1000 * 1000000.0;

                methods = null;

                cprops.ID = 30000 + i + 1;

                i += 1;

            }

            //Adjust Acentric Factors and Rackett parameters to fit NBP and Density

            DensityFitting dfit = new DensityFitting();
            PRVSFitting prvsfit = new PRVSFitting();
            SRKVSFitting srkvsfit = new SRKVSFitting();
            NBPFitting nbpfit = new NBPFitting() { Flowsheet = flowsheet };
            MaterialStream tms = new MaterialStream("", "");
            PropertyPackage pp = default(PropertyPackage);
            double fzra = 0;
            double fw = 0;
            double fprvs = 0;
            double fsrkvs = 0;

            if (flowsheet.PropertyPackages.Count > 0)
            {
                pp = (DWSIM.Thermodynamics.PropertyPackages.PropertyPackage)flowsheet.PropertyPackages.Values.First();
            }
            else
            {
                pp = new PengRobinsonPropertyPackage();
            }

            foreach (var c in ccol.Values)
            {
                tms.Phases[0].Compounds.Add(c.Name, c);
            }

            bool recalcVc = false;

            i = 0;
            foreach (var c in ccol.Values)
            {
                if (adjustAf)
                {
                    nbpfit._pp = pp;
                    nbpfit._ms = tms;
                    nbpfit._idx = i;
                    if (c.ConstantProperties.Acentric_Factor < 0)
                    {
                        c.ConstantProperties.Acentric_Factor = 0.5;
                        recalcVc = true;
                    }
                    try
                    {
                        fw = nbpfit.MinimizeError();
                    }
                    catch (Exception ex)
                    {
                        flowsheet.ShowMessage("Error fitting Acentric Factor for compound '" + c.Name + "': " + ex.Message, IFlowsheet.MessageType.GeneralError);
                    }
                    c.ConstantProperties.Acentric_Factor *= fw;
                }
                c.ConstantProperties.Z_Rackett = DWSIM.Thermodynamics.PropertyPackages.Auxiliary.PROPS.Zc1(c.ConstantProperties.Acentric_Factor);
                if (c.ConstantProperties.Z_Rackett < 0)
                {
                    c.ConstantProperties.Z_Rackett = 0.2;
                    recalcVc = true;
                }
                c.ConstantProperties.Critical_Compressibility = DWSIM.Thermodynamics.PropertyPackages.Auxiliary.PROPS.Zc1(c.ConstantProperties.Acentric_Factor);
                c.ConstantProperties.Critical_Volume = DWSIM.Thermodynamics.PropertyPackages.Auxiliary.PROPS.Vc(c.ConstantProperties.Critical_Temperature, c.ConstantProperties.Critical_Pressure, c.ConstantProperties.Acentric_Factor, c.ConstantProperties.Critical_Compressibility);
                if (adjustZR)
                {
                    dfit._comp = c;
                    try
                    {
                        fzra = dfit.MinimizeError();
                    }
                    catch (Exception ex)
                    {
                        flowsheet.ShowMessage("Error fitting Rackett Parameter for compound '" + c.Name + "': " + ex.Message, IFlowsheet.MessageType.GeneralError);
                    }
                    c.ConstantProperties.Z_Rackett *= fzra;
                }
                if (c.ConstantProperties.Critical_Compressibility < 0 | recalcVc)
                {
                    c.ConstantProperties.Critical_Compressibility = c.ConstantProperties.Z_Rackett;
                    c.ConstantProperties.Critical_Volume = DWSIM.Thermodynamics.PropertyPackages.Auxiliary.PROPS.Vc(c.ConstantProperties.Critical_Temperature, c.ConstantProperties.Critical_Pressure, c.ConstantProperties.Acentric_Factor, c.ConstantProperties.Critical_Compressibility);
                }

                c.ConstantProperties.PR_Volume_Translation_Coefficient = 1;
                prvsfit._comp = c;
                fprvs = prvsfit.MinimizeError();
                var _with8 = c.ConstantProperties;
                if (Math.Abs(fprvs) < 99.0)
                    _with8.PR_Volume_Translation_Coefficient *= fprvs;
                else
                    _with8.PR_Volume_Translation_Coefficient = 0.0;
                c.ConstantProperties.SRK_Volume_Translation_Coefficient = 1;
                srkvsfit._comp = c;
                fsrkvs = srkvsfit.MinimizeError();
                var _with9 = c.ConstantProperties;
                if (Math.Abs(fsrkvs) < 99.0)
                    _with9.SRK_Volume_Translation_Coefficient *= fsrkvs;
                else
                    _with9.SRK_Volume_Translation_Coefficient = 0.0;
                recalcVc = false;
                i += 1;
            }

            pp = null;
            dfit = null;
            nbpfit = null;
            tms = null;

            return ccol;

        }

        void ParseCurveData(string[] datalines, IUnitsOfMeasure su)
        {

            cb.Clear();
            tbp.Clear();
            sgc.Clear();
            mwc.Clear();
            visc100.Clear();
            visc210.Clear();

            foreach (string line in datalines)
            {
                var val = line.Trim().Split(new[] { ' ', '\t' });
                tbp.Add(cv.ConvertToSI(su.temperature, val[1].ToDoubleWithSeparator(decsep1)));
                cb.Add(val[0].ToDoubleWithSeparator(decsep1) / 100);
                if (hasmwc)
                {
                    mwc.Add(val[2].ToDoubleWithSeparator(decsep1));
                    if (hassgc)
                    {
                        sgc.Add(val[3].ToDoubleWithSeparator(decsep1));
                        if (hasvisc100c)
                        {
                            visc100.Add(cv.ConvertToSI(su.cinematic_viscosity, val[4].ToDoubleWithSeparator(decsep1)));
                            if (hasvisc210c)
                            {
                                visc210.Add(cv.ConvertToSI(su.cinematic_viscosity, val[5].ToDoubleWithSeparator(decsep1)));
                            }
                        }
                        else
                        {
                            if (hasvisc210c)
                            {
                                visc210.Add(cv.ConvertToSI(su.cinematic_viscosity, val[4].ToDoubleWithSeparator(decsep1)));
                            }
                        }
                    }
                    else
                    {
                        if (hasvisc100c)
                        {
                            visc100.Add(cv.ConvertToSI(su.cinematic_viscosity, val[3].ToDoubleWithSeparator(decsep1)));
                        }
                        else
                        {
                            if (hasvisc210c)
                            {
                                visc210.Add(cv.ConvertToSI(su.cinematic_viscosity, val[3].ToDoubleWithSeparator(decsep1)));
                            }
                        }
                    }
                }
                else
                {
                    if (hassgc)
                    {
                        sgc.Add(val[2].ToDoubleWithSeparator(decsep1));
                        if (hasvisc100c)
                        {
                            visc100.Add(cv.ConvertToSI(su.cinematic_viscosity, val[3].ToDoubleWithSeparator(decsep1)));
                            if (hasvisc210c)
                            {
                                visc210.Add(cv.ConvertToSI(su.cinematic_viscosity, val[4].ToDoubleWithSeparator(decsep1)));
                            }
                        }
                        else
                        {
                            if (hasvisc210c)
                            {
                                visc210.Add(cv.ConvertToSI(su.cinematic_viscosity, val[3].ToDoubleWithSeparator(decsep1)));
                            }
                        }
                    }
                    else
                    {
                        if (hasvisc100c)
                        {
                            visc100.Add(cv.ConvertToSI(su.cinematic_viscosity, val[2].ToDoubleWithSeparator(decsep1)));
                            if (hasvisc210c)
                            {
                                visc210.Add(cv.ConvertToSI(su.cinematic_viscosity, val[3].ToDoubleWithSeparator(decsep1)));
                            }
                        }
                        else
                        {
                            if (hasvisc210c)
                            {
                                visc210.Add(cv.ConvertToSI(su.cinematic_viscosity, val[2].ToDoubleWithSeparator(decsep1)));
                            }
                        }
                    }
                }
            }


        }


        private double GetFV(double[] coeffs, double fv0, double t)
        {

            //TBP(K) = aa + bb*fv + cc*fv^2 + dd*fv^3 + ee*fv^4 + ff*fv^5 + gg*fv^6 (fv 0 ~ 1)

            double f = 0;
            double f0 = 0;
            double df = 0;
            int cnt = 0;
            double fv = fv0;
            do
            {
                f0 = (coeffs[0] + coeffs[1] * fv + coeffs[2] * Math.Pow(fv, 2) + coeffs[3] * Math.Pow(fv, 3) + coeffs[4] * Math.Pow(fv, 4) + coeffs[5] * Math.Pow(fv, 5) + coeffs[6] * Math.Pow(fv, 6));
                f = -t + (coeffs[0] + coeffs[1] * fv + coeffs[2] * Math.Pow(fv, 2) + coeffs[3] * Math.Pow(fv, 3) + coeffs[4] * Math.Pow(fv, 4) + coeffs[5] * Math.Pow(fv, 5) + coeffs[6] * Math.Pow(fv, 6));
                df = coeffs[1] + 2 * coeffs[2] * fv + 3 * coeffs[3] * Math.Pow(fv, 2) + 4 * coeffs[4] * Math.Pow(fv, 3) + 5 * coeffs[5] * Math.Pow(fv, 4) + 6 * coeffs[6] * Math.Pow(fv, 5);
                fv = -f / df * 0.3 + fv;
                if (fv < 0)
                    fv = Math.Abs(fv);
                cnt += 1;
            } while (!(Math.Abs(f) < 1E-09 | cnt >= 1000));

            return fv;

        }

        private double GetT(double[] coeffs, double fv)
        {

            //TBP(K) = aa + bb*fv + cc*fv^2 + dd*fv^3 + ee*fv^4 + ff*fv^5 + gg*fv^6 (fv 0 ~ 1)

            return (coeffs[0] + coeffs[1] * fv + coeffs[2] * Math.Pow(fv, 2) + coeffs[3] * Math.Pow(fv, 3) + coeffs[4] * Math.Pow(fv, 4) + coeffs[5] * Math.Pow(fv, 5) + coeffs[6] * Math.Pow(fv, 6));

        }

        public void CalculateMolarFractions(Dictionary<string, Compound> ccol)
        {
            double sum1 = 0;
            double fm = 0;
            double fv = 0;
            double fw = 0;
            int i = 0;

            switch (curvebasis)
            {
                case 0:
                    //liquid volume percent
                    i = 0;
                    sum1 = 0;
                    foreach (var subst in ccol.Values)
                    {
                        fv = (tccol[i].fvf - tccol[i].fv0) / (tccol[tccol.Count - 1].fvf - tccol[0].fv0);
                        fw = fv * subst.ConstantProperties.PF_SG.GetValueOrDefault();
                        fm = fw / subst.ConstantProperties.Molar_Weight;
                        sum1 += fm;
                        i = i + 1;
                    }

                    i = 0;
                    foreach (var subst in ccol.Values)
                    {
                        fv = (tccol[i].fvf - tccol[i].fv0) / (tccol[tccol.Count - 1].fvf - tccol[0].fv0);
                        fw = fv * subst.ConstantProperties.PF_SG.GetValueOrDefault();
                        fm = fw / subst.ConstantProperties.Molar_Weight;
                        subst.MoleFraction = fm / sum1;
                        i = i + 1;
                    }

                    break;
                case 1:
                    //mole percent
                    i = 0;
                    foreach (var subst in ccol.Values)
                    {
                        subst.MoleFraction = (tccol[i].fvf - tccol[i].fv0) / (tccol[tccol.Count - 1].fvf - tccol[0].fv0);
                        i = i + 1;
                    }

                    break;
                case 2:
                    //weight percent
                    i = 0;
                    sum1 = 0;
                    foreach (var subst in ccol.Values)
                    {
                        fw = (tccol[i].fvf - tccol[i].fv0) / (tccol[tccol.Count - 1].fvf - tccol[0].fv0);
                        fm = fw / subst.ConstantProperties.Molar_Weight;
                        sum1 += fm;
                        i = i + 1;
                    }

                    i = 0;
                    foreach (var subst in ccol.Values)
                    {
                        fw = (tccol[i].fvf - tccol[i].fv0) / (tccol[tccol.Count - 1].fvf - tccol[0].fv0);
                        fm = fw / subst.ConstantProperties.Molar_Weight;
                        subst.MoleFraction = fm / sum1;
                        i = i + 1;
                    }


                    break;
            }

            double wxtotal = 0;

            foreach (var subst in ccol.Values)
            {
                wxtotal += subst.MoleFraction.GetValueOrDefault() * subst.ConstantProperties.Molar_Weight;
            }

            foreach (var subst in ccol.Values)
            {
                subst.MassFraction = subst.MoleFraction * subst.ConstantProperties.Molar_Weight / wxtotal;
            }

        }
    }
}
