using System;
using System.Collections.Generic;
using DWSIM.Interfaces;
using System.Runtime.InteropServices;
using DWSIM.UI.Desktop.Shared;
using System.Xml.Linq;
using System.IO;
using System.Reflection;
using DWSIM.SharedClassesCSharp.FilePicker.Windows;
using DWSIM.GlobalSettings;
using System.Linq;
using DWSIM.Interfaces.Enums;
using DWSIM.SharedClasses;
using DWSIM.Thermodynamics.PropertyPackages;
using DWSIM.Thermodynamics;
using System.Threading.Tasks;
using DWSIM.Thermodynamics.AdvancedEOS;
using CapeOpen;
using DWSIM.Thermodynamics.BaseClasses;

namespace DWSIM.Automation
{

    [Guid("ed615e8f-da69-4c24-80e2-bfe342168060")]
    public interface AutomationInterface
    {
        Interfaces.IFlowsheet LoadFlowsheet(string filepath);
        void SaveFlowsheet(IFlowsheet flowsheet, string filepath, bool compressed);
        void SaveFlowsheet2(IFlowsheet flowsheet, string filepath);
        void CalculateFlowsheet(IFlowsheet flowsheet, ISimulationObject sender);
        List<Exception> CalculateFlowsheet2(IFlowsheet flowsheet);
        List<Exception> CalculateFlowsheet3(IFlowsheet flowsheet, int timeout_seconds);
        IFlowsheet CreateFlowsheet();
        void ReleaseResources();
        object GetMainWindow();

    }

    [Guid("37437090-e541-4f2c-9856-d1e27df32ecb"), ClassInterface(ClassInterfaceType.None)]
    [ComVisible(true)]
    public class Automation : AutomationInterface
    {

        FormMain fm = null;

        public Automation()
        {
            GlobalSettings.Settings.AutomationMode = true;
            fm = new FormMain();
        }

        public Interfaces.IFlowsheet LoadFlowsheet(string filepath)
        {
            if (System.IO.Path.GetExtension(filepath).ToLower().Contains("dwxmz"))
            {
                return fm.LoadAndExtractXMLZIP(new WindowsFile(filepath), null, true);
            }
            else
            {
                return fm.LoadXML(new WindowsFile(filepath), null, "", true);
            }
        }

        public void SaveFlowsheet(IFlowsheet flowsheet, string filepath, bool compressed)
        {
            if (compressed)
            {
                fm.SaveXMLZIP(new WindowsFile(filepath), (FormFlowsheet)flowsheet);
            }
            else
            {
                fm.SaveXML(new WindowsFile(filepath), (FormFlowsheet)flowsheet);
            }
        }

        public void CalculateFlowsheet(IFlowsheet flowsheet, ISimulationObject sender)
        {
            GlobalSettings.Settings.CalculatorActivated = true;
            GlobalSettings.Settings.SolverBreakOnException = true;
            if ((sender != null))
            {
                FlowsheetSolver.FlowsheetSolver.CalculateObject(flowsheet, sender.Name);
            }
            else
            {
                FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(flowsheet, GlobalSettings.Settings.SolverMode);
            }
        }

        public List<Exception> CalculateFlowsheet2(IFlowsheet flowsheet)
        {
            GlobalSettings.Settings.CalculatorActivated = true;
            GlobalSettings.Settings.SolverBreakOnException = true;
            return FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(flowsheet, GlobalSettings.Settings.SolverMode);
        }

        public List<Exception> CalculateFlowsheet3(IFlowsheet flowsheet, int timeout_seconds)
        {
            GlobalSettings.Settings.CalculatorActivated = true;
            GlobalSettings.Settings.SolverBreakOnException = true;
            GlobalSettings.Settings.SolverTimeoutSeconds = timeout_seconds;
            return FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(flowsheet, GlobalSettings.Settings.SolverMode);
        }

        public void SaveFlowsheet2(IFlowsheet flowsheet, string filepath)
        {
            SaveFlowsheet(flowsheet, filepath, true);
        }

        public IFlowsheet CreateFlowsheet()
        {
            return new FormFlowsheet();
        }

        public void ReleaseResources()
        {
            if (fm != null && !fm.IsDisposed)
            {
                fm.Dispose();
                fm = null;
            }
        }

        public object GetMainWindow()
        {
            return fm;
        }
    }

    [Guid("22694b87-1ba6-4341-81dd-8d33f48643d7"), ClassInterface(ClassInterfaceType.None)]
    [ComVisible(true)]
    public class Automation2 : AutomationInterface
    {

        Eto.Forms.Application app;
        UI.Forms.Flowsheet fm;

        public Automation2()
        {
            GlobalSettings.Settings.AutomationMode = true;
            AppDomain currentDomain = AppDomain.CurrentDomain;
            currentDomain.AssemblyResolve += new ResolveEventHandler(LoadAssembly);
            app = UI.Desktop.Program.MainApp(null);
            app.Attach(this);
            FlowsheetBase.FlowsheetBase.AddPropPacks();
        }

        static Assembly LoadAssembly(object sender, ResolveEventArgs args)
        {
            string assemblyPath = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), new AssemblyName(args.Name).Name + ".dll");
            if (!File.Exists(assemblyPath))
            {
                return null;
            }
            else
            {
                Assembly assembly = Assembly.LoadFrom(assemblyPath);
                return assembly;
            }
        }

        public Interfaces.IFlowsheet LoadFlowsheet(string filepath)
        {
            GlobalSettings.Settings.AutomationMode = true;
            fm = new UI.Forms.Flowsheet();
            LoadSimulation(filepath);
            return fm.FlowsheetObject;
        }

        public void ReleaseResources()
        {
            fm?.Dispose();
            fm = null;
        }

        public void SaveFlowsheet(IFlowsheet flowsheet, string filepath, bool compressed)
        {
            fm.FlowsheetObject = (Flowsheet)flowsheet;
            fm.SaveSimulation(filepath);
        }

        public void CalculateFlowsheet(IFlowsheet flowsheet, ISimulationObject sender)
        {
            GlobalSettings.Settings.CalculatorActivated = true;
            GlobalSettings.Settings.SolverBreakOnException = true;
            fm.FlowsheetObject.SolveFlowsheet2();
        }

        public List<Exception> CalculateFlowsheet2(IFlowsheet flowsheet)
        {
            GlobalSettings.Settings.CalculatorActivated = true;
            GlobalSettings.Settings.SolverBreakOnException = true;
            return FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(flowsheet, GlobalSettings.Settings.SolverMode);
        }

        public List<Exception> CalculateFlowsheet3(IFlowsheet flowsheet, int timeout_seconds)
        {
            GlobalSettings.Settings.CalculatorActivated = true;
            GlobalSettings.Settings.SolverTimeoutSeconds = timeout_seconds;
            return FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(flowsheet, GlobalSettings.Settings.SolverMode);
        }

        private void LoadSimulation(string path)
        {
            if (System.IO.Path.GetExtension(path).ToLower() == ".dwxmz")
            {
                var xdoc = fm.FlowsheetObject.LoadZippedXML(path);
                xdoc = null;
            }
            else if (System.IO.Path.GetExtension(path).ToLower() == ".dwxml")
            {
                fm.FlowsheetObject.LoadFromXML(XDocument.Load(path));
            }
            else if (System.IO.Path.GetExtension(path).ToLower() == ".xml")
            {
                fm.FlowsheetObject.LoadFromMXML(XDocument.Load(path));
            }
            fm.FlowsheetObject.FilePath = path;
            fm.FlowsheetObject.FlowsheetOptions.FilePath = path;
        }

        public void SaveFlowsheet2(IFlowsheet flowsheet, string filepath)
        {
            SaveFlowsheet(flowsheet, filepath, true);
        }

        public IFlowsheet CreateFlowsheet()
        {
            GlobalSettings.Settings.AutomationMode = true;
            fm = new UI.Forms.Flowsheet();
            return fm.FlowsheetObject;
        }

        public object GetMainWindow()
        {
            throw new NotImplementedException();
        }
    }

    [Guid("62486815-2330-4CDE-8962-41F576B0C2B8"), ClassInterface(ClassInterfaceType.None)]
    [ComVisible(true)]
    public class Automation3 : AutomationInterface
    {

        public Dictionary<String, IPropertyPackage> AvailablePropertyPackages { get; } = new Dictionary<string, IPropertyPackage>();

        public Dictionary<String, ICompoundConstantProperties> AvailableCompounds { get; } = new Dictionary<string, ICompoundConstantProperties>();

        public Automation3()
        {
            Settings.AutomationMode = true;
            Settings.InspectorEnabled = false;
            Settings.CultureInfo = "en";
            GlobalSettings.Settings.AutomationMode = true;
            AppDomain currentDomain = AppDomain.CurrentDomain;
            currentDomain.AssemblyResolve += new ResolveEventHandler(LoadAssembly);
            FlowsheetBase.FlowsheetBase.AddPropPacks();
            LoadItems();
            LoadExtenders();
        }

        private void LoadItems()
        {

            // proppacks

            if (AvailablePropertyPackages.Count > 0) return;

            var plist = new System.Collections.Concurrent.BlockingCollection<PropertyPackage>();

            var t1 = TaskHelper.Run(() =>
            {
                var CPPP = new CoolPropPropertyPackage();
                CPPP.ComponentName = "CoolProp";
                plist.Add(CPPP);

                var CPIPP = new CoolPropIncompressiblePurePropertyPackage();
                CPIPP.ComponentName = "CoolProp (Incompressible Fluids)";
                CPIPP.ComponentDescription = "CoolProp (Incompressible Fluids)";
                plist.Add(CPIPP);

                var CPIMPP = new CoolPropIncompressibleMixturePropertyPackage();
                CPIMPP.ComponentName = "CoolProp (Incompressible Mixtures)";
                CPIMPP.ComponentDescription = "CoolProp (Incompressible Mixtures)";
                plist.Add(CPIMPP);

                var STPP = new SteamTablesPropertyPackage();
                STPP.ComponentName = "Steam Tables (IAPWS-IF97)";
                plist.Add(STPP);

                var SEAPP = new SeawaterPropertyPackage();
                SEAPP.ComponentName = "Seawater IAPWS-08";
                plist.Add(SEAPP);
            });

            var t2 = TaskHelper.Run(() =>
            {

                var PRPP = new PengRobinsonPropertyPackage();
                PRPP.ComponentName = "Peng-Robinson (PR)";
                plist.Add(PRPP);

            });

            var t3 = TaskHelper.Run(() =>
            {

                var PRSV2PP = new PRSV2PropertyPackage();
                PRSV2PP.ComponentName = "Peng-Robinson-Stryjek-Vera 2 (PRSV2-M)";
                plist.Add(PRSV2PP);

                var PRSV2PPVL = new PRSV2VLPropertyPackage();
                PRSV2PPVL.ComponentName = "Peng-Robinson-Stryjek-Vera 2 (PRSV2-VL)";
                plist.Add(PRSV2PPVL);

            });

            var t4 = TaskHelper.Run(() =>
            {

                var SRKPP = new SRKPropertyPackage();
                SRKPP.ComponentName = "Soave-Redlich-Kwong (SRK)";
                plist.Add(SRKPP);

            });

            var t6 = TaskHelper.Run(() =>
            {

                var UPP = new UNIFACPropertyPackage();
                UPP.ComponentName = "UNIFAC";
                plist.Add(UPP);

                var ULLPP = new UNIFACLLPropertyPackage();
                ULLPP.ComponentName = "UNIFAC-LL";
                plist.Add(ULLPP);

                var MUPP = new MODFACPropertyPackage();
                MUPP.ComponentName = "Modified UNIFAC (Dortmund)";
                plist.Add(MUPP);

                var NUPP = new NISTMFACPropertyPackage();
                NUPP.ComponentName = "Modified UNIFAC (NIST)";
                plist.Add(NUPP);

            });

            var t10 = TaskHelper.Run(() =>
            {

                var WPP = new WilsonPropertyPackage();
                WPP.ComponentName = "Wilson";
                plist.Add(WPP);

                var NRTLPP = new NRTLPropertyPackage();
                NRTLPP.ComponentName = "NRTL";
                plist.Add(NRTLPP);

                var UQPP = new UNIQUACPropertyPackage();
                UQPP.ComponentName = "UNIQUAC";
                plist.Add(UQPP);

                var CSLKPP = new ChaoSeaderPropertyPackage();
                CSLKPP.ComponentName = "Chao-Seader";
                plist.Add(CSLKPP);

                var GSLKPP = new GraysonStreedPropertyPackage();
                GSLKPP.ComponentName = "Grayson-Streed";
                plist.Add(GSLKPP);

                var RPP = new RaoultPropertyPackage();
                RPP.ComponentName = "Raoult's Law";
                plist.Add(RPP);

                var LKPPP = new LKPPropertyPackage();
                LKPPP.ComponentName = "Lee-Kesler-Plöcker";
                plist.Add(LKPPP);

            });

            var t11 = TaskHelper.Run(() =>
            {

                var ISPP = new IdealElectrolytePropertyPackage();
                plist.Add(ISPP);

                var BOPP = new BlackOilPropertyPackage();
                BOPP.ComponentName = "Black Oil";
                plist.Add(BOPP);

                var GERGPP = new GERG2008PropertyPackage();
                plist.Add(GERGPP);

                var PCSAFTPP = new PCSAFT2PropertyPackage();
                plist.Add(PCSAFTPP);

                var PR78PP = new PengRobinson1978PropertyPackage();
                PR78PP.ComponentName = "Peng-Robinson 1978 (PR78)";
                plist.Add(PR78PP);

                var PR78Adv = new PengRobinson1978AdvancedPropertyPackage();
                plist.Add(PR78Adv);

                var SRKAdv = new SoaveRedlichKwongAdvancedPropertyPackage();
                plist.Add(SRKAdv);

            });

            Task.WaitAll(t1, t2, t3, t4, t6, t10, t11);

            foreach (var pp in plist)
            {
                AvailablePropertyPackages.Add(((ICapeIdentification)pp).ComponentName, pp);
            }

            var otherpps = SharedClasses.Utility.LoadAdditionalPropertyPackages();

            foreach (var pp in otherpps)
            {
                if (!AvailablePropertyPackages.ContainsKey(((ICapeIdentification)pp).ComponentName))
                    AvailablePropertyPackages.Add(((ICapeIdentification)pp).ComponentName, pp);
                else
                    Console.WriteLine(String.Format("Error adding External Property Package '{0}'. Check the 'ppacks' and 'extenders' folders for duplicate items.", ((ICapeIdentification)pp).ComponentName));
            }

            if (!Settings.IsRunningOnMono())
            {
                var COPP = new CAPEOPENPropertyPackage();
                COPP.ComponentName = "CAPE-OPEN";
                AvailablePropertyPackages.Add(COPP.ComponentName.ToString(), COPP);
            }

            // compounds

            if (AvailableCompounds.Count() > 0) return;

            var addedcomps = new List<String>();
            var casnumbers = new List<String>();

            var csdb = new Thermodynamics.Databases.ChemSep();
            csdb.Load();
            var cpa = csdb.Transfer();
            foreach (ConstantProperties cp in cpa)
            { if (!AvailableCompounds.ContainsKey(cp.Name)) AvailableCompounds.Add(cp.Name, cp); }

            var cpdb = new Thermodynamics.Databases.CoolProp();
            cpdb.Load();
            cpa = cpdb.Transfer();
            addedcomps = AvailableCompounds.Keys.Select((x) => x.ToLower()).ToList();
            foreach (ConstantProperties cp in cpa)
            { if (!AvailableCompounds.ContainsKey(cp.Name)) AvailableCompounds.Add(cp.Name, cp); }

            var bddb = new Thermodynamics.Databases.Biodiesel();
            bddb.Load();
            cpa = bddb.Transfer();
            addedcomps = AvailableCompounds.Keys.Select((x) => x.ToLower()).ToList();
            foreach (ConstantProperties cp in cpa)
            { if (!AvailableCompounds.ContainsKey(cp.Name)) AvailableCompounds.Add(cp.Name, cp); }

            var chedl = new Thermodynamics.Databases.ChEDL_Thermo();
            chedl.Load();
            cpa = chedl.Transfer().ToArray();

            addedcomps = AvailableCompounds.Keys.Select((x) => x.ToLower()).ToList();
            casnumbers = AvailableCompounds.Values.Select((x) => x.CAS_Number).ToList();

            foreach (ConstantProperties cp in cpa)
            {
                if (!addedcomps.Contains(cp.Name.ToLower()) && !addedcomps.Contains(cp.Name))
                    if (!casnumbers.Contains(cp.CAS_Number))
                        if (!AvailableCompounds.ContainsKey(cp.Name)) AvailableCompounds.Add(cp.Name, cp);
            }

            var elec = new Thermodynamics.Databases.Electrolyte();
            elec.Load();
            cpa = elec.Transfer().ToArray();
            addedcomps = AvailableCompounds.Keys.Select((x) => x.ToLower()).ToList();
            foreach (ConstantProperties cp in cpa)
            { if (!AvailableCompounds.ContainsKey(cp.Name)) AvailableCompounds.Add(cp.Name, cp); }

            var comps = Thermodynamics.Databases.UserDB.LoadAdditionalCompounds();
            foreach (ConstantProperties cp in comps)
            { if (!AvailableCompounds.ContainsKey(cp.Name)) AvailableCompounds.Add(cp.Name, cp); }

            using (var filestr = Assembly.GetAssembly(elec.GetType()).GetManifestResourceStream("DWSIM.Thermodynamics.FoodProp.xml"))
            {
                var fcomps = Thermodynamics.Databases.UserDB.ReadComps(filestr);
                foreach (var cp in fcomps)
                {
                    cp.CurrentDB = "FoodProp";
                    if (!AvailableCompounds.ContainsKey(cp.Name)) AvailableCompounds.Add(cp.Name, cp);
                }
            }

            csdb.Dispose();
            cpdb.Dispose();
            chedl.Dispose();

        }

        [DispId(0)]
        public string GetVersion()
        {

            var version = Assembly.GetExecutingAssembly().GetName().Version;
            var date = File.GetLastWriteTimeUtc(Assembly.GetExecutingAssembly().Location).ToString();

            return String.Format("DWSIM version {0} ({1})", version, date);

        }

        static Assembly LoadAssembly(object sender, ResolveEventArgs args)
        {
            var directories = new List<string>
            {
                Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "extenders"),
                Directory.GetParent(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)).FullName,
                Path.Combine(Directory.GetParent(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)).FullName, "extenders"),
                Directory.GetCurrentDirectory(),
                Path.Combine(Directory.GetCurrentDirectory(), "extenders")
            };

            foreach (var dir in directories)
            {
                var fullPath = Path.Combine(dir, new AssemblyName(args.Name).Name + ".dll");
                if (File.Exists(fullPath))
                {
                    var assembly = Assembly.LoadFrom(fullPath);
                    return assembly;
                }
            }

            return null;

        }

        [DispId(1)]
        public IFlowsheet LoadFlowsheet(string filepath, Action UIUpdHandler = null)
        {
            Settings.AutomationMode = true;
            Settings.CultureInfo = "en";
            var fsheet = new Flowsheet2(null, UIUpdHandler);
            fsheet.SupressDataLoading = true;
            fsheet.AvailableCompounds = AvailableCompounds;
            fsheet.PropertyPackages = AvailablePropertyPackages;
            fsheet.Init();
            if (System.IO.Path.GetExtension(filepath).ToLower().EndsWith("z"))
            {
                fsheet.LoadZippedXML(filepath);
            }
            else
            {
                fsheet.LoadFromXML(XDocument.Load(filepath));
            }
            return fsheet;
        }

        [DispId(2)]
        public IFlowsheet LoadFlowsheet2(string filepath)
        {
            Settings.AutomationMode = true;
            Settings.CultureInfo = "en";
            var fsheet = new Flowsheet2(null, null);
            fsheet.SupressDataLoading = true;
            fsheet.AvailableCompounds = AvailableCompounds;
            fsheet.PropertyPackages = AvailablePropertyPackages;
            fsheet.Init();
            if (System.IO.Path.GetExtension(filepath).ToLower().EndsWith("z"))
            {
                fsheet.LoadZippedXML(filepath);
            }
            else
            {
                fsheet.LoadFromXML(XDocument.Load(filepath));
            }
            return fsheet;
        }

        [DispId(3)]
        public void ReleaseResources()
        {

        }

        [DispId(4)]
        public void SaveFlowsheet(IFlowsheet flowsheet, string filepath, bool compressed)
        {
            ((Flowsheet2)flowsheet).SaveSimulation(filepath);
        }

        [DispId(5)]
        public void CalculateFlowsheet(IFlowsheet flowsheet, ISimulationObject sender)
        {
            Settings.CalculatorActivated = true;
            Settings.SolverBreakOnException = true;
            ((Flowsheet2)flowsheet).SolveFlowsheet2();
        }

        [DispId(6)]
        public void CalculateFlowsheet2(IFlowsheet flowsheet)
        {
            Settings.CalculatorActivated = true;
            Settings.SolverBreakOnException = true;
            ((Flowsheet2)flowsheet).SolveFlowsheet2();
        }

        [DispId(7)]
        public void CalculateFlowsheet3(IFlowsheet flowsheet, int timeout_seconds)
        {
            Settings.CalculatorActivated = true;
            Settings.SolverBreakOnException = true;
            Settings.SolverTimeoutSeconds = timeout_seconds;
            ((Flowsheet2)flowsheet).SolveFlowsheet2();
        }

        [DispId(8)]
        public List<Exception> CalculateFlowsheet4(IFlowsheet flowsheet)
        {
            Settings.CalculatorActivated = true;
            Settings.SolverBreakOnException = true;
            return ((Flowsheet2)flowsheet).SolveFlowsheet2();
        }

        [DispId(9)]
        public void SaveFlowsheet2(IFlowsheet flowsheet, string filepath)
        {
            SaveFlowsheet(flowsheet, filepath, true);
        }

        [DispId(10)]
        public IFlowsheet CreateFlowsheet()
        {
            Settings.AutomationMode = true;
            var f = new Flowsheet2(null, null);
            f.SupressDataLoading = true;
            f.AvailableCompounds = AvailableCompounds;
            f.PropertyPackages = AvailablePropertyPackages;
            f.Init();
            return f;
        }

        public object GetMainWindow()
        {
            throw new NotImplementedException();
        }

        public IFlowsheet LoadFlowsheet(string filepath)
        {
            try
            {
                return LoadFlowsheet(filepath, null);
            }
            catch (Exception ex)
            {
                Logging.Logger.LogError("Automation Error (LoadFlowsheet)", ex);
                throw ex;
            }
        }

        List<Exception> AutomationInterface.CalculateFlowsheet2(IFlowsheet flowsheet)
        {
            try
            {
                return CalculateFlowsheet4(flowsheet);
            }
            catch (Exception ex)
            {
                Logging.Logger.LogError("Automation Error (CalculateFlowsheet2)", ex);
                throw ex;
            }
        }

        List<Exception> AutomationInterface.CalculateFlowsheet3(IFlowsheet flowsheet, int timeout_seconds)
        {
            try
            {
                Settings.SolverTimeoutSeconds = timeout_seconds;
                return CalculateFlowsheet4(flowsheet);
            }
            catch (Exception ex)
            {
                Logging.Logger.LogError("Automation Error (CalculateFlowsheet3)", ex);
                throw ex;
            }
        }

        private List<Assembly> LoadExtenderDLLs()
        {
            List<Assembly> extenderdlls = new List<Assembly>();
            if (Directory.Exists(SharedClasses.Utility.GetExtendersRootDirectory()))
            {
                DirectoryInfo dinfo = new DirectoryInfo(SharedClasses.Utility.GetExtendersRootDirectory());
                FileInfo[] files = dinfo.GetFiles("*Extensions*.dll");
                if (!(files == null))
                {
                    foreach (FileInfo fi in files)
                    {
                        extenderdlls.Add(Assembly.LoadFrom(fi.FullName));
                    }
                }
            }
            return extenderdlls;
        }

        List<IExtenderCollection> GetExtenders(List<Assembly> alist)
        {
            List<Type> availableTypes = new List<Type>();
            foreach (var currentAssembly in alist)
            {
                try
                {
                    availableTypes.AddRange(currentAssembly.GetExportedTypes());
                }
                catch
                { }
            }
            var extList = availableTypes.FindAll(t => t.GetInterfaces().Contains(typeof(IExtenderCollection)));
            return extList.ConvertAll(t => (IExtenderCollection)Activator.CreateInstance(t));
        }

        void LoadExtenders()
        {

            List<IExtenderCollection> extlist = GetExtenders(LoadExtenderDLLs());

            foreach (var extender in extlist)
            {
                try
                {
                    if (extender.Level == ExtenderLevel.MainWindow)
                    {
                        foreach (var item in extender.Collection)
                        {
                            var load = false;
                            if (item is IExtender5) load = ((IExtender5)item).LoadInAutomationMode;
                            if (load)
                            {
                                item.SetMainWindow(null);
                                item.Run();
                            }
                        }
                    }
                }
                catch (Exception ex)
                {
                    Logging.Logger.LogError("Extender Initialization", ex);
                }
            }
        }

    }

}
