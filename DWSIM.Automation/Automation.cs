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
using Org.BouncyCastle.Utilities.Collections;
using System.Runtime.InteropServices.ComTypes;
using System.Windows.Forms;
using static System.Net.Mime.MediaTypeNames;
using System.Collections.ObjectModel;

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

        public Automation3()
        {
            Settings.AutomationMode = true;
            Settings.InspectorEnabled = false;
            Settings.CultureInfo = "en";
            GlobalSettings.Settings.AutomationMode = true;
            AppDomain currentDomain = AppDomain.CurrentDomain;
            currentDomain.AssemblyResolve += new ResolveEventHandler(LoadAssembly);
            FlowsheetBase.FlowsheetBase.AddPropPacks();
            LoadExtenders();
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
                            item.SetMainWindow(null);
                            item.Run();
                        }
                    } }
                catch (Exception ex)
                {
                    Logging.Logger.LogError("Extender Initialization", ex);
                }
            }
        }

    }

}
