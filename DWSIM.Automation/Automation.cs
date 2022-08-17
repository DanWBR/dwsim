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
    public class Automation : AutomationInterface
    {

        FormMain fm = null;

        public Automation()
        {
            GlobalSettings.Settings.AutomationMode = true;
            Console.WriteLine("Initializing DWSIM in Automation Mode, please wait...");
            fm = new FormMain();
        }

        public Interfaces.IFlowsheet LoadFlowsheet(string filepath)
        {
            Console.WriteLine("Loading Flowsheet data, please wait...");
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
            Console.WriteLine("Saving the Flowsheet, please wait...");
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
            GlobalSettings.Settings.SolverMode = 0;
            GlobalSettings.Settings.SolverTimeoutSeconds = 120;
            GlobalSettings.Settings.EnableGPUProcessing = false;
            GlobalSettings.Settings.EnableParallelProcessing = true;
            Console.WriteLine("Solving Flowsheet, please wait...");
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
            GlobalSettings.Settings.SolverMode = 0;
            GlobalSettings.Settings.SolverTimeoutSeconds = 120;
            GlobalSettings.Settings.EnableGPUProcessing = false;
            GlobalSettings.Settings.EnableParallelProcessing = true;
            return FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(flowsheet, GlobalSettings.Settings.SolverMode);
        }

        public List<Exception> CalculateFlowsheet3(IFlowsheet flowsheet, int timeout_seconds)
        {
            GlobalSettings.Settings.CalculatorActivated = true;
            GlobalSettings.Settings.SolverBreakOnException = true;
            GlobalSettings.Settings.SolverMode = 0;
            GlobalSettings.Settings.SolverTimeoutSeconds = timeout_seconds;
            GlobalSettings.Settings.EnableGPUProcessing = false;
            GlobalSettings.Settings.EnableParallelProcessing = true;
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
    public class Automation2 : AutomationInterface
    {

        Eto.Forms.Application app;
        UI.Forms.Flowsheet fm;

        public Automation2()
        {
            GlobalSettings.Settings.AutomationMode = true;
            AppDomain currentDomain = AppDomain.CurrentDomain;
            currentDomain.AssemblyResolve += new ResolveEventHandler(LoadAssembly);
            Console.WriteLine("Initializing DWSIM Automation Interface...");
            app = UI.Desktop.Program.MainApp(null);
            app.Attach(this);
            FlowsheetBase.FlowsheetBase.AddPropPacks();
            Console.WriteLine("DWSIM Automation Interface initialized successfully.");
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
            Console.WriteLine("Initializing the Flowsheet, please wait...");
            fm = new UI.Forms.Flowsheet();
            Console.WriteLine("Loading Flowsheet data, please wait...");
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
            Console.WriteLine("Saving the Flowsheet, please wait...");
            fm.FlowsheetObject = (Flowsheet)flowsheet;
            fm.SaveSimulation(filepath);
        }

        public void CalculateFlowsheet(IFlowsheet flowsheet, ISimulationObject sender)
        {
            GlobalSettings.Settings.CalculatorActivated = true;
            GlobalSettings.Settings.SolverBreakOnException = true;
            GlobalSettings.Settings.SolverMode = 1;
            GlobalSettings.Settings.EnableGPUProcessing = false;
            GlobalSettings.Settings.EnableParallelProcessing = true;
            Console.WriteLine("Solving Flowsheet, please wait...");
            fm.FlowsheetObject.SolveFlowsheet2();
        }

        public List<Exception> CalculateFlowsheet2(IFlowsheet flowsheet)
        {
            GlobalSettings.Settings.CalculatorActivated = true;
            GlobalSettings.Settings.SolverBreakOnException = true;
            GlobalSettings.Settings.SolverMode = 1;
            GlobalSettings.Settings.SolverTimeoutSeconds = 120;
            GlobalSettings.Settings.EnableGPUProcessing = false;
            GlobalSettings.Settings.EnableParallelProcessing = true;
            return FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(flowsheet, GlobalSettings.Settings.SolverMode);
        }

        public List<Exception> CalculateFlowsheet3(IFlowsheet flowsheet, int timeout_seconds)
        {
            GlobalSettings.Settings.CalculatorActivated = true;
            GlobalSettings.Settings.SolverBreakOnException = true;
            GlobalSettings.Settings.SolverMode = 1;
            GlobalSettings.Settings.SolverTimeoutSeconds = timeout_seconds;
            GlobalSettings.Settings.EnableGPUProcessing = false;
            GlobalSettings.Settings.EnableParallelProcessing = true;
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
            Console.WriteLine("Initializing the Flowsheet, please wait...");
            fm = new UI.Forms.Flowsheet();
            return fm.FlowsheetObject;
        }

        public object GetMainWindow()
        {
            throw new NotImplementedException();
        }
    }

    [Guid("62486815-2330-4CDE-8962-41F576B0C2B8"), ClassInterface(ClassInterfaceType.None)]
    public class Automation3
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

        public void ReleaseResources()
        {

        }

        public void SaveFlowsheet(IFlowsheet flowsheet, string filepath, bool compressed)
        {
            ((Flowsheet2)flowsheet).SaveSimulation(filepath);
        }

        public void CalculateFlowsheet(IFlowsheet flowsheet, ISimulationObject sender)
        {
            Settings.CalculatorActivated = true;
            Settings.SolverBreakOnException = true;
            Settings.SolverMode = 1;
            Settings.EnableGPUProcessing = false;
            Settings.EnableParallelProcessing = true;
            ((Flowsheet2)flowsheet).SolveFlowsheet2();
        }

        public void CalculateFlowsheet2(IFlowsheet flowsheet)
        {
            Settings.CalculatorActivated = true;
            Settings.SolverBreakOnException = true;
            Settings.SolverMode = 1;
            Settings.SolverTimeoutSeconds = 3600;
            Settings.EnableGPUProcessing = false;
            Settings.EnableParallelProcessing = false;
            ((Flowsheet2)flowsheet).SolveFlowsheet2();
        }

        public void CalculateFlowsheet3(IFlowsheet flowsheet, int timeout_seconds)
        {
            Settings.CalculatorActivated = true;
            Settings.SolverBreakOnException = true;
            Settings.SolverMode = 1;
            Settings.SolverTimeoutSeconds = timeout_seconds;
            Settings.EnableGPUProcessing = false;
            Settings.EnableParallelProcessing = true;
            ((Flowsheet2)flowsheet).SolveFlowsheet2();
        }

        public List<Exception> CalculateFlowsheet4(IFlowsheet flowsheet)
        {
            Settings.CalculatorActivated = true;
            Settings.SolverBreakOnException = true;
            Settings.SolverMode = 1;
            Settings.SolverTimeoutSeconds = 3600;
            Settings.EnableGPUProcessing = false;
            Settings.EnableParallelProcessing = false;
            return ((Flowsheet2)flowsheet).SolveFlowsheet2();
        }

        public void SaveFlowsheet2(IFlowsheet flowsheet, string filepath)
        {
            SaveFlowsheet(flowsheet, filepath, true);
        }

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

    }

}
