﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Interfaces;
using DWSIM.FlowsheetSolver;
using System.Runtime.InteropServices;
using DWSIM.UI.Desktop.Shared;
using System.Xml.Linq;
using DWSIM.UnitOperations.UnitOperations.Auxiliary;
using System.IO;

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
        IFlowsheet CreateFlowsheet();


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
                return fm.LoadAndExtractXMLZIP(filepath, null, true);
            }
            else
            {
                return fm.LoadXML(filepath, null, "", true);
            }
        }

        public void SaveFlowsheet(IFlowsheet flowsheet, string filepath, bool compressed)
        {
            Console.WriteLine("Saving the Flowsheet, please wait...");
            if (compressed)
            {
                fm.SaveXMLZIP(filepath, (FormFlowsheet)flowsheet);
            }
            else
            {
                fm.SaveXML(filepath, (FormFlowsheet)flowsheet);
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

        public void SaveFlowsheet2(IFlowsheet flowsheet, string filepath)
        {
            SaveFlowsheet(flowsheet, filepath, true);
        }

        public IFlowsheet CreateFlowsheet()
        {
            return new FormFlowsheet();
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
            Console.WriteLine("Initializing DWSIM Automation Interface...");
            app = UI.Desktop.Program.MainApp(null);
            app.Attach(this);
            FlowsheetBase.FlowsheetBase.AddPropPacks();
            Console.WriteLine("DWSIM Automation Interface initialized successfully.");
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
            GlobalSettings.Settings.EnableGPUProcessing = false;
            GlobalSettings.Settings.EnableParallelProcessing = true;
            Console.WriteLine("Solving Flowsheet, please wait...");
            fm.FlowsheetObject.SolveFlowsheet2();
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
    }


}
