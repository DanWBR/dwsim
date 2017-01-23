using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Interfaces;
using DWSIM.FlowsheetSolver;
using System.Runtime.InteropServices;

namespace DWSIM.Automation
{

    [Guid("ed615e8f-da69-4c24-80e2-bfe342168060")]
    public interface AutomationInterface
    {
        Interfaces.IFlowsheet LoadFlowsheet(string filepath);
        void SaveFlowsheet(IFlowsheet flowsheet, string filepath, bool compressed);
        void CalculateFlowsheet(IFlowsheet flowsheet, ISimulationObject sender = null);
    }

    [Guid("37437090-e541-4f2c-9856-d1e27df32ecb"), ClassInterface(ClassInterfaceType.None)]
    public class Automation: AutomationInterface
    {

        FormMain fm = null;

        public Automation()
        {
            fm = new FormMain();
        }

        public Interfaces.IFlowsheet LoadFlowsheet(string filepath)
        {
            if (System.IO.Path.GetExtension(filepath).ToLower().Contains("dwxmz"))
            {
                return fm.LoadAndExtractXMLZIP(filepath, true);
            }
            else
            {
                return fm.LoadXML(filepath, "", true);
            }
        }

        public void SaveFlowsheet(IFlowsheet flowsheet, string filepath, bool compressed)
        {
            if (compressed) {
                fm.SaveXMLZIP(filepath, (FormFlowsheet)flowsheet);
            }
            else {
                fm.SaveXML(filepath, (FormFlowsheet)flowsheet);
            }
        }

        public void CalculateFlowsheet(IFlowsheet flowsheet, ISimulationObject sender = null)
        {
            if ((sender != null))
            {
                FlowsheetSolver.FlowsheetSolver.CalculateObject(this, sender.Name);
            }
            else {
                FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(this, GlobalSettings.Settings.SolverMode);
            }
        }

    }

}
