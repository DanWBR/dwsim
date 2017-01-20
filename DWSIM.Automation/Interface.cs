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
    [ComVisible(true)]public class Interface
    {

        FormMain fm;

        public Interface()
        {
            FormMain fm = new FormMain();
        }

        public Interfaces.IFlowsheet LoadFlowsheet(string filepath)
        {
            return fm.LoadXML(filepath, "", true);            
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
