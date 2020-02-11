using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AddObjectsToDWSIM
{
    class Program
    {
        static void Main()
        {
            // replace with DWSIM's installation directory on your computer
            System.IO.Directory.SetCurrentDirectory("C:/Program Files/DWSIM5");

            //create automation manager
            DWSIM.Automation.Automation interf = new DWSIM.Automation.Automation();

            DWSIM.Interfaces.IFlowsheet sim;

            //load *.dwxmz empty simulation file
            string fileName = "simulation_template.dwxmz";

            sim = interf.LoadFlowsheet(fileName);

            var c1 = sim.AddObject(DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.Cooler, 100, 100, "COOLER-001"); 
            var e1 = sim.AddObject(DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.EnergyStream, 130, 150, "HEAT_OUT");
            var m1 = sim.AddObject(DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream, 50, 100, "INLET");
            var m2 = sim.AddObject(DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream, 150, 100, "OUTLET");

            // create the graphic object connectors manually as they are not being drawn on screen. 

            ((dynamic)c1.GraphicObject).PositionConnectors();
            ((dynamic)m1.GraphicObject).PositionConnectors();
            ((dynamic)m2.GraphicObject).PositionConnectors();
            ((dynamic)e1.GraphicObject).PositionConnectors();

            // connect the graphic objects.

            sim.ConnectObjects(m1.GraphicObject, c1.GraphicObject, 0, 0);
            sim.ConnectObjects(c1.GraphicObject, m2.GraphicObject, 0, 0);
            sim.ConnectObjects(c1.GraphicObject, e1.GraphicObject, 0, 0);

            // create and add an instance of PR Property Package
            
            var pr = new DWSIM.Thermodynamics.PropertyPackages.PengRobinsonPropertyPackage();
            pr.ComponentName = "Peng-Robinson (PR)";
            pr.ComponentDescription = "Any Description"; // <-- important to set any text as description.

            sim.AddPropertyPackage(pr);

            m1.PropertyPackage = sim.PropertyPackages.Values.First();
            m2.PropertyPackage = sim.PropertyPackages.Values.First();
            c1.PropertyPackage = sim.PropertyPackages.Values.First();

            // request a calculation

            sim.RequestCalculation();

            // save file as dwxmz (compressed XML)

            string fileNameToSave = "created_file.dwxmz";
            interf.SaveFlowsheet(sim, fileNameToSave, true);
        }
    }
}