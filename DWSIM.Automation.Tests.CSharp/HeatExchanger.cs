using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HeatExchangerArea
{
    class HeatExchanger
    {
        static void Main()
        {
            System.IO.Directory.SetCurrentDirectory("C:/Program Files/DWSIM5"); // replace with DWSIM's installation directory on your computer

            //create automation manager

            DWSIM.Automation.Automation interf = new DWSIM.Automation.Automation();

            DWSIM.Interfaces.IFlowsheet sim;

            // workaround

            DWSIM.GlobalSettings.Settings.OldUI = false;

            // base path

            var basepath = "//Mac/Home/Downloads/";

            //load *.dwxml empty simulation file

            string fileName = basepath + "Simulation_template.dwxml";

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

            // create and add an instance of NRTL Property Package

            var nrtl = new DWSIM.Thermodynamics.PropertyPackages.NRTLPropertyPackage();
            nrtl.ComponentName = "NRTL";
            nrtl.ComponentDescription = "Any Description"; // <-- important to set any text as description.

            sim.AddPropertyPackage(nrtl);

            m1.PropertyPackage = sim.PropertyPackages.Values.First();
            m2.PropertyPackage = sim.PropertyPackages.Values.First();
            c1.PropertyPackage = sim.PropertyPackages.Values.First();

            // set inlet stream temperature
            // default properties: T = 298.15 K, P = 101325 Pa, Mass Flow = 1 kg/s

            var ms1 = (DWSIM.Thermodynamics.Streams.MaterialStream)m1;

            ms1.SetTemperature(400); // K

            // set cooler outlet temperature

            var cooler = (DWSIM.UnitOperations.UnitOperations.Cooler)c1;

            cooler.CalcMode = DWSIM.UnitOperations.UnitOperations.Cooler.CalculationMode.OutletTemperature;
            cooler.OutletTemperature = 320; // K

            // request a calculation

            sim.RequestCalculation();

            Console.WriteLine(String.Format("Cooler Heat Load: {0} kW", cooler.DeltaQ.GetValueOrDefault()));

            // compounds

            var selected = sim.SelectedCompounds;

            var available = sim.AvailableCompounds;

            var ethanol = available["Ethanol"];

            selected.Add("Ethanol", ethanol);

            // add ethanol compound to existing material streams
            // this is not exposed directly to the user, but can be done in a future update using a simplified function.

            foreach (var obj in sim.SimulationObjects.Values.Where(x => x is DWSIM.Thermodynamics.Streams.MaterialStream))
            {
                var ms = (DWSIM.Thermodynamics.Streams.MaterialStream)obj;
                foreach (DWSIM.Thermodynamics.BaseClasses.Phase p in ms.Phases.Values)
                {
                    var comp = new DWSIM.Thermodynamics.BaseClasses.Compound("Ethanol", "");
                    comp.ConstantProperties = ethanol;
                    p.Compounds.Add("Ethanol", comp);
                }
            }

            // change overall molar composition of m1

            ms1.SetOverallComposition(new []{ 0.3, 0.3, 0.4 }); // methanol, water, ethanol

            // request a calculation

            sim.RequestCalculation();

            Console.WriteLine(String.Format("Cooler Heat Load: {0} kW", cooler.DeltaQ.GetValueOrDefault()));

            // change cooler calculation mode

            cooler.CalcMode = DWSIM.UnitOperations.UnitOperations.Cooler.CalculationMode.OutletVaporFraction;
            cooler.OutletVaporFraction = 0.3; // K

            // request a calculation

            sim.RequestCalculation();

            // get outlet ms vapor fraction

            var ms2 = (DWSIM.Thermodynamics.Streams.MaterialStream)m2;

            var msvf = ms2.GetPhase("Vapor").Properties.molarfraction.GetValueOrDefault();

            Console.WriteLine(String.Format("Cooler Heat Load: {0} kW", cooler.DeltaQ.GetValueOrDefault()));
            Console.WriteLine(String.Format("Outlet Vapor Fraction: {0}", msvf));

            // save file

            string fileNameToSave = basepath + "Created_file_HeatExchanger.dwxml";
            interf.SaveFlowsheet(sim, fileNameToSave, false); //use true for dwxmz

            Console.WriteLine("Done! press any key to close.");
            Console.ReadKey();

        }
    }
}
