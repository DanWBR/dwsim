using System;
using System.IO;
using System.Linq;
using System.Reflection;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.Thermodynamics.Streams;
using DWSIM.UnitOperations.Reactors;
using DWSIM.UnitOperations.Streams;
using DWSIM.UnitOperations.UnitOperations;

namespace DWSIM.Automation.Tests.CSharp
{
    class pfrReactor
    {
        [STAThread]
        static void Main()
        {
            System.IO.Directory.SetCurrentDirectory(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location));

            //create automation manager

            var interf = new DWSIM.Automation.Automation3();

            var sim = interf.CreateFlowsheet();

            // add compounds

            sim.AddCompound("Water");
            sim.AddCompound("Ethylene glycol");
            sim.AddCompound("Ethylene oxide");

            // add equilibrium reaction

            var comps1 = new System.Collections.Generic.Dictionary<string, double>() {
                    { "Ethylene oxide", -1 }, {"Water", -1 }, { "Ethylene glycol", 1} };

            var dorders = new System.Collections.Generic.Dictionary<string, double>() {
                    { "Ethylene oxide",  1}, {"Water", 0 }, { "Ethylene glycol", 0} };

            var rorders = new System.Collections.Generic.Dictionary<string, double>() {
                    { "Ethylene oxide",  0}, {"Water", 0 }, { "Ethylene glycol", 0} };

            var kr1 = sim.CreateKineticReaction("Ethylene Glycol Production", "Production of Ethylene Glycol from Ethylene Oxide and Water",
                                                comps1, dorders, rorders, "Ethylene oxide", "Mixture", "Molar Concentration", "kmol/m3", "kmol/[m3.h]",
                                                0.5, 0.0, 0.0, 0.0, "", "");

            sim.AddReaction(kr1);
            sim.AddReactionToSet(kr1.ID, "DefaultSet", true, 0);

            // add objects

            var m1 = (MaterialStream)sim.AddObject(ObjectType.MaterialStream, 50, 50, "inlet");
            var m2 = (MaterialStream)sim.AddObject(ObjectType.MaterialStream, 150, 50, "outlet");
            var e1 = sim.AddObject(ObjectType.EnergyStream, 100, 50, "heat");
            var pfr = (Reactor_PFR)sim.AddObject(ObjectType.RCT_PFR, 100, 50, "reactor");

            pfr.ConnectFeedMaterialStream(m1, 0);
            pfr.ConnectProductMaterialStream(m2, 0);
            pfr.ConnectFeedEnergyStream(e1, 1);

            pfr.ReactorOperationMode = OperationMode.Isothermic;
            pfr.ReactorSizingType = Reactor_PFR.SizingType.Length;
            pfr.Volume = 1.0; // m3
            pfr.Length = 1.2; // m

            sim.AutoLayout();

            // property package

            sim.CreateAndAddPropertyPackage("Raoult's Law");

            m1.SetTemperature(328.2); // K

            m1.SetMolarFlow(0.0); // mol/s

            m1.SetOverallCompoundMolarFlow("Ethylene oxide", 2.39);
            m1.SetOverallCompoundMolarFlow("Water", 9.57);

            // request a calculation

            interf.CalculateFlowsheet2(sim);

            Console.WriteLine(String.Format("Reactor Heat Load: {0} kW", pfr.DeltaQ.GetValueOrDefault()));
            foreach (var c in pfr.ComponentConversions)
            {
                if (c.Value > 0) Console.WriteLine(String.Format("{0} conversion: {1}", c.Key, c.Value));
            }

            Console.WriteLine("Done! press any key to close.");
            Console.ReadKey();

        }
    }
}
