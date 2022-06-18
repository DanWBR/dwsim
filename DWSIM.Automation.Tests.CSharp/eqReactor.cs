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
    class eqReactor
    {
        [STAThread]
        static void Main()
        {
            System.IO.Directory.SetCurrentDirectory(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location));

            //create automation manager

            var interf = new DWSIM.Automation.Automation3();

            var sim = interf.CreateFlowsheet();

            // add compounds

            sim.AddCompound("Carbon dioxide");
            sim.AddCompound("Carbon monoxide");
            sim.AddCompound("Water");
            sim.AddCompound("Hydrogen");
            sim.AddCompound("Methane");

            // add equilibrium reaction

            var comps1 = new System.Collections.Generic.Dictionary<string, double>() {
                    { "Methane", -1 }, {"Water", -2 }, { "Carbon dioxide", 1}, {  "Hydrogen", 4  } };

            var er1 = sim.CreateEquilibriumReaction("R1", "", comps1, "Methane", "Vapor", "Fugacity", "", 0, "");

            var comps2 = new System.Collections.Generic.Dictionary<string, double>() {
                    { "Methane", -1 }, {"Water", -1 }, { "Carbon monoxide", 1}, {  "Hydrogen", 3  } };

            var er2 = sim.CreateEquilibriumReaction("R2", "", comps2, "Water", "Vapor", "Fugacity", "", 0, "");

            sim.AddReaction(er1);
            sim.AddReaction(er2);
            sim.AddReactionToSet(er1.ID, "DefaultSet", true, 0);
            sim.AddReactionToSet(er2.ID, "DefaultSet", true, 0);

            // add objects

            var m1 = (MaterialStream)sim.AddObject(ObjectType.MaterialStream, 50, 50, "inlet");
            var m2 = (MaterialStream)sim.AddObject(ObjectType.MaterialStream, 150, 50, "gas outlet");
            var m3 = (MaterialStream)sim.AddObject(ObjectType.MaterialStream, 150, 50, "liquid outlet");
            var e1 = sim.AddObject(ObjectType.EnergyStream, 100, 50, "heat");
            var eq1 = (Reactor_Equilibrium)sim.AddObject(ObjectType.RCT_Equilibrium, 100, 50, "reactor");

            eq1.ConnectFeedMaterialStream(m1, 0);
            eq1.ConnectProductMaterialStream(m2, 0);
            eq1.ConnectProductMaterialStream(m3, 1);
            eq1.ConnectFeedEnergyStream(e1, 1);

            eq1.ReactorOperationMode = OperationMode.Isothermic;
            eq1.DeltaP = 0.0;

            sim.AutoLayout();

            // property package

            sim.CreateAndAddPropertyPackage("Peng-Robinson (PR)");

            m1.SetTemperature(1000.0); // K
            m1.SetMolarFlow(5.0); // mol/s

            m1.SetOverallCompoundMolarFlow("Methane", 2.0);
            m1.SetOverallCompoundMolarFlow("Water", 3.0);
            m1.SetOverallCompoundMolarFlow("Carbon dioxide", 0.0);
            m1.SetOverallCompoundMolarFlow("Carbon monoxide", 0.0);
            m1.SetOverallCompoundMolarFlow("Hydrogen", 0.0);

            // request a calculation

            interf.CalculateFlowsheet2(sim);

            Console.WriteLine(String.Format("Reactor Heat Load: {0} kW", eq1.DeltaQ.GetValueOrDefault()));
            Console.WriteLine(String.Format("R1 Reaction Extent: {0} mol/s", eq1.ReactionExtents["R1"]));
            Console.WriteLine(String.Format("R2 Reaction Extent: {0} mol/s", eq1.ReactionExtents["R2"]));
            foreach (var c in eq1.ComponentConversions)
                {
                if (c.Value > 0) Console.WriteLine(String.Format("{0} conversion: {1}", c.Key, c.Value));
            }
           
            Console.WriteLine("Done! press any key to close.");
            Console.ReadKey();

        }
    }
}
