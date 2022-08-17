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
    class convReactor
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

            // add conversion reactions

            var comps1 = new System.Collections.Generic.Dictionary<string, double>() {
                    { "Methane", -1 }, {"Water", -2 }, { "Carbon dioxide", 1}, {  "Hydrogen", 4  } };

            var r1 = sim.CreateConversionReaction("R1", "", comps1, "Methane", "Vapor", "50");

            var comps2 = new System.Collections.Generic.Dictionary<string, double>() {
                    { "Methane", -1 }, {"Water", -1 }, { "Carbon monoxide", 1}, {  "Hydrogen", 3  } };

            var r2 = sim.CreateConversionReaction("R2", "", comps2, "Water", "Vapor", "50");

            sim.AddReaction(r1);
            sim.AddReaction(r2);
            sim.AddReactionToSet(r1.ID, "DefaultSet", true, 0);
            sim.AddReactionToSet(r2.ID, "DefaultSet", true, 0);

            // add objects

            var m1 = (MaterialStream)sim.AddObject(ObjectType.MaterialStream, 50, 50, "inlet");
            var m2 = (MaterialStream)sim.AddObject(ObjectType.MaterialStream, 150, 50, "gas outlet");
            var m3 = (MaterialStream)sim.AddObject(ObjectType.MaterialStream, 150, 50, "liquid outlet");
            var e1 = sim.AddObject(ObjectType.EnergyStream, 100, 50, "heat");
            var cr1 = (Reactor_Conversion)sim.AddObject(ObjectType.RCT_Conversion, 100, 50, "reactor");

            cr1.ConnectFeedMaterialStream(m1, 0);
            cr1.ConnectProductMaterialStream(m2, 0);
            cr1.ConnectProductMaterialStream(m3, 1);
            cr1.ConnectFeedEnergyStream(e1, 1);

            cr1.ReactorOperationMode = OperationMode.Isothermic;
            cr1.DeltaP = 0.0;

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

            Console.WriteLine(String.Format("Reactor Heat Load: {0} kW", cr1.DeltaQ.GetValueOrDefault()));
            foreach (var c in cr1.ComponentConversions)
            {
                if (c.Value > 0) Console.WriteLine(String.Format("{0} overall conversion: {1}", c.Key, c.Value));
            }

            Console.WriteLine("Done! press any key to close.");
            Console.ReadKey();

        }
    }
}

