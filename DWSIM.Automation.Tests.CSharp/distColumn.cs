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
    class distColumn
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
            sim.AddCompound("Ethanol");

            // add objects

            var m1 = (MaterialStream)sim.AddObject(ObjectType.MaterialStream, 50, 50, "feed");
            var m2 = (MaterialStream)sim.AddObject(ObjectType.MaterialStream, 250, 50, "distillate");
            var m3 = (MaterialStream)sim.AddObject(ObjectType.MaterialStream, 250, 50, "bottoms");
            var e1 = (EnergyStream)sim.AddObject(ObjectType.EnergyStream, 100, 50, "cond. duty");
            var e2 = (EnergyStream)sim.AddObject(ObjectType.EnergyStream, 100, 50, "reb. duty");
            var d1 = (DistillationColumn)sim.AddObject(ObjectType.DistillationColumn, 100, 50, "column");

            d1.SetNumberOfStages(20);
            d1.ConnectFeed(m1, 10);
            d1.ConnectDistillate(m2);
            d1.ConnectBottoms(m3);
            d1.ConnectCondenserDuty(e1);
            d1.ConnectReboilerDuty(e2);

            d1.SetCondenserSpec("Reflux Ratio", 2, "");
            d1.SetReboilerSpec("Product Molar Flow Rate", 75, "mol/s", "");

            d1.SetTopPressure(101325.0);
            d1.ColumnPressureDrop = 0.0;

            sim.AutoLayout();

            // property package

            sim.CreateAndAddPropertyPackage("NRTL");

            m1.SetTemperature(300); // K
            m1.SetMolarFlow(100.0); // mol/s

            m1.SetOverallCompoundMolarFlow("Water", 50.0);
            m1.SetOverallCompoundMolarFlow("Ethanol", 50.0);

            // request a calculation

            interf.CalculateFlowsheet2(sim);

            Console.WriteLine(String.Format("Condenser Duty: {0} kW", e1.EnergyFlow.GetValueOrDefault()));
            Console.WriteLine(String.Format("Reboiler Duty: {0} kW", e2.EnergyFlow.GetValueOrDefault()));

            Console.WriteLine();

            Console.WriteLine(String.Format("Distillate Flow Rate: {0} mol/s", m2.GetMolarFlow()));
            Console.WriteLine(String.Format("Bottoms Flow Rate Duty: {0} mol/s", m3.GetMolarFlow()));

            Console.WriteLine();

            Console.WriteLine(String.Format("Distillate Temperature: {0} K", m2.GetTemperature()));
            Console.WriteLine(String.Format("Bottoms Temperature: {0} K", m3.GetTemperature()));

            Console.WriteLine();

            Console.WriteLine(String.Format("Distillate Composition: Water: {0}, Ethanol: {1}", 
                m2.Phases[0].Compounds["Water"].MoleFraction.GetValueOrDefault(),
                m2.Phases[0].Compounds["Ethanol"].MoleFraction.GetValueOrDefault()));
            Console.WriteLine(String.Format("Bottoms Composition: Water: {0}, Ethanol: {1}",
                m3.Phases[0].Compounds["Water"].MoleFraction.GetValueOrDefault(),
                m3.Phases[0].Compounds["Ethanol"].MoleFraction.GetValueOrDefault()));


            Console.WriteLine("Done! press any key to close.");
            Console.ReadKey();

        }
    }
}

