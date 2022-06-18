using System;
using System.IO;
using System.Linq;
using System.Reflection;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.Thermodynamics.Streams;
using DWSIM.UnitOperations.Streams;
using DWSIM.UnitOperations.UnitOperations;

namespace DWSIM.Automation.Tests.CSharp
{
     class newAPI
    {
        [STAThread]
        static void Main()
        {
            System.IO.Directory.SetCurrentDirectory(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location));

            //create automation manager

            var interf = new DWSIM.Automation.Automation3();

            var sim = interf.CreateFlowsheet();

            // add water

            sim.AddCompound("Water");

            var m1 = (MaterialStream)sim.AddObject(ObjectType.MaterialStream, 50, 50, "inlet");
            var m2 = (MaterialStream)sim.AddObject(ObjectType.MaterialStream, 150, 50, "outlet");
            var e1 = (EnergyStream)sim.AddObject(ObjectType.EnergyStream, 100, 50, "power");
            var h1 = (Heater)sim.AddObject(ObjectType.Heater, 100, 50, "heater");

            //var connections = h1.GetConnectionPortsList();

            //foreach (var item in connections)
            //{
            //    Console.WriteLine(item);
            //}

            h1.ConnectFeedMaterialStream(m1, 0);
            h1.ConnectProductMaterialStream(m2, 0);
            h1.ConnectFeedEnergyStream(e1, 1);

            sim.AutoLayout();

            // steam table sproperty package

            var availablepps = sim.GetAvailablePropertyPackages();
            var steamppname = availablepps.Where(pp => pp.ToLower().Contains("steam")).FirstOrDefault();

            sim.CreateAndAddPropertyPackage(steamppname);

            // set inlet stream temperature
            // default properties: T = 298.15 K, P = 101325 Pa, Mass Flow = 1 kg/s

            m1.SetTemperature(300); // K
            m1.SetMassFlow(100); // kg/s

            // set heater outlet temperature

            h1.CalcMode = Heater.CalculationMode.OutletTemperature;
            h1.OutletTemperature = 400; // K

            // request a calculation

            interf.CalculateFlowsheet2(sim);

            Console.WriteLine(String.Format("Heater Heat Load: {0} kW", h1.DeltaQ.GetValueOrDefault()));

            Console.WriteLine("Done! press any key to close.");
            Console.ReadKey();

        }
    }
}
