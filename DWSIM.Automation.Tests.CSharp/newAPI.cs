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

            var m1 = (MaterialStream)sim.AddObject(ObjectType.MaterialStream, 50, 50, "inlet1");
            var m2 = (MaterialStream)sim.AddObject(ObjectType.MaterialStream, 150, 50, "inlet2");
            var m3 = (MaterialStream)sim.AddObject(ObjectType.MaterialStream, 150, 50, "outlet");
            var mx1 = (Mixer)sim.AddObject(ObjectType.Mixer, 100, 50, "mixer");

            mx1.ConnectFeedMaterialStream(m1, 0);
            mx1.ConnectFeedMaterialStream(m2, 1);
            mx1.ConnectProductMaterialStream(m3, 0);

            for (int i = 0; i < 5; i++)
            {
                var isattached = mx1.GraphicObject.InputConnectors[i].IsAttached;
                if (!isattached)
                    // port is available to connect
                    mx1.ConnectFeedMaterialStream(m2, i);
                else
                    // try next port, current one is already connected
                    mx1.ConnectFeedMaterialStream(m2, i + 1);
            }

            sim.AutoLayout();

            // steam table sproperty package

            var availablepps = sim.GetAvailablePropertyPackages();
            var steamppname = availablepps.Where(pp => pp.ToLower().Contains("steam")).FirstOrDefault();

            sim.CreateAndAddPropertyPackage(steamppname);

            // set inlet stream temperature
            // default properties: T = 298.15 K, P = 101325 Pa, Mass Flow = 1 kg/s

            m1.SetTemperature(300); // K
            m1.SetMassFlow(100); // kg/s

            m2.SetTemperature(348); // K
            m2.SetMassFlow(50); // kg/s

            // request a calculation

            interf.CalculateFlowsheet2(sim);

            // save file

            string fileNameToSave = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Desktop), "mixer.dwxmz");
            interf.SaveFlowsheet(sim, fileNameToSave, true); //use true for dwxmz

            Console.WriteLine("Done! press any key to close.");
            Console.ReadKey();

        }
    }
}
