using System;
using System.Linq;
using System.Reflection;
using System.IO;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.Thermodynamics.Streams;
using DWSIM.UnitOperations.UnitOperations;
using System.Diagnostics;

class oldAPI
{
    [STAThread]
    static void Main()
    {
        System.IO.Directory.SetCurrentDirectory(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location));

        //create automation manager

        var interf = new DWSIM.Automation.Automation3();

        var sim = interf.CreateFlowsheet();

        // add water

        var water = sim.AvailableCompounds["Water"];

        sim.SelectedCompounds.Add(water.Name, water);

        var m1 = (MaterialStream)sim.AddObject(ObjectType.MaterialStream, 50, 50, "inlet");
        var m2 = (MaterialStream)sim.AddObject(ObjectType.MaterialStream, 150, 50, "outlet");
        var e1 = sim.AddObject(ObjectType.EnergyStream, 100, 50, "power");
        var h1 = (Heater)sim.AddObject(ObjectType.Heater, 100, 50, "heater");

        sim.ConnectObjects(m1.GraphicObject, h1.GraphicObject, -1, -1);
        sim.ConnectObjects(h1.GraphicObject, m2.GraphicObject, -1, -1);
        sim.ConnectObjects(e1.GraphicObject, h1.GraphicObject, -1, -1);

        sim.AutoLayout();

        // steam table sproperty package

        var stables = new DWSIM.Thermodynamics.PropertyPackages.SteamTablesPropertyPackage();

        sim.AddPropertyPackage(stables);

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

        // save file

        string fileNameToSave = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Desktop), "heatersample.dwxml");
        interf.SaveFlowsheet(sim, fileNameToSave, false); //use true for dwxmz

        Console.WriteLine("Done! press any key to close.");
        Console.ReadKey();

    }
}