using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.Thermodynamics.Streams;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

public class Test3
{
    [STAThread]
    static void Main()
    {

        System.IO.Directory.SetCurrentDirectory(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location));

        //create automation manager

        var interf = new DWSIM.Automation.Automation2();

        var sim = interf.LoadFlowsheet(@"C:\Users\Daniel\Downloads\150747-205762.dwxml");

        sim.SetMessageListener((s, mt) => Console.WriteLine(s));

        var msobj = sim.GetFlowsheetSimulationObject("PRODUCED GAS");

        msobj.SetPropertyValue("PROP_MS_104/Metano", 10);

        interf.SaveFlowsheet(sim, @"C:\Users\Daniel\Downloads\150747-205762.dwxml", false);

        var errors = interf.CalculateFlowsheet3(sim, 3600);

        var obj = sim.GetSelectedFlowsheetSimulationObject("compressed gas");
        var temperature = obj.GetPropertyValue("PROP_MS_0");

        Console.WriteLine("Solved.");

        Console.ReadLine();
    }

}