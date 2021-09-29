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

        var sim = interf.LoadFlowsheet(@"C:\309829-342429-1.dwxmz");

        sim.SetMessageListener((s, mt) => Console.WriteLine(s));

        var t1 = (double)sim.GetFlowsheetSimulationObject("RE-01").GetPropertyValue("PROP_RE_1") - 273.15;

        var errors = interf.CalculateFlowsheet3(sim, 3600);

        var t2 = (double)sim.GetFlowsheetSimulationObject("RE-01").GetPropertyValue("PROP_RE_1") - 273.15;

        Console.WriteLine(t1);
        Console.WriteLine(t2);

        Console.WriteLine("Solved.");

        Console.ReadLine();
    }

}