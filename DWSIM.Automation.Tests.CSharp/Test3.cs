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

        var sim = interf.LoadFlowsheet(@"");

        sim.SetMessageListener((s, mt) => Console.WriteLine(s));

        var errors = interf.CalculateFlowsheet3(sim, 3600);

        interf.SaveFlowsheet2(sim, @"");

        Console.WriteLine("Solved.");

        Console.ReadLine();
    }

}