using System;
using System.Diagnostics;
using System.IO;
using System.Reflection;

public class LoopTest
{
    [STAThread]
    static void Main()
    {

        System.IO.Directory.SetCurrentDirectory(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location));

        //create automation manager

        var sw = new Stopwatch();
        sw.Start();

        var interf = new DWSIM.Automation.Automation3();

        for (int i = 0; i < 100; i++)
        {

            // load Cavett's Problem simulation file
            var sim = interf.LoadFlowsheet(Path.Combine("samples", "Biodiesel Production.dwxmz"));

            sim.SetMessageListener((s, mt) => Console.WriteLine(s));

            interf.CalculateFlowsheet2(sim);

            sim = null;

            interf.ReleaseResources();

        }

        sw.Stop();

        Console.WriteLine("Finished.");

        Console.ReadLine();
    }

}