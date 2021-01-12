using System;
using System.IO;
using System.Reflection;

public class Test3
{
    [STAThread]
    static void Main()
    {
        System.IO.Directory.SetCurrentDirectory(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location));
        var interf = new DWSIM.Automation.Automation2();
        for (var i = 0; i <= 200; i++)
        {
            Console.WriteLine($"Iteration #{i + 1}");
            var sim = interf.LoadFlowsheet("samples" + System.IO.Path.DirectorySeparatorChar + "Carbon Combustion.dwxml");
            interf.ReleaseResources();
        }
        Console.WriteLine("Done.");
        Console.ReadLine();
    }
}