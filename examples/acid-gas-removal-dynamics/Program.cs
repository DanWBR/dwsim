using System;
using System.IO;
using System.Reflection;

/// <summary>
/// Entry point for the Acid Gas Removal Dynamic flowsheet generator.
///
/// Usage:
///   GenerateFlowsheet.exe [outputFile]
///
/// If no argument is given the output is written as
/// "acid-gas-removal-dynamics.dwxmz" inside the same directory as this
/// executable.
/// </summary>
class Program
{
    [STAThread]
    static int Main(string[] args)
    {
        try
        {
            // Directory that contains the Python integration scripts.
            // The EXE is built into the DWSIM output folder; the scripts live
            // two levels up inside the source tree.
            string exeDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
            string scriptDir = Path.GetFullPath(
                args.Length > 1 ? args[1] :
                Path.Combine(exeDir, @"..\..\examples\acid-gas-removal-dynamics"));

            // Absolute path for the output file.
            string outputFile = Path.GetFullPath(
                args.Length > 0 ? args[0] :
                Path.Combine(scriptDir, "acid-gas-removal-dynamics.dwxmz"));

            // Set current directory to EXE location so that DWSIM can resolve
            // its own assemblies via the AssemblyResolve event.
            Directory.SetCurrentDirectory(exeDir);

            Console.WriteLine("[GenerateFlowsheet] Initializing DWSIM Automation...");
            var automation = new DWSIM.Automation.Automation3();

            // Switch to the scripts directory so that File.ReadAllText calls
            // inside the template resolve correctly.
            Directory.SetCurrentDirectory(scriptDir);

            Console.WriteLine("[GenerateFlowsheet] Building acid-gas-removal flowsheet...");
            AcidGasRemovalDynamicTemplate.Generate(outputFile);

            Console.WriteLine("[GenerateFlowsheet] Flowsheet saved to: " + outputFile);
            return 0;
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine("[GenerateFlowsheet] ERROR: " + ex.Message);
            Console.Error.WriteLine(ex.StackTrace);
            return 1;
        }
    }
}
