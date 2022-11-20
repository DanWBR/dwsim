using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace DWSIM.Tests
{
    class Program
    {
        static void Main(string[] args)
        {

            var interf = new DWSIM.Automation.Automation3();

            var samples = Directory.EnumerateFiles("samples", "*.dwxm*", SearchOption.TopDirectoryOnly).OrderBy(x => x).ToList();
            var fossee = Directory.EnumerateFiles("tests", "*.dwxm*", SearchOption.TopDirectoryOnly).OrderBy(x => x).ToList();
            var basic = Directory.EnumerateFiles(Path.Combine("tests", "basic"), "*.dwxm*", SearchOption.TopDirectoryOnly).OrderBy(x => x).ToList();

            Console.WriteLine();
            Console.WriteLine("[SET 1/3] Testing Basic Blocks...");
            Console.WriteLine();

            RunTests(basic, interf);

            Console.WriteLine();
            Console.WriteLine("[SET 2/3] Testing Sample Flowsheets...");
            Console.WriteLine();

            samples = samples.Where(x => !x.Contains("Dynamic") && !x.Contains("Cantera") && !x.Contains("Pervaporation")).ToList();

            RunTests(samples, interf);

            //Console.WriteLine();
            //Console.WriteLine("[SET 3/3] Testing FOSSEE Flowsheets...");
            //Console.WriteLine();

            //RunTests(fossee, interf);

            Console.WriteLine("Finished. Press Any Key to Continue...");
            Console.ReadKey();
        }

        static void RunTests(List<string> filelist, DWSIM.Automation.Automation3 interf)
        {

            TimeSpan totaltime;
            var partials = new List<TimeSpan>();
            DateTime dt0, dp0, dpf;

            dt0 = DateTime.Now;

            System.IO.Directory.SetCurrentDirectory(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location));

            DWSIM.Interfaces.IFlowsheet sim;

            List<Exception> errors;

            int totaltests, passedtests = 0, i = 1;

            totaltests = filelist.Count;

            var failed = new List<string>();

            foreach (var s in filelist)
            {

                dp0 = DateTime.Now;

                Console.WriteLine();
                Console.WriteLine("[" + i + "/" + totaltests + "] " + "Loading '" + Path.GetFileNameWithoutExtension(s) + "'...");
                Console.WriteLine();

                string status = "PASSED";

                try
                {

                    sim = interf.LoadFlowsheet(s);

                    Console.WriteLine();
                    Console.WriteLine("[" + i + "/" + totaltests + "] " + "Running '" + Path.GetFileNameWithoutExtension(s) + "'...");
                    Console.WriteLine();

                    errors = interf.CalculateFlowsheet4(sim);

                    if (errors.Count > 0)
                    {
                        status = "FAILED";
                        failed.Add(Path.GetFileNameWithoutExtension(s));
                        foreach (var error in errors)
                        {
                            Console.WriteLine(error.ToString());
                        }
                    }
                    else { passedtests += 1; }

                }
                catch(Exception ex)
                {
                    status = "FAILED";
                    failed.Add(Path.GetFileNameWithoutExtension(s));
                    Console.WriteLine("[" + i + "/" + totaltests + "] ERROR: " + ex.Message);
                }
                finally {
                    dpf = DateTime.Now;
                }

                partials.Add(dpf - dp0);

                Console.WriteLine();
                Console.WriteLine("[" + i + "/" + totaltests + "] " + "Test Result for '" + Path.GetFileNameWithoutExtension(s) + "': " + status + ", time taken: " + (dpf - dp0).TotalSeconds.ToString("N2") + " s");
                Console.WriteLine();

                i += 1;

            }

            totaltime = DateTime.Now - dt0;

            double srate = (double)passedtests / (double)totaltests * 100;

            Console.WriteLine("Success Rate: " + srate.ToString("N2") + "% (" + passedtests + "/" + totaltests + ")");
            Console.WriteLine("Total Elapsed Time: " + totaltime.ToString());

            if (failed.Count > 0)
            {
                Console.WriteLine();
                Console.WriteLine("Failed Tests:");
                foreach (var s in failed)
                {
                    Console.WriteLine(s);
                }
                Console.WriteLine();
            }

            Console.WriteLine("Press Any Key to Continue...");
            Console.ReadKey();

        }

    }
}
