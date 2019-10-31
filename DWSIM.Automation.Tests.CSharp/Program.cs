using System;

static class Module1
{

    public static void Main()
    {

        System.IO.Directory.SetCurrentDirectory("C:/Program Files/DWSIM5"); // replace with DWSIM's installation directory on your computer

        //create automation manager
        DWSIM.Automation.Automation interf = new DWSIM.Automation.Automation();

        DWSIM.Interfaces.IFlowsheet sim;

        //load Cavett's Problem simulation file
        sim = interf.LoadFlowsheet("samples" + System.IO.Path.DirectorySeparatorChar + "Cavett's Problem.dwxml");

        //use CAPE-OPEN interfaces to manipulate objects
        CapeOpen.ICapeThermoMaterialObject feed, vap_out, liq_out;

        feed = (CapeOpen.ICapeThermoMaterialObject)sim.GetFlowsheetSimulationObject("2");
        vap_out = (CapeOpen.ICapeThermoMaterialObject)sim.GetFlowsheetSimulationObject("8");
        liq_out = (CapeOpen.ICapeThermoMaterialObject)sim.GetFlowsheetSimulationObject("18");

        //mass flow rate values in kg/s
        double[] flows = new double[4];

        flows[0] = 170.0;
        flows[1] = 180.0;
        flows[2] = 190.0;
        flows[3] = 200.0;

        //vapor and liquid flows
        double vflow = 0;
        double lflow = 0;

        for (var i = 0; i <= flows.Length - 1; i++)
        {
            //set feed mass flow
            feed.SetProp("totalflow", "overall", null, "", "mass", new double[] { flows[i] });
            //calculate the flowsheet (run the simulation)
            Console.WriteLine("Running simulation with F = " + flows[i] + " kg/s, please wait...");
            interf.CalculateFlowsheet(sim, null);
            //check for errors during the last run
            if (sim.Solved == false)
            {
                Console.WriteLine("Error solving flowsheet: " + sim.ErrorMessage);
            }
            //get vapor outlet mass flow value
            vflow = ((double[])vap_out.GetProp("totalflow", "overall", null, "", "mass"))[0];
            //get liquid outlet mass flow value
            lflow = ((double[])liq_out.GetProp("totalflow", "overall", null, "", "mass"))[0];
            //display results
            Console.WriteLine("Simulation run #" + (i + 1) + " results:\nFeed: " + flows[i] + ", Vapor: " + vflow + ", Liquid: " + lflow + " kg/s\nMass balance error: " + (flows[i] - vflow - lflow) + " kg/s");
        }

        Console.WriteLine("Finished OK! Press any key to close.");
        Console.ReadKey();

    }

}