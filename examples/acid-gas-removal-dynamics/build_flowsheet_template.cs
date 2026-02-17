using System;
using System.Collections.Generic;
using System.IO;
using DWSIM.Automation;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.DynamicsManager;
using DWSIM.FlowsheetSolver;

// Template generator for an acid-gas-removal dynamic model.
// This file is intentionally conservative and creates a connected skeleton
// that you can finish/calibrate in the DWSIM UI.

public static class AcidGasRemovalDynamicTemplate
{
    public static void Generate(string outputFile)
    {
        var automation = new Automation3();
        var sim = automation.CreateFlowsheet();

        // Compounds (example acid gas system, adjust as needed)
        foreach (var c in new[] { "Methane", "Ethane", "Propane", "Carbon dioxide", "Hydrogen sulfide", "Water" })
            sim.AddCompound(c);

        // Property package (PR is common for gas/amine pre-calibration workflows)
        sim.CreateAndAddPropertyPackage("Peng-Robinson (PR)");

        // --- Core process blocks (names mapped to the supplied diagram) ---
        var feed = sim.AddObject(ObjectType.MaterialStream, 40, 220, "Feed");
        var saturationMixer = sim.AddObject(ObjectType.Mixer, 120, 220, "Saturation mixer");
        var feedSeparator = sim.AddObject(ObjectType.Vessel, 210, 220, "Feed water separator");
        var absorber = sim.AddObject(ObjectType.AbsorptionColumn, 340, 220, "ABSORBER");
        var richCooler = sim.AddObject(ObjectType.Cooler, 500, 180, "Gas-gas HX");
        var salesSeparator = sim.AddObject(ObjectType.Vessel, 620, 170, "Sales gas water sep");

        var regenerator1 = sim.AddObject(ObjectType.DistillationColumn, 540, 300, "REGENERATOR I");
        var regenerator2 = sim.AddObject(ObjectType.DistillationColumn, 700, 300, "REGENERATOR II");
        var regenerator3 = sim.AddObject(ObjectType.DistillationColumn, 860, 300, "REGENERATOR III");

        var leanPump = sim.AddObject(ObjectType.Pump, 980, 430, "Amine rec. pump");
        var leanSaturator = sim.AddObject(ObjectType.Mixer, 1080, 430, "Rec. amine saturator");

        // Key streams
        var saturatedFeed = sim.AddObject(ObjectType.MaterialStream, 170, 220, "Saturated feed");
        var feedWater = sim.AddObject(ObjectType.MaterialStream, 210, 280, "Feed sep. water");
        var absFeed = sim.AddObject(ObjectType.MaterialStream, 310, 180, "Abs. feed");
        var hotRichGas = sim.AddObject(ObjectType.MaterialStream, 420, 180, "Hot rich gas");
        var coolRichGas = sim.AddObject(ObjectType.MaterialStream, 560, 180, "Cool rich gas");
        var richGasToSales = sim.AddObject(ObjectType.MaterialStream, 660, 170, "Saturated Sales gas");
        var salesGas = sim.AddObject(ObjectType.MaterialStream, 760, 140, "Sales gas");
        var salesWater = sim.AddObject(ObjectType.MaterialStream, 760, 200, "Sales sep. water");

        var richAmine = sim.AddObject(ObjectType.MaterialStream, 420, 255, "Rich amine");
        var iFlashOut = sim.AddObject(ObjectType.MaterialStream, 560, 255, "I Flash out");
        var iiFlashOut = sim.AddObject(ObjectType.MaterialStream, 720, 255, "II Flash out");
        var iiiFlashOut = sim.AddObject(ObjectType.MaterialStream, 890, 255, "III Flash out");
        var acidicGas = sim.AddObject(ObjectType.MaterialStream, 1040, 250, "Acidic gas to compressor");

        var leanAmine = sim.AddObject(ObjectType.MaterialStream, 930, 470, "LEAN AMINE");
        var leanToAbs = sim.AddObject(ObjectType.MaterialStream, 300, 300, "Input lean amine");

        // Position connector geometry before programmatic connect.
        foreach (var o in sim.SimulationObjects.Values)
            ((dynamic)o.GraphicObject).PositionConnectors();

        // Skeleton connections (finish/tune in UI as needed).
        sim.ConnectObjects(feed.GraphicObject, saturationMixer.GraphicObject, 0, 0);
        sim.ConnectObjects(saturationMixer.GraphicObject, saturatedFeed.GraphicObject, 0, 0);
        sim.ConnectObjects(saturatedFeed.GraphicObject, feedSeparator.GraphicObject, 0, 0);
        sim.ConnectObjects(feedSeparator.GraphicObject, absFeed.GraphicObject, 0, 0);
        sim.ConnectObjects(absFeed.GraphicObject, absorber.GraphicObject, 0, 0);
        sim.ConnectObjects(absorber.GraphicObject, hotRichGas.GraphicObject, 0, 0);
        sim.ConnectObjects(hotRichGas.GraphicObject, richCooler.GraphicObject, 0, 0);
        sim.ConnectObjects(richCooler.GraphicObject, coolRichGas.GraphicObject, 0, 0);
        sim.ConnectObjects(coolRichGas.GraphicObject, salesSeparator.GraphicObject, 0, 0);
        sim.ConnectObjects(salesSeparator.GraphicObject, salesGas.GraphicObject, 0, 0);

        sim.ConnectObjects(absorber.GraphicObject, richAmine.GraphicObject, 1, 0);
        sim.ConnectObjects(richAmine.GraphicObject, regenerator1.GraphicObject, 0, 0);
        sim.ConnectObjects(regenerator1.GraphicObject, iFlashOut.GraphicObject, 0, 0);
        sim.ConnectObjects(iFlashOut.GraphicObject, regenerator2.GraphicObject, 0, 0);
        sim.ConnectObjects(regenerator2.GraphicObject, iiFlashOut.GraphicObject, 0, 0);
        sim.ConnectObjects(iiFlashOut.GraphicObject, regenerator3.GraphicObject, 0, 0);
        sim.ConnectObjects(regenerator3.GraphicObject, iiiFlashOut.GraphicObject, 0, 0);
        sim.ConnectObjects(iiiFlashOut.GraphicObject, acidicGas.GraphicObject, 0, 0);

        sim.ConnectObjects(regenerator3.GraphicObject, leanAmine.GraphicObject, 1, 0);
        sim.ConnectObjects(leanAmine.GraphicObject, leanPump.GraphicObject, 0, 0);
        sim.ConnectObjects(leanPump.GraphicObject, leanSaturator.GraphicObject, 0, 0);
        sim.ConnectObjects(leanSaturator.GraphicObject, leanToAbs.GraphicObject, 0, 0);
        sim.ConnectObjects(leanToAbs.GraphicObject, absorber.GraphicObject, 1, 1);

        // Basic feed initialization (SI units)
        ((dynamic)feed).SetTemperature(313.15);
        ((dynamic)feed).SetPressure(3_500_000.0);
        ((dynamic)feed).SetMassFlow(2.0);

        // Dynamic setup: one integrator + one schedule
        sim.DynamicMode = true;

        var integ = new Integrator
        {
            ID = Guid.NewGuid().ToString(),
            Description = "Acid gas baseline integrator",
            IntegrationStep = TimeSpan.FromSeconds(1),
            Duration = TimeSpan.FromHours(1),
            CalculationRateControl = 1,
            CalculationRateEquilibrium = 5,
            CalculationRatePressureFlow = 1,
            RealTime = false,
            RealTimeStepMs = 1000
        };

        var sch = new Schedule
        {
            ID = Guid.NewGuid().ToString(),
            Description = "Baseline dynamic schedule",
            CurrentIntegrator = integ.ID,
            UseCurrentStateAsInitial = true,
            UsesEventList = false,
            UsesCauseAndEffectMatrix = false,
            ResetContentsOfAllObjects = false
        };

        sim.DynamicsManager.IntegratorList.Add(integ.ID, integ);
        sim.DynamicsManager.ScheduleList.Add(sch.ID, sch);
        sim.DynamicsManager.CurrentSchedule = sch.ID;

        // Optional script placeholders - attach in Script Manager using provided .py files.
        var pre = new Script
        {
            ID = Guid.NewGuid().ToString(),
            Title = "Feed Profile (Pre-Step)",
            Linked = true,
            LinkedObjectType = DWSIM.Interfaces.Enums.Scripts.ObjectType.Integrator,
            LinkedEventType = DWSIM.Interfaces.Enums.Scripts.EventType.IntegratorPreStep,
            PythonInterpreter = DWSIM.Interfaces.Enums.Scripts.Interpreter.IronPython,
            ScriptText = File.ReadAllText("integrator_pre_step_feed_profile.py")
        };

        sim.Scripts.Add(pre.ID, pre);

        automation.SaveFlowsheet(sim, outputFile, true);
    }
}
