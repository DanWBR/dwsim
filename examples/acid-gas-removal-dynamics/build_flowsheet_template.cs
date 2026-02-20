using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using DWSIM.Automation;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.DynamicsManager;
using DWSIM.FlowsheetSolver;

// Acid gas removal dynamic template with amine-ready defaults and KPI monitoring.

public static class AcidGasRemovalDynamicTemplate
{
    public static void Generate(string outputFile)
    {
        var automation = new Automation3();
        var sim = automation.CreateFlowsheet();

        // Gas-side compounds + optional heavies.
        AddCompoundOrThrow(sim, "Methane");
        AddCompoundOrThrow(sim, "Carbon dioxide");
        AddCompoundOrThrow(sim, "Hydrogen sulfide");
        AddCompoundOrThrow(sim, "Water");
        AddCompoundIfAvailable(sim, "Nitrogen");
        AddCompoundIfAvailable(sim, "Ethane");
        AddCompoundIfAvailable(sim, "Propane");

        // Amine package compounds: pick one amine at minimum (prefer MDEA, then MEA, then DEA).
        var amineAdded = AddFirstAvailableCompound(sim, new[]
        {
            "Methyl diethanolamine",
            "Monoethanolamine",
            "Diethanolamine"
        });
        if (!amineAdded)
            throw new Exception("Could not add any amine compound (MDEA/MEA/DEA). Please verify your component database.");

        // Thermodynamic package selection for amine systems.
        var ppName = SelectAminePropertyPackage(sim);
        sim.CreateAndAddPropertyPackage(ppName);

        // --- Core process blocks ---
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
        var absFeed = sim.AddObject(ObjectType.MaterialStream, 310, 180, "Abs. feed");
        var hotRichGas = sim.AddObject(ObjectType.MaterialStream, 420, 180, "Hot rich gas");
        var coolRichGas = sim.AddObject(ObjectType.MaterialStream, 560, 180, "Cool rich gas");
        var salesGas = sim.AddObject(ObjectType.MaterialStream, 760, 140, "Sales gas");

        var richAmine = sim.AddObject(ObjectType.MaterialStream, 420, 255, "Rich amine");
        var iFlashOut = sim.AddObject(ObjectType.MaterialStream, 560, 255, "I Flash out");
        var iiFlashOut = sim.AddObject(ObjectType.MaterialStream, 720, 255, "II Flash out");
        var iiiFlashOut = sim.AddObject(ObjectType.MaterialStream, 890, 255, "III Flash out");
        var acidicGas = sim.AddObject(ObjectType.MaterialStream, 1040, 250, "Acidic gas to compressor");

        var leanAmine = sim.AddObject(ObjectType.MaterialStream, 930, 470, "LEAN AMINE");
        var leanToAbs = sim.AddObject(ObjectType.MaterialStream, 300, 300, "Input lean amine");

        foreach (var o in sim.SimulationObjects.Values)
            ((dynamic)o.GraphicObject).PositionConnectors();

        // Skeleton connectivity.
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

        // Baseline feed specs (SI). Equivalent of P/T/flow sanity check.
        ((dynamic)feed).SetTemperature(313.15);
        ((dynamic)feed).SetPressure(3_500_000.0);
        ((dynamic)feed).SetMassFlow(2.0);

        // Dynamic setup: one integrator + one schedule.
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

        // Preconfigure monitored variables for requested KPIs.
        ConfigureKpiMonitors(sim, integ,
            feed, salesGas, absorber, hotRichGas, richAmine, acidicGas, leanAmine, regenerator3);

        sim.DynamicsManager.IntegratorList.Add(integ.ID, integ);
        sim.DynamicsManager.ScheduleList.Add(sch.ID, sch);
        sim.DynamicsManager.CurrentSchedule = sch.ID;

        // Attach script manager scripts (pre-step and post-step).
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

        var post = new Script
        {
            ID = Guid.NewGuid().ToString(),
            Title = "KPI Logger (Post-Step)",
            Linked = true,
            LinkedObjectType = DWSIM.Interfaces.Enums.Scripts.ObjectType.Integrator,
            LinkedEventType = DWSIM.Interfaces.Enums.Scripts.EventType.IntegratorStep,
            PythonInterpreter = DWSIM.Interfaces.Enums.Scripts.Interpreter.IronPython,
            ScriptText = File.ReadAllText("integrator_post_step_kpi_logger.py")
        };

        sim.Scripts.Add(pre.ID, pre);
        sim.Scripts.Add(post.ID, post);

        automation.SaveFlowsheet(sim, outputFile, true);
    }

    private static void ConfigureKpiMonitors(
        IFlowsheet sim, Integrator integ,
        dynamic feed, dynamic salesGas, dynamic absorber, dynamic absorberTopGas, dynamic absorberBottomLiquid,
        dynamic acidGas, dynamic leanAmine, dynamic regenerator)
    {
        // Sales gas H2S/CO2
        AddMonitoredVariable(integ, salesGas, "PROP_MS_106/Hydrogen sulfide", "Sales gas H2S mole fraction");
        AddMonitoredVariable(integ, salesGas, "PROP_MS_106/Carbon dioxide", "Sales gas CO2 mole fraction");

        // Absorber ΔP (represented by top and bottom pressures for direct difference).
        AddMonitoredVariable(integ, absorber, "PROP_AC_0", "Absorber top pressure");
        AddMonitoredVariable(integ, absorber, "PROP_AC_1", "Absorber bottom pressure");

        // Absorber top/bottom compositions.
        AddMonitoredVariable(integ, absorberTopGas, "PROP_MS_106/Hydrogen sulfide", "Absorber top gas H2S mole fraction");
        AddMonitoredVariable(integ, absorberTopGas, "PROP_MS_106/Carbon dioxide", "Absorber top gas CO2 mole fraction");
        AddMonitoredVariable(integ, absorberBottomLiquid, "PROP_MS_102/Hydrogen sulfide", "Absorber bottom liquid H2S mole fraction");
        AddMonitoredVariable(integ, absorberBottomLiquid, "PROP_MS_102/Carbon dioxide", "Absorber bottom liquid CO2 mole fraction");

        // Regenerator overhead acid gas flow.
        AddMonitoredVariable(integ, acidGas, "PROP_MS_2", "Regenerator overhead acid gas mass flow");

        // Lean amine loading / temperature.
        AddMonitoredVariable(integ, leanAmine, "CO2 Loading", "Lean amine CO2 loading");
        AddMonitoredVariable(integ, leanAmine, "PROP_MS_0", "Lean amine temperature");

        // Reboiler duty (using third regenerator block in this template).
        AddMonitoredVariable(integ, regenerator, "PROP_DC_6", "Regenerator III reboiler duty");

        // Keep feed flow monitored as operation sanity signal.
        AddMonitoredVariable(integ, feed, "PROP_MS_2", "Feed mass flow");
    }

    private static void AddMonitoredVariable(Integrator integ, dynamic obj, string propertyId, string description)
    {
        string[] props = obj.GetProperties(PropertyType.ALL);
        if (!props.Contains(propertyId))
        {
            throw new Exception($"Required KPI property '{propertyId}' not available on object '{obj.GraphicObject.Tag}'.");
        }

        var mv = new MonitoredVariable
        {
            ID = Guid.NewGuid().ToString(),
            Description = description,
            ObjectID = obj.Name,
            PropertyID = propertyId,
            PropertyUnits = obj.GetPropertyUnit(propertyId) ?? ""
        };

        integ.MonitoredVariables.Add(mv);
    }

    private static string SelectAminePropertyPackage(IFlowsheet sim)
    {
        var pps = sim.GetAvailablePropertyPackages().ToList();

        string Match(params string[] terms)
        {
            return pps.FirstOrDefault(pp =>
            {
                var l = pp.ToLowerInvariant();
                return terms.All(t => l.Contains(t));
            });
        }

        // Prefer amine-specific package, then electrolyte methods, then fallback.
        var preferred =
            Match("amines") ??
            Match("electrolyte", "nrtl") ??
            Match("electrolyte") ??
            Match("peng", "robinson") ??
            pps.FirstOrDefault();

        if (string.IsNullOrWhiteSpace(preferred))
            throw new Exception("No property package is available in this DWSIM installation.");

        return preferred;
    }

    private static void AddCompoundOrThrow(IFlowsheet sim, string name)
    {
        try
        {
            sim.AddCompound(name);
        }
        catch (Exception ex)
        {
            throw new Exception($"Required compound '{name}' could not be added.", ex);
        }
    }

    private static bool AddCompoundIfAvailable(IFlowsheet sim, string name)
    {
        try
        {
            sim.AddCompound(name);
            return true;
        }
        catch
        {
            return false;
        }
    }

    private static bool AddFirstAvailableCompound(IFlowsheet sim, IEnumerable<string> names)
    {
        foreach (var n in names)
        {
            if (AddCompoundIfAvailable(sim, n)) return true;
        }
        return false;
    }
}
