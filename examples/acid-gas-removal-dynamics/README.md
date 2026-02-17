# Acid Gas Removal - Dynamic Simulation Delivery Pack

This folder contains a **ready-to-use delivery pack** for building and running an acid gas removal dynamic simulation in DWSIM.

## Delivered items

- `build_flowsheet_template.cs`  
  Automation template that creates the main blocks/connections, configures one dynamic schedule/integrator, and saves a `.dwxmz` simulation file.
- `integrator_pre_step_feed_profile.py`  
  Script Manager script for feed-flow disturbances (Integrator Pre-Step).
- `integrator_post_step_kpi_logger.py`  
  Script Manager script to print KPIs every dynamic step (Integrator Post-Step).

## How to generate the simulation file

1. Compile/run the automation template in an environment with DWSIM assemblies available.
2. Call:

```csharp
AcidGasRemovalDynamicTemplate.Generate("acid-gas-removal-dynamics.dwxmz");
```

3. Open the generated `acid-gas-removal-dynamics.dwxmz` in DWSIM.

> Notes:
> - The template uses conservative default values to create a robust starting model.
> - You should calibrate thermodynamics, column internals, and stream specs for your exact case.

## How to run dynamic simulation

1. Open **Script Manager** and confirm the `Feed Profile (Pre-Step)` script is present.
2. (Optional) add `integrator_post_step_kpi_logger.py` as **Integrator Post-Step** script.
3. Open **Dynamics Manager** and verify:
   - Schedule = `Baseline dynamic schedule`
   - Integrator = `Acid gas baseline integrator`
   - `dt = 1 s`, duration = `1 h`
4. Solve steady-state first.
5. Enable **Dynamic Mode**.
6. Run **Integrator Controls -> Play**.

## Expected outputs

- Time-series values in integrator monitored variables.
- Script log messages with step KPIs.
- Dynamic state snapshots/history in the flowsheet session.

## Recommended next refinements

- Replace placeholder values with plant data.
- Add monitored variables for:
  - absorber top/bottom composition,
  - regenerator overhead acid gas flow,
  - lean amine circulation rate,
  - sales gas H2S/CO2 specs.
- Add PID/Python controllers for lean amine flow and regenerator duty.
