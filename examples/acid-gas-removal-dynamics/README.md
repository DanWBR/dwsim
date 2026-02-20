# Acid Gas Removal - Dynamic Simulation Delivery Pack

> Reissued as a fresh PR bundle with the same corrected amine-thermo and KPI-monitoring behavior.

This pack generates an acid-gas-removal dynamic model with:
- amine-capable component set,
- amine-priority thermodynamic package selection,
- preconfigured dynamic schedule/integrator,
- preconfigured monitored variables for the requested KPIs,
- prelinked Script Manager Python scripts for disturbance + KPI logging.

## Delivered items

- `build_flowsheet_template.cs`  
  Automation template that creates the flowsheet skeleton, sets dynamic configuration, and injects KPI monitors/scripts.
- `integrator_pre_step_feed_profile.py`  
  Integrator Pre-Step feed-flow profile script.
- `integrator_post_step_kpi_logger.py`  
  Integrator Post-Step KPI logger script.

## What is now preconfigured

### 1) Components list
The generator always adds:
- Methane, Carbon dioxide, Hydrogen sulfide, Water
- at least one amine from: **Methyl diethanolamine**, **Monoethanolamine**, **Diethanolamine**
- optional if available: Nitrogen, Ethane, Propane

### 2) Thermodynamic package
The generator auto-selects with this priority:
1. package name containing `Amines`
2. package containing `Electrolyte` + `NRTL`
3. any package containing `Electrolyte`
4. Peng-Robinson fallback

### 3) Feed specs
Template initializes feed at:
- `T = 313.15 K`
- `P = 3.5e6 Pa`
- `Mass flow = 2.0 kg/s`

### 4) Dynamic setup
- Dynamic mode enabled
- Schedule: `Baseline dynamic schedule`
- Integrator: `Acid gas baseline integrator`
- `dt = 1 s`, duration `1 h`

### 5) Preconfigured monitored variables (requested KPIs)
- Sales gas H2S mole fraction
- Sales gas CO2 mole fraction
- Absorber top pressure
- Absorber bottom pressure (for ΔP = bottom - top)
- Absorber top gas H2S mole fraction
- Absorber top gas CO2 mole fraction
- Absorber bottom liquid H2S mole fraction
- Absorber bottom liquid CO2 mole fraction
- Regenerator overhead acid gas mass flow
- Lean amine CO2 loading
- Lean amine temperature
- Regenerator III reboiler duty
- Feed mass flow (sanity signal)

## How to generate the simulation file

Compile/run in an environment with DWSIM assemblies and call:

```csharp
AcidGasRemovalDynamicTemplate.Generate("acid-gas-removal-dynamics.dwxmz");
```

## How to run

1. Open generated `.dwxmz` in DWSIM.
2. Check Thermodynamics page for selected components + package.
3. Solve steady-state first.
4. Enable Dynamic Mode.
5. Run Integrator Controls.

## Scripts attached by generator

- `Feed Profile (Pre-Step)` linked to Integrator Pre-Step.
- `KPI Logger (Post-Step)` linked to Integrator Post-Step.

The logger prints the KPI set each step, including computed absorber ΔP.
