# Integrator Post-Step script for DWSIM Script Manager
# Link to: Object = Integrator, Event = Integrator Post-Step
# Purpose: print requested acid-gas-removal KPIs every dynamic step.

import System

def get_stream(tag):
    s = Flowsheet.GetFlowsheetSimulationObject(tag)
    if s is None:
        raise Exception("Stream tag '{0}' was not found.".format(tag))
    return s

sales = get_stream("Sales gas")
acid = get_stream("Acidic gas to compressor")
lean = get_stream("LEAN AMINE")

absorber = get_stream("ABSORBER")
reg3 = get_stream("REGENERATOR III")

schedule = Flowsheet.DynamicsManager.ScheduleList[Flowsheet.DynamicsManager.CurrentSchedule]
integrator = Flowsheet.DynamicsManager.IntegratorList[schedule.CurrentIntegrator]
t = (integrator.CurrentTime - System.DateTime()).TotalSeconds

sales_h2s = sales.GetPropertyValue("PROP_MS_106/Hydrogen sulfide")
sales_co2 = sales.GetPropertyValue("PROP_MS_106/Carbon dioxide")

p_top = absorber.GetPropertyValue("PROP_AC_0")
p_bottom = absorber.GetPropertyValue("PROP_AC_1")
absorber_dp = p_bottom - p_top

lean_loading = lean.GetPropertyValue("CO2 Loading")
lean_temp = lean.GetPropertyValue("PROP_MS_0")
acid_flow = acid.GetPropertyValue("PROP_MS_2")
reb_duty = reg3.GetPropertyValue("PROP_DC_6")

msg = (
    "t={0:8.1f}s | Sales y(H2S)={1:.3e}, y(CO2)={2:.3e} | "
    "Abs dP={3:.3f} | AcidGasFlow={4:.3f} | LeanLoading={5:.4f} | "
    "LeanT={6:.2f} | RebDuty={7:.3f}"
).format(t, sales_h2s, sales_co2, absorber_dp, acid_flow, lean_loading, lean_temp, reb_duty)

Flowsheet.ShowMessage(msg, Flowsheet.MessageType.Information)
