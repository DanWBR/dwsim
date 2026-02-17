# Integrator Post-Step script for DWSIM Script Manager
# Link to: Object = Integrator, Event = Integrator Post-Step
# Purpose: print simple KPIs each dynamic step.

import System

def get_stream(tag):
    s = Flowsheet.GetFlowsheetSimulationObject(tag)
    if s is None:
        raise Exception("Stream tag '{0}' was not found.".format(tag))
    return s

feed = get_stream("Feed")
product = get_stream("Sales gas")
acid_to_compressor = get_stream("Acidic gas to compressor")

schedule = Flowsheet.DynamicsManager.ScheduleList[Flowsheet.DynamicsManager.CurrentSchedule]
integrator = Flowsheet.DynamicsManager.IntegratorList[schedule.CurrentIntegrator]
t = (integrator.CurrentTime - System.DateTime()).TotalSeconds

msg = (
    "t={0:8.1f}s | Feed={1:8.3f} kg/s | Sales={2:8.3f} kg/s | AcidGas={3:8.3f} kg/s"
    .format(t, feed.GetMassFlow(), product.GetMassFlow(), acid_to_compressor.GetMassFlow())
)

Flowsheet.ShowMessage(msg, Flowsheet.MessageType.Information)
