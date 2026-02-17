# Integrator Pre-Step script for DWSIM Script Manager
# Link to: Object = Integrator, Event = Integrator Pre-Step
# Purpose: apply a time-varying feed mass flow profile in dynamic simulation.

import System

FEED_TAG = "Feed"       # update to your feed stream tag in the flowsheet
BASE_FLOW = 2.0          # kg/s
RAMP_SLOPE = 0.005       # kg/s^2
STEP_TIME = 600.0        # s
STEP_DELTA = 0.5         # kg/s

stream = Flowsheet.GetFlowsheetSimulationObject(FEED_TAG)
if stream is None:
    raise Exception("Stream tag '{0}' was not found.".format(FEED_TAG))

schedule = Flowsheet.DynamicsManager.ScheduleList[Flowsheet.DynamicsManager.CurrentSchedule]
integrator = Flowsheet.DynamicsManager.IntegratorList[schedule.CurrentIntegrator]

# DateTime starts at 0001-01-01 in this context; use elapsed seconds from zero date.
t = (integrator.CurrentTime - System.DateTime()).TotalSeconds

# Example combined profile: ramp + delayed step.
new_flow = BASE_FLOW + RAMP_SLOPE * t
if t >= STEP_TIME:
    new_flow += STEP_DELTA

if new_flow < 0.0:
    new_flow = 0.0

stream.SetMassFlow(new_flow)

# Do not call RequestCalculation() here.
# The integrator loop will solve the flowsheet right after Pre-Step.
