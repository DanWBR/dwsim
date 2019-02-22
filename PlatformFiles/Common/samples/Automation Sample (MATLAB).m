NET.addAssembly('DWSIM.Automation.dll');
NET.addAssembly('DWSIM.Interfaces.dll');
NET.addAssembly('CapeOpen.dll');

%create automation manager
interf = DWSIM.Automation.Automation();
sim = interf.LoadFlowsheet("samples\Cavett's Problem.dwxml");

%use CAPE-OPEN interfaces to manipulate objects
feed = sim.GetFlowsheetSimulationObject("2");
vap_out = sim.GetFlowsheetSimulationObject("8");
liq_out = sim.GetFlowsheetSimulationObject("18");

% mass flow rate values in kg/s
flows = zeros(4,1);
flowData = zeros(4,1);
flows(1) = 170.0;
flows(2) = 180.0;
flows(3) = 190.0;
flows(4) = 200.0;

for i = 1:4
    %set feed mass flow
    flowData(1) = flows(i);
    feed.SetProp("totalflow", "overall", [], "", "mass", flowData);
    %calculate the flowsheet (run the simulation)
    fprintf("\nRunning simulation with F = %6.2f kg/s, please wait...\n", flows(i));
    interf.CalculateFlowsheet(sim, []);
    %check for errors during the last run
    if ~sim.Solved
        fprintf("Error solving flowsheet: %s\n", sim.ErrorMessage);
    end           
    %get vapor outlet mass flow value
    vflow = vap_out.GetProp("totalflow", "overall", [], "", "mass");
    %get liquid outlet mass flow value
    lflow = liq_out.GetProp("totalflow", "overall", [], "", "mass");
    %display results
    fprintf("Simulation run # %i results:\nFeed: %6.2f ,Vapor: %6.2f, Liquid: %6.2f kg/s\nMass balance error: %6.2f kg/s\n",...
        i, flows(i), vflow(1), lflow(1), (flows(i) - vflow(1) - lflow(1)));
end