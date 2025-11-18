using DWSIM.OTS.SimulationHost.Models.Scenario;

namespace DWSIM.OTS.SimulationHost.Services;

public interface IScenarioExecutor
{
    Task<ScenarioDefinition> LoadScenarioAsync(string scenarioPath);
    Task<string> RunScenarioAsync(string sessionId, string scenarioId, bool autostart = true);
    Task StopScenarioAsync(string scenarioRunId);
    Task<ScenarioRun?> GetScenarioRunAsync(string scenarioRunId);
    Task<List<ScenarioRun>> GetActiveScenarioRunsAsync();
}
