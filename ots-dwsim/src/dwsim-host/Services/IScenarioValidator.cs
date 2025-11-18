using DWSIM.OTS.SimulationHost.Models.Scenario;

namespace DWSIM.OTS.SimulationHost.Services;

public interface IScenarioValidator
{
    Task<ScenarioValidationResult> ValidateAsync(ScenarioDefinition scenario);
    Task<ScenarioValidationResult> ValidateFromFileAsync(string filePath);
    Task<ScenarioValidationResult> ValidateJsonAsync(string json);
}
