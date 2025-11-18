using DWSIM.OTS.SimulationHost.Models.Scenario;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using Newtonsoft.Json.Schema;

namespace DWSIM.OTS.SimulationHost.Services;

public class ScenarioValidator : IScenarioValidator
{
    private readonly ILogger<ScenarioValidator> _logger;
    private readonly string _schemaPath;
    private JSchema? _schema;

    public ScenarioValidator(ILogger<ScenarioValidator> logger, IConfiguration configuration)
    {
        _logger = logger;
        _schemaPath = configuration["ScenarioSchemaPath"]
            ?? Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "schemas", "scenario.schema.json");
    }

    public async Task<ScenarioValidationResult> ValidateAsync(ScenarioDefinition scenario)
    {
        try
        {
            var json = JsonConvert.SerializeObject(scenario);
            return await ValidateJsonAsync(json);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error validating scenario");
            return new ScenarioValidationResult
            {
                IsValid = false,
                Errors = new List<string> { $"Validation error: {ex.Message}" }
            };
        }
    }

    public async Task<ScenarioValidationResult> ValidateFromFileAsync(string filePath)
    {
        try
        {
            if (!File.Exists(filePath))
            {
                return new ScenarioValidationResult
                {
                    IsValid = false,
                    Errors = new List<string> { $"Scenario file not found: {filePath}" }
                };
            }

            var json = await File.ReadAllTextAsync(filePath);
            return await ValidateJsonAsync(json);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error validating scenario from file {FilePath}", filePath);
            return new ScenarioValidationResult
            {
                IsValid = false,
                Errors = new List<string> { $"Error reading scenario file: {ex.Message}" }
            };
        }
    }

    public async Task<ScenarioValidationResult> ValidateJsonAsync(string json)
    {
        var result = new ScenarioValidationResult { IsValid = true };

        try
        {
            // Load schema if not already loaded
            if (_schema == null)
            {
                await LoadSchemaAsync();
            }

            if (_schema == null)
            {
                result.IsValid = false;
                result.Errors.Add("Failed to load scenario schema");
                return result;
            }

            // Parse JSON
            JObject scenarioJson;
            try
            {
                scenarioJson = JObject.Parse(json);
            }
            catch (JsonException ex)
            {
                result.IsValid = false;
                result.Errors.Add($"Invalid JSON: {ex.Message}");
                return result;
            }

            // Validate against schema
            IList<string> validationErrors;
            bool isValid = scenarioJson.IsValid(_schema, out validationErrors);

            if (!isValid)
            {
                result.IsValid = false;
                result.Errors.AddRange(validationErrors);
                _logger.LogWarning("Scenario validation failed with {ErrorCount} errors", validationErrors.Count);
            }

            // Additional semantic validation
            var semanticValidation = ValidateSemantics(scenarioJson);
            result.Warnings.AddRange(semanticValidation.Warnings);

            if (!semanticValidation.IsValid)
            {
                result.IsValid = false;
                result.Errors.AddRange(semanticValidation.Errors);
            }

            if (result.IsValid)
            {
                _logger.LogInformation("Scenario validation succeeded");
            }
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Unexpected error during scenario validation");
            result.IsValid = false;
            result.Errors.Add($"Validation error: {ex.Message}");
        }

        return result;
    }

    private async Task LoadSchemaAsync()
    {
        try
        {
            if (!File.Exists(_schemaPath))
            {
                _logger.LogWarning("Scenario schema not found at {Path}", _schemaPath);
                return;
            }

            var schemaJson = await File.ReadAllTextAsync(_schemaPath);
            _schema = JSchema.Parse(schemaJson);
            _logger.LogInformation("Loaded scenario schema from {Path}", _schemaPath);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error loading scenario schema");
        }
    }

    private ScenarioValidationResult ValidateSemantics(JObject scenario)
    {
        var result = new ScenarioValidationResult { IsValid = true };

        try
        {
            // Check events are time-ordered
            var events = scenario["events"] as JArray;
            if (events != null && events.Count > 1)
            {
                double previousTime = -1;
                for (int i = 0; i < events.Count; i++)
                {
                    var evt = events[i] as JObject;
                    var timeS = evt?["time_s"]?.Value<double>() ?? 0;

                    if (timeS < previousTime)
                    {
                        result.Warnings.Add($"Event {i} at time {timeS}s is not in chronological order");
                    }

                    previousTime = timeS;
                }
            }

            // Check for duplicate scenario_id if we have access to repository
            // (would require additional dependency injection)

            // Validate seed is reasonable
            var seed = scenario["seed"]?.Value<int>() ?? 0;
            if (seed < 0)
            {
                result.Errors.Add("Seed must be non-negative");
                result.IsValid = false;
            }

            // Check required_flowsheet exists if specified
            var requiredFlowsheet = scenario["metadata"]?["required_flowsheet"]?.Value<string>();
            if (!string.IsNullOrEmpty(requiredFlowsheet))
            {
                // TODO: Check if flowsheet exists in repository
                _logger.LogDebug("Required flowsheet: {Flowsheet}", requiredFlowsheet);
            }

            // Validate event types
            if (events != null)
            {
                var validTypes = new HashSet<string> { "fault", "set", "controller", "note", "random_fault", "alarm", "operator_prompt" };

                for (int i = 0; i < events.Count; i++)
                {
                    var evt = events[i] as JObject;
                    var type = evt?["type"]?.Value<string>();

                    if (type != null && !validTypes.Contains(type))
                    {
                        result.Warnings.Add($"Event {i} has unknown type '{type}'");
                    }
                }
            }
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error in semantic validation");
            result.Warnings.Add($"Semantic validation incomplete: {ex.Message}");
        }

        return result;
    }
}
