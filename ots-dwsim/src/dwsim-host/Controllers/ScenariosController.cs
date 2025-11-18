using Microsoft.AspNetCore.Mvc;
using DWSIM.OTS.SimulationHost.Models.Scenario;
using DWSIM.OTS.SimulationHost.Services;

namespace DWSIM.OTS.SimulationHost.Controllers;

[ApiController]
[Route("api/v1/scenarios")]
[Produces("application/json")]
public class ScenariosController : ControllerBase
{
    private readonly IScenarioExecutor _scenarioExecutor;
    private readonly IScenarioValidator _scenarioValidator;
    private readonly ILogger<ScenariosController> _logger;

    public ScenariosController(
        IScenarioExecutor scenarioExecutor,
        IScenarioValidator scenarioValidator,
        ILogger<ScenariosController> logger)
    {
        _scenarioExecutor = scenarioExecutor;
        _scenarioValidator = scenarioValidator;
        _logger = logger;
    }

    /// <summary>
    /// Validate a scenario file
    /// </summary>
    [HttpPost("validate")]
    [ProducesResponseType(typeof(ScenarioValidationResult), StatusCodes.Status200OK)]
    public async Task<ActionResult<ScenarioValidationResult>> ValidateScenario([FromBody] ScenarioDefinition scenario)
    {
        var result = await _scenarioValidator.ValidateAsync(scenario);
        return Ok(result);
    }

    /// <summary>
    /// Validate a scenario from file path
    /// </summary>
    [HttpPost("validate/file")]
    [ProducesResponseType(typeof(ScenarioValidationResult), StatusCodes.Status200OK)]
    public async Task<ActionResult<ScenarioValidationResult>> ValidateScenarioFile([FromBody] ValidateFileRequest request)
    {
        var result = await _scenarioValidator.ValidateFromFileAsync(request.FilePath);
        return Ok(result);
    }

    /// <summary>
    /// Load a scenario definition
    /// </summary>
    [HttpGet("{scenarioId}")]
    [ProducesResponseType(typeof(ScenarioDefinition), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<ScenarioDefinition>> GetScenario(string scenarioId)
    {
        try
        {
            var scenario = await _scenarioExecutor.LoadScenarioAsync(scenarioId);
            return Ok(scenario);
        }
        catch (FileNotFoundException ex)
        {
            return NotFound(new { error = ex.Message });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error loading scenario {ScenarioId}", scenarioId);
            return StatusCode(500, new { error = "Internal server error" });
        }
    }

    /// <summary>
    /// Get scenario run status
    /// </summary>
    [HttpGet("runs/{scenarioRunId}")]
    [ProducesResponseType(typeof(ScenarioRun), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<ScenarioRun>> GetScenarioRun(string scenarioRunId)
    {
        var run = await _scenarioExecutor.GetScenarioRunAsync(scenarioRunId);

        if (run == null)
        {
            return NotFound(new { error = $"Scenario run not found: {scenarioRunId}" });
        }

        return Ok(run);
    }

    /// <summary>
    /// Get all active scenario runs
    /// </summary>
    [HttpGet("runs")]
    [ProducesResponseType(typeof(List<ScenarioRun>), StatusCodes.Status200OK)]
    public async Task<ActionResult<List<ScenarioRun>>> GetActiveRuns()
    {
        var runs = await _scenarioExecutor.GetActiveScenarioRunsAsync();
        return Ok(runs);
    }

    /// <summary>
    /// Stop a running scenario
    /// </summary>
    [HttpPost("runs/{scenarioRunId}/stop")]
    [ProducesResponseType(StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult> StopScenarioRun(string scenarioRunId)
    {
        try
        {
            await _scenarioExecutor.StopScenarioAsync(scenarioRunId);
            return Ok(new { status = "stopped" });
        }
        catch (KeyNotFoundException ex)
        {
            return NotFound(new { error = ex.Message });
        }
    }
}

public class ValidateFileRequest
{
    public required string FilePath { get; set; }
}
