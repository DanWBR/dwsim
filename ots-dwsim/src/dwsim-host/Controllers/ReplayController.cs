using Microsoft.AspNetCore.Mvc;
using DWSIM.OTS.SimulationHost.Models;
using DWSIM.OTS.SimulationHost.Services;

namespace DWSIM.OTS.SimulationHost.Controllers;

[ApiController]
[Route("api/v1/replay")]
[Produces("application/json")]
public class ReplayController : ControllerBase
{
    private readonly IReplayEngine _replayEngine;
    private readonly ILogger<ReplayController> _logger;

    public ReplayController(
        IReplayEngine replayEngine,
        ILogger<ReplayController> logger)
    {
        _replayEngine = replayEngine;
        _logger = logger;
    }

    /// <summary>
    /// Start a replay of a recorded session
    /// </summary>
    /// <param name="request">Replay request with original session ID and parameters</param>
    /// <returns>Replay response with replay ID and initial status</returns>
    [HttpPost]
    [ProducesResponseType(typeof(ReplayResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<ReplayResponse>> StartReplay([FromBody] ReplayRequest request)
    {
        try
        {
            if (string.IsNullOrEmpty(request.OriginalSessionId))
            {
                return BadRequest(new { error = "OriginalSessionId is required" });
            }

            if (request.SpeedMultiplier <= 0)
            {
                return BadRequest(new { error = "SpeedMultiplier must be greater than 0" });
            }

            var response = await _replayEngine.StartReplayAsync(request);
            return Ok(response);
        }
        catch (InvalidOperationException ex)
        {
            _logger.LogWarning(ex, "Invalid replay request");
            return BadRequest(new { error = ex.Message });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error starting replay");
            return StatusCode(500, new { error = "Internal server error" });
        }
    }

    /// <summary>
    /// Get status of a replay
    /// </summary>
    /// <param name="replayId">Replay ID</param>
    /// <returns>Current replay status and progress</returns>
    [HttpGet("{replayId}")]
    [ProducesResponseType(typeof(ReplayResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<ReplayResponse>> GetReplayStatus(string replayId)
    {
        var response = await _replayEngine.GetReplayStatusAsync(replayId);

        if (response == null)
        {
            return NotFound(new { error = $"Replay {replayId} not found" });
        }

        return Ok(response);
    }

    /// <summary>
    /// Get comparison results between original and replayed session
    /// </summary>
    /// <param name="replayId">Replay ID</param>
    /// <returns>Detailed comparison with determinism verification</returns>
    [HttpGet("{replayId}/comparison")]
    [ProducesResponseType(typeof(ReplayComparison), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<ReplayComparison>> GetComparison(string replayId)
    {
        var comparison = await _replayEngine.GetComparisonAsync(replayId);

        if (comparison == null)
        {
            return NotFound(new { error = $"Comparison for replay {replayId} not found or not yet generated" });
        }

        return Ok(comparison);
    }

    /// <summary>
    /// Cancel an in-progress replay
    /// </summary>
    /// <param name="replayId">Replay ID to cancel</param>
    /// <returns>Success status</returns>
    [HttpPost("{replayId}/cancel")]
    [ProducesResponseType(StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult> CancelReplay(string replayId)
    {
        var cancelled = await _replayEngine.CancelReplayAsync(replayId);

        if (!cancelled)
        {
            return NotFound(new { error = $"Replay {replayId} not found" });
        }

        return Ok(new { message = "Replay cancelled", replayId });
    }

    /// <summary>
    /// Get all replays for a specific session
    /// </summary>
    /// <param name="sessionId">Original session ID</param>
    /// <returns>List of replays</returns>
    [HttpGet("session/{sessionId}")]
    [ProducesResponseType(typeof(List<ReplayResponse>), StatusCodes.Status200OK)]
    public async Task<ActionResult<List<ReplayResponse>>> GetReplaysForSession(string sessionId)
    {
        var replays = await _replayEngine.GetReplaysForSessionAsync(sessionId);
        return Ok(replays);
    }
}
