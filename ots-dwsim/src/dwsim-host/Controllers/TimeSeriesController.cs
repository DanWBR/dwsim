using Microsoft.AspNetCore.Mvc;
using DWSIM.OTS.SimulationHost.Models;
using DWSIM.OTS.SimulationHost.Services;

namespace DWSIM.OTS.SimulationHost.Controllers;

[ApiController]
[Route("api/v1/timeseries")]
[Produces("application/json")]
public class TimeSeriesController : ControllerBase
{
    private readonly ITimeSeriesLogger _timeSeriesLogger;
    private readonly ILogger<TimeSeriesController> _logger;

    public TimeSeriesController(
        ITimeSeriesLogger timeSeriesLogger,
        ILogger<TimeSeriesController> logger)
    {
        _timeSeriesLogger = timeSeriesLogger;
        _logger = logger;
    }

    /// <summary>
    /// Query time-series data for one or more tags
    /// </summary>
    [HttpPost("query")]
    [ProducesResponseType(typeof(List<TimeSeriesQueryResult>), StatusCodes.Status200OK)]
    public async Task<ActionResult<List<TimeSeriesQueryResult>>> Query([FromBody] TimeSeriesQueryRequest request)
    {
        try
        {
            var results = await _timeSeriesLogger.QueryAsync(request);
            return Ok(results);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error querying time-series data");
            return StatusCode(500, new { error = "Internal server error" });
        }
    }

    /// <summary>
    /// Get time-series statistics for a session
    /// </summary>
    [HttpGet("stats/{sessionId}")]
    [ProducesResponseType(typeof(TimeSeriesStats), StatusCodes.Status200OK)]
    public async Task<ActionResult<TimeSeriesStats>> GetStats(string sessionId)
    {
        try
        {
            var stats = await _timeSeriesLogger.GetStatsAsync(sessionId);
            return Ok(stats);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error getting time-series stats for session {SessionId}", sessionId);
            return StatusCode(500, new { error = "Internal server error" });
        }
    }

    /// <summary>
    /// Check if TimescaleDB connection is healthy
    /// </summary>
    [HttpGet("health")]
    [ProducesResponseType(StatusCodes.Status200OK)]
    public async Task<ActionResult> GetHealth()
    {
        var isConnected = await _timeSeriesLogger.IsConnectedAsync();

        return Ok(new
        {
            status = isConnected ? "connected" : "disconnected",
            timestamp = DateTime.UtcNow
        });
    }
}
