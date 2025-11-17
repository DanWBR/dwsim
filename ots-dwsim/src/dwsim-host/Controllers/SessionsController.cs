using Microsoft.AspNetCore.Mvc;
using DWSIM.OTS.SimulationHost.Models;
using DWSIM.OTS.SimulationHost.Services;

namespace DWSIM.OTS.SimulationHost.Controllers;

[ApiController]
[Route("api/v1/sessions")]
[Produces("application/json")]
public class SessionsController : ControllerBase
{
    private readonly ISessionManager _sessionManager;
    private readonly ILogger<SessionsController> _logger;

    public SessionsController(ISessionManager sessionManager, ILogger<SessionsController> logger)
    {
        _sessionManager = sessionManager;
        _logger = logger;
    }

    /// <summary>
    /// Create a new simulation session
    /// </summary>
    [HttpPost]
    [ProducesResponseType(typeof(CreateSessionResponse), StatusCodes.Status201Created)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    public async Task<ActionResult<CreateSessionResponse>> CreateSession([FromBody] CreateSessionRequest request)
    {
        try
        {
            var response = await _sessionManager.CreateSessionAsync(request);
            return CreatedAtAction(nameof(GetSession), new { sessionId = response.SessionId }, response);
        }
        catch (FileNotFoundException ex)
        {
            _logger.LogWarning(ex, "Flowsheet not found: {Flowsheet}", request.Flowsheet);
            return BadRequest(new { error = ex.Message });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error creating session");
            return StatusCode(500, new { error = "Internal server error" });
        }
    }

    /// <summary>
    /// Get session information
    /// </summary>
    [HttpGet("{sessionId}")]
    [ProducesResponseType(typeof(SessionInfo), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<SessionInfo>> GetSession(string sessionId)
    {
        var session = await _sessionManager.GetSessionAsync(sessionId);

        if (session == null)
        {
            return NotFound(new { error = $"Session not found: {sessionId}" });
        }

        return Ok(session);
    }

    /// <summary>
    /// Get all sessions
    /// </summary>
    [HttpGet]
    [ProducesResponseType(typeof(List<SessionInfo>), StatusCodes.Status200OK)]
    public async Task<ActionResult<List<SessionInfo>>> GetAllSessions()
    {
        var sessions = await _sessionManager.GetAllSessionsAsync();
        return Ok(sessions);
    }

    /// <summary>
    /// Start a session
    /// </summary>
    [HttpPost("{sessionId}/start")]
    [ProducesResponseType(typeof(StartSessionResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<StartSessionResponse>> StartSession(string sessionId, [FromBody] StartSessionRequest request)
    {
        try
        {
            var response = await _sessionManager.StartSessionAsync(sessionId, request);
            return Ok(response);
        }
        catch (KeyNotFoundException)
        {
            return NotFound(new { error = $"Session not found: {sessionId}" });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error starting session {SessionId}", sessionId);
            return StatusCode(500, new { error = "Internal server error" });
        }
    }

    /// <summary>
    /// Pause a running session
    /// </summary>
    [HttpPost("{sessionId}/pause")]
    [ProducesResponseType(typeof(SessionInfo), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<SessionInfo>> PauseSession(string sessionId)
    {
        try
        {
            var session = await _sessionManager.PauseSessionAsync(sessionId);
            return Ok(session);
        }
        catch (KeyNotFoundException)
        {
            return NotFound(new { error = $"Session not found: {sessionId}" });
        }
    }

    /// <summary>
    /// Stop and finalize a session
    /// </summary>
    [HttpPost("{sessionId}/stop")]
    [ProducesResponseType(typeof(SessionInfo), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<SessionInfo>> StopSession(string sessionId)
    {
        try
        {
            var session = await _sessionManager.StopSessionAsync(sessionId);
            return Ok(session);
        }
        catch (KeyNotFoundException)
        {
            return NotFound(new { error = $"Session not found: {sessionId}" });
        }
    }

    /// <summary>
    /// Create a snapshot of the current session state
    /// </summary>
    [HttpPost("{sessionId}/snapshot")]
    [ProducesResponseType(typeof(SnapshotResponse), StatusCodes.Status201Created)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<SnapshotResponse>> CreateSnapshot(string sessionId, [FromBody] SnapshotRequest request)
    {
        try
        {
            var response = await _sessionManager.CreateSnapshotAsync(sessionId, request);
            return CreatedAtAction(nameof(GetSession), new { sessionId }, response);
        }
        catch (KeyNotFoundException)
        {
            return NotFound(new { error = $"Session not found: {sessionId}" });
        }
    }

    /// <summary>
    /// Restore session from a snapshot
    /// </summary>
    [HttpPost("{sessionId}/restore")]
    [ProducesResponseType(typeof(SessionInfo), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<SessionInfo>> RestoreSnapshot(string sessionId, [FromBody] RestoreSnapshotRequest request)
    {
        try
        {
            var session = await _sessionManager.RestoreSnapshotAsync(sessionId, request);
            return Ok(session);
        }
        catch (KeyNotFoundException)
        {
            return NotFound(new { error = $"Session not found: {sessionId}" });
        }
    }

    /// <summary>
    /// Step the simulation forward by N seconds
    /// </summary>
    [HttpPost("{sessionId}/time/step")]
    [ProducesResponseType(typeof(StartSessionResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<StartSessionResponse>> StepTime(string sessionId, [FromBody] TimeStepRequest request)
    {
        try
        {
            var response = await _sessionManager.StepSessionAsync(sessionId, request);
            return Ok(response);
        }
        catch (KeyNotFoundException)
        {
            return NotFound(new { error = $"Session not found: {sessionId}" });
        }
    }

    /// <summary>
    /// Set simulation time factor (speed multiplier)
    /// </summary>
    [HttpPost("{sessionId}/time/set_factor")]
    [ProducesResponseType(typeof(TimeFactorResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<TimeFactorResponse>> SetTimeFactor(string sessionId, [FromBody] TimeFactorRequest request)
    {
        try
        {
            var response = await _sessionManager.SetTimeFactorAsync(sessionId, request);
            return Ok(response);
        }
        catch (KeyNotFoundException)
        {
            return NotFound(new { error = $"Session not found: {sessionId}" });
        }
    }

    /// <summary>
    /// Read a tag value from the simulation
    /// </summary>
    [HttpGet("{sessionId}/tags/{*tagPath}")]
    [ProducesResponseType(typeof(TagValue), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<TagValue>> ReadTag(string sessionId, string tagPath)
    {
        try
        {
            var tagValue = await _sessionManager.ReadTagAsync(sessionId, tagPath);

            if (tagValue == null)
            {
                return NotFound(new { error = $"Tag not found: {tagPath}" });
            }

            return Ok(tagValue);
        }
        catch (KeyNotFoundException)
        {
            return NotFound(new { error = $"Session not found: {sessionId}" });
        }
    }

    /// <summary>
    /// Write a value to a tag in the simulation
    /// </summary>
    [HttpPost("{sessionId}/tags/{*tagPath}")]
    [ProducesResponseType(typeof(WriteTagResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<WriteTagResponse>> WriteTag(string sessionId, string tagPath, [FromBody] WriteTagRequest request)
    {
        try
        {
            var response = await _sessionManager.WriteTagAsync(sessionId, tagPath, request);
            return Ok(response);
        }
        catch (KeyNotFoundException)
        {
            return NotFound(new { error = $"Session not found: {sessionId}" });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error writing tag {TagPath}", tagPath);
            return StatusCode(500, new { error = "Internal server error" });
        }
    }

    /// <summary>
    /// Get event log for a session
    /// </summary>
    [HttpGet("{sessionId}/events")]
    [ProducesResponseType(typeof(List<EventLogEntry>), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<List<EventLogEntry>>> GetEvents(
        string sessionId,
        [FromQuery] DateTime? from,
        [FromQuery] DateTime? to)
    {
        try
        {
            var events = await _sessionManager.GetEventsAsync(sessionId, from, to);
            return Ok(events);
        }
        catch (KeyNotFoundException)
        {
            return NotFound(new { error = $"Session not found: {sessionId}" });
        }
    }
}
