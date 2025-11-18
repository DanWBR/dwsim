using Microsoft.AspNetCore.Mvc;
using DWSIM.OTS.SimulationHost.Models;
using DWSIM.OTS.SimulationHost.Services;

namespace DWSIM.OTS.SimulationHost.Controllers;

[ApiController]
[Route("api/v1/assessment")]
[Produces("application/json")]
public class AssessmentController : ControllerBase
{
    private readonly IAssessmentEngine _assessmentEngine;
    private readonly ILogger<AssessmentController> _logger;

    public AssessmentController(
        IAssessmentEngine assessmentEngine,
        ILogger<AssessmentController> logger)
    {
        _assessmentEngine = assessmentEngine;
        _logger = logger;
    }

    /// <summary>
    /// Assess a session based on KPI rules
    /// </summary>
    /// <param name="request">Assessment request with session ID and rules</param>
    /// <returns>Assessment report with scores and recommendations</returns>
    [HttpPost]
    [ProducesResponseType(typeof(AssessmentReport), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<AssessmentReport>> AssessSession([FromBody] AssessmentRequest request)
    {
        try
        {
            if (string.IsNullOrEmpty(request.SessionId))
            {
                return BadRequest(new { error = "SessionId is required" });
            }

            var report = await _assessmentEngine.AssessSessionAsync(request);
            return Ok(report);
        }
        catch (InvalidOperationException ex)
        {
            _logger.LogWarning(ex, "Invalid assessment request");
            return BadRequest(new { error = ex.Message });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error assessing session");
            return StatusCode(500, new { error = "Internal server error" });
        }
    }

    /// <summary>
    /// Get an existing assessment report
    /// </summary>
    /// <param name="assessmentId">Assessment ID</param>
    /// <returns>Assessment report</returns>
    [HttpGet("{assessmentId}")]
    [ProducesResponseType(typeof(AssessmentReport), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<AssessmentReport>> GetAssessment(string assessmentId)
    {
        var report = await _assessmentEngine.GetAssessmentAsync(assessmentId);

        if (report == null)
        {
            return NotFound(new { error = $"Assessment {assessmentId} not found" });
        }

        return Ok(report);
    }

    /// <summary>
    /// Get all assessments for a session
    /// </summary>
    /// <param name="sessionId">Session ID</param>
    /// <returns>List of assessment reports</returns>
    [HttpGet("session/{sessionId}")]
    [ProducesResponseType(typeof(List<AssessmentReport>), StatusCodes.Status200OK)]
    public async Task<ActionResult<List<AssessmentReport>>> GetSessionAssessments(string sessionId)
    {
        var reports = await _assessmentEngine.GetSessionAssessmentsAsync(sessionId);
        return Ok(reports);
    }

    /// <summary>
    /// Create a new rule set
    /// </summary>
    /// <param name="ruleSet">Rule set definition</param>
    /// <returns>Created rule set</returns>
    [HttpPost("rulesets")]
    [ProducesResponseType(typeof(RuleSet), StatusCodes.Status201Created)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    public async Task<ActionResult<RuleSet>> CreateRuleSet([FromBody] RuleSet ruleSet)
    {
        try
        {
            if (string.IsNullOrEmpty(ruleSet.Name))
            {
                return BadRequest(new { error = "Rule set name is required" });
            }

            var created = await _assessmentEngine.CreateRuleSetAsync(ruleSet);
            return CreatedAtAction(nameof(GetRuleSet), new { ruleSetId = created.RuleSetId }, created);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error creating rule set");
            return StatusCode(500, new { error = "Internal server error" });
        }
    }

    /// <summary>
    /// Get a rule set by ID
    /// </summary>
    /// <param name="ruleSetId">Rule set ID</param>
    /// <returns>Rule set</returns>
    [HttpGet("rulesets/{ruleSetId}")]
    [ProducesResponseType(typeof(RuleSet), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<RuleSet>> GetRuleSet(string ruleSetId)
    {
        var ruleSet = await _assessmentEngine.GetRuleSetAsync(ruleSetId);

        if (ruleSet == null)
        {
            return NotFound(new { error = $"Rule set {ruleSetId} not found" });
        }

        return Ok(ruleSet);
    }

    /// <summary>
    /// Get all rule sets
    /// </summary>
    /// <returns>List of rule sets</returns>
    [HttpGet("rulesets")]
    [ProducesResponseType(typeof(List<RuleSet>), StatusCodes.Status200OK)]
    public async Task<ActionResult<List<RuleSet>>> GetAllRuleSets()
    {
        var ruleSets = await _assessmentEngine.GetAllRuleSetsAsync();
        return Ok(ruleSets);
    }

    /// <summary>
    /// Update an existing rule set
    /// </summary>
    /// <param name="ruleSetId">Rule set ID</param>
    /// <param name="ruleSet">Updated rule set</param>
    /// <returns>Updated rule set</returns>
    [HttpPut("rulesets/{ruleSetId}")]
    [ProducesResponseType(typeof(RuleSet), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<RuleSet>> UpdateRuleSet(string ruleSetId, [FromBody] RuleSet ruleSet)
    {
        try
        {
            if (ruleSetId != ruleSet.RuleSetId)
            {
                return BadRequest(new { error = "Rule set ID mismatch" });
            }

            var updated = await _assessmentEngine.UpdateRuleSetAsync(ruleSet);
            return Ok(updated);
        }
        catch (InvalidOperationException ex)
        {
            return NotFound(new { error = ex.Message });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error updating rule set");
            return StatusCode(500, new { error = "Internal server error" });
        }
    }

    /// <summary>
    /// Delete a rule set
    /// </summary>
    /// <param name="ruleSetId">Rule set ID to delete</param>
    /// <returns>Success status</returns>
    [HttpDelete("rulesets/{ruleSetId}")]
    [ProducesResponseType(StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult> DeleteRuleSet(string ruleSetId)
    {
        var deleted = await _assessmentEngine.DeleteRuleSetAsync(ruleSetId);

        if (!deleted)
        {
            return NotFound(new { error = $"Rule set {ruleSetId} not found" });
        }

        return Ok(new { message = "Rule set deleted", ruleSetId });
    }
}
