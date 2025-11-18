using DWSIM.OTS.SimulationHost.Models;

namespace DWSIM.OTS.SimulationHost.Services;

/// <summary>
/// Service for assessing operator performance based on KPI rules
/// </summary>
public interface IAssessmentEngine
{
    /// <summary>
    /// Assess a session based on KPI rules
    /// </summary>
    /// <param name="request">Assessment request</param>
    /// <returns>Assessment report with scores and recommendations</returns>
    Task<AssessmentReport> AssessSessionAsync(AssessmentRequest request);

    /// <summary>
    /// Get an existing assessment report
    /// </summary>
    /// <param name="assessmentId">Assessment ID</param>
    /// <returns>Assessment report</returns>
    Task<AssessmentReport?> GetAssessmentAsync(string assessmentId);

    /// <summary>
    /// Get all assessments for a session
    /// </summary>
    /// <param name="sessionId">Session ID</param>
    /// <returns>List of assessment reports</returns>
    Task<List<AssessmentReport>> GetSessionAssessmentsAsync(string sessionId);

    /// <summary>
    /// Create a new rule set
    /// </summary>
    /// <param name="ruleSet">Rule set definition</param>
    /// <returns>Created rule set</returns>
    Task<RuleSet> CreateRuleSetAsync(RuleSet ruleSet);

    /// <summary>
    /// Get a rule set by ID
    /// </summary>
    /// <param name="ruleSetId">Rule set ID</param>
    /// <returns>Rule set</returns>
    Task<RuleSet?> GetRuleSetAsync(string ruleSetId);

    /// <summary>
    /// Get all rule sets
    /// </summary>
    /// <returns>List of rule sets</returns>
    Task<List<RuleSet>> GetAllRuleSetsAsync();

    /// <summary>
    /// Update an existing rule set
    /// </summary>
    /// <param name="ruleSet">Updated rule set</param>
    /// <returns>Updated rule set</returns>
    Task<RuleSet> UpdateRuleSetAsync(RuleSet ruleSet);

    /// <summary>
    /// Delete a rule set
    /// </summary>
    /// <param name="ruleSetId">Rule set ID</param>
    /// <returns>True if deleted successfully</returns>
    Task<bool> DeleteRuleSetAsync(string ruleSetId);
}
