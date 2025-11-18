using DWSIM.OTS.SimulationHost.Models;
using System.Collections.Concurrent;
using System.Text.Json;

namespace DWSIM.OTS.SimulationHost.Services;

/// <summary>
/// Engine for assessing operator performance based on KPI rules
/// </summary>
public class AssessmentEngine : IAssessmentEngine
{
    private readonly ILogger<AssessmentEngine> _logger;
    private readonly ISessionManager _sessionManager;
    private readonly ITimeSeriesLogger _timeSeriesLogger;
    private readonly IConfiguration _configuration;
    private readonly ConcurrentDictionary<string, AssessmentReport> _assessments = new();
    private readonly ConcurrentDictionary<string, RuleSet> _ruleSets = new();
    private readonly string _ruleSetPath;

    public AssessmentEngine(
        ILogger<AssessmentEngine> logger,
        ISessionManager sessionManager,
        ITimeSeriesLogger timeSeriesLogger,
        IConfiguration configuration)
    {
        _logger = logger;
        _sessionManager = sessionManager;
        _timeSeriesLogger = timeSeriesLogger;
        _configuration = configuration;

        _ruleSetPath = configuration["RuleSetPath"] ?? "./data/rulesets";
        Directory.CreateDirectory(_ruleSetPath);

        // Load existing rule sets
        _ = LoadRuleSetsAsync();

        // Create default rule set if none exist
        _ = EnsureDefaultRuleSetAsync();
    }

    public async Task<AssessmentReport> AssessSessionAsync(AssessmentRequest request)
    {
        _logger.LogInformation("Assessing session {SessionId}", request.SessionId);

        var assessmentId = Guid.NewGuid().ToString();

        // Get session info
        var session = await _sessionManager.GetSessionAsync(request.SessionId);
        if (session == null)
        {
            throw new InvalidOperationException($"Session {request.SessionId} not found");
        }

        // Get rules to apply
        List<KpiRule> rules;
        double passingScore = 70.0;

        if (!string.IsNullOrEmpty(request.RuleSetId))
        {
            var ruleSet = await GetRuleSetAsync(request.RuleSetId);
            if (ruleSet == null)
            {
                throw new InvalidOperationException($"Rule set {request.RuleSetId} not found");
            }
            rules = ruleSet.Rules;
            passingScore = ruleSet.PassingScore;
        }
        else if (request.CustomRules != null && request.CustomRules.Count > 0)
        {
            rules = request.CustomRules;
        }
        else
        {
            // Use default rule set
            var defaultRuleSet = _ruleSets.Values.FirstOrDefault(r => r.Name == "Default");
            rules = defaultRuleSet?.Rules ?? new List<KpiRule>();
        }

        if (rules.Count == 0)
        {
            throw new InvalidOperationException("No KPI rules defined for assessment");
        }

        // Get time-series data for session
        var stats = await _timeSeriesLogger.GetStatsAsync(request.SessionId);

        // Evaluate each KPI
        var kpiResults = new List<KpiResult>();
        var totalOperatorActions = 0;
        var totalAlarms = 0;

        foreach (var rule in rules)
        {
            var kpiResult = await EvaluateKpiAsync(rule, request.SessionId, session, stats);
            kpiResults.Add(kpiResult);

            // Track summary stats
            if (rule.Type == KpiType.OperatorActionCount)
            {
                totalOperatorActions = (int)kpiResult.ActualValue;
            }
            else if (rule.Type == KpiType.AlarmCount)
            {
                totalAlarms = (int)kpiResult.ActualValue;
            }
        }

        // Calculate overall score (weighted average)
        var totalWeight = rules.Sum(r => r.Weight);
        var weightedScore = kpiResults.Sum(r => r.Score * r.Weight) / totalWeight;

        // Determine grade
        var grade = CalculateGrade(weightedScore);

        // Check if all required KPIs passed
        var requiredKpis = kpiResults.Where(r => rules.First(rule => rule.RuleId == r.RuleId).IsRequired);
        var allRequiredPassed = requiredKpis.All(r => r.Passed);

        var passed = weightedScore >= passingScore && allRequiredPassed;

        // Generate recommendations
        var recommendations = GenerateRecommendations(kpiResults, rules);

        // Create assessment report
        var report = new AssessmentReport
        {
            AssessmentId = assessmentId,
            SessionId = request.SessionId,
            GeneratedAt = DateTime.UtcNow,
            OverallScore = Math.Round(weightedScore, 2),
            Passed = passed,
            PassingScore = passingScore,
            Grade = grade,
            KpiResults = kpiResults,
            Recommendations = recommendations,
            Summary = new AssessmentSummary
            {
                TotalKpis = kpiResults.Count,
                KpisPassed = kpiResults.Count(r => r.Passed),
                KpisFailed = kpiResults.Count(r => !r.Passed),
                SessionDuration = stats.Duration ?? TimeSpan.Zero,
                TotalOperatorActions = totalOperatorActions,
                TotalAlarms = totalAlarms,
                LevelBreakdown = kpiResults
                    .GroupBy(r => r.Level)
                    .ToDictionary(g => g.Key, g => g.Count())
            }
        };

        // Generate PDF if requested
        if (request.GeneratePdf)
        {
            report.PdfReportPath = await GeneratePdfReportAsync(report);
        }

        // Store assessment
        _assessments[assessmentId] = report;

        _logger.LogInformation(
            "Assessment complete: Score {Score}/100, Grade {Grade}, Status {Status}",
            report.OverallScore,
            report.Grade,
            report.Passed ? "PASS" : "FAIL");

        return report;
    }

    public async Task<AssessmentReport?> GetAssessmentAsync(string assessmentId)
    {
        _assessments.TryGetValue(assessmentId, out var report);
        return await Task.FromResult(report);
    }

    public async Task<List<AssessmentReport>> GetSessionAssessmentsAsync(string sessionId)
    {
        var reports = _assessments.Values
            .Where(a => a.SessionId == sessionId)
            .OrderByDescending(a => a.GeneratedAt)
            .ToList();

        return await Task.FromResult(reports);
    }

    public async Task<RuleSet> CreateRuleSetAsync(RuleSet ruleSet)
    {
        if (string.IsNullOrEmpty(ruleSet.RuleSetId))
        {
            ruleSet.RuleSetId = Guid.NewGuid().ToString();
        }

        ruleSet.CreatedAt = DateTime.UtcNow;
        _ruleSets[ruleSet.RuleSetId] = ruleSet;

        await SaveRuleSetAsync(ruleSet);

        _logger.LogInformation("Created rule set {RuleSetId}: {Name}", ruleSet.RuleSetId, ruleSet.Name);
        return ruleSet;
    }

    public async Task<RuleSet?> GetRuleSetAsync(string ruleSetId)
    {
        _ruleSets.TryGetValue(ruleSetId, out var ruleSet);
        return await Task.FromResult(ruleSet);
    }

    public async Task<List<RuleSet>> GetAllRuleSetsAsync()
    {
        return await Task.FromResult(_ruleSets.Values.ToList());
    }

    public async Task<RuleSet> UpdateRuleSetAsync(RuleSet ruleSet)
    {
        if (!_ruleSets.ContainsKey(ruleSet.RuleSetId))
        {
            throw new InvalidOperationException($"Rule set {ruleSet.RuleSetId} not found");
        }

        ruleSet.UpdatedAt = DateTime.UtcNow;
        _ruleSets[ruleSet.RuleSetId] = ruleSet;

        await SaveRuleSetAsync(ruleSet);

        _logger.LogInformation("Updated rule set {RuleSetId}: {Name}", ruleSet.RuleSetId, ruleSet.Name);
        return ruleSet;
    }

    public async Task<bool> DeleteRuleSetAsync(string ruleSetId)
    {
        if (_ruleSets.TryRemove(ruleSetId, out var ruleSet))
        {
            var filePath = Path.Combine(_ruleSetPath, $"{ruleSetId}.json");
            if (File.Exists(filePath))
            {
                File.Delete(filePath);
            }

            _logger.LogInformation("Deleted rule set {RuleSetId}: {Name}", ruleSetId, ruleSet.Name);
            return await Task.FromResult(true);
        }

        return await Task.FromResult(false);
    }

    private async Task<KpiResult> EvaluateKpiAsync(
        KpiRule rule,
        string sessionId,
        SessionInfo session,
        TimeSeriesStats stats)
    {
        var result = new KpiResult
        {
            RuleId = rule.RuleId,
            Name = rule.Name,
            Type = rule.Type,
            TargetValue = rule.Thresholds.Target,
            Weight = rule.Weight
        };

        try
        {
            switch (rule.Type)
            {
                case KpiType.TimeToSteadyState:
                    result.ActualValue = await CalculateTimeToSteadyStateAsync(sessionId, rule.TagPaths);
                    result.Units = "seconds";
                    break;

                case KpiType.AlarmCount:
                    result.ActualValue = await CalculateAlarmCountAsync(sessionId);
                    result.Units = "count";
                    break;

                case KpiType.SetpointDeviationIntegral:
                    result.ActualValue = await CalculateSetpointDeviationIntegralAsync(sessionId, rule.TagPaths);
                    result.Units = "integral";
                    break;

                case KpiType.OperatorActionCount:
                    result.ActualValue = await CalculateOperatorActionCountAsync(sessionId);
                    result.Units = "count";
                    break;

                case KpiType.ProcessVariableExcursions:
                    result.ActualValue = await CalculateExcursionsAsync(sessionId, rule.TagPaths, rule.Thresholds);
                    result.Units = "count";
                    break;

                case KpiType.AverageValue:
                    result.ActualValue = await CalculateAverageValueAsync(sessionId, rule.TagPaths);
                    result.Units = "value";
                    break;

                case KpiType.MaxValue:
                    result.ActualValue = await CalculateMaxValueAsync(sessionId, rule.TagPaths);
                    result.Units = "value";
                    break;

                case KpiType.MinValue:
                    result.ActualValue = await CalculateMinValueAsync(sessionId, rule.TagPaths);
                    result.Units = "value";
                    break;

                case KpiType.Variability:
                    result.ActualValue = await CalculateVariabilityAsync(sessionId, rule.TagPaths);
                    result.Units = "stddev";
                    break;

                case KpiType.TimeInRange:
                    result.ActualValue = await CalculateTimeInRangeAsync(sessionId, rule.TagPaths, rule.Thresholds);
                    result.Units = "percent";
                    break;

                default:
                    result.ActualValue = 0;
                    break;
            }

            // Calculate score and performance level
            (result.Score, result.Level) = CalculateKpiScore(result.ActualValue, rule.Thresholds, rule.Type);

            // Determine if passed based on thresholds
            result.Passed = DetermineKpiPassed(result.ActualValue, rule.Thresholds, rule.Type);

            _logger.LogDebug("KPI {Name}: Value={Value}, Score={Score}, Level={Level}",
                rule.Name, result.ActualValue, result.Score, result.Level);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error evaluating KPI {Name}", rule.Name);
            result.Score = 0;
            result.Passed = false;
            result.Level = PerformanceLevel.Fail;
        }

        return result;
    }

    private async Task<double> CalculateTimeToSteadyStateAsync(string sessionId, List<string> tagPaths)
    {
        if (tagPaths.Count == 0)
            return 0;

        // Query time-series data for primary tag
        var queryRequest = new TimeSeriesQueryRequest
        {
            SessionId = sessionId,
            TagPaths = new List<string> { tagPaths[0] },
            Aggregation = "none"
        };

        var data = await _timeSeriesLogger.QueryAsync(queryRequest);
        if (data.Count == 0 || data[0].Points.Count == 0)
            return 0;

        var points = data[0].Points.OrderBy(p => p.Time).ToList();

        // Simple steady-state detection: when variance over a window becomes small
        const int windowSize = 10;
        const double varianceThreshold = 0.01;

        for (int i = windowSize; i < points.Count; i++)
        {
            var window = points.Skip(i - windowSize).Take(windowSize).Select(p => p.Value).ToList();
            var mean = window.Average();
            var variance = window.Select(v => Math.Pow(v - mean, 2)).Average();

            if (variance < varianceThreshold)
            {
                // Reached steady state
                var steadyStateTime = points[i].Time;
                var startTime = points[0].Time;
                return (steadyStateTime - startTime).TotalSeconds;
            }
        }

        // Never reached steady state
        return (points.Last().Time - points.First().Time).TotalSeconds;
    }

    private async Task<double> CalculateAlarmCountAsync(string sessionId)
    {
        // In a full implementation, query alarm log table
        // For now, return mock value
        return await Task.FromResult(0);
    }

    private async Task<double> CalculateSetpointDeviationIntegralAsync(string sessionId, List<string> tagPaths)
    {
        if (tagPaths.Count < 2)
            return 0;

        // Assume tagPaths[0] is setpoint, tagPaths[1] is process value
        var queryRequest = new TimeSeriesQueryRequest
        {
            SessionId = sessionId,
            TagPaths = tagPaths.Take(2).ToList(),
            Aggregation = "none"
        };

        var data = await _timeSeriesLogger.QueryAsync(queryRequest);
        if (data.Count < 2)
            return 0;

        var setpoints = data[0].Points.OrderBy(p => p.Time).ToList();
        var pvs = data[1].Points.OrderBy(p => p.Time).ToList();

        // Calculate integral of absolute error
        var integral = 0.0;
        var minCount = Math.Min(setpoints.Count, pvs.Count);

        for (int i = 1; i < minCount; i++)
        {
            var dt = (setpoints[i].Time - setpoints[i - 1].Time).TotalSeconds;
            var error = Math.Abs(setpoints[i].Value - pvs[i].Value);
            integral += error * dt;
        }

        return integral;
    }

    private async Task<double> CalculateOperatorActionCountAsync(string sessionId)
    {
        // In a full implementation, query event log for operator tag writes
        // For now, return mock value
        return await Task.FromResult(0);
    }

    private async Task<double> CalculateExcursionsAsync(string sessionId, List<string> tagPaths, KpiThresholds thresholds)
    {
        if (tagPaths.Count == 0)
            return 0;

        var queryRequest = new TimeSeriesQueryRequest
        {
            SessionId = sessionId,
            TagPaths = tagPaths,
            Aggregation = "none"
        };

        var data = await _timeSeriesLogger.QueryAsync(queryRequest);
        var excursions = 0;

        foreach (var tagData in data)
        {
            foreach (var point in tagData.Points)
            {
                if (thresholds.Min.HasValue && point.Value < thresholds.Min.Value)
                    excursions++;
                else if (thresholds.Max.HasValue && point.Value > thresholds.Max.Value)
                    excursions++;
            }
        }

        return excursions;
    }

    private async Task<double> CalculateAverageValueAsync(string sessionId, List<string> tagPaths)
    {
        if (tagPaths.Count == 0)
            return 0;

        var queryRequest = new TimeSeriesQueryRequest
        {
            SessionId = sessionId,
            TagPaths = tagPaths,
            Aggregation = "none"
        };

        var data = await _timeSeriesLogger.QueryAsync(queryRequest);
        if (data.Count == 0 || data[0].Points.Count == 0)
            return 0;

        return data[0].Points.Average(p => p.Value);
    }

    private async Task<double> CalculateMaxValueAsync(string sessionId, List<string> tagPaths)
    {
        if (tagPaths.Count == 0)
            return 0;

        var queryRequest = new TimeSeriesQueryRequest
        {
            SessionId = sessionId,
            TagPaths = tagPaths,
            Aggregation = "none"
        };

        var data = await _timeSeriesLogger.QueryAsync(queryRequest);
        if (data.Count == 0 || data[0].Points.Count == 0)
            return 0;

        return data[0].Points.Max(p => p.Value);
    }

    private async Task<double> CalculateMinValueAsync(string sessionId, List<string> tagPaths)
    {
        if (tagPaths.Count == 0)
            return 0;

        var queryRequest = new TimeSeriesQueryRequest
        {
            SessionId = sessionId,
            TagPaths = tagPaths,
            Aggregation = "none"
        };

        var data = await _timeSeriesLogger.QueryAsync(queryRequest);
        if (data.Count == 0 || data[0].Points.Count == 0)
            return 0;

        return data[0].Points.Min(p => p.Value);
    }

    private async Task<double> CalculateVariabilityAsync(string sessionId, List<string> tagPaths)
    {
        if (tagPaths.Count == 0)
            return 0;

        var queryRequest = new TimeSeriesQueryRequest
        {
            SessionId = sessionId,
            TagPaths = tagPaths,
            Aggregation = "none"
        };

        var data = await _timeSeriesLogger.QueryAsync(queryRequest);
        if (data.Count == 0 || data[0].Points.Count == 0)
            return 0;

        var values = data[0].Points.Select(p => p.Value).ToList();
        var mean = values.Average();
        var variance = values.Select(v => Math.Pow(v - mean, 2)).Average();

        return Math.Sqrt(variance);
    }

    private async Task<double> CalculateTimeInRangeAsync(string sessionId, List<string> tagPaths, KpiThresholds thresholds)
    {
        if (tagPaths.Count == 0 || !thresholds.Min.HasValue || !thresholds.Max.HasValue)
            return 0;

        var queryRequest = new TimeSeriesQueryRequest
        {
            SessionId = sessionId,
            TagPaths = tagPaths,
            Aggregation = "none"
        };

        var data = await _timeSeriesLogger.QueryAsync(queryRequest);
        if (data.Count == 0 || data[0].Points.Count == 0)
            return 0;

        var totalPoints = data[0].Points.Count;
        var pointsInRange = data[0].Points.Count(p =>
            p.Value >= thresholds.Min.Value && p.Value <= thresholds.Max.Value);

        return (double)pointsInRange / totalPoints * 100.0;
    }

    private (double score, PerformanceLevel level) CalculateKpiScore(double actualValue, KpiThresholds thresholds, KpiType type)
    {
        // For metrics where lower is better (alarms, excursions, deviation, etc.)
        var lowerIsBetter = type == KpiType.AlarmCount ||
                           type == KpiType.ProcessVariableExcursions ||
                           type == KpiType.SetpointDeviationIntegral ||
                           type == KpiType.Variability ||
                           type == KpiType.TimeToSteadyState;

        if (thresholds.Excellent.HasValue && thresholds.Poor.HasValue)
        {
            // Use performance bands
            if (lowerIsBetter)
            {
                if (actualValue <= thresholds.Excellent.Value)
                    return (100, PerformanceLevel.Excellent);
                else if (actualValue <= thresholds.Good ?? thresholds.Excellent.Value)
                    return (85, PerformanceLevel.Good);
                else if (actualValue <= thresholds.Acceptable ?? thresholds.Good ?? thresholds.Excellent.Value)
                    return (70, PerformanceLevel.Acceptable);
                else if (actualValue <= thresholds.Poor.Value)
                    return (50, PerformanceLevel.Poor);
                else
                    return (0, PerformanceLevel.Fail);
            }
            else
            {
                if (actualValue >= thresholds.Excellent.Value)
                    return (100, PerformanceLevel.Excellent);
                else if (actualValue >= thresholds.Good ?? thresholds.Excellent.Value)
                    return (85, PerformanceLevel.Good);
                else if (actualValue >= thresholds.Acceptable ?? thresholds.Good ?? thresholds.Excellent.Value)
                    return (70, PerformanceLevel.Acceptable);
                else if (actualValue >= thresholds.Poor.Value)
                    return (50, PerformanceLevel.Poor);
                else
                    return (0, PerformanceLevel.Fail);
            }
        }
        else if (thresholds.Target.HasValue)
        {
            // Score based on distance from target
            var deviation = Math.Abs(actualValue - thresholds.Target.Value);
            var percentDeviation = thresholds.Target.Value != 0
                ? (deviation / Math.Abs(thresholds.Target.Value)) * 100.0
                : deviation;

            if (percentDeviation < 5)
                return (100, PerformanceLevel.Excellent);
            else if (percentDeviation < 10)
                return (85, PerformanceLevel.Good);
            else if (percentDeviation < 20)
                return (70, PerformanceLevel.Acceptable);
            else if (percentDeviation < 50)
                return (50, PerformanceLevel.Poor);
            else
                return (0, PerformanceLevel.Fail);
        }

        // Default scoring
        return (50, PerformanceLevel.Acceptable);
    }

    private bool DetermineKpiPassed(double actualValue, KpiThresholds thresholds, KpiType type)
    {
        if (thresholds.Min.HasValue && actualValue < thresholds.Min.Value)
            return false;

        if (thresholds.Max.HasValue && actualValue > thresholds.Max.Value)
            return false;

        return true;
    }

    private string CalculateGrade(double score)
    {
        if (score >= 90) return "A";
        if (score >= 80) return "B";
        if (score >= 70) return "C";
        if (score >= 60) return "D";
        return "F";
    }

    private List<string> GenerateRecommendations(List<KpiResult> kpiResults, List<KpiRule> rules)
    {
        var recommendations = new List<string>();

        foreach (var result in kpiResults.Where(r => !r.Passed))
        {
            var rule = rules.First(r => r.RuleId == result.RuleId);

            switch (result.Type)
            {
                case KpiType.TimeToSteadyState:
                    recommendations.Add($"Reduce time to steady state by optimizing control parameters and reducing unnecessary adjustments.");
                    break;

                case KpiType.AlarmCount:
                    recommendations.Add($"Reduce alarm frequency by addressing root causes and improving process control.");
                    break;

                case KpiType.SetpointDeviationIntegral:
                    recommendations.Add($"Minimize setpoint deviation by improving control response and reducing disturbances.");
                    break;

                case KpiType.OperatorActionCount:
                    if (result.ActualValue > (result.TargetValue ?? 0))
                        recommendations.Add($"Reduce excessive operator interventions by tuning automatic controllers.");
                    else
                        recommendations.Add($"Increase operator engagement when needed to maintain process control.");
                    break;

                case KpiType.ProcessVariableExcursions:
                    recommendations.Add($"Prevent process variable excursions by implementing tighter control and better disturbance rejection.");
                    break;

                case KpiType.Variability:
                    recommendations.Add($"Reduce process variability through improved controller tuning and disturbance handling.");
                    break;
            }
        }

        if (recommendations.Count == 0)
        {
            recommendations.Add("Excellent performance! Continue following best practices.");
        }

        return recommendations;
    }

    private async Task<string> GeneratePdfReportAsync(AssessmentReport report)
    {
        // In a full implementation, use a PDF library like QuestPDF or iTextSharp
        // For now, just create a placeholder path
        var pdfPath = Path.Combine(_ruleSetPath, $"assessment_{report.AssessmentId}.pdf");

        _logger.LogInformation("PDF report generation not yet implemented. Would save to {Path}", pdfPath);

        return await Task.FromResult(pdfPath);
    }

    private async Task LoadRuleSetsAsync()
    {
        try
        {
            var files = Directory.GetFiles(_ruleSetPath, "*.json");

            foreach (var file in files)
            {
                var json = await File.ReadAllTextAsync(file);
                var ruleSet = JsonSerializer.Deserialize<RuleSet>(json);

                if (ruleSet != null)
                {
                    _ruleSets[ruleSet.RuleSetId] = ruleSet;
                    _logger.LogInformation("Loaded rule set {RuleSetId}: {Name}", ruleSet.RuleSetId, ruleSet.Name);
                }
            }
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error loading rule sets");
        }
    }

    private async Task SaveRuleSetAsync(RuleSet ruleSet)
    {
        try
        {
            var filePath = Path.Combine(_ruleSetPath, $"{ruleSet.RuleSetId}.json");
            var json = JsonSerializer.Serialize(ruleSet, new JsonSerializerOptions { WriteIndented = true });
            await File.WriteAllTextAsync(filePath, json);

            _logger.LogDebug("Saved rule set to {Path}", filePath);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error saving rule set {RuleSetId}", ruleSet.RuleSetId);
        }
    }

    private async Task EnsureDefaultRuleSetAsync()
    {
        if (_ruleSets.Values.Any(r => r.Name == "Default"))
            return;

        var defaultRuleSet = new RuleSet
        {
            RuleSetId = "default",
            Name = "Default",
            Description = "Default assessment rules for distillation column operation",
            PassingScore = 70.0,
            Rules = new List<KpiRule>
            {
                new KpiRule
                {
                    RuleId = "kpi_steady_state",
                    Name = "Time to Steady State",
                    Description = "Time required to reach steady-state operation",
                    Type = KpiType.TimeToSteadyState,
                    Weight = 1.0,
                    Thresholds = new KpiThresholds
                    {
                        Excellent = 300,
                        Good = 600,
                        Acceptable = 900,
                        Poor = 1200
                    },
                    TagPaths = new List<string> { "Units.Column1.Temperature" }
                },
                new KpiRule
                {
                    RuleId = "kpi_alarms",
                    Name = "Alarm Count",
                    Description = "Number of alarms raised during operation",
                    Type = KpiType.AlarmCount,
                    Weight = 1.5,
                    Thresholds = new KpiThresholds
                    {
                        Excellent = 0,
                        Good = 2,
                        Acceptable = 5,
                        Poor = 10,
                        Max = 15
                    },
                    TagPaths = new List<string>()
                },
                new KpiRule
                {
                    RuleId = "kpi_temp_variability",
                    Name = "Temperature Variability",
                    Description = "Standard deviation of column temperature",
                    Type = KpiType.Variability,
                    Weight = 1.0,
                    Thresholds = new KpiThresholds
                    {
                        Excellent = 1.0,
                        Good = 2.0,
                        Acceptable = 5.0,
                        Poor = 10.0
                    },
                    TagPaths = new List<string> { "Units.Column1.Temperature" }
                }
            }
        };

        await CreateRuleSetAsync(defaultRuleSet);
    }
}
