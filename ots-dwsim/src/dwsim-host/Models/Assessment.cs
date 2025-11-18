namespace DWSIM.OTS.SimulationHost.Models;

/// <summary>
/// Request to assess a session based on KPI rules
/// </summary>
public class AssessmentRequest
{
    /// <summary>
    /// Session ID to assess
    /// </summary>
    public string SessionId { get; set; } = string.Empty;

    /// <summary>
    /// Assessment rule set ID (references a predefined rule set)
    /// </summary>
    public string? RuleSetId { get; set; }

    /// <summary>
    /// Custom KPI rules (if not using predefined rule set)
    /// </summary>
    public List<KpiRule>? CustomRules { get; set; }

    /// <summary>
    /// Include detailed breakdown in report
    /// </summary>
    public bool IncludeDetails { get; set; } = true;

    /// <summary>
    /// Generate PDF report
    /// </summary>
    public bool GeneratePdf { get; set; } = false;
}

/// <summary>
/// Assessment report for a session
/// </summary>
public class AssessmentReport
{
    /// <summary>
    /// Unique assessment ID
    /// </summary>
    public string AssessmentId { get; set; } = string.Empty;

    /// <summary>
    /// Session ID assessed
    /// </summary>
    public string SessionId { get; set; } = string.Empty;

    /// <summary>
    /// When assessment was generated
    /// </summary>
    public DateTime GeneratedAt { get; set; }

    /// <summary>
    /// Overall score (0-100)
    /// </summary>
    public double OverallScore { get; set; }

    /// <summary>
    /// Pass/fail status
    /// </summary>
    public bool Passed { get; set; }

    /// <summary>
    /// Minimum passing score
    /// </summary>
    public double PassingScore { get; set; } = 70.0;

    /// <summary>
    /// Performance grade (A, B, C, D, F)
    /// </summary>
    public string Grade { get; set; } = string.Empty;

    /// <summary>
    /// Individual KPI results
    /// </summary>
    public List<KpiResult> KpiResults { get; set; } = new();

    /// <summary>
    /// Summary statistics
    /// </summary>
    public AssessmentSummary Summary { get; set; } = new();

    /// <summary>
    /// Path to PDF report (if generated)
    /// </summary>
    public string? PdfReportPath { get; set; }

    /// <summary>
    /// Recommendations for improvement
    /// </summary>
    public List<string> Recommendations { get; set; } = new();
}

/// <summary>
/// KPI rule definition
/// </summary>
public class KpiRule
{
    /// <summary>
    /// Unique rule ID
    /// </summary>
    public string RuleId { get; set; } = string.Empty;

    /// <summary>
    /// Rule name
    /// </summary>
    public string Name { get; set; } = string.Empty;

    /// <summary>
    /// Rule description
    /// </summary>
    public string Description { get; set; } = string.Empty;

    /// <summary>
    /// KPI type
    /// </summary>
    public KpiType Type { get; set; }

    /// <summary>
    /// Weight in overall score (0-1)
    /// </summary>
    public double Weight { get; set; } = 1.0;

    /// <summary>
    /// Target/threshold values
    /// </summary>
    public KpiThresholds Thresholds { get; set; } = new();

    /// <summary>
    /// Tag paths relevant to this KPI
    /// </summary>
    public List<string> TagPaths { get; set; } = new();

    /// <summary>
    /// Whether this KPI is required to pass
    /// </summary>
    public bool IsRequired { get; set; } = false;
}

/// <summary>
/// KPI types
/// </summary>
public enum KpiType
{
    /// <summary>
    /// Time to reach steady state
    /// </summary>
    TimeToSteadyState,

    /// <summary>
    /// Number of alarms raised
    /// </summary>
    AlarmCount,

    /// <summary>
    /// Integral of setpoint deviation
    /// </summary>
    SetpointDeviationIntegral,

    /// <summary>
    /// Number of operator actions
    /// </summary>
    OperatorActionCount,

    /// <summary>
    /// Process variable excursions beyond limits
    /// </summary>
    ProcessVariableExcursions,

    /// <summary>
    /// Average process variable value
    /// </summary>
    AverageValue,

    /// <summary>
    /// Maximum process variable value
    /// </summary>
    MaxValue,

    /// <summary>
    /// Minimum process variable value
    /// </summary>
    MinValue,

    /// <summary>
    /// Process variable variability (standard deviation)
    /// </summary>
    Variability,

    /// <summary>
    /// Time within operating range
    /// </summary>
    TimeInRange,

    /// <summary>
    /// Custom calculated metric
    /// </summary>
    Custom
}

/// <summary>
/// Thresholds for KPI evaluation
/// </summary>
public class KpiThresholds
{
    /// <summary>
    /// Target/ideal value
    /// </summary>
    public double? Target { get; set; }

    /// <summary>
    /// Minimum acceptable value
    /// </summary>
    public double? Min { get; set; }

    /// <summary>
    /// Maximum acceptable value
    /// </summary>
    public double? Max { get; set; }

    /// <summary>
    /// Excellent performance threshold (for grading)
    /// </summary>
    public double? Excellent { get; set; }

    /// <summary>
    /// Good performance threshold
    /// </summary>
    public double? Good { get; set; }

    /// <summary>
    /// Acceptable performance threshold
    /// </summary>
    public double? Acceptable { get; set; }

    /// <summary>
    /// Poor performance threshold
    /// </summary>
    public double? Poor { get; set; }
}

/// <summary>
/// Result of evaluating a single KPI
/// </summary>
public class KpiResult
{
    /// <summary>
    /// Rule ID
    /// </summary>
    public string RuleId { get; set; } = string.Empty;

    /// <summary>
    /// Rule name
    /// </summary>
    public string Name { get; set; } = string.Empty;

    /// <summary>
    /// KPI type
    /// </summary>
    public KpiType Type { get; set; }

    /// <summary>
    /// Actual measured value
    /// </summary>
    public double ActualValue { get; set; }

    /// <summary>
    /// Target value (from rule)
    /// </summary>
    public double? TargetValue { get; set; }

    /// <summary>
    /// Score for this KPI (0-100)
    /// </summary>
    public double Score { get; set; }

    /// <summary>
    /// Whether this KPI passed
    /// </summary>
    public bool Passed { get; set; }

    /// <summary>
    /// Performance level
    /// </summary>
    public PerformanceLevel Level { get; set; }

    /// <summary>
    /// Weight in overall score
    /// </summary>
    public double Weight { get; set; }

    /// <summary>
    /// Detailed breakdown (if available)
    /// </summary>
    public string? Details { get; set; }

    /// <summary>
    /// Units
    /// </summary>
    public string? Units { get; set; }
}

/// <summary>
/// Performance level for a KPI
/// </summary>
public enum PerformanceLevel
{
    Excellent,
    Good,
    Acceptable,
    Poor,
    Fail
}

/// <summary>
/// Summary statistics for assessment
/// </summary>
public class AssessmentSummary
{
    /// <summary>
    /// Total number of KPIs evaluated
    /// </summary>
    public int TotalKpis { get; set; }

    /// <summary>
    /// Number of KPIs passed
    /// </summary>
    public int KpisPassed { get; set; }

    /// <summary>
    /// Number of KPIs failed
    /// </summary>
    public int KpisFailed { get; set; }

    /// <summary>
    /// Session duration
    /// </summary>
    public TimeSpan SessionDuration { get; set; }

    /// <summary>
    /// Total operator actions
    /// </summary>
    public int TotalOperatorActions { get; set; }

    /// <summary>
    /// Total alarms raised
    /// </summary>
    public int TotalAlarms { get; set; }

    /// <summary>
    /// Breakdown by performance level
    /// </summary>
    public Dictionary<PerformanceLevel, int> LevelBreakdown { get; set; } = new();
}

/// <summary>
/// Predefined rule set for assessments
/// </summary>
public class RuleSet
{
    /// <summary>
    /// Unique rule set ID
    /// </summary>
    public string RuleSetId { get; set; } = string.Empty;

    /// <summary>
    /// Rule set name
    /// </summary>
    public string Name { get; set; } = string.Empty;

    /// <summary>
    /// Description
    /// </summary>
    public string Description { get; set; } = string.Empty;

    /// <summary>
    /// Scenario ID this rule set applies to (optional)
    /// </summary>
    public string? ScenarioId { get; set; }

    /// <summary>
    /// Minimum passing score
    /// </summary>
    public double PassingScore { get; set; } = 70.0;

    /// <summary>
    /// KPI rules in this set
    /// </summary>
    public List<KpiRule> Rules { get; set; } = new();

    /// <summary>
    /// When rule set was created
    /// </summary>
    public DateTime CreatedAt { get; set; }

    /// <summary>
    /// Last modified timestamp
    /// </summary>
    public DateTime? UpdatedAt { get; set; }
}
