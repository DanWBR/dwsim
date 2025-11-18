namespace DWSIM.OTS.SimulationHost.Models;

/// <summary>
/// Request to start a replay of a recorded session
/// </summary>
public class ReplayRequest
{
    /// <summary>
    /// Original session ID to replay
    /// </summary>
    public string OriginalSessionId { get; set; } = string.Empty;

    /// <summary>
    /// Optional: Start time for replay (defaults to session start)
    /// </summary>
    public DateTime? StartTime { get; set; }

    /// <summary>
    /// Optional: End time for replay (defaults to session end)
    /// </summary>
    public DateTime? EndTime { get; set; }

    /// <summary>
    /// Speed multiplier (1.0 = real-time, 2.0 = 2x speed, etc.)
    /// </summary>
    public double SpeedMultiplier { get; set; } = 1.0;

    /// <summary>
    /// Tag paths to track for comparison (if empty, tracks all logged tags)
    /// </summary>
    public List<string> TagsToCompare { get; set; } = new();

    /// <summary>
    /// Whether to verify determinism by comparing outputs
    /// </summary>
    public bool VerifyDeterminism { get; set; } = true;
}

/// <summary>
/// Response after starting a replay
/// </summary>
public class ReplayResponse
{
    /// <summary>
    /// Unique replay ID
    /// </summary>
    public string ReplayId { get; set; } = string.Empty;

    /// <summary>
    /// New session ID created for replay
    /// </summary>
    public string ReplaySessionId { get; set; } = string.Empty;

    /// <summary>
    /// Original session ID being replayed
    /// </summary>
    public string OriginalSessionId { get; set; } = string.Empty;

    /// <summary>
    /// Replay status
    /// </summary>
    public ReplayStatus Status { get; set; }

    /// <summary>
    /// Timestamp when replay was created
    /// </summary>
    public DateTime CreatedAt { get; set; }

    /// <summary>
    /// Total number of events to replay
    /// </summary>
    public int TotalEvents { get; set; }

    /// <summary>
    /// Number of events processed so far
    /// </summary>
    public int ProcessedEvents { get; set; }

    /// <summary>
    /// Current progress percentage (0-100)
    /// </summary>
    public double ProgressPercent { get; set; }

    /// <summary>
    /// Error message if replay failed
    /// </summary>
    public string? ErrorMessage { get; set; }
}

/// <summary>
/// Replay status enumeration
/// </summary>
public enum ReplayStatus
{
    Initializing,
    Running,
    Completed,
    Failed,
    Cancelled
}

/// <summary>
/// Comparison result between original and replayed session
/// </summary>
public class ReplayComparison
{
    /// <summary>
    /// Replay ID
    /// </summary>
    public string ReplayId { get; set; } = string.Empty;

    /// <summary>
    /// Original session ID
    /// </summary>
    public string OriginalSessionId { get; set; } = string.Empty;

    /// <summary>
    /// Replay session ID
    /// </summary>
    public string ReplaySessionId { get; set; } = string.Empty;

    /// <summary>
    /// Overall determinism score (0.0 = completely different, 1.0 = identical)
    /// </summary>
    public double DeterminismScore { get; set; }

    /// <summary>
    /// Whether replay passes determinism check (max deviation < threshold)
    /// </summary>
    public bool IsDeterministic { get; set; }

    /// <summary>
    /// Maximum allowed deviation percentage for determinism check
    /// </summary>
    public double MaxAllowedDeviation { get; set; } = 0.2;

    /// <summary>
    /// Tag-by-tag comparison results
    /// </summary>
    public List<TagComparison> TagComparisons { get; set; } = new();

    /// <summary>
    /// Summary statistics
    /// </summary>
    public ComparisonStats Stats { get; set; } = new();

    /// <summary>
    /// Time when comparison was generated
    /// </summary>
    public DateTime GeneratedAt { get; set; }
}

/// <summary>
/// Comparison for a single tag
/// </summary>
public class TagComparison
{
    /// <summary>
    /// Tag path
    /// </summary>
    public string TagPath { get; set; } = string.Empty;

    /// <summary>
    /// Number of data points compared
    /// </summary>
    public int DataPointsCompared { get; set; }

    /// <summary>
    /// Mean absolute error
    /// </summary>
    public double MeanAbsoluteError { get; set; }

    /// <summary>
    /// Root mean square error
    /// </summary>
    public double RootMeanSquareError { get; set; }

    /// <summary>
    /// Maximum deviation observed
    /// </summary>
    public double MaxDeviation { get; set; }

    /// <summary>
    /// Maximum deviation percentage
    /// </summary>
    public double MaxDeviationPercent { get; set; }

    /// <summary>
    /// Correlation coefficient (1.0 = perfect correlation)
    /// </summary>
    public double Correlation { get; set; }

    /// <summary>
    /// Whether this tag passes determinism check
    /// </summary>
    public bool PassesDeterminismCheck { get; set; }
}

/// <summary>
/// Summary statistics for comparison
/// </summary>
public class ComparisonStats
{
    /// <summary>
    /// Total tags compared
    /// </summary>
    public int TotalTagsCompared { get; set; }

    /// <summary>
    /// Number of tags passing determinism check
    /// </summary>
    public int TagsPassing { get; set; }

    /// <summary>
    /// Number of tags failing determinism check
    /// </summary>
    public int TagsFailing { get; set; }

    /// <summary>
    /// Total data points compared across all tags
    /// </summary>
    public long TotalDataPointsCompared { get; set; }

    /// <summary>
    /// Average correlation across all tags
    /// </summary>
    public double AverageCorrelation { get; set; }

    /// <summary>
    /// Average RMSE across all tags
    /// </summary>
    public double AverageRMSE { get; set; }
}

/// <summary>
/// Event log entry from original session
/// </summary>
public class EventLogEntry
{
    /// <summary>
    /// Timestamp when event occurred
    /// </summary>
    public DateTime Time { get; set; }

    /// <summary>
    /// Simulation time when event occurred
    /// </summary>
    public DateTime SimTime { get; set; }

    /// <summary>
    /// Event type (tag_write, scenario_event, etc.)
    /// </summary>
    public string EventType { get; set; } = string.Empty;

    /// <summary>
    /// Tag path affected
    /// </summary>
    public string? TagPath { get; set; }

    /// <summary>
    /// Value written (for tag_write events)
    /// </summary>
    public double? Value { get; set; }

    /// <summary>
    /// Additional event data (JSON)
    /// </summary>
    public string? Data { get; set; }
}
