namespace DWSIM.OTS.SimulationHost.Models.Scenario;

/// <summary>
/// Complete scenario definition matching the JSON schema
/// </summary>
public class ScenarioDefinition
{
    public required string ScenarioId { get; set; }
    public string? Title { get; set; }
    public required string Description { get; set; }
    public string? Author { get; set; }
    public int Seed { get; set; }
    public Dictionary<string, object> InitialState { get; set; } = new();
    public List<ScenarioEvent> Events { get; set; } = new();
    public ScenarioMetadata? Metadata { get; set; }
}

/// <summary>
/// A single event in the scenario timeline
/// </summary>
public class ScenarioEvent
{
    public double TimeS { get; set; }
    public required string Type { get; set; }
    public string? Target { get; set; }
    public object? Payload { get; set; }
    public EventRepeat? Repeat { get; set; }
}

/// <summary>
/// Event repeat configuration
/// </summary>
public class EventRepeat
{
    public double IntervalS { get; set; }
    public int? Count { get; set; }
}

/// <summary>
/// Scenario metadata and configuration
/// </summary>
public class ScenarioMetadata
{
    public double? RecommendedTimeFactor { get; set; }
    public int? ExpectedDurationSeconds { get; set; }
    public string? Difficulty { get; set; }
    public List<string>? Tags { get; set; }
    public List<string>? LearningObjectives { get; set; }
    public string? RequiredFlowsheet { get; set; }
    public PassCriteria? PassCriteria { get; set; }
}

/// <summary>
/// Assessment pass criteria
/// </summary>
public class PassCriteria
{
    public int? MaxAlarms { get; set; }
    public int? MaxTimeS { get; set; }
    public List<string>? RequiredActions { get; set; }
    public Dictionary<string, TargetValue>? TargetValues { get; set; }
}

/// <summary>
/// Target value with tolerance for assessment
/// </summary>
public class TargetValue
{
    public double Value { get; set; }
    public double? Tolerance { get; set; }
}

/// <summary>
/// Scenario execution status
/// </summary>
public enum ScenarioStatus
{
    NotStarted,
    Running,
    Paused,
    Completed,
    Failed,
    Stopped
}

/// <summary>
/// Active scenario run instance
/// </summary>
public class ScenarioRun
{
    public required string ScenarioRunId { get; set; }
    public required string SessionId { get; set; }
    public required string ScenarioId { get; set; }
    public ScenarioStatus Status { get; set; }
    public DateTime StartedAt { get; set; }
    public DateTime? CompletedAt { get; set; }
    public DateTime CurrentSimTime { get; set; }
    public int CurrentEventIndex { get; set; }
    public List<string> Errors { get; set; } = new();
    public Dictionary<string, object> State { get; set; } = new();
}

/// <summary>
/// Scenario validation result
/// </summary>
public class ScenarioValidationResult
{
    public bool IsValid { get; set; }
    public List<string> Errors { get; set; } = new();
    public List<string> Warnings { get; set; } = new();
}
