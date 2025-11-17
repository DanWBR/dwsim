namespace DWSIM.OTS.SimulationHost.Models;

public enum SessionStatus
{
    Created,
    Running,
    Paused,
    Stopped,
    Error
}

public class CreateSessionRequest
{
    public required string Flowsheet { get; set; }
    public string SessionName { get; set; } = string.Empty;
    public int Seed { get; set; } = 12345;
    public Dictionary<string, object>? Environment { get; set; }
    public int MaxRealTimeSec { get; set; } = 3600;
}

public class CreateSessionResponse
{
    public required string SessionId { get; set; }
    public SessionStatus Status { get; set; }
    public DateTime CreatedAt { get; set; }
}

public class SessionInfo
{
    public required string SessionId { get; set; }
    public required string SessionName { get; set; }
    public SessionStatus Status { get; set; }
    public DateTime CreatedAt { get; set; }
    public DateTime? StartedAt { get; set; }
    public DateTime? StoppedAt { get; set; }
    public DateTime SimTime { get; set; }
    public double TimeFactor { get; set; } = 1.0;
    public required string FlowsheetPath { get; set; }
}

public class StartSessionRequest
{
    public string StartMode { get; set; } = "run";
    public double TimeStepSeconds { get; set; } = 0.5;
}

public class StartSessionResponse
{
    public required string SessionId { get; set; }
    public SessionStatus Status { get; set; }
    public DateTime SimTime { get; set; }
    public double TimeFactor { get; set; }
}

public class SnapshotRequest
{
    public required string Name { get; set; }
}

public class SnapshotResponse
{
    public required string SnapshotId { get; set; }
    public DateTime SavedAt { get; set; }
}

public class RestoreSnapshotRequest
{
    public required string SnapshotId { get; set; }
}

public class TimeStepRequest
{
    public double Seconds { get; set; }
}

public class TimeFactorRequest
{
    public double Factor { get; set; } = 1.0;
}

public class TimeFactorResponse
{
    public double TimeFactor { get; set; }
}

public class TagValue
{
    public required string Tag { get; set; }
    public object? Value { get; set; }
    public string? Units { get; set; }
    public DateTime SimTime { get; set; }
}

public class WriteTagRequest
{
    public required object Value { get; set; }
    public string User { get; set; } = "system";
    public string Mode { get; set; } = "manual";
}

public class WriteTagResponse
{
    public string Status { get; set; } = "ok";
    public object? AppliedValue { get; set; }
}

public class RunScenarioRequest
{
    public required string ScenarioId { get; set; }
    public bool Autostart { get; set; } = true;
}

public class RunScenarioResponse
{
    public string Status { get; set; } = "scheduled";
    public required string ScenarioRunId { get; set; }
}

public class EventLogEntry
{
    public required string Type { get; set; }
    public string? User { get; set; }
    public string? Action { get; set; }
    public string? Target { get; set; }
    public object? Value { get; set; }
    public DateTime SimTime { get; set; }
    public DateTime RealTime { get; set; }
}

public class HealthResponse
{
    public long Uptime { get; set; }
    public int SessionsActive { get; set; }
}
