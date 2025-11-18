namespace DWSIM.OTS.SimulationHost.Models;

/// <summary>
/// Process variable time-series data point
/// </summary>
public class ProcessVariableDataPoint
{
    public DateTime Time { get; set; }
    public required string SessionId { get; set; }
    public required string TagPath { get; set; }
    public double? Value { get; set; }
    public string? Units { get; set; }
    public DateTime SimTime { get; set; }
    public string Quality { get; set; } = "good";
}

/// <summary>
/// Time-series query request
/// </summary>
public class TimeSeriesQueryRequest
{
    public required string SessionId { get; set; }
    public List<string> TagPaths { get; set; } = new();
    public DateTime? StartTime { get; set; }
    public DateTime? EndTime { get; set; }
    public string Aggregation { get; set; } = "raw"; // raw, 1min, 1hour
    public int? Limit { get; set; }
}

/// <summary>
/// Time-series query result
/// </summary>
public class TimeSeriesQueryResult
{
    public required string TagPath { get; set; }
    public List<TimeSeriesPoint> Points { get; set; } = new();
    public string? Units { get; set; }
}

/// <summary>
/// Single time-series point
/// </summary>
public class TimeSeriesPoint
{
    public DateTime Time { get; set; }
    public double Value { get; set; }
    public DateTime? SimTime { get; set; }
}

/// <summary>
/// Aggregated time-series point
/// </summary>
public class AggregatedTimeSeriesPoint
{
    public DateTime Bucket { get; set; }
    public double AvgValue { get; set; }
    public double MinValue { get; set; }
    public double MaxValue { get; set; }
    public double? StdDevValue { get; set; }
    public int SampleCount { get; set; }
}

/// <summary>
/// Time-series statistics
/// </summary>
public class TimeSeriesStats
{
    public required string SessionId { get; set; }
    public int TotalTags { get; set; }
    public long TotalDataPoints { get; set; }
    public DateTime? FirstSample { get; set; }
    public DateTime? LastSample { get; set; }
    public TimeSpan? Duration { get; set; }
}
