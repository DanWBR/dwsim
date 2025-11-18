namespace DWSIM.OTS.SimulationHost.Models;

/// <summary>
/// Snapshot metadata and information
/// </summary>
public class Snapshot
{
    public required string SnapshotId { get; set; }
    public required string SessionId { get; set; }
    public required string Name { get; set; }
    public DateTime CreatedAt { get; set; }
    public DateTime SimTime { get; set; }
    public long SizeBytes { get; set; }
    public string? Description { get; set; }
    public Dictionary<string, object> Metadata { get; set; } = new();
    public required string FilePath { get; set; }
}

/// <summary>
/// Snapshot statistics
/// </summary>
public class SnapshotStats
{
    public int TotalSnapshots { get; set; }
    public long TotalSizeBytes { get; set; }
    public DateTime? OldestSnapshot { get; set; }
    public DateTime? NewestSnapshot { get; set; }
}
