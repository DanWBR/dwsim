using DWSIM.OTS.SimulationHost.Models;
using DWSIM.Interfaces;
using System.Collections.Concurrent;
using System.IO.Compression;
using System.Text.Json;

namespace DWSIM.OTS.SimulationHost.Services;

public class SnapshotManager : ISnapshotManager
{
    private readonly ILogger<SnapshotManager> _logger;
    private readonly IConfiguration _configuration;
    private readonly string _snapshotBasePath;
    private readonly ConcurrentDictionary<string, Snapshot> _snapshotIndex = new();

    public SnapshotManager(ILogger<SnapshotManager> logger, IConfiguration configuration)
    {
        _logger = logger;
        _configuration = configuration;

        _snapshotBasePath = _configuration["SnapshotPath"]
            ?? Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "snapshots");

        if (!Directory.Exists(_snapshotBasePath))
        {
            Directory.CreateDirectory(_snapshotBasePath);
            _logger.LogInformation("Created snapshot directory: {Path}", _snapshotBasePath);
        }

        // Load existing snapshot index
        LoadSnapshotIndex();
    }

    public async Task<Snapshot> CreateSnapshotAsync(string sessionId, string name, IFlowsheet flowsheet)
    {
        try
        {
            var snapshotId = $"snap-{Guid.NewGuid().ToString()[..8]}";
            var timestamp = DateTime.UtcNow;

            _logger.LogInformation("Creating snapshot {SnapshotId} for session {SessionId}", snapshotId, sessionId);

            // Create session-specific directory
            var sessionDir = Path.Combine(_snapshotBasePath, sessionId);
            if (!Directory.Exists(sessionDir))
            {
                Directory.CreateDirectory(sessionDir);
            }

            var snapshotPath = Path.Combine(sessionDir, $"{snapshotId}.dwxmz");

            // Serialize flowsheet using DWSIM's built-in serialization
            await Task.Run(() =>
            {
                try
                {
                    // DWSIM uses XML serialization - we'll use the automation interface
                    // to save the flowsheet in compressed format
                    var automationInterface = new DWSIM.Automation.Automation2();
                    automationInterface.SaveFlowsheet(flowsheet, snapshotPath, true);

                    _logger.LogInformation("Flowsheet serialized to {Path}", snapshotPath);
                }
                catch (Exception ex)
                {
                    _logger.LogError(ex, "Error serializing flowsheet");
                    throw;
                }
            });

            // Get file size
            var fileInfo = new FileInfo(snapshotPath);
            var sizeBytes = fileInfo.Exists ? fileInfo.Length : 0;

            // Create snapshot metadata
            var snapshot = new Snapshot
            {
                SnapshotId = snapshotId,
                SessionId = sessionId,
                Name = name,
                CreatedAt = timestamp,
                SimTime = timestamp, // TODO: Get actual sim time from flowsheet
                SizeBytes = sizeBytes,
                FilePath = snapshotPath,
                Metadata = new Dictionary<string, object>
                {
                    { "flowsheet_name", flowsheet.Options.SimulationName ?? "Unknown" },
                    { "unit_count", flowsheet.SimulationObjects?.Count ?? 0 },
                    { "stream_count", flowsheet.SimulationObjects?.Values.Count(o => o is DWSIM.Interfaces.IFlowsheet) ?? 0 }
                }
            };

            // Save metadata
            await SaveSnapshotMetadataAsync(snapshot);

            // Add to index
            _snapshotIndex.TryAdd(snapshotId, snapshot);

            _logger.LogInformation("Created snapshot {SnapshotId}: {Name} ({Size} bytes)",
                snapshotId, name, sizeBytes);

            return snapshot;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error creating snapshot for session {SessionId}", sessionId);
            throw;
        }
    }

    public async Task<IFlowsheet?> RestoreSnapshotAsync(string snapshotId)
    {
        try
        {
            if (!_snapshotIndex.TryGetValue(snapshotId, out var snapshot))
            {
                _logger.LogWarning("Snapshot not found: {SnapshotId}", snapshotId);
                return null;
            }

            if (!File.Exists(snapshot.FilePath))
            {
                _logger.LogError("Snapshot file not found: {Path}", snapshot.FilePath);
                return null;
            }

            _logger.LogInformation("Restoring snapshot {SnapshotId} from {Path}",
                snapshotId, snapshot.FilePath);

            IFlowsheet? flowsheet = null;

            await Task.Run(() =>
            {
                try
                {
                    // Load flowsheet using DWSIM automation interface
                    var automationInterface = new DWSIM.Automation.Automation2();
                    flowsheet = automationInterface.LoadFlowsheet(snapshot.FilePath);

                    _logger.LogInformation("Flowsheet restored from snapshot {SnapshotId}", snapshotId);
                }
                catch (Exception ex)
                {
                    _logger.LogError(ex, "Error deserializing flowsheet from snapshot");
                    throw;
                }
            });

            return flowsheet;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error restoring snapshot {SnapshotId}", snapshotId);
            throw;
        }
    }

    public Task<Snapshot?> GetSnapshotAsync(string snapshotId)
    {
        _snapshotIndex.TryGetValue(snapshotId, out var snapshot);
        return Task.FromResult(snapshot);
    }

    public Task<List<Snapshot>> GetSnapshotsForSessionAsync(string sessionId)
    {
        var snapshots = _snapshotIndex.Values
            .Where(s => s.SessionId == sessionId)
            .OrderByDescending(s => s.CreatedAt)
            .ToList();

        return Task.FromResult(snapshots);
    }

    public async Task DeleteSnapshotAsync(string snapshotId)
    {
        try
        {
            if (!_snapshotIndex.TryGetValue(snapshotId, out var snapshot))
            {
                _logger.LogWarning("Snapshot not found: {SnapshotId}", snapshotId);
                return;
            }

            // Delete file
            if (File.Exists(snapshot.FilePath))
            {
                File.Delete(snapshot.FilePath);
                _logger.LogInformation("Deleted snapshot file: {Path}", snapshot.FilePath);
            }

            // Delete metadata
            var metadataPath = GetMetadataPath(snapshotId);
            if (File.Exists(metadataPath))
            {
                File.Delete(metadataPath);
            }

            // Remove from index
            _snapshotIndex.TryRemove(snapshotId, out _);

            _logger.LogInformation("Deleted snapshot {SnapshotId}", snapshotId);

            await Task.CompletedTask;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error deleting snapshot {SnapshotId}", snapshotId);
            throw;
        }
    }

    public Task<SnapshotStats> GetStatsAsync()
    {
        var snapshots = _snapshotIndex.Values.ToList();

        var stats = new SnapshotStats
        {
            TotalSnapshots = snapshots.Count,
            TotalSizeBytes = snapshots.Sum(s => s.SizeBytes),
            OldestSnapshot = snapshots.Any() ? snapshots.Min(s => s.CreatedAt) : null,
            NewestSnapshot = snapshots.Any() ? snapshots.Max(s => s.CreatedAt) : null
        };

        return Task.FromResult(stats);
    }

    private async Task SaveSnapshotMetadataAsync(Snapshot snapshot)
    {
        try
        {
            var metadataPath = GetMetadataPath(snapshot.SnapshotId);
            var json = JsonSerializer.Serialize(snapshot, new JsonSerializerOptions
            {
                WriteIndented = true
            });

            await File.WriteAllTextAsync(metadataPath, json);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error saving snapshot metadata");
        }
    }

    private void LoadSnapshotIndex()
    {
        try
        {
            if (!Directory.Exists(_snapshotBasePath))
            {
                return;
            }

            // Find all metadata files
            var metadataFiles = Directory.GetFiles(_snapshotBasePath, "*.meta.json", SearchOption.AllDirectories);

            foreach (var file in metadataFiles)
            {
                try
                {
                    var json = File.ReadAllText(file);
                    var snapshot = JsonSerializer.Deserialize<Snapshot>(json);

                    if (snapshot != null && File.Exists(snapshot.FilePath))
                    {
                        _snapshotIndex.TryAdd(snapshot.SnapshotId, snapshot);
                    }
                }
                catch (Exception ex)
                {
                    _logger.LogWarning(ex, "Error loading snapshot metadata from {File}", file);
                }
            }

            _logger.LogInformation("Loaded {Count} snapshots from index", _snapshotIndex.Count);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error loading snapshot index");
        }
    }

    private string GetMetadataPath(string snapshotId)
    {
        return Path.Combine(_snapshotBasePath, $"{snapshotId}.meta.json");
    }
}
