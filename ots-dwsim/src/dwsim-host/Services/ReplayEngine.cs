using DWSIM.OTS.SimulationHost.Models;
using System.Collections.Concurrent;

namespace DWSIM.OTS.SimulationHost.Services;

/// <summary>
/// Engine for replaying recorded simulation sessions with determinism verification
/// </summary>
public class ReplayEngine : IReplayEngine
{
    private readonly ILogger<ReplayEngine> _logger;
    private readonly ISessionManager _sessionManager;
    private readonly ISnapshotManager _snapshotManager;
    private readonly ITimeSeriesLogger _timeSeriesLogger;
    private readonly ConcurrentDictionary<string, ReplayState> _activeReplays = new();

    public ReplayEngine(
        ILogger<ReplayEngine> logger,
        ISessionManager sessionManager,
        ISnapshotManager snapshotManager,
        ITimeSeriesLogger timeSeriesLogger)
    {
        _logger = logger;
        _sessionManager = sessionManager;
        _snapshotManager = snapshotManager;
        _timeSeriesLogger = timeSeriesLogger;
    }

    public async Task<ReplayResponse> StartReplayAsync(ReplayRequest request)
    {
        _logger.LogInformation("Starting replay for session {SessionId}", request.OriginalSessionId);

        var replayId = Guid.NewGuid().ToString();

        // Get original session info
        var originalSession = await _sessionManager.GetSessionAsync(request.OriginalSessionId);
        if (originalSession == null)
        {
            throw new InvalidOperationException($"Original session {request.OriginalSessionId} not found");
        }

        // Create replay state
        var replayState = new ReplayState
        {
            ReplayId = replayId,
            OriginalSessionId = request.OriginalSessionId,
            Status = ReplayStatus.Initializing,
            CreatedAt = DateTime.UtcNow,
            Request = request
        };

        _activeReplays[replayId] = replayState;

        // Start replay in background
        _ = Task.Run(async () => await ExecuteReplayAsync(replayState));

        return new ReplayResponse
        {
            ReplayId = replayId,
            OriginalSessionId = request.OriginalSessionId,
            Status = ReplayStatus.Initializing,
            CreatedAt = replayState.CreatedAt,
            ReplaySessionId = string.Empty, // Will be set when session is created
            TotalEvents = 0,
            ProcessedEvents = 0,
            ProgressPercent = 0
        };
    }

    public async Task<ReplayResponse?> GetReplayStatusAsync(string replayId)
    {
        if (!_activeReplays.TryGetValue(replayId, out var state))
        {
            return null;
        }

        return new ReplayResponse
        {
            ReplayId = state.ReplayId,
            OriginalSessionId = state.OriginalSessionId,
            ReplaySessionId = state.ReplaySessionId ?? string.Empty,
            Status = state.Status,
            CreatedAt = state.CreatedAt,
            TotalEvents = state.TotalEvents,
            ProcessedEvents = state.ProcessedEvents,
            ProgressPercent = state.TotalEvents > 0
                ? (double)state.ProcessedEvents / state.TotalEvents * 100.0
                : 0,
            ErrorMessage = state.ErrorMessage
        };
    }

    public async Task<ReplayComparison?> GetComparisonAsync(string replayId)
    {
        if (!_activeReplays.TryGetValue(replayId, out var state))
        {
            return null;
        }

        if (state.Comparison == null)
        {
            // Generate comparison if not already done
            await GenerateComparisonAsync(state);
        }

        return state.Comparison;
    }

    public async Task<bool> CancelReplayAsync(string replayId)
    {
        if (!_activeReplays.TryGetValue(replayId, out var state))
        {
            return false;
        }

        state.CancellationTokenSource?.Cancel();
        state.Status = ReplayStatus.Cancelled;

        _logger.LogInformation("Replay {ReplayId} cancelled", replayId);
        return true;
    }

    public async Task<List<ReplayResponse>> GetReplaysForSessionAsync(string sessionId)
    {
        var replays = _activeReplays.Values
            .Where(r => r.OriginalSessionId == sessionId)
            .Select(r => new ReplayResponse
            {
                ReplayId = r.ReplayId,
                OriginalSessionId = r.OriginalSessionId,
                ReplaySessionId = r.ReplaySessionId ?? string.Empty,
                Status = r.Status,
                CreatedAt = r.CreatedAt,
                TotalEvents = r.TotalEvents,
                ProcessedEvents = r.ProcessedEvents,
                ProgressPercent = r.TotalEvents > 0
                    ? (double)r.ProcessedEvents / r.TotalEvents * 100.0
                    : 0,
                ErrorMessage = r.ErrorMessage
            })
            .ToList();

        return replays;
    }

    private async Task ExecuteReplayAsync(ReplayState state)
    {
        try
        {
            state.Status = ReplayStatus.Running;
            state.CancellationTokenSource = new CancellationTokenSource();
            var cancellationToken = state.CancellationTokenSource.Token;

            // Step 1: Load original session data
            var originalSession = await _sessionManager.GetSessionAsync(state.OriginalSessionId);
            if (originalSession == null)
            {
                throw new InvalidOperationException("Original session not found");
            }

            _logger.LogInformation("Loading time-series data for session {SessionId}", state.OriginalSessionId);

            // Step 2: Get time-series stats to determine time range
            var stats = await _timeSeriesLogger.GetStatsAsync(state.OriginalSessionId);
            if (!stats.FirstSample.HasValue || !stats.LastSample.HasValue)
            {
                throw new InvalidOperationException("No time-series data found for original session");
            }

            var startTime = state.Request.StartTime ?? stats.FirstSample.Value;
            var endTime = state.Request.EndTime ?? stats.LastSample.Value;

            // Step 3: Query all tag data from original session
            var tagsToCompare = state.Request.TagsToCompare;
            if (tagsToCompare.Count == 0)
            {
                // Get all unique tags from the session
                tagsToCompare = await GetAllTagsForSessionAsync(state.OriginalSessionId);
            }

            _logger.LogInformation("Replaying {TagCount} tags from {Start} to {End}",
                tagsToCompare.Count, startTime, endTime);

            var queryRequest = new TimeSeriesQueryRequest
            {
                SessionId = state.OriginalSessionId,
                TagPaths = tagsToCompare,
                StartTime = startTime,
                EndTime = endTime,
                Aggregation = "none"
            };

            var originalData = await _timeSeriesLogger.QueryAsync(queryRequest);
            state.OriginalData = originalData;

            // Step 4: Create new replay session
            _logger.LogInformation("Creating replay session from flowsheet {FlowsheetId}",
                originalSession.FlowsheetId);

            var createRequest = new CreateSessionRequest
            {
                FlowsheetId = originalSession.FlowsheetId,
                SessionName = $"Replay of {originalSession.SessionName}",
                Description = $"Replay of session {state.OriginalSessionId}"
            };

            var replaySession = await _sessionManager.CreateSessionAsync(createRequest);
            state.ReplaySessionId = replaySession.SessionId;

            _logger.LogInformation("Created replay session {ReplaySessionId}", state.ReplaySessionId);

            // Step 5: Restore initial snapshot if available
            var snapshots = await _snapshotManager.ListSnapshotsAsync(state.OriginalSessionId);
            var initialSnapshot = snapshots
                .OrderBy(s => s.CreatedAt)
                .FirstOrDefault();

            if (initialSnapshot != null)
            {
                _logger.LogInformation("Restoring initial snapshot {SnapshotId}", initialSnapshot.SnapshotId);
                await _sessionManager.RestoreSnapshotAsync(state.ReplaySessionId, initialSnapshot.SnapshotId);
            }

            // Step 6: Start replay session
            await _sessionManager.StartSessionAsync(state.ReplaySessionId);

            // Step 7: Apply tag writes at their original sim times
            // For simplicity, we'll assume all data points represent the state evolution
            // In a full implementation, we would reconstruct operator actions from event logs

            state.TotalEvents = originalData.Sum(d => d.Points.Count);
            state.ProcessedEvents = 0;

            _logger.LogInformation("Replay has {TotalEvents} data points to process", state.TotalEvents);

            // Wait for simulation to reach the end time
            var duration = endTime - startTime;
            var adjustedDuration = TimeSpan.FromSeconds(duration.TotalSeconds / state.Request.SpeedMultiplier);

            _logger.LogInformation("Waiting {Duration} for replay to complete (speed {Speed}x)",
                adjustedDuration, state.Request.SpeedMultiplier);

            // In a real implementation, we would:
            // 1. Parse event logs (operator actions, scenario events)
            // 2. Apply them at their original sim_time
            // 3. Let the simulation run
            // 4. Collect output data
            // For now, we'll just run the simulation and collect data

            await Task.Delay(adjustedDuration, cancellationToken);

            state.ProcessedEvents = state.TotalEvents;

            // Step 8: Stop replay session
            await _sessionManager.StopSessionAsync(state.ReplaySessionId);

            _logger.LogInformation("Replay session {ReplaySessionId} completed", state.ReplaySessionId);

            // Step 9: Generate comparison
            if (state.Request.VerifyDeterminism)
            {
                await GenerateComparisonAsync(state);
            }

            state.Status = ReplayStatus.Completed;
            _logger.LogInformation("Replay {ReplayId} completed successfully", state.ReplayId);
        }
        catch (OperationCanceledException)
        {
            state.Status = ReplayStatus.Cancelled;
            _logger.LogInformation("Replay {ReplayId} was cancelled", state.ReplayId);
        }
        catch (Exception ex)
        {
            state.Status = ReplayStatus.Failed;
            state.ErrorMessage = ex.Message;
            _logger.LogError(ex, "Replay {ReplayId} failed", state.ReplayId);
        }
    }

    private async Task GenerateComparisonAsync(ReplayState state)
    {
        if (string.IsNullOrEmpty(state.ReplaySessionId))
        {
            _logger.LogWarning("Cannot generate comparison: replay session not created");
            return;
        }

        _logger.LogInformation("Generating comparison for replay {ReplayId}", state.ReplayId);

        // Query replay session data
        var stats = await _timeSeriesLogger.GetStatsAsync(state.OriginalSessionId);
        var startTime = state.Request.StartTime ?? stats.FirstSample.Value;
        var endTime = state.Request.EndTime ?? stats.LastSample.Value;

        var tagsToCompare = state.Request.TagsToCompare;
        if (tagsToCompare.Count == 0 && state.OriginalData != null)
        {
            tagsToCompare = state.OriginalData.Select(d => d.TagPath).ToList();
        }

        var replayQueryRequest = new TimeSeriesQueryRequest
        {
            SessionId = state.ReplaySessionId,
            TagPaths = tagsToCompare,
            StartTime = startTime,
            EndTime = endTime,
            Aggregation = "none"
        };

        var replayData = await _timeSeriesLogger.QueryAsync(replayQueryRequest);

        // Perform comparison
        var comparison = new ReplayComparison
        {
            ReplayId = state.ReplayId,
            OriginalSessionId = state.OriginalSessionId,
            ReplaySessionId = state.ReplaySessionId,
            GeneratedAt = DateTime.UtcNow,
            MaxAllowedDeviation = 0.2 // 0.2% max deviation for determinism
        };

        var tagComparisons = new List<TagComparison>();

        foreach (var originalTag in state.OriginalData ?? new List<TimeSeriesQueryResult>())
        {
            var replayTag = replayData.FirstOrDefault(r => r.TagPath == originalTag.TagPath);
            if (replayTag == null)
            {
                _logger.LogWarning("Tag {TagPath} not found in replay data", originalTag.TagPath);
                continue;
            }

            var tagComparison = CompareTagData(originalTag, replayTag);
            tagComparisons.Add(tagComparison);
        }

        comparison.TagComparisons = tagComparisons;

        // Calculate overall statistics
        comparison.Stats = new ComparisonStats
        {
            TotalTagsCompared = tagComparisons.Count,
            TagsPassing = tagComparisons.Count(t => t.PassesDeterminismCheck),
            TagsFailing = tagComparisons.Count(t => !t.PassesDeterminismCheck),
            TotalDataPointsCompared = tagComparisons.Sum(t => t.DataPointsCompared),
            AverageCorrelation = tagComparisons.Any()
                ? tagComparisons.Average(t => t.Correlation)
                : 0,
            AverageRMSE = tagComparisons.Any()
                ? tagComparisons.Average(t => t.RootMeanSquareError)
                : 0
        };

        // Calculate overall determinism score
        comparison.DeterminismScore = comparison.Stats.AverageCorrelation;
        comparison.IsDeterministic = comparison.Stats.TagsFailing == 0 &&
            tagComparisons.All(t => t.MaxDeviationPercent <= comparison.MaxAllowedDeviation);

        state.Comparison = comparison;

        _logger.LogInformation(
            "Comparison complete: {Passing}/{Total} tags pass determinism check (score: {Score:F4})",
            comparison.Stats.TagsPassing,
            comparison.Stats.TotalTagsCompared,
            comparison.DeterminismScore);
    }

    private TagComparison CompareTagData(TimeSeriesQueryResult original, TimeSeriesQueryResult replay)
    {
        var comparison = new TagComparison
        {
            TagPath = original.TagPath
        };

        // Align time series by timestamp
        var originalPoints = original.Points.OrderBy(p => p.Time).ToList();
        var replayPoints = replay.Points.OrderBy(p => p.Time).ToList();

        if (originalPoints.Count == 0 || replayPoints.Count == 0)
        {
            return comparison;
        }

        // For simplicity, compare point-by-point (assumes same sampling rate)
        var minCount = Math.Min(originalPoints.Count, replayPoints.Count);
        comparison.DataPointsCompared = minCount;

        var errors = new List<double>();
        var squaredErrors = new List<double>();
        var deviationPercents = new List<double>();

        for (int i = 0; i < minCount; i++)
        {
            var origValue = originalPoints[i].Value;
            var replayValue = replayPoints[i].Value;

            var error = Math.Abs(origValue - replayValue);
            errors.Add(error);
            squaredErrors.Add(error * error);

            if (Math.Abs(origValue) > 1e-10)
            {
                var deviationPercent = (error / Math.Abs(origValue)) * 100.0;
                deviationPercents.Add(deviationPercent);
            }
        }

        // Calculate statistics
        comparison.MeanAbsoluteError = errors.Any() ? errors.Average() : 0;
        comparison.RootMeanSquareError = squaredErrors.Any()
            ? Math.Sqrt(squaredErrors.Average())
            : 0;
        comparison.MaxDeviation = errors.Any() ? errors.Max() : 0;
        comparison.MaxDeviationPercent = deviationPercents.Any()
            ? deviationPercents.Max()
            : 0;

        // Calculate correlation
        comparison.Correlation = CalculateCorrelation(
            originalPoints.Take(minCount).Select(p => p.Value).ToList(),
            replayPoints.Take(minCount).Select(p => p.Value).ToList()
        );

        // Determinism check: max deviation < 0.2%
        comparison.PassesDeterminismCheck = comparison.MaxDeviationPercent <= 0.2;

        return comparison;
    }

    private double CalculateCorrelation(List<double> x, List<double> y)
    {
        if (x.Count != y.Count || x.Count == 0)
            return 0;

        var n = x.Count;
        var meanX = x.Average();
        var meanY = y.Average();

        var numerator = 0.0;
        var denomX = 0.0;
        var denomY = 0.0;

        for (int i = 0; i < n; i++)
        {
            var dx = x[i] - meanX;
            var dy = y[i] - meanY;
            numerator += dx * dy;
            denomX += dx * dx;
            denomY += dy * dy;
        }

        if (denomX == 0 || denomY == 0)
            return 0;

        return numerator / Math.Sqrt(denomX * denomY);
    }

    private async Task<List<string>> GetAllTagsForSessionAsync(string sessionId)
    {
        // Query unique tag paths from time-series database
        // For now, return a default list
        // In full implementation, this would query the database for distinct tag_path values

        return new List<string>
        {
            "Streams.Feed.Temperature",
            "Streams.Feed.Flow",
            "Streams.Feed.Pressure",
            "Streams.Feed.MolarFlow",
            "Streams.Feed.MassFlow",
            "Units.Column1.DutyRequired",
            "Units.Column1.Temperature",
            "Units.Column1.Pressure"
        };
    }

    /// <summary>
    /// Internal state for tracking replay execution
    /// </summary>
    private class ReplayState
    {
        public string ReplayId { get; set; } = string.Empty;
        public string OriginalSessionId { get; set; } = string.Empty;
        public string? ReplaySessionId { get; set; }
        public ReplayStatus Status { get; set; }
        public DateTime CreatedAt { get; set; }
        public int TotalEvents { get; set; }
        public int ProcessedEvents { get; set; }
        public string? ErrorMessage { get; set; }
        public ReplayRequest Request { get; set; } = new();
        public List<TimeSeriesQueryResult>? OriginalData { get; set; }
        public ReplayComparison? Comparison { get; set; }
        public CancellationTokenSource? CancellationTokenSource { get; set; }
    }
}
