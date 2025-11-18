using DWSIM.OTS.SimulationHost.Models;

namespace DWSIM.OTS.SimulationHost.Services;

/// <summary>
/// Background service that automatically logs process variables to TimescaleDB
/// </summary>
public class TimeSeriesLoggerService : BackgroundService
{
    private readonly ILogger<TimeSeriesLoggerService> _logger;
    private readonly IServiceProvider _serviceProvider;
    private readonly TimeSpan _pollingInterval;

    public TimeSeriesLoggerService(
        ILogger<TimeSeriesLoggerService> logger,
        IServiceProvider serviceProvider,
        IConfiguration configuration)
    {
        _logger = logger;
        _serviceProvider = serviceProvider;

        // Get polling interval from configuration (default 1 second)
        var intervalMs = configuration.GetValue<int?>("TimeSeriesLogging:PollingIntervalMs") ?? 1000;
        _pollingInterval = TimeSpan.FromMilliseconds(intervalMs);
    }

    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        _logger.LogInformation("Time-series logger service starting with polling interval {Interval}ms",
            _pollingInterval.TotalMilliseconds);

        // Wait a bit for the application to fully start
        await Task.Delay(TimeSpan.FromSeconds(5), stoppingToken);

        while (!stoppingToken.IsCancellationRequested)
        {
            try
            {
                await PollAndLogAsync(stoppingToken);
                await Task.Delay(_pollingInterval, stoppingToken);
            }
            catch (OperationCanceledException)
            {
                // Expected when stopping
                break;
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error in time-series logging loop");
                await Task.Delay(TimeSpan.FromSeconds(5), stoppingToken);
            }
        }

        _logger.LogInformation("Time-series logger service stopped");
    }

    private async Task PollAndLogAsync(CancellationToken cancellationToken)
    {
        using var scope = _serviceProvider.CreateScope();
        var sessionManager = scope.ServiceProvider.GetRequiredService<ISessionManager>();
        var timeSeriesLogger = scope.ServiceProvider.GetRequiredService<ITimeSeriesLogger>();

        // Check if TimescaleDB is connected
        if (!await timeSeriesLogger.IsConnectedAsync())
        {
            _logger.LogDebug("TimescaleDB not connected, skipping data logging");
            return;
        }

        // Get all active sessions
        var sessions = await sessionManager.GetAllSessionsAsync();
        var activeSessions = sessions.Where(s => s.Status == SessionStatus.Running).ToList();

        if (activeSessions.Count == 0)
        {
            return; // No active sessions
        }

        _logger.LogDebug("Polling {Count} active sessions for time-series data", activeSessions.Count);

        // Define tags to log (could be made configurable)
        var tagsToLog = new[]
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

        var dataPoints = new List<ProcessVariableDataPoint>();
        var now = DateTime.UtcNow;

        foreach (var session in activeSessions)
        {
            foreach (var tagPath in tagsToLog)
            {
                try
                {
                    var tagValue = await sessionManager.ReadTagAsync(session.SessionId, tagPath);

                    if (tagValue != null && tagValue.Value is double numericValue)
                    {
                        dataPoints.Add(new ProcessVariableDataPoint
                        {
                            Time = now,
                            SessionId = session.SessionId,
                            TagPath = tagPath,
                            Value = numericValue,
                            Units = tagValue.Units,
                            SimTime = session.SimTime,
                            Quality = "good"
                        });
                    }
                }
                catch (Exception ex)
                {
                    _logger.LogDebug(ex, "Error reading tag {TagPath} from session {SessionId}",
                        tagPath, session.SessionId);
                }
            }
        }

        if (dataPoints.Count > 0)
        {
            await timeSeriesLogger.LogBatchAsync(dataPoints);
            _logger.LogDebug("Logged {Count} data points to TimescaleDB", dataPoints.Count);
        }
    }
}
