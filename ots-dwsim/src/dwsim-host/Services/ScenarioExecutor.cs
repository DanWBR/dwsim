using DWSIM.OTS.SimulationHost.Models.Scenario;
using Newtonsoft.Json;
using System.Collections.Concurrent;

namespace DWSIM.OTS.SimulationHost.Services;

public class ScenarioExecutor : IScenarioExecutor
{
    private readonly ILogger<ScenarioExecutor> _logger;
    private readonly ISessionManager _sessionManager;
    private readonly IScenarioValidator _validator;
    private readonly IConfiguration _configuration;
    private readonly ConcurrentDictionary<string, ScenarioRun> _activeRuns = new();
    private readonly ConcurrentDictionary<string, CancellationTokenSource> _runCancellations = new();
    private readonly string _scenarioBasePath;

    public ScenarioExecutor(
        ILogger<ScenarioExecutor> logger,
        ISessionManager sessionManager,
        IScenarioValidator validator,
        IConfiguration configuration)
    {
        _logger = logger;
        _sessionManager = sessionManager;
        _validator = validator;
        _configuration = configuration;

        _scenarioBasePath = _configuration["ScenarioPath"]
            ?? Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "scenarios");

        if (!Directory.Exists(_scenarioBasePath))
        {
            Directory.CreateDirectory(_scenarioBasePath);
            _logger.LogInformation("Created scenario directory: {Path}", _scenarioBasePath);
        }
    }

    public async Task<ScenarioDefinition> LoadScenarioAsync(string scenarioPath)
    {
        try
        {
            var fullPath = ResolveScenarioPath(scenarioPath);

            if (!File.Exists(fullPath))
            {
                throw new FileNotFoundException($"Scenario file not found: {scenarioPath}");
            }

            _logger.LogInformation("Loading scenario from {Path}", fullPath);

            var json = await File.ReadAllTextAsync(fullPath);
            var scenario = JsonConvert.DeserializeObject<ScenarioDefinition>(json);

            if (scenario == null)
            {
                throw new InvalidOperationException("Failed to deserialize scenario");
            }

            // Validate scenario
            var validationResult = await _validator.ValidateAsync(scenario);

            if (!validationResult.IsValid)
            {
                var errors = string.Join(", ", validationResult.Errors);
                throw new InvalidOperationException($"Scenario validation failed: {errors}");
            }

            if (validationResult.Warnings.Count > 0)
            {
                _logger.LogWarning("Scenario loaded with {WarningCount} warnings: {Warnings}",
                    validationResult.Warnings.Count,
                    string.Join(", ", validationResult.Warnings));
            }

            _logger.LogInformation("Loaded scenario {ScenarioId}: {Title}",
                scenario.ScenarioId, scenario.Title ?? scenario.Description);

            return scenario;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error loading scenario from {Path}", scenarioPath);
            throw;
        }
    }

    public async Task<string> RunScenarioAsync(string sessionId, string scenarioId, bool autostart = true)
    {
        try
        {
            // Verify session exists
            var session = await _sessionManager.GetSessionAsync(sessionId);
            if (session == null)
            {
                throw new KeyNotFoundException($"Session not found: {sessionId}");
            }

            // Load scenario
            var scenario = await LoadScenarioAsync(scenarioId);

            // Create scenario run
            var scenarioRunId = $"run-{Guid.NewGuid().ToString()[..8]}";
            var scenarioRun = new ScenarioRun
            {
                ScenarioRunId = scenarioRunId,
                SessionId = sessionId,
                ScenarioId = scenario.ScenarioId,
                Status = ScenarioStatus.NotStarted,
                StartedAt = DateTime.UtcNow,
                CurrentSimTime = session.SimTime,
                CurrentEventIndex = 0
            };

            _activeRuns.TryAdd(scenarioRunId, scenarioRun);

            _logger.LogInformation("Created scenario run {RunId} for session {SessionId}",
                scenarioRunId, sessionId);

            // Apply initial state
            await ApplyInitialStateAsync(sessionId, scenario.InitialState);

            // Start scenario execution
            if (autostart)
            {
                await StartScenarioExecutionAsync(scenarioRunId, scenario, sessionId);
            }

            return scenarioRunId;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error running scenario {ScenarioId} on session {SessionId}",
                scenarioId, sessionId);
            throw;
        }
    }

    public async Task StopScenarioAsync(string scenarioRunId)
    {
        try
        {
            if (!_activeRuns.TryGetValue(scenarioRunId, out var run))
            {
                throw new KeyNotFoundException($"Scenario run not found: {scenarioRunId}");
            }

            // Cancel execution
            if (_runCancellations.TryGetValue(scenarioRunId, out var cts))
            {
                cts.Cancel();
            }

            run.Status = ScenarioStatus.Stopped;
            run.CompletedAt = DateTime.UtcNow;

            _logger.LogInformation("Stopped scenario run {RunId}", scenarioRunId);

            await Task.CompletedTask;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error stopping scenario run {RunId}", scenarioRunId);
            throw;
        }
    }

    public Task<ScenarioRun?> GetScenarioRunAsync(string scenarioRunId)
    {
        _activeRuns.TryGetValue(scenarioRunId, out var run);
        return Task.FromResult(run);
    }

    public Task<List<ScenarioRun>> GetActiveScenarioRunsAsync()
    {
        var runs = _activeRuns.Values
            .Where(r => r.Status == ScenarioStatus.Running || r.Status == ScenarioStatus.Paused)
            .ToList();
        return Task.FromResult(runs);
    }

    private async Task StartScenarioExecutionAsync(string scenarioRunId, ScenarioDefinition scenario, string sessionId)
    {
        var cts = new CancellationTokenSource();
        _runCancellations.TryAdd(scenarioRunId, cts);

        // Execute in background
        _ = Task.Run(async () =>
        {
            try
            {
                await ExecuteScenarioAsync(scenarioRunId, scenario, sessionId, cts.Token);
            }
            catch (OperationCanceledException)
            {
                _logger.LogInformation("Scenario run {RunId} was cancelled", scenarioRunId);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error executing scenario run {RunId}", scenarioRunId);

                if (_activeRuns.TryGetValue(scenarioRunId, out var run))
                {
                    run.Status = ScenarioStatus.Failed;
                    run.Errors.Add(ex.Message);
                    run.CompletedAt = DateTime.UtcNow;
                }
            }
            finally
            {
                _runCancellations.TryRemove(scenarioRunId, out _);
            }
        }, cts.Token);

        await Task.CompletedTask;
    }

    private async Task ExecuteScenarioAsync(
        string scenarioRunId,
        ScenarioDefinition scenario,
        string sessionId,
        CancellationToken cancellationToken)
    {
        if (!_activeRuns.TryGetValue(scenarioRunId, out var run))
        {
            return;
        }

        run.Status = ScenarioStatus.Running;

        _logger.LogInformation("Starting execution of scenario {ScenarioId} with {EventCount} events",
            scenario.ScenarioId, scenario.Events.Count);

        // Sort events by time
        var sortedEvents = scenario.Events.OrderBy(e => e.TimeS).ToList();

        for (int i = 0; i < sortedEvents.Count; i++)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                break;
            }

            var evt = sortedEvents[i];
            run.CurrentEventIndex = i;

            try
            {
                // Wait until the event time
                await WaitForEventTimeAsync(sessionId, evt.TimeS, cancellationToken);

                // Execute event
                await ExecuteEventAsync(sessionId, evt);

                _logger.LogInformation("Executed event {Index}/{Total}: {Type} at {Time}s",
                    i + 1, sortedEvents.Count, evt.Type, evt.TimeS);

                // Handle repeating events
                if (evt.Repeat != null)
                {
                    await ExecuteRepeatingEventAsync(sessionId, evt, cancellationToken);
                }
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error executing event {Index} in scenario {ScenarioId}",
                    i, scenario.ScenarioId);
                run.Errors.Add($"Event {i}: {ex.Message}");
            }
        }

        run.Status = ScenarioStatus.Completed;
        run.CompletedAt = DateTime.UtcNow;

        _logger.LogInformation("Completed scenario run {RunId}", scenarioRunId);
    }

    private async Task WaitForEventTimeAsync(string sessionId, double targetTimeS, CancellationToken cancellationToken)
    {
        // Poll session to wait for simulation time to reach target
        // This is a simplified implementation - production would use more sophisticated timing

        while (!cancellationToken.IsCancellationRequested)
        {
            var session = await _sessionManager.GetSessionAsync(sessionId);
            if (session == null)
            {
                throw new InvalidOperationException("Session no longer exists");
            }

            var currentSimSeconds = (session.SimTime - session.CreatedAt).TotalSeconds;

            if (currentSimSeconds >= targetTimeS)
            {
                break;
            }

            // Wait a bit before checking again
            await Task.Delay(100, cancellationToken);
        }
    }

    private async Task ExecuteEventAsync(string sessionId, ScenarioEvent evt)
    {
        switch (evt.Type.ToLower())
        {
            case "set":
                await ExecuteSetEventAsync(sessionId, evt);
                break;

            case "fault":
                await ExecuteFaultEventAsync(sessionId, evt);
                break;

            case "controller":
                await ExecuteControllerEventAsync(sessionId, evt);
                break;

            case "note":
                await ExecuteNoteEventAsync(sessionId, evt);
                break;

            case "random_fault":
                await ExecuteRandomFaultEventAsync(sessionId, evt);
                break;

            default:
                _logger.LogWarning("Unknown event type: {Type}", evt.Type);
                break;
        }
    }

    private async Task ExecuteSetEventAsync(string sessionId, ScenarioEvent evt)
    {
        if (string.IsNullOrEmpty(evt.Target) || evt.Payload == null)
        {
            _logger.LogWarning("Set event missing target or payload");
            return;
        }

        var writeRequest = new WriteTagRequest
        {
            Value = evt.Payload,
            User = "scenario",
            Mode = "auto"
        };

        await _sessionManager.WriteTagAsync(sessionId, evt.Target, writeRequest);

        _logger.LogDebug("Set {Target} = {Value}", evt.Target, evt.Payload);
    }

    private async Task ExecuteFaultEventAsync(string sessionId, ScenarioEvent evt)
    {
        // Fault is similar to set but marks it as a fault injection in logs
        if (string.IsNullOrEmpty(evt.Target) || evt.Payload == null)
        {
            _logger.LogWarning("Fault event missing target or payload");
            return;
        }

        var writeRequest = new WriteTagRequest
        {
            Value = evt.Payload,
            User = "scenario_fault",
            Mode = "fault"
        };

        await _sessionManager.WriteTagAsync(sessionId, evt.Target, writeRequest);

        _logger.LogInformation("Injected fault: {Target} = {Value}", evt.Target, evt.Payload);
    }

    private Task ExecuteControllerEventAsync(string sessionId, ScenarioEvent evt)
    {
        // TODO: Implement controller parameter modification
        _logger.LogInformation("Controller event: {Target} {Payload}", evt.Target, evt.Payload);
        return Task.CompletedTask;
    }

    private Task ExecuteNoteEventAsync(string sessionId, ScenarioEvent evt)
    {
        // Note events are just logged for instructor/trainee information
        _logger.LogInformation("Scenario note: {Message}", evt.Payload);
        return Task.CompletedTask;
    }

    private Task ExecuteRandomFaultEventAsync(string sessionId, ScenarioEvent evt)
    {
        // TODO: Implement random fault selection based on configuration
        _logger.LogInformation("Random fault event: {Payload}", evt.Payload);
        return Task.CompletedTask;
    }

    private async Task ExecuteRepeatingEventAsync(string sessionId, ScenarioEvent evt, CancellationToken cancellationToken)
    {
        if (evt.Repeat == null)
        {
            return;
        }

        var count = evt.Repeat.Count ?? int.MaxValue;

        for (int i = 0; i < count; i++)
        {
            if (cancellationToken.IsCancellationRequested)
            {
                break;
            }

            await Task.Delay(TimeSpan.FromSeconds(evt.Repeat.IntervalS), cancellationToken);
            await ExecuteEventAsync(sessionId, evt);
        }
    }

    private async Task ApplyInitialStateAsync(string sessionId, Dictionary<string, object> initialState)
    {
        _logger.LogInformation("Applying initial state with {Count} variables", initialState.Count);

        foreach (var (tag, value) in initialState)
        {
            try
            {
                var writeRequest = new WriteTagRequest
                {
                    Value = value,
                    User = "scenario_init",
                    Mode = "auto"
                };

                await _sessionManager.WriteTagAsync(sessionId, tag, writeRequest);

                _logger.LogDebug("Set initial state: {Tag} = {Value}", tag, value);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error setting initial state for tag {Tag}", tag);
            }
        }
    }

    private string ResolveScenarioPath(string scenarioPath)
    {
        // If absolute path, use it
        if (Path.IsPathRooted(scenarioPath) && File.Exists(scenarioPath))
        {
            return scenarioPath;
        }

        // Try relative to scenario base path
        var fullPath = Path.Combine(_scenarioBasePath, scenarioPath);
        if (File.Exists(fullPath))
        {
            return fullPath;
        }

        // Try with .json extension
        if (!Path.HasExtension(scenarioPath))
        {
            fullPath = Path.Combine(_scenarioBasePath, scenarioPath + ".json");
            if (File.Exists(fullPath))
            {
                return fullPath;
            }
        }

        // Return original path (will fail later if not found)
        return scenarioPath;
    }
}
