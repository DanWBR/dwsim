using DWSIM.OTS.SimulationHost.Models;

namespace DWSIM.OTS.SimulationHost.Services;

/// <summary>
/// Service for replaying recorded simulation sessions
/// </summary>
public interface IReplayEngine
{
    /// <summary>
    /// Start a replay of a recorded session
    /// </summary>
    /// <param name="request">Replay request parameters</param>
    /// <returns>Replay response with replay ID and status</returns>
    Task<ReplayResponse> StartReplayAsync(ReplayRequest request);

    /// <summary>
    /// Get current status of a replay
    /// </summary>
    /// <param name="replayId">Replay ID</param>
    /// <returns>Replay status and progress</returns>
    Task<ReplayResponse?> GetReplayStatusAsync(string replayId);

    /// <summary>
    /// Get comparison between original and replayed session
    /// </summary>
    /// <param name="replayId">Replay ID</param>
    /// <returns>Detailed comparison results</returns>
    Task<ReplayComparison?> GetComparisonAsync(string replayId);

    /// <summary>
    /// Cancel an in-progress replay
    /// </summary>
    /// <param name="replayId">Replay ID</param>
    /// <returns>True if cancelled successfully</returns>
    Task<bool> CancelReplayAsync(string replayId);

    /// <summary>
    /// Get all replays for a session
    /// </summary>
    /// <param name="sessionId">Original session ID</param>
    /// <returns>List of replays</returns>
    Task<List<ReplayResponse>> GetReplaysForSessionAsync(string sessionId);
}
