using DWSIM.OTS.SimulationHost.Models;

namespace DWSIM.OTS.SimulationHost.Services;

public interface ISessionManager
{
    Task<CreateSessionResponse> CreateSessionAsync(CreateSessionRequest request);
    Task<SessionInfo?> GetSessionAsync(string sessionId);
    Task<List<SessionInfo>> GetAllSessionsAsync();
    Task<StartSessionResponse> StartSessionAsync(string sessionId, StartSessionRequest request);
    Task<SessionInfo> PauseSessionAsync(string sessionId);
    Task<SessionInfo> StopSessionAsync(string sessionId);
    Task<SnapshotResponse> CreateSnapshotAsync(string sessionId, SnapshotRequest request);
    Task<SessionInfo> RestoreSnapshotAsync(string sessionId, RestoreSnapshotRequest request);
    Task<StartSessionResponse> StepSessionAsync(string sessionId, TimeStepRequest request);
    Task<TimeFactorResponse> SetTimeFactorAsync(string sessionId, TimeFactorRequest request);
    Task<TagValue?> ReadTagAsync(string sessionId, string tagPath);
    Task<WriteTagResponse> WriteTagAsync(string sessionId, string tagPath, WriteTagRequest request);
    Task<List<EventLogEntry>> GetEventsAsync(string sessionId, DateTime? from, DateTime? to);
}
