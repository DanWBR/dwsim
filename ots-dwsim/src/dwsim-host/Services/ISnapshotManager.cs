using DWSIM.OTS.SimulationHost.Models;
using DWSIM.Interfaces;

namespace DWSIM.OTS.SimulationHost.Services;

public interface ISnapshotManager
{
    Task<Snapshot> CreateSnapshotAsync(string sessionId, string name, IFlowsheet flowsheet);
    Task<IFlowsheet?> RestoreSnapshotAsync(string snapshotId);
    Task<Snapshot?> GetSnapshotAsync(string snapshotId);
    Task<List<Snapshot>> GetSnapshotsForSessionAsync(string sessionId);
    Task DeleteSnapshotAsync(string snapshotId);
    Task<SnapshotStats> GetStatsAsync();
}
