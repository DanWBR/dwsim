using DWSIM.OTS.SimulationHost.Models;

namespace DWSIM.OTS.SimulationHost.Services;

public interface ITimeSeriesLogger
{
    Task LogAsync(ProcessVariableDataPoint dataPoint);
    Task LogBatchAsync(IEnumerable<ProcessVariableDataPoint> dataPoints);
    Task<List<TimeSeriesQueryResult>> QueryAsync(TimeSeriesQueryRequest request);
    Task<TimeSeriesStats> GetStatsAsync(string sessionId);
    Task<bool> IsConnectedAsync();
}
