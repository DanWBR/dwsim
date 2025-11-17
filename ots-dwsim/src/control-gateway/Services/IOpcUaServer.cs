using DWSIM.OTS.ControlGateway.Models;

namespace DWSIM.OTS.ControlGateway.Services;

public interface IOpcUaServer
{
    Task StartAsync(OpcUaMapping mapping, CancellationToken cancellationToken = default);
    Task StopAsync(CancellationToken cancellationToken = default);
    Task UpdateTagValueAsync(string tagPath, object value);
    Task<object?> ReadTagValueAsync(string tagPath);
    bool IsRunning { get; }
}
