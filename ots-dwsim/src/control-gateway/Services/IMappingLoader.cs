using DWSIM.OTS.ControlGateway.Models;

namespace DWSIM.OTS.ControlGateway.Services;

public interface IMappingLoader
{
    Task<OpcUaMapping> LoadMappingAsync(string filePath);
    OpcUaMapping ApplySessionContext(OpcUaMapping template, string sessionId);
}
