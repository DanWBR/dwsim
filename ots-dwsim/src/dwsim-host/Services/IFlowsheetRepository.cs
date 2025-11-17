namespace DWSIM.OTS.SimulationHost.Services;

public interface IFlowsheetRepository
{
    Task<string?> ResolveFlowsheetPathAsync(string flowsheetName);
    Task<List<string>> GetAvailableFlowsheetsAsync();
}
