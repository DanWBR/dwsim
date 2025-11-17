namespace DWSIM.OTS.SimulationHost.Services;

public class FlowsheetRepository : IFlowsheetRepository
{
    private readonly IConfiguration _configuration;
    private readonly ILogger<FlowsheetRepository> _logger;
    private readonly string _flowsheetBasePath;

    public FlowsheetRepository(IConfiguration configuration, ILogger<FlowsheetRepository> logger)
    {
        _configuration = configuration;
        _logger = logger;

        // Default flowsheet path - can be overridden in appsettings.json
        _flowsheetBasePath = _configuration["FlowsheetPath"]
            ?? Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "data", "flowsheets");

        // Ensure directory exists
        if (!Directory.Exists(_flowsheetBasePath))
        {
            Directory.CreateDirectory(_flowsheetBasePath);
            _logger.LogInformation("Created flowsheet directory: {Path}", _flowsheetBasePath);
        }
    }

    public Task<string?> ResolveFlowsheetPathAsync(string flowsheetName)
    {
        try
        {
            // If it's already an absolute path and exists, use it
            if (Path.IsPathRooted(flowsheetName) && File.Exists(flowsheetName))
            {
                _logger.LogDebug("Resolved flowsheet as absolute path: {Path}", flowsheetName);
                return Task.FromResult<string?>(flowsheetName);
            }

            // Try to find in the flowsheet base path
            var fullPath = Path.Combine(_flowsheetBasePath, flowsheetName);

            if (File.Exists(fullPath))
            {
                _logger.LogDebug("Resolved flowsheet: {Name} -> {Path}", flowsheetName, fullPath);
                return Task.FromResult<string?>(fullPath);
            }

            // Try with common extensions if not specified
            if (!Path.HasExtension(flowsheetName))
            {
                var extensions = new[] { ".dwxmz", ".dwxml", ".xml" };
                foreach (var ext in extensions)
                {
                    var pathWithExt = Path.Combine(_flowsheetBasePath, flowsheetName + ext);
                    if (File.Exists(pathWithExt))
                    {
                        _logger.LogDebug("Resolved flowsheet with extension: {Name} -> {Path}",
                            flowsheetName, pathWithExt);
                        return Task.FromResult<string?>(pathWithExt);
                    }
                }
            }

            _logger.LogWarning("Flowsheet not found: {Name}", flowsheetName);
            return Task.FromResult<string?>(null);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error resolving flowsheet path for {Name}", flowsheetName);
            return Task.FromResult<string?>(null);
        }
    }

    public Task<List<string>> GetAvailableFlowsheetsAsync()
    {
        try
        {
            var flowsheets = new List<string>();

            if (Directory.Exists(_flowsheetBasePath))
            {
                var extensions = new[] { "*.dwxmz", "*.dwxml", "*.xml" };

                foreach (var ext in extensions)
                {
                    var files = Directory.GetFiles(_flowsheetBasePath, ext, SearchOption.AllDirectories);
                    flowsheets.AddRange(files.Select(f => Path.GetRelativePath(_flowsheetBasePath, f)));
                }
            }

            _logger.LogDebug("Found {Count} flowsheets in {Path}", flowsheets.Count, _flowsheetBasePath);
            return Task.FromResult(flowsheets);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error getting available flowsheets");
            return Task.FromResult(new List<string>());
        }
    }
}
