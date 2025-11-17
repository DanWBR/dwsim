using DWSIM.OTS.ControlGateway.Models;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;

namespace DWSIM.OTS.ControlGateway.Services;

/// <summary>
/// Hosted service that manages the OPC UA server lifecycle
/// </summary>
public class ControlGatewayService : BackgroundService
{
    private readonly ILogger<ControlGatewayService> _logger;
    private readonly IConfiguration _configuration;
    private readonly IMappingLoader _mappingLoader;
    private readonly IOpcUaServer _opcUaServer;
    private readonly IHttpClientFactory _httpClientFactory;

    public ControlGatewayService(
        ILogger<ControlGatewayService> logger,
        IConfiguration configuration,
        IMappingLoader mappingLoader,
        IOpcUaServer opcUaServer,
        IHttpClientFactory httpClientFactory)
    {
        _logger = logger;
        _configuration = configuration;
        _mappingLoader = mappingLoader;
        _opcUaServer = opcUaServer;
        _httpClientFactory = httpClientFactory;
    }

    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        try
        {
            _logger.LogInformation("Control Gateway Service starting");

            // Load OPC UA mapping configuration
            var mappingPath = _configuration["MappingPath"] ?? "config/opcua-mapping-template.yaml";
            var mapping = await _mappingLoader.LoadMappingAsync(mappingPath);

            // Get session ID from configuration or environment
            var sessionId = _configuration["SessionId"] ?? Guid.NewGuid().ToString();

            // Apply session context to mapping
            var sessionMapping = _mappingLoader.ApplySessionContext(mapping, sessionId);

            // Start OPC UA server
            await _opcUaServer.StartAsync(sessionMapping, stoppingToken);

            _logger.LogInformation("Control Gateway running with session {SessionId}", sessionId);
            _logger.LogInformation("OPC UA Namespace: {NamespaceUri}", sessionMapping.Namespace.Uri);
            _logger.LogInformation("Security Mode: {SecurityMode}", sessionMapping.Security.Mode);

            // Main loop - poll simulation host and update OPC UA nodes
            var updateInterval = TimeSpan.FromMilliseconds(sessionMapping.UpdateRates.DefaultMs);

            while (!stoppingToken.IsCancellationRequested)
            {
                try
                {
                    // TODO: Poll simulation host for tag updates
                    // await PollSimulationHostAsync(sessionId, sessionMapping, stoppingToken);

                    await Task.Delay(updateInterval, stoppingToken);
                }
                catch (OperationCanceledException)
                {
                    // Expected when stopping
                    break;
                }
                catch (Exception ex)
                {
                    _logger.LogError(ex, "Error in polling loop");
                    await Task.Delay(TimeSpan.FromSeconds(5), stoppingToken);
                }
            }
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Fatal error in Control Gateway Service");
            throw;
        }
    }

    public override async Task StopAsync(CancellationToken cancellationToken)
    {
        _logger.LogInformation("Control Gateway Service stopping");

        await _opcUaServer.StopAsync(cancellationToken);

        await base.StopAsync(cancellationToken);

        _logger.LogInformation("Control Gateway Service stopped");
    }

    private async Task PollSimulationHostAsync(string sessionId, OpcUaMapping mapping, CancellationToken cancellationToken)
    {
        // This method would poll the simulation host REST API for tag values
        // and update the OPC UA server nodes accordingly

        var httpClient = _httpClientFactory.CreateClient("SimulationHost");

        // Example: Read tags from explicit mappings
        foreach (var explicitMapping in mapping.ExplicitMappings)
        {
            try
            {
                var response = await httpClient.GetAsync(
                    $"/api/v1/sessions/{sessionId}/tags/{explicitMapping.DwsimTag}",
                    cancellationToken);

                if (response.IsSuccessStatusCode)
                {
                    var tagValue = await response.Content.ReadFromJsonAsync<TagValueResponse>(cancellationToken);
                    if (tagValue != null)
                    {
                        await _opcUaServer.UpdateTagValueAsync(explicitMapping.DwsimTag, tagValue.Value ?? 0);
                    }
                }
            }
            catch (Exception ex)
            {
                _logger.LogDebug(ex, "Error polling tag {TagPath}", explicitMapping.DwsimTag);
            }
        }
    }

    private class TagValueResponse
    {
        public string Tag { get; set; } = string.Empty;
        public object? Value { get; set; }
        public string? Units { get; set; }
        public DateTime SimTime { get; set; }
    }
}
