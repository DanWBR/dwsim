using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Serilog;
using DWSIM.OTS.ControlGateway.Services;

Log.Logger = new LoggerConfiguration()
    .WriteTo.Console()
    .WriteTo.File("logs/control-gateway-.log", rollingInterval: RollingInterval.Day)
    .CreateLogger();

try
{
    Log.Information("Starting DWSIM OTS Control Gateway");

    var builder = Host.CreateApplicationBuilder(args);

    // Configure Serilog
    builder.Services.AddSerilog();

    // Register services
    builder.Services.AddSingleton<IMappingLoader, MappingLoader>();
    builder.Services.AddSingleton<IOpcUaServer, OpcUaServer>();
    builder.Services.AddHostedService<ControlGatewayService>();

    // HTTP Client for communication with Simulation Host
    builder.Services.AddHttpClient("SimulationHost", client =>
    {
        var hostUrl = builder.Configuration["SimulationHost:Url"] ?? "http://localhost:5000";
        client.BaseAddress = new Uri(hostUrl);
        client.Timeout = TimeSpan.FromSeconds(30);
    });

    var app = builder.Build();

    await app.RunAsync();

    return 0;
}
catch (Exception ex)
{
    Log.Fatal(ex, "Control Gateway terminated unexpectedly");
    return 1;
}
finally
{
    Log.CloseAndFlush();
}
