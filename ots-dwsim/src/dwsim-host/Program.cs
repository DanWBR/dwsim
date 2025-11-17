using DWSIM.OTS.SimulationHost.Services;
using Serilog;

var builder = WebApplication.CreateBuilder(args);

// Configure Serilog
Log.Logger = new LoggerConfiguration()
    .WriteTo.Console()
    .WriteTo.File("logs/dwsim-host-.log", rollingInterval: RollingInterval.Day)
    .CreateLogger();

builder.Host.UseSerilog();

// Add services to the container
builder.Services.AddControllers()
    .AddNewtonsoftJson(options =>
    {
        options.SerializerSettings.ReferenceLoopHandling = Newtonsoft.Json.ReferenceLoopHandling.Ignore;
    });

builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen(c =>
{
    c.SwaggerDoc("v1", new Microsoft.OpenApi.Models.OpenApiInfo
    {
        Title = "DWSIM OTS Simulation Host API",
        Version = "v1",
        Description = "REST API for DWSIM Operator Training System - Simulation Host",
        Contact = new Microsoft.OpenApi.Models.OpenApiContact
        {
            Name = "DWSIM OTS",
            Url = new Uri("https://github.com/DanWBR/dwsim6")
        }
    });
});

// Register application services
builder.Services.AddSingleton<ISessionManager, SessionManager>();
builder.Services.AddSingleton<IFlowsheetRepository, FlowsheetRepository>();

// CORS for development
builder.Services.AddCors(options =>
{
    options.AddPolicy("AllowAll", policy =>
    {
        policy.AllowAnyOrigin()
              .AllowAnyMethod()
              .AllowAnyHeader();
    });
});

var app = builder.Build();

// Configure the HTTP request pipeline
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI(c =>
    {
        c.SwaggerEndpoint("/swagger/v1/swagger.json", "DWSIM OTS API v1");
        c.RoutePrefix = string.Empty; // Swagger at root
    });
}

app.UseCors("AllowAll");
app.UseAuthorization();
app.MapControllers();

// Health check endpoint
app.MapGet("/health", () => new
{
    status = "healthy",
    timestamp = DateTime.UtcNow,
    version = "1.0.0-alpha"
});

Log.Information("DWSIM OTS Simulation Host starting...");
app.Run();
