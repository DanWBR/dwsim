# DWSIM Simulation Host - Production Dockerfile
FROM mcr.microsoft.com/dotnet/aspnet:8.0 AS base
WORKDIR /app
EXPOSE 5000

# Build stage
FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build
WORKDIR /src

# Copy project file and restore dependencies
COPY ["DWSIM.OTS.SimulationHost.csproj", "./"]
RUN dotnet restore "DWSIM.OTS.SimulationHost.csproj"

# Copy source code and build
COPY . .
RUN dotnet build "DWSIM.OTS.SimulationHost.csproj" -c Release -o /app/build

# Publish stage
FROM build AS publish
RUN dotnet publish "DWSIM.OTS.SimulationHost.csproj" -c Release -o /app/publish /p:UseAppHost=false

# Final runtime stage
FROM base AS final
WORKDIR /app

# Create data directories
RUN mkdir -p /app/data/flowsheets \
    && mkdir -p /app/data/snapshots \
    && mkdir -p /app/data/scenarios \
    && mkdir -p /app/data/rulesets \
    && mkdir -p /app/schemas

# Copy published application
COPY --from=publish /app/publish .

# Copy schemas
COPY schemas/ /app/schemas/

# Set environment variables
ENV ASPNETCORE_URLS=http://+:5000
ENV ASPNETCORE_ENVIRONMENT=Production

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
    CMD curl --fail http://localhost:5000/health || exit 1

ENTRYPOINT ["dotnet", "DWSIM.OTS.SimulationHost.dll"]
