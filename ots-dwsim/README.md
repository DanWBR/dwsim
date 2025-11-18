# DWSIM Operator Training System (OTS)

**Version:** 1.0.0
**License:** GPL-3.0 (inherited from DWSIM)

## Overview

A complete, production-ready Operator Training System built on top of DWSIM. This package provides a comprehensive OTS solution with:

- **REST API** for simulation control and session management
- **Scenario System** with deterministic, repeatable fault injection
- **Snapshot/Restore** for state preservation
- **TimescaleDB Integration** for high-performance time-series logging
- **Replay Engine** with determinism verification
- **Assessment Engine** with configurable KPI rules
- **Operator HMI** with real-time process visualization
- **Instructor Console** for training management and performance analysis
- **Docker Deployment** for easy production setup

## Features

✅ **Complete Implementation (17/17 tasks - 100%)**

### Phase A - Foundation
- ✅ CI/CD configuration with GitHub Actions
- ✅ DWSIM Simulation Host REST API (.NET 8)
- ✅ Session lifecycle management (create/start/stop/pause)
- ✅ Tag read/write endpoints with OpenAPI documentation
- ✅ OPC UA server skeleton
- ✅ OPC UA tag mapping loader

### Phase B - Scenarios
- ✅ JSON Schema-based scenario validator
- ✅ Scenario executor with time-accurate event scheduler
- ✅ Snapshot/restore functionality

### Phase C - User Interfaces
- ✅ React Operator HMI with Material-UI
- ✅ React Instructor Console with comprehensive features

### Phase D - Data Pipeline
- ✅ TimescaleDB integration with hypertables and aggregates
- ✅ Background time-series logger (1Hz polling)
- ✅ Replay engine with determinism verification
- ✅ Assessment engine with 10 KPI types

### Phase E - Deployment
- ✅ Production Docker images for all services
- ✅ Docker Compose orchestration
- ✅ Comprehensive deployment documentation

## Architecture

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│ Instructor UI   │    │  Operator HMI   │    │  API Clients    │
│  React + MUI    │    │  React + MUI    │    │                 │
│  (Port 3001)    │    │  (Port 3000)    │    │                 │
└────────┬────────┘    └────────┬────────┘    └────────┬────────┘
         │                      │                       │
         └──────────────────────┴───────────────────────┘
                                │
                    ┌───────────▼────────────┐
                    │ DWSIM Simulation Host  │
                    │   REST API (.NET 8)    │
                    │    (Port 5000)         │
                    │                        │
                    │ • Session Management   │
                    │ • Scenario Execution   │
                    │ • Snapshot/Restore     │
                    │ • Replay Engine        │
                    │ • Assessment Engine    │
                    └───────────┬────────────┘
                                │
                    ┌───────────▼────────────┐
                    │    TimescaleDB         │
                    │  Time-Series Storage   │
                    │    (Port 5433)         │
                    │                        │
                    │ • Process Variables    │
                    │ • Event Logs           │
                    │ • Continuous Aggs      │
                    │ • Compression Policies │
                    └────────────────────────┘
```

## Quick Start

### Prerequisites

- **Docker**: 20.10+ (required)
- **Docker Compose**: 1.29+ (required)
- **Minimum Resources**: 4 CPU cores, 8 GB RAM, 50 GB storage

### Docker Deployment (Recommended)

```bash
# 1. Clone repository
git clone https://github.com/KURIANGEORGE57/dwsim.git
cd dwsim/ots-dwsim

# 2. Create environment file
cp .env.example .env
# Edit .env and set POSTGRES_PASSWORD

# 3. Start all services
docker-compose up -d

# 4. Verify all services are running
docker-compose ps

# 5. Access the applications
# Operator HMI: http://localhost:3000
# Instructor Console: http://localhost:3001
# API Swagger UI: http://localhost:5000/swagger
```

### Deployment Output

```
Creating ots-timescaledb ... done
Creating ots-dwsim-host ... done
Creating ots-operator-hmi ... done
Creating ots-instructor-ui ... done
```

All services include health checks and will show status as "Up (healthy)" when ready.

### View Logs

```bash
# All services
docker-compose logs -f

# Specific service
docker-compose logs -f dwsim-host
```

### Stop Services

```bash
docker-compose down

# To also remove volumes (WARNING: deletes all data)
docker-compose down -v
```

## Components

### DWSIM Simulation Host (`/src/dwsim-host`)
ASP.NET Core 8 REST API service providing:
- Session lifecycle management (create, start, stop, pause)
- Tag read/write with DWSIM automation interface
- Scenario execution with time-accurate event scheduling
- Snapshot/restore for state preservation
- TimescaleDB integration for time-series logging
- Replay engine with determinism verification (< 0.2% deviation)
- Assessment engine with 10 configurable KPI types
- OpenAPI/Swagger documentation

**Key Files:**
- `Controllers/` - REST API endpoints
- `Services/` - Business logic (SessionManager, ScenarioExecutor, ReplayEngine, AssessmentEngine)
- `Models/` - Data models and DTOs

### Operator HMI (`/src/hmi-operator`)
React + TypeScript operator interface with:
- Real-time process mimic (SVG-based)
- Trend charts with 60-second rolling window
- Control panel for setpoint adjustments
- Alarm list with filtering
- Material-UI dark theme
- 1Hz polling for real-time updates

### Instructor Console (`/src/hmi-instructor`)
React + TypeScript instructor dashboard with:
- **Dashboard**: Overview of sessions, assessments, pass rates
- **Session Management**: Start/stop sessions, create snapshots
- **Scenario Manager**: JSON editor with validation
- **Rule Set Manager**: Configure KPI rules and thresholds
- **Assessment Viewer**: Detailed performance analysis with recommendations
- **Replay Manager**: Monitor replays and determinism verification

### TimescaleDB Database (`/src/infra/db`)
PostgreSQL extension for time-series data:
- Hypertables partitioned by time
- Continuous aggregates (1-minute, 1-hour rollups)
- Automatic compression after 7 days
- 90-day retention policy
- Event log for operator actions

## API Documentation

Full OpenAPI specification: [docs/api/openapi.yaml](docs/api/openapi.yaml)

Base URL: `http://localhost:5000/api/v1`

Key endpoints:
- `POST /api/v1/sessions` - Create new simulation session
- `POST /api/v1/sessions/{id}/start` - Start simulation
- `GET /api/v1/sessions/{id}/tags/{path}` - Read tag value
- `POST /api/v1/sessions/{id}/scenario/run` - Execute scenario

## Scenario Format

Scenarios are defined in JSON following the schema: [docs/schemas/scenario.schema.json](docs/schemas/scenario.schema.json)

Example scenario: [samples/scenarios/distill_startup_fault.json](samples/scenarios/distill_startup_fault.json)

## Development

### Building

```bash
cd src/dwsim-host
dotnet build -c Release

cd ../orchestrator
dotnet build -c Release

cd ../hmi-operator
npm run build
```

### Testing

```bash
# Unit tests
dotnet test

# Integration tests
cd tests/integration
dotnet test
```

## Production Deployment

For production deployment with SSL/TLS, load balancing, backups, and monitoring:

**See [DEPLOYMENT.md](DEPLOYMENT.md) for comprehensive deployment guide**

Covers:
- System requirements and server preparation
- SSL/TLS configuration with Nginx or Traefik
- Data persistence and backup procedures
- Security hardening and access control
- Monitoring with health checks and metrics
- Scaling and high-availability setup
- Troubleshooting common issues

## Contributing

This OTS is a template implementation. Teams can extend it by:

1. Adding custom unit operations via `IUnitPlugin` interface
2. Creating plant-specific tag mappings
3. Implementing custom assessment scripts
4. Adding ML surrogate models in `/src/surrogate`

## License

Copyright 2008-2025 Daniel Wagner and contributors

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

## Support

- Documentation: [docs/](docs/)
- Issues: https://github.com/DanWBR/dwsim6/issues
- DWSIM Website: http://dwsim.inforside.com.br
