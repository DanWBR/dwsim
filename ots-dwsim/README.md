# DWSIM Operator Training System (OTS)

**Version:** 1.0.0-alpha
**License:** GPL-3.0 (inherited from DWSIM)

## Overview

A complete, standalone Operator Training System built on top of DWSIM. This package provides a deployable OTS solution that reuses DWSIM flowsheets/models and includes:

- **REST API** for simulation control and session management
- **OPC UA Server** for industrial control system integration
- **Scenario Manager** with deterministic, repeatable fault injection
- **Operator HMI** for process visualization and control
- **Instructor Station** for scenario management and trainee monitoring
- **Replay Engine** for session playback and assessment

## Architecture

```
┌─────────────────┐      ┌──────────────────┐
│ Instructor UI   │◄────►│  Orchestrator    │
└─────────────────┘      │ Scenario Manager │
                         └──────────┬───────┘
                                    │
┌─────────────────┐                 │
│  Operator HMI   │◄────┐          │
└─────────────────┘     │          ▼
                        │  ┌────────────────┐
                        └──┤ Control Gateway│
                           │   (OPC UA)     │
                           └───────┬────────┘
                                   │
                           ┌───────▼────────┐
                           │ DWSIM Sim Host │
                           │  (per-session) │
                           └────────────────┘
```

## Quick Start

### Prerequisites

- .NET 8 SDK
- Docker & Docker Compose (optional, recommended)
- Node.js 18+ (for UI development)
- PostgreSQL 15+ / TimescaleDB (for production)

### Development Setup

```bash
# Clone repository
cd ots-dwsim

# Start backend services
cd src/dwsim-host
dotnet restore
dotnet run

# In another terminal, start orchestrator
cd src/orchestrator
dotnet run

# Start frontend (development)
cd src/hmi-operator
npm install
npm run dev
```

### Docker Quick Start

```bash
# Build and start all services
docker-compose up --build

# Access UIs
# Operator HMI: http://localhost:3000
# Instructor UI: http://localhost:3001
# API Docs: http://localhost:5000/swagger
```

## Components

### `/src/dwsim-host`
.NET 8 service embedding DWSIM assemblies. Provides REST API for flowsheet loading, session lifecycle, variable read/write, and time control.

### `/src/orchestrator`
Session manager coordinating multiple simulation instances, scenario execution, and logging.

### `/src/control-gateway`
OPC UA server exposing simulation variables following ISA-95 naming conventions.

### `/src/hmi-operator`
React-based operator interface with mimics, trends, and alarm lists.

### `/src/hmi-instructor`
React-based instructor station for scenario creation, session monitoring, and trainee assessment.

### `/src/replay`
Event replay engine for deterministic session playback and analysis.

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

## Deployment

See [docs/deployment/README.md](docs/deployment/README.md) for production deployment instructions.

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
