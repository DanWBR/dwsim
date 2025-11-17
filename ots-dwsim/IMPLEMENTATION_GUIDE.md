# DWSIM OTS Implementation Guide

## Overview

This guide provides step-by-step instructions for developers implementing the DWSIM Operator Training System (OTS). The system is organized into multiple phases with clear acceptance criteria.

## Repository Structure

```
ots-dwsim/
├── src/
│   ├── dwsim-host/           # ✅ COMPLETED - REST API simulation host
│   ├── control-gateway/      # ✅ COMPLETED - OPC UA server skeleton
│   ├── orchestrator/         # ⏳ TODO - Session orchestration
│   ├── hmi-operator/         # ⏳ TODO - React operator HMI
│   ├── hmi-instructor/       # ⏳ TODO - React instructor UI
│   ├── replay/               # ⏳ TODO - Replay engine
│   ├── docs/
│   │   ├── api/              # ✅ COMPLETED - OpenAPI spec
│   │   └── schemas/          # ✅ COMPLETED - Scenario JSON schema
│   └── infra/                # ⏳ TODO - K8s/Helm charts
├── samples/
│   ├── flowsheets/           # ⏳ TODO - Sample DWSIM flowsheets
│   └── scenarios/            # ✅ COMPLETED - Sample scenario JSON
├── .github/workflows/        # ✅ COMPLETED - CI/CD pipeline
├── docker-compose.yml        # ✅ COMPLETED - Development stack
└── README.md                 # ✅ COMPLETED
```

## Phase A: Foundation (Backend + Control) ✅ COMPLETED

### A.1: Repository Setup ✅
- [x] Created directory structure
- [x] Added CI/CD workflow (`.github/workflows/ci.yml`)
- [x] Created docker-compose for development

**Acceptance**: CI builds succeed

### A.2: DWSIM Simulation Host ✅
- [x] Created .NET 8 Web API project
- [x] Implemented session lifecycle API
- [x] Created tag read/write endpoints
- [x] Added Swagger/OpenAPI documentation
- [x] Integrated with DWSIM.Automation

**Location**: `src/dwsim-host/`

**Key Files**:
- `Program.cs` - ASP.NET Core setup
- `Controllers/SessionsController.cs` - REST API endpoints
- `Services/SessionManager.cs` - Core session logic
- `Services/FlowsheetRepository.cs` - Flowsheet file management
- `Models/SessionModels.cs` - DTOs

**Acceptance**:
```bash
cd src/dwsim-host
dotnet build
dotnet run
curl http://localhost:5000/health
```

### A.3: Session Lifecycle ✅
Implemented endpoints:
- `POST /api/v1/sessions` - Create session
- `POST /api/v1/sessions/{id}/start` - Start simulation
- `POST /api/v1/sessions/{id}/pause` - Pause
- `POST /api/v1/sessions/{id}/stop` - Stop

**Acceptance**: Can create session and receive valid session ID

### A.4: Tag Read/Write ✅
- `GET /api/v1/sessions/{id}/tags/{path}` - Read tag
- `POST /api/v1/sessions/{id}/tags/{path}` - Write tag

**Acceptance**: Read/write operations log correctly

### A.5: OPC UA Server Skeleton ✅
- [x] Created Control Gateway project
- [x] Implemented mapping loader (YAML)
- [x] Created OPC UA server interface
- [x] Added basic node management

**Location**: `src/control-gateway/`

**Key Files**:
- `Program.cs` - Service host
- `Services/OpcUaServer.cs` - OPC UA implementation
- `Services/MappingLoader.cs` - YAML mapping parser
- `Models/OpcUaMapping.cs` - Mapping models
- `config/opcua-mapping-template.yaml` - Tag mapping template

**Acceptance**: Service starts without errors

### A.6: Tag Mapping Loader ✅
- [x] YAML deserializer
- [x] Session context replacement
- [x] Folder structure creation

**Acceptance**: Mapping loads and validates successfully

---

## Phase B: Orchestration & Scenarios ⏳ IN PROGRESS

### B.1: Scenario JSON Schema Validator ✅
- [x] Created JSON Schema v7 definition
- [x] Sample scenario file created
- [x] Added to CI validation

**Files**:
- `src/docs/schemas/scenario.schema.json`
- `samples/scenarios/distill_startup_fault.json`

**TODO**:
1. Create C# validator service
2. Add validation endpoint to API
3. Create scenario library management

**Acceptance**:
```bash
ajv validate -s src/docs/schemas/scenario.schema.json \
  -d samples/scenarios/distill_startup_fault.json
```

### B.2: Scenario Executor ⏳ TODO

**Tasks**:
1. Create `ScenarioExecutor` service
2. Implement event scheduler with precise timing
3. Add event handlers for each event type:
   - `set` - Set tag value
   - `fault` - Inject fault
   - `controller` - Modify controller parameters
   - `note` - Instructor note
4. Implement repeating events

**Location**: Create `src/dwsim-host/Services/ScenarioExecutor.cs`

**Interface**:
```csharp
public interface IScenarioExecutor
{
    Task<string> LoadScenarioAsync(string scenarioPath);
    Task<string> RunScenarioAsync(string sessionId, string scenarioId);
    Task StopScenarioAsync(string scenarioRunId);
    Task<ScenarioStatus> GetStatusAsync(string scenarioRunId);
}
```

**Acceptance**:
- Load sample scenario
- Execute events at correct sim_time
- Log all events
- Verify deterministic execution with same seed

### B.3: Snapshot & Restore ⏳ TODO

**Tasks**:
1. Implement DWSIM flowsheet serialization
2. Create snapshot storage (file or database)
3. Add restore logic
4. Test state consistency

**Files to create**:
- `src/dwsim-host/Services/SnapshotManager.cs`
- `src/dwsim-host/Models/Snapshot.cs`

**Acceptance**:
- Create snapshot at t=100s
- Modify simulation
- Restore snapshot
- Verify variables match within 0.1%

---

## Phase C: User Interfaces ⏳ TODO

### C.1: Operator HMI (React)

**Tasks**:
1. Initialize React + TypeScript project
2. Install dependencies:
   ```bash
   npx create-react-app hmi-operator --template typescript
   cd hmi-operator
   npm install @mui/material recharts axios
   ```
3. Create components:
   - `ProcessMimic` - SVG flowsheet display
   - `TrendChart` - Real-time trends
   - `AlarmList` - Active alarms
   - `ControlPanel` - Valve/pump controls
4. Implement WebSocket for real-time updates
5. Add OPC UA client (use node-opcua or web gateway)

**Location**: `src/hmi-operator/`

**Key Components**:
- `src/components/ProcessMimic.tsx`
- `src/components/TrendChart.tsx`
- `src/components/AlarmList.tsx`
- `src/components/ControlPanel.tsx`
- `src/services/ApiClient.ts`

**Acceptance**:
- Display at least 3 process variables
- Update values every 1 second
- Operator can write setpoint
- Log all operator actions

### C.2: Instructor UI (React)

**Tasks**:
1. Initialize React project
2. Create components:
   - `ScenarioEditor` - JSON scenario editor
   - `SessionMonitor` - Live session list
   - `EventTimeline` - Event visualization
   - `SnapshotManager` - Snapshot controls
3. Implement scenario validation
4. Add file upload for scenarios

**Location**: `src/hmi-instructor/`

**Acceptance**:
- Create new scenario via UI
- Validate against schema
- Start scenario on session
- View live operator actions

---

## Phase D: Logging, Replay & Assessment ⏳ TODO

### D.1: Time-Series Logging

**Tasks**:
1. Setup TimescaleDB schema:
```sql
CREATE TABLE process_variables (
    time TIMESTAMPTZ NOT NULL,
    session_id UUID NOT NULL,
    tag_path TEXT NOT NULL,
    value DOUBLE PRECISION,
    sim_time TIMESTAMPTZ NOT NULL
);
SELECT create_hypertable('process_variables', 'time');
```

2. Create logging service:
   - Poll tags at configured interval
   - Batch insert to TimescaleDB
   - Handle connection failures

3. Create query API for historical data

**Files to create**:
- `src/dwsim-host/Services/TimeSeriesLogger.cs`
- `src/infra/db/init.sql`

**Acceptance**:
- Run 10-minute simulation
- Query data: `SELECT * FROM process_variables WHERE session_id = '...' ORDER BY time`
- Verify continuous data coverage

### D.2: Replay Engine

**Tasks**:
1. Create replay service that:
   - Loads event log
   - Creates new session
   - Applies events at original sim_time
   - Compares outputs
2. Implement determinism verification

**Acceptance**:
- Replay recorded session
- Compare key variables
- Max deviation < 0.2%

### D.3: Assessment Engine

**Tasks**:
1. Define KPI rules (JSON or DSL)
2. Create rule evaluator
3. Generate assessment report

**Example KPIs**:
- Time to steady state
- Number of alarms raised
- Setpoint deviation integral
- Operator actions count

**Acceptance**:
- Run scenario with known KPIs
- Verify score calculation
- Generate PDF report

---

## Phase E: Packaging & Deployment ⏳ TODO

### E.1: Docker Images

**Tasks**:
1. Create production Dockerfiles:

**`src/dwsim-host/Dockerfile`**:
```dockerfile
FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build
WORKDIR /src
COPY . .
RUN dotnet restore "ots-dwsim/src/dwsim-host/DWSIM.OTS.SimulationHost.csproj"
RUN dotnet publish "ots-dwsim/src/dwsim-host/DWSIM.OTS.SimulationHost.csproj" \
    -c Release -o /app/publish

FROM mcr.microsoft.com/dotnet/aspnet:8.0
WORKDIR /app
COPY --from=build /app/publish .
EXPOSE 5000
ENTRYPOINT ["dotnet", "DWSIM.OTS.SimulationHost.dll"]
```

2. Build and test images:
```bash
docker-compose up --build
docker-compose ps
```

**Acceptance**: All services start and health checks pass

### E.2: Documentation & Installer

**Tasks**:
1. Create deployment guide
2. Create Kubernetes manifests (optional)
3. Create Windows installer (optional)
4. Write operator manual
5. Write instructor manual

**Files to create**:
- `docs/deployment/README.md`
- `docs/deployment/kubernetes/`
- `docs/manuals/operator-manual.pdf`
- `docs/manuals/instructor-manual.pdf`

---

## Testing Strategy

### Unit Tests
```bash
dotnet test tests/unit --logger "console;verbosity=detailed"
```

### Integration Tests
```bash
# Start dependencies
docker-compose up -d postgres timescaledb

# Run tests
dotnet test tests/integration
```

### End-to-End Test
```bash
# 1. Start all services
docker-compose up -d

# 2. Create session
SESSION_ID=$(curl -X POST http://localhost:5000/api/v1/sessions \
  -H "Content-Type: application/json" \
  -d '{"flowsheet":"sample.dwx","session_name":"e2e_test"}' \
  | jq -r '.session_id')

# 3. Start session
curl -X POST http://localhost:5000/api/v1/sessions/$SESSION_ID/start \
  -H "Content-Type: application/json" \
  -d '{"start_mode":"run"}'

# 4. Read tag
curl http://localhost:5000/api/v1/sessions/$SESSION_ID/tags/Streams.Feed.Temperature

# 5. Run scenario
curl -X POST http://localhost:5000/api/v1/sessions/$SESSION_ID/scenario/run \
  -H "Content-Type: application/json" \
  -d '{"scenario_id":"samples/scenarios/distill_startup_fault.json"}'

# 6. Verify events
curl http://localhost:5000/api/v1/sessions/$SESSION_ID/events

# 7. Stop session
curl -X POST http://localhost:5000/api/v1/sessions/$SESSION_ID/stop
```

---

## Development Workflow

### Daily Development
```bash
# Terminal 1: Start backend
cd src/dwsim-host
dotnet watch run

# Terminal 2: Start control gateway
cd src/control-gateway
dotnet run

# Terminal 3: Start operator HMI
cd src/hmi-operator
npm start

# Terminal 4: Start instructor UI
cd src/hmi-instructor
npm start
```

### Before Commit
```bash
# Format code
dotnet format

# Run tests
dotnet test

# Validate scenarios
ajv validate -s src/docs/schemas/scenario.schema.json -d "samples/scenarios/*.json"

# Lint YAML
yamllint src/control-gateway/config/*.yaml
```

---

## Troubleshooting

### DWSIM Flowsheet Load Errors
- Ensure flowsheet is in `samples/flowsheets/`
- Check file extension (.dwxmz or .dwxml)
- Verify flowsheet opens in DWSIM GUI

### OPC UA Connection Issues
- Check port 4840 is not blocked
- Verify `opcua-mapping-template.yaml` syntax
- Check logs: `tail -f logs/control-gateway/*.log`

### Session Start Failures
- Check DWSIM assemblies are referenced correctly
- Verify flowsheet has no solver errors
- Review logs in `logs/dwsim-host/`

---

## Next Steps

1. **Immediate Priority**:
   - Implement scenario executor (Phase B.2)
   - Create basic operator HMI (Phase C.1)
   - Add sample DWSIM flowsheet

2. **Week 1-2**:
   - Complete Phase B (Scenarios)
   - Start Phase C (UIs)
   - Add integration tests

3. **Week 3-4**:
   - Complete Phase D (Logging & Replay)
   - Add comprehensive tests
   - Performance optimization

4. **Week 5-6**:
   - Phase E (Docker & Docs)
   - User acceptance testing
   - Production deployment

---

## Contributing

### Code Style
- C#: Follow Microsoft C# coding conventions
- TypeScript/React: Use ESLint + Prettier
- Commit messages: Conventional Commits format

### Pull Request Process
1. Create feature branch: `git checkout -b feature/scenario-executor`
2. Implement with tests
3. Update documentation
4. Submit PR with description
5. Wait for CI to pass
6. Request review

### Contact
- Issues: GitHub Issues
- Discussions: GitHub Discussions
- DWSIM Community: http://dwsim.inforside.com.br

---

**Document Version**: 1.0
**Last Updated**: 2025-11-17
**Status**: Living document - updated as implementation progresses
