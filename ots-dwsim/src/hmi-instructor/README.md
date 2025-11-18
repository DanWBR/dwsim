# DWSIM OTS - Instructor Console

React-based instructor console for managing operator training sessions, scenarios, assessments, and performance analysis.

## Features

### Dashboard
- Overview of active sessions and training statistics
- Pass rate and average score metrics
- Recent sessions and assessments at a glance

### Session Management
- View all training sessions with real-time status
- Start/stop sessions
- Create snapshots for state preservation
- Initiate assessments and replays
- Delete completed sessions

### Scenario Manager
- JSON-based scenario editor with syntax highlighting
- Real-time schema validation
- Load example scenarios
- View event timeline and scenario details
- Interactive schema reference guide

### Rule Set Manager
- Create and manage KPI rule sets for assessments
- View rule details with thresholds and weights
- Associate rule sets with specific scenarios
- Configure passing scores and required KPIs

### Assessment Viewer
- Browse all assessment reports
- Detailed KPI-by-KPI breakdown
- Performance grades and scoring
- Recommendations for improvement
- Visual progress indicators

### Replay Manager
- Monitor session replays and progress
- Determinism verification results
- Tag-by-tag comparison statistics
- Correlation and RMSE metrics
- Pass/fail indicators for each tag

## Technology Stack

- **React 18** - UI framework
- **TypeScript** - Type safety
- **Material-UI (MUI) v5** - Component library and theming
- **React Router** - Client-side routing
- **Axios** - HTTP client for REST API
- **Vite** - Build tool and dev server

## Getting Started

### Installation

```bash
cd src/hmi-instructor
npm install
```

### Development

```bash
npm run dev
```

Runs the app in development mode on `http://localhost:3001`

### Build

```bash
npm run build
```

Builds the app for production to the `dist` folder.

### Environment

The app expects the DWSIM Simulation Host API to be running on `http://localhost:5000`

This is configured in `vite.config.ts` as a proxy:

```typescript
proxy: {
  '/api': {
    target: 'http://localhost:5000',
    changeOrigin: true,
  }
}
```

## Project Structure

```
src/hmi-instructor/
├── src/
│   ├── components/
│   │   ├── Dashboard.tsx           # Main dashboard overview
│   │   ├── SessionList.tsx         # Training session management
│   │   ├── ScenarioManager.tsx     # Scenario editor and validator
│   │   ├── RuleSetManager.tsx      # KPI rule set configuration
│   │   ├── AssessmentViewer.tsx    # Assessment report viewer
│   │   └── ReplayManager.tsx       # Replay monitoring and comparison
│   ├── services/
│   │   └── ApiClient.ts            # REST API client with TypeScript types
│   ├── App.tsx                     # Main app with routing and theme
│   └── main.tsx                    # React entry point
├── package.json                    # Dependencies
├── tsconfig.json                   # TypeScript config
├── vite.config.ts                  # Vite config
└── index.html                      # HTML template
```

## API Integration

The instructor console integrates with the DWSIM Simulation Host REST API:

- `GET/POST /api/v1/sessions` - Session management
- `POST /api/v1/scenarios/validate` - Scenario validation
- `GET/POST /api/v1/assessment` - Assessment operations
- `GET/POST/PUT/DELETE /api/v1/assessment/rulesets` - Rule set management
- `GET/POST /api/v1/replay` - Replay operations

All API types are defined in `src/services/ApiClient.ts`

## Key Components

### Dashboard
- Real-time metrics and statistics
- Active session monitoring
- Recent activity feeds

### Session List
- Complete session lifecycle management
- Bulk operations (start, stop, delete)
- Quick access to assessment and replay

### Scenario Manager
- JSON editor with validation
- Example scenario templates
- Event timeline visualization
- Schema reference documentation

### Rule Set Manager
- KPI rule configuration
- Threshold and weight management
- Required vs optional KPIs
- Rule set versioning

### Assessment Viewer
- Comprehensive report display
- KPI performance breakdown
- Visual score indicators
- Actionable recommendations

### Replay Manager
- Real-time replay progress
- Determinism verification
- Statistical comparison results
- Tag-level deviation analysis

## Dark Theme

The instructor console uses a custom dark theme optimized for extended use:

- Background: `#1a1a2e`
- Paper: `#16213e`
- Primary: `#4ecdc4` (cyan)
- Secondary: `#ff6b6b` (red)
- Monospace font for technical data

## Future Enhancements

- User authentication and authorization
- Multi-instructor collaboration
- Live session monitoring with WebSockets
- Export assessment reports to PDF
- Batch assessment creation
- Historical trend analysis
- Trainee performance tracking over time
