# DWSIM OTS - Operator HMI

React-based operator interface for the DWSIM Operator Training System.

## Features

- **Process Mimic**: SVG-based process flow diagram showing real-time equipment status
- **Trend Charts**: Live trending of process variables with Recharts
- **Control Panel**: Manual control of setpoints and process variables
- **Alarm Management**: Real-time alarm display and acknowledgment
- **Session Controls**: Start, pause, stop simulation sessions

## Technology Stack

- React 18 + TypeScript
- Material-UI (MUI) v5 for components
- Recharts for trending
- Axios for API communication
- React Scripts build system

## Quick Start

### Development

```bash
# Install dependencies
npm install

# Start development server
npm start

# Access at http://localhost:3000
```

### Build for Production

```bash
# Create optimized production build
npm run build

# Output in build/ directory
```

## Configuration

Environment variables (`.env` file):

```env
REACT_APP_API_URL=http://localhost:5000
REACT_APP_OPCUA_URL=opc.tcp://localhost:4840
```

## Architecture

```
src/
├── components/        # React components
│   ├── ProcessMimic.tsx     # SVG process diagram
│   ├── TrendChart.tsx       # Real-time trending
│   ├── ControlPanel.tsx     # Operator controls
│   └── AlarmList.tsx        # Alarm display
├── services/          # API clients
│   └── ApiClient.ts   # REST API communication
├── types/             # TypeScript definitions
│   └── index.ts       # Shared types
├── App.tsx            # Main application
└── index.tsx          # Entry point
```

## API Integration

The HMI communicates with the DWSIM Simulation Host via REST API:

- `GET /api/v1/sessions` - List sessions
- `GET /api/v1/sessions/{id}/tags/{path}` - Read tag values
- `POST /api/v1/sessions/{id}/tags/{path}` - Write tag values
- `POST /api/v1/sessions/{id}/start` - Start simulation

See `src/services/ApiClient.ts` for full API client implementation.

## Components

### ProcessMimic

Displays a simplified process flow diagram with real-time data:

```typescript
<ProcessMimic processVars={processVars} />
```

### TrendChart

Shows historical trending of process variables:

```typescript
<TrendChart processVars={processVars} />
```

### ControlPanel

Provides manual control interface:

```typescript
<ControlPanel sessionId={sessionId} processVars={processVars} />
```

### AlarmList

Displays active alarms:

```typescript
<AlarmList />
```

## Development Guidelines

### Adding New Tags

1. Add tag path to the polling list in `App.tsx`:
```typescript
const tags = [
  'Streams.Feed.Temperature',
  'Your.New.Tag.Path'
];
```

2. Tag will automatically appear in trends and mimic

### Customizing Process Mimic

Edit `src/components/ProcessMimic.tsx` to modify the SVG diagram.

### Adding Controls

Add new controls in `src/components/ControlPanel.tsx` using MUI components.

## Testing

```bash
# Run tests
npm test

# Run with coverage
npm test -- --coverage
```

## Deployment

### Docker

```bash
# Build image
docker build -t ots-hmi-operator .

# Run container
docker run -p 3000:3000 -e REACT_APP_API_URL=http://api:5000 ots-hmi-operator
```

### Static Hosting

Build the app and serve the `build/` directory:

```bash
npm run build
npx serve -s build -p 3000
```

## Troubleshooting

### API Connection Issues

- Ensure backend is running on configured URL
- Check CORS settings in backend
- Verify proxy configuration in `package.json`

### Process Variables Not Updating

- Check that session ID is valid
- Verify backend tag paths match frontend expectations
- Review browser console for API errors

## Future Enhancements

- [ ] WebSocket for real-time updates (replace polling)
- [ ] OPC UA web client integration
- [ ] Advanced alarm management (acknowledge, silence)
- [ ] Custom mimic builder
- [ ] Multi-session support
- [ ] Mobile-responsive layout

## License

GPL-3.0 (inherited from DWSIM)
