import React, { useEffect, useState } from 'react';
import {
  AppBar,
  Box,
  Container,
  CssBaseline,
  Grid,
  Paper,
  Toolbar,
  Typography,
  ThemeProvider,
  createTheme,
  Button,
  Chip
} from '@mui/material';
import {
  PlayArrow,
  Pause,
  Stop,
  Speed
} from '@mui/icons-material';

import ProcessMimic from './components/ProcessMimic';
import TrendChart from './components/TrendChart';
import ControlPanel from './components/ControlPanel';
import AlarmList from './components/AlarmList';
import { apiClient } from './services/ApiClient';
import { SessionInfo, SessionStatus, ProcessVariable } from './types';

import './App.css';

const darkTheme = createTheme({
  palette: {
    mode: 'dark',
    primary: {
      main: '#1976d2',
    },
    secondary: {
      main: '#dc004e',
    },
    background: {
      default: '#121212',
      paper: '#1e1e1e',
    },
  },
});

function App() {
  const [session, setSession] = useState<SessionInfo | null>(null);
  const [sessionId, setSessionId] = useState<string>('');
  const [processVars, setProcessVars] = useState<ProcessVariable[]>([]);
  const [lastUpdate, setLastUpdate] = useState<Date>(new Date());

  // Demo session ID - in production this would come from user selection
  const DEMO_SESSION_ID = 'demo-session-1';

  useEffect(() => {
    // Initialize session on mount
    initializeSession();

    // Setup polling for session status
    const interval = setInterval(() => {
      if (sessionId) {
        refreshSession();
        refreshProcessVariables();
      }
    }, 1000); // Update every second

    return () => clearInterval(interval);
  }, [sessionId]);

  const initializeSession = async () => {
    try {
      // Try to get existing sessions
      const sessions = await apiClient.getSessions();

      if (sessions.length > 0) {
        const activeSession = sessions[0];
        setSessionId(activeSession.session_id);
        setSession(activeSession);
      } else {
        // Create a demo session
        console.log('No existing sessions found');
      }
    } catch (error) {
      console.error('Error initializing session:', error);
    }
  };

  const refreshSession = async () => {
    try {
      const sessionData = await apiClient.getSession(sessionId);
      setSession(sessionData);
    } catch (error) {
      console.error('Error refreshing session:', error);
    }
  };

  const refreshProcessVariables = async () => {
    try {
      // Read multiple tags in parallel
      const tags = [
        'Streams.Feed.Temperature',
        'Streams.Feed.Flow',
        'Streams.Feed.Pressure',
        'Units.Column1.DutyRequired'
      ];

      const values = await Promise.all(
        tags.map(tag => apiClient.readTag(sessionId, tag).catch(err => null))
      );

      const pvs: ProcessVariable[] = values
        .map((val, idx) => {
          if (!val) return null;
          return {
            tagPath: tags[idx],
            displayName: tags[idx].split('.').pop() || tags[idx],
            value: val.value as number,
            units: val.units,
            timestamp: val.sim_time,
            color: getTagColor(tags[idx])
          };
        })
        .filter(pv => pv !== null) as ProcessVariable[];

      setProcessVars(pvs);
      setLastUpdate(new Date());
    } catch (error) {
      console.error('Error refreshing process variables:', error);
    }
  };

  const getTagColor = (tagPath: string): string => {
    if (tagPath.includes('Temperature')) return '#ff6b6b';
    if (tagPath.includes('Flow')) return '#4ecdc4';
    if (tagPath.includes('Pressure')) return '#45b7d1';
    return '#95e1d3';
  };

  const handleStartSession = async () => {
    if (!sessionId) return;
    try {
      await apiClient.startSession(sessionId);
      await refreshSession();
    } catch (error) {
      console.error('Error starting session:', error);
    }
  };

  const handlePauseSession = async () => {
    if (!sessionId) return;
    try {
      await apiClient.pauseSession(sessionId);
      await refreshSession();
    } catch (error) {
      console.error('Error pausing session:', error);
    }
  };

  const handleStopSession = async () => {
    if (!sessionId) return;
    try {
      await apiClient.stopSession(sessionId);
      await refreshSession();
    } catch (error) {
      console.error('Error stopping session:', error);
    }
  };

  const getStatusColor = (status: SessionStatus): 'default' | 'success' | 'warning' | 'error' => {
    switch (status) {
      case SessionStatus.Running: return 'success';
      case SessionStatus.Paused: return 'warning';
      case SessionStatus.Error: return 'error';
      default: return 'default';
    }
  };

  return (
    <ThemeProvider theme={darkTheme}>
      <CssBaseline />
      <Box sx={{ display: 'flex', flexDirection: 'column', height: '100vh' }}>
        {/* Header */}
        <AppBar position="static">
          <Toolbar>
            <Typography variant="h6" component="div" sx={{ flexGrow: 1 }}>
              DWSIM OTS - Operator Console
            </Typography>
            {session && (
              <>
                <Chip
                  label={session.status.toUpperCase()}
                  color={getStatusColor(session.status)}
                  sx={{ mr: 2 }}
                />
                <Chip
                  icon={<Speed />}
                  label={`${session.time_factor}x`}
                  variant="outlined"
                  sx={{ mr: 2 }}
                />
                <Typography variant="body2" sx={{ mr: 2 }}>
                  {session.session_name}
                </Typography>
              </>
            )}
            <Button
              color="inherit"
              startIcon={<PlayArrow />}
              onClick={handleStartSession}
              disabled={session?.status === SessionStatus.Running}
            >
              Start
            </Button>
            <Button
              color="inherit"
              startIcon={<Pause />}
              onClick={handlePauseSession}
              disabled={session?.status !== SessionStatus.Running}
            >
              Pause
            </Button>
            <Button
              color="inherit"
              startIcon={<Stop />}
              onClick={handleStopSession}
            >
              Stop
            </Button>
          </Toolbar>
        </AppBar>

        {/* Main Content */}
        <Container maxWidth={false} sx={{ flexGrow: 1, py: 2 }}>
          <Grid container spacing={2} sx={{ height: '100%' }}>
            {/* Left Column - Process Mimic & Trends */}
            <Grid item xs={12} md={8}>
              <Grid container spacing={2}>
                <Grid item xs={12}>
                  <Paper sx={{ p: 2, height: 400 }}>
                    <Typography variant="h6" gutterBottom>
                      Process Flow Diagram
                    </Typography>
                    <ProcessMimic processVars={processVars} />
                  </Paper>
                </Grid>
                <Grid item xs={12}>
                  <Paper sx={{ p: 2, height: 300 }}>
                    <Typography variant="h6" gutterBottom>
                      Trend Chart
                    </Typography>
                    <TrendChart processVars={processVars} />
                  </Paper>
                </Grid>
              </Grid>
            </Grid>

            {/* Right Column - Controls & Alarms */}
            <Grid item xs={12} md={4}>
              <Grid container spacing={2}>
                <Grid item xs={12}>
                  <Paper sx={{ p: 2 }}>
                    <Typography variant="h6" gutterBottom>
                      Control Panel
                    </Typography>
                    <ControlPanel sessionId={sessionId} processVars={processVars} />
                  </Paper>
                </Grid>
                <Grid item xs={12}>
                  <Paper sx={{ p: 2, height: 400, overflow: 'auto' }}>
                    <Typography variant="h6" gutterBottom>
                      Alarms
                    </Typography>
                    <AlarmList />
                  </Paper>
                </Grid>
              </Grid>
            </Grid>
          </Grid>
        </Container>

        {/* Footer */}
        <Paper
          sx={{
            p: 1,
            textAlign: 'center',
            borderRadius: 0,
            backgroundColor: 'background.paper'
          }}
        >
          <Typography variant="caption" color="text.secondary">
            Last Update: {lastUpdate.toLocaleTimeString()} | DWSIM OTS v1.0.0-alpha
          </Typography>
        </Paper>
      </Box>
    </ThemeProvider>
  );
}

export default App;
