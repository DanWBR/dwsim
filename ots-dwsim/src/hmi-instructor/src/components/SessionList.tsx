import { useState, useEffect } from 'react';
import {
  Box,
  Typography,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Paper,
  Chip,
  IconButton,
  Button,
  CircularProgress,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
} from '@mui/material';
import {
  PlayArrow,
  Stop,
  Delete,
  Assessment,
  Replay,
  CameraAlt,
} from '@mui/icons-material';
import ApiClient, { SessionInfo } from '../services/ApiClient';

function SessionList() {
  const [sessions, setSessions] = useState<SessionInfo[]>([]);
  const [loading, setLoading] = useState(true);
  const [selectedSession, setSelectedSession] = useState<string | null>(null);
  const [assessDialogOpen, setAssessDialogOpen] = useState(false);

  const fetchSessions = async () => {
    try {
      const data = await ApiClient.getAllSessions();
      setSessions(data);
    } catch (error) {
      console.error('Error fetching sessions:', error);
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    fetchSessions();
    const interval = setInterval(fetchSessions, 3000);
    return () => clearInterval(interval);
  }, []);

  const handleStart = async (sessionId: string) => {
    try {
      await ApiClient.startSession(sessionId);
      fetchSessions();
    } catch (error) {
      console.error('Error starting session:', error);
    }
  };

  const handleStop = async (sessionId: string) => {
    try {
      await ApiClient.stopSession(sessionId);
      fetchSessions();
    } catch (error) {
      console.error('Error stopping session:', error);
    }
  };

  const handleDelete = async (sessionId: string) => {
    if (confirm('Are you sure you want to delete this session?')) {
      try {
        await ApiClient.deleteSession(sessionId);
        fetchSessions();
      } catch (error) {
        console.error('Error deleting session:', error);
      }
    }
  };

  const handleAssess = async (sessionId: string) => {
    setSelectedSession(sessionId);
    setAssessDialogOpen(true);
  };

  const handleConfirmAssess = async () => {
    if (selectedSession) {
      try {
        await ApiClient.assessSession(selectedSession);
        alert('Assessment completed successfully!');
        setAssessDialogOpen(false);
      } catch (error) {
        console.error('Error assessing session:', error);
        alert('Error creating assessment');
      }
    }
  };

  const handleReplay = async (sessionId: string) => {
    try {
      await ApiClient.startReplay(sessionId);
      alert('Replay started successfully!');
    } catch (error) {
      console.error('Error starting replay:', error);
      alert('Error starting replay');
    }
  };

  const handleSnapshot = async (sessionId: string) => {
    const name = prompt('Enter snapshot name:');
    if (name) {
      try {
        await ApiClient.createSnapshot(sessionId, name);
        alert('Snapshot created successfully!');
      } catch (error) {
        console.error('Error creating snapshot:', error);
        alert('Error creating snapshot');
      }
    }
  };

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'Running':
        return 'success';
      case 'Paused':
        return 'warning';
      case 'Stopped':
        return 'default';
      case 'Error':
        return 'error';
      default:
        return 'info';
    }
  };

  if (loading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="60vh">
        <CircularProgress />
      </Box>
    );
  }

  return (
    <Box>
      <Typography variant="h4" gutterBottom>
        Training Sessions
      </Typography>
      <Typography variant="subtitle1" color="text.secondary" gutterBottom>
        Manage and monitor all operator training sessions
      </Typography>

      <TableContainer component={Paper} sx={{ mt: 3 }}>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>Session Name</TableCell>
              <TableCell>Session ID</TableCell>
              <TableCell>Flowsheet</TableCell>
              <TableCell>Status</TableCell>
              <TableCell>Created</TableCell>
              <TableCell>Started</TableCell>
              <TableCell>Actions</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {sessions.map((session) => (
              <TableRow key={session.sessionId}>
                <TableCell>
                  <Typography variant="body2" fontWeight="bold">
                    {session.sessionName}
                  </Typography>
                  {session.description && (
                    <Typography variant="caption" color="text.secondary">
                      {session.description}
                    </Typography>
                  )}
                </TableCell>
                <TableCell>
                  <Typography variant="caption" sx={{ fontFamily: 'monospace' }}>
                    {session.sessionId.substring(0, 8)}...
                  </Typography>
                </TableCell>
                <TableCell>{session.flowsheetId}</TableCell>
                <TableCell>
                  <Chip
                    label={session.status}
                    color={getStatusColor(session.status) as any}
                    size="small"
                  />
                </TableCell>
                <TableCell>
                  <Typography variant="caption">
                    {new Date(session.createdAt).toLocaleString()}
                  </Typography>
                </TableCell>
                <TableCell>
                  <Typography variant="caption">
                    {session.startedAt
                      ? new Date(session.startedAt).toLocaleString()
                      : '-'}
                  </Typography>
                </TableCell>
                <TableCell>
                  <Box display="flex" gap={0.5}>
                    {session.status !== 'Running' && (
                      <IconButton
                        size="small"
                        color="success"
                        onClick={() => handleStart(session.sessionId)}
                        title="Start Session"
                      >
                        <PlayArrow />
                      </IconButton>
                    )}
                    {session.status === 'Running' && (
                      <IconButton
                        size="small"
                        color="warning"
                        onClick={() => handleStop(session.sessionId)}
                        title="Stop Session"
                      >
                        <Stop />
                      </IconButton>
                    )}
                    {session.status === 'Running' && (
                      <IconButton
                        size="small"
                        color="info"
                        onClick={() => handleSnapshot(session.sessionId)}
                        title="Create Snapshot"
                      >
                        <CameraAlt />
                      </IconButton>
                    )}
                    {session.status === 'Stopped' && (
                      <>
                        <IconButton
                          size="small"
                          color="primary"
                          onClick={() => handleAssess(session.sessionId)}
                          title="Assess Performance"
                        >
                          <Assessment />
                        </IconButton>
                        <IconButton
                          size="small"
                          color="secondary"
                          onClick={() => handleReplay(session.sessionId)}
                          title="Replay Session"
                        >
                          <Replay />
                        </IconButton>
                      </>
                    )}
                    <IconButton
                      size="small"
                      color="error"
                      onClick={() => handleDelete(session.sessionId)}
                      title="Delete Session"
                    >
                      <Delete />
                    </IconButton>
                  </Box>
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
        {sessions.length === 0 && (
          <Box p={4} textAlign="center">
            <Typography color="text.secondary">No sessions found</Typography>
          </Box>
        )}
      </TableContainer>

      {/* Assessment Confirmation Dialog */}
      <Dialog open={assessDialogOpen} onClose={() => setAssessDialogOpen(false)}>
        <DialogTitle>Create Assessment</DialogTitle>
        <DialogContent>
          <Typography>
            Create an assessment for session: {selectedSession?.substring(0, 8)}...?
          </Typography>
          <Typography variant="body2" color="text.secondary" sx={{ mt: 2 }}>
            This will evaluate operator performance using the default rule set.
          </Typography>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setAssessDialogOpen(false)}>Cancel</Button>
          <Button onClick={handleConfirmAssess} variant="contained" color="primary">
            Create Assessment
          </Button>
        </DialogActions>
      </Dialog>
    </Box>
  );
}

export default SessionList;
