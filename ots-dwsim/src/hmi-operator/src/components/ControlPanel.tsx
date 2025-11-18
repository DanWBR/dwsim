import React, { useState } from 'react';
import {
  Box,
  TextField,
  Button,
  Typography,
  Slider,
  Stack,
  Alert,
  Snackbar
} from '@mui/material';
import { Send } from '@mui/icons-material';
import { apiClient } from '../services/ApiClient';
import { ProcessVariable } from '../types';

interface ControlPanelProps {
  sessionId: string;
  processVars: ProcessVariable[];
}

const ControlPanel: React.FC<ControlPanelProps> = ({ sessionId, processVars }) => {
  const [flowSetpoint, setFlowSetpoint] = useState(2.0);
  const [tempSetpoint, setTempSetpoint] = useState(298.15);
  const [notification, setNotification] = useState<{
    open: boolean;
    message: string;
    severity: 'success' | 'error';
  }>({ open: false, message: '', severity: 'success' });

  const handleWriteFlow = async () => {
    if (!sessionId) {
      showNotification('No active session', 'error');
      return;
    }

    try {
      await apiClient.writeTag(sessionId, 'Streams.Feed.Flow', {
        value: flowSetpoint,
        user: 'operator',
        mode: 'manual'
      });

      showNotification(`Feed flow set to ${flowSetpoint} kg/s`, 'success');
    } catch (error) {
      console.error('Error writing flow:', error);
      showNotification('Failed to write flow setpoint', 'error');
    }
  };

  const handleWriteTemp = async () => {
    if (!sessionId) {
      showNotification('No active session', 'error');
      return;
    }

    try {
      await apiClient.writeTag(sessionId, 'Streams.Feed.Temperature', {
        value: tempSetpoint,
        user: 'operator',
        mode: 'manual'
      });

      showNotification(`Feed temperature set to ${tempSetpoint} K`, 'success');
    } catch (error) {
      console.error('Error writing temperature:', error);
      showNotification('Failed to write temperature setpoint', 'error');
    }
  };

  const handleEmergencyStop = async () => {
    if (!sessionId) return;

    try {
      // Set flow to zero
      await apiClient.writeTag(sessionId, 'Streams.Feed.Flow', {
        value: 0,
        user: 'operator',
        mode: 'emergency'
      });

      showNotification('EMERGENCY STOP ACTIVATED', 'success');
    } catch (error) {
      showNotification('Failed to execute emergency stop', 'error');
    }
  };

  const showNotification = (message: string, severity: 'success' | 'error') => {
    setNotification({ open: true, message, severity });
  };

  const handleCloseNotification = () => {
    setNotification({ ...notification, open: false });
  };

  // Get current values from process vars
  const getCurrentValue = (varName: string): number | null => {
    const pv = processVars.find(v => v.tagPath.includes(varName));
    return typeof pv?.value === 'number' ? pv.value : null;
  };

  const currentFlow = getCurrentValue('Flow');
  const currentTemp = getCurrentValue('Temperature');

  return (
    <Box>
      <Stack spacing={3}>
        {/* Flow Control */}
        <Box>
          <Typography variant="body2" gutterBottom>
            Feed Flow Rate
          </Typography>
          <Typography variant="caption" color="text.secondary" gutterBottom display="block">
            Current: {currentFlow !== null ? `${currentFlow.toFixed(2)} kg/s` : '--'}
          </Typography>
          <Slider
            value={flowSetpoint}
            onChange={(_, value) => setFlowSetpoint(value as number)}
            min={0}
            max={10}
            step={0.1}
            marks={[
              { value: 0, label: '0' },
              { value: 5, label: '5' },
              { value: 10, label: '10' }
            ]}
            valueLabelDisplay="auto"
            valueLabelFormat={(value) => `${value.toFixed(1)} kg/s`}
          />
          <Button
            variant="contained"
            size="small"
            fullWidth
            startIcon={<Send />}
            onClick={handleWriteFlow}
            sx={{ mt: 1 }}
          >
            Set Flow: {flowSetpoint.toFixed(1)} kg/s
          </Button>
        </Box>

        {/* Temperature Control */}
        <Box>
          <Typography variant="body2" gutterBottom>
            Feed Temperature
          </Typography>
          <Typography variant="caption" color="text.secondary" gutterBottom display="block">
            Current: {currentTemp !== null ? `${currentTemp.toFixed(2)} K` : '--'}
          </Typography>
          <TextField
            type="number"
            value={tempSetpoint}
            onChange={(e) => setTempSetpoint(parseFloat(e.target.value))}
            fullWidth
            size="small"
            inputProps={{
              min: 273.15,
              max: 600,
              step: 1
            }}
            helperText="Range: 273-600 K"
          />
          <Button
            variant="contained"
            size="small"
            fullWidth
            startIcon={<Send />}
            onClick={handleWriteTemp}
            sx={{ mt: 1 }}
          >
            Set Temperature
          </Button>
        </Box>

        {/* Emergency Controls */}
        <Box sx={{ pt: 2, borderTop: '1px solid #333' }}>
          <Button
            variant="contained"
            color="error"
            fullWidth
            onClick={handleEmergencyStop}
            sx={{ fontWeight: 'bold' }}
          >
            EMERGENCY STOP
          </Button>
        </Box>

        {/* Info */}
        <Typography variant="caption" color="text.secondary" sx={{ pt: 1 }}>
          All control actions are logged for training assessment.
        </Typography>
      </Stack>

      {/* Notification Snackbar */}
      <Snackbar
        open={notification.open}
        autoHideDuration={3000}
        onClose={handleCloseNotification}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
      >
        <Alert
          onClose={handleCloseNotification}
          severity={notification.severity}
          variant="filled"
        >
          {notification.message}
        </Alert>
      </Snackbar>
    </Box>
  );
};

export default ControlPanel;
