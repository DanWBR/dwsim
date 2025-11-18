import React, { useState, useEffect } from 'react';
import {
  List,
  ListItem,
  ListItemText,
  Chip,
  Box,
  Typography
} from '@mui/material';
import {
  Warning,
  Error,
  Info
} from '@mui/icons-material';
import { Alarm } from '../types';

const AlarmList: React.FC = () => {
  const [alarms, setAlarms] = useState<Alarm[]>([]);

  useEffect(() => {
    // In production, this would poll the backend for active alarms
    // For now, we'll show demo alarms

    const demoAlarms: Alarm[] = [
      {
        id: '1',
        tag: 'Streams.Feed.Temperature',
        severity: 'high',
        message: 'High temperature detected in feed stream',
        active: true,
        raised_at: new Date(Date.now() - 5000).toISOString()
      },
      {
        id: '2',
        tag: 'Units.Column1.Pressure',
        severity: 'medium',
        message: 'Column pressure approaching high limit',
        active: true,
        raised_at: new Date(Date.now() - 15000).toISOString()
      }
    ];

    setAlarms(demoAlarms);

    // Simulate alarm updates
    const interval = setInterval(() => {
      // In production, fetch real alarms from backend
      setAlarms(prev => {
        // Randomly clear some alarms
        return prev.map(alarm => ({
          ...alarm,
          active: Math.random() > 0.1
        }));
      });
    }, 5000);

    return () => clearInterval(interval);
  }, []);

  const getSeverityColor = (severity: Alarm['severity']): 'error' | 'warning' | 'info' | 'default' => {
    switch (severity) {
      case 'critical': return 'error';
      case 'high': return 'error';
      case 'medium': return 'warning';
      case 'low': return 'info';
      default: return 'default';
    }
  };

  const getSeverityIcon = (severity: Alarm['severity']) => {
    switch (severity) {
      case 'critical':
      case 'high':
        return <Error fontSize="small" />;
      case 'medium':
        return <Warning fontSize="small" />;
      default:
        return <Info fontSize="small" />;
    }
  };

  const activeAlarms = alarms.filter(a => a.active);

  return (
    <Box>
      {activeAlarms.length === 0 ? (
        <Box sx={{ textAlign: 'center', py: 4 }}>
          <Typography variant="body2" color="text.secondary">
            No active alarms
          </Typography>
          <Typography variant="caption" color="success.main" sx={{ mt: 1, display: 'block' }}>
            ✓ System operating normally
          </Typography>
        </Box>
      ) : (
        <List dense>
          {activeAlarms.map((alarm) => (
            <ListItem
              key={alarm.id}
              sx={{
                borderLeft: `4px solid`,
                borderLeftColor: `${getSeverityColor(alarm.severity)}.main`,
                mb: 1,
                bgcolor: 'background.default',
                borderRadius: 1
              }}
            >
              <ListItemText
                primary={
                  <Box sx={{ display: 'flex', alignItems: 'center', gap: 1, mb: 0.5 }}>
                    {getSeverityIcon(alarm.severity)}
                    <Typography variant="body2" sx={{ fontWeight: 'bold' }}>
                      {alarm.message}
                    </Typography>
                  </Box>
                }
                secondary={
                  <Box>
                    <Typography variant="caption" display="block">
                      Tag: {alarm.tag}
                    </Typography>
                    <Typography variant="caption" color="text.secondary">
                      Raised: {new Date(alarm.raised_at!).toLocaleTimeString()}
                    </Typography>
                    <Box sx={{ mt: 0.5 }}>
                      <Chip
                        label={alarm.severity.toUpperCase()}
                        size="small"
                        color={getSeverityColor(alarm.severity)}
                        sx={{ height: 20, fontSize: '0.7rem' }}
                      />
                    </Box>
                  </Box>
                }
              />
            </ListItem>
          ))}
        </List>
      )}
    </Box>
  );
};

export default AlarmList;
