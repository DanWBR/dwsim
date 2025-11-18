import { useState } from 'react';
import {
  Box,
  Typography,
  Paper,
  TextField,
  Button,
  Alert,
  Card,
  CardContent,
} from '@mui/material';
import { CheckCircle, Error, Upload } from '@mui/icons-material';
import ApiClient, { Scenario } from '../services/ApiClient';

function ScenarioManager() {
  const [scenarioJson, setScenarioJson] = useState('');
  const [validationResult, setValidationResult] = useState<{
    valid: boolean;
    errors?: string[];
  } | null>(null);
  const [scenario, setScenario] = useState<Scenario | null>(null);

  const handleValidate = async () => {
    try {
      const parsed = JSON.parse(scenarioJson);
      setScenario(parsed);

      const result = await ApiClient.validateScenario(parsed);
      setValidationResult(result);
    } catch (error: any) {
      setValidationResult({
        valid: false,
        errors: [error.message || 'Invalid JSON format'],
      });
    }
  };

  const handleLoadExample = () => {
    const exampleScenario = {
      scenario_id: 'example_scenario_v1',
      description: 'Example distillation column startup with composition upset',
      seed: 12345,
      duration_s: 1800,
      initial_state: {
        'Units.Column1.Status': 'idle',
        'Streams.Feed.Flow': 5.0,
        'Streams.Feed.Temperature': 25.0,
      },
      events: [
        {
          time_s: 0,
          type: 'set',
          target: 'Units.Column1.Status',
          payload: 'start',
        },
        {
          time_s: 300,
          type: 'note',
          payload: 'Column startup phase complete',
        },
        {
          time_s: 600,
          type: 'fault',
          target: 'Streams.Feed.Composition',
          payload: {
            description: 'Feed composition upset',
            A: 0.3,
            B: 0.7,
          },
        },
        {
          time_s: 900,
          type: 'random_fault',
          target: 'Units.Column1.Controller',
          payload: {
            fault_types: ['sensor_bias', 'actuator_stuck'],
            probability: 0.3,
          },
        },
      ],
      pass_criteria: {
        max_alarms: 5,
        min_uptime_percent: 90,
        max_temp_deviation: 10,
      },
    };

    setScenarioJson(JSON.stringify(exampleScenario, null, 2));
    setValidationResult(null);
  };

  return (
    <Box>
      <Typography variant="h4" gutterBottom>
        Scenario Manager
      </Typography>
      <Typography variant="subtitle1" color="text.secondary" gutterBottom>
        Create, validate, and manage training scenarios
      </Typography>

      <Box sx={{ mt: 3, display: 'flex', gap: 2, flexDirection: { xs: 'column', md: 'row' } }}>
        {/* Editor Panel */}
        <Paper sx={{ flex: 1, p: 2 }}>
          <Box display="flex" justifyContent="space-between" alignItems="center" mb={2}>
            <Typography variant="h6">Scenario JSON</Typography>
            <Box display="flex" gap={1}>
              <Button
                variant="outlined"
                size="small"
                startIcon={<Upload />}
                onClick={handleLoadExample}
              >
                Load Example
              </Button>
              <Button
                variant="contained"
                size="small"
                onClick={handleValidate}
                disabled={!scenarioJson}
              >
                Validate
              </Button>
            </Box>
          </Box>

          <TextField
            multiline
            rows={25}
            fullWidth
            value={scenarioJson}
            onChange={(e) => setScenarioJson(e.target.value)}
            placeholder="Paste your scenario JSON here or load an example..."
            sx={{
              fontFamily: 'monospace',
              '& .MuiInputBase-input': {
                fontFamily: 'monospace',
                fontSize: '0.875rem',
              },
            }}
          />
        </Paper>

        {/* Validation Results Panel */}
        <Box sx={{ flex: 1 }}>
          <Paper sx={{ p: 2, mb: 2 }}>
            <Typography variant="h6" gutterBottom>
              Validation Results
            </Typography>

            {!validationResult && (
              <Alert severity="info">
                Enter or paste a scenario JSON and click "Validate" to check if it meets the schema
                requirements.
              </Alert>
            )}

            {validationResult && validationResult.valid && (
              <Alert severity="success" icon={<CheckCircle />}>
                Scenario is valid and ready to use!
              </Alert>
            )}

            {validationResult && !validationResult.valid && (
              <Alert severity="error" icon={<Error />}>
                <Typography variant="body2" fontWeight="bold" gutterBottom>
                  Validation Errors:
                </Typography>
                <ul style={{ margin: '8px 0', paddingLeft: '20px' }}>
                  {validationResult.errors?.map((error, index) => (
                    <li key={index}>
                      <Typography variant="body2">{error}</Typography>
                    </li>
                  ))}
                </ul>
              </Alert>
            )}
          </Paper>

          {scenario && validationResult?.valid && (
            <Paper sx={{ p: 2 }}>
              <Typography variant="h6" gutterBottom>
                Scenario Details
              </Typography>

              <Card sx={{ mb: 2, bgcolor: 'background.default' }}>
                <CardContent>
                  <Typography variant="subtitle2" color="text.secondary">
                    Scenario ID
                  </Typography>
                  <Typography variant="body1" fontWeight="bold" gutterBottom>
                    {scenario.scenario_id}
                  </Typography>

                  <Typography variant="subtitle2" color="text.secondary" sx={{ mt: 2 }}>
                    Description
                  </Typography>
                  <Typography variant="body2" gutterBottom>
                    {scenario.description}
                  </Typography>

                  <Typography variant="subtitle2" color="text.secondary" sx={{ mt: 2 }}>
                    Duration
                  </Typography>
                  <Typography variant="body2" gutterBottom>
                    {scenario.duration_s} seconds ({Math.round(scenario.duration_s / 60)} minutes)
                  </Typography>

                  <Typography variant="subtitle2" color="text.secondary" sx={{ mt: 2 }}>
                    Random Seed
                  </Typography>
                  <Typography variant="body2" gutterBottom>
                    {scenario.seed}
                  </Typography>

                  <Typography variant="subtitle2" color="text.secondary" sx={{ mt: 2 }}>
                    Number of Events
                  </Typography>
                  <Typography variant="body2" gutterBottom>
                    {scenario.events.length}
                  </Typography>
                </CardContent>
              </Card>

              <Typography variant="subtitle1" gutterBottom>
                Event Timeline
              </Typography>
              {scenario.events.map((event, index) => (
                <Card key={index} sx={{ mb: 1, bgcolor: 'background.default' }}>
                  <CardContent sx={{ py: 1.5 }}>
                    <Box display="flex" justifyContent="space-between" alignItems="center">
                      <Box>
                        <Typography variant="body2" fontWeight="bold">
                          t = {event.time_s}s: {event.type}
                        </Typography>
                        {event.target && (
                          <Typography variant="caption" color="text.secondary">
                            Target: {event.target}
                          </Typography>
                        )}
                      </Box>
                      <Box
                        sx={{
                          px: 1.5,
                          py: 0.5,
                          borderRadius: 1,
                          bgcolor:
                            event.type === 'fault' || event.type === 'random_fault'
                              ? 'error.main'
                              : event.type === 'set'
                              ? 'primary.main'
                              : event.type === 'controller'
                              ? 'warning.main'
                              : 'info.main',
                        }}
                      >
                        <Typography variant="caption">{event.type}</Typography>
                      </Box>
                    </Box>
                  </CardContent>
                </Card>
              ))}
            </Paper>
          )}
        </Box>
      </Box>

      <Paper sx={{ p: 2, mt: 3 }}>
        <Typography variant="h6" gutterBottom>
          Scenario Schema Reference
        </Typography>
        <Typography variant="body2" color="text.secondary" paragraph>
          A valid scenario must include:
        </Typography>
        <ul>
          <li>
            <Typography variant="body2">
              <strong>scenario_id:</strong> Unique identifier (string)
            </Typography>
          </li>
          <li>
            <Typography variant="body2">
              <strong>description:</strong> Human-readable description
            </Typography>
          </li>
          <li>
            <Typography variant="body2">
              <strong>seed:</strong> Random seed for reproducibility (integer)
            </Typography>
          </li>
          <li>
            <Typography variant="body2">
              <strong>duration_s:</strong> Total scenario duration in seconds
            </Typography>
          </li>
          <li>
            <Typography variant="body2">
              <strong>events:</strong> Array of timed events (fault, set, controller, note,
              random_fault)
            </Typography>
          </li>
          <li>
            <Typography variant="body2">
              <strong>pass_criteria:</strong> Optional criteria for operator success
            </Typography>
          </li>
        </ul>
      </Paper>
    </Box>
  );
}

export default ScenarioManager;
