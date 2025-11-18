import { useState, useEffect } from 'react';
import {
  Box,
  Typography,
  Paper,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Chip,
  CircularProgress,
  LinearProgress,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  Button,
  Grid,
  Card,
  CardContent,
} from '@mui/material';
import { CheckCircle, Cancel, Visibility } from '@mui/icons-material';
import ApiClient, { ReplayResponse, ReplayComparison } from '../services/ApiClient';

function ReplayManager() {
  const [replays, setReplays] = useState<ReplayResponse[]>([]);
  const [loading, setLoading] = useState(true);
  const [viewDialogOpen, setViewDialogOpen] = useState(false);
  const [selectedComparison, setSelectedComparison] = useState<ReplayComparison | null>(null);

  const fetchReplays = async () => {
    try {
      // In a real implementation, we'd fetch all replays from a list endpoint
      // For now, we'll just show empty state
      setReplays([]);
    } catch (error) {
      console.error('Error fetching replays:', error);
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    fetchReplays();
    const interval = setInterval(fetchReplays, 3000);
    return () => clearInterval(interval);
  }, []);

  const handleViewComparison = async (replayId: string) => {
    try {
      const comparison = await ApiClient.getReplayComparison(replayId);
      setSelectedComparison(comparison);
      setViewDialogOpen(true);
    } catch (error) {
      console.error('Error fetching comparison:', error);
    }
  };

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'Completed':
        return 'success';
      case 'Running':
        return 'info';
      case 'Failed':
        return 'error';
      case 'Cancelled':
        return 'warning';
      default:
        return 'default';
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
        Replay Manager
      </Typography>
      <Typography variant="subtitle1" color="text.secondary" gutterBottom>
        Monitor session replays and determinism verification
      </Typography>

      <TableContainer component={Paper} sx={{ mt: 3 }}>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>Replay ID</TableCell>
              <TableCell>Original Session</TableCell>
              <TableCell>Replay Session</TableCell>
              <TableCell>Status</TableCell>
              <TableCell>Progress</TableCell>
              <TableCell>Created</TableCell>
              <TableCell>Actions</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {replays.map((replay) => (
              <TableRow key={replay.ReplayId}>
                <TableCell>
                  <Typography variant="caption" sx={{ fontFamily: 'monospace' }}>
                    {replay.ReplayId.substring(0, 8)}...
                  </Typography>
                </TableCell>
                <TableCell>
                  <Typography variant="caption" sx={{ fontFamily: 'monospace' }}>
                    {replay.OriginalSessionId.substring(0, 8)}...
                  </Typography>
                </TableCell>
                <TableCell>
                  <Typography variant="caption" sx={{ fontFamily: 'monospace' }}>
                    {replay.ReplaySessionId
                      ? replay.ReplaySessionId.substring(0, 8) + '...'
                      : '-'}
                  </Typography>
                </TableCell>
                <TableCell>
                  <Chip
                    label={replay.Status}
                    color={getStatusColor(replay.Status) as any}
                    size="small"
                  />
                </TableCell>
                <TableCell>
                  <Box display="flex" alignItems="center" gap={1}>
                    <Box flex={1}>
                      <LinearProgress
                        variant="determinate"
                        value={replay.ProgressPercent}
                        sx={{ height: 8, borderRadius: 1 }}
                      />
                    </Box>
                    <Typography variant="caption">
                      {Math.round(replay.ProgressPercent)}%
                    </Typography>
                  </Box>
                </TableCell>
                <TableCell>
                  <Typography variant="caption">
                    {new Date(replay.CreatedAt).toLocaleString()}
                  </Typography>
                </TableCell>
                <TableCell>
                  {replay.Status === 'Completed' && (
                    <Button
                      size="small"
                      variant="outlined"
                      startIcon={<Visibility />}
                      onClick={() => handleViewComparison(replay.ReplayId)}
                    >
                      View Comparison
                    </Button>
                  )}
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
        {replays.length === 0 && (
          <Box p={4} textAlign="center">
            <Typography color="text.secondary">
              No replays found. Create a replay from a stopped session to verify determinism.
            </Typography>
          </Box>
        )}
      </TableContainer>

      {/* View Comparison Dialog */}
      <Dialog
        open={viewDialogOpen}
        onClose={() => setViewDialogOpen(false)}
        maxWidth="lg"
        fullWidth
      >
        <DialogTitle>
          Determinism Verification
          {selectedComparison && (
            <Box display="flex" gap={1} mt={1}>
              <Chip
                label={`Score: ${(selectedComparison.DeterminismScore * 100).toFixed(1)}%`}
                color="primary"
                size="small"
              />
              {selectedComparison.IsDeterministic ? (
                <Chip icon={<CheckCircle />} label="DETERMINISTIC" color="success" size="small" />
              ) : (
                <Chip
                  icon={<Cancel />}
                  label="NON-DETERMINISTIC"
                  color="error"
                  size="small"
                />
              )}
            </Box>
          )}
        </DialogTitle>
        <DialogContent>
          {selectedComparison && (
            <Box>
              <Grid container spacing={2} sx={{ mb: 3 }}>
                <Grid item xs={12} sm={6} md={3}>
                  <Card>
                    <CardContent>
                      <Typography color="text.secondary" variant="caption">
                        Tags Compared
                      </Typography>
                      <Typography variant="h5">
                        {selectedComparison.Stats.TotalTagsCompared}
                      </Typography>
                    </CardContent>
                  </Card>
                </Grid>
                <Grid item xs={12} sm={6} md={3}>
                  <Card>
                    <CardContent>
                      <Typography color="text.secondary" variant="caption">
                        Passing
                      </Typography>
                      <Typography variant="h5" color="success.main">
                        {selectedComparison.Stats.TagsPassing}
                      </Typography>
                    </CardContent>
                  </Card>
                </Grid>
                <Grid item xs={12} sm={6} md={3}>
                  <Card>
                    <CardContent>
                      <Typography color="text.secondary" variant="caption">
                        Failing
                      </Typography>
                      <Typography variant="h5" color="error.main">
                        {selectedComparison.Stats.TagsFailing}
                      </Typography>
                    </CardContent>
                  </Card>
                </Grid>
                <Grid item xs={12} sm={6} md={3}>
                  <Card>
                    <CardContent>
                      <Typography color="text.secondary" variant="caption">
                        Avg Correlation
                      </Typography>
                      <Typography variant="h5">
                        {(selectedComparison.Stats.AverageCorrelation * 100).toFixed(1)}%
                      </Typography>
                    </CardContent>
                  </Card>
                </Grid>
              </Grid>

              <Typography variant="h6" gutterBottom>
                Tag Comparisons
              </Typography>

              {selectedComparison.TagComparisons.map((tag, index) => (
                <Paper key={index} sx={{ p: 2, mb: 2, bgcolor: 'background.default' }}>
                  <Box display="flex" justifyContent="space-between" alignItems="start">
                    <Box flex={1}>
                      <Typography variant="body1" fontWeight="bold">
                        {tag.TagPath}
                      </Typography>
                      <Grid container spacing={2} sx={{ mt: 1 }}>
                        <Grid item xs={12} sm={6} md={3}>
                          <Typography variant="caption" color="text.secondary">
                            Correlation
                          </Typography>
                          <Typography variant="body2">
                            {(tag.Correlation * 100).toFixed(2)}%
                          </Typography>
                        </Grid>
                        <Grid item xs={12} sm={6} md={3}>
                          <Typography variant="caption" color="text.secondary">
                            RMSE
                          </Typography>
                          <Typography variant="body2">
                            {tag.RootMeanSquareError.toFixed(4)}
                          </Typography>
                        </Grid>
                        <Grid item xs={12} sm={6} md={3}>
                          <Typography variant="caption" color="text.secondary">
                            Max Deviation
                          </Typography>
                          <Typography variant="body2">
                            {tag.MaxDeviationPercent.toFixed(3)}%
                          </Typography>
                        </Grid>
                        <Grid item xs={12} sm={6} md={3}>
                          <Typography variant="caption" color="text.secondary">
                            Points
                          </Typography>
                          <Typography variant="body2">{tag.DataPointsCompared}</Typography>
                        </Grid>
                      </Grid>
                    </Box>
                    <Box>
                      {tag.PassesDeterminismCheck ? (
                        <Chip icon={<CheckCircle />} label="PASS" color="success" size="small" />
                      ) : (
                        <Chip icon={<Cancel />} label="FAIL" color="error" size="small" />
                      )}
                    </Box>
                  </Box>
                </Paper>
              ))}

              <Paper sx={{ p: 2, mt: 3, bgcolor: 'info.dark' }}>
                <Typography variant="body2">
                  <strong>Determinism Threshold:</strong> Maximum allowed deviation is{' '}
                  {selectedComparison.MaxAllowedDeviation}%
                </Typography>
                <Typography variant="body2" sx={{ mt: 1 }}>
                  {selectedComparison.IsDeterministic
                    ? 'All tags are within the acceptable deviation range. The simulation is deterministic and reproducible.'
                    : 'Some tags exceeded the acceptable deviation. Review the failing tags to identify sources of non-determinism.'}
                </Typography>
              </Paper>
            </Box>
          )}
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setViewDialogOpen(false)}>Close</Button>
        </DialogActions>
      </Dialog>
    </Box>
  );
}

export default ReplayManager;
