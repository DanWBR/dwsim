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
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  Button,
  Grid,
  Card,
  CardContent,
  LinearProgress,
} from '@mui/material';
import { CheckCircle, Cancel, Visibility } from '@mui/icons-material';
import ApiClient, { SessionInfo, AssessmentReport } from '../services/ApiClient';

function AssessmentViewer() {
  const [sessions, setSessions] = useState<SessionInfo[]>([]);
  const [assessments, setAssessments] = useState<AssessmentReport[]>([]);
  const [loading, setLoading] = useState(true);
  const [viewDialogOpen, setViewDialogOpen] = useState(false);
  const [selectedAssessment, setSelectedAssessment] = useState<AssessmentReport | null>(null);

  const fetchData = async () => {
    try {
      const sessionData = await ApiClient.getAllSessions();
      setSessions(sessionData);

      const allAssessments: AssessmentReport[] = [];
      for (const session of sessionData) {
        const sessionAssessments = await ApiClient.getSessionAssessments(session.sessionId);
        allAssessments.push(...sessionAssessments);
      }
      setAssessments(allAssessments);
    } catch (error) {
      console.error('Error fetching assessments:', error);
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    fetchData();
  }, []);

  const handleView = (assessment: AssessmentReport) => {
    setSelectedAssessment(assessment);
    setViewDialogOpen(true);
  };

  const getGradeColor = (grade: string) => {
    switch (grade) {
      case 'A':
        return 'success';
      case 'B':
        return 'info';
      case 'C':
        return 'warning';
      default:
        return 'error';
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
        Assessment Reports
      </Typography>
      <Typography variant="subtitle1" color="text.secondary" gutterBottom>
        View and analyze operator performance assessments
      </Typography>

      <TableContainer component={Paper} sx={{ mt: 3 }}>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>Assessment ID</TableCell>
              <TableCell>Session</TableCell>
              <TableCell>Generated</TableCell>
              <TableCell>Score</TableCell>
              <TableCell>Grade</TableCell>
              <TableCell>Status</TableCell>
              <TableCell>KPIs</TableCell>
              <TableCell>Actions</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {assessments.map((assessment) => (
              <TableRow key={assessment.AssessmentId}>
                <TableCell>
                  <Typography variant="caption" sx={{ fontFamily: 'monospace' }}>
                    {assessment.AssessmentId.substring(0, 8)}...
                  </Typography>
                </TableCell>
                <TableCell>
                  <Typography variant="caption" sx={{ fontFamily: 'monospace' }}>
                    {assessment.SessionId.substring(0, 8)}...
                  </Typography>
                </TableCell>
                <TableCell>
                  <Typography variant="caption">
                    {new Date(assessment.GeneratedAt).toLocaleString()}
                  </Typography>
                </TableCell>
                <TableCell>
                  <Typography variant="body2" fontWeight="bold">
                    {Math.round(assessment.OverallScore)}/100
                  </Typography>
                </TableCell>
                <TableCell>
                  <Chip
                    label={assessment.Grade}
                    color={getGradeColor(assessment.Grade) as any}
                    size="small"
                  />
                </TableCell>
                <TableCell>
                  {assessment.Passed ? (
                    <Chip icon={<CheckCircle />} label="PASS" color="success" size="small" />
                  ) : (
                    <Chip icon={<Cancel />} label="FAIL" color="error" size="small" />
                  )}
                </TableCell>
                <TableCell>
                  <Typography variant="body2">
                    {assessment.Summary.KpisPassed}/{assessment.Summary.TotalKpis}
                  </Typography>
                </TableCell>
                <TableCell>
                  <Button
                    size="small"
                    variant="outlined"
                    startIcon={<Visibility />}
                    onClick={() => handleView(assessment)}
                  >
                    View
                  </Button>
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
        {assessments.length === 0 && (
          <Box p={4} textAlign="center">
            <Typography color="text.secondary">No assessments found</Typography>
          </Box>
        )}
      </TableContainer>

      {/* View Assessment Dialog */}
      <Dialog
        open={viewDialogOpen}
        onClose={() => setViewDialogOpen(false)}
        maxWidth="lg"
        fullWidth
      >
        <DialogTitle>
          Assessment Report
          {selectedAssessment && (
            <Box display="flex" gap={1} mt={1}>
              <Chip
                label={`Grade: ${selectedAssessment.Grade}`}
                color={getGradeColor(selectedAssessment.Grade) as any}
                size="small"
              />
              <Chip
                label={`Score: ${Math.round(selectedAssessment.OverallScore)}/100`}
                color="primary"
                size="small"
              />
              {selectedAssessment.Passed ? (
                <Chip icon={<CheckCircle />} label="PASS" color="success" size="small" />
              ) : (
                <Chip icon={<Cancel />} label="FAIL" color="error" size="small" />
              )}
            </Box>
          )}
        </DialogTitle>
        <DialogContent>
          {selectedAssessment && (
            <Box>
              <Grid container spacing={2} sx={{ mb: 3 }}>
                <Grid item xs={12} sm={6} md={3}>
                  <Card>
                    <CardContent>
                      <Typography color="text.secondary" variant="caption">
                        Total KPIs
                      </Typography>
                      <Typography variant="h5">
                        {selectedAssessment.Summary.TotalKpis}
                      </Typography>
                    </CardContent>
                  </Card>
                </Grid>
                <Grid item xs={12} sm={6} md={3}>
                  <Card>
                    <CardContent>
                      <Typography color="text.secondary" variant="caption">
                        Passed
                      </Typography>
                      <Typography variant="h5" color="success.main">
                        {selectedAssessment.Summary.KpisPassed}
                      </Typography>
                    </CardContent>
                  </Card>
                </Grid>
                <Grid item xs={12} sm={6} md={3}>
                  <Card>
                    <CardContent>
                      <Typography color="text.secondary" variant="caption">
                        Failed
                      </Typography>
                      <Typography variant="h5" color="error.main">
                        {selectedAssessment.Summary.KpisFailed}
                      </Typography>
                    </CardContent>
                  </Card>
                </Grid>
                <Grid item xs={12} sm={6} md={3}>
                  <Card>
                    <CardContent>
                      <Typography color="text.secondary" variant="caption">
                        Alarms
                      </Typography>
                      <Typography variant="h5">
                        {selectedAssessment.Summary.TotalAlarms}
                      </Typography>
                    </CardContent>
                  </Card>
                </Grid>
              </Grid>

              <Typography variant="h6" gutterBottom>
                KPI Results
              </Typography>

              {selectedAssessment.KpiResults.map((kpi, index) => (
                <Paper key={index} sx={{ p: 2, mb: 2, bgcolor: 'background.default' }}>
                  <Box display="flex" justifyContent="space-between" alignItems="start">
                    <Box flex={1}>
                      <Typography variant="body1" fontWeight="bold">
                        {kpi.Name}
                      </Typography>
                      <Box display="flex" gap={1} mt={1} alignItems="center">
                        <Chip
                          label={kpi.Level}
                          size="small"
                          color={
                            kpi.Level === 'Excellent'
                              ? 'success'
                              : kpi.Level === 'Good'
                              ? 'info'
                              : kpi.Level === 'Acceptable'
                              ? 'warning'
                              : 'error'
                          }
                        />
                        <Typography variant="body2">
                          Actual: {kpi.ActualValue.toFixed(2)} {kpi.Units}
                        </Typography>
                        {kpi.TargetValue && (
                          <Typography variant="body2" color="text.secondary">
                            Target: {kpi.TargetValue.toFixed(2)}
                          </Typography>
                        )}
                      </Box>
                    </Box>
                    <Box textAlign="right" minWidth={120}>
                      <Typography variant="h5" color={kpi.Passed ? 'success.main' : 'error.main'}>
                        {Math.round(kpi.Score)}
                      </Typography>
                      <Typography variant="caption" color="text.secondary">
                        Score
                      </Typography>
                    </Box>
                  </Box>
                  <Box mt={2}>
                    <LinearProgress
                      variant="determinate"
                      value={kpi.Score}
                      color={kpi.Passed ? 'success' : 'error'}
                    />
                  </Box>
                </Paper>
              ))}

              {selectedAssessment.Recommendations.length > 0 && (
                <Box mt={3}>
                  <Typography variant="h6" gutterBottom>
                    Recommendations
                  </Typography>
                  <Paper sx={{ p: 2, bgcolor: 'info.dark' }}>
                    <ul style={{ margin: 0, paddingLeft: '20px' }}>
                      {selectedAssessment.Recommendations.map((rec, index) => (
                        <li key={index}>
                          <Typography variant="body2">{rec}</Typography>
                        </li>
                      ))}
                    </ul>
                  </Paper>
                </Box>
              )}
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

export default AssessmentViewer;
