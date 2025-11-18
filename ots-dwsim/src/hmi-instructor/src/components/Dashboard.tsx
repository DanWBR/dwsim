import { useState, useEffect } from 'react';
import {
  Grid,
  Card,
  CardContent,
  Typography,
  Box,
  CircularProgress,
} from '@mui/material';
import {
  PlayArrow as SessionIcon,
  Assignment as ScenarioIcon,
  Assessment as AssessmentIcon,
  CheckCircle as PassIcon,
  Cancel as FailIcon,
} from '@mui/icons-material';
import ApiClient, { SessionInfo, AssessmentReport } from '../services/ApiClient';

function Dashboard() {
  const [sessions, setSessions] = useState<SessionInfo[]>([]);
  const [assessments, setAssessments] = useState<AssessmentReport[]>([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    const fetchData = async () => {
      try {
        const sessionData = await ApiClient.getAllSessions();
        setSessions(sessionData);

        // Fetch assessments for all sessions
        const allAssessments: AssessmentReport[] = [];
        for (const session of sessionData) {
          const sessionAssessments = await ApiClient.getSessionAssessments(session.sessionId);
          allAssessments.push(...sessionAssessments);
        }
        setAssessments(allAssessments);
      } catch (error) {
        console.error('Error fetching dashboard data:', error);
      } finally {
        setLoading(false);
      }
    };

    fetchData();
    const interval = setInterval(fetchData, 5000); // Refresh every 5 seconds

    return () => clearInterval(interval);
  }, []);

  if (loading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="60vh">
        <CircularProgress />
      </Box>
    );
  }

  const activeSessions = sessions.filter((s) => s.status === 'Running').length;
  const totalSessions = sessions.length;
  const recentAssessments = assessments.slice(0, 5);
  const passRate =
    assessments.length > 0
      ? Math.round((assessments.filter((a) => a.Passed).length / assessments.length) * 100)
      : 0;
  const averageScore =
    assessments.length > 0
      ? Math.round(
          assessments.reduce((sum, a) => sum + a.OverallScore, 0) / assessments.length
        )
      : 0;

  return (
    <Box>
      <Typography variant="h4" gutterBottom>
        Instructor Dashboard
      </Typography>
      <Typography variant="subtitle1" color="text.secondary" gutterBottom>
        Overview of training sessions and operator performance
      </Typography>

      <Grid container spacing={3} sx={{ mt: 2 }}>
        {/* Active Sessions Card */}
        <Grid item xs={12} sm={6} md={3}>
          <Card>
            <CardContent>
              <Box display="flex" alignItems="center" justifyContent="space-between">
                <Box>
                  <Typography color="text.secondary" gutterBottom>
                    Active Sessions
                  </Typography>
                  <Typography variant="h4">{activeSessions}</Typography>
                  <Typography variant="body2" color="text.secondary">
                    of {totalSessions} total
                  </Typography>
                </Box>
                <SessionIcon sx={{ fontSize: 60, color: 'primary.main', opacity: 0.3 }} />
              </Box>
            </CardContent>
          </Card>
        </Grid>

        {/* Total Assessments Card */}
        <Grid item xs={12} sm={6} md={3}>
          <Card>
            <CardContent>
              <Box display="flex" alignItems="center" justifyContent="space-between">
                <Box>
                  <Typography color="text.secondary" gutterBottom>
                    Assessments
                  </Typography>
                  <Typography variant="h4">{assessments.length}</Typography>
                  <Typography variant="body2" color="text.secondary">
                    total completed
                  </Typography>
                </Box>
                <AssessmentIcon sx={{ fontSize: 60, color: 'primary.main', opacity: 0.3 }} />
              </Box>
            </CardContent>
          </Card>
        </Grid>

        {/* Pass Rate Card */}
        <Grid item xs={12} sm={6} md={3}>
          <Card>
            <CardContent>
              <Box display="flex" alignItems="center" justifyContent="space-between">
                <Box>
                  <Typography color="text.secondary" gutterBottom>
                    Pass Rate
                  </Typography>
                  <Typography variant="h4">{passRate}%</Typography>
                  <Typography variant="body2" color="text.secondary">
                    trainee success
                  </Typography>
                </Box>
                <PassIcon
                  sx={{
                    fontSize: 60,
                    color: passRate >= 70 ? 'success.main' : 'error.main',
                    opacity: 0.3,
                  }}
                />
              </Box>
            </CardContent>
          </Card>
        </Grid>

        {/* Average Score Card */}
        <Grid item xs={12} sm={6} md={3}>
          <Card>
            <CardContent>
              <Box display="flex" alignItems="center" justifyContent="space-between">
                <Box>
                  <Typography color="text.secondary" gutterBottom>
                    Avg Score
                  </Typography>
                  <Typography variant="h4">{averageScore}</Typography>
                  <Typography variant="body2" color="text.secondary">
                    out of 100
                  </Typography>
                </Box>
                <ScenarioIcon sx={{ fontSize: 60, color: 'primary.main', opacity: 0.3 }} />
              </Box>
            </CardContent>
          </Card>
        </Grid>

        {/* Recent Sessions */}
        <Grid item xs={12} md={6}>
          <Card>
            <CardContent>
              <Typography variant="h6" gutterBottom>
                Recent Sessions
              </Typography>
              {sessions.slice(0, 5).map((session) => (
                <Box
                  key={session.sessionId}
                  sx={{
                    display: 'flex',
                    justifyContent: 'space-between',
                    alignItems: 'center',
                    py: 1,
                    borderBottom: '1px solid rgba(255,255,255,0.1)',
                  }}
                >
                  <Box>
                    <Typography variant="body2" fontWeight="bold">
                      {session.sessionName}
                    </Typography>
                    <Typography variant="caption" color="text.secondary">
                      {new Date(session.createdAt).toLocaleString()}
                    </Typography>
                  </Box>
                  <Box
                    sx={{
                      px: 2,
                      py: 0.5,
                      borderRadius: 1,
                      bgcolor:
                        session.status === 'Running'
                          ? 'success.main'
                          : session.status === 'Stopped'
                          ? 'grey.700'
                          : 'warning.main',
                    }}
                  >
                    <Typography variant="caption">{session.status}</Typography>
                  </Box>
                </Box>
              ))}
              {sessions.length === 0 && (
                <Typography variant="body2" color="text.secondary" sx={{ py: 2 }}>
                  No sessions yet
                </Typography>
              )}
            </CardContent>
          </Card>
        </Grid>

        {/* Recent Assessments */}
        <Grid item xs={12} md={6}>
          <Card>
            <CardContent>
              <Typography variant="h6" gutterBottom>
                Recent Assessments
              </Typography>
              {recentAssessments.map((assessment) => (
                <Box
                  key={assessment.AssessmentId}
                  sx={{
                    display: 'flex',
                    justifyContent: 'space-between',
                    alignItems: 'center',
                    py: 1,
                    borderBottom: '1px solid rgba(255,255,255,0.1)',
                  }}
                >
                  <Box>
                    <Typography variant="body2" fontWeight="bold">
                      Session: {assessment.SessionId.substring(0, 8)}...
                    </Typography>
                    <Typography variant="caption" color="text.secondary">
                      {new Date(assessment.GeneratedAt).toLocaleString()}
                    </Typography>
                  </Box>
                  <Box display="flex" alignItems="center" gap={1}>
                    <Typography variant="h6" color={assessment.Passed ? 'success.main' : 'error.main'}>
                      {assessment.Grade}
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      ({Math.round(assessment.OverallScore)})
                    </Typography>
                    {assessment.Passed ? (
                      <PassIcon color="success" fontSize="small" />
                    ) : (
                      <FailIcon color="error" fontSize="small" />
                    )}
                  </Box>
                </Box>
              ))}
              {assessments.length === 0 && (
                <Typography variant="body2" color="text.secondary" sx={{ py: 2 }}>
                  No assessments yet
                </Typography>
              )}
            </CardContent>
          </Card>
        </Grid>
      </Grid>
    </Box>
  );
}

export default Dashboard;
