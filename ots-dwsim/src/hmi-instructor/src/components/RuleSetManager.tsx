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
  Button,
  IconButton,
  Chip,
  CircularProgress,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  TextField,
} from '@mui/material';
import { Edit, Delete, Add, Visibility } from '@mui/icons-material';
import ApiClient, { RuleSet } from '../services/ApiClient';

function RuleSetManager() {
  const [ruleSets, setRuleSets] = useState<RuleSet[]>([]);
  const [loading, setLoading] = useState(true);
  const [viewDialogOpen, setViewDialogOpen] = useState(false);
  const [selectedRuleSet, setSelectedRuleSet] = useState<RuleSet | null>(null);

  const fetchRuleSets = async () => {
    try {
      const data = await ApiClient.getAllRuleSets();
      setRuleSets(data);
    } catch (error) {
      console.error('Error fetching rule sets:', error);
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    fetchRuleSets();
  }, []);

  const handleView = (ruleSet: RuleSet) => {
    setSelectedRuleSet(ruleSet);
    setViewDialogOpen(true);
  };

  const handleDelete = async (ruleSetId: string) => {
    if (confirm('Are you sure you want to delete this rule set?')) {
      try {
        await ApiClient.deleteRuleSet(ruleSetId);
        fetchRuleSets();
      } catch (error) {
        console.error('Error deleting rule set:', error);
      }
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
      <Box display="flex" justifyContent="space-between" alignItems="center" mb={3}>
        <Box>
          <Typography variant="h4" gutterBottom>
            Assessment Rule Sets
          </Typography>
          <Typography variant="subtitle1" color="text.secondary">
            Manage KPI rules for evaluating operator performance
          </Typography>
        </Box>
        <Button variant="contained" startIcon={<Add />}>
          Create Rule Set
        </Button>
      </Box>

      <TableContainer component={Paper}>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>Name</TableCell>
              <TableCell>Description</TableCell>
              <TableCell>Scenario</TableCell>
              <TableCell>Rules</TableCell>
              <TableCell>Passing Score</TableCell>
              <TableCell>Created</TableCell>
              <TableCell>Actions</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {ruleSets.map((ruleSet) => (
              <TableRow key={ruleSet.RuleSetId}>
                <TableCell>
                  <Typography variant="body2" fontWeight="bold">
                    {ruleSet.Name}
                  </Typography>
                </TableCell>
                <TableCell>
                  <Typography variant="body2" color="text.secondary">
                    {ruleSet.Description}
                  </Typography>
                </TableCell>
                <TableCell>
                  {ruleSet.ScenarioId ? (
                    <Chip label={ruleSet.ScenarioId} size="small" />
                  ) : (
                    <Typography variant="caption" color="text.secondary">
                      Any
                    </Typography>
                  )}
                </TableCell>
                <TableCell>{ruleSet.Rules.length}</TableCell>
                <TableCell>
                  <Chip label={`${ruleSet.PassingScore}%`} color="primary" size="small" />
                </TableCell>
                <TableCell>
                  <Typography variant="caption">
                    {new Date(ruleSet.CreatedAt).toLocaleDateString()}
                  </Typography>
                </TableCell>
                <TableCell>
                  <Box display="flex" gap={0.5}>
                    <IconButton
                      size="small"
                      color="info"
                      onClick={() => handleView(ruleSet)}
                      title="View Details"
                    >
                      <Visibility />
                    </IconButton>
                    <IconButton size="small" color="primary" title="Edit">
                      <Edit />
                    </IconButton>
                    <IconButton
                      size="small"
                      color="error"
                      onClick={() => handleDelete(ruleSet.RuleSetId)}
                      title="Delete"
                    >
                      <Delete />
                    </IconButton>
                  </Box>
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
        {ruleSets.length === 0 && (
          <Box p={4} textAlign="center">
            <Typography color="text.secondary">No rule sets found</Typography>
          </Box>
        )}
      </TableContainer>

      {/* View Rule Set Dialog */}
      <Dialog
        open={viewDialogOpen}
        onClose={() => setViewDialogOpen(false)}
        maxWidth="md"
        fullWidth
      >
        <DialogTitle>{selectedRuleSet?.Name}</DialogTitle>
        <DialogContent>
          {selectedRuleSet && (
            <Box>
              <Typography variant="body2" color="text.secondary" paragraph>
                {selectedRuleSet.Description}
              </Typography>

              <Typography variant="h6" gutterBottom sx={{ mt: 2 }}>
                KPI Rules ({selectedRuleSet.Rules.length})
              </Typography>

              {selectedRuleSet.Rules.map((rule, index) => (
                <Paper key={index} sx={{ p: 2, mb: 2, bgcolor: 'background.default' }}>
                  <Box display="flex" justifyContent="space-between" alignItems="start">
                    <Box>
                      <Typography variant="body1" fontWeight="bold">
                        {rule.Name}
                      </Typography>
                      <Typography variant="body2" color="text.secondary">
                        {rule.Description}
                      </Typography>
                      <Box display="flex" gap={1} mt={1}>
                        <Chip label={rule.Type} size="small" color="primary" />
                        <Chip label={`Weight: ${rule.Weight}`} size="small" variant="outlined" />
                        {rule.IsRequired && (
                          <Chip label="Required" size="small" color="error" />
                        )}
                      </Box>
                    </Box>
                  </Box>

                  {rule.Thresholds && (
                    <Box mt={2}>
                      <Typography variant="caption" color="text.secondary">
                        Thresholds:
                      </Typography>
                      <Box display="flex" gap={1} mt={0.5} flexWrap="wrap">
                        {rule.Thresholds.Excellent && (
                          <Chip
                            label={`Excellent: ${rule.Thresholds.Excellent}`}
                            size="small"
                            variant="outlined"
                          />
                        )}
                        {rule.Thresholds.Good && (
                          <Chip
                            label={`Good: ${rule.Thresholds.Good}`}
                            size="small"
                            variant="outlined"
                          />
                        )}
                        {rule.Thresholds.Acceptable && (
                          <Chip
                            label={`Acceptable: ${rule.Thresholds.Acceptable}`}
                            size="small"
                            variant="outlined"
                          />
                        )}
                        {rule.Thresholds.Target && (
                          <Chip
                            label={`Target: ${rule.Thresholds.Target}`}
                            size="small"
                            color="secondary"
                          />
                        )}
                      </Box>
                    </Box>
                  )}

                  {rule.TagPaths.length > 0 && (
                    <Box mt={1}>
                      <Typography variant="caption" color="text.secondary">
                        Tags: {rule.TagPaths.join(', ')}
                      </Typography>
                    </Box>
                  )}
                </Paper>
              ))}
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

export default RuleSetManager;
