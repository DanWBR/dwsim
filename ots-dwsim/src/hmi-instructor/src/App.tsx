import { useState } from 'react';
import { Routes, Route, Link as RouterLink } from 'react-router-dom';
import {
  ThemeProvider,
  createTheme,
  CssBaseline,
  AppBar,
  Toolbar,
  Typography,
  Drawer,
  List,
  ListItem,
  ListItemButton,
  ListItemIcon,
  ListItemText,
  Box,
  Container,
  IconButton,
} from '@mui/material';
import {
  Dashboard as DashboardIcon,
  PlayArrow as SessionIcon,
  Assignment as ScenarioIcon,
  Assessment as AssessmentIcon,
  Replay as ReplayIcon,
  Rule as RuleIcon,
  Menu as MenuIcon,
} from '@mui/icons-material';

import Dashboard from './components/Dashboard';
import SessionList from './components/SessionList';
import ScenarioManager from './components/ScenarioManager';
import RuleSetManager from './components/RuleSetManager';
import AssessmentViewer from './components/AssessmentViewer';
import ReplayManager from './components/ReplayManager';

const darkTheme = createTheme({
  palette: {
    mode: 'dark',
    primary: {
      main: '#4ecdc4',
    },
    secondary: {
      main: '#ff6b6b',
    },
    background: {
      default: '#1a1a2e',
      paper: '#16213e',
    },
  },
  typography: {
    fontFamily: '"Roboto Mono", "Courier New", monospace',
  },
});

const drawerWidth = 240;

const menuItems = [
  { text: 'Dashboard', icon: <DashboardIcon />, path: '/' },
  { text: 'Sessions', icon: <SessionIcon />, path: '/sessions' },
  { text: 'Scenarios', icon: <ScenarioIcon />, path: '/scenarios' },
  { text: 'Rule Sets', icon: <RuleIcon />, path: '/rulesets' },
  { text: 'Assessments', icon: <AssessmentIcon />, path: '/assessments' },
  { text: 'Replays', icon: <ReplayIcon />, path: '/replays' },
];

function App() {
  const [mobileOpen, setMobileOpen] = useState(false);

  const handleDrawerToggle = () => {
    setMobileOpen(!mobileOpen);
  };

  const drawer = (
    <Box sx={{ overflow: 'auto' }}>
      <Toolbar>
        <Typography variant="h6" noWrap component="div">
          OTS Instructor
        </Typography>
      </Toolbar>
      <List>
        {menuItems.map((item) => (
          <ListItem key={item.text} disablePadding>
            <ListItemButton component={RouterLink} to={item.path}>
              <ListItemIcon sx={{ color: 'primary.main' }}>{item.icon}</ListItemIcon>
              <ListItemText primary={item.text} />
            </ListItemButton>
          </ListItem>
        ))}
      </List>
    </Box>
  );

  return (
    <ThemeProvider theme={darkTheme}>
      <CssBaseline />
      <Box sx={{ display: 'flex' }}>
        <AppBar
          position="fixed"
          sx={{
            width: { sm: `calc(100% - ${drawerWidth}px)` },
            ml: { sm: `${drawerWidth}px` },
          }}
        >
          <Toolbar>
            <IconButton
              color="inherit"
              aria-label="open drawer"
              edge="start"
              onClick={handleDrawerToggle}
              sx={{ mr: 2, display: { sm: 'none' } }}
            >
              <MenuIcon />
            </IconButton>
            <Typography variant="h6" noWrap component="div">
              DWSIM Operator Training System - Instructor Console
            </Typography>
          </Toolbar>
        </AppBar>
        <Box
          component="nav"
          sx={{ width: { sm: drawerWidth }, flexShrink: { sm: 0 } }}
        >
          <Drawer
            variant="temporary"
            open={mobileOpen}
            onClose={handleDrawerToggle}
            ModalProps={{
              keepMounted: true,
            }}
            sx={{
              display: { xs: 'block', sm: 'none' },
              '& .MuiDrawer-paper': { boxSizing: 'border-box', width: drawerWidth },
            }}
          >
            {drawer}
          </Drawer>
          <Drawer
            variant="permanent"
            sx={{
              display: { xs: 'none', sm: 'block' },
              '& .MuiDrawer-paper': { boxSizing: 'border-box', width: drawerWidth },
            }}
            open
          >
            {drawer}
          </Drawer>
        </Box>
        <Box
          component="main"
          sx={{
            flexGrow: 1,
            p: 3,
            width: { sm: `calc(100% - ${drawerWidth}px)` },
          }}
        >
          <Toolbar />
          <Container maxWidth="xl">
            <Routes>
              <Route path="/" element={<Dashboard />} />
              <Route path="/sessions" element={<SessionList />} />
              <Route path="/scenarios" element={<ScenarioManager />} />
              <Route path="/rulesets" element={<RuleSetManager />} />
              <Route path="/assessments" element={<AssessmentViewer />} />
              <Route path="/replays" element={<ReplayManager />} />
            </Routes>
          </Container>
        </Box>
      </Box>
    </ThemeProvider>
  );
}

export default App;
