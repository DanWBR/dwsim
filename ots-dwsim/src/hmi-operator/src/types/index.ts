// TypeScript types matching the backend API models

export enum SessionStatus {
  Created = 'created',
  Running = 'running',
  Paused = 'paused',
  Stopped = 'stopped',
  Error = 'error'
}

export interface SessionInfo {
  session_id: string;
  session_name: string;
  status: SessionStatus;
  created_at: string;
  started_at?: string;
  stopped_at?: string;
  sim_time: string;
  time_factor: number;
  flowsheet_path: string;
}

export interface TagValue {
  tag: string;
  value: number | string | boolean | object | null;
  units?: string;
  sim_time: string;
}

export interface WriteTagRequest {
  value: number | string | boolean | object;
  user?: string;
  mode?: string;
}

export interface EventLogEntry {
  type: string;
  user?: string;
  action?: string;
  target?: string;
  value?: any;
  sim_time: string;
  real_time: string;
}

export interface CreateSessionRequest {
  flowsheet: string;
  session_name?: string;
  seed?: number;
  environment?: Record<string, any>;
  max_real_time_sec?: number;
}

export interface CreateSessionResponse {
  session_id: string;
  status: SessionStatus;
  created_at: string;
}

export interface HealthResponse {
  status: string;
  timestamp: string;
  version: string;
  uptime?: number;
  sessions_active?: number;
}

export interface RunScenarioRequest {
  scenario_id: string;
  autostart?: boolean;
}

export interface RunScenarioResponse {
  status: string;
  scenario_run_id: string;
}

// Process variable for display
export interface ProcessVariable {
  tagPath: string;
  displayName: string;
  value: number | string;
  units?: string;
  timestamp: string;
  color?: string;
}

// Alarm definition
export interface Alarm {
  id: string;
  tag: string;
  severity: 'low' | 'medium' | 'high' | 'critical';
  message: string;
  active: boolean;
  raised_at?: string;
  cleared_at?: string;
}
