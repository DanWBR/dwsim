import axios, { AxiosInstance } from 'axios';

// Type definitions
export interface SessionInfo {
  sessionId: string;
  sessionName: string;
  flowsheetId: string;
  status: 'Created' | 'Running' | 'Paused' | 'Stopped' | 'Error';
  createdAt: string;
  startedAt?: string;
  stoppedAt?: string;
  simTime?: string;
  description?: string;
}

export interface Scenario {
  scenario_id: string;
  description: string;
  seed: number;
  duration_s: number;
  initial_state?: Record<string, any>;
  events: ScenarioEvent[];
  pass_criteria?: Record<string, any>;
}

export interface ScenarioEvent {
  time_s: number;
  type: string;
  target?: string;
  payload?: any;
}

export interface RuleSet {
  RuleSetId: string;
  Name: string;
  Description: string;
  ScenarioId?: string;
  PassingScore: number;
  Rules: KpiRule[];
  CreatedAt: string;
  UpdatedAt?: string;
}

export interface KpiRule {
  RuleId: string;
  Name: string;
  Description: string;
  Type: string;
  Weight: number;
  Thresholds: {
    Target?: number;
    Min?: number;
    Max?: number;
    Excellent?: number;
    Good?: number;
    Acceptable?: number;
    Poor?: number;
  };
  TagPaths: string[];
  IsRequired: boolean;
}

export interface AssessmentReport {
  AssessmentId: string;
  SessionId: string;
  GeneratedAt: string;
  OverallScore: number;
  Passed: boolean;
  PassingScore: number;
  Grade: string;
  KpiResults: KpiResult[];
  Summary: AssessmentSummary;
  Recommendations: string[];
  PdfReportPath?: string;
}

export interface KpiResult {
  RuleId: string;
  Name: string;
  Type: string;
  ActualValue: number;
  TargetValue?: number;
  Score: number;
  Passed: boolean;
  Level: string;
  Weight: number;
  Details?: string;
  Units?: string;
}

export interface AssessmentSummary {
  TotalKpis: number;
  KpisPassed: number;
  KpisFailed: number;
  SessionDuration: string;
  TotalOperatorActions: number;
  TotalAlarms: number;
  LevelBreakdown: Record<string, number>;
}

export interface ReplayResponse {
  ReplayId: string;
  ReplaySessionId: string;
  OriginalSessionId: string;
  Status: string;
  CreatedAt: string;
  TotalEvents: number;
  ProcessedEvents: number;
  ProgressPercent: number;
  ErrorMessage?: string;
}

export interface ReplayComparison {
  ReplayId: string;
  OriginalSessionId: string;
  ReplaySessionId: string;
  DeterminismScore: number;
  IsDeterministic: boolean;
  MaxAllowedDeviation: number;
  TagComparisons: TagComparison[];
  Stats: ComparisonStats;
  GeneratedAt: string;
}

export interface TagComparison {
  TagPath: string;
  DataPointsCompared: number;
  MeanAbsoluteError: number;
  RootMeanSquareError: number;
  MaxDeviation: number;
  MaxDeviationPercent: number;
  Correlation: number;
  PassesDeterminismCheck: boolean;
}

export interface ComparisonStats {
  TotalTagsCompared: number;
  TagsPassing: number;
  TagsFailing: number;
  TotalDataPointsCompared: number;
  AverageCorrelation: number;
  AverageRMSE: number;
}

export interface Snapshot {
  SnapshotId: string;
  SessionId: string;
  Name: string;
  Description?: string;
  CreatedAt: string;
  FilePath: string;
  SizeBytes: number;
}

class ApiClient {
  private client: AxiosInstance;

  constructor(baseURL: string = '/api/v1') {
    this.client = axios.create({
      baseURL,
      headers: {
        'Content-Type': 'application/json',
      },
    });
  }

  // Session Management
  async getAllSessions(): Promise<SessionInfo[]> {
    const response = await this.client.get('/sessions');
    return response.data;
  }

  async getSession(sessionId: string): Promise<SessionInfo> {
    const response = await this.client.get(`/sessions/${sessionId}`);
    return response.data;
  }

  async createSession(flowsheetId: string, name: string, description?: string): Promise<SessionInfo> {
    const response = await this.client.post('/sessions', {
      FlowsheetId: flowsheetId,
      SessionName: name,
      Description: description,
    });
    return response.data;
  }

  async deleteSession(sessionId: string): Promise<void> {
    await this.client.delete(`/sessions/${sessionId}`);
  }

  async startSession(sessionId: string): Promise<void> {
    await this.client.post(`/sessions/${sessionId}/start`);
  }

  async stopSession(sessionId: string): Promise<void> {
    await this.client.post(`/sessions/${sessionId}/stop`);
  }

  // Scenario Management
  async validateScenario(scenario: Scenario): Promise<{ valid: boolean; errors?: string[] }> {
    const response = await this.client.post('/scenarios/validate', scenario);
    return response.data;
  }

  async loadScenario(sessionId: string, scenarioId: string): Promise<void> {
    await this.client.post(`/sessions/${sessionId}/scenario`, { ScenarioId: scenarioId });
  }

  async startScenario(sessionId: string): Promise<void> {
    await this.client.post(`/sessions/${sessionId}/scenario/start`);
  }

  // Snapshot Management
  async listSnapshots(sessionId: string): Promise<Snapshot[]> {
    const response = await this.client.get(`/sessions/${sessionId}/snapshots`);
    return response.data;
  }

  async createSnapshot(sessionId: string, name: string, description?: string): Promise<Snapshot> {
    const response = await this.client.post(`/sessions/${sessionId}/snapshot`, {
      Name: name,
      Description: description,
    });
    return response.data;
  }

  // Assessment Management
  async assessSession(
    sessionId: string,
    ruleSetId?: string,
    customRules?: KpiRule[]
  ): Promise<AssessmentReport> {
    const response = await this.client.post('/assessment', {
      SessionId: sessionId,
      RuleSetId: ruleSetId,
      CustomRules: customRules,
      IncludeDetails: true,
      GeneratePdf: false,
    });
    return response.data;
  }

  async getAssessment(assessmentId: string): Promise<AssessmentReport> {
    const response = await this.client.get(`/assessment/${assessmentId}`);
    return response.data;
  }

  async getSessionAssessments(sessionId: string): Promise<AssessmentReport[]> {
    const response = await this.client.get(`/assessment/session/${sessionId}`);
    return response.data;
  }

  // Rule Set Management
  async getAllRuleSets(): Promise<RuleSet[]> {
    const response = await this.client.get('/assessment/rulesets');
    return response.data;
  }

  async getRuleSet(ruleSetId: string): Promise<RuleSet> {
    const response = await this.client.get(`/assessment/rulesets/${ruleSetId}`);
    return response.data;
  }

  async createRuleSet(ruleSet: Omit<RuleSet, 'RuleSetId' | 'CreatedAt'>): Promise<RuleSet> {
    const response = await this.client.post('/assessment/rulesets', ruleSet);
    return response.data;
  }

  async updateRuleSet(ruleSet: RuleSet): Promise<RuleSet> {
    const response = await this.client.put(`/assessment/rulesets/${ruleSet.RuleSetId}`, ruleSet);
    return response.data;
  }

  async deleteRuleSet(ruleSetId: string): Promise<void> {
    await this.client.delete(`/assessment/rulesets/${ruleSetId}`);
  }

  // Replay Management
  async startReplay(
    originalSessionId: string,
    speedMultiplier: number = 1.0,
    verifyDeterminism: boolean = true
  ): Promise<ReplayResponse> {
    const response = await this.client.post('/replay', {
      OriginalSessionId: originalSessionId,
      SpeedMultiplier: speedMultiplier,
      VerifyDeterminism: verifyDeterminism,
    });
    return response.data;
  }

  async getReplayStatus(replayId: string): Promise<ReplayResponse> {
    const response = await this.client.get(`/replay/${replayId}`);
    return response.data;
  }

  async getReplayComparison(replayId: string): Promise<ReplayComparison> {
    const response = await this.client.get(`/replay/${replayId}/comparison`);
    return response.data;
  }

  async cancelReplay(replayId: string): Promise<void> {
    await this.client.post(`/replay/${replayId}/cancel`);
  }

  async getReplaysForSession(sessionId: string): Promise<ReplayResponse[]> {
    const response = await this.client.get(`/replay/session/${sessionId}`);
    return response.data;
  }
}

export default new ApiClient();
