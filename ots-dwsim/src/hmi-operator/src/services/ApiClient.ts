import axios, { AxiosInstance } from 'axios';
import {
  SessionInfo,
  TagValue,
  WriteTagRequest,
  EventLogEntry,
  CreateSessionRequest,
  CreateSessionResponse,
  HealthResponse,
  RunScenarioRequest,
  RunScenarioResponse
} from '../types';

class ApiClient {
  private client: AxiosInstance;
  private baseURL: string;

  constructor() {
    // Use environment variable or default to localhost
    this.baseURL = process.env.REACT_APP_API_URL || 'http://localhost:5000';

    this.client = axios.create({
      baseURL: this.baseURL + '/api/v1',
      timeout: 30000,
      headers: {
        'Content-Type': 'application/json'
      }
    });

    // Add request interceptor for logging
    this.client.interceptors.request.use(
      (config) => {
        console.log(`API Request: ${config.method?.toUpperCase()} ${config.url}`);
        return config;
      },
      (error) => Promise.reject(error)
    );

    // Add response interceptor for error handling
    this.client.interceptors.response.use(
      (response) => response,
      (error) => {
        console.error('API Error:', error.response?.data || error.message);
        return Promise.reject(error);
      }
    );
  }

  // Health check
  async getHealth(): Promise<HealthResponse> {
    const response = await axios.get(`${this.baseURL}/health`);
    return response.data;
  }

  // Sessions
  async getSessions(): Promise<SessionInfo[]> {
    const response = await this.client.get('/sessions');
    return response.data;
  }

  async getSession(sessionId: string): Promise<SessionInfo> {
    const response = await this.client.get(`/sessions/${sessionId}`);
    return response.data;
  }

  async createSession(request: CreateSessionRequest): Promise<CreateSessionResponse> {
    const response = await this.client.post('/sessions', request);
    return response.data;
  }

  async startSession(sessionId: string): Promise<SessionInfo> {
    const response = await this.client.post(`/sessions/${sessionId}/start`, {
      start_mode: 'run',
      time_step_seconds: 0.5
    });
    return response.data;
  }

  async pauseSession(sessionId: string): Promise<SessionInfo> {
    const response = await this.client.post(`/sessions/${sessionId}/pause`);
    return response.data;
  }

  async stopSession(sessionId: string): Promise<SessionInfo> {
    const response = await this.client.post(`/sessions/${sessionId}/stop`);
    return response.data;
  }

  // Tags
  async readTag(sessionId: string, tagPath: string): Promise<TagValue> {
    const response = await this.client.get(`/sessions/${sessionId}/tags/${tagPath}`);
    return response.data;
  }

  async writeTag(sessionId: string, tagPath: string, request: WriteTagRequest): Promise<void> {
    await this.client.post(`/sessions/${sessionId}/tags/${tagPath}`, request);
  }

  // Events
  async getEvents(sessionId: string, from?: Date, to?: Date): Promise<EventLogEntry[]> {
    const params: any = {};
    if (from) params.from = from.toISOString();
    if (to) params.to = to.toISOString();

    const response = await this.client.get(`/sessions/${sessionId}/events`, { params });
    return response.data;
  }

  // Scenarios
  async runScenario(sessionId: string, request: RunScenarioRequest): Promise<RunScenarioResponse> {
    const response = await this.client.post(`/sessions/${sessionId}/scenario/run`, request);
    return response.data;
  }

  // Time control
  async setTimeFactor(sessionId: string, factor: number): Promise<void> {
    await this.client.post(`/sessions/${sessionId}/time/set_factor`, { factor });
  }

  async stepTime(sessionId: string, seconds: number): Promise<void> {
    await this.client.post(`/sessions/${sessionId}/time/step`, { seconds });
  }
}

// Export singleton instance
export const apiClient = new ApiClient();
export default apiClient;
