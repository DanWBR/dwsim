-- DWSIM OTS Database Initialization
-- This script sets up PostgreSQL and TimescaleDB for the Operator Training System

-- ============================================================================
-- SESSION METADATA (PostgreSQL)
-- ============================================================================

CREATE TABLE IF NOT EXISTS sessions (
    session_id UUID PRIMARY KEY,
    session_name VARCHAR(255) NOT NULL,
    flowsheet_path TEXT NOT NULL,
    status VARCHAR(50) NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    started_at TIMESTAMPTZ,
    stopped_at TIMESTAMPTZ,
    seed INTEGER,
    time_factor DOUBLE PRECISION DEFAULT 1.0,
    metadata JSONB
);

CREATE INDEX idx_sessions_status ON sessions(status);
CREATE INDEX idx_sessions_created_at ON sessions(created_at);

-- ============================================================================
-- SNAPSHOTS
-- ============================================================================

CREATE TABLE IF NOT EXISTS snapshots (
    snapshot_id VARCHAR(100) PRIMARY KEY,
    session_id UUID NOT NULL REFERENCES sessions(session_id) ON DELETE CASCADE,
    name VARCHAR(255) NOT NULL,
    file_path TEXT NOT NULL,
    size_bytes BIGINT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    sim_time TIMESTAMPTZ NOT NULL,
    metadata JSONB
);

CREATE INDEX idx_snapshots_session_id ON snapshots(session_id);
CREATE INDEX idx_snapshots_created_at ON snapshots(created_at);

-- ============================================================================
-- SCENARIOS
-- ============================================================================

CREATE TABLE IF NOT EXISTS scenarios (
    scenario_id VARCHAR(255) PRIMARY KEY,
    title VARCHAR(500),
    description TEXT,
    author VARCHAR(255),
    seed INTEGER NOT NULL,
    scenario_data JSONB NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS scenario_runs (
    run_id VARCHAR(100) PRIMARY KEY,
    session_id UUID NOT NULL REFERENCES sessions(session_id) ON DELETE CASCADE,
    scenario_id VARCHAR(255) NOT NULL REFERENCES scenarios(scenario_id),
    status VARCHAR(50) NOT NULL,
    started_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    completed_at TIMESTAMPTZ,
    current_event_index INTEGER DEFAULT 0,
    errors TEXT[],
    metadata JSONB
);

CREATE INDEX idx_scenario_runs_session_id ON scenario_runs(session_id);
CREATE INDEX idx_scenario_runs_status ON scenario_runs(status);

-- ============================================================================
-- EVENT LOG
-- ============================================================================

CREATE TABLE IF NOT EXISTS event_log (
    id BIGSERIAL PRIMARY KEY,
    session_id UUID NOT NULL REFERENCES sessions(session_id) ON DELETE CASCADE,
    event_type VARCHAR(100) NOT NULL,
    user_name VARCHAR(255),
    action VARCHAR(255),
    target VARCHAR(500),
    value JSONB,
    sim_time TIMESTAMPTZ NOT NULL,
    real_time TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_event_log_session_id ON event_log(session_id);
CREATE INDEX idx_event_log_sim_time ON event_log(sim_time);
CREATE INDEX idx_event_log_event_type ON event_log(event_type);

-- ============================================================================
-- TIME-SERIES DATA (TimescaleDB)
-- ============================================================================

-- Process variables time-series
CREATE TABLE IF NOT EXISTS process_variables (
    time TIMESTAMPTZ NOT NULL,
    session_id UUID NOT NULL,
    tag_path VARCHAR(500) NOT NULL,
    value DOUBLE PRECISION,
    units VARCHAR(50),
    sim_time TIMESTAMPTZ NOT NULL,
    quality VARCHAR(20) DEFAULT 'good'
);

-- Convert to hypertable (TimescaleDB)
SELECT create_hypertable('process_variables', 'time', if_not_exists => TRUE);

-- Create indexes
CREATE INDEX IF NOT EXISTS idx_pv_session_tag_time
    ON process_variables (session_id, tag_path, time DESC);

CREATE INDEX IF NOT EXISTS idx_pv_sim_time
    ON process_variables (sim_time);

-- Set retention policy (keep data for 90 days)
SELECT add_retention_policy('process_variables', INTERVAL '90 days', if_not_exists => TRUE);

-- Enable compression (compress chunks older than 7 days)
ALTER TABLE process_variables SET (
    timescaledb.compress,
    timescaledb.compress_segmentby = 'session_id,tag_path',
    timescaledb.compress_orderby = 'time DESC'
);

SELECT add_compression_policy('process_variables', INTERVAL '7 days', if_not_exists => TRUE);

-- ============================================================================
-- CONTINUOUS AGGREGATES (Pre-computed aggregations)
-- ============================================================================

-- 1-minute aggregates
CREATE MATERIALIZED VIEW IF NOT EXISTS process_variables_1min
WITH (timescaledb.continuous) AS
SELECT
    time_bucket('1 minute', time) AS bucket,
    session_id,
    tag_path,
    AVG(value) as avg_value,
    MIN(value) as min_value,
    MAX(value) as max_value,
    STDDEV(value) as stddev_value,
    COUNT(*) as sample_count
FROM process_variables
GROUP BY bucket, session_id, tag_path
WITH NO DATA;

SELECT add_continuous_aggregate_policy('process_variables_1min',
    start_offset => INTERVAL '1 hour',
    end_offset => INTERVAL '1 minute',
    schedule_interval => INTERVAL '1 minute',
    if_not_exists => TRUE);

-- 1-hour aggregates
CREATE MATERIALIZED VIEW IF NOT EXISTS process_variables_1hour
WITH (timescaledb.continuous) AS
SELECT
    time_bucket('1 hour', time) AS bucket,
    session_id,
    tag_path,
    AVG(value) as avg_value,
    MIN(value) as min_value,
    MAX(value) as max_value,
    STDDEV(value) as stddev_value,
    COUNT(*) as sample_count
FROM process_variables
GROUP BY bucket, session_id, tag_path
WITH NO DATA;

SELECT add_continuous_aggregate_policy('process_variables_1hour',
    start_offset => INTERVAL '1 day',
    end_offset => INTERVAL '1 hour',
    schedule_interval => INTERVAL '1 hour',
    if_not_exists => TRUE);

-- ============================================================================
-- ALARMS
-- ============================================================================

CREATE TABLE IF NOT EXISTS alarms (
    time TIMESTAMPTZ NOT NULL,
    session_id UUID NOT NULL,
    alarm_id VARCHAR(100) NOT NULL,
    tag_path VARCHAR(500) NOT NULL,
    severity VARCHAR(20) NOT NULL,
    message TEXT NOT NULL,
    state VARCHAR(20) NOT NULL, -- active, acknowledged, cleared
    raised_at TIMESTAMPTZ,
    acknowledged_at TIMESTAMPTZ,
    cleared_at TIMESTAMPTZ,
    acknowledged_by VARCHAR(255)
);

SELECT create_hypertable('alarms', 'time', if_not_exists => TRUE);

CREATE INDEX IF NOT EXISTS idx_alarms_session_id ON alarms(session_id, time DESC);
CREATE INDEX IF NOT EXISTS idx_alarms_state ON alarms(state);

-- ============================================================================
-- FUNCTIONS AND TRIGGERS
-- ============================================================================

-- Function to update updated_at timestamp
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language 'plpgsql';

-- Trigger for scenarios table
CREATE TRIGGER update_scenarios_updated_at BEFORE UPDATE ON scenarios
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- ============================================================================
-- HELPER VIEWS
-- ============================================================================

-- View for active sessions
CREATE OR REPLACE VIEW active_sessions AS
SELECT
    s.*,
    COUNT(DISTINCT pv.tag_path) as tag_count,
    MAX(pv.time) as last_update
FROM sessions s
LEFT JOIN process_variables pv ON s.session_id = pv.session_id
WHERE s.status IN ('running', 'paused')
GROUP BY s.session_id;

-- View for session statistics
CREATE OR REPLACE VIEW session_stats AS
SELECT
    s.session_id,
    s.session_name,
    s.status,
    COUNT(DISTINCT pv.tag_path) as total_tags,
    COUNT(pv.*) as total_samples,
    MIN(pv.time) as first_sample,
    MAX(pv.time) as last_sample,
    COUNT(DISTINCT el.id) as event_count,
    COUNT(DISTINCT sn.snapshot_id) as snapshot_count
FROM sessions s
LEFT JOIN process_variables pv ON s.session_id = pv.session_id
LEFT JOIN event_log el ON s.session_id = el.session_id
LEFT JOIN snapshots sn ON s.session_id = sn.session_id
GROUP BY s.session_id, s.session_name, s.status;

-- ============================================================================
-- SAMPLE DATA (Optional - for testing)
-- ============================================================================

-- Uncomment to insert sample scenario
/*
INSERT INTO scenarios (scenario_id, title, description, author, seed, scenario_data)
VALUES (
    'sample_distill',
    'Distillation Column Startup',
    'Basic distillation column startup procedure',
    'OTS Template',
    12345,
    '{"events": []}'::jsonb
) ON CONFLICT DO NOTHING;
*/

-- ============================================================================
-- GRANTS (Adjust as needed for your security model)
-- ============================================================================

-- Grant permissions to ots_user
GRANT ALL ON ALL TABLES IN SCHEMA public TO ots_user;
GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO ots_user;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA public TO ots_user;

-- ============================================================================
-- COMPLETION MESSAGE
-- ============================================================================

DO $$
BEGIN
    RAISE NOTICE 'DWSIM OTS database initialization complete!';
    RAISE NOTICE 'TimescaleDB hypertables created: process_variables, alarms';
    RAISE NOTICE 'Continuous aggregates: process_variables_1min, process_variables_1hour';
    RAISE NOTICE 'Retention policy: 90 days';
    RAISE NOTICE 'Compression policy: 7 days';
END $$;
