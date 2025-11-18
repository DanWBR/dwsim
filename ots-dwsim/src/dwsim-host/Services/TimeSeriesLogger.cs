using DWSIM.OTS.SimulationHost.Models;
using Npgsql;
using System.Text;

namespace DWSIM.OTS.SimulationHost.Services;

public class TimeSeriesLogger : ITimeSeriesLogger, IDisposable
{
    private readonly ILogger<TimeSeriesLogger> _logger;
    private readonly string _connectionString;
    private NpgsqlConnection? _connection;
    private readonly SemaphoreSlim _connectionLock = new(1, 1);

    public TimeSeriesLogger(ILogger<TimeSeriesLogger> logger, IConfiguration configuration)
    {
        _logger = logger;

        // Get connection string from configuration
        _connectionString = configuration.GetConnectionString("TimescaleDB")
            ?? "Host=localhost;Port=5433;Database=ots_timeseries;Username=ots_user;Password=ots_password";

        _ = InitializeConnectionAsync();
    }

    private async Task InitializeConnectionAsync()
    {
        try
        {
            await _connectionLock.WaitAsync();

            _connection = new NpgsqlConnection(_connectionString);
            await _connection.OpenAsync();

            _logger.LogInformation("Connected to TimescaleDB: {Database}",
                _connection.Database);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Failed to connect to TimescaleDB");
            _connection = null;
        }
        finally
        {
            _connectionLock.Release();
        }
    }

    public async Task<bool> IsConnectedAsync()
    {
        if (_connection == null)
        {
            return false;
        }

        try
        {
            return _connection.State == System.Data.ConnectionState.Open;
        }
        catch
        {
            return false;
        }
    }

    public async Task LogAsync(ProcessVariableDataPoint dataPoint)
    {
        await LogBatchAsync(new[] { dataPoint });
    }

    public async Task LogBatchAsync(IEnumerable<ProcessVariableDataPoint> dataPoints)
    {
        if (_connection == null || _connection.State != System.Data.ConnectionState.Open)
        {
            _logger.LogWarning("Cannot log data: TimescaleDB connection not available");
            return;
        }

        try
        {
            var points = dataPoints.ToList();
            if (points.Count == 0) return;

            await using var transaction = await _connection.BeginTransactionAsync();

            try
            {
                const string sql = @"
                    INSERT INTO process_variables (time, session_id, tag_path, value, units, sim_time, quality)
                    VALUES (@time, @session_id, @tag_path, @value, @units, @sim_time, @quality)";

                await using var cmd = new NpgsqlCommand(sql, _connection, transaction);

                foreach (var point in points)
                {
                    cmd.Parameters.Clear();
                    cmd.Parameters.AddWithValue("time", point.Time);
                    cmd.Parameters.AddWithValue("session_id", Guid.Parse(point.SessionId));
                    cmd.Parameters.AddWithValue("tag_path", point.TagPath);
                    cmd.Parameters.AddWithValue("value", point.Value.HasValue ? (object)point.Value.Value : DBNull.Value);
                    cmd.Parameters.AddWithValue("units", point.Units ?? (object)DBNull.Value);
                    cmd.Parameters.AddWithValue("sim_time", point.SimTime);
                    cmd.Parameters.AddWithValue("quality", point.Quality);

                    await cmd.ExecuteNonQueryAsync();
                }

                await transaction.CommitAsync();

                _logger.LogDebug("Logged {Count} data points to TimescaleDB", points.Count);
            }
            catch (Exception ex)
            {
                await transaction.RollbackAsync();
                _logger.LogError(ex, "Error logging batch to TimescaleDB");
                throw;
            }
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error in LogBatchAsync");
        }
    }

    public async Task<List<TimeSeriesQueryResult>> QueryAsync(TimeSeriesQueryRequest request)
    {
        if (_connection == null || _connection.State != System.Data.ConnectionState.Open)
        {
            _logger.LogWarning("Cannot query data: TimescaleDB connection not available");
            return new List<TimeSeriesQueryResult>();
        }

        try
        {
            var results = new List<TimeSeriesQueryResult>();

            foreach (var tagPath in request.TagPaths)
            {
                var points = await QueryTagAsync(
                    request.SessionId,
                    tagPath,
                    request.StartTime,
                    request.EndTime,
                    request.Aggregation,
                    request.Limit);

                results.Add(new TimeSeriesQueryResult
                {
                    TagPath = tagPath,
                    Points = points,
                    Units = points.FirstOrDefault()?.SimTime != null ? null : null // TODO: Get units from first point
                });
            }

            return results;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error querying time-series data");
            return new List<TimeSeriesQueryResult>();
        }
    }

    private async Task<List<TimeSeriesPoint>> QueryTagAsync(
        string sessionId,
        string tagPath,
        DateTime? startTime,
        DateTime? endTime,
        string aggregation,
        int? limit)
    {
        var points = new List<TimeSeriesPoint>();

        try
        {
            string sql;
            if (aggregation == "1min")
            {
                sql = @"
                    SELECT bucket as time, avg_value as value
                    FROM process_variables_1min
                    WHERE session_id = @session_id AND tag_path = @tag_path";
            }
            else if (aggregation == "1hour")
            {
                sql = @"
                    SELECT bucket as time, avg_value as value
                    FROM process_variables_1hour
                    WHERE session_id = @session_id AND tag_path = @tag_path";
            }
            else
            {
                sql = @"
                    SELECT time, value, sim_time
                    FROM process_variables
                    WHERE session_id = @session_id AND tag_path = @tag_path";
            }

            if (startTime.HasValue)
                sql += " AND time >= @start_time";

            if (endTime.HasValue)
                sql += " AND time <= @end_time";

            sql += " ORDER BY time DESC";

            if (limit.HasValue)
                sql += $" LIMIT {limit.Value}";

            await using var cmd = new NpgsqlCommand(sql, _connection);
            cmd.Parameters.AddWithValue("session_id", Guid.Parse(sessionId));
            cmd.Parameters.AddWithValue("tag_path", tagPath);

            if (startTime.HasValue)
                cmd.Parameters.AddWithValue("start_time", startTime.Value);

            if (endTime.HasValue)
                cmd.Parameters.AddWithValue("end_time", endTime.Value);

            await using var reader = await cmd.ExecuteReaderAsync();

            while (await reader.ReadAsync())
            {
                var point = new TimeSeriesPoint
                {
                    Time = reader.GetDateTime(0),
                    Value = reader.IsDBNull(1) ? 0 : reader.GetDouble(1)
                };

                if (!reader.IsDBNull(2))
                {
                    point.SimTime = reader.GetDateTime(2);
                }

                points.Add(point);
            }
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error querying tag {TagPath}", tagPath);
        }

        return points;
    }

    public async Task<TimeSeriesStats> GetStatsAsync(string sessionId)
    {
        var stats = new TimeSeriesStats
        {
            SessionId = sessionId
        };

        if (_connection == null || _connection.State != System.Data.ConnectionState.Open)
        {
            return stats;
        }

        try
        {
            const string sql = @"
                SELECT
                    COUNT(DISTINCT tag_path) as total_tags,
                    COUNT(*) as total_points,
                    MIN(time) as first_sample,
                    MAX(time) as last_sample
                FROM process_variables
                WHERE session_id = @session_id";

            await using var cmd = new NpgsqlCommand(sql, _connection);
            cmd.Parameters.AddWithValue("session_id", Guid.Parse(sessionId));

            await using var reader = await cmd.ExecuteReaderAsync();

            if (await reader.ReadAsync())
            {
                stats.TotalTags = reader.GetInt32(0);
                stats.TotalDataPoints = reader.GetInt64(1);

                if (!reader.IsDBNull(2))
                    stats.FirstSample = reader.GetDateTime(2);

                if (!reader.IsDBNull(3))
                    stats.LastSample = reader.GetDateTime(3);

                if (stats.FirstSample.HasValue && stats.LastSample.HasValue)
                {
                    stats.Duration = stats.LastSample.Value - stats.FirstSample.Value;
                }
            }
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error getting time-series stats for session {SessionId}", sessionId);
        }

        return stats;
    }

    public void Dispose()
    {
        _connection?.Dispose();
        _connectionLock?.Dispose();
        GC.SuppressFinalize(this);
    }
}
