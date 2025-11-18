# DWSIM OTS - Deployment Guide

This guide provides comprehensive instructions for deploying the DWSIM Operator Training System in production environments.

## Table of Contents

1. [System Requirements](#system-requirements)
2. [Quick Start with Docker](#quick-start-with-docker)
3. [Production Deployment](#production-deployment)
4. [Configuration](#configuration)
5. [Security Considerations](#security-considerations)
6. [Monitoring and Maintenance](#monitoring-and-maintenance)
7. [Troubleshooting](#troubleshooting)
8. [Backup and Recovery](#backup-and-recovery)

## System Requirements

### Minimum Requirements
- **CPU**: 4 cores
- **RAM**: 8 GB
- **Storage**: 50 GB SSD
- **OS**: Linux (Ubuntu 20.04+), Windows Server 2019+, macOS 11+
- **Docker**: 20.10+
- **Docker Compose**: 1.29+

### Recommended for Production
- **CPU**: 8+ cores
- **RAM**: 16+ GB
- **Storage**: 200+ GB SSD (for time-series data)
- **Network**: 1 Gbps

## Quick Start with Docker

### 1. Clone the Repository

```bash
git clone https://github.com/KURIANGEORGE57/dwsim.git
cd dwsim/ots-dwsim
```

### 2. Create Environment File

```bash
cp .env.example .env
```

Edit `.env` and set secure passwords:

```bash
POSTGRES_PASSWORD=your_secure_password_here
```

### 3. Start All Services

```bash
docker-compose up -d
```

### 4. Verify Deployment

Check that all services are running:

```bash
docker-compose ps
```

All services should show status as "Up" with healthy state.

### 5. Access the Applications

- **Operator HMI**: http://localhost:3000
- **Instructor Console**: http://localhost:3001
- **API Documentation**: http://localhost:5000/swagger

## Production Deployment

### Architecture Overview

```
┌─────────────────┐      ┌─────────────────┐      ┌─────────────────┐
│  Operator HMI   │      │ Instructor UI   │      │   API Clients   │
│   (Port 3000)   │      │   (Port 3001)   │      │                 │
└────────┬────────┘      └────────┬────────┘      └────────┬────────┘
         │                        │                         │
         └────────────────────────┴─────────────────────────┘
                                  │
                      ┌───────────▼───────────┐
                      │  DWSIM Simulation     │
                      │      Host API         │
                      │    (Port 5000)        │
                      └───────────┬───────────┘
                                  │
                      ┌───────────▼───────────┐
                      │   TimescaleDB         │
                      │  Time-Series Store    │
                      │    (Port 5433)        │
                      └───────────────────────┘
```

### Step 1: Server Preparation

#### Install Docker

**Ubuntu/Debian:**
```bash
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh
sudo usermod -aG docker $USER
```

**CentOS/RHEL:**
```bash
sudo yum install -y docker
sudo systemctl start docker
sudo systemctl enable docker
```

#### Install Docker Compose

```bash
sudo curl -L "https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose
```

### Step 2: Configure Firewall

```bash
# Allow HTTP/HTTPS
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp

# Allow application ports
sudo ufw allow 3000/tcp  # Operator HMI
sudo ufw allow 3001/tcp  # Instructor UI
sudo ufw allow 5000/tcp  # API

# Enable firewall
sudo ufw enable
```

### Step 3: SSL/TLS Configuration (Recommended)

For production, use a reverse proxy with SSL termination:

#### Option A: Nginx Reverse Proxy

Create `nginx/ots.conf`:

```nginx
server {
    listen 443 ssl http2;
    server_name ots.example.com;

    ssl_certificate /etc/ssl/certs/ots.crt;
    ssl_certificate_key /etc/ssl/private/ots.key;

    # Operator HMI
    location / {
        proxy_pass http://localhost:3000;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }

    # Instructor UI
    location /instructor/ {
        proxy_pass http://localhost:3001/;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }

    # API
    location /api/ {
        proxy_pass http://localhost:5000/api/;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
```

#### Option B: Traefik (with Let's Encrypt)

Add to `docker-compose.yml`:

```yaml
services:
  traefik:
    image: traefik:v2.10
    command:
      - "--providers.docker=true"
      - "--entrypoints.web.address=:80"
      - "--entrypoints.websecure.address=:443"
      - "--certificatesresolvers.letsencrypt.acme.tlschallenge=true"
      - "--certificatesresolvers.letsencrypt.acme.email=admin@example.com"
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock
      - traefik_certs:/letsencrypt
```

### Step 4: Configure Data Persistence

Modify `docker-compose.yml` to use bind mounts for critical data:

```yaml
volumes:
  # Use named volumes (recommended)
  timescaledb_data:
    driver: local
    driver_opts:
      type: none
      o: bind
      device: /opt/ots/data/timescaledb

  # Or use NFS for shared storage
  flowsheet_data:
    driver: local
    driver_opts:
      type: nfs
      o: addr=nfs.example.com,rw
      device: ":/ots/flowsheets"
```

### Step 5: Deploy with Docker Compose

```bash
# Pull latest images
docker-compose pull

# Start services
docker-compose up -d

# View logs
docker-compose logs -f

# Check health status
docker-compose ps
```

## Configuration

### Environment Variables

All configuration is done via environment variables in `.env`:

```bash
# Database
POSTGRES_PASSWORD=secure_password

# API Settings (optional overrides)
ASPNETCORE_ENVIRONMENT=Production
FLOWSHEET_PATH=/app/data/flowsheets
SNAPSHOT_PATH=/app/data/snapshots

# Time-Series Logging
TIMESERIES_ENABLED=true
TIMESERIES_POLLING_INTERVAL_MS=1000
TIMESERIES_BATCH_SIZE=100

# Ports (if different from defaults)
DWSIM_HOST_PORT=5000
OPERATOR_HMI_PORT=3000
INSTRUCTOR_UI_PORT=3001
```

### Application Configuration

Edit `src/dwsim-host/appsettings.Production.json` for production-specific settings:

```json
{
  "Logging": {
    "LogLevel": {
      "Default": "Warning",
      "DWSIM.OTS": "Information"
    }
  },
  "ConnectionStrings": {
    "TimescaleDB": "Host=timescaledb;Database=ots_timeseries;Username=ots_user"
  },
  "TimeSeriesLogging": {
    "Enabled": true,
    "PollingIntervalMs": 1000,
    "BatchSize": 100
  }
}
```

## Security Considerations

### 1. Change Default Passwords

**Never use default passwords in production!**

```bash
# Generate strong password
openssl rand -base64 32

# Update .env
POSTGRES_PASSWORD=<generated_password>
```

### 2. Network Security

- Use Docker networks to isolate services
- Expose only necessary ports to the host
- Use SSL/TLS for all public-facing services
- Implement firewall rules

### 3. Access Control

- Implement authentication for Instructor UI (future enhancement)
- Use API keys for external integrations
- Restrict database access to internal network only

### 4. Data Protection

- Enable encryption at rest for TimescaleDB
- Use encrypted volumes for sensitive data
- Implement regular backup procedures

### 5. Container Security

```bash
# Run security scan on images
docker scan ots-dwsim-host:latest

# Update base images regularly
docker-compose pull
docker-compose up -d
```

## Monitoring and Maintenance

### Health Checks

All services include health checks. Monitor with:

```bash
# Check all service health
docker-compose ps

# View health check logs
docker inspect --format='{{json .State.Health}}' ots-dwsim-host | jq
```

### Logging

Centralized logging with Docker:

```bash
# View all logs
docker-compose logs -f

# View specific service
docker-compose logs -f dwsim-host

# Save logs to file
docker-compose logs --no-color > ots-logs-$(date +%Y%m%d).log
```

### Metrics Collection (Optional)

Add Prometheus and Grafana for metrics:

```yaml
services:
  prometheus:
    image: prom/prometheus:latest
    volumes:
      - ./monitoring/prometheus.yml:/etc/prometheus/prometheus.yml
      - prometheus_data:/prometheus
    ports:
      - "9090:9090"

  grafana:
    image: grafana/grafana:latest
    volumes:
      - grafana_data:/var/lib/grafana
    ports:
      - "3002:3000"
    depends_on:
      - prometheus
```

### Resource Limits

Add resource limits to `docker-compose.yml`:

```yaml
services:
  dwsim-host:
    deploy:
      resources:
        limits:
          cpus: '4'
          memory: 4G
        reservations:
          cpus: '2'
          memory: 2G
```

## Troubleshooting

### Common Issues

#### 1. Services Won't Start

```bash
# Check logs
docker-compose logs

# Verify network connectivity
docker network inspect ots-network

# Check port conflicts
sudo netstat -tulpn | grep -E '3000|3001|5000|5433'
```

#### 2. Database Connection Errors

```bash
# Check TimescaleDB status
docker-compose ps timescaledb

# Connect to database
docker-compose exec timescaledb psql -U ots_user -d ots_timeseries

# Verify schema
\dt
```

#### 3. Frontend Can't Connect to API

- Check nginx proxy configuration
- Verify CORS settings in API
- Check browser console for errors
- Verify network connectivity: `docker-compose exec operator-hmi ping dwsim-host`

#### 4. High Memory Usage

```bash
# Check container resource usage
docker stats

# Adjust TimescaleDB settings
# Edit postgresql.conf and tune:
shared_buffers = 2GB
effective_cache_size = 6GB
work_mem = 50MB
```

### Debug Mode

Enable debug logging:

```bash
# Update .env
ASPNETCORE_ENVIRONMENT=Development

# Restart services
docker-compose restart dwsim-host
```

## Backup and Recovery

### Automated Backup Script

Create `backup.sh`:

```bash
#!/bin/bash
BACKUP_DIR="/opt/ots/backups/$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"

# Backup TimescaleDB
docker-compose exec -T timescaledb pg_dump -U ots_user ots_timeseries > "$BACKUP_DIR/timescaledb.sql"

# Backup volumes
docker run --rm -v ots-dwsim_flowsheet_data:/data -v "$BACKUP_DIR":/backup alpine tar czf /backup/flowsheets.tar.gz /data
docker run --rm -v ots-dwsim_snapshot_data:/data -v "$BACKUP_DIR":/backup alpine tar czf /backup/snapshots.tar.gz /data
docker run --rm -v ots-dwsim_scenario_data:/data -v "$BACKUP_DIR":/backup alpine tar czf /backup/scenarios.tar.gz /data
docker run --rm -v ots-dwsim_ruleset_data:/data -v "$BACKUP_DIR":/backup alpine tar czf /backup/rulesets.tar.gz /data

# Compress backup
tar czf "$BACKUP_DIR.tar.gz" -C "$BACKUP_DIR" .
rm -rf "$BACKUP_DIR"

# Rotate old backups (keep last 7 days)
find /opt/ots/backups -name "*.tar.gz" -mtime +7 -delete

echo "Backup completed: $BACKUP_DIR.tar.gz"
```

Make executable and schedule:

```bash
chmod +x backup.sh

# Add to crontab (daily at 2 AM)
echo "0 2 * * * /opt/ots/backup.sh" | crontab -
```

### Restore from Backup

```bash
#!/bin/bash
BACKUP_FILE="$1"

# Extract backup
RESTORE_DIR="/tmp/ots_restore_$(date +%s)"
mkdir -p "$RESTORE_DIR"
tar xzf "$BACKUP_FILE" -C "$RESTORE_DIR"

# Stop services
docker-compose stop

# Restore database
cat "$RESTORE_DIR/timescaledb.sql" | docker-compose exec -T timescaledb psql -U ots_user ots_timeseries

# Restore volumes
docker run --rm -v ots-dwsim_flowsheet_data:/data -v "$RESTORE_DIR":/backup alpine sh -c "tar xzf /backup/flowsheets.tar.gz -C /"
docker run --rm -v ots-dwsim_snapshot_data:/data -v "$RESTORE_DIR":/backup alpine sh -c "tar xzf /backup/snapshots.tar.gz -C /"
docker run --rm -v ots-dwsim_scenario_data:/data -v "$RESTORE_DIR":/backup alpine sh -c "tar xzf /backup/scenarios.tar.gz -C /"
docker run --rm -v ots-dwsim_ruleset_data:/data -v "$RESTORE_DIR":/backup alpine sh -c "tar xzf /backup/rulesets.tar.gz -C /"

# Start services
docker-compose start

# Cleanup
rm -rf "$RESTORE_DIR"

echo "Restore completed from: $BACKUP_FILE"
```

## Scaling

### Horizontal Scaling

For high-availability deployments:

```yaml
services:
  dwsim-host:
    deploy:
      mode: replicated
      replicas: 3
      endpoint_mode: vip
```

Add load balancer (HAProxy):

```yaml
  haproxy:
    image: haproxy:latest
    volumes:
      - ./haproxy.cfg:/usr/local/etc/haproxy/haproxy.cfg:ro
    ports:
      - "80:80"
    depends_on:
      - dwsim-host
```

### Database Replication

For production, configure TimescaleDB replication:

```yaml
  timescaledb-primary:
    image: timescale/timescaledb:latest-pg15
    environment:
      POSTGRES_DB: ots_timeseries
      REPLICATION_MODE: master

  timescaledb-replica:
    image: timescale/timescaledb:latest-pg15
    environment:
      POSTGRES_DB: ots_timeseries
      REPLICATION_MODE: slave
      REPLICATION_HOST: timescaledb-primary
```

## Update Procedure

```bash
# 1. Backup current deployment
./backup.sh

# 2. Pull latest changes
git pull origin main

# 3. Pull new images
docker-compose pull

# 4. Restart services with new images
docker-compose up -d

# 5. Verify all services are healthy
docker-compose ps

# 6. Check logs for errors
docker-compose logs -f --tail=100
```

## Support

For issues and questions:
- GitHub Issues: https://github.com/KURIANGEORGE57/dwsim/issues
- Documentation: https://docs.claude.com/ots-dwsim

## License

This deployment guide is part of the DWSIM OTS project. See LICENSE file for details.
