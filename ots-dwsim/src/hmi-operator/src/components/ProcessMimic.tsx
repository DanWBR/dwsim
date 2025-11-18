import React from 'react';
import { Box, Typography } from '@mui/material';
import { ProcessVariable } from '../types';

interface ProcessMimicProps {
  processVars: ProcessVariable[];
}

const ProcessMimic: React.FC<ProcessMimicProps> = ({ processVars }) => {
  // Simple SVG-based process diagram
  // In production, this would be a sophisticated P&ID drawing

  const getVarValue = (varName: string): string => {
    const pv = processVars.find(v => v.tagPath.includes(varName));
    if (!pv) return '--.--';

    const value = typeof pv.value === 'number' ? pv.value.toFixed(2) : pv.value;
    return pv.units ? `${value} ${pv.units}` : String(value);
  };

  const getVarNumeric = (varName: string): number => {
    const pv = processVars.find(v => v.tagPath.includes(varName));
    return typeof pv?.value === 'number' ? pv.value : 0;
  };

  // Normalize flow for animation (0-100%)
  const flowRate = Math.min(100, Math.max(0, (getVarNumeric('Flow') / 5.0) * 100));

  return (
    <Box
      sx={{
        width: '100%',
        height: '100%',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        position: 'relative'
      }}
    >
      <svg
        width="100%"
        height="100%"
        viewBox="0 0 800 300"
        style={{ backgroundColor: '#0a0a0a', borderRadius: '4px' }}
      >
        {/* Feed Stream */}
        <g id="feed-stream">
          {/* Pipe */}
          <rect x="50" y="140" width="150" height="20" fill="#555" stroke="#888" strokeWidth="2" />

          {/* Flow animation */}
          <rect x="50" y="140" width={flowRate * 1.5} height="20" fill="#4ecdc4" opacity="0.5">
            <animate
              attributeName="opacity"
              values="0.3;0.7;0.3"
              dur="1s"
              repeatCount="indefinite"
            />
          </rect>

          {/* Arrow */}
          <polygon points="190,150 210,140 210,160" fill="#4ecdc4" />

          {/* Label */}
          <text x="100" y="130" fill="#4ecdc4" fontSize="14" fontWeight="bold">FEED</text>
          <text x="60" y="180" fill="#4ecdc4" fontSize="12">
            T: {getVarValue('Temperature')}
          </text>
          <text x="60" y="195" fill="#4ecdc4" fontSize="12">
            F: {getVarValue('Flow')}
          </text>
        </g>

        {/* Column Unit */}
        <g id="column">
          <rect x="250" y="50" width="100" height="200" fill="#333" stroke="#666" strokeWidth="3" rx="5" />
          <text x="270" y="35" fill="#ccc" fontSize="16" fontWeight="bold">COLUMN-1</text>

          {/* Internal stages (simplified) */}
          {[0, 1, 2, 3, 4].map(i => (
            <line
              key={i}
              x1="255"
              y1={70 + i * 40}
              x2="345"
              y2={70 + i * 40}
              stroke="#555"
              strokeWidth="1"
            />
          ))}

          {/* Duty indicator */}
          <circle cx="300" cy="150" r="30" fill="none" stroke="#ff6b6b" strokeWidth="2" />
          <text x="270" y="155" fill="#ff6b6b" fontSize="10">
            {getVarValue('DutyRequired')}
          </text>
        </g>

        {/* Top Product */}
        <g id="top-product">
          <rect x="350" y="70" width="150" height="20" fill="#555" stroke="#888" strokeWidth="2" />
          <polygon points="490,80 510,70 510,90" fill="#95e1d3" />
          <text x="380" y="65" fill="#95e1d3" fontSize="14" fontWeight="bold">DISTILLATE</text>
        </g>

        {/* Bottom Product */}
        <g id="bottom-product">
          <rect x="350" y="210" width="150" height="20" fill="#555" stroke="#888" strokeWidth="2" />
          <polygon points="490,220 510,210 510,230" fill="#f9ca24" />
          <text x="380" y="250" fill="#f9ca24" fontSize="14" fontWeight="bold">BOTTOMS</text>
        </g>

        {/* Pressure indicator */}
        <g id="pressure-indicator">
          <circle cx="300" cy="60" r="15" fill="#1e1e1e" stroke="#45b7d1" strokeWidth="2" />
          <text x="292" y="65" fill="#45b7d1" fontSize="10" fontWeight="bold">P</text>
        </g>

        {/* Temperature indicator */}
        <g id="temp-indicator">
          <rect x="285" y="240" width="30" height="5" fill="#ff6b6b" />
          <text x="280" y="260" fill="#ff6b6b" fontSize="10">
            T: {getVarValue('Temperature')}
          </text>
        </g>

        {/* Status text */}
        <text x="600" y="30" fill="#ccc" fontSize="12">
          Process Status: ONLINE
        </text>
        <text x="600" y="50" fill="#4ecdc4" fontSize="12">
          Flow: {flowRate.toFixed(0)}%
        </text>
      </svg>

      {/* Overlay info */}
      <Box
        sx={{
          position: 'absolute',
          bottom: 10,
          right: 10,
          backgroundColor: 'rgba(0,0,0,0.7)',
          padding: 1,
          borderRadius: 1
        }}
      >
        <Typography variant="caption" color="text.secondary">
          Simplified P&ID - Production version would include detailed equipment
        </Typography>
      </Box>
    </Box>
  );
};

export default ProcessMimic;
