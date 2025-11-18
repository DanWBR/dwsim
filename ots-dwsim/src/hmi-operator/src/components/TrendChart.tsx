import React, { useEffect, useState } from 'react';
import {
  LineChart,
  Line,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  Legend,
  ResponsiveContainer
} from 'recharts';
import { ProcessVariable } from '../types';

interface TrendChartProps {
  processVars: ProcessVariable[];
}

interface DataPoint {
  timestamp: string;
  time: number;
  [key: string]: number | string;
}

const TrendChart: React.FC<TrendChartProps> = ({ processVars }) => {
  const [data, setData] = useState<DataPoint[]>([]);
  const [startTime] = useState(Date.now());

  useEffect(() => {
    if (processVars.length === 0) return;

    // Add new data point
    const newPoint: DataPoint = {
      timestamp: new Date().toLocaleTimeString(),
      time: (Date.now() - startTime) / 1000 // seconds since start
    };

    // Add all process variable values
    processVars.forEach(pv => {
      if (typeof pv.value === 'number') {
        newPoint[pv.displayName] = pv.value;
      }
    });

    setData(prevData => {
      const newData = [...prevData, newPoint];
      // Keep last 60 data points (1 minute at 1Hz)
      if (newData.length > 60) {
        return newData.slice(-60);
      }
      return newData;
    });
  }, [processVars, startTime]);

  // Define colors for different variables
  const getColor = (name: string): string => {
    if (name.includes('Temperature') || name === 'Temperature') return '#ff6b6b';
    if (name.includes('Flow') || name === 'Flow') return '#4ecdc4';
    if (name.includes('Pressure') || name === 'Pressure') return '#45b7d1';
    if (name.includes('Duty') || name === 'DutyRequired') return '#f9ca24';
    return '#95e1d3';
  };

  // Get unique variable names
  const variables = processVars
    .filter(pv => typeof pv.value === 'number')
    .map(pv => pv.displayName);

  return (
    <ResponsiveContainer width="100%" height="90%">
      {data.length > 0 ? (
        <LineChart
          data={data}
          margin={{ top: 5, right: 30, left: 20, bottom: 5 }}
        >
          <CartesianGrid strokeDasharray="3 3" stroke="#333" />
          <XAxis
            dataKey="time"
            stroke="#888"
            tickFormatter={(value) => `${value.toFixed(0)}s`}
          />
          <YAxis stroke="#888" />
          <Tooltip
            contentStyle={{
              backgroundColor: '#1e1e1e',
              border: '1px solid #444',
              borderRadius: '4px'
            }}
            labelFormatter={(value) => `Time: ${value}s`}
          />
          <Legend />

          {variables.map(varName => (
            <Line
              key={varName}
              type="monotone"
              dataKey={varName}
              stroke={getColor(varName)}
              strokeWidth={2}
              dot={false}
              animationDuration={300}
            />
          ))}
        </LineChart>
      ) : (
        <div style={{
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          height: '100%',
          color: '#666'
        }}>
          Waiting for data...
        </div>
      )}
    </ResponsiveContainer>
  );
};

export default TrendChart;
