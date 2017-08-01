/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.IO;

namespace SwarmOps
{
    /// <summary>
    /// Store fitness-values during optimization runs
    /// and write them to a file afterwards. The number
    /// of iterations per optimization run must be known
    /// in advance.
    /// </summary>
    public class FitnessTraceMean : FitnessTrace
    {
        #region Constructors.
        /// <summary>
        /// Construct a new object.
        /// </summary>
        /// <param name="numRuns">Number of optimization to be performed.</param>
        /// <param name="numIterations">Number of iterations per optimization run.</param>
        /// <param name="numIntervals">Approximate number of intervals to show mean.</param>
        public FitnessTraceMean(int numIterations, int numIntervals)
            : this(numIterations, numIntervals, null)
        {
        }

        /// <summary>
        /// Construct a new object.
        /// </summary>
        /// <param name="numIterations">Number of iterations per optimization run.</param>
        /// <param name="numIntervals">Approximate number of intervals to show mean.</param>
        /// <param name="chainedFitnessTrace">Chained FitnessTrace object.</param>
        public FitnessTraceMean(int numIterations, int numIntervals, FitnessTrace chainedFitnessTrace)
            : base(chainedFitnessTrace, numIterations, numIntervals, 0)
        {
            // Allocate trace.
            Trace = new StatisticsAccumulator[MaxIntervals];
            for (int i = 0; i < MaxIntervals; i++)
            {
                Trace[i] = new StatisticsAccumulator();
            }
        }
        #endregion

        #region Public members.
        /// <summary>
        /// Clear the stored fitness trace.
        /// </summary>
        public void Clear()
        {
            for (int i = 0; i < Trace.Length; i++)
            {
                Trace[i].Clear();
            }
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Log a fitness.
        /// </summary>
        /// <param name="index">Index into fitness-trace, mapped from optimization iteration.</param>
        /// <param name="fitness">Fitness value to log.</param>
        /// <param name="feasible">Feasibility (constraint satisfaction) to log.</param>
        protected override void Log(int index, double fitness, bool feasible)
        {
            Trace[index].Accumulate(fitness);
        }

        /// <summary>
        /// Write fitness-trace to a TextWriter stream.
        /// </summary>
        public override void Write(TextWriter writer)
        {
            writer.WriteLine("# Iteration\tMean Fitness\tStdError\tMin\tMax");
            writer.WriteLine();

            for (int i = 0; i < Trace.Length; i++)
            {
                StatisticsAccumulator trace = Trace[i];

                if (trace.Count > 0)
                {
                    double mean = trace.Mean;
                    double stdDev = trace.StandardDeviation;
                    double min = trace.Min;
                    double max = trace.Max;

                    writer.WriteLine(
                        "{0} {1} {2} {3} {4}",
                        Iteration(i),
                        Tools.FormatNumber(mean),
                        Tools.FormatNumber(stdDev),
                        Tools.FormatNumber(min),
                        Tools.FormatNumber(max));
                }
                else
                {
                    break;
                }
            }

            writer.Close();
        }
        #endregion

        #region Protected member variables.
        /// <summary>
        /// Storage for the fitness trace.
        /// </summary>
        protected StatisticsAccumulator[] Trace;
        #endregion
    }
}
