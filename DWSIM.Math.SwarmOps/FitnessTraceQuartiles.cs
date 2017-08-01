/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.IO;
using System.Collections.Generic;

namespace SwarmOps
{
    /// <summary>
    /// Store fitness-values at intervals during optimization
    /// runs and write their quartiles to a file afterwards.
    /// This only supports a fixed number of optimization
    /// runs and iterations per run which must therefore
    /// be known in advance. Keep the number of intervals
    /// small because it requires much memory.
    /// </summary>
    /// <remarks>
    /// A matrix of fitness values is being stored and
    /// their quartiles computed after optimization.
    /// </remarks>
    public class FitnessTraceQuartiles : FitnessTrace
    {
        #region Constructors.
        /// <summary>
        /// Construct a new object.
        /// </summary>
        /// <param name="numRuns">Number of optimization to be performed.</param>
        /// <param name="numIterations">Number of iterations per optimization run.</param>
        /// <param name="numIntervals">Approximate number of intervals to show quartiles.</param>
        public FitnessTraceQuartiles(int numRuns, int numIterations, int numIntervals)
            : this(numRuns, numIterations, numIntervals, null)
        {
        }

        /// <summary>
        /// Construct a new object.
        /// </summary>
        /// <param name="numRuns">Number of optimization to be performed.</param>
        /// <param name="numIterations">Number of iterations per optimization run.</param>
        /// <param name="numIntervals">Approximate number of intervals to show quartiles.</param>
        /// <param name="chainedFitnessTrace">Chained FitnessTrace object.</param>
        public FitnessTraceQuartiles(int numRuns, int numIterations, int numIntervals, FitnessTrace chainedFitnessTrace)
            : base(chainedFitnessTrace, numIterations, numIntervals, 0.5)
        {
            NumRuns = numRuns;
            NumIterations = numIterations;

            // Allocate trace.
            Trace = new List<double>[MaxIntervals];
            for (int i = 0; i < MaxIntervals; i++)
            {
                Trace[i] = new List<double>(numRuns);
            }
        }
        #endregion

        #region Public fields.
        /// <summary>
        /// Number of optimization runs being fitness-traced.
        /// </summary>
        public int NumRuns
        {
            get;
            private set;
        }

        /// <summary>
        /// Number of iterations per optimization run being fitness-traced.
        /// </summary>
        public int NumIterations
        {
            get;
            private set;
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
            Trace[index].Add(fitness);
        }

        /// <summary>
        /// Write fitness-trace to a TextWriter stream.
        /// </summary>
        public override void Write(TextWriter writer)
        {
            writer.WriteLine("# Iteration\tMin\tQ1\tMedian\tQ3\tMax");
            writer.WriteLine();

            for (int i = 0; i < Trace.Length; i++)
            {
                Quartiles quartiles = new Quartiles();

                double[] traceArray = Trace[i].ToArray();

                if (traceArray.Length > 0)
                {
                    quartiles.ComputeUnsortedInplace(traceArray);

                    writer.WriteLine(
                        "{0} {1} {2} {3} {4} {5}",
                        Iteration(i),
                        Tools.FormatNumber(quartiles.Min),
                        Tools.FormatNumber(quartiles.Q1),
                        Tools.FormatNumber(quartiles.Median),
                        Tools.FormatNumber(quartiles.Q3),
                        Tools.FormatNumber(quartiles.Max));
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
        /// Matrix for storage of the fitness trace.
        /// </summary>
        protected List<double>[] Trace;
        #endregion
    }
}
