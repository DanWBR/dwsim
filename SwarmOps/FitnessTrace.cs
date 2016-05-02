/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.IO;
using System.Diagnostics;

namespace SwarmOps
{
    /// <summary>
    /// Base-class for tracing fitness progress during
    /// optimization.
    public abstract class FitnessTrace
    {
        #region Constructors.
        /// <summary>
        /// Construct a new object.
        /// </summary>
        public FitnessTrace(FitnessTrace chainedFitnessTrace, int numIterations, int numIntervals, double offset)
        {
            ChainedFitnessTrace = chainedFitnessTrace;

            // The number of intervals at which to log/show fitness cannot
            // be greater than the number of optimization iterations.
            int intervals = System.Math.Min(numIntervals, numIterations);

            // The stride is the number of optimization iterations that each
            // interval is made up of.
            Stride = numIterations / intervals;

            // Offset for the first fitness log.
            Offset = (int)(offset * Stride);

            // The maximum number of intervals at which to log the fitness.
            // Consider numIterations==10, Offset==0, Stride==3, this requires
            // logging at iterations 0, 3, 6, 9. That is, a total of four log
            // entries. The reason that Ceiling is used is that
            // (numIterations - Offset) / Stride == 10 / 3 == 3 using integer
            // rounding.
            MaxIntervals = (int)System.Math.Ceiling((double)(numIterations - Offset) / Stride);
        }
        #endregion

        #region Public fields.
        /// <summary>
        /// Number of intervals during optimization at which to log and show fitness.
        /// </summary>
        public int MaxIntervals
        {
            get;
            private set;
        }

        /// <summary>
        /// Offset for showing the first trace-element.
        /// </summary>
        public int Offset
        {
            get;
            private set;
        }

        /// <summary>
        /// Stride between the intervals for showing trace-elements.
        /// </summary>
        public int Stride
        {
            get;
            private set;
        }

        /// <summary>
        /// Chained fitness tracer.
        /// </summary>
        public FitnessTrace ChainedFitnessTrace
        {
            get;
            private set;
        }
        #endregion

        #region Public members.
        /// <summary>
        /// Add a fitness value to the trace at the given iteration number.
        /// </summary>
        /// <param name="iteration">Iteration number of fitness.</param>
        /// <param name="fitness">Fitness value to be traced.</param>
        /// <param name="feasible">Feasibility (constraint satisfaction) to be traced.</param>
        public void Add(int iteration, double fitness, bool feasible)
        {
            // If the optimization-iteration falls on an interval then log the fitness.
            if ((iteration - Offset) % Stride == 0)
            {
                int index = (iteration - Offset) / Stride;

                if (index < MaxIntervals)
                {
                    Log(index, fitness, feasible);
                }
            }

            // Call chained fitness-tracer.
            if (ChainedFitnessTrace != null)
            {
                ChainedFitnessTrace.Add(iteration, fitness, feasible);
            }
        }

        /// <summary>
        /// Write fitness-trace to a TextWriter stream.
        /// </summary>
        public abstract void Write(TextWriter writer);

        /// <summary>
        /// Write fitness-trace to a file.
        /// </summary>
        /// <param name="filename">Name of file.</param>
        public void WriteToFile(string filename)
        {
            TextWriter writer = new StreamWriter(filename);

            Write(writer);
        }
        #endregion

        #region Protected members.
        /// <summary>
        /// Log a fitness.
        /// </summary>
        /// <param name="index">Index into fitness-trace, mapped from optimization iteration.</param>
        /// <param name="fitness">Fitness value to log.</param>
        /// <param name="feasible">Feasibility (constraint satisfaction) to log.</param>
        protected abstract void Log(int index, double fitness, bool feasible);

        /// <summary>
        /// Map fitness-trace index to optimization iteration.
        /// </summary>
        protected int Iteration(int index)
        {
            return index * Stride + Offset + 1;
        }
        #endregion
    }
}
