/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Collections.Generic;
using System.Linq;
using SwarmOps.Extensions;

namespace SwarmOps
{
    /// <summary>
    /// Wrapper for an optimizer providing statistics such as
    /// mean fitness achieved over a number of optimization runs,
    /// best results achieved, etc. Transparently supports the
    /// same methods as the the optimizer itself, but stores the
    /// optimization results so as to compute the statistics.
    /// </summary>
    public class Statistics : OptimizerWrapper
    {
        #region Constructors.
        /// <summary>
        /// Create a Statistics-object.
        /// </summary>
        /// <param name="optimizer">Optimizer-object being wrapped.</param>
        /// <param name="onlyFeasible">Only use feasible results.</param>
        public Statistics(Optimizer optimizer, bool onlyFeasible)
            : base(optimizer)
        {
            OnlyFeasible = onlyFeasible;
            Results = new List<Result>();
        }
        #endregion

        #region Public fields.
        /// <summary>
        /// Only use feasible results.
        /// </summary>
        public bool OnlyFeasible
        {
            get;
            private set;
        }

        /// <summary>
        /// Number of results regardless of feasibility.
        /// </summary>
        public int Count
        {
            get;
            protected set;
        }

        /// <summary>
        /// Number of feasible results.
        /// </summary>
        public int CountFeasible
        {
            get;
            protected set;
        }

        /// <summary>
        /// Fraction of results that are feasible (satisfy constraints).
        /// </summary>
        public double FeasibleFraction
        {
            get
            {
                return (double)CountFeasible / Count;
            }
        }

        /// <summary>
        /// Optimization results stored for later computation of statistics.
        /// </summary>
        public List<Result> Results
        {
            get;
            private set;
        }

        /// <summary>
        /// Best optimization results based on fitness alone. There may be several,
        /// equally good results. To get the first call BestResult instead.
        /// </summary>
        public IEnumerable<Result> BestResults
        {
            get;
            private set;
        }

        /// <summary>
        /// Best optimization result achieved, based on fitness alone.
        /// </summary>
        public Result BestResult
        {
            get
            {
                IEnumerator<Result> results = BestResults.GetEnumerator();
                results.MoveNext();
                return results.Current;
            }
        }

        /// <summary>
        /// Parameters for best optimization result achieved.
        /// </summary>
        public double[] BestParameters
        {
            get
            {
                return BestResult.Parameters;
            }
        }

        /// <summary>
        /// Quartiles for fitness results.
        /// </summary>
        public Quartiles FitnessQuartiles
        {
            get;
            private set;
        }

        /// <summary>
        /// Fitness for best solution found.
        /// </summary>
        public double? FitnessMin
        {
            get { return FitnessQuartiles.Min; }
        }

        /// <summary>
        /// Fitness for worst solution found.
        /// </summary>
        public double? FitnessMax
        {
            get { return FitnessQuartiles.Max; }
        }

        /// <summary>
        /// Fitness mean or average for all optimization results.
        /// </summary>
        public double? FitnessMean
        {
            get;
            private set;
        }

        /// <summary>
        /// Standard deviation of fitness for all optimization results.
        /// </summary>
        public double? FitnessStdDev
        {
            get;
            private set;
        }

        /// <summary>
        /// Quartiles for iterations results.
        /// </summary>
        public Quartiles IterationsQuartiles
        {
            get;
            private set;
        }

        /// <summary>
        /// Lowest number of iterations used in a single optimization run.
        /// </summary>
        public double? IterationsMin
        {
            get { return IterationsQuartiles.Min; }
        }

        /// <summary>
        /// Highest number of iterations used in a single optimization run.
        /// </summary>
        public double? IterationsMax
        {
            get { return IterationsQuartiles.Max; }
        }

        /// <summary>
        /// Mean number of iterations used in optimization runs.
        /// </summary>
        public double? IterationsMean
        {
            get;
            private set;
        }

        /// <summary>
        /// Standard deviation for the number of iterations used in optimization runs.
        /// </summary>
        public double? IterationsStdDev
        {
            get;
            private set;
        }
        #endregion

        #region Public methods.
        /// <summary>
        /// Compute the statistics. Call this after all
        /// optimization runs have executed.
        /// </summary>
        public void Compute()
        {
            FitnessQuartiles = new Quartiles();
            IterationsQuartiles = new Quartiles();

            if (Results.Count > 0)
            {
                // Fitness quartiles.
                double[] fitnessArray = Results.Select(o => o.Fitness).ToArray();
                FitnessQuartiles.ComputeUnsortedInplace(fitnessArray);

                // Iterations quartiles.
                double[] iterationsArray = Results.Select(o => o.Iterations).ToArray();
                IterationsQuartiles.ComputeUnsortedInplace(iterationsArray);

                // Fitness mean and stddev.
                FitnessMean = Results.Average(o => o.Fitness);
                FitnessStdDev = Results.StdDev(o => o.Fitness);

                // Iterations mean and stddev.
                IterationsMean = Results.Average(o => o.Iterations);
                IterationsStdDev = Results.StdDev(o => o.Iterations);
            }
            else
            {
                // Fitness mean and stddev.
                FitnessMean = null;
                FitnessStdDev = null;

                // Iterations mean and stddev.
                IterationsMean = null;
                IterationsStdDev = null;
            }

            // Best results.
            BestResults = Results.Where(o => o.Fitness == FitnessMin);
        }

        /// <summary>
        /// Clear the stored data used for computing statistics.
        /// </summary>
        public void Clear()
        {
            Results.Clear();

            Count = 0;
            CountFeasible = 0;
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Return the name of the problem.
        /// </summary>
        public override string Name
        {
            get { return "Statistics (" + Optimizer.Name + ")"; }
        }

        /// <summary>
        /// Perform one optimization run and return the best found solution.
        /// This just wraps around the Optimizer and stores the results for
        /// later computation of statistics.
        /// </summary>
        /// <param name="parameters">Control parameters for the optimizer.</param>
        /// <param name="fitnessLimit">Preemptive Fitness Limit</param>
        public override Result Optimize(double[] parameters, double fitnessLimit)
        {
            // Call through to the Optimizer.
            Result result = Optimizer.Optimize(parameters, fitnessLimit);

            // Store optimization result for later use by the Compute() method,
            // if feasibility is required then only store feasible results.
            if (!OnlyFeasible || result.Feasible)
            {
                Results.Add(result);
            }

            if (result.Feasible)
            {
                // Increase count of feasible results.
                CountFeasible++;
            }

            // Increase total count of results, regardless of feasibility.
            Count++;

            // Return the optimization result.
            return result;
        }
        #endregion
    }
}
