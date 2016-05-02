/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Diagnostics;

namespace SwarmOps.Optimizers.Parallel
{
    /// <summary>
    /// Parallel version of MetaFitness where parallelization
    /// is made on the optimization runs.
    /// </summary>
    public class MetaFitness : SwarmOps.Optimizers.MetaFitness
    {
        #region Constructors.
        /// <summary>
        /// Construct the object, un-weighted problems.
        /// </summary>
        /// <param name="optimizer">Optimize to be used.</param>
        /// <param name="problems">Array of problems to be optimized.</param>
        /// <param name="numRuns">Number of optimization runs per problem.</param>
        /// <param name="maxIterations">Max number of optimization iterations.</param>
        public MetaFitness(Optimizer optimizer, Problem[] problems, int numRuns, int maxIterations)
            : base(optimizer, problems, numRuns, maxIterations)
        {
        }

        /// <summary>
        /// Construct the object, weighted problems.
        /// </summary>
        /// <param name="optimizer">Optimize to be used.</param>
        /// <param name="weightedProblems">Array of weighted problems to be optimized.</param>
        /// <param name="numRuns">Number of optimization runs per problem.</param>
        /// <param name="maxIterations">Max number of optimization iterations.</param>
        public MetaFitness(Optimizer optimizer, WeightedProblem[] weightedProblems, int numRuns, int maxIterations)
            : base(optimizer, weightedProblems, numRuns, maxIterations)
        {
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the optimization problem.
        /// </summary>
        public override string Name
        {
            get { return "MetaFitnessParallel (" + Optimizer.Name + ")"; }
        }

        /// <summary>
        /// Compute the meta-fitness measure by passing the
        /// given parameters to the Optimizer, and perform
        /// optimization runs on the array of problems
        /// until the fitness compute exceeds the fitnessLimit.
        /// </summary>
        /// <param name="parameters">Parameters to use for the Optimizer.</param>
        /// <param name="fitnessLimit">Preemptive Fitness Limit</param>
        /// <returns>Fitness value.</returns>
        public override double Fitness(double[] parameters, double fitnessLimit)
        {
            // Initialize the fitnes-sum.
            double fitnessSum = 0;

            // Object used to lock thread-access to fitnessSum.
            object fitnessSumLock = new object();

            // Iterate over the problems.
            // NOTE: Because the optimizer's Problem is assigned in this outer-loop
            // it is not possible to parallellize this outer-loop as it would require
            // a new Optimizer-object for each thread. SwarmOps was not designed that
            // way.
            for (int i = 0; i < ProblemIndex.Count && fitnessSum < fitnessLimit; i++)
            {
                // Assign the problem to the optimizer.
                Optimizer.Problem = ProblemIndex.GetProblem(i);

                // Get the weight associated with this problem.
                double weight = ProblemIndex.GetWeight(i);

                // Use another fitness summation because we need to keep
                // track of the performance on each problem.
                double fitnessSumInner = 0;

                // Perform a number of optimization runs. (Parallel)
                System.Threading.Tasks.Parallel.For(0, NumRuns, Globals.ParallelOptions, (j, loopState) =>
                {
                    // Perform one optimization run on the problem.
                    Result result = Optimizer.Optimize(parameters, fitnessLimit - fitnessSum);

                    // Get the best fitness result from optimization and adjust it
                    // by subtracting its minimum possible value.
                    double fitness = result.Fitness;
                    double fitnessAdjusted = fitness - Optimizer.MinFitness;

                    // Ensure adjusted fitness is non-negative, otherwise Preemptive
                    // Fitness Evaluation does not work.
                    Debug.Assert(fitnessAdjusted >= 0);

                    // Apply weight to the adjusted fitness.
                    fitnessAdjusted *= weight;

                    // Accumulate inner fitness sum.
                    fitnessSumInner += fitnessAdjusted;

                    // Accumulate outer fitness sum. (Thread-safe.)
                    // It is OK to use a lock here because the majority of the execution
                    // time will occur in the Optimizer.Optimize() above and the locked
                    // part is negligible in comparison.
                    lock (fitnessSumLock)
                    {
                        fitnessSum += fitnessAdjusted;

                        if (fitnessSum > fitnessLimit)
                        {
                            // It should be safe to use Stop() instead of Break()
                            // because the computations can be stopped part-way
                            // through without loss of data.
                            // Experiments indicate that using Stop() saves 10%
                            // more computation time than Break()

                            loopState.Stop();
                        }
                    }
                });

                // Set the fitness result achieved on the problem.
                // This was why we needed an extra summation variable.
                ProblemIndex.SetFitness(i, fitnessSumInner);
            }

            // Sort the optimization problems so that the worst
            // performing will be attempted optimized first, when
            // this method is called again.
            ProblemIndex.Sort();

            return fitnessSum;
        }
        #endregion
    }
}
