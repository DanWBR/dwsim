/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Diagnostics;

namespace SwarmOps
{
    /// <summary>
    /// Performs a number of optimization runs and returns the
    /// sum of the fitnesses. Ignores feasibility (constraint
    /// satisfaction.)
    /// This allows for Preemptive Fitness Evaluation.
    /// </summary>
    public class RepeatSum : Repeat
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="optimizer">Optimizer to use.</param>
        /// <param name="numRuns">Number of optimization runs to perform.</param>
        public RepeatSum(Optimizer optimizer, int numRuns)
            : base(optimizer, numRuns)
        {
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Return problem-name.
        /// </summary>
        public override string Name
        {
            get { return "RepeatSum(" + Optimizer.Name + ")"; }
        }

        /// <summary>
        /// Return minimum fitness possible. This is zero as
        /// the fitness summation is normalized.
        /// </summary>
        public override double MinFitness
        {
            get { return 0; }
        }

        /// <summary>
        /// Compute the fitness by repeating a number of optimization runs
        /// and sum the fitnesses achieved in the runs.
        /// </summary>
        /// <param name="parameters">Parameters to use for the Optimizer.</param>
        /// <param name="fitnessLimit">Preemptive Fitness Limit</param>
        /// <returns>Fitness value.</returns>
        public override double Fitness(double[] parameters, double fitnessLimit)
        {
            // Initialize the fitnses sum.
            double fitnessSum = 0;

            // Perform a number of optimization runs.
            for (int i = 0; i < NumRuns && fitnessSum < fitnessLimit; i++)
            {
                // Perform one optimization run.
                Result result = Optimizer.Optimize(parameters, fitnessLimit-fitnessSum);

                // Compute the normalized fitness.
                double fitnessNormalized = result.Fitness - Optimizer.MinFitness;

                Debug.Assert(fitnessNormalized >= 0);

                // Accumulate the fitness sum.
                fitnessSum += fitnessNormalized;
            }

            return fitnessSum;
        }
        #endregion
    }
}
