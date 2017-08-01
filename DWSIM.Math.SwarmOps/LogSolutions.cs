/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Collections.Generic;

namespace SwarmOps
{
    /// <summary>
    /// Log best solutions found during optimization, that is, log
    /// parameters and their associated fitness. Transparently wraps
    /// around a problem-object.
    /// </summary>
    public class LogSolutions : ProblemWrapper
    {
        #region Constructors.
        /// <summary>
        /// Create the object.
        /// </summary>
        /// <param name="problem">Problem-object to be wrapped.</param>
        /// <param name="capacity">Log capacity.</param>
        /// <param name="onlyFeasible">Only log feasible solutions.</param>
        public LogSolutions(Problem problem, int capacity, bool onlyFeasible)
            : base(problem)
        {
            Capacity = capacity;
            OnlyFeasible = onlyFeasible;

            Log = new List<Solution>(capacity);
        }
        #endregion

        #region Public fields.
        /// <summary>
        /// Maximum capacity of log.
        /// </summary>
        public int Capacity
        {
            get;
            private set;
        }

        /// <summary>
        /// Only log feasible solutions.
        /// </summary>
        public bool OnlyFeasible
        {
            get;
            private set;
        }

        /// <summary>
        /// Log of candidate solutions.
        /// </summary>
        public List<Solution> Log
        {
            get;
            private set;
        }
        #endregion

        #region Public methods.
        /// <summary>
        /// Clear the log.
        /// </summary>
        public void Clear()
        {
            Log.Clear();
        }
        #endregion

        #region Problem base-class overrides.
        /// <summary>
        /// Return the name of the problem.
        /// </summary>
        public override string Name
        {
            get { return "LogSolutions (" + Problem.Name + ")"; }
        }

        /// <summary>
        /// Compute the fitness measure by passing the
        /// given parameters to the wrapped problem, and if
        /// candidate solution is an improvement then log
        /// the results.
        /// </summary>
        /// <param name="parameters">Candidate solution.</param>
        /// <param name="fitnessLimit">Preemptive Fitness Limit</param>
        /// <param name="newFeasible">Feasibility of old candidate solution.</param>
        /// <param name="oldFeasible">Feasibility of new candidate solution.</param>
        /// <returns>Fitness value.</returns>
        public override double Fitness(double[] parameters, double fitnessLimit, bool oldFeasible, bool newFeasible)
        {
            double fitness = Problem.Fitness(parameters, fitnessLimit, oldFeasible, newFeasible);

            // Log solutions. If feasibiilty is required then only log feasible solutions.
            if (!OnlyFeasible || newFeasible)
            {
                // Log solutions with better fitness and feasibility.
                if (Tools.BetterFeasibleFitness(oldFeasible, newFeasible, fitnessLimit, fitness))
                {
                    // Ensure log does not exceed desired capacity.
                    if (Log.Count >= Capacity)
                    {
                        // Assume log is sorted in ascending (worsening) order.
                        // Remove worst from the log.
                        Log.RemoveAt(Log.Count - 1);
                    }

                    Solution candidateSolution = new Solution(parameters, fitness, newFeasible);

                    // Add new solution to the log.
                    Log.Add(candidateSolution);

                    // Sort log according to fitness.
                    // This could be implemented more efficiently
                    // but it is not crucial for runtime performance
                    // so this simple implementation is sufficient.
                    Log.Sort(new Solution.FitnessComparer());
                }
            }

            return fitness;
        }
        #endregion
    }
}
