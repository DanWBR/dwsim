/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System;
using System.Collections.Generic;

namespace SwarmOps
{
    /// <summary>
    /// Candidate solution found during optimization, consisting of parameters
    /// and fitness value.
    /// </summary>
    public class Solution
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="parameters">Candidate solution parameters.</param>
        /// <param name="fitness">Fitness for candidate solution.</param>
        /// <param name="feasible">Feasibility of candidate solution.</param>
        public Solution(double[] parameters, double fitness, bool feasible)
        {
            Parameters = parameters.Clone() as double[];
            Fitness = fitness;
            Feasible = feasible;
        }
        #endregion

        #region Public fields.
        /// <summary>
        /// Candidate solution parameters.
        /// </summary>
        public double[] Parameters
        {
            get;
            protected set;
        }

        /// <summary>
        /// Fitness of candidate solution.
        /// </summary>
        public double Fitness
        {
            get;
            protected set;
        }

        /// <summary>
        /// Feasibility of candidate solution.
        /// </summary>
        public bool Feasible
        {
            get;
            protected set;
        }
        #endregion

        #region Comparer.
        /// <summary>
        /// Comparer used for sorting a list of Solution-objects
        /// according to fitness and feasibility in ascending (worsening) order.
        /// </summary>
        public class FitnessComparer : IComparer<Solution>
        {
            public int Compare(Solution x, Solution y)
            {
                int retVal;

                if (x.Fitness == y.Fitness && x.Feasible == y.Feasible)
                {
                    retVal = 0;
                }
                else if (Tools.BetterFeasibleFitness(x.Feasible, y.Feasible, x.Fitness, y.Fitness))
                {
                    retVal = 1;
                }
                else
                {
                    retVal = -1;
                }

                return retVal;
            }
        }
        #endregion
    }
}
