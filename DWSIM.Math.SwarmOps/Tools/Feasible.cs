/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System;
using System.Diagnostics;

namespace SwarmOps
{
    public static partial class Tools
    {
        /// <summary>
        /// Return whether feasibility (constraint satisfaction) of new candidate
        /// solution is same as or better than feasibility of old candidate solution.
        /// </summary>
        /// <param name="oldFeasible">Feasibility of old candidate solution.</param>
        /// <param name="newFeasible">Feasibility of new candidate solution.</param>
        public static bool BetterFeasible(bool oldFeasible, bool newFeasible)
        {
            return (!oldFeasible || newFeasible);
        }

        /// <summary>
        /// Return whether the new candidate solution is better than the old,
        /// depending on their feasibility (constraint satisfaction) and fitness.
        /// </summary>
        /// <param name="oldFeasible">Feasibility of old candidate solution.</param>
        /// <param name="newFeasible">Feasibility of new candidate solution.</param>
        /// <param name="oldFitness">Fitness of old candidate solution.</param>
        /// <param name="newFitness">Fitness of new candidate solution.</param>
        public static bool BetterFeasibleFitness(bool oldFeasible, bool newFeasible, double oldFitness, double newFitness)
        {
            return ((!oldFeasible && (newFeasible || newFitness < oldFitness)) ||
                    (oldFeasible && newFeasible && newFitness < oldFitness));
        }
    }
}
