/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

namespace SwarmOps
{
    /// <summary>
    /// Problem and weight pair for use in meta-optimization.
    /// </summary>
    public class WeightedProblem
    {
        #region Constructors.
        /// <summary>
        /// Construct the object, weight will be set to 1.0
        /// </summary>
        /// <param name="problem">Problem object.</param>
        public WeightedProblem(Problem problem)
            : this(1.0, problem)
        {
        }

        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="weight">Weight.</param>
        /// <param name="problem">Problem object.</param>
        public WeightedProblem(double weight, Problem problem)
        {
            Weight = weight;
            Problem = problem;
        }
        #endregion

        #region Public fields.
        /// <summary>
        /// Problem object.
        /// </summary>
        public Problem Problem
        {
            get;
            protected set;
        }

        /// <summary>
        /// Weight.
        /// </summary>
        public double Weight
        {
            get;
            protected set;
        }
        #endregion
    }
}
