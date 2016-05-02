/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace SwarmOps
{
    /// <summary>
    /// Used for sorting optimization problems so that those hardest
    /// to optimize are tried first. This is used in the MetaFitness
    /// class where Pre-emptive Fitness Evaluation seeks to abort the
    /// meta-fitness evaluation as early as possible.
    /// </summary>
    public class ProblemIndex
    {
        #region Constructors.
        /// <summary>
        /// Construct the ProblemIndex-object.
        /// </summary>
        /// <param name="problems">The problems to be indexed.</param>
        public ProblemIndex(Problem[] problems)
            : base()
        {
            Debug.Assert(problems.Length > 0);

            int numProblems = problems.Count();
            double weight = 1.0 / numProblems;

            Index = new List<ProblemFitness>(numProblems);

            foreach (Problem problem in problems)
            {
                Index.Add(new ProblemFitness(problem, weight));
            }
        }

        /// <summary>
        /// Construct the ProblemIndex-object.
        /// </summary>
        /// <param name="weightedProblems">The problems to be indexed.</param>
        public ProblemIndex(WeightedProblem[] weightedProblems)
            : base()
        {
            // Ensure array has elements.
            Debug.Assert(weightedProblems.Length > 0);

            Index = new List<ProblemFitness>(weightedProblems.Length);

            double weightSum = weightedProblems.Sum(o => o.Weight);

            foreach (WeightedProblem weightedProblem in weightedProblems)
            {
                Problem problem = weightedProblem.Problem;
                double weight = weightedProblem.Weight;
                double weightNormalized = weight / weightSum;

                // Ensure weight is positive.
                Debug.Assert(weight > 0);

                Index.Add(new ProblemFitness(problem, weightNormalized));
            }
        }
        #endregion

        #region Internal class.
        /// <summary>
        /// Associate an optimization problem with a fitness.
        /// </summary>
        class ProblemFitness
        {
            public ProblemFitness(Problem problem, double weight)
            {
                Problem = problem;
                Weight = weight;
                Fitness = problem.MaxFitness;
            }

            /// <summary>
            /// The optimization problem.
            /// </summary>
            public Problem Problem
            {
                get;
                private set;
            }

            /// <summary>
            /// Weight of the optimization problem.
            /// </summary>
            public double Weight
            {
                get;
                private set;
            }

            /// <summary>
            /// The fitness achieved when last optimizing that problem.
            /// </summary>
            public double Fitness
            {
                get;
                set;
            }

            public static int Compare(ProblemFitness x, ProblemFitness y)
            {
                Debug.Assert(x != null && y != null);

                return System.Math.Sign(y.Fitness - x.Fitness);
            }
        }
        #endregion

        #region Internal variables.
        /// <summary>
        /// The list of optimization problems sorted according to fitness.
        /// </summary>
        List<ProblemFitness> Index;
        #endregion

        #region Public fields.
        /// <summary>
        /// Return the number of problems.
        /// </summary>
        public int Count
        {
            get { return Index.Count; }
        }
        #endregion

        #region Public methods.
        /// <summary>
        /// Return the i'th optimization problem, sorted so that the
        /// problems with the worst fitness have lowest indices.
        /// </summary>
        public Problem GetProblem(int i)
        {
            return Index[i].Problem;
        }

        /// <summary>
        /// Return the weight associated with the i'th problem.
        /// </summary>
        public double GetWeight(int i)
        {
            return Index[i].Weight;
        }

        /// <summary>
        /// Set the fitness associated with the i'th problem.
        /// </summary>
        public void SetFitness(int i, double fitness)
        {
            Index[i].Fitness = fitness;
        }

        /// <summary>
        /// Sort the optimization problems according to their associated fitness.
        /// </summary>
        public void Sort()
        {
            Index.Sort(ProblemFitness.Compare);
        }
        #endregion
    }
}
