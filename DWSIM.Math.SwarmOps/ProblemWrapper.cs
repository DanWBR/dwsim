/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

namespace SwarmOps
{
    /// <summary>
    /// Transparently wrap a Problem-object.
    /// </summary>
    public abstract class ProblemWrapper : Problem
    {
        #region Constructors.
        /// <summary>
        /// Create the object.
        /// </summary>
        /// <param name="problem">Problem-object to be wrapped.</param>
        public ProblemWrapper(Problem problem)
            : base()
        {
            Problem = problem;
        }
        #endregion

        #region Public fields.
        /// <summary>
        /// The problem that is being wrapped.
        /// </summary>
        public Problem Problem
        {
            get;
            private set;
        }
        #endregion

        #region Problem base-class overrides.
        /// <summary>
        /// Return Name of the wrapped problem.
        /// </summary>
        public override string Name
        {
            get { return Problem.Name; }
        }

        /// <summary>
        /// Return LowerBound of wrapped problem.
        /// </summary>
        public override double[] LowerBound
        {
            get { return Problem.LowerBound; }
        }

        /// <summary>
        /// Return UpperBound of wrapped problem.
        /// </summary>
        public override double[] UpperBound
        {
            get { return Problem.UpperBound; }
        }

        /// <summary>
        /// Return LowerInit of wrapped problem.
        /// </summary>
        public override double[] LowerInit
        {
            get { return Problem.LowerInit; }
        }

        /// <summary>
        /// Return UpperInit of wrapped problem.
        /// </summary>
        public override double[] UpperInit
        {
            get { return Problem.UpperInit; }
        }

        /// <summary>
        /// Return Dimensionality of wrapped problem.
        /// </summary>
        public override int Dimensionality
        {
            get { return Problem.Dimensionality; }
        }

        /// <summary>
        /// Return MinFitness of wrapped problem.
        /// </summary>
        public override double MinFitness
        {
            get { return Problem.MinFitness; }
        }

        /// <summary>
        /// Return MaxFitness of wrapped problem.
        /// </summary>
        public override double MaxFitness
        {
            get { return Problem.MaxFitness; }
        }

        /// <summary>
        /// Return AcceptableFitness of wrapped problem.
        /// </summary>
        public override double AcceptableFitness
        {
            get { return Problem.AcceptableFitness; }
        }

        /// <summary>
        /// Return ParameterName of wrapped problem.
        /// </summary>
        public override string[] ParameterName
        {
            get { return Problem.ParameterName; }
        }

        /// <summary>
        /// Return HasGradient of wrapped problem.
        /// </summary>
        public override bool HasGradient
        {
            get { return Problem.HasGradient; }
        }

        /// <summary>
        /// Compute the gradient of the wrapped problem.
        /// </summary>
        /// <param name="x">Candidate solution.</param>
        /// <param name="v">Array for holding the gradient.</param>
        /// <returns>
        /// Computation time-complexity factor. E.g. if fitness takes
        /// time O(n) to compute and gradient takes time O(n*n) to compute,
        /// then return n.
        /// </returns>
        public override int Gradient(double[] x, ref double[] v)
        {
            return Problem.Gradient(x, ref v);
        }

        /// <summary>
        /// Enforce constraints and evaluate feasiblity of the wrapped problem.
        /// </summary>
        /// <param name="parameters">Candidate solution.</param>
        public override bool EnforceConstraints(ref double[] parameters)
        {
            return Problem.EnforceConstraints(ref parameters);
        }

        /// <summary>
        /// Evaluate feasibility (constraint satisfaction) of the wrapped problem.
        /// </summary>
        /// <param name="parameters">Candidate solution.</param>
        public override bool Feasible(double[] parameters)
        {
            return Problem.Feasible(parameters);
        }

        /// <summary>
        /// Propagate signal to wrapped problem.
        /// </summary>
        public override void BeginOptimizationRun()
        {
            Problem.BeginOptimizationRun();
        }

        /// <summary>
        /// Propagate signal to wrapped problem.
        /// </summary>
        public override void EndOptimizationRun()
        {
            Problem.EndOptimizationRun();
        }

        /// <summary>
        /// Return whether optimization of wrapped problem is allowed to continue.
        /// </summary>
        /// <param name="iterations">Number of iterations performed in optimization run.</param>
        /// <param name="fitness">Best fitness found in optimization run.</param>
        /// <param name="feasible">Feasibility of best found candidate solution.</param>
        public override bool Continue(int iterations, double fitness, bool feasible)
        {
            return Problem.Continue(iterations, fitness, feasible);
        }
        #endregion
    }
}
