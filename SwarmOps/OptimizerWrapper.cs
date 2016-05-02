/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

namespace SwarmOps
{
    /// <summary>
    /// Transparently wrap an Optimizer-object.
    /// </summary>
    public abstract class OptimizerWrapper : Optimizer
    {
        #region Constructors.
        /// <summary>
        /// Create an OptimizerWrapper-object.
        /// </summary>
        /// <remarks>
        /// This is very similar to ProblemWrapper but C# does not allow
        /// for multiple inheritance and we need this class to inherit from
        /// Optimizer and therefore cannot make it inherit from ProblemWrapper
        /// as well.
        /// </remarks>
        /// <param name="optimizer">Optimizer-object being wrapped.</param>
        public OptimizerWrapper(Optimizer optimizer)
            : base()
        {
            Optimizer = optimizer;
        }
        #endregion

        #region Public fields.
        /// <summary>
        /// The optimizer that is being wrapped.
        /// </summary>
        public Optimizer Optimizer
        {
            get;
            private set;
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Return LowerBound of wrapped Optimizer.
        /// </summary>
        public override double[] LowerBound
        {
            get { return Optimizer.LowerBound; }
        }

        /// <summary>
        /// Return UpperBound of wrapped Optimizer.
        /// </summary>
        public override double[] UpperBound
        {
            get { return Optimizer.UpperBound; }
        }

        /// <summary>
        /// Return LowerInit of wrapped Optimizer.
        /// </summary>
        public override double[] LowerInit
        {
            get { return Optimizer.LowerInit; }
        }

        /// <summary>
        /// Return UpperInit of wrapped Optimizer.
        /// </summary>
        public override double[] UpperInit
        {
            get { return Optimizer.UpperInit; }
        }

        /// <summary>
        /// Return Dimensionality of wrapped Optimizer.
        /// </summary>
        public override int Dimensionality
        {
            get { return Optimizer.Dimensionality; }
        }

        /// <summary>
        /// Return MinFitness of wrapped Optimizer.
        /// </summary>
        public override double MinFitness
        {
            get { return Optimizer.MinFitness; }
        }

        /// <summary>
        /// Return MaxFitness of wrapped Optimizer.
        /// </summary>
        public override double MaxFitness
        {
            get { return Optimizer.MaxFitness; }
        }

        /// <summary>
        /// Return AcceptableFitness of wrapped Optimizer.
        /// </summary>
        public override double AcceptableFitness
        {
            get { return Optimizer.AcceptableFitness; }
        }

        /// <summary>
        /// Return ParameterName of wrapped Optimizer.
        /// </summary>
        public override string[] ParameterName
        {
            get { return Optimizer.ParameterName; }
        }

        /// <summary>
        /// Return DefaultParameters of wrapped Optimizer.
        /// </summary>
        public override double[] DefaultParameters
        {
            get { return Optimizer.DefaultParameters; }
        }

        /// <summary>
        /// Enforce constraints and evaluate feasiblity of the wrapped Optimizer.
        /// </summary>
        /// <param name="parameters">Control parameters.</param>
        public override bool EnforceConstraints(ref double[] parameters)
        {
            return Optimizer.EnforceConstraints(ref parameters);
        }

        /// <summary>
        /// Evaluate feasibility (constraint satisfaction) of the wrapped Optimizer.
        /// </summary>
        /// <param name="parameters">Control parameters.</param>
        public override bool Feasible(double[] parameters)
        {
            return Optimizer.Feasible(parameters);
        }

        /// <summary>
        /// Propagate signal to wrapped Optimizer.
        /// </summary>
        public override void BeginOptimizationRun()
        {
            Optimizer.BeginOptimizationRun();
        }

        /// <summary>
        /// Propagate signal to wrapped Optimizer.
        /// </summary>
        public override void EndOptimizationRun()
        {
            Optimizer.EndOptimizationRun();
        }

        /// <summary>
        /// Return whether optimization of wrapped Optimizer is allowed to continue.
        /// </summary>
        /// <param name="iterations">Number of iterations performed in optimization run.</param>
        /// <param name="fitness">Best fitness found in optimization run.</param>
        /// <param name="feasible">Feasibility of best found candidate solution.</param>
        public override bool Continue(int iterations, double fitness, bool feasible)
        {
            return Optimizer.Continue(iterations, fitness, feasible);
        }
        #endregion
    }
}
