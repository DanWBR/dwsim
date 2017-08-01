/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

namespace SwarmOps
{
    /// <summary>
    /// Base-class for an optimizer.
    /// </summary>
    public abstract class Optimizer : Problem
    {
        #region Constructors.
        /// <summary>
        /// Construct the object. This does not set the Problem
        /// which has to be done before the optimizer is being run.
        /// </summary>
        public Optimizer()
            : base()
        {
        }

        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="problem">Problem to optimize.</param>
        public Optimizer(Problem problem)
            : base()
        {
            Problem = problem;
        }
        #endregion

        #region Public fields.
        /// <summary>
        /// Problem to be optimized.
        /// </summary>
        public Problem Problem
        {
            get;
            set;
        }

        /// <summary>
        /// Fitness-trace used for tracing the progress of optimization.
        /// </summary>
        public FitnessTrace FitnessTrace
        {
            get;
            set;
        }
        #endregion

        #region Public methods.
        /// <summary>
        /// Optimize using default parameters.
        /// </summary>
        public Result Optimize()
        {
            return Optimize(DefaultParameters);
        }
        #endregion

        #region Override these.
        /// <summary>
        /// Default control parameters for the optimizer.
        /// </summary>
        public abstract double[] DefaultParameters
        {
            get;
        }

        /// <summary>
        /// Perform one optimization run and return the best found solution.
        /// </summary>
        /// <param name="parameters">Control parameters for the optimizer.</param>
        public virtual Result Optimize(double[] parameters)
        {
            return Optimize(parameters, Problem.MaxFitness);
        }

        /// <summary>
        /// Perform one optimization run and return the best found solution.
        /// </summary>
        /// <param name="parameters">Control parameters for the optimizer.</param>
        /// <param name="fitnessLimit">Preemptive Fitness Limit</param>
        public virtual Result Optimize(double[] parameters, double fitnessLimit)
        {
            return Optimize(parameters);
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Return MinFitness for the Problem.
        /// </summary>
        public override double MinFitness
        {
            get { return Problem.MinFitness; }
        }

        /// <summary>
        /// Compute fitness by performing one optimization run.
        /// </summary>
        /// <param name="parameters">Control parameters for the optimizer.</param>
        /// <param name="fitnessLimit">Preemptive Fitness Limit</param>
        /// <returns>Fitness value.</returns>
        public override double Fitness(double[] parameters, double fitnessLimit)
        {
            Result result = Optimize(parameters, fitnessLimit);

            return result.Fitness;
        }
        #endregion

        #region Internal methods.
        /// <summary>
        /// Trace fitness progress of optimization run.
        /// </summary>
        /// <param name="iteration">Iteration number.</param>
        /// <param name="fitness">Best-found fitness for this optimization run.</param>
        protected void Trace(int iteration, double fitness, bool feasible)
        {
            if (FitnessTrace != null)
            {
                FitnessTrace.Add(iteration, fitness, feasible);
            }
        }
        #endregion
    }
}
