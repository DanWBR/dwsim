/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System;

namespace SwarmOps
{
    /// <summary>
    /// Base-class for an optimization problem.
    /// </summary>
    public abstract class Problem
    {
        #region Constructors.
        /// <summary>
        /// Create the object.
        /// </summary>
        public Problem() :
            this(0, true)
        {
        }

        /// <summary>
        /// Create the object.
        /// </summary>
        /// <param name="maxIterations">Max optimization iterations to perform.</param>
        public Problem(int maxIterations) :
            this(maxIterations, true)
        {
        }

        /// <summary>
        /// Create the object.
        /// </summary>
        /// <param name="maxIterations">Max optimization iterations to perform.</param>
        /// <param name="requireFeasible">Require solution to be feasible (satisfy constraints.)</param>
        public Problem(int maxIterations, bool requireFeasible)
        {
            MaxIterations = maxIterations;
            RequireFeasible = requireFeasible;
        }
        #endregion

        #region Public fields.
        /// <summary>
        /// Maximum number of optimization iterations to perform.
        /// </summary>
        public int MaxIterations
        {
            get;
            set;
        }

        /// <summary>
        /// Require solution is feasible (satisfies constraints).
        /// </summary>
        public bool RequireFeasible
        {
            get;
            set;
        }
        #endregion

        #region Public fields, override these.
        /// <summary>
        /// Return name of the optimization problem.
        /// </summary>
        public abstract string Name
        {
            get;
        }

        /// <summary>
        /// Array with names of parameters.
        /// </summary>
        public virtual string[] ParameterName
        {
            get { return null; }
        }

        /// <summary>
        /// Lower search-space boundary.
        /// </summary>
        public abstract double[] LowerBound
        {
            get;
        }

        /// <summary>
        /// Upper search-space boundary.
        /// </summary>
        public abstract double[] UpperBound
        {
            get;
        }

        /// <summary>
        /// Lower initialization boundary,
        /// if different from search-space boundary.
        /// </summary>
        public virtual double[] LowerInit
        {
            get { return LowerBound; }
        }

        /// <summary>
        /// Upper initialization boundary,
        /// if different from search-space boundary.
        /// </summary>
        public virtual double[] UpperInit
        {
            get { return UpperBound; }
        }

        /// <summary>
        /// Maximum (i.e. worst) fitness possible.
        /// </summary>
        public virtual double MaxFitness
        {
            get { return double.MaxValue; }
        }

        /// <summary>
        /// Minimum (i.e. best) fitness possible. This is
        /// especially important if using meta-optimization
        /// where the Fitness is assumed to be non-negative,
        /// and should be roughly equivalent amongst all the
        /// problems meta-optimized for.
        /// </summary>
        public abstract double MinFitness
        {
            get;
        }

        /// <summary>
        /// Threshold for an acceptable fitness value.
        /// </summary>
        public virtual double AcceptableFitness
        {
            get { return MinFitness; }
        }

        /// <summary>
        /// Threshold for an acceptable tolerance.
        /// </summary>
        public double Tolerance
        {
            get;
            set;
        }

        /// <summary>
        /// Minimum number of iterations.
        /// </summary>
        public double MinIterations
        {
            get;
            set;
        }

        /// <summary>
        /// Previous fitness value.
        /// </summary>
        public virtual double PreviousFitnessValue
        {
            get;
            set;
        }

        /// <summary>
        /// Return dimensionality of the problem, that is, the number
        /// of parameters in a candidate solution.
        /// </summary>
        public abstract int Dimensionality
        {
            get;
        }

        /// <summary>
        /// Has the gradient has been implemented?
        /// </summary>
        public virtual bool HasGradient
        {
            get { return false; }
        }
        #endregion

        #region Public methods, override these.
        /// <summary>
        /// Compute and return fitness for the given parameters.
        /// </summary>
        /// <param name="parameters">Candidate solution.</param>
        public virtual double Fitness(double[] parameters)
        {
            return Fitness(parameters, true);
        }

        /// <summary>
        /// Compute and return fitness for the given parameters.
        /// The fitness evaluation is aborted preemptively, if the
        /// fitness becomes higher (i.e. worse) than fitnessLimit, and
        /// if it is not possible for the fitness to improve.
        /// </summary>
        /// <param name="parameters">Candidate solution.</param>
        /// <param name="fitnessLimit">Preemptive Fitness Limit.</param>
        public virtual double Fitness(double[] parameters, double fitnessLimit)
        {
            return Fitness(parameters);
        }

        /// <summary>
        /// Compute and return fitness for the given parameters.
        /// The fitness evaluation is aborted preemptively if
        /// feasibility of the new candidate solution is same or better
        /// as that of the old candidate solution, and if the
        /// fitness becomes higher (i.e. worse) than fitnessLimit and
        /// if it is not possible for the fitness to improve.
        /// </summary>
        /// <param name="parameters">Candidate solution.</param>
        /// <param name="fitnessLimit">Preemptive Fitness Limit.</param>
        /// <param name="newFeasible">Feasibility of old candidate solution.</param>
        /// <param name="oldFeasible">Feasibility of new candidate solution.</param>
        public virtual double Fitness(double[] parameters, double fitnessLimit, bool oldFeasible, bool newFeasible)
        {
            double fitness;

            if (Tools.BetterFeasible(oldFeasible, newFeasible))
            {
                fitness = Fitness(parameters, fitnessLimit);
            }
            else
            {
                fitness = Fitness(parameters);
            }

            return fitness;
        }


        /// <summary>
        /// Compute and return fitness for the given parameters.
        /// </summary>
        /// <param name="parameters">Candidate solution.</param>
        /// <param name="feasible">Feasibility of candidate solution.</param>
        public virtual double Fitness(double[] parameters, bool feasible)
        {
            return Fitness(parameters, MaxFitness, feasible, feasible);
        }

        /// <summary>
        /// Compute the gradient of the fitness-function.
        /// </summary>
        /// <param name="x">Candidate solution.</param>
        /// <param name="v">Array for holding the gradient.</param>
        /// <returns>
        /// Computation time-complexity factor. E.g. if fitness takes
        /// time O(n) to compute and gradient takes time O(n*n) to compute,
        /// then return n.
        /// </returns>
        public virtual int Gradient(double[] x, ref double[] v)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Enforce constraints and evaluate feasiblity.
        /// </summary>
        /// <remarks>
        /// If you do not wish to enforce constraints you should make
        /// this call Feasible().
        /// </remarks>
        /// <param name="parameters">Candidate solution.</param>
        public virtual bool EnforceConstraints(ref double[] parameters)
        {
            // By default we bound the candidate solution to the search-space boundaries.
            Tools.Bound(ref parameters, LowerBound, UpperBound);

            // Since we know that candidate solution is now within bounds and this is
            // all that is required for feasibility, we could just return true here.
            // Feasible() is called for educational purposes and because most optimizers
            // do not call Feasible() but call EnforceConstraints() so if the user were
            // to only override Feasible() constraint handling would not work as expected.
            return Feasible(parameters);
        }

        /// <summary>
        /// Evaluate feasibility (constraint satisfaction).
        /// </summary>
        /// <param name="parameters">Candidate solution.</param>
        public virtual bool Feasible(double[] parameters)
        {
            return Tools.BetweenBounds(parameters, LowerBound, UpperBound);
        }

        /// <summary>
        /// Called at the beginning of an optimization run.
        /// </summary>
        public virtual void BeginOptimizationRun()
        {
            // Do nothing by default.
        }

        /// <summary>
        /// Called at the end of an optimization run.
        /// </summary>
        public virtual void EndOptimizationRun()
        {
            // Do nothing by default.
        }

        /// <summary>
        /// Return whether optimization is allowed to continue.
        /// </summary>
        /// <param name="iterations">Number of iterations performed in optimization run.</param>
        /// <param name="fitness">Best fitness found in optimization run.</param>
        /// <param name="feasible">Feasibility of best found candidate solution.</param>
        public virtual bool Continue(int iterations, double fitness, bool feasible)
        {
            if (iterations <= MinIterations) return true;
            var comparison = (iterations < MaxIterations && Math.Abs((fitness - PreviousFitnessValue) / fitness) > Tolerance &&
                    !(fitness <= AcceptableFitness && (!RequireFeasible || feasible)));
            PreviousFitnessValue = fitness;
            return comparison;
        }
        #endregion
    }
}
