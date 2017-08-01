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
    /// Prints parameters and fitness to Console. Useful in
    /// viewing the progress of meta-optimization. Works as
    /// a 'transparent' wrapper for the problem to be optimized.
    /// Note that feasibility is computed here.
    /// </summary>
    public class FitnessPrint : ProblemWrapper
    {
        #region Constructors.
        /// <summary>
        /// Constructs a new object.
        /// </summary>
        /// <param name="problem">The problem being wrapped.</param>
        public FitnessPrint(Problem problem)
            : this(problem, false)
        {
        }

        /// <summary>
        /// Constructs a new object.
        /// </summary>
        /// <param name="problem">The problem being wrapped.</param>
        /// <param name="formatAsArray">Format output string as C# array.</param>
        public FitnessPrint(Problem problem, bool formatAsArray)
            : base(problem)
        {
            FormatAsArray = formatAsArray;
        }
        #endregion

        #region Public fields.
        /// <summary>
        /// Format output string as C# array.
        /// </summary>
        public bool FormatAsArray
        {
            get;
            set;
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Return name of the optimization problem.
        /// </summary>
        public override string Name
        {
            get { return "FitnessPrint (" + Problem.Name + ")"; }
        }

        /// <summary>
        /// Compute fitness of wrapped problem and print the result.
        /// </summary>
        public override double Fitness(double[] parameters, double fitnessLimit, bool oldFeasible, bool newFeasible)
        {
            double fitness = Problem.Fitness(parameters, fitnessLimit);

            Tools.PrintSolution(parameters, fitness, fitnessLimit, oldFeasible, newFeasible, FormatAsArray);

            return fitness;
        }

        /// <summary>
        /// At beginning of new optimization run print a newline.
        /// </summary>
        public override void BeginOptimizationRun()
        {
            Tools.PrintNewline();
        }
        #endregion
    }
}
