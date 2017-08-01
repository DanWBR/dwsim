/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

namespace SwarmOps
{
    /// <summary>
    /// Base-class for performing a number of optimization runs.
    /// </summary>
    public abstract class Repeat : Problem
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="optimizer">Optimizer to use.</param>
        /// <param name="numRuns">Number of optimization runs.</param>
        public Repeat(Optimizer optimizer, int numRuns)
            : base()
        {
            Optimizer = optimizer;
            NumRuns = numRuns;
        }
        #endregion

        #region Public fields.
        /// <summary>
        /// Optimizer to use.
        /// </summary>
        public Optimizer Optimizer
        {
            get;
            private set;
        }

        /// <summary>
        /// Number of optimization runs to perform.
        /// </summary>
        public int NumRuns
        {
            get;
            private set;
        }
        #endregion

        #region Public methods.
        /// <summary>
        /// Compute fitness using Optimizer's default parameters.
        /// </summary>
        /// <returns></returns>
        public double Fitness()
        {
            return Fitness(Optimizer.DefaultParameters);
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Return Name of Optimizer.
        /// </summary>
        public override string Name
        {
            get { return Optimizer.Name; }
        }

        /// <summary>
        /// Return LowerBound of Optimizer.
        /// </summary>
        public override double[] LowerBound
        {
            get { return Optimizer.LowerBound; }
        }

        /// <summary>
        /// Return UpperBound of Optimizer.
        /// </summary>
        public override double[] UpperBound
        {
            get { return Optimizer.UpperBound; }
        }

        /// <summary>
        /// Return LowerInit of Optimizer.
        /// </summary>
        public override double[] LowerInit
        {
            get { return Optimizer.LowerInit; }
        }

        /// <summary>
        /// Return UpperInit of Optimizer.
        /// </summary>
        public override double[] UpperInit
        {
            get { return Optimizer.UpperInit; }
        }

        /// <summary>
        /// Return Dimensionality of Optimizer.
        /// </summary>
        public override int Dimensionality
        {
            get { return Optimizer.Dimensionality; }
        }
        #endregion
    }
}
