#region Copyright © 2009, De Santiago-Castillo JA. All rights reserved.

//Copyright © 2009 Jose Antonio De Santiago-Castillo 
//E-mail:JAntonioDeSantiago@gmail.com
//Web: www.DotNumerics.com
//
#endregion

using System;
using System.Collections.Generic;
using System.Text;

namespace DotNumerics.Optimization
{    
    /// <summary>
    /// Represents a varaible that can be used in optimization classes with bounded constrains.
    /// </summary>
    public class OptBoundVariable: OptVariable
    {

        #region Fields

        /// <summary>
        /// The lower bound.
        /// </summary>
        protected double _LowerBound = double.NegativeInfinity;

        /// <summary>
        /// The upper bound.
        /// </summary>
        protected double _UpperBound = double.PositiveInfinity;

        private bool _UseBounds = true;


        #endregion


        #region Constructor

        /// <summary>
        /// Initializes a new instance of the OptBoundVariable class.
        /// </summary>
        public OptBoundVariable()
        {

        }
        /// <summary>
        /// Initializes a new instance of the OptBoundVariable class.
        /// </summary>
        /// <param name="initialGuess">The initial guess.</param>
        public OptBoundVariable(double initialGuess)
        {
            this._InitialGuess = initialGuess;
        }

        /// <summary>
        /// Initializes a new instance of the OptBoundVariable class.
        /// </summary>
        /// <param name="name">The variable name.</param>
        /// <param name="initialGuess">The initial guess.</param>
        public OptBoundVariable(string name, double initialGuess)
        {
            this._Name = name;
            this._InitialGuess = initialGuess;
        }

        /// <summary>
        /// Initializes a new instance of the OptBoundVariable class.
        /// </summary>
        /// <param name="initialGuess">The initial guess.</param>
        /// <param name="isFixed">Value that indicates if the variable is fixed.</param>
        public OptBoundVariable(double initialGuess, bool isFixed)
        {
            this._InitialGuess = initialGuess;
            this._Fixed = isFixed;
        }

        /// <summary>
        /// Initializes a new instance of the OptBoundVariable class.
        /// </summary>
        /// <param name="name">The variable name.</param>
        /// <param name="initialGuess">The initial guess.</param>
        /// <param name="isFixed">Value that indicates if the variable is fixed.</param>
        public OptBoundVariable(string name, double initialGuess, bool isFixed)
        {
            this._Name = name;
            this._InitialGuess = initialGuess;
            this._Fixed = isFixed;
        }

        /// <summary>
        /// Initializes a new instance of the OptBoundVariable class.
        /// </summary>
        /// <param name="initialGuess">The initial guess.</param>
        /// <param name="lowerBound">The lower bound.</param>
        /// <param name="upperBound">The upper bound.</param>
        public OptBoundVariable(double initialGuess, double lowerBound, double upperBound)
        {
            this._InitialGuess = initialGuess;
            this._LowerBound = lowerBound;
            this._UpperBound = upperBound;
        }
        /// <summary>
        /// Initializes a new instance of the OptBoundVariable class.
        /// </summary>
        /// <param name="name">The variable name.</param>
        /// <param name="initialGuess">The initial guess.</param>
        /// <param name="lowerBound">The lower bound.</param>
        /// <param name="upperBound">The upper bound.</param>
        public OptBoundVariable(string name, double initialGuess, double lowerBound, double upperBound)
        {
            this._Name = name;
            this._InitialGuess = initialGuess;
            this._LowerBound = lowerBound;
            this._UpperBound = upperBound;
        }

        /// <summary>
        /// Initializes a new instance of the OptBoundVariable class.
        /// </summary>
        /// <param name="initialGuess">The initial guess.</param>
        /// <param name="isFixed">Value that indicates if the variable is fixed.</param>
        /// <param name="lowerBound">The lower bound.</param>
        /// <param name="upperBound">The upper bound.</param>
        public OptBoundVariable(double initialGuess, bool isFixed, double lowerBound, double upperBound)
        {
            this._InitialGuess = initialGuess;
            this._Fixed = isFixed;
            this._LowerBound = lowerBound;
            this._UpperBound = upperBound;
        }

        /// <summary>
        /// Initializes a new instance of the OptBoundVariable class.
        /// </summary>
        /// <param name="name">The variable name.</param>
        /// <param name="initialGuess">The initial guess.</param>
        /// <param name="isFixed">Value that indicates if the variable is fixed.</param>
        /// <param name="lowerBound">The lower bound.</param>
        /// <param name="upperBound">The upper bound.</param>
        public OptBoundVariable(string name, double initialGuess, bool isFixed, double lowerBound, double upperBound)
        {
            this._Name = name;
            this._InitialGuess = initialGuess;
            this._Fixed = isFixed;
            this._LowerBound = lowerBound;
            this._UpperBound = upperBound;
        }

        #endregion


        #region Properties

        /// <summary>
        /// The lower bound.
        /// </summary>
        public double LowerBound
        {
            get { return _LowerBound; }
            set { _LowerBound = value; }
        }

        /// <summary>
        /// The upper bound.
        /// </summary>
        public double UpperBound
        {
            get { return _UpperBound; }
            set { _UpperBound = value; }
        }

        #endregion

        #region Internal Properites

        internal bool UseBounds
        {
            get { return _UseBounds; }
            set { _UseBounds = value; }
        }

        #endregion

        #region Methods

        internal static OptBoundVariable[] GetClon(OptBoundVariable[] variables)
        {
            OptBoundVariable[] cloned = new OptBoundVariable[variables.Length];
            for (int i = 0; i < variables.Length; i++)
            {
                cloned[i] = variables[i].GetClon();
            }
            return cloned;
        }

        internal OptBoundVariable GetClon()
        {
            OptBoundVariable clon = new OptBoundVariable(this._Name, this._InitialGuess, this._Fixed, this._LowerBound, this._UpperBound);
            return clon;
        }


        public override string ToString()
        {
            string s = "n: " + this._Name + ", ig: " + this.InitialGuess.ToString() + ", f: " + this.Fixed.ToString() + ", l: " + this._LowerBound + ", u:" + this._UpperBound;
            return s;
        }


        #endregion
    }
}
