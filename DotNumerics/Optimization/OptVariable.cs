using System;
using System.Collections.Generic;
using System.Text;

namespace DotNumerics.Optimization
{
    /// <summary>
    /// Represents a varaible that can be used in optimization classes.
    /// </summary>
    public class OptVariable
    {
        #region Fields

        /// <summary>
        /// The variable name.
        /// </summary>
        protected string _Name = "P";

        /// <summary>
        /// The initial guess for this variable.
        /// </summary>
        protected double _InitialGuess = 1;

        /// <summary>
        /// Value that indicates if the variable is fixed.
        /// </summary>
        protected bool _Fixed = false;

        #endregion

        #region Constructor

        /// <summary>
        /// Initializes a new instance of the OptVariable class.
        /// </summary>
        public OptVariable()
        {

        }

        /// <summary>
        /// Initializes a new instance of the OptVariable class.
        /// </summary>
        /// <param name="initialGuess">The initial guess.</param>
        public OptVariable(double initialGuess)
        {
            this._InitialGuess = initialGuess;
        }

        /// <summary>
        /// Initializes a new instance of the OptVariable class.
        /// </summary>
        /// <param name="name">The variable name.</param>
        /// <param name="initialGuess">The initial guess.</param>
        public OptVariable(string name, double initialGuess)
        {
            this._Name = name;
            this._InitialGuess = initialGuess;
        }

        /// <summary>
        /// Initializes a new instance of the OptVariable class.
        /// </summary>
        /// <param name="initialGuess">The initial guess.</param>
        /// <param name="isFixed">Value that indicates if the variable is fixed.</param>
        public OptVariable(double initialGuess, bool isFixed)
        {
            this._InitialGuess = initialGuess;
            this._Fixed = isFixed;
        }

        /// <summary>
        /// Initializes a new instance of the OptVariable class.
        /// </summary>
        /// <param name="name">The variable name.</param>
        /// <param name="initialGuess">The initial guess.</param>
        /// <param name="isFixed">Value that indicates if the variable is fixed.</param>
        public OptVariable(string name, double initialGuess, bool isFixed)
        {
            this._Name = name;
            this._InitialGuess = initialGuess;
            this._Fixed = isFixed;
        }

        #endregion


        #region Properties

        /// <summary>
        /// The variable name.
        /// </summary>
        public string Name
        {
            get { return _Name; }
            set { _Name = value; }
        }

        /// <summary>
        /// The initial guess for this variable.
        /// </summary>
        public double InitialGuess
        {
            get { return _InitialGuess; }
            set { _InitialGuess = value; }
        }

        /// <summary>
        /// Value that indicates if the variable is fixed.
        /// </summary>
        public bool Fixed
        {
            get { return _Fixed; }
            set { _Fixed = value; }
        }

        #endregion

        public override string ToString()
        {
            string s = "n: " + this._Name + ", ig: " + this.InitialGuess.ToString() + ", f: " + this.Fixed.ToString();
            return s;
        }



    }
}
