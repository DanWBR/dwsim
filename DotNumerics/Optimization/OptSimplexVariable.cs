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
    /// Represents a varaible that can be used in a simplex optimization classe.
    /// </summary>
    public class OptSimplexVariable : OptVariable
    {

        #region Fields

        /// <summary>
        /// The scale factor controls the internal variable size. Variable=IntVar*ScaleFactor where IntVar is the internal variable. 
        /// Try to set the ScaleFactor of the same order that the variable, with this value the internal size will be near to 1 . 
        /// The default value is 1. This value modifies the changes and accuracy of this variable. 
        /// </summary>
        private double _ScaleFactor = 1;

        #endregion

        #region Constructor

        /// <summary>
        /// Initializes a new instance of the OptSimplexVariable class.
        /// </summary>
        public OptSimplexVariable()
        {

        }

        /// <summary>
        /// Initializes a new instance of the OptSimplexVariable class.
        /// </summary>
        /// <param name="initialGuess">The initial guess.</param>
        public OptSimplexVariable(double initialGuess)
        {
            this._InitialGuess = initialGuess;
        }

        /// <summary>
        /// Initializes a new instance of the OptSimplexVariable class.
        /// </summary>
        /// <param name="name">The variable name.</param>
        /// <param name="initialGuess">The initial guess.</param>
        public OptSimplexVariable(string name, double initialGuess)
        {
            this._Name = name;
            this._InitialGuess = initialGuess;
        }

        /// <summary>
        /// Initializes a new instance of the OptSimplexVariable class.
        /// </summary>
        /// <param name="initialGuess">The initial guess.</param>
        /// <param name="scaleFactor">A scale factor used to control the changes and accuracy of this variable.</param>
        public OptSimplexVariable(double initialGuess, double scaleFactor)
        {
            this._InitialGuess = initialGuess;
            this.ScaleFactor = scaleFactor;
        }

        /// <summary>
        /// Initializes a new instance of the OptSimplexVariable class.
        /// </summary>
        /// <param name="initialGuess">The initial guess.</param>
        /// <param name="isFixed">Value that indicates if the variable is fixed.</param>
        public OptSimplexVariable(double initialGuess, bool isFixed)
        {
            this._InitialGuess = initialGuess;
            this._Fixed = isFixed;
        }

        /// <summary>
        /// Initializes a new instance of the OptSimplexVariable class.
        /// </summary>
        /// <param name="name">The variable name.</param>
        /// <param name="initialGuess">The initial guess.</param>
        /// <param name="isFixed">Value that indicates if the variable is fixed.</param>
        public OptSimplexVariable(string name, double initialGuess, bool isFixed)
        {
            this._Name = name;
            this._InitialGuess = initialGuess;
            this._Fixed = isFixed;
        }

        /// <summary>
        /// Initializes a new instance of the OptSimplexVariable class.
        /// </summary>
        /// <param name="initialGuess">The initial guess.</param>
        /// <param name="isFixed">Value that indicates if the variable is fixed.</param>
        /// <param name="scaleFactor">A scale factor used to control the changes and accuracy of this variable.</param>
        public OptSimplexVariable(double initialGuess, bool isFixed , double scaleFactor)
        {
            this._InitialGuess = initialGuess;
            this._Fixed = isFixed;
            this.ScaleFactor = scaleFactor;
        }

        /// <summary>
        /// Initializes a new instance of the OptSimplexVariable class.
        /// </summary>
        /// <param name="name">The variable name.</param>
        /// <param name="initialGuess">The initial guess.</param>
        /// <param name="isFixed">Value that indicates if the variable is fixed.</param>
        /// <param name="scaleFactor">A scale factor used to control the changes and accuracy of this variable.</param>
        public OptSimplexVariable(string name, double initialGuess, bool isFixed, double scaleFactor)
        {
            this._Name = name;
            this._InitialGuess = initialGuess;
            this._Fixed = isFixed;
            this.ScaleFactor = scaleFactor;
        }

        #endregion


        #region Properties

        /// <summary>
        /// The scale factor controls the internal variable size. Variable=IntVar*ScaleFactor where IntVar is the internal variable. 
        /// Try to set the ScaleFactor of the same order that the variable, with this value the internal size will be near to 1 . 
        /// The default value is 1. This value modifies the changes and accuracy of this variable. 
        /// </summary>
        public double ScaleFactor
        {
            get { return _ScaleFactor; }
            set
            {
                if (value <= 0)
                {
                    value = 1;
                }
                _ScaleFactor = value;
            }
        }

        #endregion

        public override string ToString()
        {
            string s = "n: " + this._Name + ", ig: " + this.InitialGuess.ToString() + ", f: " + this.Fixed.ToString() + ", s: " + this._ScaleFactor.ToString();
            return s;
        }

    }
}
