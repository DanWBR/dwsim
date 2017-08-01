#region Copyright © 2009 Jose Antonio De Santiago-Castillo.

//Copyright © 2009 Jose Antonio De Santiago-Castillo 
//E-mail:JAntonioDeSantiago@gmail.com
//Web: www.DotNumerics.com
//
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;


namespace DotNumerics
{
    //[DebuggerDisplay("{_real} + {_imaginary} i")]
    /// <summary>
    /// Represents a Complex number.
    /// </summary>
    public struct Complex: IFormattable
    {

        #region Fields

        private double _real;
        private double _imaginary;

        #endregion


        #region Constructor

        /// <summary>
        /// Initializes a new instance of the Complex class.
        /// </summary>
        /// <param name="real">The real part of the Complex number.</param>
        /// <param name="imaginary">The imaginary part of the complex number. </param>
        [DebuggerStepThrough()]
        public Complex(double real, double imaginary)
        {
            this._real = real;
            this._imaginary = imaginary;
        }


        #endregion

        #region Properties

        /// <summary>
        /// Gets or sets the real value of the complex number.
        /// </summary>
        public double Real
        {
            get { return _real; }
            set { _real = value; }
        }

        /// <summary>
        /// Gets or sets the imaginary value of the complex number.
        /// </summary>
        public double Imaginary
        {
            get { return _imaginary; }
            set { _imaginary = value; }
        }

        #endregion


        #region Methods

        /// <summary>
        /// Returns the conjugate of this complex number. 
        /// </summary>
        [DebuggerBrowsableAttribute(DebuggerBrowsableState.Never)]
        public Complex Conjugate
        {
            [DebuggerStepThrough()]
            get { return new Complex(_real, -_imaginary); }
        }

        ///// <summary>
        ///// 
        ///// </summary>
        //[DebuggerBrowsableAttribute(DebuggerBrowsableState.Never)]
        //public double Norm
        //{
        //    [DebuggerStepThrough()]
        //    get { return _real * _real + _imaginary * _imaginary; }
        //}

        /// <summary>
        /// Gets the modulus or absolute value of this complex number. 
        /// </summary>
        [DebuggerBrowsableAttribute(DebuggerBrowsableState.Never)]
        public double Modulus
        {
            [DebuggerStepThrough()]
            get { return System.Math.Sqrt(_real * _real + _imaginary * _imaginary); }
        }

        /// <summary>
        /// Gets or stes the argument of a this complex number. 
        /// </summary>
        [DebuggerBrowsableAttribute(DebuggerBrowsableState.Never)]
        public double Argument
        {
            [DebuggerStepThrough()]
            get { return System.Math.Atan2(_imaginary, _real); }
            set
            {
                double modulus = Modulus;
                this._real = Math.Cos(value) * modulus;
                this._imaginary = Math.Sin(value) * modulus;
            }
        }

        //[DebuggerStepThrough()]
        //public static Complex Polar(double modulus, double argument)
        //{
        //    return new Complex(
        //       modulus * System.Math.Cos(argument),
        //       modulus * System.Math.Sin(argument));
        //}

        #endregion

        /// <summary>
        /// Complex addition.
        /// </summary>
        /// <param name="z1">The left side of the addition operator.</param>
        /// <param name="z2">The right side matrix of the addition operator.</param>
        /// <returns>A value that represents the result of the addition.</returns>
        public static Complex operator +(Complex z1, Complex z2)
        {
            return new Complex(z1._real + z2._real, z1._imaginary + z2._imaginary);
        }

        /// <summary>
        /// Unary addition.
        /// </summary>
        /// <param name="c">The value.</param>
        /// <returns>The value.</returns>
        public static Complex operator +(Complex c)
        {
            return c;
        }

        /// <summary>
        /// Unary minus.
        /// </summary>
        /// <param name="c">The value</param>
        /// <returns> -value</returns>
        public static Complex operator -(Complex c)
        {
            return new Complex(-c.Real, -c.Imaginary);
        }

        /// <summary>
        /// Complex subtraction.
        /// </summary>
        /// <param name="z1">The left side of the operator.</param>
        /// <param name="z2">The right side of the operator.</param>
        /// <returns>A value that represents the result of the operation.</returns>
        public static Complex operator -(Complex z1, Complex z2)
        {
            return new Complex(z1._real - z2._real, z1._imaginary - z2._imaginary);
        }


        /// <summary>
        /// Complex multiplication.
        /// </summary>
        /// <param name="z1">The left side of the operator.</param>
        /// <param name="z2">The right side of the operator.</param>
        /// <returns>A value that represents the result of the operation.</returns>
        public static Complex operator *(Complex z1, Complex z2)
        {
            return new Complex(
               z1._real * z2._real - z1._imaginary * z2._imaginary,
               z1._real * z2._imaginary + z1._imaginary * z2._real);
        }

        /// <summary>
        /// Complex multiplication.
        /// </summary>
        /// <param name="z1">The left side of the operator.</param>
        /// <param name="z2">The right side of the operator.</param>
        /// <returns>A value that represents the result of the operation.</returns>
        public static Complex operator *(double d1, Complex z2)
        {
            return new Complex(d1 * z2._real, d1 * z2._imaginary);
        }

        /// <summary>
        /// Complex multiplication.
        /// </summary>
        /// <param name="z1">The left side of the operator.</param>
        /// <param name="z2">The right side of the operator.</param>
        /// <returns>A value that represents the result of the operation.</returns>
        public static Complex operator *(Complex z1, double d2)
        {
            return d2 * z1;
        }

        /// <summary>
        /// Complex division.
        /// </summary>
        /// <param name="z1">The left side of the operator.</param>
        /// <param name="z2">The right side of the operator.</param>
        /// <returns>A value that represents the result of the operation.</returns>
        public static Complex operator /(Complex z1, Complex z2)
        {
            double value = z2._real * z2._real + z2._imaginary * z2._imaginary;

            return new Complex(
               (z1._real * z2._real + z1._imaginary * z2._imaginary) / value,
               (z1._imaginary * z2._real - z1._real * z2._imaginary) / value);
        }

        /// <summary>
        /// Equality operator.
        /// </summary>
        /// <param name="z1">The left side of the operator.</param>
        /// <param name="z2">The right side of the operator.</param>
        /// <returns>A value that represents the result of the operation.</returns>
        public static bool operator ==(Complex z1, Complex z2)
        {
            return (z1._real == z2._real && z1._imaginary == z2._imaginary);
        }

        /// <summary>
        /// Inequality operator.
        /// </summary>
        /// <param name="z1">The left side of the operator.</param>
        /// <param name="z2">The right side of the operator.</param>
        /// <returns>A value that represents the result of the operation.</returns>
        public static bool operator !=(Complex z1, Complex z2)
        {
            return (z1._real != z2._real || z1._imaginary != z2._imaginary);
        }

        /// <summary>
        /// Indicates whether this instance and a specific object are equals.
        /// </summary>
        /// <param name="obj">Another object to compare to.</param>
        /// <returns>true if obj and this instance are the same type and represent the same value;
        /// otherwise, false.</returns>
        public override bool Equals(object obj)
        {
            return base.Equals(obj);
        }

        /// <summary>
        /// Returns the hash code for this instance.
        /// </summary>
        /// <returns>A 32-bit signed integer that is the hash code for this instance.</returns>
        public override int GetHashCode()
        {
            return _real.GetHashCode() ^ _imaginary.GetHashCode();
        }



        /// <summary>
        /// Converts this instance to its equivalent string representation.
        /// </summary>
        /// <returns>The string representation.</returns>
        public override string ToString()
        {
            //return (String.Format("{0} + {1}i", _real, _imaginary));
            return (_imaginary >= 0) ? _real.ToString() + " +" + _imaginary.ToString() + " i" : _real.ToString() + " " + _imaginary.ToString() + " i";
        }

        /// <summary>
        /// Converts this instance to its equivalent string representation,
        /// using the specified format.
        /// </summary>
        /// <param name="format">A numeric format string.</param>
        /// <returns>The string representation of the value of this instance as specified by format.</returns>
        public string ToString(string format)
        {
            //return (String.Format("{0} + {1}i", _real, _imaginary));
            return (_imaginary >= 0) ? _real.ToString(format) + " +" + _imaginary.ToString(format) + " i" : _real.ToString(format) + " " + _imaginary.ToString(format) + " i";
        }



        #region IFormattable Members

        /// <summary>
        ///  Converts the numeric value of this instance to its equivalent string representation
        ///  using the specified format and culture-specific format information.
        /// </summary>
        /// <param name="format">
        /// A numeric format string.
        /// </param>
        /// <param name="formatProvider">
        /// An System.IFormatProvider that supplies culture-specific formatting information.
        /// </param>
        /// <returns>
        /// The string representation of the value of this instance as specified by format and provider.
        /// </returns>
        public string ToString(string format, IFormatProvider formatProvider)
        {
            string s = "";
            if (_imaginary >= 0)
            {
                s = _real.ToString(format, formatProvider) + " +" + _imaginary.ToString(format, formatProvider) + " i";
            }
            else
            {
                s = _real.ToString(format, formatProvider) + " " + _imaginary.ToString(format, formatProvider) + " i";
            }
            return s;
        }

        #endregion
    }
}
