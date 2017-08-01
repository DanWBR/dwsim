#region Copyright © 2009, De Santiago-Castillo JA. All rights reserved.

//Copyright © 2009 Jose Antonio De Santiago-Castillo 
//E-mail:JAntonioDeSantiago@gmail.com
//Web: www.DotNumerics.com
//
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;
using System.IO;

namespace DotNumerics.LinearAlgebra
{
    /// <summary>
    /// Represents a Vector.
    /// </summary>
    [DebuggerDisplay(": {Type} , Length : {Length}", Name = "vector")]
    [DebuggerTypeProxy(typeof(VectorDebuggerDisplay))]
    public class Vector
    {

        #region Fields
        /// <summary>
        /// Los datos del vector
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        protected double[] _Data;

        //private int MeLength = 1;

        /// <summary>
        /// El tipo de vector.
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        protected VectorType _Type = VectorType.Column;

        #endregion

        #region Constructor

        /// <summary>
        /// Initializes a new instance of the Vector class of the given size.
        /// </summary>
        /// <param name="length">The vector length</param>
        public Vector(int length) : this(VectorType.Column, length) { }

        /// <summary>
        /// Initializes a new instance of the Vector class of the given size and type.
        /// </summary>
        /// <param name="type">The vector type</param>
        /// <param name="length">length">The vector length</param>
        public Vector(VectorType type, int length)
        {
            if (length < 1) throw new System.ArgumentException("length < 1");
            this._Type = type;
            this._Data = new double[length];
        }

        /// <summary>
        /// Initializes a new instance of the Vector class that contains elements 
        /// copied from the specified array.
        /// </summary>
        /// <param name="data">The array whose elements are copied to the vector.</param>
        public Vector(double[] data) : this(VectorType.Column, data) { }

        /// <summary>
        /// Initializes a new instance of the Vector class that contains elements
        /// copied from the specified array.
        /// </summary>
        /// <param name="type">The vector type</param>
        /// <param name="data">The array whose elements are copied to the vector.</param>
        public Vector(VectorType type, double[] data)
        {
            if (data.Length < 1) throw new System.ArgumentException("data.Length < 1");
            this._Type = type;
            this._Data = new double[data.Length];

            data.CopyTo(this._Data, 0);

            //for (int i = 0; i < data.Length; i++)
            //{
            //    this.MeData[i] = data[i];
            //}
        }

        #endregion

        #region Public Properties

        /// <summary>
        /// Los datos del vector
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        internal double[] Data
        {
            get { return this._Data; }
        }

        /// <summary>
        /// Returns the number of elements.
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        public int Length
        {
            get { return this._Data.Length; }
        }

        /// <summary>
        /// The vector type.
        /// </summary>
        public VectorType Type
        {
            get { return this._Type; }
            set { this._Type = value; }
        }

        /// <summary>
        /// Gets or sets the element at the specified index.
        /// </summary>
        /// <param name="index">The zero-based index of the element to get or set.</param>
        /// <returns>The element at the specified index.</returns>
        public virtual double this[int index]
        {
            get
            {
                return this._Data[index];
            }
            set
            {
                this._Data[index] = value;
            }
        }

        #endregion

        #region Operators


        /// <summary>
        /// Vector addition.
        /// </summary>
        /// <param name="A">The left side vector of the addition operator.</param>
        /// <param name="B">The right side vector of the addition operator.</param>
        /// <returns>A vector that represents the result of the addition.</returns>
        public static Vector operator +(Vector A, Vector B)
        {
            return A.Add(B);
        }


        /// <summary>
        /// Unary minus.
        /// </summary>
        /// <param name="v">The vector.</param>
        /// <returns>Vector r[i] = -this[i]</returns>
        public static Vector operator -(Vector v)
        {
            return v.UnaryMinus();
        }


        /// <summary>
        /// Vector subtraction.
        /// </summary>
        /// <param name="A"> The left side vector of the subtraction operator.</param>
        /// <param name="B">The right side vector of the subtraction operator.</param>
        /// <returns>A vector that represents the result of the vector subtraction.</returns>
        public static Vector operator -(Vector A, Vector B)
        {
            return A.Subtract(B);
        }


        /// <summary>
        /// Scalar-Vector multiplication.
        /// </summary>
        /// <param name="s"> The left side scalar of the multiplication operator.</param>
        /// <param name="A">The right side vector of the multiplication operator.</param>
        /// <returns>A vector that represents the result of the multiplication.</returns>
        public static Vector operator *(double s, Vector A)
        {
            return A.Multiply(s);
        }

        /// <summary>
        /// Vector-Scalar multiplication.
        /// </summary>
        /// <param name="A">The left side vector of the multiplication operator.</param>
        /// <param name="s"> The right side scalar of the multiplication operator.</param>
        /// <returns>A vector that represents the result of the multiplication.</returns>
        public static Vector operator *(Vector A, double s)
        {
            return A.Multiply(s);
        }




        ///// <summary>
        ///// Vector - Vector multiplication. 
        ///// Row Vector * Column Vector: Inner product.
        ///// Column Vector * Row Vector: Outer product.
        ///// </summary>
        ///// <param name="A"> The left side vector of the multiplication operator.</param>
        ///// <param name="B">The right side vector of the multiplication operator.</param>
        ///// <returns>A value that represents the result of the vector multiplication.</returns>
        ///// <remarks>
        ///// The dot product is the result of multiplying all the components of two vectors together and adding the results.
        ///// </remarks>
        //public static Matrix operator *(Vector A, Vector B)
        //{
        //    Matrix matrixA = A;
        //    Matrix matrixB = B;

        //    return  matrixA * matrixB;
        //}

        /// <summary>
        /// Dot product or scalar product.
        /// </summary>
        /// <param name="A"> The left side vector of the operator.</param>
        /// <param name="B">The right side vector of the operator.</param>
        /// <remarks>
        /// The dot product is the result of multiplying all the components of two vectors together and adding the results, res= Sum(A[i]*B[i]).
        /// </remarks>
        /// <returns>The dot product = Sum(A[i]*B[i])</returns>
        public static double DotProduct(Vector A, Vector B)
        {
            //if (A.Type != VectorType.Row || B.Type != VectorType.Column || B.Length != A.Length)
            //{
            //    throw new System.ArgumentException("Vector dimensions or type are not valid.");
            //}

            if ( B.Length != A.Length)
            {
                throw new System.ArgumentException("Vector dimensions must agree.");
            }

            double C = 0.0;

            double[] AData = A.Data;
            double[] BData = B.Data;

            for (int i = 0; i < AData.Length; i++)
            {
                C += AData[i] * BData[i];
            }

            return C;
        }

        /// <summary>
        /// Dot product of this vector with another vector.
        /// </summary>
        /// <param name="B">The other vector.</param>
        /// <remarks>
        /// The dot product is the result of multiplying all the components of two vectors together and adding the results, res= Sum(A[i]*B[i]).
        /// </remarks>
        /// <returns>r = Sum(this[i]*B[i])</returns>
        public double DotProduct( Vector B)
        {
            return Vector.DotProduct(this, B);
        }



        /// <summary>
        /// Transposed vector.
        /// </summary>
        /// <returns>The transposed vector.</returns>
        /// <remarks>
        /// Transposition turns a row vector into a column vector ( Or a column vector into a row vector).
        /// </remarks>
        public Vector Transpose()
        {
            Vector AT = new Vector(this._Data);

            if (this._Type == VectorType.Column) AT.Type = VectorType.Row;
            else AT.Type = VectorType.Column;

            return AT;
        }

        #region  Vector  And matrix Operations

        /// <summary>
        /// Matrix- Vector multiplication.
        /// </summary>
        /// <param name="A"> The left side matrix of the multiplication operator.</param>
        /// <param name="B">The right side vector of the multiplication operator.</param>
        /// <returns>A matrix that represents the result of the matrix multiplication.</returns>
        public static Matrix operator *(BaseMatrix A, Vector B)
        {

            int BRows;
            int BColumns;

            if (B.Type == VectorType.Column)
            {
                BColumns = 1;
                BRows = B.Length;
            }
            else
            {
                BColumns = B.Length;
                BRows = 1;
            }



            if (A.ColumnCount != BRows)
            {
                throw new System.ArgumentException("Matrix dimensions are not valid.");
            }

            Matrix C = new Matrix(A.RowCount, BColumns);

            double[] AData = A.Data;
            double[] BData = B.Data;
            double[] CData = C.Data;

            int ARows = A.RowCount;
            int AColumns = A.ColumnCount;


            double Sum = 0.0;
            for (int j = 0; j < BColumns; j++)
            {
                for (int i = 0; i < ARows; i++)
                {
                    Sum = 0.0;
                    for (int k = 0; k < AColumns; k++)
                    {
                        Sum += AData[i + k * ARows] * BData[k + j * BRows];
                    }
                    CData[i + j * ARows] = Sum;
                }
            }
            return C;
        }


        ///// <summary>
        ///// Vector-Matrix multiplication.
        ///// </summary>
        ///// <param name="A"> The left side vector of the multiplication operator.</param>
        ///// <param name="B">The right side matrix of the multiplication operator.</param>
        ///// <returns>A matrix that represents the result of the matrix multiplication.</returns>
        //public static Matrix operator *(Vector A, BaseMatrix B)
        //{

        //    int ARows;
        //    int AColumns;

        //    if (A.Type == VectorType.Column)
        //    {
        //        AColumns = 1;
        //        ARows = A.Length;
        //    }
        //    else
        //    {
        //        AColumns = A.Length;
        //        ARows = 1;
        //    }



        //    if (B.Rows != AColumns)
        //    {
        //        throw new System.ArgumentException("Matrix dimensions are not valid.");
        //    }

        //    Matrix C = new Matrix(ARows, B.Columns);

        //    double[] AData = A.Data;
        //    double[] BData = B.Data;
        //    double[] CData = C.Data;

        //    int BRows = B.Rows;
        //    int BColumns = B.Columns;


        //    double Sum = 0.0;
        //    for (int j = 0; j < BColumns; j++)
        //    {
        //        for (int i = 0; i < ARows; i++)
        //        {
        //            Sum = 0.0;
        //            for (int k = 0; k < AColumns; k++)
        //            {
        //                Sum += AData[i + k * ARows] * BData[k + j * BRows];
        //            }
        //            CData[i + j * ARows] = Sum;
        //        }
        //    }
        //    return C;
        //}

        #endregion

        #endregion

        /// <summary>
        /// Implicit Vector to Matrix conversion.
        /// </summary>
        /// <param name="V">The Vector</param>
        /// <returns>The Matrix.</returns>
        public static implicit operator Matrix (Vector V)
        {
            Matrix NewMatrix;
            if (V.Type == VectorType.Column)
            {
                NewMatrix = new Matrix(V.Length, 1, V.Data); 
            }
            else
            {
                NewMatrix = new Matrix(1, V.Length, V.Data);
            }
            return NewMatrix;
        }

        #region Public Methods


        #region To Array

        /// <summary>
        ///  Copies the elements of this vector to a new array.
        /// </summary>
        /// <returns>An array containing copies of the elements of this vector.</returns>
        public double[] ToArray()
        {

            double[] VectData = new double[this.Data.Length];

            this._Data.CopyTo(VectData, 0);          

            return VectData;
        }

        #endregion


        #region Add

        /// <summary>
        /// Add a scalar to all elements of this vector.
        /// </summary>
        /// <param name="s">The scalar.</param>
        /// <returns>
        /// Vector r[i] = this[i] + s
        /// </returns>
        public Vector Add( double s)
        {
            Vector v = new Vector(this._Type, this._Data.Length);
            double[] vData = v.Data;
            for (int i = 0; i < vData.Length; i++)
            {
                vData[i] = this._Data[i] + s;
            }

            return v;
        }

        /// <summary>
        /// In place add a scalar to all elements of this vector.
        /// </summary>
        /// <param name="s">The scalar.</param>
        public void AddInplace(double s)
        {
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] += s;
            }
        }


        /// <summary>
        /// Add a Vector.
        /// </summary>
        /// <param name="B">The vector B.</param>
        /// <returns>
        /// Vector r[i] = this[i] + B[i]
        /// </returns>
        public Vector Add(Vector B)
        {
            if (B.Type != this.Type || B.Length != this.Length)
            {
                throw new System.ArgumentException("Vector dimensions or type are not valid.");
            }

            Vector r = new Vector(this._Type, this.Length);
            double[] rData = r.Data;
            for (int i = 0; i < rData.Length; i++)
            {
                rData[i] = this._Data[i] + B[i];
            }

            return r;
        }

        /// <summary>
        /// In place add a Vector.
        /// </summary>
        /// <param name="B">The vector B.</param>
        public void AddInplace(Vector B)
        {
            if (B.Type != this.Type || B.Length != this.Length)
            {
                throw new System.ArgumentException("Vector dimensions or type are not valid.");
            }

            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] += B[i];
            }

        }

        #endregion


        #region Subtract

        /// <summary>
        /// Subtract a scalar to all elements of this vector.
        /// </summary>
        /// <param name="s">The scalar.</param>
        /// <returns>
        /// Vector r[i] = this[i] - s
        /// </returns>
        public Vector Subtract(double s)
        {
            Vector v = new Vector(this._Type, this.Length);
            double[] vData = v.Data;
            for (int i = 0; i < vData.Length; i++)
            {
                vData[i] = this._Data[i] - s;
            }

            return v;
        }

        /// <summary>
        /// In place subtract a scalar to all elements of this vector.
        /// </summary>
        /// <param name="s">The scalar.</param>
        public void SubtractInplace(double s)
        {
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] -= s;
            }
        }


        /// <summary>
        /// Subtract a Vector.
        /// </summary>
        /// <param name="B">The vector B.</param>
        /// <returns>
        /// Vector r[i] = this[i] - B[i]
        /// </returns>
        public Vector Subtract(Vector B)
        {
            if (B.Type != this.Type || B.Length != this.Length)
            {
                throw new System.ArgumentException("Vector dimensions or type are not valid.");
            }

            Vector r = new Vector(this._Type, this.Length);
            double[] rData = r.Data;
            for (int i = 0; i < rData.Length; i++)
            {
                rData[i] = this._Data[i] - B[i];
            }

            return r;
        }

        /// <summary>
        /// In place add a Vector.
        /// </summary>
        /// <param name="B">The vector B.</param>
        public void SubtractInplace(Vector B)
        {
            if (B.Type != this.Type || B.Length != this.Length)
            {
                throw new System.ArgumentException("Vector dimensions or type are not valid.");
            }

            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] -= B[i];
            }

        }

        #endregion


        #region Multiply


        /// <summary>
        /// Multiply a scalar to all elements of this vector.
        /// </summary>
        /// <param name="s">The scalar.</param>
        /// <returns>
        /// Vector r[i] = this[i] * s
        /// </returns>
        public Vector Multiply(double s)
        {
            Vector v = new Vector(this._Type, this.Length);
            double[] vData = v.Data;
            for (int i = 0; i < vData.Length; i++)
            {
                vData[i] = this._Data[i] * s;
            }

            return v;
        }


        /// <summary>
        /// In place multiply this vector with a scalar.
        /// </summary>
        /// <param name="scalar">The scalar </param>
        public void MultiplyInplace(double scalar)
        {
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] *= scalar;
            }
        }

        #endregion

        #region UnaryMinus

        /// <summary>
        /// Unary minus.
        /// </summary>
        /// <returns>
        /// Vector r[i] = -this[i]
        /// </returns>
        public Vector UnaryMinus()
        {
            Vector v = new Vector(this._Type, this.Length);
            double[] vData = v.Data;
            for (int i = 0; i < vData.Length; i++)
            {
                vData[i] -= this._Data[i] ;
            }

            return v;
        }

        /// <summary>
        /// In place unary minus of this vector.
        /// </summary>
        public void UnaryMinusInplace()
        {
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] = -this._Data[i];
            }
        }


        #endregion


        #region Norm



        /// <summary>
        /// Calculate the norm of the vector (The 2-norm of the vector). 
        /// </summary>
        /// <remarks>
        /// The 2-norm of a vector is the square root of the sum of squares of the vector coefficients.
        /// res = sum(u[i]^2)
        /// </remarks>
        /// <returns>The norm</returns>
        public double  Norm()
        {

            double norm = 0.0;

            for (int i = 0; i < this._Data.Length; i++)
            {
                norm += this._Data[i] * this._Data[i];
            }

            norm = Math.Sqrt(norm);
            return norm;
        }

        /// <summary>
        /// Calculate the 1-norm of the vector.
        /// </summary>
        /// <returns>
        /// r = sum(abs(this[i]))
        /// </returns>
        public double Norm1()
        {
            double sum = 0;
            for (int i = 0; i < this._Data.Length; i++)
            {
                sum += Math.Abs(this._Data[i]);
            }

            return sum;
        }


        /// <summary>
        /// Calculate the p-Norm.
        /// </summary>
        /// <returns>
        /// res = Sum(abs(u[i])^p))^(1/p)
        /// </returns>
        public double NormP(int p)
        {
            if (p < 1)
            {
                throw new ArgumentOutOfRangeException("p, p < 1");
            }

            if (1 == p)
            {
                return Norm1();
            }

            if (2 == p)
            {
                return Norm();
            }

            double sum = 0;
            for (int i = 0; i < this._Data.Length; i++)
            {
                sum += Math.Pow(Math.Abs(this._Data[i]), p);
            }

            return Math.Pow(sum, 1.0 / p);
        }


        /// <summary>
        /// Infinity-Norm.
        /// </summary>
        /// <returns>
        /// res = max(abs(u[i]))
        /// </returns>
        public double NormInf()
        {
            double max = 0;
            for (int i = 0; i < this._Data.Length; i++)
            {
                max = Math.Max(max, Math.Abs(this._Data[i]));
            }

            return max;
        }


        #endregion


        /// <summary>
        /// Normalizes this vector to a unit vector with respect to the Eucliden 2-Norm.
        /// </summary>
        public Vector Normalize()
        {
            double norm = Norm();
            Vector normalized = this.Clone();
            if (norm < 1E-13)
            {
                return normalized;
            }
            normalized.MultiplyInplace(1.0/norm);
            return normalized;
        }

        /// <summary>
        /// Creates a copy of the vector.
        /// </summary>
        /// <returns>The copy of the vector.</returns>
        public Vector Clone()
        {
            Vector NewVector = new Vector(this._Type, this._Data);
            return NewVector;
        }

        /// <summary>
        /// Returns the equivalent string representation of the vector.
        /// </summary>
        /// <returns>The string representation of the vector.</returns>
        public string VectorToString()
        {
            using (StringWriter writer = new StringWriter())
            {
                if (this._Type == VectorType.Column)
                {
                    for (int i = 0; i < this._Data.Length; i++)
                    {
                        writer.Write(this._Data[i]);
                        if (i < this._Data.Length - 1) writer.WriteLine();
                    }
                }
                else if (this._Type == VectorType.Row)
                {
                    for (int i = 0; i < this._Data.Length; i++)
                    {
                        if (i < this._Data.Length - 1)
                            writer.Write(this._Data[i] + ", ");
                        else
                            writer.Write(this._Data[i]);
                    }
                }
                return writer.ToString();
            }
        }


        #endregion
    }
}
