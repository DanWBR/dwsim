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

namespace DotNumerics.LinearAlgebra
{
    /// <summary>
    /// Represents a general Matrix.
    /// </summary>
    public sealed class Matrix : BaseMatrix
    {
        #region  Public Constructors

        /// <summary>
        /// Initializes a new instance of the Matrix class of the given size.
        /// </summary>
        /// <param name="size">Size</param>
        public Matrix(int size) : base(size) { }

        /// <summary>
        /// Initializes a new instance of the Matrix class of the given size.
        /// </summary>
        /// <param name="rows">Number of rows.</param>
        /// <param name="columns">Number of columns.</param>
        public Matrix(int rows, int columns) : base(rows, columns) { }
            
        /// <summary>
        /// Initializes a new instance of the Matrix class using a array.
        /// </summary>
        /// <param name="data">The data of the matrix.</param>
        public Matrix(double[,] data)
            : base(data.GetLength(0), data.GetLength(1))
        {
            for (int column = 0; column < base._ColumnCount; column++)
            {
                for (int row = 0; row < base._RowCount; row++)
                {
                    this._Data[row + column * this._RowCount] = data[row, column];
                }
            }
        }


        /// <summary>
        /// Initializes a new instance of the Matrix class of the given size using a array.
        /// </summary>
        /// <param name="rows">Number of rows.</param>
        /// <param name="columns">Number of columns.</param>
        /// <param name="Data">The data, the data is copied.</param>
        internal Matrix(int rows, int columns, double[] Data) : base(rows, columns, Data) { }



        #endregion


        #region Public Methods

        /// <summary>
        /// In place addition A=A+B
        /// </summary>
        /// <param name="B">The Matrix</param>
        public void AddInplace(BaseMatrix B)
        {
            base.CheckMatrixDimensions(B);
            
            double[] BData = B.Data;
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] += BData[i];
            }
        }

        /// <summary>
        /// In place matrix subtraction, A=A-B
        /// </summary>
        /// <param name="B">The Matrix</param>
        public void SubtractInplace(BaseMatrix B)
        {
            CheckMatrixDimensions(B);
            double[] BData = B.Data;
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] -= BData[i];
            }
        }

        /// <summary>
        /// Unary minus.
        /// </summary>
        /// <returns> -this</returns>
        public Matrix UnaryMinus()
        {
            Matrix C= new Matrix(this._RowCount, this._ColumnCount);
            double[] dataC=C.Data;
            for (int i = 0; i < this._Data.Length; i++)
            {
                dataC[i] = -this._Data[i];
            }
            return C;
        }


        #endregion


        #region Overloading Operators


        #region Matrix-Matrix Addition

        /// <summary>
        /// Matrix addition.
        /// </summary>
        /// <param name="A">The left side matrix of the addition operator.</param>
        /// <param name="B">The right side matrix of the addition operator.</param>
        /// <returns>A matrix that represents the result of the matrix addition.</returns>
        public static Matrix operator +(Matrix A, Matrix B)
        {
            return A.Add(B);
        }

        #endregion

        #region Matrix-Matrix Subtraction

        ///// <summary>Matrix Subtraction</summary>
        /// <summary>
        /// Matrix subtraction.
        /// </summary>
        /// <param name="A"> The left side matrix of the subtraction operator.</param>
        /// <param name="B">The right side matrix of the subtraction operator.</param>
        /// <returns>A matrix that represents the result of the matrix subtraction.</returns>
        public static Matrix operator -(Matrix A, Matrix B)
        {
            return A.Subtract(B);
        }

        /// <summary>
        /// Unary minus.
        /// </summary>
        /// <param name="A"> The Matric.</param>
        /// <returns>Matrix r[i] = -this[i]</returns>
        public static Matrix operator -(Matrix A)
        {
            return A.UnaryMinus();
        }

        #endregion


        #region Scalar-Matrix Multiplication

        /// <summary>
        /// Scalar-Matrix multiplication.
        /// </summary>
        /// <param name="s"> The left side scalar of the multiplication operator.</param>
        /// <param name="A">The right side matrix of the multiplication operator.</param>
        /// <returns>A matrix that represents the result of the multiplication.</returns>
        public static Matrix operator *(double s, Matrix A)
        {
            return A.Multiply(s);
        }

        /// <summary>
        /// Scalar-Matrix multiplication.
        /// </summary>
        /// <param name="A">The right side matrix of the multiplication operator.</param>
        /// <param name="s"> The left side scalar of the multiplication operator.</param>
        /// <returns>A matrix that represents the result of the multiplication.</returns>
        public static Matrix operator *(Matrix A, double s)
        {
            return A.Multiply(s);
        }

        internal static void MultiplicationSM(double s, double[] A, double[] C)
        {
            for (int i = 0; i < C.Length; i++)
            {
                C[i] = s * A[i];
            }
        }

        #endregion

        #endregion



        #region Public Methods

        //public double this[int row, int column]
        //{
        //    get
        //    {
        //        return this.MeData[row - 1 + (column - 1) * this.MeRowCount];
        //    }

        //    set
        //    {
        //        this.MeData[row - 1 + (column - 1) * this.MeRowCount] = value;
        //    }
        //}

        /// <summary>
        /// Creates a copy of the matrix.
        /// </summary>
        /// <returns>The copy of the Matrix.</returns>
        public Matrix Clone()
        {
            Matrix NewMatrix = new Matrix(this._RowCount, this._ColumnCount, this._Data);
            return NewMatrix;
        }


        #region Static methods


        /// <summary>Generate a matrix with random elements</summary>
        /// <param name="rows">Number of rows.</param>
        /// <param name="columns">Number of columns.</param>
        /// <returns>An m-by-n matrix with uniformly distributed
        /// random elements in <c>[0, 1)</c> interval.</returns>
        public static Matrix Random(int rows, int columns)
        {
            System.Random random = new System.Random();

            Matrix X = new Matrix(rows, columns);

            double[] XData = X.Data;

            for (int i = 0; i < XData.Length; i++)
            {
                XData[i] = random.NextDouble();
            }
            return X;
        }

        /// <summary>Generate a matrix with random elements</summary>
        /// <param name="rows">Number of rows.</param>
        /// <param name="columns">Number of columns.</param>
        /// <param name="Seed">
        /// A number used to calculate a starting value for the pseudo-random number
        /// sequence. If a negative number is specified, the absolute value of the number
        /// is used.
        /// </param>
        /// <returns>
        /// An m-by-n matrix with uniformly distributed
        /// random elements in <c>[0, 1)</c> interval.
        /// </returns>
        public static Matrix Random(int rows, int columns, int Seed)
        {
            System.Random random = new System.Random(Seed);

            Matrix X = new Matrix(rows, columns);

            double[] XData = X.Data;

            for (int i = 0; i < XData.Length; i++)
            {
                XData[i] = random.NextDouble();
            }
            return X;
        }


        #endregion

        #endregion


    }
}
