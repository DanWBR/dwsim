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

//A general tridiagonal matrix is a matrix whose nonzero elements are found only on the diagonal, subdiagonal, and superdiagonal of the matrix; that is: 
//aij = 0    if |i-j| > 1 

namespace DotNumerics.LinearAlgebra
{
    /// <summary>
    /// Represents a Tridiagonal Matrix.
    /// </summary>
    public sealed class TridiagonalMatrix : BaseMatrix
    {
        #region  Public Constructors

        /// <summary>
        /// Initializes a new instance of the TridiagonalMatrix class of the given size.
        /// </summary>
        /// <param name="size">Size</param>
        public TridiagonalMatrix(int size) : base(size) { }

        /// <summary>
        /// Initializes a new instance of the TridiagonalMatrix class of the given size using a array
        /// </summary>
        /// <param name="size">Size</param>
        /// <param name="Data">The data</param>
        internal TridiagonalMatrix(int size, double[] Data) : base(size, Data) { }

        #endregion


        #region Public Methods

        public override double this[int row, int column]
        {
            get
            {
                if (column >= this._ColumnCount)
                {
                    throw new ArgumentException("Index was outside the bounds of the matrix.");
                }
                return this._Data[row + column * this._RowCount];
            }
            set
            {
                if (column >= this._ColumnCount)
                {
                    throw new ArgumentException("Index was outside the bounds of the matrix.");
                }
                //aij = 0    if |i-j| > 1 
                if (Math.Abs(row - column) <= 1)
                {
                    this._Data[row + column * this._RowCount] = value;
                }
            }
        }


        internal void GetPackedMatrix(out double[] SubDiagonal, out double[] SuperDiagonal, out double[] Diagonal)
        {
            Diagonal = new double[this._RowCount];
            SubDiagonal = new double[this._RowCount - 1];
            SuperDiagonal = new double[this._RowCount - 1];

            //Para la diagonal
            for (int i = 0; i < this._RowCount; i++)
            {
                Diagonal[i] = this._Data[i + i * this._RowCount];
            }

            //Para la SubDiagonal
            for (int i = 0; i < this._RowCount - 1; i++)
            {
                SubDiagonal[i] = this._Data[i + 1 + i * this._RowCount];
            }

            //Para la SuperDiagonal
            for (int i = 0; i < this._RowCount - 1; i++)
            {
                SuperDiagonal[i] = this._Data[i + (i + 1) * this._RowCount];
            }
        }

        public TridiagonalMatrix Clone()
        {
            TridiagonalMatrix NewMatrix = new TridiagonalMatrix(this._RowCount, this._Data);
            return NewMatrix;
        }



        #region Static methods


        /// <summary>Generate a TridiagonalMatrix with random elements</summary>
        /// <param name="size">Size</param>
        public static TridiagonalMatrix Random(int size)
        {
            System.Random random = new System.Random();

            TridiagonalMatrix X = new TridiagonalMatrix(size);

            double[] XData = X.Data;

            for (int j = 0; j < X.ColumnCount; j++)
            {
                for (int i = 0; i < X.RowCount; i++)
                {
                    X[i, j] = random.NextDouble();
                }
            }
            return X;
        }

        #endregion

        #endregion

        #region Overloading Operators

        /// <summary>
        /// Matrix addition.
        /// </summary>
        /// <param name="A">The left side matrix of the addition operator.</param>
        /// <param name="B">The right side matrix of the addition operator.</param>
        /// <returns>A matrix that represents the result of the matrix addition.</returns>
        public static TridiagonalMatrix operator +(TridiagonalMatrix A, TridiagonalMatrix B)
        {
            if (B.RowCount != A.RowCount || B.ColumnCount != A.ColumnCount)
            {
                throw new System.ArgumentException("Matrix dimensions are not valid.");
            }

            TridiagonalMatrix C = new TridiagonalMatrix(A.RowCount);

            double[] AData = A.Data;
            double[] BData = B.Data;
            double[] CData = C.Data;

            for (int i = 0; i < AData.Length; i++)
            {
                CData[i] = AData[i] + BData[i];
            }

            return C;
        }

        /// <summary>
        /// Matrix subtraction.
        /// </summary>
        /// <param name="A"> The left side matrix of the subtraction operator.</param>
        /// <param name="B">The right side matrix of the subtraction operator.</param>
        /// <returns>A matrix that represents the result of the matrix subtraction.</returns>
        public static TridiagonalMatrix operator -(TridiagonalMatrix A, TridiagonalMatrix B)
        {
            if (B.RowCount != A.RowCount || B.ColumnCount != A.ColumnCount)
            {
                throw new System.ArgumentException("Matrix dimensions are not valid.");
            }

            TridiagonalMatrix C = new TridiagonalMatrix(A.RowCount);

            double[] AData = A.Data;
            double[] BData = B.Data;
            double[] CData = C.Data;

            for (int i = 0; i < AData.Length; i++)
            {
                CData[i] = AData[i] - BData[i];
            }

            return C;
        }

        #region Scalar-Matrix Multiplication

        /// <summary>
        /// Scalar-Matrix multiplication.
        /// </summary>
        /// <param name="s"> The left side scalar of the multiplication operator.</param>
        /// <param name="A">The right side matrix of the multiplication operator.</param>
        /// <returns>A matrix that represents the result of the multiplication.</returns>
        public static TridiagonalMatrix operator *(double s, TridiagonalMatrix A)
        {
            TridiagonalMatrix C = new TridiagonalMatrix(A.RowCount);

            double[] AData = A.Data;
            double[] CData = C.Data;


            Matrix.MultiplicationSM(s, AData, CData);

            return C;
        }

        #endregion

        /// <summary>
        /// Implicit TridiagonalMatrix to Matrix conversion.
        /// </summary>
        /// <param name="tridiagonal"> The TridiagonalMatrix.</param>
        /// <returns>The Matrix.</returns>
        public static implicit operator Matrix(TridiagonalMatrix tridiagonal)
        {
            Matrix NewMatrix = new Matrix(tridiagonal.RowCount, tridiagonal.ColumnCount, tridiagonal.Data);
            return NewMatrix;
        }


        #endregion

    }
}
