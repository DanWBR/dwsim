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

//IBM Def
//A general band matrix has its nonzero elements arranged uniformly near the diagonal, such that: 

//aij = 0    if (i-j) > ml or (j-i) > mu 
//where ml and mu are the lower and upper band widths, respectively, and ml+mu+1 is the total band width. 
//
//The matrix A is symmetric if it has the property A = AT, which means: 
//It has the same number of rows as it has columns; that is, it has n rows and n columns. 
//The value of every element aij on one side of the main diagonal equals its mirror 
//image aji on the other side: aij = aji for 1 <= i <= n and 1 <= j <= n. 

namespace DotNumerics.LinearAlgebra
{
    /// <summary>
    /// Represents a  symmetric band matrix.
    /// </summary>
    public sealed class SymmetricBandMatrix : BaseBandMatrix
    {

        #region  Public Constructors


        /// <summary>
        /// Initializes a new instance of the SymmetricBandMatrix class of the given size.
        /// </summary>
        /// <param name="size">Size</param>
        /// <param name="BandWidth">Number of bands below or above the main diagonal</param>
        public SymmetricBandMatrix(int size, int BandWidth) : base(size, BandWidth, BandWidth) { }

        /// <summary>
        /// Initializes a new instance of the SymmetricBandMatrix class of the given size using a array
        /// </summary>
        /// <param name="size">Size</param>
        /// <param name="BandWidth">Number of bands below or above the main diagonal</param>
        /// <param name="Data">The matix data </param>>
        internal SymmetricBandMatrix(int size, int BandWidth, double[] Data) : base(size, BandWidth, BandWidth) { }

        #endregion


        #region Public Methods

        /// <summary>
        /// Returns the value of a element of the matrix.
        /// </summary>
        /// <param name="row">The row value (zero-based).</param>
        /// <param name="column">The column value (zero-based).</param>
        /// <returns>The matrix value at (row, column).</returns>
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
                //A general band matrix has its nonzero elements arranged uniformly near the diagonal, such that: 
                //aij = 0    if (i-j) > ml or (j-i) > mu 
                if ((row - column) <= this.MeLowerBandWidth && (column - row) <= this.MeUpperBandWidth)
                {
                    //aij = aji for 1 <= i <= n and 1 <= j <= n.
                    this._Data[row + column * this._RowCount] = value;
                    this._Data[column + row * this._RowCount] = value;
                }
            }
        }


        /// <summary>
        /// Creates a copy of the matrix.
        /// </summary>
        /// <returns>The copy of the Matrix.</returns>
        public SymmetricBandMatrix Clone()
        {
            SymmetricBandMatrix NewBandMatix = new SymmetricBandMatrix(this._RowCount, this.MeLowerBandWidth, this._Data);
            return NewBandMatix;
        }

        internal Matrix GetSymmetricBandPackedMatrix()
        {
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.
            // *
            // *  KD      (input) INTEGER
            // *          The number of superdiagonals of the matrix A if UPLO = 'U',
            // *          or the number of subdiagonals if UPLO = 'L'.  KD >= 0.
            // *
            // *  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB, N)
            // *          On entry, the upper or lower triangle of the symmetric band
            // *          matrix A, stored in the first KD+1 rows of the array.  The
            // *          j-th column of A is stored in the j-th column of the array AB
            // *          as follows:
            // *          if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
            // *          if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
            // *
            // *          On exit, AB is overwritten by values generated during the
            // *          reduction to tridiagonal form.  If UPLO = 'U', the first
            // *          superdiagonal and the diagonal of the tridiagonal matrix T
            // *          are returned in rows KD and KD+1 of AB, and if UPLO = 'L',
            // *          the diagonal and first subdiagonal of T are returned in the
            // *          first two rows of AB.
            // *
            // *  LDAB    (input) INTEGER
            // *          The leading dimension of the array AB.  LDAB >= KD + 1.
            // *
            int MatrixRows = this.MeLowerBandWidth + 1;
            int MatrixColumns = this._ColumnCount;
            Matrix MatrixSymmetricBandStorageExt = new Matrix(MatrixRows, MatrixColumns);
            double[] GeneralBandStorage = MatrixSymmetricBandStorageExt.Data;
            int Index;
            for (int colum = 1; colum <= MatrixColumns; colum++)
            {
                for (int row = Math.Max(1, colum - this.MeLowerBandWidth); row <= colum; row++)
                {
                    Index = this.MeLowerBandWidth + 1 + row - colum;
                    GeneralBandStorage[Index - 1 + (colum - 1) * MatrixRows] = this._Data[row - 1 + (colum - 1) * this._RowCount];
                }
            }
            return MatrixSymmetricBandStorageExt;
        }


        #region Static methods


        /// <summary>Generate a BandMatrix with random elements</summary>
        /// <param name="size">Size</param>
        /// <param name="BandWidth">Number of bands below or above the main diagonal</param>
        public static SymmetricBandMatrix Random(int size, int BandWidth)
        {
            System.Random random = new System.Random();

            SymmetricBandMatrix X = new SymmetricBandMatrix(size, BandWidth);

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
        public static SymmetricBandMatrix operator +(SymmetricBandMatrix A, SymmetricBandMatrix B)
        {
            if (B.RowCount != A.RowCount || B.ColumnCount != A.ColumnCount || B.LowerBandWidth != A.LowerBandWidth || B.UpperBandWidth != A.UpperBandWidth)
            {
                throw new System.ArgumentException("Matrix dimensions are not valid.");
            }

            SymmetricBandMatrix C = new SymmetricBandMatrix(A.RowCount, A.LowerBandWidth);

            double[] AData = A.Data;
            double[] BData = B.Data;
            double[] CData = C.Data;

            for (int i = 0; i < AData.Length; i++)
            {
                CData[i] = AData[i] + BData[i];
            }

            return C;
        }

        ///// <summary>Matrix Subtraction</summary>
        /// <summary>
        /// Matrix subtraction.
        /// </summary>
        /// <param name="A"> The left side matrix of the subtraction operator.</param>
        /// <param name="B">The right side matrix of the subtraction operator.</param>
        /// <returns>A matrix that represents the result of the matrix subtraction.</returns>
        public static SymmetricBandMatrix operator -(SymmetricBandMatrix A, SymmetricBandMatrix B)
        {
            if (B.RowCount != A.RowCount || B.ColumnCount != A.ColumnCount || B.LowerBandWidth != A.LowerBandWidth || B.UpperBandWidth != A.UpperBandWidth)
            {
                throw new System.ArgumentException("Matrix dimensions are not valid.");
            }

            SymmetricBandMatrix C = new SymmetricBandMatrix(A.RowCount, A.LowerBandWidth);

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
        public static SymmetricBandMatrix operator *(double s, SymmetricBandMatrix A)
        {
            SymmetricBandMatrix C = new SymmetricBandMatrix(A.RowCount, A.LowerBandWidth);

            double[] AData = A.Data;
            double[] CData = C.Data;


            Matrix.MultiplicationSM(s, AData, CData);

            return C;
        }

        #endregion

        #endregion

    }
}
