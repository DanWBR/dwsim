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


namespace DotNumerics.LinearAlgebra
{
    /// <summary>
    /// Represents a Band Matrix.
    /// </summary>
    public sealed class BandMatrix : BaseBandMatrix
    {

        #region  Public Constructors


        /// <summary>
        /// Initializes a new instance of the BandMatrix class of the given size.
        /// </summary>
        /// <param name="rows">Number of rows.</param>
        /// <param name="columns">Number of columns.</param>
        /// <param name="lowerBandWidth">Number of bands below the main diagonal</param>
        /// <param name="upperBandWidth">Number of bands above the main diagonal</param>
        public BandMatrix(int rows, int columns, int lowerBandWidth, int upperBandWidth) : base(rows, columns, lowerBandWidth, upperBandWidth) { }

        /// <summary>
        /// Initializes a new instance of the BandMatrix class of the given size using a array
        /// </summary>
        /// <param name="rows">Number of rows.</param>
        /// <param name="columns">Number of columns.</param>
        /// <param name="lowerBandWidth">Number of bands below the main diagonal</param>
        /// <param name="upperBandWidth">Number of bands above the main diagonal</param>
        /// <param name="Data">The matix data </param>
        internal BandMatrix(int rows, int columns, int lowerBandWidth, int upperBandWidth, double[] Data) : base(rows, columns, lowerBandWidth, upperBandWidth, Data) { }

        #endregion




        #region Overloading Operators


        /// <summary>
        /// Matrix addition.
        /// </summary>
        /// <param name="A">The left side matrix of the addition operator.</param>
        /// <param name="B">The right side matrix of the addition operator.</param>
        /// <returns>A matrix that represents the result of the matrix addition.</returns>
        public static BandMatrix operator +(BandMatrix A, BandMatrix B)
        {
            if (B.RowCount != A.RowCount || B.ColumnCount != A.ColumnCount || B.LowerBandWidth != A.LowerBandWidth || B.UpperBandWidth != A.UpperBandWidth)
            {
                throw new System.ArgumentException("Matrix dimensions are not valid.");
            }

            BandMatrix C = new BandMatrix(A.RowCount, A.ColumnCount, A.LowerBandWidth, B.UpperBandWidth);

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
        public static BandMatrix operator -(BandMatrix A, BandMatrix B)
        {
            if (B.RowCount != A.RowCount || B.ColumnCount != A.ColumnCount || B.LowerBandWidth != A.LowerBandWidth || B.UpperBandWidth != A.UpperBandWidth)
            {
                throw new System.ArgumentException("Matrix dimensions are not valid.");
            }

            BandMatrix C = new BandMatrix(A.RowCount, A.ColumnCount, A.LowerBandWidth, B.UpperBandWidth);

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
        public static BandMatrix operator *(double s, BandMatrix A)
        {
            BandMatrix C = new BandMatrix(A.RowCount, A.ColumnCount, A.LowerBandWidth, A.UpperBandWidth);

            double[] AData = A.Data;
            double[] CData = C.Data;


            Matrix.MultiplicationSM(s, AData, CData);

            return C;
        }

        #endregion

        #endregion



        #region Public Methods

        /// <summary>
        /// Creates a copy of the matrix.
        /// </summary>
        /// <returns>The copy of the Matrix.</returns>
        public BandMatrix Clone()
        {
            BandMatrix NewBandMatix = new BandMatrix(this._RowCount, this._ColumnCount, this.MeLowerBandWidth, this.MeUpperBandWidth, this._Data);
            return NewBandMatix;
        }


        internal Matrix GetBandPackedMatrix()
        {
            int MatrixRows = 2 * this.MeLowerBandWidth + this.MeUpperBandWidth + 1;
            int MatrixColumns = this._ColumnCount;
            Matrix MatrixBandStorageExt = new Matrix(MatrixRows, MatrixColumns);
            double[] GeneralBandStorage = MatrixBandStorageExt.Data;
            int Index;
            int TempInt = this.MeLowerBandWidth + this.MeUpperBandWidth + 1;
            for (int colum = 1; colum <= MatrixColumns; colum++)
            {
                for (int row = Math.Max(1, colum - this.MeUpperBandWidth); row <= Math.Min(this._ColumnCount, colum + this.MeLowerBandWidth); row++)
                {
                    Index = TempInt + row - colum;
                    GeneralBandStorage[Index - 1 + (colum - 1) * MatrixRows] = this._Data[row - 1 + (colum - 1) * this._RowCount];
                }
            }
            return MatrixBandStorageExt;
        }


        #region Static methods


        /// <summary>Generate a BandMatrix with random elements</summary>
        /// <param name="rows">Number of rows.</param>
        /// <param name="columns">Number of columns.</param>
        /// <param name="lowerBandWidth">Number of bands below the main diagonal</param>
        /// <param name="upperBandWidth">Number of bands above the main diagonal</param>
        public static BandMatrix Random(int rows, int columns, int lowerBandWidth, int upperBandWidth)
        {
            System.Random random = new System.Random();

            BandMatrix X = new BandMatrix(rows, columns, lowerBandWidth, upperBandWidth);

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

        #region Private Methods

        #endregion


    }
}
