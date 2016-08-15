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
    /// Represents the base class for band matrices.
    /// </summary>
    public abstract class BaseBandMatrix: BaseMatrix
    {

        #region Fields

        /// <summary>
        /// Number of bands below the main diagonal (lowerBandWidth)
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        protected int MeLowerBandWidth;

        /// <summary>
        /// Number of bands above the main diagonal (upperBandWidth)
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        protected int MeUpperBandWidth;

        ///// <summary>
        ///// Number of rows of the reduced matrix
        ///// </summary>
        //[DebuggerBrowsable(DebuggerBrowsableState.Never)]
        //private int MeReducedMatrixRows;

        #endregion

        #region  Public Constructors


        /// <summary>
        /// Initializes a new instance of the BandMatrix class of the given size.
        /// </summary>
        /// <param name="rows">Number of rows.</param>
        /// <param name="columns">Number of columns.</param>
        /// <param name="lowerBandWidth">Number of bands below the main diagonal</param>
        /// <param name="upperBandWidth">Number of bands above the main diagonal</param>
        public BaseBandMatrix(int rows, int columns, int lowerBandWidth, int upperBandWidth)
            : base(rows, columns)
        {
            if (lowerBandWidth < 0 || lowerBandWidth >= rows) throw new System.ArgumentException("lowerBandWidth < 0 || lowerBandWidth >= rows");  //Debe de ser menor que rows
            if (upperBandWidth < 0 || upperBandWidth >= columns) throw new System.ArgumentException("upperBandWidth < 0 || upperBandWidth >= columns"); //Debe de ser menor que columns

            // For an m by n matrix A with band widths ml and mu, the array AGB must have a leading dimension, lda, 
            //greater than or equal to ml+mu+1. The size of the second dimension must be (at least) n, the number of columns in the matrix. 
            //Using the array AGB, which is declared as AGB(ml+mu+1, n)

            this.MeLowerBandWidth = lowerBandWidth;
            this.MeUpperBandWidth = upperBandWidth;
            //this.MeReducedMatrixRows = lowerBandWidth + upperBandWidth + 1;
        }

        /// <summary>
        /// Initializes a new instance of the BandMatrix class of the given size using a array
        /// </summary>
        /// <param name="rows">Number of rows.</param>
        /// <param name="columns">Number of columns.</param>
        /// <param name="lowerBandWidth">Number of bands below the main diagonal</param>
        /// <param name="upperBandWidth">Number of bands above the main diagonal</param>
        /// <param name="Data">The matix data </param>
        internal BaseBandMatrix(int rows, int columns, int lowerBandWidth, int upperBandWidth, double[] Data)
            : base(rows, columns, Data)
        {
            if (lowerBandWidth < 0 || lowerBandWidth >= rows) throw new System.ArgumentException("lowerBandWidth < 0 || lowerBandWidth >= rows");  //Debe de ser menor que rows
            if (upperBandWidth < 0 || upperBandWidth >= columns) throw new System.ArgumentException("upperBandWidth < 0 || upperBandWidth >= columns"); //Debe de ser menor que columns

            this.MeLowerBandWidth = lowerBandWidth;
            this.MeUpperBandWidth = upperBandWidth;
            //this.MeReducedMatrixRows = lowerBandWidth + upperBandWidth + 1;

        }

        /// <summary>
        /// Initializes a new instance of the BandMatrix class of the given size using a array
        /// </summary>
        /// <param name="size">Size</param>
        /// <param name="lowerBandWidth">Number of bands below the main diagonal</param>
        /// <param name="upperBandWidth">Number of bands above the main diagonal</param>
        internal BaseBandMatrix(int size, int lowerBandWidth, int upperBandWidth)
            : base(size)
        {
            if (lowerBandWidth < 0 || lowerBandWidth >= size) throw new System.ArgumentException("lowerBandWidth < 0 || lowerBandWidth >= rows");  //Debe de ser menor que rows
            if (upperBandWidth < 0 || upperBandWidth >= size) throw new System.ArgumentException("upperBandWidth < 0 || upperBandWidth >= columns"); //Debe de ser menor que columns

            this.MeLowerBandWidth = lowerBandWidth;
            this.MeUpperBandWidth = upperBandWidth;
            //this.MeReducedMatrixRows = lowerBandWidth + upperBandWidth + 1;

        }

        /// <summary>
        /// Initializes a new instance of the BandMatrix class of the given size using a array
        /// </summary>
        /// <param name="size">Size</param>
        /// <param name="Data">The matix data </param>
        /// <param name="lowerBandWidth">Number of bands below the main diagonal</param>
        /// <param name="upperBandWidth">Number of bands above the main diagonal</param>
        internal BaseBandMatrix(int size, int lowerBandWidth, int upperBandWidth, double[] Data)
            : base(size, Data)
        {
            if (lowerBandWidth < 0 || lowerBandWidth >= size) throw new System.ArgumentException("lowerBandWidth < 0 || lowerBandWidth >= rows");  //Debe de ser menor que rows
            if (upperBandWidth < 0 || upperBandWidth >= size) throw new System.ArgumentException("upperBandWidth < 0 || upperBandWidth >= columns"); //Debe de ser menor que columns

            this.MeLowerBandWidth = lowerBandWidth;
            this.MeUpperBandWidth = upperBandWidth;
            //this.MeReducedMatrixRows = lowerBandWidth + upperBandWidth + 1;

        }

        #endregion


        #region Overloading Operators


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
                    this._Data[row  + column * this._RowCount] = value;
                }
            }
        }

        /// <summary>
        /// Number of bands below the main diagonal (lowerBandWidth)
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        public int LowerBandWidth
        {
            get { return MeLowerBandWidth; }
        }

        /// <summary>
        /// Number of bands above the main diagonal (upperBandWidth)
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        public int UpperBandWidth
        {
            get { return MeUpperBandWidth; }
        }


        #region Static methods

        /// <summary>
        /// Implicit BaseBandMatrix to Matrix conversion.
        /// </summary>
        /// <param name="bandMatrix">The BandMatrix</param>
        /// <returns>The Matrix.</returns>
        public static implicit operator Matrix(BaseBandMatrix bandMatrix)
        {
            Matrix NewMatrix = new Matrix(bandMatrix.RowCount, bandMatrix.ColumnCount, bandMatrix.Data);
            return NewMatrix;
        }


        #endregion

        #endregion

        #region Private Methods



        #endregion


    }
}