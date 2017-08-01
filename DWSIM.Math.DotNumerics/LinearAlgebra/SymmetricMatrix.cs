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

//The matrix A is symmetric if it has the property A = AT, which means: 
//It has the same number of rows as it has columns; that is, it has n rows and n columns. 
//The value of every element aij on one side of the main diagonal equals its mirror 
//image aji on the other side: aij = aji for 1 <= i <= n and 1 <= j <= n. 

namespace DotNumerics.LinearAlgebra
{
    /// <summary>
    /// Represents a  symmetric matrix.
    /// </summary>
    public sealed class SymmetricMatrix : BaseMatrix
    {

        #region  Public Constructors

        /// <summary>
        /// Initializes a new instance of the SymmetricMatrix class of the given size.
        /// </summary>
        /// <param name="size">Size</param>
        public SymmetricMatrix(int size) : base(size) { }

        /// <summary>
        /// Initializes a new instance of the SymmetricMatrix class of the given size using a array
        /// </summary>
        /// <param name="size">Size</param>
        /// <param name="Data">The data</param>
        internal SymmetricMatrix(int size, double[] Data) : base(size, Data) { }

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
                //aij = aji for 1 <= i <= n and 1 <= j <= n.
                this._Data[row + column * this._RowCount] = value;
                this._Data[column + row * this._RowCount] = value;
            }
        }



        public SymmetricMatrix Clone()
        {
            SymmetricMatrix NewMatrix = new SymmetricMatrix(this._RowCount, this._Data);
            return NewMatrix;
        }



        #region Static methods

        #region Static methods


        /// <summary>
        /// Generate a matrix with random elements
        /// </summary>
        /// <param name="size">Size</param>
        /// <returns>An m-by-n matrix with uniformly distributed
        /// random elements in <c>[0, 1)</c> interval.</returns>
        public static SymmetricMatrix Random(int size)
        {
            System.Random random = new System.Random();

            SymmetricMatrix X = new SymmetricMatrix(size);

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


        /// <summary>
        /// Generate a matrix with random elements
        /// </summary>
        /// <param name="size">Size</param>
        /// <param name="Seed">
        /// A number used to calculate a starting value for the pseudo-random number
        /// sequence. If a negative number is specified, the absolute value of the number
        /// is used.
        /// </param>
        /// <returns>An m-by-n matrix with uniformly distributed
        /// random elements in <c>[0, 1)</c> interval.</returns>
        public static SymmetricMatrix Random(int size, int Seed)
        {
            System.Random random = new System.Random(Seed);

            SymmetricMatrix X = new SymmetricMatrix(size);

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

        #endregion


        #region Overloading Operators

        /// <summary>
        /// Matrix addition.
        /// </summary>
        /// <param name="A">The left side matrix of the addition operator.</param>
        /// <param name="B">The right side matrix of the addition operator.</param>
        /// <returns>A matrix that represents the result of the matrix addition.</returns>
        public static SymmetricMatrix operator +(SymmetricMatrix A, SymmetricMatrix B)
        {
            if (B.RowCount != A.RowCount || B.ColumnCount != A.ColumnCount)
            {
                throw new System.ArgumentException("Matrix dimensions are not valid.");
            }

            SymmetricMatrix C = new SymmetricMatrix(A.RowCount);

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
        public static SymmetricMatrix operator -(SymmetricMatrix A, SymmetricMatrix B)
        {
            if (B.RowCount != A.RowCount || B.ColumnCount != A.ColumnCount)
            {
                throw new System.ArgumentException("Matrix dimensions are not valid.");
            }

            SymmetricMatrix C = new SymmetricMatrix(A.RowCount);

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
        public static SymmetricMatrix operator *(double s, SymmetricMatrix A)
        {
            SymmetricMatrix C = new SymmetricMatrix(A.RowCount);

            double[] AData = A.Data;
            double[] CData = C.Data;


            Matrix.MultiplicationSM(s, AData, CData);

            return C;
        }

        /// <summary>
        /// Implicit SymmetricMatrix to Matrix conversion.
        /// </summary>
        /// <param name="symmetricMatrix">The SymmetricMatrix.</param>
        /// <returns>The matrix.</returns>
        public static implicit operator Matrix(SymmetricMatrix symmetricMatrix)
        {
            Matrix NewMatrix = new Matrix(symmetricMatrix.RowCount, symmetricMatrix.ColumnCount, symmetricMatrix.Data);
            return NewMatrix;
        }

        #endregion

        #endregion

    }
}
