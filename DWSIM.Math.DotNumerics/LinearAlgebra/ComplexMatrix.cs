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
using DotNumerics.FortranLibrary;

namespace DotNumerics.LinearAlgebra
{
    //[DebuggerDisplay("[{RowCount},{ColumnCount}]", Name = "MatrixComplex")]
    /// <summary>
    /// Represents a Complex Matrix.
    /// </summary>
    [DebuggerDisplay("[{RowCount},{ColumnCount}]")]
    [DebuggerTypeProxy(typeof(MatrixComplexDebuggerDisplay))]
    public class ComplexMatrix : IMatrix<Complex>
    {
        #region Fields
        /// <summary>
        /// Los datos de la matriz, los datos se almacenan en un un array unidimensional,
        /// Los elementos se almacenan por columnas, esto para que sean compatible con los Arrays de Fortran
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        protected Complex[] _Data;
        /// <summary>
        /// El numero de renglones
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        protected int _RowCount;

        /// <summary>
        /// El numero de columnas
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        protected int _ColumnCount;

        #endregion


        #region  Public Constructors

        /// <summary>
        /// Initializes a new instance of the MatrixComplex class of the given size.
        /// </summary>
        /// <param name="rows">Number of rows.</param>
        /// <param name="columns">Number of columns.</param>
        public ComplexMatrix(int rows, int columns)
        {
            if (rows < 1) throw new System.ArgumentException("rows < 1");
            if (columns < 1) throw new System.ArgumentException("columns < 1");

            this._Data = new Complex[rows * columns];
            this._RowCount = rows;
            this._ColumnCount = columns;
        }

        /// <summary>
        /// Initializes a new instance of the MatrixComplex class of the given size using a array
        /// </summary>
        /// <param name="rows">Number of rows.</param>
        /// <param name="columns">Number of columns.</param>
        /// <param name="Data">The data</param>
        internal ComplexMatrix(int rows, int columns, Complex[] Data)
        {
            if (rows < 1) throw new System.ArgumentException("rows < 1");
            if (columns < 1) throw new System.ArgumentException("columns < 1");
            this._Data = new Complex[rows * columns];
            this._RowCount = rows;
            this._ColumnCount = columns;
            //Si incluye la posibilidad de que los datos tengan menos valores que la matriz a crear 
            for (int i = 0; i < Math.Min(this._Data.Length, Data.Length); i++)
            {
                this._Data[i] = Data[i];
            }
        }

        /// <summary>
        /// Initializes a new instance of the MatrixComplex class of the given size.
        /// </summary>
        /// <param name="size">Size</param>
        public ComplexMatrix(int size)
        {
            if (size < 1) throw new System.ArgumentException("size < 1");

            this._Data = new Complex[size * size];
            this._RowCount = size;
            this._ColumnCount = size;
        }

        /// <summary>
        /// Initializes a new instance of the MatrixComplex class of the given size using a array
        /// </summary>
        /// <param name="size">Size</param>
        /// <param name="Data">The data</param>
        internal ComplexMatrix(int size, Complex[] Data)
        {
            if (size < 1) throw new System.ArgumentException("size < 1");

            this._Data = new Complex[size * size];
            this._RowCount = size;
            this._ColumnCount = size;
            //Si incluye la posibilidad de que los datos tengan menos valores que la matriz a crear 
            for (int i = 0; i < Math.Min(this._Data.Length, Data.Length); i++)
            {
                this._Data[i] = Data[i];
            }
        }

        #endregion


        #region Public Properties

        /// <summary>
        /// Los datos de la matriz
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        internal Complex[] Data
        {
            get { return this._Data; }
        }

        /// <summary>
        /// Returns the number of rows.
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        public int RowCount
        {
            get { return _RowCount; }
            set { _RowCount = value; }
        }
        /// <summary>
        /// Returns the number of columns.
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        public int ColumnCount
        {
            get { return _ColumnCount; }
            set { _ColumnCount = value; }
        }

        /// <summary>
        /// Gets a value indicating if the matrix is square.
        /// </summary>
        public bool IsSquare
        {
            get
            {
                bool isSquare = false;
                if (this._ColumnCount == this.RowCount) isSquare = true;
                return isSquare;
            }
        }

        /// <summary>
        /// Returns the value of a element of the matrix.
        /// </summary>
        /// <param name="row">The row value (zero-based).</param>
        /// <param name="column">The column value (zero-based).</param>
        /// <returns>The matrix value at (row, column).</returns>
        public virtual Complex this[int row, int column]
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
                this._Data[row + column * this._RowCount] = value;
            }
        }

        #endregion

        #region	 Private Methods

        /// <summary>Check if size(this) == size(B) </summary>
        private void CheckMatrixDimensions(ComplexMatrix B)
        {
            if (this._RowCount != B.RowCount || B.ColumnCount != this._ColumnCount)
            {
                throw new System.ArgumentException("Matrix dimensions must agree.");
            }
        }

        /// <summary>Check if size(this) == size(B) </summary>
        private void CheckMatrixDimensions(Matrix B)
        {
            if (this._RowCount != B.RowCount || B.ColumnCount != this._ColumnCount)
            {
                throw new System.ArgumentException("Matrix dimensions must agree.");
            }
        }
        #endregion //  Private Methods

        #region Elementary linear operations

        ///// <summary>
        ///// aij=Math.Abs(aij)
        ///// </summary>
        //public virtual void ElementsAbs()
        //{
        //    for (int i = 0; i < this.MeData.Length; i++)
        //    {
        //        this.MeData[i] =Complex.  Math.Abs(this.MeData[i]);
        //    }
        //}

        ///// <summary>
        ///// Element-by-element division: aij = aij/bij
        ///// </summary>
        ///// <param name="B"></param>
        //public virtual void ElemntsDiv(MatrixComplex B)
        //{
        //    CheckMatrixDimensions(B);
        //    Complex[] BData = B.Data;
        //    for (int i = 0; i < this.MeData.Length; i++)
        //    {
        //        this.MeData[i] /= BData[i];
        //    }
        //}

        /// <summary>
        /// Element-by-element multiplication: aij = aij*bij
        /// </summary>
        /// <param name="B">The B MatrixComplex</param>
        public virtual void ElemntsMult(ComplexMatrix B)
        {
            CheckMatrixDimensions(B);
            Complex[] BData = B.Data;
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] = this._Data[i] * BData[i];
            }
        }

        /// <summary>
        /// In place addition A=A+B
        /// </summary>
        /// <param name="B">The B MatrixComplex</param>
        public virtual void Add(ComplexMatrix B)
        {
            CheckMatrixDimensions(B);
            Complex[] BData = B.Data;
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] = this._Data[i] + BData[i];
            }
        }

        /// <summary>
        /// In place scalar-matrix multiplication, A=s*A
        /// </summary>
        /// <param name="s">The scalar s.</param>
        public virtual void Multiply(double s)
        {
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i].Real = s * this._Data[i].Real;
                this._Data[i].Imaginary = s * this._Data[i].Imaginary;
            }
        }
        /// <summary>
        /// In place scalar-matrix multiplication, A=c*A
        /// </summary>
        /// <param name="s">The scalar s.</param>
        public virtual void MultiplyC(Complex c)
        {
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] = c * this._Data[i];
            }
        }


        /// <summary>
        /// In place matrix subtraction, A=A-B.
        /// </summary>
        /// <param name="B">The B MatrixComplex.</param>
        public virtual void Subtract(ComplexMatrix B)
        {
            CheckMatrixDimensions(B);
            Complex[] BData = B.Data;
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i].Real = this._Data[i].Real - BData[i].Real;
                this._Data[i].Imaginary = this._Data[i].Imaginary - BData[i].Imaginary;
            }
        }

        /// <summary>
        /// In place unary minus -A.
        /// </summary>
        public virtual void UnaryMinus()
        {
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i].Real = -this._Data[i].Real;
                this._Data[i].Imaginary = -this._Data[i].Imaginary;
            }
        }

        #endregion

        #region Methods

        /// <summary>
        /// Gets the column vectors of this matrix.
        /// </summary>
        /// <returns>The columns vectors.</returns>
        public ComplexVector[] GetColumnVectors()
        {
            ComplexVector[] columnVects = new ComplexVector[this._ColumnCount];

            Complex[] VectData;
            for (int j = 0; j < this._ColumnCount; j++)
            {
                columnVects[j] = new ComplexVector(VectorType.Column, this._RowCount);
                VectData = columnVects[j].Data;
                for (int i = 0; i < VectData.Length; i++)
                {
                    VectData[i] = this._Data[i + j * this._RowCount];
                }
            }

            return columnVects;
        }

        /// <summary>
        /// Gets the row vectors of this matrix.
        /// </summary>
        /// <returns>The row vectors.</returns>
        public ComplexVector[] GetRowVectors()
        {
            ComplexVector[] rowVects = new ComplexVector[this.RowCount];

            Complex[] VectData;
            for (int i = 0; i < this._RowCount; i++)
            {
                rowVects[i] = new ComplexVector(VectorType.Row, this._ColumnCount);
                VectData = rowVects[i].Data;
                for (int j = 0; j < VectData.Length; j++)
                {
                    VectData[j] = this._Data[i + j * this._RowCount];
                }
            }

            return rowVects;
        }

        /// <summary>
        /// Gets a matrix that contains the real part of this matrix.
        /// </summary>
        /// <returns>A matrix that contains the real part of this matrix. </returns>
        public Matrix GetReal()
        {
            Matrix RealMatrix = new Matrix(this.RowCount, this.ColumnCount);
            double[] RealData = RealMatrix.Data; 
            for (int i = 0; i < this._Data.Length; i++)
            {
                RealData[i] = this._Data[i].Real;
            }
            return RealMatrix;
        }

        /// <summary>
        /// Gets a matrix that contains the imaginary part of this matrix.
        /// </summary>
        /// <returns>A matrix that contains the imaginary part of this matrix. </returns>
        public Matrix GetImag()
        {
            Matrix ImagMatrix = new Matrix(this.RowCount, this.ColumnCount);
            double[] ImagData = ImagMatrix.Data;
            for (int i = 0; i < this._Data.Length; i++)
            {
                ImagData[i] = this._Data[i].Imaginary;
            }
            return ImagMatrix;
        }


        /// <summary>
        /// Sets the real part of the elements of this matrix equal to the elemnets of a real matrix.
        /// </summary>
        /// <param name="RM">A matrix that contains the values of the real part.</param>
        public void SetReal(Matrix RM)
        {
            this.CheckMatrixDimensions(RM);
            double[] RealData = RM.Data;
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i].Real = RealData[i];
            }
        }

        /// <summary>
        /// Sets the imaginary part of the elements of this matrix equal to the elemnets of a real matrix.
        /// </summary>
        /// <param name="IM">A matrix that contains the values of the imaginary part.</param>
        public void SetImag(Matrix IM)
        {
            this.CheckMatrixDimensions(IM);
            double[] ImagData = IM.Data;
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i].Imaginary = ImagData[i];
            }
        }


        /// <summary>
        /// Returns the string of the  matrix.
        /// </summary>
        /// <returns>The string of the  matrix.</returns>
        public string MatrixToString()
        {
            using (StringWriter writer = new StringWriter())
            {
                for (int i = 0; i < this._RowCount; i++)
                {
                    for (int j = 0; j < this._ColumnCount; j++)
                        writer.Write(this[i, j] + ", ");
                    writer.WriteLine();
                }
                return writer.ToString();
            }
        }
        /// <summary>
        /// Returns the string of the  matrix.
        /// </summary>
        /// <param name="format">A numeric format string.</param>
        /// <returns>The string of the  matrix.</returns>
        public string MatrixToString(string format)
        {
            using (StringWriter writer = new StringWriter())
            {
                for (int i = 0; i < this._RowCount; i++)
                {
                    for (int j = 0; j < this._ColumnCount; j++)
                        writer.Write(this[i, j].ToString(format) + ", ");
                    writer.WriteLine();
                }
                return writer.ToString();
            }
        }


        ///// <summary>
        ///// maximum column sum.
        ///// </summary>
        ///// <returns>maximum column sum.</returns>
        //public double Norm1()
        //{
        //    double n = 0.0;
        //    double ColSum = 0.0;
        //    int NRows = this.MeRowCount;

        //    for (int j = 0; j < this.MeColumnCount; j++)
        //    {
        //        ColSum = 0.0;
        //        for (int i = 0; i < this.MeRowCount; i++)
        //        {
        //            ColSum += Math.Abs(this.MeData[i + j * NRows]);

        //        }
        //        n = Math.Max(n, ColSum);
        //    }
        //    return n;
        //}
        ///// <summary>
        ///// 
        ///// </summary>
        ///// <returns></returns>
        //public double InfinityNorm()
        //{
        //    double n = 0.0;
        //    double RowSum = 0.0;
        //    int NRows = this.MeRowCount;
        //    for (int i = 0; i < this.MeRowCount; i++)
        //    {
        //        RowSum = 0.0;
        //        for (int j = 0; j < this.MeColumnCount; j++)
        //        {
        //            RowSum += Math.Abs(this.MeData[i + j * NRows]);
        //        }
        //        n = Math.Max(n, RowSum);
        //    }
        //    return n;
        //}

        ///// <summary>Frobenius norm</summary>
        ///// <returns>Sqrt of sum of squares of all elements.</returns>
        //public double FrobeniusNorm()
        //{
        //    double n=0;
        //    for(int i=0; i<this.MeData.Length; i++)
        //    {
        //        n=this.Hypot(n,this.MeData[i]);
        //    }
        //    return n;
        //}

        ///// <summary>sqrt(a^2 + b^2) without under/overflow.</summary>
        //private  double Hypot(double a, double b) 
        //{
        //    double r;
        //    if (Math.Abs(a) > Math.Abs(b)) 
        //    {
        //        r = b/a;
        //        r = Math.Abs(a) * Math.Sqrt(1 + r * r);
        //    } 
        //    else if (b != 0) 
        //    {
        //        r = a/b;
        //        r = Math.Abs(b) * Math.Sqrt(1 + r * r);
        //    } 
        //    else 
        //    {
        //        r = 0.0;
        //    }
        //    return r;
        //}


        #endregion

        #region Matrix-Matrix Multiplication

        /// <summary>
        /// Matrix multiplication.
        /// </summary>
        /// <param name="A"> The left side matrix of the multiplication operator.</param>
        /// <param name="B">The right side matrix of the multiplication operator.</param>
        /// <returns>A matrix that represents the result of the matrix multiplication.</returns>
        public static ComplexMatrix operator *(ComplexMatrix A, ComplexMatrix B)
        {
            if (B.RowCount != A.ColumnCount)
            {
                throw new System.ArgumentException("Matrix dimensions are not valid.");
            }

            ComplexMatrix C = new ComplexMatrix(A.RowCount, B.ColumnCount);

            Complex[] AData = A.Data;
            Complex[] BData = B.Data;
            Complex[] CData = C.Data;

            int ARows = A.RowCount;
            int AColumns = A.ColumnCount;

            int BRows = B.RowCount;
            int BColumns = B.ColumnCount;

            Complex Sum = new Complex(0.0, 0.0);
            for (int j = 0; j < BColumns; j++)
            {
                for (int i = 0; i < ARows; i++)
                {
                    Sum.Imaginary = 0.0;
                    Sum.Real = 0.0;
                    for (int k = 0; k < AColumns; k++)
                    {
                        Sum += AData[i + k * ARows] * BData[k + j * BRows];
                    }
                    CData[i + j * ARows] = Sum;
                }
            }
            return C;
        }

        /// <summary>
        /// Matrix multiplication.
        /// </summary>
        /// <param name="A"> The left side matrix of the multiplication operator.</param>
        /// <param name="B">The right side matrix of the multiplication operator.</param>
        /// <returns>A matrix that represents the result of the matrix multiplication.</returns>
        public static ComplexMatrix operator *(BaseMatrix A, ComplexMatrix B)
        {
            if (B.RowCount != A.ColumnCount)
            {
                throw new System.ArgumentException("Matrix dimensions are not valid.");
            }

            ComplexMatrix C = new ComplexMatrix(A.RowCount, B.ColumnCount);

            double[] AData = A.Data;
            Complex[] BData = B.Data;
            Complex[] CData = C.Data;

            int ARows = A.RowCount;
            int AColumns = A.ColumnCount;

            int BRows = B.RowCount;
            int BColumns = B.ColumnCount;

            Complex Sum = new Complex(0.0, 0.0);
            for (int j = 0; j < BColumns; j++)
            {
                for (int i = 0; i < ARows; i++)
                {
                    Sum.Imaginary = 0.0;
                    Sum.Real = 0.0;
                    for (int k = 0; k < AColumns; k++)
                    {
                        Sum += AData[i + k * ARows] * BData[k + j * BRows];
                    }
                    CData[i + j * ARows] = Sum;
                }
            }
            return C;
        }

        /// <summary>complex-Matrix multiplication.</summary>
        /// <param name="c"> The left side scalar of the multiplication operator.</param>
        /// <param name="B">The right side matrix of the multiplication operator.</param>
        /// <returns>A matrix that represents the result of the multiplication.</returns>
        public static ComplexMatrix operator *(Complex c, ComplexMatrix B)
        {

            ComplexMatrix C = new ComplexMatrix(B.RowCount, B.ColumnCount);

            Complex[] BData = B.Data;
            Complex[] CData = C.Data;


            for (int i = 0; i < BData.Length; i++)
            {
                CData[i] = c * BData[i];
            }
            return C;
        }


        #endregion

        #region Matrix-Matrix Addition
        /// <summary>
        /// Matrix addition.
        /// </summary>
        /// <param name="A">The left side matrix of the addition operator.</param>
        /// <param name="B">The right side matrix of the addition operator.</param>
        /// <returns>A matrix that represents the result of the matrix addition.</returns>
        public static ComplexMatrix operator +(ComplexMatrix A, ComplexMatrix B)
        {
            if (B.RowCount != A.RowCount || B.ColumnCount != A.ColumnCount)
            {
                throw new System.ArgumentException("Matrix dimensions are not valid.");
            }

            ComplexMatrix C = new ComplexMatrix(A.RowCount, A.ColumnCount);

            Complex[] AData = A.Data;
            Complex[] BData = B.Data;
            Complex[] CData = C.Data;

            for (int i = 0; i < AData.Length; i++)
            {
                CData[i] = AData[i] + BData[i];
            }

            return C;
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
        public static ComplexMatrix operator -(ComplexMatrix A, ComplexMatrix B)
        {
            if (B.RowCount != A.RowCount || B.ColumnCount != A.ColumnCount)
            {
                throw new System.ArgumentException("Matrix dimensions are not valid.");
            }

            ComplexMatrix C = new ComplexMatrix(A.RowCount, A.ColumnCount);

            Complex[] AData = A.Data;
            Complex[] BData = B.Data;
            Complex[] CData = C.Data;

            for (int i = 0; i < AData.Length; i++)
            {
                CData[i] = AData[i] - BData[i];
            }

            return C;
        }

        #endregion


        #region IMatrix<Complex> Members

        /// <summary>
        /// Copy all elements of this matrix to a rectangular 2D array.
        /// </summary>
        /// <returns>A rectangular 2D array.</</returns>
        public Complex[,] CopyToArray()
        {
            Complex[,] matrixData = new Complex[this._RowCount, this._ColumnCount];

            for (int j = 0; j < this._ColumnCount; j++)
            {
                for (int i = 0; i < this._RowCount; i++)
                {
                    matrixData[i, j] = this._Data[i + j * this._RowCount];
                }
            }

            return matrixData;
        }
        /// <summary>
        /// Copy all elements of this matrix to a jagged array.
        /// </summary>
        /// <returns>A jagged array.</returns>
        public Complex[][] CopyToJaggedArray()
        {
            Complex[][] newData = new Complex[this._RowCount][];
            for (int i = 0; i < this._RowCount; i++)
            {
                Complex[] row = new Complex[this._ColumnCount];
                for (int j = 0; j < this._ColumnCount; j++)
                {
                    row[j] = this._Data[i + j * this._RowCount];
                }

                newData[i] = row;
            }

            return newData;
        }

        #endregion
    }
}