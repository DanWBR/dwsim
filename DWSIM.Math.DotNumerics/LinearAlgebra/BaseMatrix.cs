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
using DotNumerics.LinearAlgebra.CSLapack;


namespace DotNumerics.LinearAlgebra
{

    /// <summary>
    /// Represents a Base Matrix.
    /// </summary>
    [DebuggerDisplay("[{RowCount},{ColumnCount}]")]
    //[DebuggerDisplay("[{RowCount},{ColumnCount}]", Name = "BandMatrix")]
    [DebuggerTypeProxy(typeof(MatrixDebuggerDisplay))]
    public abstract class BaseMatrix: IMatrix<double>
    {
        #region Static Fields

        internal static DGETRF _dgetrf;
        internal static DGETRI _dgetri;

        #endregion


        #region Fields
        /// <summary>
        /// Los datos de la matriz, los datos se almacenan en un un array unidimensional,
        /// Los elementos se almacenan por columnas, esto para que sean compatible con los Arrays de Fortran
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        protected double[] _Data;
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
        /// Initializes a new instance of the BaseMatrix class of the given size.
        /// </summary>
        /// <param name="rows">Number of rows.</param>
        /// <param name="columns">Number of columns.</param>
        public BaseMatrix(int rows, int columns)
        {
            if (rows < 1) throw new System.ArgumentException("rows < 1");
            if (columns < 1) throw new System.ArgumentException("columns < 1");

            this._Data = new double[rows * columns];
            this._RowCount = rows;
            this._ColumnCount = columns;
        }

        /// <summary>
        /// Initializes a new instance of the BaseMatrix class of the given size using a array
        /// </summary>
        /// <param name="rows">Number of rows.</param>
        /// <param name="columns">Number of columns.</param>
        /// <param name="data">The data, the data is copied.</param>
        internal BaseMatrix(int rows, int columns, double[] data)
        {
            if (rows < 1) throw new System.ArgumentException("rows < 1");
            if (columns < 1) throw new System.ArgumentException("columns < 1");
            this._Data = new double[rows * columns];
            this._RowCount = rows;
            this._ColumnCount = columns;

            data.CopyTo(this._Data, 0);
            ////Si incluye la posibilidad de que los datos tengan menos valores que la matriz a crear 
            //for (int i = 0; i < Math.Min(this.MeData.Length, data.Length); i++)
            //{
            //    this.MeData[i] = data[i];
            //}
        }

        /// <summary>
        /// Initializes a new instance of the BaseMatrix class of the given size.
        /// </summary>
        /// <param name="size">Size</param>
        public BaseMatrix(int size)
        {
            if (size < 1) throw new System.ArgumentException("size < 1");

            this._Data = new double[size * size];
            this._RowCount = size;
            this._ColumnCount = size;
        }

        /// <summary>
        /// Initializes a new instance of the BaseMatrix class of the given size using a array
        /// </summary>
        /// <param name="size">Size</param>
        /// <param name="data">The data</param>
        internal BaseMatrix(int size, double[] data)
        {
            if (size < 1) throw new System.ArgumentException("size < 1");

            this._Data = new double[size * size];
            this._RowCount = size;
            this._ColumnCount = size;

            data.CopyTo(this._Data, 0);

            ////Si incluye la posibilidad de que los datos tengan menos valores que la matriz a crear 
            //for (int i = 0; i < Math.Min(this.MeData.Length, data.Length); i++)
            //{
            //    this.MeData[i] = Data[i];
            //}
        }

        #endregion


        #region Public Properties

        /// <summary>
        /// Los datos de la matriz
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        internal double[] Data
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
            //set { MeRowCount = value; }
        }
        /// <summary>
        /// Returns the number of columns.
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        public int ColumnCount
        {
            get { return _ColumnCount; }
            //set { MeColumnCount = value; }
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
        /// Gets or set the value of a element of this matrix.
        /// </summary>
        /// <param name="row">The row value (zero-based).</param>
        /// <param name="column">The column value (zero-based).</param>
        /// <returns>The matrix element at (row, column).</returns>
        public virtual double this[int row, int column]
        {
            get
            {
                if (column >= this._ColumnCount)
                {
                    throw new ArgumentException("Index was outside the bounds of the matrix.");             
                }

                return this._Data[row + column  * this._RowCount];
            }
            set
            {
                if (column >= this._ColumnCount)
                {
                    throw new ArgumentException("Index was outside the bounds of the matrix.");             
                }

                this._Data[row  + column * this._RowCount] = value;
            }
        }

        #endregion

        #region	 Private Methods

        /// <summary>Check if size(this) == size(B) </summary>
        internal protected void CheckMatrixDimensions(BaseMatrix B)
        {
            if (this._RowCount != B.RowCount || B.ColumnCount != this._ColumnCount)
            {
                throw new System.ArgumentException("Matrix dimensions must agree.");
            }
        }
        #endregion //  Private Methods

        #region Elementary linear operations

        /// <summary>
        /// aij=Math.Abs(aij)
        /// </summary>
        public virtual void ElementsAbs()
        {
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] = Math.Abs(this._Data[i]);
            }
        }

        /// <summary>
        /// Element-by-element division: aij = aij/bij
        /// </summary>
        /// <param name="B">The B Matrix.</param>
        public virtual void ElemntsDiv(BaseMatrix B)
        {
            CheckMatrixDimensions(B);
            double[] BData = B.Data;
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] /= BData[i];
            }
        }

        /// <summary>
        /// Element-by-element multiplication: aij = aij*bij
        /// </summary>
        /// <param name="B">The B Matrix.</param>
        public virtual void ElemntsMult(BaseMatrix B)
        {
            CheckMatrixDimensions(B);
            double[] BData = B.Data;
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] *= BData[i];
            }
        }

        /// <summary>
        /// Addition C=A+B
        /// </summary>
        /// <param name="B">The Matrix</param>
        /// <returns>C=A+B</returns>
        public virtual Matrix Add(BaseMatrix B)
        {
            CheckMatrixDimensions(B);

            Matrix C = new Matrix(this._RowCount, this._ColumnCount);

            double[] BData = B.Data;
            double[] dataC = C.Data;
            for (int i = 0; i < this._Data.Length; i++)
            {
                dataC[i] = this._Data[i] + BData[i];
            }

            return C;
        }



        /// <summary>
        /// In place scalar-matrix multiplication, A=s*A
        /// </summary>
        /// <param name="s">The scalar</param>
        public virtual void MultiplyInplace(double s)
        {
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] *= s;
            }
        }

        /// <summary>
        /// Scalar-matrix multiplication, C=s*A
        /// </summary>
        /// <param name="s">The scalar</param>
        /// <returns>C=s*A</returns>
        public Matrix Multiply(double s)
        {
            Matrix C = new Matrix(this._RowCount, this._ColumnCount);

            double[] dataC = C.Data;
            for (int i = 0; i < this._Data.Length; i++)
            {
                dataC[i] = this._Data[i] * s;
            }

            return C;
        }

        /// <summary>
        /// Matrix-Matrix multiplication, C=A*B
        /// </summary>
        /// <param name="B">The matrix.</param>
        /// <returns>C=A*B</returns>
        public Matrix Multiply(BaseMatrix B)
        {

            if (B.RowCount != this.ColumnCount)
            {
                throw new System.ArgumentException("Matrix dimensions are not valid.");
            }

            Matrix C = new Matrix(this.RowCount, B.ColumnCount);

            double[] AData = this.Data;
            double[] BData = B.Data;
            double[] CData = C.Data;

            int ARows = this.RowCount;
            int AColumns = this.ColumnCount;

            int BRows = B.RowCount;
            int BColumns = B.ColumnCount;

            double Sum = 0.0;
            int indexBJ;
            int indexAJ;
            for (int j = 0; j < BColumns; j++)
            {
                indexBJ = j * BRows;
                indexAJ = j * ARows;
                for (int i = 0; i < ARows; i++)
                {
                    Sum = 0.0;
                    for (int k = 0; k < AColumns; k++)
                    {
                        Sum += AData[i + k * ARows] * BData[k + indexBJ];
                    }
                    CData[i + indexAJ] = Sum;
                }
            }
            return C;




            //To reading every time elements from array , why we are taking some group of element i.e. Block size, then no need to read every element. A groups of element will be on catche and we can do fast as given above algo. This algorithm called " Block Algorithm". This Block algorithm can be applied many place where this type of situation will come.

            //Block Algorithm for Matrix Multiplication:

            //Code: C
            //            #define n 1000
            //int main()
            //{
            //    int a[n][n],b[n][n],c[n][n];
            //    c[0][0]=0;
            //    for( i=0;i<n;++i)
            //    {
            //        for(j=0;j<n;++j)
            //        {
            //            for(k=0;k<n;++k)
            //            {
            //                c[i][j] = c[i][j] + a[i][k] * b[k][j]
            //            }
            //        }
            //    }
            //    return 0;
            //}
            //To reading every time elements from array , why we are taking some group of element i.e. Block size, then no need to read every element. A groups of element will be on catche and we can do fast as given above algo. This algorithm called " Block Algorithm". This Block algorithm can be applied many place where this type of situation will come.

            //Block Algorithm for Matrix Multiplication:

            //Code: C

            //#define n 1000
            //#define BlockSize  100
            //int main()
            //{
            //    int a[n][n],b[n][n],c[n][n];
            //    c[0][0]=0;
            //    for( i1=0;i1<(n/BlockSize);++i1)
            //    {
            //        for(j1=0;j1<(n/BlockSize);++j1)
            //        {
            //            for(k1=0;k1<(n/BlockSize);++k1)
            //            {
            //                for(i=i1=0;i<min(i1+BlockSize-1);++i)
            //                {
            //                    for(j=j1=0;j<min(j1+BlockSize-1);++j)
            //                    {
            //                        for(k=k1;k<min(k1+BlockSize-1);++k)
            //                        {               
            //                            c[i][j] = c[i][j] + a[i][k] * b[k][j]
            //                        }
            //                    }
            //                }
            //            }
            //        }
            //           }
            // return 0;
            //}
        }

        /// <summary>
        /// Matrix subtraction, C=A-B
        /// </summary>
        /// <param name="B">The Matrix</param>
        /// <returns>C=A-B</returns>
        public Matrix  Subtract(BaseMatrix B)
        {
            CheckMatrixDimensions(B);

            Matrix C = new Matrix(this._RowCount, this._ColumnCount);

            double[] BData = B.Data;
            double[] dataC = C.Data;
            for (int i = 0; i < this._Data.Length; i++)
            {
                dataC[i] = this._Data[i] - BData[i];
            }

            return C;
        }

        /// <summary>
        /// In place unary minus -A
        /// </summary>
        public virtual void UnaryMinusInplace()
        {
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] = -this._Data[i];
            }
        }

        #endregion

        #region Methods

        /// <summary>
        /// Calculates the inverse of the matrix.
        /// </summary>
        /// <returns>The inverse of the matrix.</returns>
        public Matrix Inverse()
        {

            if (this.IsSquare != true)
            {
                throw new System.ArgumentException("This is not a square matrix.");
            }

            if (BaseMatrix._dgetrf == null)
            {
                BaseMatrix._dgetrf = new DGETRF();
            }
            if (BaseMatrix._dgetri == null)
            {
                BaseMatrix._dgetri = new DGETRI();
            }


            Matrix inverseMatrix= new Matrix(this.RowCount, this.ColumnCount, this.Data);

            double[] inverseData= inverseMatrix.Data;

            int[] ipiv= new int[this.RowCount];

           

             int Info = 0;

            double[] Work = new double[1];
            int LWork = -1;

            //Calculamos LWORK 
            BaseMatrix._dgetri.Run(this.RowCount, ref inverseData, 0, this.RowCount, ipiv, 0, ref Work, 0, LWork, ref Info);
            LWork = Convert.ToInt32(Work[0]);
            if (LWork > 0)
            {
                Work = new double[LWork];

                BaseMatrix._dgetrf.Run(this.RowCount, this.ColumnCount, ref inverseData, 0, this.RowCount, ref ipiv, 0, ref Info);


                #region Error
                /// = 0:  successful exit
                /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
                /// .GT. 0:  if INFO = i, U(i,i) is exactly zero. The factorization
                /// has been completed, but the factor U is exactly
                /// singular, and division by zero will occur if it is used
                /// to solve a system of equations.

                if (Info < 0)
                {
                    string infoSTg = Math.Abs(Info).ToString();
                    throw new ArgumentException("the " + infoSTg + " -th argument had an illegal value");
                }
                else if (Info > 0)
                {
                    string infoSTg = Math.Abs(Info).ToString();
                    throw new Exception("The matrix is numerically singular..");
                }

                #endregion


                BaseMatrix._dgetri.Run(this.RowCount, ref inverseData, 0, this.RowCount, ipiv, 0, ref Work, 0, LWork, ref Info);
            }
            else
            {

                //Error
            }

            #region Error
            /// (output) INTEGER
            /// = 0:  successful exit
            /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
            /// .GT. 0:  if INFO = i, U(i,i) is exactly zero; the matrix is
            /// singular and its inverse could not be computed.

            if (Info < 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new ArgumentException("the " + infoSTg + " -th argument had an illegal value");
            }
            else if (Info > 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new Exception("The matrix is numerically singular..");
            }

            #endregion

            return inverseMatrix;
        }

        /// <summary>
        /// Calculates the determinant of the matrix.
        /// </summary>
        /// <returns>The determinant of the matrix.</returns>
        public double Determinant()
        {
            double det = 1.0;


            if (this.IsSquare != true)
            {
                throw new System.ArgumentException("This is not a square matrix.");
            }

            if (BaseMatrix._dgetrf == null)
            {
                BaseMatrix._dgetrf = new DGETRF();
            }


            Matrix clonMatrix = new Matrix(this.RowCount, this.ColumnCount, this.Data);

            double[] clonData = clonMatrix.Data;

            int[] ipiv = new int[this.RowCount];



            int Info = 0;



            BaseMatrix._dgetrf.Run(this.RowCount, this.ColumnCount, ref clonData, 0, this.RowCount, ref ipiv, 0, ref Info);


            #region Error
            // <param name="INFO">
            // (output) INTEGER
            //= 0:  successful exit
            // .LT. 0:  if INFO = -i, the i-th argument had an illegal value
            // .GT. 0:  if INFO = i, U(i,i) is exactly zero. The factorization
            // has been completed, but the factor U is exactly
            // singular, and division by zero will occur if it is used
            // to solve a system of equations.
            if (Info < 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new ArgumentException("the " + infoSTg + " -th argument had an illegal value");
            }
            //else if (Info > 0)
            //{
            //    string infoSTg = Math.Abs(Info).ToString();
            //    throw new Exception("The matrix is numerically singular..");
            //}

            #endregion



            //The LU factorization yields three matrices, the product of which is the original 
            //complex matrix. Therefore the determinat is the product of the three determinants 
            //of P, L and U. The determinant of the triangular matrices L and U is the product 
            //of the elements on the diagonal - as for any triangular matrix (for L this is 1 
            //as all elements of the diagonal are one.) The determinant of P is either +1 or -1 
            //depending of whether the number of row permutations is even or odd. 

            //Thank you very much for your answer. It seems to be that your message got tructated somehow, but I think I got the point.
            //I also did some searching on the web and found the following two pieces of code which both claim to calculate the determinant of a square matrix.

            //============================================

            //call dgetrf(n,n,a,n,piv,info)
            //det = 0d0
            //if (info.ne.0) then
            //return
            //endif
            //det = 1d0
            //do 10,i=1,n
            //if (piv(i).ne.i) then
            //det = -det * a(i,i)
            //else
            //det = det * a(i,i)
            //endif
            //10 continue
            //end



            for (int i = 0; i < this._RowCount; i++)
            {
                if (ipiv[i] != (i + 1))  // i+1 debido a que aqui la base es 0 y en fortran es 1
                {
                    det *= -clonMatrix[i, i];
                }
                else
                {
                    det *= clonMatrix[i, i];
                }

            }

            return det;
        }

        /// <summary>
        /// Gets the column vectors of this matrix.
        /// </summary>
        /// <returns>The columns vectors.</returns>
        public Vector[] GetColumnVectors()
        {
            Vector[] columnVects = new Vector[this._ColumnCount];

            double[] VectData;
            for (int j = 0; j < this._ColumnCount; j++)
            {
                columnVects[j] = new Vector(VectorType.Column, this._RowCount);
                VectData = columnVects[j].Data;
                for (int i = 0; i < VectData.Length; i++)
                {
                    VectData[i] = this._Data[i + j * this._RowCount];
                }
            }

            return columnVects;
        }


        /// <summary>
        ///  Gets a column vector of this matrix at the selected position.
        /// </summary>
        /// <param name="columnIndex">The column index (zero-based).</param>
        /// <returns>The column vector.</returns>
        public Vector GetColumnVector(int columnIndex)
        {

            if (columnIndex >= this._ColumnCount)
            {
                throw new System.ArgumentException("columnIndex >= number of columns.");
            }

            if (columnIndex < 0)
            {
                throw new System.ArgumentException("columnIndex < 0");
            }

            Vector columnVect;

            double[] VectData;
            columnVect = new Vector(VectorType.Column, this._RowCount);
            VectData = columnVect.Data;
            for (int i = 0; i < VectData.Length; i++)
            {
                VectData[i] = this._Data[i + columnIndex * this._RowCount];
            }

            return columnVect;
        }

        /// <summary>
        ///  Gets a column array of this matrix at the selected position.
        /// </summary>
        /// <param name="columnIndex">The column index (zero-based).</param>
        /// <returns>The column array.</returns>
        public double[] GetColumnArray(int columnIndex)
        {

            if (columnIndex >= this._ColumnCount)
            {
                throw new System.ArgumentException("columnIndex >= number of columns.");
            }

            if (columnIndex < 0)
            {
                throw new System.ArgumentException("columnIndex < 0");
            }

            double[] VectData = new double[this._RowCount];
            for (int i = 0; i < VectData.Length; i++)
            {
                VectData[i] = this._Data[i + columnIndex * this._RowCount];
            }

            return VectData;
        }


        /// <summary>
        /// Gets the row vectors of this matrix.
        /// </summary>
        /// <returns>The row vectors.</returns>
        public Vector[] GetRowVectors()
        {
            Vector[] rowVects = new Vector[this.RowCount];

            double[] VectData;
            for (int i = 0; i < this._RowCount; i++)
            {
                rowVects[i] = new Vector(VectorType.Row, this._ColumnCount);
                VectData = rowVects[i].Data;
                for (int j = 0; j < VectData.Length; j++)
                {
                    VectData[j] = this._Data[i + j * this._RowCount];
                }
            }

            return rowVects;
        }


        /// <summary>
        ///  Gets a row vector of this matrix at the selected position.
        /// </summary>
        /// <param name="rowIndex">The row index (zero-based).</param>
        /// <returns>The row vector.</returns>
        public Vector GetRowVector(int rowIndex)
        {

            if (rowIndex >= this._RowCount)
            {
                throw new System.ArgumentException("rowIndex >= number of rows.");
            }

            if (rowIndex < 0)
            {
                throw new System.ArgumentException("rowIndex < 0");
            }

            Vector rowVect;

            double[] VectData;
            rowVect = new Vector(VectorType.Row, this._ColumnCount);
            VectData = rowVect.Data;
            for (int j = 0; j < VectData.Length; j++)
            {
                VectData[j] = this._Data[rowIndex + j * this._RowCount];
            }

            return rowVect;
        }

        /// <summary>
        ///  Gets a row array of this matrix at the selected position.
        /// </summary>
        /// <param name="rowIndex">The row index (zero-based).</param>
        /// <returns>The row array.</returns>
        public double[] GetRowArray(int rowIndex)
        {

            if (rowIndex >= this._RowCount)
            {
                throw new System.ArgumentException("rowIndex >= number of rows.");
            }

            if (rowIndex < 0)
            {
                throw new System.ArgumentException("rowIndex < 0");
            }

            double[] VectData = new double[this._ColumnCount];

            for (int j = 0; j < VectData.Length; j++)
            {
                VectData[j] = this._Data[rowIndex + j * this._RowCount];
            }
            //}

            return VectData;
        }


        /// <summary>
        /// Returns the equivalent string representation of the matrix.
        /// </summary>
        /// <returns>The string representation of the  matrix.</returns>
        public string MatrixToString()
        {
            using (StringWriter writer = new StringWriter())
            {
                for (int i = 0; i < this._RowCount; i++)
                {
                    for (int j = 0; j < this._ColumnCount; j++)
                        writer.Write(this[i, j].ToString() + ", ");
                    writer.WriteLine();
                }
                return writer.ToString();
            }
        }

        /// <summary>
        /// Returns the equivalent string representation of the matrix.
        /// </summary>
        /// <param name="format">A numeric format string.</param>
        /// <returns>The string representation of the  matrix.</returns>
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


        /// <summary>
        /// One Norm for the matrix.
        /// </summary>
        /// <returns>The maximum column sum.</returns>
        public double Norm1()
        {
            double n = 0.0;
            double ColSum = 0.0;
            int NRows = this._RowCount;

            for (int j = 0; j < this._ColumnCount; j++)
            {
                ColSum = 0.0;
                for (int i = 0; i < this._RowCount; i++)
                {
                    ColSum += Math.Abs(this._Data[i + j * NRows]);

                }
                n = Math.Max(n, ColSum);
            }
            return n;
        }
        /// <summary>
        /// Infinity Norm for the matrix.
        /// </summary>
        /// <returns>The maximum row sum.</returns>
        public double NormInf()
        {
            double n = 0.0;
            double RowSum = 0.0;
            int NRows = this._RowCount;
            for (int i = 0; i < this._RowCount; i++)
            {
                RowSum = 0.0;
                for (int j = 0; j < this._ColumnCount; j++)
                {
                    RowSum += Math.Abs(this._Data[i + j * NRows]);
                }
                n = Math.Max(n, RowSum);
            }
            return n;
        }

        /// <summary>Frobenius norm</summary>
        /// <returns>The square root of sum of squares of all elements.</returns>
        public double FrobeniusNorm()
        {
            double n=0;
            for(int i=0; i<this._Data.Length; i++)
            {
                n=this.Hypot(n,this._Data[i]);
            }
            return n;
        }

        /// <summary>sqrt(a^2 + b^2) without under/overflow.</summary>
		private  double Hypot(double a, double b) 
		{
			double r;
			if (Math.Abs(a) > Math.Abs(b)) 
			{
				r = b/a;
				r = Math.Abs(a) * Math.Sqrt(1 + r * r);
			} 
			else if (b != 0) 
			{
				r = a/b;
				r = Math.Abs(b) * Math.Sqrt(1 + r * r);
			} 
			else 
			{
				r = 0.0;
			}
			return r;
		}

        /// <summary>
        /// Sum of elements =SUMij(A[i,j])
        /// </summary>
        /// <returns>The sum of elements.</returns>
        public double ElementsSum()
        {
            double TemSum = 0.0;
            for (int i = 0; i < this._Data.Length; i++)
            {
                TemSum += this._Data[i];
            }
            return TemSum;
        }
        /// <summary>
        /// Transposed matrix.
        /// </summary>
        /// <returns>The transposed matrix.</returns>
        public Matrix Transpose()
        {
            Matrix AT = new Matrix(this._ColumnCount, this._RowCount);
            int ATRows = AT.RowCount;
            int ATColumns = AT.ColumnCount;
            double[] ATData = AT.Data;
            for (int j = 0; j < this._ColumnCount; j++)
            {
                for (int i = 0; i < this._RowCount; i++)
                {
                    ATData[j + i * ATRows] = this._Data[i + j * this._RowCount];
                }
            }
            return AT;
        }

        /// <summary>Returns the trace of the matrix.</summary>
        /// <returns>Sum of the diagonal elements.</returns>
        public double Trace
        {
            get
            {
                double trace = 0;
                for (int i = 0; i < Math.Min(this.RowCount, this.ColumnCount ); i++)
                {
                    trace += this[i, i];
                }
                return trace;
            }
        }

        #endregion

        #region Matrix-Matrix Multiplication

        /// <summary>
        /// Matrix multiplication.
        /// </summary>
        /// <param name="A"> The left side matrix of the multiplication operator.</param>
        /// <param name="B">The right side matrix of the multiplication operator.</param>
        /// <returns>A matrix that represents the result of the matrix multiplication.</returns>
        public static Matrix operator *(BaseMatrix A, BaseMatrix B)
        {
            return A.Multiply(B);
        }

        #endregion

        #region Matrix-Matrix Addition

        /// <summary>
        /// Matrix addition.
        /// </summary>
        /// <param name="A">The left side matrix of the addition operator.</param>
        /// <param name="B">The right side matrix of the addition operator.</param>
        /// <returns>A matrix that represents the result of the matrix addition.</returns>
        public static Matrix operator +(BaseMatrix A, BaseMatrix B)
        {
            return A.Add(B);
        }


        #endregion

        #region Matrix-Matrix Subtraction

        /// <summary>
        /// Matrix subtraction.
        /// </summary>
        /// <param name="A"> The left side matrix of the subtraction operator.</param>
        /// <param name="B">The right side matrix of the subtraction operator.</param>
        /// <returns>A matrix that represents the result of the matrix subtraction.</returns>
        public static Matrix operator -(BaseMatrix A, BaseMatrix B)
        {
            return A.Subtract(B);
        }


        #endregion


        #region IMatrix<double> Members

        /// <summary>
        /// Copy all elements of this matrix to a rectangular 2D array.
        /// </summary>
        /// <returns>A rectangular 2D array.</returns>
        public double[,] CopyToArray()
        {
            double[,] matrixData = new double[this._RowCount, this._ColumnCount];

            for (int j = 0; j < this._ColumnCount; j++)
            {
                for (int i = 0; i < this._RowCount; i++)
                {
                    matrixData[i,j] = this._Data[i + j * this._RowCount];
                }
            }

            return matrixData;
        }

        /// <summary>
        /// Copy all elements of this matrix to a jagged array.
        /// </summary>
        /// <returns>A jagged array.</returns>
        public double[][] CopyToJaggedArray()
        {

            double[][] newData = new double[this._RowCount][];
            for (int i = 0; i < this._RowCount; i++)
            {
                double[] row = new double[this._ColumnCount];
                for (int j = 0; j < this._ColumnCount; j++)
                {
                    row[j] = this._Data[i + j * this._RowCount];
                }

                newData[i] = row;
            }

            return newData;
        }

        public ComplexMatrix CopyToComplex()
        {
            ComplexMatrix complexMatrix = new ComplexMatrix(this._RowCount, this._ColumnCount);
            Complex[] data = complexMatrix.Data;
            for (int i = 0; i < this._Data.Length; i++)
            {
                data[i].Real = this._Data[i];
            }

            return complexMatrix;
        }

        #endregion
    }
}
