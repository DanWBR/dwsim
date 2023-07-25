namespace Mapack
{
    using System;
    using System.IO;
    using System.Globalization;

    /// <summary>Matrix provides the fundamental operations of numerical linear algebra.</summary>
    public class Matrix
    {
        private double[][] data;
        private int rows;
        private int columns;

        private static Random random = new Random();
    
        /// <summary>Constructs an empty matrix of the given size.</summary>
        /// <param name="rows">Number of rows.</param>
        /// <param name="columns">Number of columns.</param>
        public Matrix(int rows, int columns)
        {
            this.rows = rows;
            this.columns = columns;
            this.data = new double[rows][];
            for (int i = 0; i < rows; i++)
            {
                this.data[i] = new double[columns];
            }
        }
    
        /// <summary>Constructs a matrix of the given size and assigns a given value to all diagonal elements.</summary>
        /// <param name="rows">Number of rows.</param>
        /// <param name="columns">Number of columns.</param>
        /// <param name="value">Value to assign to the diagnoal elements.</param>
        public Matrix(int rows, int columns, double value)
        {
            this.rows = rows;
            this.columns = columns;
            this.data = new double[rows][];

            for (int i = 0; i < rows; i++)
            {
                data[i] = new double[columns];
            }

            for (int i = 0; i < rows; i++)
            {
                data[i][i] = value;
            }
        }
    
        /// <summary>Constructs a matrix from the given array.</summary>
        /// <param name="value">The array the matrix gets constructed from.</param>
        [CLSCompliant(false)]
        public Matrix(double[][] value)
        {
            this.rows = value.Length;
            this.columns = value[0].Length;
    
            for (int i = 0; i < rows; i++)
            {
                if (value[i].Length != columns)
                {
                    throw new ArgumentException("Argument out of range."); 
                }
            }
    
            this.data = value;
        }           
    
        /// <summary>Determines weather two instances are equal.</summary>
        public override bool Equals(object obj)
        {
            return Equals(this, (Matrix) obj);
        }

        /// <summary>Determines weather two instances are equal.</summary>
        public static bool Equals(Matrix left, Matrix right)
        {
            if (((object) left) == ((object) right))
            {
                return true;
            }

            if ((((object) left) == null) || (((object) right) == null))
            {
                return false;
            }

            if ((left.Rows != right.Rows) || (left.Columns != right.Columns))
            {
                return false;
            }

            for (int i = 0; i < left.Rows; i++)
            {
                for (int j = 0; j < left.Columns; j++)
                {
                    if (left[i, j] != right[i, j])
                    {
                        return false;   
                    }   
                }   
            }
            
            return true;
        }

        /// <summary>Serves as a hash function for a particular type, suitable for use in hashing algorithms and data structures like a hash table.</summary>
        public override int GetHashCode()
        {
            return (this.Rows + this.Columns);
        }

        internal double[][] Array
        {
            get 
            { 
                return this.data; 
            }
        }
    
        /// <summary>Returns the number of columns.</summary>
        public int Rows
        {
            get 
            { 
                return this.rows; 
            }
        }

        /// <summary>Returns the number of columns.</summary>
        public int Columns
        {
            get 
            { 
                return this.columns; 
            }
        }

        /// <summary>Return <see langword="true"/> if the matrix is a square matrix.</summary>
        public bool Square
        {
            get 
            { 
                return (rows == columns); 
            }
        }

        /// <summary>Returns <see langword="true"/> if the matrix is symmetric.</summary>
        public bool Symmetric
        {
            get
            {
                if (this.Square)
                {
                    for (int i = 0; i < rows; i++)
                    {
                        for (int j = 0; j <= i; j++)
                        {
                            if (data[i][j] != data[j][i])
                            {
                                return false;
                            }
                        }
                    }

                    return true;
                }

                return false;
            }
        }

        /// <summary>Access the value at the given location.</summary>
        public double this[int row, int column]
        {
            set 
            { 
                this.data[row][column] = value; 
            }
            
            get 
            { 
                return this.data[row][column]; 
            }
        }

        /// <summary>Returns a sub matrix extracted from the current matrix.</summary>
        /// <param name="startRow">Start row index</param>
        /// <param name="endRow">End row index</param>
        /// <param name="startColumn">Start column index</param>
        /// <param name="endColumn">End column index</param>
        public Matrix Submatrix(int startRow, int endRow, int startColumn, int endColumn)
        {
            if ((startRow > endRow) || (startColumn > endColumn) ||  (startRow < 0) || (startRow >= this.rows) ||  (endRow < 0) || (endRow >= this.rows) ||  (startColumn < 0) || (startColumn >= this.columns) ||  (endColumn < 0) || (endColumn >= this.columns)) 
            { 
                throw new ArgumentException("Argument out of range."); 
            } 
            
            Matrix X = new Matrix(endRow - startRow + 1, endColumn - startColumn + 1);
            double[][] x = X.Array;
            for (int i = startRow; i <= endRow; i++)
            {
                for (int j = startColumn; j <= endColumn; j++)
                {
                    x[i - startRow][j - startColumn] = data[i][j];
                }
            }
                    
            return X;
        }

        /// <summary>Returns a sub matrix extracted from the current matrix.</summary>
        /// <param name="rowIndexes">Array of row indices</param>
        /// <param name="columnIndexes">Array of column indices</param>
        public Matrix Submatrix(int[] rowIndexes, int[] columnIndexes)
        {
            Matrix X = new Matrix(rowIndexes.Length, columnIndexes.Length);
            double[][] x = X.Array;
            for (int i = 0; i < rowIndexes.Length; i++)
            {
                for (int j = 0; j < columnIndexes.Length; j++)
                {
                    if ((rowIndexes[i] < 0) || (rowIndexes[i] >= rows) || (columnIndexes[j] < 0) || (columnIndexes[j] >= columns))
                    { 
                        throw new ArgumentException("Argument out of range."); 
                    }

                    x[i][j] = data[rowIndexes[i]][columnIndexes[j]];
                }
            }

            return X;
        }

        /// <summary>Returns a sub matrix extracted from the current matrix.</summary>
        /// <param name="i0">Starttial row index</param>
        /// <param name="i1">End row index</param>
        /// <param name="c">Array of row indices</param>
        public Matrix Submatrix(int i0, int i1, int[] c)
        {
            if ((i0 > i1) || (i0 < 0) || (i0 >= this.rows) || (i1 < 0) || (i1 >= this.rows)) 
            { 
                throw new ArgumentException("Argument out of range."); 
            } 
            
            Matrix X = new Matrix(i1 - i0 + 1, c.Length);
            double[][] x = X.Array;
            for (int i = i0; i <= i1; i++)
            {
                for (int j = 0; j < c.Length; j++)
                {
                    if ((c[j] < 0) || (c[j] >= columns)) 
                    { 
                        throw new ArgumentException("Argument out of range."); 
                    } 

                    x[i - i0][j] = data[i][c[j]];
                }
            }

            return X;
        }

        /// <summary>Returns a sub matrix extracted from the current matrix.</summary>
        /// <param name="r">Array of row indices</param>
        /// <param name="j0">Start column index</param>
        /// <param name="j1">End column index</param>
        public Matrix Submatrix(int[] r, int j0, int j1)
        {
            if ((j0 > j1) || (j0 < 0) || (j0 >= columns) || (j1 < 0) || (j1 >= columns)) 
            { 
                throw new ArgumentException("Argument out of range."); 
            } 
            
            Matrix X = new Matrix(r.Length, j1-j0+1);
            double[][] x = X.Array;
            for (int i = 0; i < r.Length; i++)
            {
                for (int j = j0; j <= j1; j++) 
                {
                    if ((r[i] < 0) || (r[i] >= this.rows))
                    {
                        throw new ArgumentException("Argument out of range."); 
                    }

                    x[i][j - j0] = data[r[i]][j];
                }
            }

            return X;
        }

        /// <summary>Creates a copy of the matrix.</summary>
        public Matrix Clone()
        {
            Matrix X = new Matrix(rows, columns);
            double[][] x = X.Array;
            for (int i = 0; i < rows; i++)
            {
                for (int j = 0; j < columns; j++)
                {
                    x[i][j] = data[i][j];
                }
            }

            return X;
        }

        /// <summary>Returns the transposed matrix.</summary>
        public Matrix Transpose()
        {
            Matrix X = new Matrix(columns, rows);
            double[][] x = X.Array;
            for (int i = 0; i < rows; i++)
            {
                for (int j = 0; j < columns; j++)
                {
                    x[j][i] = data[i][j];
                }
            }

            return X;
        }

        /// <summary>Returns the One Norm for the matrix.</summary>
        /// <value>The maximum column sum.</value>
        public double Norm1
        {
            get
            {
                double f = 0;
                for (int j = 0; j < columns; j++)
                {
                    double s = 0;
                    for (int i = 0; i < rows; i++)
                    {
                        s += Math.Abs(data[i][j]);
                    }

                    f = Math.Max(f, s);
                }
                return f;
            }       
        }

        /// <summary>Returns the Infinity Norm for the matrix.</summary>
        /// <value>The maximum row sum.</value>
        public double InfinityNorm
        {
            get
            {
                double f = 0;
                for (int i = 0; i < rows; i++)
                {
                    double s = 0;
                    for (int j = 0; j < columns; j++)
                        s += Math.Abs(data[i][j]);
                    f = Math.Max(f, s);
                }
                return f;
            }
        }

        /// <summary>Returns the Frobenius Norm for the matrix.</summary>
        /// <value>The square root of sum of squares of all elements.</value>
        public double FrobeniusNorm
        {
            get
            {
                double f = 0;
                for (int i = 0; i < rows; i++)
                {
                    for (int j = 0; j < columns; j++)
                    {
                        f = Hypotenuse(f, data[i][j]);
                    }
                }

                return f;
            }                   
        }

        /// <summary>Unary minus.</summary>
        public static Matrix Negate(Matrix value)
        {
            if (value == null)
            {
                throw new ArgumentNullException("value");
            }

            int rows = value.Rows;
            int columns = value.Columns;
            double[][] data = value.Array;

            Matrix X = new Matrix(rows, columns);
            double[][] x = X.Array;
            for (int i = 0; i < rows; i++)
            {
                for (int j = 0; j < columns; j++)
                {
                    x[i][j] = -data[i][j];
                }
            }

            return X;
        }

        /// <summary>Unary minus.</summary>
        public static Matrix operator-(Matrix value)
        {
            if (value == null)
            {
                throw new ArgumentNullException("value");
            }

            return Negate(value);
        }

        /// <summary>Matrix equality.</summary>
        public static bool operator==(Matrix left, Matrix right)
        {
            return Equals(left, right);
        }

        /// <summary>Matrix inequality.</summary>
        public static bool operator!=(Matrix left, Matrix right)
        {
            return !Equals(left, right);
        }

        /// <summary>Matrix addition.</summary>
        public static Matrix Add(Matrix left, Matrix right)
        {
            if (left == null)
            {
                throw new ArgumentNullException("left");
            }

            if (right == null)
            {
                throw new ArgumentNullException("right");
            }

            int rows = left.Rows;
            int columns = left.Columns;
            double[][] data = left.Array;

            if ((rows != right.Rows) || (columns != right.Columns))
            {
                throw new ArgumentException("Matrix dimension do not match.");
            }

            Matrix X = new Matrix(rows, columns);
            double[][] x = X.Array;
            for (int i = 0; i < rows; i++)
            {
                for (int j = 0; j < columns; j++)
                {
                    x[i][j] = data[i][j] + right[i,j];
                }
            }
            return X;
        }

        /// <summary>Matrix addition.</summary>
        public static Matrix operator+(Matrix left, Matrix right)
        {
            if (left == null)
            {
                throw new ArgumentNullException("left");
            }

            if (right == null)
            {
                throw new ArgumentNullException("right");
            }

            return Add(left, right);
        }

        /// <summary>Matrix subtraction.</summary>
        public static Matrix Subtract(Matrix left, Matrix right)
        {
            if (left == null)
            {
                throw new ArgumentNullException("left");
            }

            if (right == null)
            {
                throw new ArgumentNullException("right");
            }

            int rows = left.Rows;
            int columns = left.Columns;
            double[][] data = left.Array;

            if ((rows != right.Rows) || (columns != right.Columns))
            {
                throw new ArgumentException("Matrix dimension do not match.");
            }

            Matrix X = new Matrix(rows, columns);
            double[][] x = X.Array;
            for (int i = 0; i < rows; i++)
            {
                for (int j = 0; j < columns; j++)
                {
                    x[i][j] = data[i][j] - right[i,j];
                }
            }
            return X;
        }

        /// <summary>Matrix subtraction.</summary>
        public static Matrix operator-(Matrix left, Matrix right)
        {
            if (left == null)
            {
                throw new ArgumentNullException("left");
            }

            if (right == null)
            {
                throw new ArgumentNullException("right");
            }

            return Subtract(left, right);
        }

        /// <summary>Matrix-scalar multiplication.</summary>
        public static Matrix Multiply(Matrix left, double right)
        {
            if (left == null)
            {
                throw new ArgumentNullException("left");
            }

            int rows = left.Rows;
            int columns = left.Columns;
            double[][] data = left.Array;

            Matrix X = new Matrix(rows, columns);

            double[][] x = X.Array;
            for (int i = 0; i < rows; i++)
            {
                for (int j = 0; j < columns; j++)
                {
                    x[i][j] = data[i][j] * right;
                }
            }

            return X;
        }

        /// <summary>Matrix-scalar multiplication.</summary>
        public static Matrix operator*(Matrix left, double right)
        {
            if (left == null)
            {
                throw new ArgumentNullException("left");
            }

            return Multiply(left, right);
        }

        /// <summary>Matrix-matrix multiplication.</summary>
        public static Matrix Multiply(Matrix left, Matrix right)
        {
            if (left == null)
            {
                throw new ArgumentNullException("left");
            }

            if (right == null)
            {
                throw new ArgumentNullException("right");
            }

            int rows = left.Rows;
            double[][] data = left.Array;

            if (right.Rows != left.columns)
            {
                throw new ArgumentException("Matrix dimensions are not valid.");
            }

            int columns = right.Columns;
            Matrix X = new Matrix(rows, columns);
            double[][] x = X.Array;

            int size = left.columns;
            double[] column = new double[size];
            for (int j = 0; j < columns; j++)
            {
                for (int k = 0; k < size; k++)
                {
                    column[k] = right[k,j];
                }
                for (int i = 0; i < rows; i++)
                {
                    double[] row = data[i];
                    double s = 0;
                    for (int k = 0; k < size; k++)
                    {
                        s += row[k] * column[k];
                    }
                    x[i][j] = s;
                } 
            }

            return X;
        }

        /// <summary>Matrix-matrix multiplication.</summary>
        public static Matrix operator*(Matrix left, Matrix right)
        {
            if (left == null)
            {
                throw new ArgumentNullException("left");
            }

            if (right == null)
            {
                throw new ArgumentNullException("right");
            }
            
            return Multiply(left, right);
        }

        /// <summary>Returns the LHS solution vetor if the matrix is square or the least squares solution otherwise.</summary>
        public Matrix Solve(Matrix rightHandSide)
        {
            return (rows == columns) ? new LuDecomposition(this).Solve(rightHandSide) : new QrDecomposition(this).Solve(rightHandSide);
        }

        /// <summary>Inverse of the matrix if matrix is square, pseudoinverse otherwise.</summary>
        public Matrix Inverse
        {
            get 
            { 
                return this.Solve(Diagonal(rows, rows, 1.0)); 
            }
        }

        /// <summary>Determinant if matrix is square.</summary>
        public double Determinant
        {
            get 
            { 
                return new LuDecomposition(this).Determinant; 
            }
        }

        /// <summary>Returns the trace of the matrix.</summary>
        /// <returns>Sum of the diagonal elements.</returns>
        public double Trace
        {
            get
            {
                double trace = 0;
                for (int i = 0; i < Math.Min(rows, columns); i++)
                {
                    trace += data[i][i];
                }
                return trace;
            }
        }

        /// <summary>Returns a matrix filled with random values.</summary>
        public static Matrix Random(int rows, int columns)
        {
            Matrix X = new Matrix(rows, columns);
            double[][] x = X.Array;
            for (int i = 0; i < rows; i++)
            {
                for (int j = 0; j < columns; j++)
                {
                    x[i][j] = random.NextDouble();
                }
            }
            return X;
        }

        /// <summary>Returns a diagonal matrix of the given size.</summary>
        public static Matrix Diagonal(int rows, int columns, double value)
        {
            Matrix X = new Matrix(rows, columns);
            double[][] x = X.Array;
            for (int i = 0; i < rows; i++)
            {
                for (int j = 0; j < columns; j++)
                {
                    x[i][j] = ((i == j) ? value : 0.0);
                }
            }
            return X;
        }

        /// <summary>Returns the matrix in a textual form.</summary>
        public override string ToString()
        {
            using (StringWriter writer = new StringWriter(CultureInfo.InvariantCulture))
            {
                for (int i = 0; i < rows; i++)
                {
                    for (int j = 0; j < columns; j++)
                    {
                        writer.Write(this.data[i][j] + " ");
                    }
    
                    writer.WriteLine();
                }

                return writer.ToString();
            }
        }

        private static double Hypotenuse(double a, double b) 
        {
            if (Math.Abs(a) > Math.Abs(b))
            {
                double r = b / a;
                return Math.Abs(a) * Math.Sqrt(1 + r * r);
            }

            if (b != 0)
            {
                double r = a / b;
                return Math.Abs(b) * Math.Sqrt(1 + r * r);
            }

            return 0.0;
        }
    }
}
