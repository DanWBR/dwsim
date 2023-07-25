namespace Mapack
{
    using System;

    /// <summary>LU decomposition of a rectangular matrix.</summary>
    /// <remarks>
    /// For an m-by-n matrix <c>A</c> with m >= n, the LU decomposition is an m-by-n
    /// unit lower triangular matrix <c>L</c>, an n-by-n upper triangular matrix <c>U</c>,
    /// and a permutation vector <c>piv</c> of length m so that <c>A(piv)=L*U</c>.
    /// If m &lt; n, then <c>L</c> is m-by-m and <c>U</c> is m-by-n.
    /// The LU decompostion with pivoting always exists, even if the matrix is
    /// singular, so the constructor will never fail.  The primary use of the
    /// LU decomposition is in the solution of square systems of simultaneous
    /// linear equations. This will fail if <see cref="NonSingular"/> returns <see langword="false"/>.
    /// </remarks>
    public class LuDecomposition
    {
        private Matrix LU;
        private int pivotSign;
        private int[] pivotVector;

        /// <summary>Construct a LU decomposition.</summary>    
        public LuDecomposition(Matrix value)
        {
            if (value == null)
            {
                throw new ArgumentNullException("value");   
            }
            
            this.LU = (Matrix) value.Clone();
            double[][] lu = LU.Array;
            int rows = value.Rows;
            int columns = value.Columns;
            pivotVector = new int[rows];
            for (int i = 0; i < rows; i++)
            {
                pivotVector[i] = i;
            }

            pivotSign = 1;
            double[] LUrowi;
            double[] LUcolj = new double[rows];
    
            // Outer loop.
            for (int j = 0; j < columns; j++)
            {
                // Make a copy of the j-th column to localize references.
                for (int i = 0; i < rows; i++)
                {
                    LUcolj[i] = lu[i][j];
                }
    
                // Apply previous transformations.
                for (int i = 0; i < rows; i++) 
                {
                    LUrowi = lu[i];
    
                    // Most of the time is spent in the following dot product.
                    int kmax = Math.Min(i,j);
                    double s = 0.0;
                    for (int k = 0; k < kmax; k++)
                    {
                        s += LUrowi[k]*LUcolj[k];
                    }
                    LUrowi[j] = LUcolj[i] -= s;
                }
         
                // Find pivot and exchange if necessary.
                int p = j;
                for (int i = j+1; i < rows; i++)
                {
                    if (Math.Abs(LUcolj[i]) > Math.Abs(LUcolj[p]))
                    {
                        p = i;
                    }
                }
    
                if (p != j)
                {
                    for (int k = 0; k < columns; k++) 
                    {
                        double t = lu[p][k]; 
                        lu[p][k] = lu[j][k]; 
                        lu[j][k] = t;
                    }
                        
                    int v = pivotVector[p]; 
                    pivotVector[p] = pivotVector[j]; 
                    pivotVector[j] = v;
                        
                    pivotSign = - pivotSign;
                }
    
                // Compute multipliers.
                     
                if (j < rows & lu[j][j] != 0.0) 
                {
                    for (int i = j+1; i < rows; i++) 
                    {
                        lu[i][j] /= lu[j][j];
                    }
                }
            }
        }

        /// <summary>Returns if the matrix is non-singular.</summary>
        public bool NonSingular
        {
            get 
            {
                for (int j = 0; j < LU.Columns; j++)
                    if (LU[j, j] == 0)
                        return false;
                return true;
            }
        }
    
        /// <summary>Returns the determinant of the matrix.</summary>
        public double Determinant
        {
            get
            {
                if (LU.Rows != LU.Columns) throw new ArgumentException("Matrix must be square.");
                double determinant = (double) pivotSign;
                for (int j = 0; j < LU.Columns; j++) 
                    determinant *= LU[j, j];
                return determinant; 
            }
        }

        /// <summary>Returns the lower triangular factor <c>L</c> with <c>A=LU</c>.</summary>
        public Matrix LowerTriangularFactor
        {
            get
            {
                int rows = LU.Rows;
                int columns = LU.Columns;
                Matrix X = new Matrix(rows, columns);
                for (int i = 0; i < rows; i++) 
                    for (int j = 0; j < columns; j++)
                        if (i > j)
                            X[i,j] = LU[i,j];
                        else if (i == j)
                            X[i,j] = 1.0;
                        else
                            X[i,j] = 0.0;
                return X;
            }
        }

        /// <summary>Returns the lower triangular factor <c>L</c> with <c>A=LU</c>.</summary>
        public Matrix UpperTriangularFactor
        {
            get
            {
                int rows = LU.Rows;
                int columns = LU.Columns;
                Matrix X = new Matrix(rows, columns);
                for (int i = 0; i < rows; i++)
                    for (int j = 0; j < columns; j++) 
                        if (i <= j)
                            X[i,j] = LU[i,j];
                        else
                            X[i,j] = 0.0;
                return X;
            }
        }       

        /// <summary>Returns the pivot permuation vector.</summary>
        public double[] PivotPermutationVector
        {
            get
            {
                int rows = LU.Rows;

                double[] p = new double[rows];
                for (int i = 0; i < rows; i++)
                {
                    p[i] = (double) this.pivotVector[i];
                }

                return p;
            }
        }   

        /// <summary>Solves a set of equation systems of type <c>A * X = B</c>.</summary>
        /// <param name="value">Right hand side matrix with as many rows as <c>A</c> and any number of columns.</param>
        /// <returns>Matrix <c>X</c> so that <c>L * U * X = B</c>.</returns>
        public Matrix Solve(Matrix value)
        {
            if (value == null)
            {
                throw new ArgumentNullException("value");   
            }

            if (value.Rows != this.LU.Rows)
            {
                throw new ArgumentException("Invalid matrix dimensions.", "value");
            }
            
            if (!this.NonSingular) 
            {
                throw new InvalidOperationException("Matrix is singular");
            }
    
            // Copy right hand side with pivoting
            int count = value.Columns;
            Matrix X = value.Submatrix(pivotVector, 0, count-1);
    
            int rows = LU.Rows;
            int columns = LU.Columns;
            double[][] lu = LU.Array;
    
            // Solve L*Y = B(piv,:)
            for (int k = 0; k < columns; k++)
            {
                for (int i = k + 1; i < columns; i++)
                {
                    for (int j = 0; j < count; j++)
                    {
                        X[i,j] -= X[k,j] * lu[i][k];
                    }
                }
            }
    
            // Solve U*X = Y;
            for (int k = columns - 1; k >= 0; k--)
            {
                for (int j = 0; j < count; j++)
                {
                    X[k,j] /= lu[k][k];
                }
    
                for (int i = 0; i < k; i++)
                {
                    for (int j = 0; j < count; j++)
                    {
                        X[i,j] -= X[k,j] * lu[i][k];
                    }
                }
            }
                
            return X;
        }
    }
}
