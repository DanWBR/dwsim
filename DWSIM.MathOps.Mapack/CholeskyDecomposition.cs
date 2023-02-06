namespace Mapack
{
    using System;

    /// <summary>Cholesky Decomposition of a symmetric, positive definite matrix.</summary>
    /// <remarks>
    /// For a symmetric, positive definite matrix <c>A</c>, the Cholesky decomposition is a
    /// lower triangular matrix <c>L</c> so that <c>A = L * L'</c>.
    /// If the matrix is not symmetric or positive definite, the constructor returns a partial 
    /// decomposition and sets two internal variables that can be queried using the
    /// <see cref="Symmetric"/> and <see cref="PositiveDefinite"/> properties.
    /// </remarks>
    public class CholeskyDecomposition
    {
        private Matrix L;
        private bool symmetric;
        private bool positiveDefinite;

        /// <summary>Construct a Cholesky Decomposition.</summary>
        public CholeskyDecomposition(Matrix value)
        {
            if (value == null)
            {
                throw new ArgumentNullException("value");
            }
            
            if (!value.Square)
            {
                throw new ArgumentException("Matrix is not square.", "value");
            }

            int dimension = value.Rows;
            L = new Matrix(dimension, dimension);
                
            double[][] a = value.Array;
            double[][] l = L.Array;

            this.positiveDefinite = true;
            this.symmetric = true;

            for (int j = 0; j < dimension; j++) 
            {
                double[] Lrowj = l[j];
                double d = 0.0;
                for (int k = 0; k < j; k++)
                {
                    double[] Lrowk = l[k];
                    double s = 0.0;
                    for (int i = 0; i < k; i++)
                    {
                        s += Lrowk[i] * Lrowj[i];
                    }
                    Lrowj[k] = s = (a[j][k] - s) / l[k][k];
                    d = d + s*s;
                    
                    this.symmetric = this.symmetric & (a[k][j] == a[j][k]); 
                }

                d = a[j][j] - d;
                
                this.positiveDefinite = this.positiveDefinite & (d > 0.0);
                l[j][j] = Math.Sqrt(Math.Max(d,0.0));
                for (int k = j + 1; k < dimension; k++)
                {
                    l[j][k] = 0.0;
                }
            }
        }

        /// <summary>Returns <see langword="true"/> if the matrix is symmetric.</summary>
        public bool Symmetric
        {
            get 
            { 
                return this.symmetric; 
            }
        }

        /// <summary>Returns <see langword="true"/> if the matrix is positive definite.</summary>
        public bool PositiveDefinite
        {
            get 
            { 
                return this.positiveDefinite; 
            }
        }

        /// <summary>Returns the left triangular factor <c>L</c> so that <c>A = L * L'</c>.</summary>
        public Matrix LeftTriangularFactor
        {
            get 
            { 
                return this.L; 
            }
        }

        /// <summary>Solves a set of equation systems of type <c>A * X = B</c>.</summary>
        /// <param name="value">Right hand side matrix with as many rows as <c>A</c> and any number of columns.</param>
        /// <returns>Matrix <c>X</c> so that <c>L * L' * X = B</c>.</returns>
        /// <exception cref="T:System.ArgumentException">Matrix dimensions do not match.</exception>
        /// <exception cref="T:System.InvalidOperationException">Matrix is not symmetrix and positive definite.</exception>
        public Matrix Solve(Matrix value)
        {
            if (value == null)
            {
                throw new ArgumentNullException("value");               
            }

            if (value.Rows != L.Rows)
            {
                throw new ArgumentException("Matrix dimensions do not match.");
            }

            if (!this.symmetric)
            {
                throw new InvalidOperationException("Matrix is not symmetric.");
            }

            if (!this.positiveDefinite)
            {
                throw new InvalidOperationException("Matrix is not positive definite.");
            }

            // Solve L*Y = B;
            int dimension = L.Rows;
            int count = value.Columns;

            Matrix B = (Matrix)value.Clone();
            double[][] l = L.Array;

            // Solve L*Y = B;
            for (int k = 0; k < dimension; k++)
            {
                for (int j = 0; j < count; j++)
                {
                    for (int i = 0; i < k; i++)
                    {
                        B[k, j] -= B[i, j] * l[k][i];
                    }
                    B[k, j] /= l[k][k];
                }
            }

            // Solve L'*X = Y;
            for (int k = dimension - 1; k >= 0; k--)
            {
                for (int j = 0; j < count; j++)
                {
                    for (int i = k + 1; i < dimension; i++)
                    {
                        B[k, j] -= B[i, j] * L[i, k];
                    }
                    B[k, j] /= l[k][k];
                }
            }

            return B;
        }
    }
}
