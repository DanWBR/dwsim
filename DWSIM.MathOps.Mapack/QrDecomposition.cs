namespace Mapack
{
    using System;

    /// <summary>QR decomposition for a rectangular matrix.</summary>
    /// <remarks>
    /// For an m-by-n matrix <c>A</c> with <c>m &gt;= n</c>, the QR decomposition is an m-by-n
    /// orthogonal matrix <c>Q</c> and an n-by-n upper triangular 
    /// matrix <c>R</c> so that <c>A = Q * R</c>.
    /// The QR decompostion always exists, even if the matrix does not have
    /// full rank, so the constructor will never fail.  The primary use of the
    /// QR decomposition is in the least squares solution of nonsquare systems
    /// of simultaneous linear equations.
    /// This will fail if <see cref="FullRank"/> returns <see langword="false"/>.
    /// </remarks>
    public class QrDecomposition
    {
        private Matrix QR;
        private double[] Rdiag;

        /// <summary>Construct a QR decomposition.</summary>
        public QrDecomposition(Matrix value)
        {
            if (value == null)
            {
                throw new ArgumentNullException("value");
            }

            this.QR = (Matrix) value.Clone();
            double[][] qr = this.QR.Array;
            int m = value.Rows;
            int n = value.Columns;
            this.Rdiag = new double[n];
    
            for (int k = 0; k < n; k++) 
            {
                // Compute 2-norm of k-th column without under/overflow.
                double nrm = 0;
                for (int i = k; i < m; i++)
                {
                    nrm = Hypotenuse(nrm,qr[i][k]);
                }
                 
                if (nrm != 0.0) 
                {
                    // Form k-th Householder vector.
                    if (qr[k][k] < 0)
                    {
                        nrm = -nrm;
                    }
                    
                    for (int i = k; i < m; i++)
                    {
                        qr[i][k] /= nrm;
                    }

                    qr[k][k] += 1.0;
    
                    // Apply transformation to remaining columns.
                    for (int j = k+1; j < n; j++) 
                    {
                        double s = 0.0;

                        for (int i = k; i < m; i++)
                        {
                            s += qr[i][k]*qr[i][j];
                        }

                        s = -s/qr[k][k];

                        for (int i = k; i < m; i++)
                        {
                            qr[i][j] += s*qr[i][k];
                        }
                    }
                }

                this.Rdiag[k] = -nrm;
            }
        }

        /// <summary>Least squares solution of <c>A * X = B</c></summary>
        /// <param name="value">Right-hand-side matrix with as many rows as <c>A</c> and any number of columns.</param>
        /// <returns>A matrix that minimized the two norm of <c>Q * R * X - B</c>.</returns>
        /// <exception cref="T:System.ArgumentException">Matrix row dimensions must be the same.</exception>
        /// <exception cref="T:System.InvalidOperationException">Matrix is rank deficient.</exception>
        public Matrix Solve(Matrix value)
        {
            if (value == null)
            {
                throw new ArgumentNullException("value");
            }

            if (value.Rows != QR.Rows)
            {
                throw new ArgumentException("Matrix row dimensions must agree.");
            }
            
            if (!this.FullRank) 
            {
                throw new InvalidOperationException("Matrix is rank deficient.");
            }
                
            // Copy right hand side
            int count = value.Columns;
            Matrix X = value.Clone();
            int m = QR.Rows;
            int n = QR.Columns;
            double[][] qr = QR.Array;
            
            // Compute Y = transpose(Q)*B
            for (int k = 0; k < n; k++) 
            {
                for (int j = 0; j < count; j++) 
                {
                    double s = 0.0; 
                    
                    for (int i = k; i < m; i++)
                    {
                        s += qr[i][k] * X[i,j];
                    }

                    s = -s / qr[k][k];
                    
                    for (int i = k; i < m; i++)
                    {
                        X[i,j] += s * qr[i][k];
                    }
                }
            }
                
            // Solve R*X = Y;
            for (int k = n-1; k >= 0; k--) 
            {
                for (int j = 0; j < count; j++) 
                {
                    X[k,j] /= Rdiag[k];
                }
    
                for (int i = 0; i < k; i++) 
                {
                    for (int j = 0; j < count; j++) 
                    {
                        X[i,j] -= X[k,j] * qr[i][k];
                    }
                }
            }
    
            return X.Submatrix(0, n-1, 0, count-1);
        }

        /// <summary>Shows if the matrix <c>A</c> is of full rank.</summary>
        /// <value>The value is <see langword="true"/> if <c>R</c>, and hence <c>A</c>, has full rank.</value>
        public bool FullRank
        {
            get
            {
                int columns = this.QR.Columns;
                for (int i = 0; i < columns; i++)
                {
                    if (this.Rdiag[i] == 0)
                    {
                        return false;
                    }
                }

                return true;
            }           
        }
    
        /// <summary>Returns the upper triangular factor <c>R</c>.</summary>
        public Matrix UpperTriangularFactor
        {
            get
            {
                int n = this.QR.Columns;
                Matrix X = new Matrix(n, n);
                double[][] x = X.Array;
                double[][] qr = QR.Array;
                for (int i = 0; i < n; i++) 
                {
                    for (int j = 0; j < n; j++) 
                    {
                        if (i < j)
                        {
                            x[i][j] = qr[i][j];
                        }
                        else if (i == j) 
                        {
                            x[i][j] = Rdiag[i];
                        }
                        else
                        {
                            x[i][j] = 0.0;
                        }
                    }
                }
    
                return X;
            }
        }

        /// <summary>Returns the orthogonal factor <c>Q</c>.</summary>
        public Matrix OrthogonalFactor
        {
            get
            {
                Matrix X = new Matrix(QR.Rows, QR.Columns);
                double[][] x = X.Array;
                double[][] qr = QR.Array;
                for (int k = QR.Columns - 1; k >= 0; k--) 
                {
                    for (int i = 0; i < QR.Rows; i++)
                    {
                        x[i][k] = 0.0;
                    }

                    x[k][k] = 1.0;
                    for (int j = k; j < QR.Columns; j++) 
                    {
                        if (qr[k][k] != 0) 
                        {
                            double s = 0.0;
                
                            for (int i = k; i < QR.Rows; i++)
                            {
                                s += qr[i][k] * x[i][j];
                            }

                            s = -s / qr[k][k];
                
                            for (int i = k; i < QR.Rows; i++)
                            {
                                x[i][j] += s * qr[i][k];
                            }
                        }
                    }
                }

                return X;
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
