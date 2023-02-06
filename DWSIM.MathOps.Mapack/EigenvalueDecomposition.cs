namespace Mapack
{
    using System;

    /// <summary>Determines the eigenvalues and eigenvectors of a real square matrix.</summary>
    /// <remarks>
    /// If <c>A</c> is symmetric, then <c>A = V * D * V'</c> and <c>A = V * V'</c>
    /// where the eigenvalue matrix <c>D</c> is diagonal and the eigenvector matrix <c>V</c> is orthogonal.
    /// If <c>A</c> is not symmetric, the eigenvalue matrix <c>D</c> is block diagonal
    /// with the real eigenvalues in 1-by-1 blocks and any complex eigenvalues,
    /// <c>lambda+i*mu</c>, in 2-by-2 blocks, <c>[lambda, mu; -mu, lambda]</c>.
    /// The columns of <c>V</c> represent the eigenvectors in the sense that <c>A * V = V * D</c>.
    /// The matrix V may be badly conditioned, or even singular, so the validity of the equation
    /// <c>A=V*D*inverse(V)</c> depends upon the condition of <c>V</c>.
    /// </remarks>
    public class EigenvalueDecomposition
    {
        private int n;              // matrix dimension
        private double[] d, e;      // storage of eigenvalues.
        private Matrix V;           // storage of eigenvectors.
        private Matrix H;           // storage of non-symmetric Hessenberg form.
        private double[] ort;       // storage for non-symmetric algorithm.
        private double cdivr, cdivi;
        private bool symmetric;

        /// <summary>Construct an eigenvalue decomposition.</summary>
        public EigenvalueDecomposition(Matrix value)
        {
            if (value == null)
            {
                throw new ArgumentNullException("value");
            }

            if (value.Rows != value.Columns) 
            {
                throw new ArgumentException("Matrix is not a square matrix.", "value");
            }
            
            n = value.Columns;
            V = new Matrix(n,n);
            d = new double[n];
            e = new double[n];
    
            // Check for symmetry.
            this.symmetric = value.Symmetric;
    
            if (this.symmetric)
            {
                for (int i = 0; i < n; i++)
                {
                    for (int j = 0; j < n; j++)
                    {
                        V[i,j] = value[i,j];
                    }
                }
         
                // Tridiagonalize.
                this.tred2();

                // Diagonalize.
                this.tql2();
            } 
            else 
            {
                H = new Matrix(n,n);
                ort = new double[n];
                     
                for (int j = 0; j < n; j++)
                {
                    for (int i = 0; i < n; i++)
                    {
                        H[i,j] = value[i,j];
                    }
                }
         
                // Reduce to Hessenberg form.
                this.orthes();
         
                // Reduce Hessenberg to real Schur form.
                this.hqr2();
            }
        }
        
        private void tred2() 
        {
            // Symmetric Householder reduction to tridiagonal form.
            // This is derived from the Algol procedures tred2 by Bowdler, Martin, Reinsch, and Wilkinson, 
            // Handbook for Auto. Comp., Vol.ii-Linear Algebra, and the corresponding Fortran subroutine in EISPACK.
            for (int j = 0; j < n; j++)
                d[j] = V[n-1,j];
    
            // Householder reduction to tridiagonal form.
            for (int i = n-1; i > 0; i--) 
            {
                // Scale to avoid under/overflow.
                double scale = 0.0;
                double h = 0.0;
                for (int k = 0; k < i; k++)
                    scale = scale + Math.Abs(d[k]);
                
                if (scale == 0.0) 
                {
                    e[i] = d[i-1];
                    for (int j = 0; j < i; j++) 
                    {
                        d[j] = V[i-1,j];
                        V[i,j] = 0.0;
                        V[j,i] = 0.0;
                    }
                }
                else
                {
                    // Generate Householder vector.
                    for (int k = 0; k < i; k++) 
                    {
                        d[k] /= scale;
                        h += d[k] * d[k];
                    }
    
                    double f = d[i-1];
                    double g = Math.Sqrt(h);
                    if (f > 0) g = -g;
    
                    e[i] = scale * g;
                    h = h - f * g;
                    d[i-1] = f - g;
                    for (int j = 0; j < i; j++)
                        e[j] = 0.0;
         
                    // Apply similarity transformation to remaining columns.
                    for (int j = 0; j < i; j++) 
                    {
                        f = d[j];
                        V[j,i] = f;
                        g = e[j] + V[j,j] * f;
                        for (int k = j+1; k <= i-1; k++) 
                        {
                            g += V[k,j] * d[k];
                            e[k] += V[k,j] * f;
                        }
                        e[j] = g;
                    }
                            
                    f = 0.0;
                    for (int j = 0; j < i; j++) 
                    {
                        e[j] /= h;
                        f += e[j] * d[j];
                    }
                    
                    double hh = f / (h + h);
                    for (int j = 0; j < i; j++)
                        e[j] -= hh * d[j];
    
                    for (int j = 0; j < i; j++) 
                    {
                        f = d[j];
                        g = e[j];
                        for (int k = j; k <= i-1; k++)
                            V[k,j] -= (f * e[k] + g * d[k]);
    
                        d[j] = V[i-1,j];
                        V[i,j] = 0.0;
                    }
                }
                d[i] = h;
            }
         
            // Accumulate transformations.
            for (int i = 0; i < n-1; i++) 
            {
                V[n-1,i] = V[i,i];
                V[i,i] = 1.0;
                double h = d[i+1];
                if (h != 0.0) 
                {
                    for (int k = 0; k <= i; k++)
                        d[k] = V[k,i+1] / h;
    
                    for (int j = 0; j <= i; j++) 
                    {
                        double g = 0.0;
                        for (int k = 0; k <= i; k++)
                            g += V[k,i+1] * V[k,j];
                        for (int k = 0; k <= i; k++)
                            V[k,j] -= g * d[k];
                    }
                }
        
                for (int k = 0; k <= i; k++)
                    V[k,i+1] = 0.0;
            }
        
            for (int j = 0; j < n; j++) 
            {
                d[j] = V[n-1,j];
                V[n-1,j] = 0.0;
            }
                
            V[n-1,n-1] = 1.0;
            e[0] = 0.0;
        } 
         
        private void tql2() 
        {
            // Symmetric tridiagonal QL algorithm.
            // This is derived from the Algol procedures tql2, by Bowdler, Martin, Reinsch, and Wilkinson, 
            // Handbook for Auto. Comp., Vol.ii-Linear Algebra, and the corresponding Fortran subroutine in EISPACK.
            for (int i = 1; i < n; i++)
                e[i-1] = e[i];
    
            e[n-1] = 0.0;
         
            double f = 0.0;
            double tst1 = 0.0;
            double eps = Math.Pow(2.0,-52.0);
    
            for (int l = 0; l < n; l++) 
            {
                // Find small subdiagonal element.
                tst1 = Math.Max(tst1,Math.Abs(d[l]) + Math.Abs(e[l]));
                int m = l;
                while (m < n) 
                {
                    if (Math.Abs(e[m]) <= eps*tst1)
                        break;
                    m++;
                }
         
                // If m == l, d[l] is an eigenvalue, otherwise, iterate.
                if (m > l) 
                {
                    int iter = 0;
                    do 
                    {
                        iter = iter + 1;  // (Could check iteration count here.)
         
                        // Compute implicit shift
                        double g = d[l];
                        double p = (d[l+1] - g) / (2.0 * e[l]);
                        double r = Hypotenuse(p,1.0);
                        if (p < 0) 
                        {
                            r = -r;
                        }
    
                        d[l] = e[l] / (p + r);
                        d[l+1] = e[l] * (p + r);
                        double dl1 = d[l+1];
                        double h = g - d[l];
                        for (int i = l+2; i < n; i++) 
                        {
                            d[i] -= h;
                        }

                        f = f + h;
         
                        // Implicit QL transformation.
                        p = d[m];
                        double c = 1.0;
                        double c2 = c;
                        double c3 = c;
                        double el1 = e[l+1];
                        double s = 0.0;
                        double s2 = 0.0;
                        for (int i = m-1; i >= l; i--) 
                        {
                            c3 = c2;
                            c2 = c;
                            s2 = s;
                            g = c * e[i];
                            h = c * p;
                            r = Hypotenuse(p,e[i]);
                            e[i+1] = s * r;
                            s = e[i] / r;
                            c = p / r;
                            p = c * d[i] - s * g;
                            d[i+1] = h + s * (c * g + s * d[i]);
         
                            // Accumulate transformation.
                            for (int k = 0; k < n; k++) 
                            {
                                h = V[k,i+1];
                                V[k,i+1] = s * V[k,i] + c * h;
                                V[k,i] = c * V[k,i] - s * h;
                            }
                        }
                            
                        p = -s * s2 * c3 * el1 * e[l] / dl1;
                        e[l] = s * p;
                        d[l] = c * p;
         
                        // Check for convergence.
                    } 
                    while (Math.Abs(e[l]) > eps*tst1);
                }
                d[l] = d[l] + f;
                e[l] = 0.0;
            }
             
            // Sort eigenvalues and corresponding vectors.
            for (int i = 0; i < n-1; i++) 
            {
                int k = i;
                double p = d[i];
                for (int j = i+1; j < n; j++) 
                {
                    if (d[j] < p) 
                    {
                        k = j;
                        p = d[j];
                    }
                }
                     
                if (k != i) 
                {
                    d[k] = d[i];
                    d[i] = p;
                    for (int j = 0; j < n; j++) 
                    {
                        p = V[j,i];
                        V[j,i] = V[j,k];
                        V[j,k] = p;
                    }
                }
            }
        }
         
        private void orthes() 
        {
            // Nonsymmetric reduction to Hessenberg form.
            // This is derived from the Algol procedures orthes and ortran, by Martin and Wilkinson, 
            // Handbook for Auto. Comp., Vol.ii-Linear Algebra, and the corresponding Fortran subroutines in EISPACK.
            int low = 0;
            int high = n-1;
         
            for (int m = low+1; m <= high-1; m++) 
            {
                // Scale column.
         
                double scale = 0.0;
                for (int i = m; i <= high; i++)
                    scale = scale + Math.Abs(H[i,m-1]);
    
                if (scale != 0.0) 
                {
                    // Compute Householder transformation.
                    double h = 0.0;
                    for (int i = high; i >= m; i--) 
                    {
                        ort[i] = H[i,m-1]/scale;
                        h += ort[i] * ort[i];
                    }
                        
                    double g = Math.Sqrt(h);
                    if (ort[m] > 0) g = -g;
    
                    h = h - ort[m] * g;
                    ort[m] = ort[m] - g;
         
                    // Apply Householder similarity transformation
                    // H = (I - u * u' / h) * H * (I - u * u') / h)
                    for (int j = m; j < n; j++) 
                    {
                        double f = 0.0;
                        for (int i = high; i >= m; i--) 
                            f += ort[i]*H[i,j];
    
                        f = f/h;
                        for (int i = m; i <= high; i++)
                            H[i,j] -= f*ort[i];
                    }
         
                    for (int i = 0; i <= high; i++) 
                    {
                        double f = 0.0;
                        for (int j = high; j >= m; j--)
                            f += ort[j]*H[i,j];
    
                        f = f/h;
                        for (int j = m; j <= high; j++)
                            H[i,j] -= f*ort[j];
                    }
    
                    ort[m] = scale*ort[m];
                    H[m,m-1] = scale*g;
                }
            }
         
            // Accumulate transformations (Algol's ortran).
            for (int i = 0; i < n; i++)
                for (int j = 0; j < n; j++)
                    V[i,j] = (i == j ? 1.0 : 0.0);
    
            for (int m = high-1; m >= low+1; m--) 
            {
                if (H[m,m-1] != 0.0) 
                {
                    for (int i = m+1; i <= high; i++)
                        ort[i] = H[i,m-1];
    
                    for (int j = m; j <= high; j++) 
                    {
                        double g = 0.0;
                        for (int i = m; i <= high; i++)
                            g += ort[i] * V[i,j];
    
                        // Double division avoids possible underflow.
                        g = (g / ort[m]) / H[m,m-1];
                        for (int i = m; i <= high; i++)
                            V[i,j] += g * ort[i];
                    }
                }
            }
        }
         
        private void cdiv(double xr, double xi, double yr, double yi)
        {
            // Complex scalar division.
            double r;
            double d;
            if (Math.Abs(yr) > Math.Abs(yi)) 
            {
                r = yi/yr;
                d = yr + r*yi;
                cdivr = (xr + r*xi)/d;
                cdivi = (xi - r*xr)/d;
            } 
            else 
            {
                r = yr/yi;
                d = yi + r*yr;
                cdivr = (r*xr + xi)/d;
                cdivi = (r*xi - xr)/d;
            }
        }

        private void hqr2() 
        {
            // Nonsymmetric reduction from Hessenberg to real Schur form.   
            // This is derived from the Algol procedure hqr2, by Martin and Wilkinson, Handbook for Auto. Comp.,
            // Vol.ii-Linear Algebra, and the corresponding  Fortran subroutine in EISPACK.
            int nn = this.n;
            int n = nn-1;
            int low = 0;
            int high = nn-1;
            double eps = Math.Pow(2.0,-52.0);
            double exshift = 0.0;
            double p = 0;
            double q = 0;
            double r = 0;
            double s = 0;
            double z = 0;
            double t;
            double w;
            double x;
            double y;
         
            // Store roots isolated by balanc and compute matrix norm
            double norm = 0.0;
            for (int i = 0; i < nn; i++) 
            {
                if (i < low | i > high) 
                {
                    d[i] = H[i,i];
                    e[i] = 0.0;
                }
                    
                for (int j = Math.Max(i-1,0); j < nn; j++)
                    norm = norm + Math.Abs(H[i,j]);
            }
         
            // Outer loop over eigenvalue index
            int iter = 0;
            while (n >= low) 
            {
                // Look for single small sub-diagonal element
                int l = n;
                while (l > low) 
                {
                    s = Math.Abs(H[l-1,l-1]) + Math.Abs(H[l,l]);
                    if (s == 0.0) s = norm;
                    if (Math.Abs(H[l,l-1]) < eps * s)
                        break;
    
                    l--;
                }
                 
                // Check for convergence
                if (l == n) 
                {
                    // One root found
                    H[n,n] = H[n,n] + exshift;
                    d[n] = H[n,n];
                    e[n] = 0.0;
                    n--;
                    iter = 0;
                } 
                else if (l == n-1) 
                {
                    // Two roots found
                    w = H[n,n-1] * H[n-1,n];
                    p = (H[n-1,n-1] - H[n,n]) / 2.0;
                    q = p * p + w;
                    z = Math.Sqrt(Math.Abs(q));
                    H[n,n] = H[n,n] + exshift;
                    H[n-1,n-1] = H[n-1,n-1] + exshift;
                    x = H[n,n];
         
                    if (q >= 0) 
                    {
                        // Real pair
                        z = (p >= 0) ? (p + z) : (p - z);
                        d[n-1] = x + z;
                        d[n] = d[n-1];
                        if (z != 0.0) 
                            d[n] = x - w / z;
                        e[n-1] = 0.0;
                        e[n] = 0.0;
                        x = H[n,n-1];
                        s = Math.Abs(x) + Math.Abs(z);
                        p = x / s;
                        q = z / s;
                        r = Math.Sqrt(p * p+q * q);
                        p = p / r;
                        q = q / r;
         
                        // Row modification
                        for (int j = n-1; j < nn; j++) 
                        {
                            z = H[n-1,j];
                            H[n-1,j] = q * z + p * H[n,j];
                            H[n,j] = q * H[n,j] - p * z;
                        }
             
                        // Column modification
                        for (int i = 0; i <= n; i++) 
                        {
                            z = H[i,n-1];
                            H[i,n-1] = q * z + p * H[i,n];
                            H[i,n] = q * H[i,n] - p * z;
                        }
             
                        // Accumulate transformations
                        for (int i = low; i <= high; i++) 
                        {
                            z = V[i,n-1];
                            V[i,n-1] = q * z + p * V[i,n];
                            V[i,n] = q * V[i,n] - p * z;
                        }
                    }
                    else 
                    {
                        // Complex pair
                        d[n-1] = x + p;
                        d[n] = x + p;
                        e[n-1] = z;
                        e[n] = -z;
                    }
                        
                    n = n - 2;
                    iter = 0;
                }
                else 
                {
                    // No convergence yet    
                    
                    // Form shift
                    x = H[n,n];
                    y = 0.0;
                    w = 0.0;
                    if (l < n) 
                    {
                        y = H[n-1,n-1];
                        w = H[n,n-1] * H[n-1,n];
                    }
         
                    // Wilkinson's original ad hoc shift
                    if (iter == 10) 
                    {
                        exshift += x;
                        for (int i = low; i <= n; i++)
                            H[i,i] -= x;
    
                        s = Math.Abs(H[n,n-1]) + Math.Abs(H[n-1,n-2]);
                        x = y = 0.75 * s;
                        w = -0.4375 * s * s;
                    }
    
                    // MATLAB's new ad hoc shift
                    if (iter == 30) 
                    {
                        s = (y - x) / 2.0;
                        s = s * s + w;
                        if (s > 0) 
                        {
                            s = Math.Sqrt(s);
                            if (y < x) s = -s;
                            s = x - w / ((y - x) / 2.0 + s);
                            for (int i = low; i <= n; i++)
                                H[i,i] -= s;
                            exshift += s;
                            x = y = w = 0.964;
                        }
                    }
         
                    iter = iter + 1;
         
                    // Look for two consecutive small sub-diagonal elements
                    int m = n-2;
                    while (m >= l) 
                    {
                        z = H[m,m];
                        r = x - z;
                        s = y - z;
                        p = (r * s - w) / H[m+1,m] + H[m,m+1];
                        q = H[m+1,m+1] - z - r - s;
                        r = H[m+2,m+1];
                        s = Math.Abs(p) + Math.Abs(q) + Math.Abs(r);
                        p = p / s;
                        q = q / s;
                        r = r / s;
                        if (m == l) 
                            break;
                        if (Math.Abs(H[m,m-1]) * (Math.Abs(q) + Math.Abs(r)) < eps * (Math.Abs(p) * (Math.Abs(H[m-1,m-1]) + Math.Abs(z) +   Math.Abs(H[m+1,m+1])))) 
                            break;
                        m--;
                    }
         
                    for (int i = m+2; i <= n; i++) 
                    {
                        H[i,i-2] = 0.0;
                        if (i > m+2)
                            H[i,i-3] = 0.0;
                    }
         
                    // Double QR step involving rows l:n and columns m:n
                    for (int k = m; k <= n-1; k++) 
                    {
                        bool notlast = (k != n-1);
                        if (k != m) 
                        {
                            p = H[k,k-1];
                            q = H[k+1,k-1];
                            r = (notlast ? H[k+2,k-1] : 0.0);
                            x = Math.Abs(p) + Math.Abs(q) + Math.Abs(r);
                            if (x != 0.0) 
                            {
                                p = p / x;
                                q = q / x;
                                r = r / x;
                            }
                        }
                            
                        if (x == 0.0)   break;
    
                        s = Math.Sqrt(p * p + q * q + r * r);
                        if (p < 0) s = -s;
                                 
                        if (s != 0) 
                        {
                            if (k != m)
                                H[k,k-1] = -s * x;
                            else 
                                if (l != m)
                                H[k,k-1] = -H[k,k-1];
    
                            p = p + s;
                            x = p / s;
                            y = q / s;
                            z = r / s;
                            q = q / p;
                            r = r / p;
         
                            // Row modification
                            for (int j = k; j < nn; j++) 
                            {
                                p = H[k,j] + q * H[k+1,j];
                                if (notlast) 
                                {
                                    p = p + r * H[k+2,j];
                                    H[k+2,j] = H[k+2,j] - p * z;
                                }
                                
                                H[k,j] = H[k,j] - p * x;
                                H[k+1,j] = H[k+1,j] - p * y;
                            }
         
                            // Column modification
                            for (int i = 0; i <= Math.Min(n,k+3); i++) 
                            {
                                p = x * H[i,k] + y * H[i,k+1];
                                if (notlast) 
                                {
                                    p = p + z * H[i,k+2];
                                    H[i,k+2] = H[i,k+2] - p * r;
                                }
                                
                                H[i,k] = H[i,k] - p;
                                H[i,k+1] = H[i,k+1] - p * q;
                            }
         
                            // Accumulate transformations
                            for (int i = low; i <= high; i++) 
                            {
                                p = x * V[i,k] + y * V[i,k+1];
                                if (notlast) 
                                {
                                    p = p + z * V[i,k+2];
                                    V[i,k+2] = V[i,k+2] - p * r;
                                }
                                
                                V[i,k] = V[i,k] - p;
                                V[i,k+1] = V[i,k+1] - p * q;
                            }
                        }
                    }
                }
            }
                
            // Backsubstitute to find vectors of upper triangular form
            if (norm == 0.0) 
            {
                return;
            }
         
            for (n = nn-1; n >= 0; n--) 
            {
                p = d[n];
                q = e[n];
         
                // Real vector
                if (q == 0) 
                {
                    int l = n;
                    H[n,n] = 1.0;
                    for (int i = n-1; i >= 0; i--) 
                    {
                        w = H[i,i] - p;
                        r = 0.0;
                        for (int j = l; j <= n; j++) 
                            r = r + H[i,j] * H[j,n];
                        
                        if (e[i] < 0.0) 
                        {
                            z = w;
                            s = r;
                        }
                        else 
                        {
                            l = i;
                            if (e[i] == 0.0) 
                            {
                                H[i,n] = (w != 0.0) ? (-r / w) : (-r / (eps * norm));
                            }
                            else
                            {
                                // Solve real equations
                                x = H[i,i+1];
                                y = H[i+1,i];
                                q = (d[i] - p) * (d[i] - p) + e[i] * e[i];
                                t = (x * s - z * r) / q;
                                H[i,n] = t;
                                H[i+1,n] = (Math.Abs(x) > Math.Abs(z)) ? ((-r - w * t) / x) : ((-s - y * t) / z);
                            }
         
                            // Overflow control
                            t = Math.Abs(H[i,n]);
                            if ((eps * t) * t > 1) 
                                for (int j = i; j <= n; j++)
                                    H[j,n] = H[j,n] / t;
                        }
                    }
                }
                else if (q < 0) 
                {
                    // Complex vector
                    int l = n-1;
    
                    // Last vector component imaginary so matrix is triangular
                    if (Math.Abs(H[n,n-1]) > Math.Abs(H[n-1,n])) 
                    {
                        H[n-1,n-1] = q / H[n,n-1];
                        H[n-1,n] = -(H[n,n] - p) / H[n,n-1];
                    }
                    else 
                    {
                        cdiv(0.0,-H[n-1,n],H[n-1,n-1]-p,q);
                        H[n-1,n-1] = cdivr;
                        H[n-1,n] = cdivi;
                    }
                        
                    H[n,n-1] = 0.0;
                    H[n,n] = 1.0;
                    for (int i = n-2; i >= 0; i--) 
                    {
                        double ra,sa,vr,vi;
                        ra = 0.0;
                        sa = 0.0;
                        for (int j = l; j <= n; j++) 
                        {
                            ra = ra + H[i,j] * H[j,n-1];
                            sa = sa + H[i,j] * H[j,n];
                        }
                        
                        w = H[i,i] - p;
         
                        if (e[i] < 0.0) 
                        {
                            z = w;
                            r = ra;
                            s = sa;
                        }
                        else 
                        {
                            l = i;
                            if (e[i] == 0) 
                            {
                                cdiv(-ra,-sa,w,q);
                                H[i,n-1] = cdivr;
                                H[i,n] = cdivi;
                            } 
                            else 
                            {
                                // Solve complex equations
                                x = H[i,i+1];
                                y = H[i+1,i];
                                vr = (d[i] - p) * (d[i] - p) + e[i] * e[i] - q * q;
                                vi = (d[i] - p) * 2.0 * q;
                                if (vr == 0.0 & vi == 0.0) 
                                    vr = eps * norm * (Math.Abs(w) + Math.Abs(q) + Math.Abs(x) + Math.Abs(y) + Math.Abs(z));
                                cdiv(x*r-z*ra+q*sa,x*s-z*sa-q*ra,vr,vi);
                                H[i,n-1] = cdivr;
                                H[i,n] = cdivi;
                                if (Math.Abs(x) > (Math.Abs(z) + Math.Abs(q))) 
                                {
                                    H[i+1,n-1] = (-ra - w * H[i,n-1] + q * H[i,n]) / x;
                                    H[i+1,n] = (-sa - w * H[i,n] - q * H[i,n-1]) / x;
                                }
                                else 
                                {
                                    cdiv(-r-y*H[i,n-1],-s-y*H[i,n],z,q);
                                    H[i+1,n-1] = cdivr;
                                    H[i+1,n] = cdivi;
                                }
                            }
         
                            // Overflow control
                            t = Math.Max(Math.Abs(H[i,n-1]),Math.Abs(H[i,n]));
                            if ((eps * t) * t > 1) 
                                for (int j = i; j <= n; j++) 
                                {
                                    H[j,n-1] = H[j,n-1] / t;
                                    H[j,n] = H[j,n] / t;
                                }
                        }
                    }
                }
            }
         
            // Vectors of isolated roots
            for (int i = 0; i < nn; i++) 
                if (i < low | i > high) 
                    for (int j = i; j < nn; j++) 
                        V[i,j] = H[i,j];
         
            // Back transformation to get eigenvectors of original matrix
            for (int j = nn-1; j >= low; j--) 
                for (int i = low; i <= high; i++) 
                {
                    z = 0.0;
                    for (int k = low; k <= Math.Min(j,high); k++)
                        z = z + V[i,k] * H[k,j];
                    V[i,j] = z;
                }
        }

        /// <summary>Returns the real parts of the eigenvalues.</summary>
        public double[] RealEigenvalues
        {
            get 
            { 
                return this.d; 
            }
        }
    
        /// <summary>Returns the imaginary parts of the eigenvalues.</summary>  
        public double[] ImaginaryEigenvalues
        {
            get 
            { 
                return this.e; 
            }
        }

        /// <summary>Returns the eigenvector matrix.</summary>
        public Matrix EigenvectorMatrix
        {
            get 
            { 
                return this.V; 
            }
        }
    
        /// <summary>Returns the block diagonal eigenvalue matrix.</summary>
        public Matrix DiagonalMatrix
        {
            get
            {
                Matrix X = new Matrix(n, n);
                double[][] x = X.Array;
    
                for (int i = 0; i < n; i++) 
                {
                    for (int j = 0; j < n; j++)
                        x[i][j] = 0.0;
    
                    x[i][i] = d[i];
                    if (e[i] > 0)
                    {
                        x[i][i+1] = e[i];
                    }
                    else if (e[i] < 0) 
                    {
                        x[i][i-1] = e[i];
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
