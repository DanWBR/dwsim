#region Translated by Jose Antonio De Santiago-Castillo.

//Translated by Jose Antonio De Santiago-Castillo.
//E-mail:JAntonioDeSantiago@gmail.com
//Website: www.DotNumerics.com
//
//Fortran to C# Translation.
//Translated by:
//F2CSharp Version 0.72 (Dicember 7, 2009)
//Code Optimizations: , assignment operator, for-loop: array indexes
//
#endregion

using System;
using DotNumerics.FortranLibrary;

namespace DotNumerics.LinearAlgebra.CSLapack
{
    /// <summary>
    /// -- LAPACK routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DGEQR2 computes a QR factorization of a real m by n matrix A:
    /// A = Q * R.
    /// 
    ///</summary>
    public class DGEQR2
    {
    

        #region Dependencies
        
        DLARF _dlarf; DLARFG _dlarfg; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; 

        #endregion

        public DGEQR2(DLARF dlarf, DLARFG dlarfg, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dlarf = dlarf; this._dlarfg = dlarfg; this._xerbla = xerbla; 

            #endregion

        }
    
        public DGEQR2()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);

            #endregion


            #region Set Dependencies
            
            this._dlarf = dlarf; this._dlarfg = dlarfg; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGEQR2 computes a QR factorization of a real m by n matrix A:
        /// A = Q * R.
        /// 
        ///</summary>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix A.  M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the m by n matrix A.
        /// On exit, the elements on and above the diagonal of the array
        /// contain the min(m,n) by n upper trapezoidal matrix R (R is
        /// upper triangular if m .GE. n); the elements below the diagonal,
        /// with the array TAU, represent the orthogonal matrix Q as a
        /// product of elementary reflectors (see Further Details).
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,M).
        ///</param>
        /// <param name="TAU">
        /// (output) DOUBLE PRECISION array, dimension (min(M,N))
        /// The scalar factors of the elementary reflectors (see Further
        /// Details).
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (N)
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0: successful exit
        /// .LT. 0: if INFO = -i, the i-th argument had an illegal value
        ///</param>
        public void Run(int M, int N, ref double[] A, int offset_a, int LDA, ref double[] TAU, int offset_tau, ref double[] WORK, int offset_work
                         , ref int INFO)
        {

            #region Variables
            
            int I = 0; int K = 0; double AII = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_tau = -1 + offset_tau;  int o_work = -1 + offset_work; 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK routine (version 3.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     November 2006
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DGEQR2 computes a QR factorization of a real m by n matrix A:
            // *  A = Q * R.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix A.  M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix A.  N >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the m by n matrix A.
            // *          On exit, the elements on and above the diagonal of the array
            // *          contain the min(m,n) by n upper trapezoidal matrix R (R is
            // *          upper triangular if m >= n); the elements below the diagonal,
            // *          with the array TAU, represent the orthogonal matrix Q as a
            // *          product of elementary reflectors (see Further Details).
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,M).
            // *
            // *  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
            // *          The scalar factors of the elementary reflectors (see Further
            // *          Details).
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (N)
            // *
            // *  INFO    (output) INTEGER
            // *          = 0: successful exit
            // *          < 0: if INFO = -i, the i-th argument had an illegal value
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  The matrix Q is represented as a product of elementary reflectors
            // *
            // *     Q = H(1) H(2) . . . H(k), where k = min(m,n).
            // *
            // *  Each H(i) has the form
            // *
            // *     H(i) = I - tau * v * v'
            // *
            // *  where tau is a real scalar, and v is a real vector with
            // *  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
            // *  and tau in TAU(i).
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MAX, MIN;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input arguments
            // *

            #endregion


            #region Body
            
            INFO = 0;
            if (M < 0)
            {
                INFO =  - 1;
            }
            else
            {
                if (N < 0)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (LDA < Math.Max(1, M))
                    {
                        INFO =  - 4;
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGEQR2",  - INFO);
                return;
            }
            // *
            K = Math.Min(M, N);
            // *
            for (I = 1; I <= K; I++)
            {
                // *
                // *        Generate elementary reflector H(i) to annihilate A(i+1:m,i)
                // *
                this._dlarfg.Run(M - I + 1, ref A[I+I * LDA + o_a], ref A, Math.Min(I + 1, M)+I * LDA + o_a, 1, ref TAU[I + o_tau]);
                if (I < N)
                {
                    // *
                    // *           Apply H(i) to A(i:m,i+1:n) from the left
                    // *
                    AII = A[I+I * LDA + o_a];
                    A[I+I * LDA + o_a] = ONE;
                    this._dlarf.Run("Left", M - I + 1, N - I, A, I+I * LDA + o_a, 1, TAU[I + o_tau]
                                    , ref A, I+(I + 1) * LDA + o_a, LDA, ref WORK, offset_work);
                    A[I+I * LDA + o_a] = AII;
                }
            }
            return;
            // *
            // *     End of DGEQR2
            // *

            #endregion

        }
    }
}
