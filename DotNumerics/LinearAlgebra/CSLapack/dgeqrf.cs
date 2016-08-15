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
    /// DGEQRF computes a QR factorization of a real M-by-N matrix A:
    /// A = Q * R.
    /// 
    ///</summary>
    public class DGEQRF
    {
    

        #region Dependencies
        
        DGEQR2 _dgeqr2; DLARFB _dlarfb; DLARFT _dlarft; XERBLA _xerbla; ILAENV _ilaenv; 

        #endregion

        public DGEQRF(DGEQR2 dgeqr2, DLARFB dlarfb, DLARFT dlarft, XERBLA xerbla, ILAENV ilaenv)
        {
    

            #region Set Dependencies
            
            this._dgeqr2 = dgeqr2; this._dlarfb = dlarfb; this._dlarft = dlarft; this._xerbla = xerbla; this._ilaenv = ilaenv; 

            #endregion

        }
    
        public DGEQRF()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DCOPY dcopy = new DCOPY();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DGEQR2 dgeqr2 = new DGEQR2(dlarf, dlarfg, xerbla);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);
            DLARFB dlarfb = new DLARFB(lsame, dcopy, dgemm, dtrmm);
            DTRMV dtrmv = new DTRMV(lsame, xerbla);
            DLARFT dlarft = new DLARFT(dgemv, dtrmv, lsame);
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);

            #endregion


            #region Set Dependencies
            
            this._dgeqr2 = dgeqr2; this._dlarfb = dlarfb; this._dlarft = dlarft; this._xerbla = xerbla; this._ilaenv = ilaenv; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGEQRF computes a QR factorization of a real M-by-N matrix A:
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
        /// On entry, the M-by-N matrix A.
        /// On exit, the elements on and above the diagonal of the array
        /// contain the min(M,N)-by-N upper trapezoidal matrix R (R is
        /// upper triangular if m .GE. n); the elements below the diagonal,
        /// with the array TAU, represent the orthogonal matrix Q as a
        /// product of min(m,n) elementary reflectors (see Further
        /// Details).
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
        /// (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK.  LWORK .GE. max(1,N).
        /// For optimum performance LWORK .GE. N*NB, where NB is
        /// the optimal blocksize.
        /// 
        /// If LWORK = -1, then a workspace query is assumed; the routine
        /// only calculates the optimal size of the WORK array, returns
        /// this value as the first entry of the WORK array, and no error
        /// message related to LWORK is issued by XERBLA.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
        ///</param>
        public void Run(int M, int N, ref double[] A, int offset_a, int LDA, ref double[] TAU, int offset_tau, ref double[] WORK, int offset_work
                         , int LWORK, ref int INFO)
        {

            #region Variables
            
            bool LQUERY = false; int I = 0; int IB = 0; int IINFO = 0; int IWS = 0; int K = 0; int LDWORK = 0; int LWKOPT = 0; 
            int NB = 0;int NBMIN = 0; int NX = 0; 

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
            // *  DGEQRF computes a QR factorization of a real M-by-N matrix A:
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
            // *          On entry, the M-by-N matrix A.
            // *          On exit, the elements on and above the diagonal of the array
            // *          contain the min(M,N)-by-N upper trapezoidal matrix R (R is
            // *          upper triangular if m >= n); the elements below the diagonal,
            // *          with the array TAU, represent the orthogonal matrix Q as a
            // *          product of min(m,n) elementary reflectors (see Further
            // *          Details).
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,M).
            // *
            // *  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
            // *          The scalar factors of the elementary reflectors (see Further
            // *          Details).
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK.  LWORK >= max(1,N).
            // *          For optimum performance LWORK >= N*NB, where NB is
            // *          the optimal blocksize.
            // *
            // *          If LWORK = -1, then a workspace query is assumed; the routine
            // *          only calculates the optimal size of the WORK array, returns
            // *          this value as the first entry of the WORK array, and no error
            // *          message related to LWORK is issued by XERBLA.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value
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
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MAX, MIN;
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input arguments
            // *

            #endregion


            #region Body
            
            INFO = 0;
            NB = this._ilaenv.Run(1, "DGEQRF", " ", M, N,  - 1,  - 1);
            LWKOPT = N * NB;
            WORK[1 + o_work] = LWKOPT;
            LQUERY = (LWORK ==  - 1);
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
                    else
                    {
                        if (LWORK < Math.Max(1, N) && !LQUERY)
                        {
                            INFO =  - 7;
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGEQRF",  - INFO);
                return;
            }
            else
            {
                if (LQUERY)
                {
                    return;
                }
            }
            // *
            // *     Quick return if possible
            // *
            K = Math.Min(M, N);
            if (K == 0)
            {
                WORK[1 + o_work] = 1;
                return;
            }
            // *
            NBMIN = 2;
            NX = 0;
            IWS = N;
            if (NB > 1 && NB < K)
            {
                // *
                // *        Determine when to cross over from blocked to unblocked code.
                // *
                NX = Math.Max(0, this._ilaenv.Run(3, "DGEQRF", " ", M, N,  - 1,  - 1));
                if (NX < K)
                {
                    // *
                    // *           Determine if workspace is large enough for blocked code.
                    // *
                    LDWORK = N;
                    IWS = LDWORK * NB;
                    if (LWORK < IWS)
                    {
                        // *
                        // *              Not enough workspace to use optimal NB:  reduce NB and
                        // *              determine the minimum value of NB.
                        // *
                        NB = LWORK / LDWORK;
                        NBMIN = Math.Max(2, this._ilaenv.Run(2, "DGEQRF", " ", M, N,  - 1,  - 1));
                    }
                }
            }
            // *
            if (NB >= NBMIN && NB < K && NX < K)
            {
                // *
                // *        Use blocked code initially
                // *
                for (I = 1; (NB >= 0) ? (I <= K - NX) : (I >= K - NX); I += NB)
                {
                    IB = Math.Min(K - I + 1, NB);
                    // *
                    // *           Compute the QR factorization of the current block
                    // *           A(i:m,i:i+ib-1)
                    // *
                    this._dgeqr2.Run(M - I + 1, IB, ref A, I+I * LDA + o_a, LDA, ref TAU, I + o_tau, ref WORK, offset_work
                                     , ref IINFO);
                    if (I + IB <= N)
                    {
                        // *
                        // *              Form the triangular factor of the block reflector
                        // *              H = H(i) H(i+1) . . . H(i+ib-1)
                        // *
                        this._dlarft.Run("Forward", "Columnwise", M - I + 1, IB, ref A, I+I * LDA + o_a, LDA
                                         , TAU, I + o_tau, ref WORK, offset_work, LDWORK);
                        // *
                        // *              Apply H' to A(i:m,i+ib:n) from the left
                        // *
                        this._dlarfb.Run("Left", "Transpose", "Forward", "Columnwise", M - I + 1, N - I - IB + 1
                                         , IB, A, I+I * LDA + o_a, LDA, WORK, offset_work, LDWORK, ref A, I+(I + IB) * LDA + o_a
                                         , LDA, ref WORK, IB + 1 + o_work, LDWORK);
                    }
                }
            }
            else
            {
                I = 1;
            }
            // *
            // *     Use unblocked code to factor the last or only block.
            // *
            if (I <= K)
            {
                this._dgeqr2.Run(M - I + 1, N - I + 1, ref A, I+I * LDA + o_a, LDA, ref TAU, I + o_tau, ref WORK, offset_work
                                 , ref IINFO);
            }
            // *
            WORK[1 + o_work] = IWS;
            return;
            // *
            // *     End of DGEQRF
            // *

            #endregion

        }
    }
}
