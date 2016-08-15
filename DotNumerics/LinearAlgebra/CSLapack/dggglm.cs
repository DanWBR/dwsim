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
    /// -- LAPACK driver routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DGGGLM solves a general Gauss-Markov linear model (GLM) problem:
    /// 
    /// minimize || y ||_2   subject to   d = A*x + B*y
    /// x
    /// 
    /// where A is an N-by-M matrix, B is an N-by-P matrix, and d is a
    /// given N-vector. It is assumed that M .LE. N .LE. M+P, and
    /// 
    /// rank(A) = M    and    rank( A B ) = N.
    /// 
    /// Under these assumptions, the constrained equation is always
    /// consistent, and there is a unique solution x and a minimal 2-norm
    /// solution y, which is obtained using a generalized QR factorization
    /// of the matrices (A, B) given by
    /// 
    /// A = Q*(R),   B = Q*T*Z.
    /// (0)
    /// 
    /// In particular, if matrix B is square nonsingular, then the problem
    /// GLM is equivalent to the following weighted linear least squares
    /// problem
    /// 
    /// minimize || inv(B)*(d-A*x) ||_2
    /// x
    /// 
    /// where inv(B) denotes the inverse of B.
    /// 
    ///</summary>
    public class DGGGLM
    {
    

        #region Dependencies
        
        DCOPY _dcopy; DGEMV _dgemv; DGGQRF _dggqrf; DORMQR _dormqr; DORMRQ _dormrq; DTRTRS _dtrtrs; XERBLA _xerbla; 
        ILAENV _ilaenv;

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 

        #endregion

        public DGGGLM(DCOPY dcopy, DGEMV dgemv, DGGQRF dggqrf, DORMQR dormqr, DORMRQ dormrq, DTRTRS dtrtrs, XERBLA xerbla, ILAENV ilaenv)
        {
    

            #region Set Dependencies
            
            this._dcopy = dcopy; this._dgemv = dgemv; this._dggqrf = dggqrf; this._dormqr = dormqr; this._dormrq = dormrq; 
            this._dtrtrs = dtrtrs;this._xerbla = xerbla; this._ilaenv = ilaenv; 

            #endregion

        }
    
        public DGGGLM()
        {
    

            #region Dependencies (Initialization)
            
            DCOPY dcopy = new DCOPY();
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
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
            DGEQRF dgeqrf = new DGEQRF(dgeqr2, dlarfb, dlarft, xerbla, ilaenv);
            DGERQ2 dgerq2 = new DGERQ2(dlarf, dlarfg, xerbla);
            DGERQF dgerqf = new DGERQF(dgerq2, dlarfb, dlarft, xerbla, ilaenv);
            DORM2R dorm2r = new DORM2R(lsame, dlarf, xerbla);
            DORMQR dormqr = new DORMQR(lsame, ilaenv, dlarfb, dlarft, dorm2r, xerbla);
            DGGQRF dggqrf = new DGGQRF(dgeqrf, dgerqf, dormqr, xerbla, ilaenv);
            DORMR2 dormr2 = new DORMR2(lsame, dlarf, xerbla);
            DORMRQ dormrq = new DORMRQ(lsame, ilaenv, dlarfb, dlarft, dormr2, xerbla);
            DTRSM dtrsm = new DTRSM(lsame, xerbla);
            DTRTRS dtrtrs = new DTRTRS(lsame, dtrsm, xerbla);

            #endregion


            #region Set Dependencies
            
            this._dcopy = dcopy; this._dgemv = dgemv; this._dggqrf = dggqrf; this._dormqr = dormqr; this._dormrq = dormrq; 
            this._dtrtrs = dtrtrs;this._xerbla = xerbla; this._ilaenv = ilaenv; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGGGLM solves a general Gauss-Markov linear model (GLM) problem:
        /// 
        /// minimize || y ||_2   subject to   d = A*x + B*y
        /// x
        /// 
        /// where A is an N-by-M matrix, B is an N-by-P matrix, and d is a
        /// given N-vector. It is assumed that M .LE. N .LE. M+P, and
        /// 
        /// rank(A) = M    and    rank( A B ) = N.
        /// 
        /// Under these assumptions, the constrained equation is always
        /// consistent, and there is a unique solution x and a minimal 2-norm
        /// solution y, which is obtained using a generalized QR factorization
        /// of the matrices (A, B) given by
        /// 
        /// A = Q*(R),   B = Q*T*Z.
        /// (0)
        /// 
        /// In particular, if matrix B is square nonsingular, then the problem
        /// GLM is equivalent to the following weighted linear least squares
        /// problem
        /// 
        /// minimize || inv(B)*(d-A*x) ||_2
        /// x
        /// 
        /// where inv(B) denotes the inverse of B.
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of rows of the matrices A and B.  N .GE. 0.
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of columns of the matrix A.  0 .LE. M .LE. N.
        ///</param>
        /// <param name="P">
        /// (input) INTEGER
        /// The number of columns of the matrix B.  P .GE. N-M.
        ///</param>
        /// <param name="A">
        /// = Q*(R),   B = Q*T*Z.
        /// (0)
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A. LDA .GE. max(1,N).
        ///</param>
        /// <param name="B">
        /// (input/output) DOUBLE PRECISION array, dimension (LDB,P)
        /// On entry, the N-by-P matrix B.
        /// On exit, if N .LE. P, the upper triangle of the subarray
        /// B(1:N,P-N+1:P) contains the N-by-N upper triangular matrix T;
        /// if N .GT. P, the elements on and above the (N-P)th subdiagonal
        /// contain the N-by-P upper trapezoidal matrix T.
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of the array B. LDB .GE. max(1,N).
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array, dimension (N)
        /// On entry, D is the left hand side of the GLM equation.
        /// On exit, D is destroyed.
        ///</param>
        /// <param name="X">
        /// (output) DOUBLE PRECISION array, dimension (M)
        ///</param>
        /// <param name="Y">
        /// (output) DOUBLE PRECISION array, dimension (P)
        /// On exit, X and Y are the solutions of the GLM problem.
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK. LWORK .GE. max(1,N+M+P).
        /// For optimum performance, LWORK .GE. M+min(N,P)+max(N,P)*NB,
        /// where NB is an upper bound for the optimal blocksizes for
        /// DGEQRF, SGERQF, DORMQR and SORMRQ.
        /// 
        /// If LWORK = -1, then a workspace query is assumed; the routine
        /// only calculates the optimal size of the WORK array, returns
        /// this value as the first entry of the WORK array, and no error
        /// message related to LWORK is issued by XERBLA.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// = 1:  the upper triangular factor R associated with A in the
        /// generalized QR factorization of the pair (A, B) is
        /// singular, so that rank(A) .LT. M; the least squares
        /// solution could not be computed.
        /// = 2:  the bottom (N-M) by (N-M) part of the upper trapezoidal
        /// factor T associated with B in the generalized QR
        /// factorization of the pair (A, B) is singular, so that
        /// rank( A B ) .LT. N; the least squares solution could not
        /// be computed.
        ///</param>
        public void Run(int N, int M, int P, ref double[] A, int offset_a, int LDA, ref double[] B, int offset_b
                         , int LDB, ref double[] D, int offset_d, ref double[] X, int offset_x, ref double[] Y, int offset_y, ref double[] WORK, int offset_work, int LWORK
                         , ref int INFO)
        {

            #region Variables
            
            bool LQUERY = false; int I = 0; int LOPT = 0; int LWKMIN = 0; int LWKOPT = 0; int NB = 0; int NB1 = 0; int NB2 = 0; 
            int NB3 = 0;int NB4 = 0; int NP = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_b = -1 - LDB + offset_b;  int o_d = -1 + offset_d;  int o_x = -1 + offset_x; 
             int o_y = -1 + offset_y; int o_work = -1 + offset_work; 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK driver routine (version 3.1) --
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
            // *  DGGGLM solves a general Gauss-Markov linear model (GLM) problem:
            // *
            // *          minimize || y ||_2   subject to   d = A*x + B*y
            // *              x
            // *
            // *  where A is an N-by-M matrix, B is an N-by-P matrix, and d is a
            // *  given N-vector. It is assumed that M <= N <= M+P, and
            // *
            // *             rank(A) = M    and    rank( A B ) = N.
            // *
            // *  Under these assumptions, the constrained equation is always
            // *  consistent, and there is a unique solution x and a minimal 2-norm
            // *  solution y, which is obtained using a generalized QR factorization
            // *  of the matrices (A, B) given by
            // *
            // *     A = Q*(R),   B = Q*T*Z.
            // *           (0)
            // *
            // *  In particular, if matrix B is square nonsingular, then the problem
            // *  GLM is equivalent to the following weighted linear least squares
            // *  problem
            // *
            // *               minimize || inv(B)*(d-A*x) ||_2
            // *                   x
            // *
            // *  where inv(B) denotes the inverse of B.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N       (input) INTEGER
            // *          The number of rows of the matrices A and B.  N >= 0.
            // *
            // *  M       (input) INTEGER
            // *          The number of columns of the matrix A.  0 <= M <= N.
            // *
            // *  P       (input) INTEGER
            // *          The number of columns of the matrix B.  P >= N-M.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,M)
            // *          On entry, the N-by-M matrix A.
            // *          On exit, the upper triangular part of the array A contains
            // *          the M-by-M upper triangular matrix R.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A. LDA >= max(1,N).
            // *
            // *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,P)
            // *          On entry, the N-by-P matrix B.
            // *          On exit, if N <= P, the upper triangle of the subarray
            // *          B(1:N,P-N+1:P) contains the N-by-N upper triangular matrix T;
            // *          if N > P, the elements on and above the (N-P)th subdiagonal
            // *          contain the N-by-P upper trapezoidal matrix T.
            // *
            // *  LDB     (input) INTEGER
            // *          The leading dimension of the array B. LDB >= max(1,N).
            // *
            // *  D       (input/output) DOUBLE PRECISION array, dimension (N)
            // *          On entry, D is the left hand side of the GLM equation.
            // *          On exit, D is destroyed.
            // *
            // *  X       (output) DOUBLE PRECISION array, dimension (M)
            // *  Y       (output) DOUBLE PRECISION array, dimension (P)
            // *          On exit, X and Y are the solutions of the GLM problem.
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK. LWORK >= max(1,N+M+P).
            // *          For optimum performance, LWORK >= M+min(N,P)+max(N,P)*NB,
            // *          where NB is an upper bound for the optimal blocksizes for
            // *          DGEQRF, SGERQF, DORMQR and SORMRQ.
            // *
            // *          If LWORK = -1, then a workspace query is assumed; the routine
            // *          only calculates the optimal size of the WORK array, returns
            // *          this value as the first entry of the WORK array, and no error
            // *          message related to LWORK is issued by XERBLA.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit.
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *          = 1:  the upper triangular factor R associated with A in the
            // *                generalized QR factorization of the pair (A, B) is
            // *                singular, so that rank(A) < M; the least squares
            // *                solution could not be computed.
            // *          = 2:  the bottom (N-M) by (N-M) part of the upper trapezoidal
            // *                factor T associated with B in the generalized QR
            // *                factorization of the pair (A, B) is singular, so that
            // *                rank( A B ) < N; the least squares solution could not
            // *                be computed.
            // *
            // *  ===================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          INT, MAX, MIN;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters
            // *

            #endregion


            #region Body
            
            INFO = 0;
            NP = Math.Min(N, P);
            LQUERY = (LWORK ==  - 1);
            if (N < 0)
            {
                INFO =  - 1;
            }
            else
            {
                if (M < 0 || M > N)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (P < 0 || P < N - M)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (LDA < Math.Max(1, N))
                        {
                            INFO =  - 5;
                        }
                        else
                        {
                            if (LDB < Math.Max(1, N))
                            {
                                INFO =  - 7;
                            }
                        }
                    }
                }
            }
            // *
            // *     Calculate workspace
            // *
            if (INFO == 0)
            {
                if (N == 0)
                {
                    LWKMIN = 1;
                    LWKOPT = 1;
                }
                else
                {
                    NB1 = this._ilaenv.Run(1, "DGEQRF", " ", N, M,  - 1,  - 1);
                    NB2 = this._ilaenv.Run(1, "DGERQF", " ", N, M,  - 1,  - 1);
                    NB3 = this._ilaenv.Run(1, "DORMQR", " ", N, M, P,  - 1);
                    NB4 = this._ilaenv.Run(1, "DORMRQ", " ", N, M, P,  - 1);
                    NB = Math.Max(NB1, Math.Max(NB2, Math.Max(NB3, NB4)));
                    LWKMIN = M + N + P;
                    LWKOPT = M + NP + Math.Max(N, P) * NB;
                }
                WORK[1 + o_work] = LWKOPT;
                // *
                if (LWORK < LWKMIN && !LQUERY)
                {
                    INFO =  - 12;
                }
            }
            // *
            if (INFO != 0)
            {
                this._xerbla.Run("DGGGLM",  - INFO);
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
            if (N == 0) return;
            // *
            // *     Compute the GQR factorization of matrices A and B:
            // *
            // *            Q'*A = ( R11 ) M,    Q'*B*Z' = ( T11   T12 ) M
            // *                   (  0  ) N-M             (  0    T22 ) N-M
            // *                      M                     M+P-N  N-M
            // *
            // *     where R11 and T22 are upper triangular, and Q and Z are
            // *     orthogonal.
            // *
            this._dggqrf.Run(N, M, P, ref A, offset_a, LDA, ref WORK, offset_work
                             , ref B, offset_b, LDB, ref WORK, M + 1 + o_work, ref WORK, M + NP + 1 + o_work, LWORK - M - NP, ref INFO);
            LOPT = (int)WORK[M + NP + 1 + o_work];
            // *
            // *     Update left-hand-side vector d = Q'*d = ( d1 ) M
            // *                                             ( d2 ) N-M
            // *
            this._dormqr.Run("Left", "Transpose", N, 1, M, ref A, offset_a
                             , LDA, WORK, offset_work, ref D, offset_d, Math.Max(1, N), ref WORK, M + NP + 1 + o_work, LWORK - M - NP
                             , ref INFO);
            LOPT = Math.Max(LOPT, Convert.ToInt32(Math.Truncate(WORK[M + NP + 1 + o_work])));
            // *
            // *     Solve T22*y2 = d2 for y2
            // *
            if (N > M)
            {
                this._dtrtrs.Run("Upper", "No transpose", "Non unit", N - M, 1, B, M + 1+(M + P - N + 1) * LDB + o_b
                                 , LDB, ref D, M + 1 + o_d, N - M, ref INFO);
                // *
                if (INFO > 0)
                {
                    INFO = 1;
                    return;
                }
                // *
                this._dcopy.Run(N - M, D, M + 1 + o_d, 1, ref Y, M + P - N + 1 + o_y, 1);
            }
            // *
            // *     Set y1 = 0
            // *
            for (I = 1; I <= M + P - N; I++)
            {
                Y[I + o_y] = ZERO;
            }
            // *
            // *     Update d1 = d1 - T12*y2
            // *
            this._dgemv.Run("No transpose", M, N - M,  - ONE, B, 1+(M + P - N + 1) * LDB + o_b, LDB
                            , Y, M + P - N + 1 + o_y, 1, ONE, ref D, offset_d, 1);
            // *
            // *     Solve triangular system: R11*x = d1
            // *
            if (M > 0)
            {
                this._dtrtrs.Run("Upper", "No Transpose", "Non unit", M, 1, A, offset_a
                                 , LDA, ref D, offset_d, M, ref INFO);
                // *
                if (INFO > 0)
                {
                    INFO = 2;
                    return;
                }
                // *
                // *        Copy D to X
                // *
                this._dcopy.Run(M, D, offset_d, 1, ref X, offset_x, 1);
            }
            // *
            // *     Backward transformation y = Z'*y
            // *
            this._dormrq.Run("Left", "Transpose", P, 1, NP, ref B, Math.Max(1, N - P + 1)+1 * LDB + o_b
                             , LDB, WORK, M + 1 + o_work, ref Y, offset_y, Math.Max(1, P), ref WORK, M + NP + 1 + o_work, LWORK - M - NP
                             , ref INFO);
            WORK[1 + o_work] = M + NP + Math.Max(LOPT, Convert.ToInt32(Math.Truncate(WORK[M + NP + 1 + o_work])));
            // *
            return;
            // *
            // *     End of DGGGLM
            // *

            #endregion

        }
    }
}
