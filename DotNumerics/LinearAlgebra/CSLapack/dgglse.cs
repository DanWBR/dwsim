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
    /// DGGLSE solves the linear equality-constrained least squares (LSE)
    /// problem:
    /// 
    /// minimize || c - A*x ||_2   subject to   B*x = d
    /// 
    /// where A is an M-by-N matrix, B is a P-by-N matrix, c is a given
    /// M-vector, and d is a given P-vector. It is assumed that
    /// P .LE. N .LE. M+P, and
    /// 
    /// rank(B) = P and  rank( (A) ) = N.
    /// ( (B) )
    /// 
    /// These conditions ensure that the LSE problem has a unique solution,
    /// which is obtained using a generalized RQ factorization of the
    /// matrices (B, A) given by
    /// 
    /// B = (0 R)*Q,   A = Z*T*Q.
    /// 
    ///</summary>
    public class DGGLSE
    {
    

        #region Dependencies
        
        DAXPY _daxpy; DCOPY _dcopy; DGEMV _dgemv; DGGRQF _dggrqf; DORMQR _dormqr; DORMRQ _dormrq; DTRMV _dtrmv; DTRTRS _dtrtrs; 
        XERBLA _xerbla;ILAENV _ilaenv; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; 

        #endregion

        public DGGLSE(DAXPY daxpy, DCOPY dcopy, DGEMV dgemv, DGGRQF dggrqf, DORMQR dormqr, DORMRQ dormrq, DTRMV dtrmv, DTRTRS dtrtrs, XERBLA xerbla, ILAENV ilaenv)
        {
    

            #region Set Dependencies
            
            this._daxpy = daxpy; this._dcopy = dcopy; this._dgemv = dgemv; this._dggrqf = dggrqf; this._dormqr = dormqr; 
            this._dormrq = dormrq;this._dtrmv = dtrmv; this._dtrtrs = dtrtrs; this._xerbla = xerbla; this._ilaenv = ilaenv; 

            #endregion

        }
    
        public DGGLSE()
        {
    

            #region Dependencies (Initialization)
            
            DAXPY daxpy = new DAXPY();
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
            DORMR2 dormr2 = new DORMR2(lsame, dlarf, xerbla);
            DORMRQ dormrq = new DORMRQ(lsame, ilaenv, dlarfb, dlarft, dormr2, xerbla);
            DGGRQF dggrqf = new DGGRQF(dgeqrf, dgerqf, dormrq, xerbla, ilaenv);
            DORM2R dorm2r = new DORM2R(lsame, dlarf, xerbla);
            DORMQR dormqr = new DORMQR(lsame, ilaenv, dlarfb, dlarft, dorm2r, xerbla);
            DTRSM dtrsm = new DTRSM(lsame, xerbla);
            DTRTRS dtrtrs = new DTRTRS(lsame, dtrsm, xerbla);

            #endregion


            #region Set Dependencies
            
            this._daxpy = daxpy; this._dcopy = dcopy; this._dgemv = dgemv; this._dggrqf = dggrqf; this._dormqr = dormqr; 
            this._dormrq = dormrq;this._dtrmv = dtrmv; this._dtrtrs = dtrtrs; this._xerbla = xerbla; this._ilaenv = ilaenv; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGGLSE solves the linear equality-constrained least squares (LSE)
        /// problem:
        /// 
        /// minimize || c - A*x ||_2   subject to   B*x = d
        /// 
        /// where A is an M-by-N matrix, B is a P-by-N matrix, c is a given
        /// M-vector, and d is a given P-vector. It is assumed that
        /// P .LE. N .LE. M+P, and
        /// 
        /// rank(B) = P and  rank( (A) ) = N.
        /// ( (B) )
        /// 
        /// These conditions ensure that the LSE problem has a unique solution,
        /// which is obtained using a generalized RQ factorization of the
        /// matrices (B, A) given by
        /// 
        /// B = (0 R)*Q,   A = Z*T*Q.
        /// 
        ///</summary>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix A.  M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrices A and B. N .GE. 0.
        ///</param>
        /// <param name="P">
        /// (input) INTEGER
        /// The number of rows of the matrix B. 0 .LE. P .LE. N .LE. M+P.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the M-by-N matrix A.
        /// On exit, the elements on and above the diagonal of the array
        /// contain the min(M,N)-by-N upper trapezoidal matrix T.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A. LDA .GE. max(1,M).
        ///</param>
        /// <param name="B">
        /// = (0 R)*Q,   A = Z*T*Q.
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of the array B. LDB .GE. max(1,P).
        ///</param>
        /// <param name="C">
        /// (input/output) DOUBLE PRECISION array, dimension (M)
        /// On entry, C contains the right hand side vector for the
        /// least squares part of the LSE problem.
        /// On exit, the residual sum of squares for the solution
        /// is given by the sum of squares of elements N-P+1 to M of
        /// vector C.
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array, dimension (P)
        /// On entry, D contains the right hand side vector for the
        /// constrained equation.
        /// On exit, D is destroyed.
        ///</param>
        /// <param name="X">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// On exit, X is the solution of the LSE problem.
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK. LWORK .GE. max(1,M+N+P).
        /// For optimum performance LWORK .GE. P+min(M,N)+max(M,N)*NB,
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
        /// = 1:  the upper triangular factor R associated with B in the
        /// generalized RQ factorization of the pair (B, A) is
        /// singular, so that rank(B) .LT. P; the least squares
        /// solution could not be computed.
        /// = 2:  the (N-P) by (N-P) part of the upper trapezoidal factor
        /// T associated with A in the generalized RQ factorization
        /// of the pair (B, A) is singular, so that
        /// rank( (A) ) .LT. N; the least squares solution could not
        /// ( (B) )
        /// be computed.
        ///</param>
        public void Run(int M, int N, int P, ref double[] A, int offset_a, int LDA, ref double[] B, int offset_b
                         , int LDB, ref double[] C, int offset_c, ref double[] D, int offset_d, ref double[] X, int offset_x, ref double[] WORK, int offset_work, int LWORK
                         , ref int INFO)
        {

            #region Variables
            
            bool LQUERY = false; int LOPT = 0; int LWKMIN = 0; int LWKOPT = 0; int MN = 0; int NB = 0; int NB1 = 0; int NB2 = 0; 
            int NB3 = 0;int NB4 = 0; int NR = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_b = -1 - LDB + offset_b;  int o_c = -1 + offset_c;  int o_d = -1 + offset_d; 
             int o_x = -1 + offset_x; int o_work = -1 + offset_work; 

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
            // *  DGGLSE solves the linear equality-constrained least squares (LSE)
            // *  problem:
            // *
            // *          minimize || c - A*x ||_2   subject to   B*x = d
            // *
            // *  where A is an M-by-N matrix, B is a P-by-N matrix, c is a given
            // *  M-vector, and d is a given P-vector. It is assumed that
            // *  P <= N <= M+P, and
            // *
            // *           rank(B) = P and  rank( (A) ) = N.
            // *                                ( (B) )
            // *
            // *  These conditions ensure that the LSE problem has a unique solution,
            // *  which is obtained using a generalized RQ factorization of the
            // *  matrices (B, A) given by
            // *
            // *     B = (0 R)*Q,   A = Z*T*Q.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix A.  M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrices A and B. N >= 0.
            // *
            // *  P       (input) INTEGER
            // *          The number of rows of the matrix B. 0 <= P <= N <= M+P.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the M-by-N matrix A.
            // *          On exit, the elements on and above the diagonal of the array
            // *          contain the min(M,N)-by-N upper trapezoidal matrix T.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A. LDA >= max(1,M).
            // *
            // *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,N)
            // *          On entry, the P-by-N matrix B.
            // *          On exit, the upper triangle of the subarray B(1:P,N-P+1:N)
            // *          contains the P-by-P upper triangular matrix R.
            // *
            // *  LDB     (input) INTEGER
            // *          The leading dimension of the array B. LDB >= max(1,P).
            // *
            // *  C       (input/output) DOUBLE PRECISION array, dimension (M)
            // *          On entry, C contains the right hand side vector for the
            // *          least squares part of the LSE problem.
            // *          On exit, the residual sum of squares for the solution
            // *          is given by the sum of squares of elements N-P+1 to M of
            // *          vector C.
            // *
            // *  D       (input/output) DOUBLE PRECISION array, dimension (P)
            // *          On entry, D contains the right hand side vector for the
            // *          constrained equation.
            // *          On exit, D is destroyed.
            // *
            // *  X       (output) DOUBLE PRECISION array, dimension (N)
            // *          On exit, X is the solution of the LSE problem.
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK. LWORK >= max(1,M+N+P).
            // *          For optimum performance LWORK >= P+min(M,N)+max(M,N)*NB,
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
            // *          = 1:  the upper triangular factor R associated with B in the
            // *                generalized RQ factorization of the pair (B, A) is
            // *                singular, so that rank(B) < P; the least squares
            // *                solution could not be computed.
            // *          = 2:  the (N-P) by (N-P) part of the upper trapezoidal factor
            // *                T associated with A in the generalized RQ factorization
            // *                of the pair (B, A) is singular, so that
            // *                rank( (A) ) < N; the least squares solution could not
            // *                    ( (B) )
            // *                be computed.
            // *
            // *  =====================================================================
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
            MN = Math.Min(M, N);
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
                    if (P < 0 || P > N || P < N - M)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (LDA < Math.Max(1, M))
                        {
                            INFO =  - 5;
                        }
                        else
                        {
                            if (LDB < Math.Max(1, P))
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
                    NB1 = this._ilaenv.Run(1, "DGEQRF", " ", M, N,  - 1,  - 1);
                    NB2 = this._ilaenv.Run(1, "DGERQF", " ", M, N,  - 1,  - 1);
                    NB3 = this._ilaenv.Run(1, "DORMQR", " ", M, N, P,  - 1);
                    NB4 = this._ilaenv.Run(1, "DORMRQ", " ", M, N, P,  - 1);
                    NB = Math.Max(NB1, Math.Max(NB2, Math.Max(NB3, NB4)));
                    LWKMIN = M + N + P;
                    LWKOPT = P + MN + Math.Max(M, N) * NB;
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
                this._xerbla.Run("DGGLSE",  - INFO);
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
            // *     Compute the GRQ factorization of matrices B and A:
            // *
            // *            B*Q' = (  0  T12 ) P   Z'*A*Q' = ( R11 R12 ) N-P
            // *                     N-P  P                  (  0  R22 ) M+P-N
            // *                                               N-P  P
            // *
            // *     where T12 and R11 are upper triangular, and Q and Z are
            // *     orthogonal.
            // *
            this._dggrqf.Run(P, M, N, ref B, offset_b, LDB, ref WORK, offset_work
                             , ref A, offset_a, LDA, ref WORK, P + 1 + o_work, ref WORK, P + MN + 1 + o_work, LWORK - P - MN, ref INFO);
            LOPT = (int)WORK[P + MN + 1 + o_work];
            // *
            // *     Update c = Z'*c = ( c1 ) N-P
            // *                       ( c2 ) M+P-N
            // *
            this._dormqr.Run("Left", "Transpose", M, 1, MN, ref A, offset_a
                             , LDA, WORK, P + 1 + o_work, ref C, offset_c, Math.Max(1, M), ref WORK, P + MN + 1 + o_work, LWORK - P - MN
                             , ref INFO);
            LOPT = Math.Max(LOPT, Convert.ToInt32(Math.Truncate(WORK[P + MN + 1 + o_work])));
            // *
            // *     Solve T12*x2 = d for x2
            // *
            if (P > 0)
            {
                this._dtrtrs.Run("Upper", "No transpose", "Non-unit", P, 1, B, 1+(N - P + 1) * LDB + o_b
                                 , LDB, ref D, offset_d, P, ref INFO);
                // *
                if (INFO > 0)
                {
                    INFO = 1;
                    return;
                }
                // *
                // *        Put the solution in X
                // *
                this._dcopy.Run(P, D, offset_d, 1, ref X, N - P + 1 + o_x, 1);
                // *
                // *        Update c1
                // *
                this._dgemv.Run("No transpose", N - P, P,  - ONE, A, 1+(N - P + 1) * LDA + o_a, LDA
                                , D, offset_d, 1, ONE, ref C, offset_c, 1);
            }
            // *
            // *     Solve R11*x1 = c1 for x1
            // *
            if (N > P)
            {
                this._dtrtrs.Run("Upper", "No transpose", "Non-unit", N - P, 1, A, offset_a
                                 , LDA, ref C, offset_c, N - P, ref INFO);
                // *
                if (INFO > 0)
                {
                    INFO = 2;
                    return;
                }
                // *
                // *        Put the solutions in X
                // *
                this._dcopy.Run(N - P, C, offset_c, 1, ref X, offset_x, 1);
            }
            // *
            // *     Compute the residual vector:
            // *
            if (M < N)
            {
                NR = M + P - N;
                if (NR > 0)
                {
                    this._dgemv.Run("No transpose", NR, N - M,  - ONE, A, N - P + 1+(M + 1) * LDA + o_a, LDA
                                    , D, NR + 1 + o_d, 1, ONE, ref C, N - P + 1 + o_c, 1);
                }
            }
            else
            {
                NR = P;
            }
            if (NR > 0)
            {
                this._dtrmv.Run("Upper", "No transpose", "Non unit", NR, A, N - P + 1+(N - P + 1) * LDA + o_a, LDA
                                , ref D, offset_d, 1);
                this._daxpy.Run(NR,  - ONE, D, offset_d, 1, ref C, N - P + 1 + o_c, 1);
            }
            // *
            // *     Backward transformation x = Q'*x
            // *
            this._dormrq.Run("Left", "Transpose", N, 1, P, ref B, offset_b
                             , LDB, WORK, 1 + o_work, ref X, offset_x, N, ref WORK, P + MN + 1 + o_work, LWORK - P - MN
                             , ref INFO);
            WORK[1 + o_work] = P + MN + Math.Max(LOPT, Convert.ToInt32(Math.Truncate(WORK[P + MN + 1 + o_work])));
            // *
            return;
            // *
            // *     End of DGGLSE
            // *

            #endregion

        }
    }
}
