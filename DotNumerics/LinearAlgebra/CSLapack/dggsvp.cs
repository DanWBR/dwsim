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
    /// DGGSVP computes orthogonal matrices U, V and Q such that
    /// 
    /// N-K-L  K    L
    /// U'*A*Q =     K ( 0    A12  A13 )  if M-K-L .GE. 0;
    /// L ( 0     0   A23 )
    /// M-K-L ( 0     0    0  )
    /// 
    /// N-K-L  K    L
    /// =     K ( 0    A12  A13 )  if M-K-L .LT. 0;
    /// M-K ( 0     0   A23 )
    /// 
    /// N-K-L  K    L
    /// V'*B*Q =   L ( 0     0   B13 )
    /// P-L ( 0     0    0  )
    /// 
    /// where the K-by-K matrix A12 and L-by-L matrix B13 are nonsingular
    /// upper triangular; A23 is L-by-L upper triangular if M-K-L .GE. 0,
    /// otherwise A23 is (M-K)-by-L upper trapezoidal.  K+L = the effective
    /// numerical rank of the (M+P)-by-N matrix (A',B')'.  Z' denotes the
    /// transpose of Z.
    /// 
    /// This decomposition is the preprocessing step for computing the
    /// Generalized Singular Value Decomposition (GSVD), see subroutine
    /// DGGSVD.
    /// 
    ///</summary>
    public class DGGSVP
    {
    

        #region Dependencies
        
        LSAME _lsame; DGEQPF _dgeqpf; DGEQR2 _dgeqr2; DGERQ2 _dgerq2; DLACPY _dlacpy; DLAPMT _dlapmt; DLASET _dlaset; 
        DORG2R _dorg2r;DORM2R _dorm2r; DORMR2 _dormr2; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 

        #endregion

        public DGGSVP(LSAME lsame, DGEQPF dgeqpf, DGEQR2 dgeqr2, DGERQ2 dgerq2, DLACPY dlacpy, DLAPMT dlapmt, DLASET dlaset, DORG2R dorg2r, DORM2R dorm2r, DORMR2 dormr2
                      , XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dgeqpf = dgeqpf; this._dgeqr2 = dgeqr2; this._dgerq2 = dgerq2; this._dlacpy = dlacpy; 
            this._dlapmt = dlapmt;this._dlaset = dlaset; this._dorg2r = dorg2r; this._dorm2r = dorm2r; this._dormr2 = dormr2; 
            this._xerbla = xerbla;

            #endregion

        }
    
        public DGGSVP()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DSWAP dswap = new DSWAP();
            IDAMAX idamax = new IDAMAX();
            DLAPMT dlapmt = new DLAPMT();
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
            DORM2R dorm2r = new DORM2R(lsame, dlarf, xerbla);
            DGEQPF dgeqpf = new DGEQPF(dgeqr2, dlarf, dlarfg, dorm2r, dswap, xerbla, idamax, dlamch, dnrm2);
            DGERQ2 dgerq2 = new DGERQ2(dlarf, dlarfg, xerbla);
            DLACPY dlacpy = new DLACPY(lsame);
            DLASET dlaset = new DLASET(lsame);
            DORG2R dorg2r = new DORG2R(dlarf, dscal, xerbla);
            DORMR2 dormr2 = new DORMR2(lsame, dlarf, xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dgeqpf = dgeqpf; this._dgeqr2 = dgeqr2; this._dgerq2 = dgerq2; this._dlacpy = dlacpy; 
            this._dlapmt = dlapmt;this._dlaset = dlaset; this._dorg2r = dorg2r; this._dorm2r = dorm2r; this._dormr2 = dormr2; 
            this._xerbla = xerbla;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGGSVP computes orthogonal matrices U, V and Q such that
        /// 
        /// N-K-L  K    L
        /// U'*A*Q =     K ( 0    A12  A13 )  if M-K-L .GE. 0;
        /// L ( 0     0   A23 )
        /// M-K-L ( 0     0    0  )
        /// 
        /// N-K-L  K    L
        /// =     K ( 0    A12  A13 )  if M-K-L .LT. 0;
        /// M-K ( 0     0   A23 )
        /// 
        /// N-K-L  K    L
        /// V'*B*Q =   L ( 0     0   B13 )
        /// P-L ( 0     0    0  )
        /// 
        /// where the K-by-K matrix A12 and L-by-L matrix B13 are nonsingular
        /// upper triangular; A23 is L-by-L upper triangular if M-K-L .GE. 0,
        /// otherwise A23 is (M-K)-by-L upper trapezoidal.  K+L = the effective
        /// numerical rank of the (M+P)-by-N matrix (A',B')'.  Z' denotes the
        /// transpose of Z.
        /// 
        /// This decomposition is the preprocessing step for computing the
        /// Generalized Singular Value Decomposition (GSVD), see subroutine
        /// DGGSVD.
        /// 
        ///</summary>
        /// <param name="JOBU">
        /// (input) CHARACTER*1
        /// = 'U':  Orthogonal matrix U is computed;
        /// = 'N':  U is not computed.
        ///</param>
        /// <param name="JOBV">
        /// (input) CHARACTER*1
        /// = 'V':  Orthogonal matrix V is computed;
        /// = 'N':  V is not computed.
        ///</param>
        /// <param name="JOBQ">
        /// (input) CHARACTER*1
        /// = 'Q':  Orthogonal matrix Q is computed;
        /// = 'N':  Q is not computed.
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix A.  M .GE. 0.
        ///</param>
        /// <param name="P">
        /// (input) INTEGER
        /// The number of rows of the matrix B.  P .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrices A and B.  N .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the M-by-N matrix A.
        /// On exit, A contains the triangular (or trapezoidal) matrix
        /// described in the Purpose section.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A. LDA .GE. max(1,M).
        ///</param>
        /// <param name="B">
        /// (input/output) DOUBLE PRECISION array, dimension (LDB,N)
        /// On entry, the P-by-N matrix B.
        /// On exit, B contains the triangular matrix described in
        /// the Purpose section.
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of the array B. LDB .GE. max(1,P).
        ///</param>
        /// <param name="TOLA">
        /// (input) DOUBLE PRECISION
        ///</param>
        /// <param name="TOLB">
        /// (input) DOUBLE PRECISION
        /// TOLA and TOLB are the thresholds to determine the effective
        /// numerical rank of matrix B and a subblock of A. Generally,
        /// they are set to
        /// TOLA = MAX(M,N)*norm(A)*MAZHEPS,
        /// TOLB = MAX(P,N)*norm(B)*MAZHEPS.
        /// The size of TOLA and TOLB may affect the size of backward
        /// errors of the decomposition.
        ///</param>
        /// <param name="K">
        /// (output) INTEGER
        ///</param>
        /// <param name="L">
        /// ( 0     0   A23 )
        ///</param>
        /// <param name="U">
        /// (output) DOUBLE PRECISION array, dimension (LDU,M)
        /// If JOBU = 'U', U contains the orthogonal matrix U.
        /// If JOBU = 'N', U is not referenced.
        ///</param>
        /// <param name="LDU">
        /// (input) INTEGER
        /// The leading dimension of the array U. LDU .GE. max(1,M) if
        /// JOBU = 'U'; LDU .GE. 1 otherwise.
        ///</param>
        /// <param name="V">
        /// (output) DOUBLE PRECISION array, dimension (LDV,M)
        /// If JOBV = 'V', V contains the orthogonal matrix V.
        /// If JOBV = 'N', V is not referenced.
        ///</param>
        /// <param name="LDV">
        /// (input) INTEGER
        /// The leading dimension of the array V. LDV .GE. max(1,P) if
        /// JOBV = 'V'; LDV .GE. 1 otherwise.
        ///</param>
        /// <param name="Q">
        /// (output) DOUBLE PRECISION array, dimension (LDQ,N)
        /// If JOBQ = 'Q', Q contains the orthogonal matrix Q.
        /// If JOBQ = 'N', Q is not referenced.
        ///</param>
        /// <param name="LDQ">
        /// (input) INTEGER
        /// The leading dimension of the array Q. LDQ .GE. max(1,N) if
        /// JOBQ = 'Q'; LDQ .GE. 1 otherwise.
        ///</param>
        /// <param name="IWORK">
        /// (workspace) INTEGER array, dimension (N)
        ///</param>
        /// <param name="TAU">
        /// (workspace) DOUBLE PRECISION array, dimension (N)
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (max(3*N,M,P))
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// 
        ///</param>
        public void Run(string JOBU, string JOBV, string JOBQ, int M, int P, int N
                         , ref double[] A, int offset_a, int LDA, ref double[] B, int offset_b, int LDB, double TOLA, double TOLB
                         , ref int K, ref int L, ref double[] U, int offset_u, int LDU, ref double[] V, int offset_v, int LDV
                         , ref double[] Q, int offset_q, int LDQ, ref int[] IWORK, int offset_iwork, ref double[] TAU, int offset_tau, ref double[] WORK, int offset_work, ref int INFO)
        {

            #region Variables
            
            bool FORWRD = false; bool WANTQ = false; bool WANTU = false; bool WANTV = false; int I = 0; int J = 0; 

            #endregion


            #region Implicit Variables
            
            int B_J = 0; int A_J = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_b = -1 - LDB + offset_b;  int o_u = -1 - LDU + offset_u; 
             int o_v = -1 - LDV + offset_v; int o_q = -1 - LDQ + offset_q;  int o_iwork = -1 + offset_iwork; 
             int o_tau = -1 + offset_tau; int o_work = -1 + offset_work; 

            #endregion


            #region Strings
            
            JOBU = JOBU.Substring(0, 1);  JOBV = JOBV.Substring(0, 1);  JOBQ = JOBQ.Substring(0, 1);  

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
            // *  DGGSVP computes orthogonal matrices U, V and Q such that
            // *
            // *                   N-K-L  K    L
            // *   U'*A*Q =     K ( 0    A12  A13 )  if M-K-L >= 0;
            // *                L ( 0     0   A23 )
            // *            M-K-L ( 0     0    0  )
            // *
            // *                   N-K-L  K    L
            // *          =     K ( 0    A12  A13 )  if M-K-L < 0;
            // *              M-K ( 0     0   A23 )
            // *
            // *                 N-K-L  K    L
            // *   V'*B*Q =   L ( 0     0   B13 )
            // *            P-L ( 0     0    0  )
            // *
            // *  where the K-by-K matrix A12 and L-by-L matrix B13 are nonsingular
            // *  upper triangular; A23 is L-by-L upper triangular if M-K-L >= 0,
            // *  otherwise A23 is (M-K)-by-L upper trapezoidal.  K+L = the effective
            // *  numerical rank of the (M+P)-by-N matrix (A',B')'.  Z' denotes the
            // *  transpose of Z.
            // *
            // *  This decomposition is the preprocessing step for computing the
            // *  Generalized Singular Value Decomposition (GSVD), see subroutine
            // *  DGGSVD.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  JOBU    (input) CHARACTER*1
            // *          = 'U':  Orthogonal matrix U is computed;
            // *          = 'N':  U is not computed.
            // *
            // *  JOBV    (input) CHARACTER*1
            // *          = 'V':  Orthogonal matrix V is computed;
            // *          = 'N':  V is not computed.
            // *
            // *  JOBQ    (input) CHARACTER*1
            // *          = 'Q':  Orthogonal matrix Q is computed;
            // *          = 'N':  Q is not computed.
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix A.  M >= 0.
            // *
            // *  P       (input) INTEGER
            // *          The number of rows of the matrix B.  P >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrices A and B.  N >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the M-by-N matrix A.
            // *          On exit, A contains the triangular (or trapezoidal) matrix
            // *          described in the Purpose section.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A. LDA >= max(1,M).
            // *
            // *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,N)
            // *          On entry, the P-by-N matrix B.
            // *          On exit, B contains the triangular matrix described in
            // *          the Purpose section.
            // *
            // *  LDB     (input) INTEGER
            // *          The leading dimension of the array B. LDB >= max(1,P).
            // *
            // *  TOLA    (input) DOUBLE PRECISION
            // *  TOLB    (input) DOUBLE PRECISION
            // *          TOLA and TOLB are the thresholds to determine the effective
            // *          numerical rank of matrix B and a subblock of A. Generally,
            // *          they are set to
            // *             TOLA = MAX(M,N)*norm(A)*MAZHEPS,
            // *             TOLB = MAX(P,N)*norm(B)*MAZHEPS.
            // *          The size of TOLA and TOLB may affect the size of backward
            // *          errors of the decomposition.
            // *
            // *  K       (output) INTEGER
            // *  L       (output) INTEGER
            // *          On exit, K and L specify the dimension of the subblocks
            // *          described in Purpose.
            // *          K + L = effective numerical rank of (A',B')'.
            // *
            // *  U       (output) DOUBLE PRECISION array, dimension (LDU,M)
            // *          If JOBU = 'U', U contains the orthogonal matrix U.
            // *          If JOBU = 'N', U is not referenced.
            // *
            // *  LDU     (input) INTEGER
            // *          The leading dimension of the array U. LDU >= max(1,M) if
            // *          JOBU = 'U'; LDU >= 1 otherwise.
            // *
            // *  V       (output) DOUBLE PRECISION array, dimension (LDV,M)
            // *          If JOBV = 'V', V contains the orthogonal matrix V.
            // *          If JOBV = 'N', V is not referenced.
            // *
            // *  LDV     (input) INTEGER
            // *          The leading dimension of the array V. LDV >= max(1,P) if
            // *          JOBV = 'V'; LDV >= 1 otherwise.
            // *
            // *  Q       (output) DOUBLE PRECISION array, dimension (LDQ,N)
            // *          If JOBQ = 'Q', Q contains the orthogonal matrix Q.
            // *          If JOBQ = 'N', Q is not referenced.
            // *
            // *  LDQ     (input) INTEGER
            // *          The leading dimension of the array Q. LDQ >= max(1,N) if
            // *          JOBQ = 'Q'; LDQ >= 1 otherwise.
            // *
            // *  IWORK   (workspace) INTEGER array, dimension (N)
            // *
            // *  TAU     (workspace) DOUBLE PRECISION array, dimension (N)
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (max(3*N,M,P))
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  The subroutine uses LAPACK subroutine DGEQPF for the QR factorization
            // *  with column pivoting to detect the effective numerical rank of the
            // *  a matrix. It may be replaced by a better rank determination strategy.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, MAX, MIN;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters
            // *

            #endregion


            #region Body
            
            WANTU = this._lsame.Run(JOBU, "U");
            WANTV = this._lsame.Run(JOBV, "V");
            WANTQ = this._lsame.Run(JOBQ, "Q");
            FORWRD = true;
            // *
            INFO = 0;
            if (!(WANTU || this._lsame.Run(JOBU, "N")))
            {
                INFO =  - 1;
            }
            else
            {
                if (!(WANTV || this._lsame.Run(JOBV, "N")))
                {
                    INFO =  - 2;
                }
                else
                {
                    if (!(WANTQ || this._lsame.Run(JOBQ, "N")))
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (M < 0)
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (P < 0)
                            {
                                INFO =  - 5;
                            }
                            else
                            {
                                if (N < 0)
                                {
                                    INFO =  - 6;
                                }
                                else
                                {
                                    if (LDA < Math.Max(1, M))
                                    {
                                        INFO =  - 8;
                                    }
                                    else
                                    {
                                        if (LDB < Math.Max(1, P))
                                        {
                                            INFO =  - 10;
                                        }
                                        else
                                        {
                                            if (LDU < 1 || (WANTU && LDU < M))
                                            {
                                                INFO =  - 16;
                                            }
                                            else
                                            {
                                                if (LDV < 1 || (WANTV && LDV < P))
                                                {
                                                    INFO =  - 18;
                                                }
                                                else
                                                {
                                                    if (LDQ < 1 || (WANTQ && LDQ < N))
                                                    {
                                                        INFO =  - 20;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGGSVP",  - INFO);
                return;
            }
            // *
            // *     QR with column pivoting of B: B*P = V*( S11 S12 )
            // *                                           (  0   0  )
            // *
            for (I = 1; I <= N; I++)
            {
                IWORK[I + o_iwork] = 0;
            }
            this._dgeqpf.Run(P, N, ref B, offset_b, LDB, ref IWORK, offset_iwork, ref TAU, offset_tau
                             , ref WORK, offset_work, ref INFO);
            // *
            // *     Update A := A*P
            // *
            this._dlapmt.Run(FORWRD, M, N, ref A, offset_a, LDA, ref IWORK, offset_iwork);
            // *
            // *     Determine the effective rank of matrix B.
            // *
            L = 0;
            for (I = 1; I <= Math.Min(P, N); I++)
            {
                if (Math.Abs(B[I+I * LDB + o_b]) > TOLB) L += 1;
            }
            // *
            if (WANTV)
            {
                // *
                // *        Copy the details of V, and form V.
                // *
                this._dlaset.Run("Full", P, P, ZERO, ZERO, ref V, offset_v
                                 , LDV);
                if (P > 1)
                {
                    this._dlacpy.Run("Lower", P - 1, N, B, 2+1 * LDB + o_b, LDB, ref V, 2+1 * LDV + o_v
                                     , LDV);
                }
                this._dorg2r.Run(P, P, Math.Min(P, N), ref V, offset_v, LDV, TAU, offset_tau
                                 , ref WORK, offset_work, ref INFO);
            }
            // *
            // *     Clean up B
            // *
            for (J = 1; J <= L - 1; J++)
            {
                B_J = J * LDB + o_b;
                for (I = J + 1; I <= L; I++)
                {
                    B[I + B_J] = ZERO;
                }
            }
            if (P > L)
            {
                this._dlaset.Run("Full", P - L, N, ZERO, ZERO, ref B, L + 1+1 * LDB + o_b
                                 , LDB);
            }
            // *
            if (WANTQ)
            {
                // *
                // *        Set Q = I and Update Q := Q*P
                // *
                this._dlaset.Run("Full", N, N, ZERO, ONE, ref Q, offset_q
                                 , LDQ);
                this._dlapmt.Run(FORWRD, N, N, ref Q, offset_q, LDQ, ref IWORK, offset_iwork);
            }
            // *
            if (P >= L && N != L)
            {
                // *
                // *        RQ factorization of (S11 S12): ( S11 S12 ) = ( 0 S12 )*Z
                // *
                this._dgerq2.Run(L, N, ref B, offset_b, LDB, ref TAU, offset_tau, ref WORK, offset_work
                                 , ref INFO);
                // *
                // *        Update A := A*Z'
                // *
                this._dormr2.Run("Right", "Transpose", M, N, L, ref B, offset_b
                                 , LDB, TAU, offset_tau, ref A, offset_a, LDA, ref WORK, offset_work, ref INFO);
                // *
                if (WANTQ)
                {
                    // *
                    // *           Update Q := Q*Z'
                    // *
                    this._dormr2.Run("Right", "Transpose", N, N, L, ref B, offset_b
                                     , LDB, TAU, offset_tau, ref Q, offset_q, LDQ, ref WORK, offset_work, ref INFO);
                }
                // *
                // *        Clean up B
                // *
                this._dlaset.Run("Full", L, N - L, ZERO, ZERO, ref B, offset_b
                                 , LDB);
                for (J = N - L + 1; J <= N; J++)
                {
                    B_J = J * LDB + o_b;
                    for (I = J - N + L + 1; I <= L; I++)
                    {
                        B[I + B_J] = ZERO;
                    }
                }
                // *
            }
            // *
            // *     Let              N-L     L
            // *                A = ( A11    A12 ) M,
            // *
            // *     then the following does the complete QR decomposition of A11:
            // *
            // *              A11 = U*(  0  T12 )*P1'
            // *                      (  0   0  )
            // *
            for (I = 1; I <= N - L; I++)
            {
                IWORK[I + o_iwork] = 0;
            }
            this._dgeqpf.Run(M, N - L, ref A, offset_a, LDA, ref IWORK, offset_iwork, ref TAU, offset_tau
                             , ref WORK, offset_work, ref INFO);
            // *
            // *     Determine the effective rank of A11
            // *
            K = 0;
            for (I = 1; I <= Math.Min(M, N - L); I++)
            {
                if (Math.Abs(A[I+I * LDA + o_a]) > TOLA) K += 1;
            }
            // *
            // *     Update A12 := U'*A12, where A12 = A( 1:M, N-L+1:N )
            // *
            this._dorm2r.Run("Left", "Transpose", M, L, Math.Min(M, N - L), ref A, offset_a
                             , LDA, TAU, offset_tau, ref A, 1+(N - L + 1) * LDA + o_a, LDA, ref WORK, offset_work, ref INFO);
            // *
            if (WANTU)
            {
                // *
                // *        Copy the details of U, and form U
                // *
                this._dlaset.Run("Full", M, M, ZERO, ZERO, ref U, offset_u
                                 , LDU);
                if (M > 1)
                {
                    this._dlacpy.Run("Lower", M - 1, N - L, A, 2+1 * LDA + o_a, LDA, ref U, 2+1 * LDU + o_u
                                     , LDU);
                }
                this._dorg2r.Run(M, M, Math.Min(M, N - L), ref U, offset_u, LDU, TAU, offset_tau
                                 , ref WORK, offset_work, ref INFO);
            }
            // *
            if (WANTQ)
            {
                // *
                // *        Update Q( 1:N, 1:N-L )  = Q( 1:N, 1:N-L )*P1
                // *
                this._dlapmt.Run(FORWRD, N, N - L, ref Q, offset_q, LDQ, ref IWORK, offset_iwork);
            }
            // *
            // *     Clean up A: set the strictly lower triangular part of
            // *     A(1:K, 1:K) = 0, and A( K+1:M, 1:N-L ) = 0.
            // *
            for (J = 1; J <= K - 1; J++)
            {
                A_J = J * LDA + o_a;
                for (I = J + 1; I <= K; I++)
                {
                    A[I + A_J] = ZERO;
                }
            }
            if (M > K)
            {
                this._dlaset.Run("Full", M - K, N - L, ZERO, ZERO, ref A, K + 1+1 * LDA + o_a
                                 , LDA);
            }
            // *
            if (N - L > K)
            {
                // *
                // *        RQ factorization of ( T11 T12 ) = ( 0 T12 )*Z1
                // *
                this._dgerq2.Run(K, N - L, ref A, offset_a, LDA, ref TAU, offset_tau, ref WORK, offset_work
                                 , ref INFO);
                // *
                if (WANTQ)
                {
                    // *
                    // *           Update Q( 1:N,1:N-L ) = Q( 1:N,1:N-L )*Z1'
                    // *
                    this._dormr2.Run("Right", "Transpose", N, N - L, K, ref A, offset_a
                                     , LDA, TAU, offset_tau, ref Q, offset_q, LDQ, ref WORK, offset_work, ref INFO);
                }
                // *
                // *        Clean up A
                // *
                this._dlaset.Run("Full", K, N - L - K, ZERO, ZERO, ref A, offset_a
                                 , LDA);
                for (J = N - L - K + 1; J <= N - L; J++)
                {
                    A_J = J * LDA + o_a;
                    for (I = J - N + L + K + 1; I <= K; I++)
                    {
                        A[I + A_J] = ZERO;
                    }
                }
                // *
            }
            // *
            if (M > K)
            {
                // *
                // *        QR factorization of A( K+1:M,N-L+1:N )
                // *
                this._dgeqr2.Run(M - K, L, ref A, K + 1+(N - L + 1) * LDA + o_a, LDA, ref TAU, offset_tau, ref WORK, offset_work
                                 , ref INFO);
                // *
                if (WANTU)
                {
                    // *
                    // *           Update U(:,K+1:M) := U(:,K+1:M)*U1
                    // *
                    this._dorm2r.Run("Right", "No transpose", M, M - K, Math.Min(M - K, L), ref A, K + 1+(N - L + 1) * LDA + o_a
                                     , LDA, TAU, offset_tau, ref U, 1+(K + 1) * LDU + o_u, LDU, ref WORK, offset_work, ref INFO);
                }
                // *
                // *        Clean up
                // *
                for (J = N - L + 1; J <= N; J++)
                {
                    A_J = J * LDA + o_a;
                    for (I = J - N + K + L + 1; I <= M; I++)
                    {
                        A[I + A_J] = ZERO;
                    }
                }
                // *
            }
            // *
            return;
            // *
            // *     End of DGGSVP
            // *

            #endregion

        }
    }
}
