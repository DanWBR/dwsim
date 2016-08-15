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
    /// DGGSVD computes the generalized singular value decomposition (GSVD)
    /// of an M-by-N real matrix A and P-by-N real matrix B:
    /// 
    /// U'*A*Q = D1*( 0 R ),    V'*B*Q = D2*( 0 R )
    /// 
    /// where U, V and Q are orthogonal matrices, and Z' is the transpose
    /// of Z.  Let K+L = the effective numerical rank of the matrix (A',B')',
    /// then R is a K+L-by-K+L nonsingular upper triangular matrix, D1 and
    /// D2 are M-by-(K+L) and P-by-(K+L) "diagonal" matrices and of the
    /// following structures, respectively:
    /// 
    /// If M-K-L .GE. 0,
    /// 
    /// K  L
    /// D1 =     K ( I  0 )
    /// L ( 0  C )
    /// M-K-L ( 0  0 )
    /// 
    /// K  L
    /// D2 =   L ( 0  S )
    /// P-L ( 0  0 )
    /// 
    /// N-K-L  K    L
    /// ( 0 R ) = K (  0   R11  R12 )
    /// L (  0    0   R22 )
    /// 
    /// where
    /// 
    /// C = diag( ALPHA(K+1), ... , ALPHA(K+L) ),
    /// S = diag( BETA(K+1),  ... , BETA(K+L) ),
    /// C**2 + S**2 = I.
    /// 
    /// R is stored in A(1:K+L,N-K-L+1:N) on exit.
    /// 
    /// If M-K-L .LT. 0,
    /// 
    /// K M-K K+L-M
    /// D1 =   K ( I  0    0   )
    /// M-K ( 0  C    0   )
    /// 
    /// K M-K K+L-M
    /// D2 =   M-K ( 0  S    0  )
    /// K+L-M ( 0  0    I  )
    /// P-L ( 0  0    0  )
    /// 
    /// N-K-L  K   M-K  K+L-M
    /// ( 0 R ) =     K ( 0    R11  R12  R13  )
    /// M-K ( 0     0   R22  R23  )
    /// K+L-M ( 0     0    0   R33  )
    /// 
    /// where
    /// 
    /// C = diag( ALPHA(K+1), ... , ALPHA(M) ),
    /// S = diag( BETA(K+1),  ... , BETA(M) ),
    /// C**2 + S**2 = I.
    /// 
    /// (R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N), and R33 is stored
    /// ( 0  R22 R23 )
    /// in B(M-K+1:L,N+M-K-L+1:N) on exit.
    /// 
    /// The routine computes C, S, R, and optionally the orthogonal
    /// transformation matrices U, V and Q.
    /// 
    /// In particular, if B is an N-by-N nonsingular matrix, then the GSVD of
    /// A and B implicitly gives the SVD of A*inv(B):
    /// A*inv(B) = U*(D1*inv(D2))*V'.
    /// If ( A',B')' has orthonormal columns, then the GSVD of A and B is
    /// also equal to the CS decomposition of A and B. Furthermore, the GSVD
    /// can be used to derive the solution of the eigenvalue problem:
    /// A'*A x = lambda* B'*B x.
    /// In some literature, the GSVD of A and B is presented in the form
    /// U'*A*X = ( 0 D1 ),   V'*B*X = ( 0 D2 )
    /// where U and V are orthogonal and X is nonsingular, D1 and D2 are
    /// ``diagonal''.  The former GSVD form can be converted to the latter
    /// form by taking the nonsingular matrix X as
    /// 
    /// X = Q*( I   0    )
    /// ( 0 inv(R) ).
    /// 
    ///</summary>
    public class DGGSVD
    {
    

        #region Dependencies
        
        LSAME _lsame; DLAMCH _dlamch; DLANGE _dlange; DCOPY _dcopy; DGGSVP _dggsvp; DTGSJA _dtgsja; XERBLA _xerbla; 

        #endregion

        public DGGSVD(LSAME lsame, DLAMCH dlamch, DLANGE dlange, DCOPY dcopy, DGGSVP dggsvp, DTGSJA dtgsja, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dlamch = dlamch; this._dlange = dlange; this._dcopy = dcopy; this._dggsvp = dggsvp; 
            this._dtgsja = dtgsja;this._xerbla = xerbla; 

            #endregion

        }
    
        public DGGSVD()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLASSQ dlassq = new DLASSQ();
            DCOPY dcopy = new DCOPY();
            XERBLA xerbla = new XERBLA();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DSWAP dswap = new DSWAP();
            IDAMAX idamax = new IDAMAX();
            DLAPMT dlapmt = new DLAPMT();
            DDOT ddot = new DDOT();
            DAXPY daxpy = new DAXPY();
            DLAS2 dlas2 = new DLAS2();
            DROT drot = new DROT();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLANGE dlange = new DLANGE(dlassq, lsame);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DGEQR2 dgeqr2 = new DGEQR2(dlarf, dlarfg, xerbla);
            DORM2R dorm2r = new DORM2R(lsame, dlarf, xerbla);
            DGEQPF dgeqpf = new DGEQPF(dgeqr2, dlarf, dlarfg, dorm2r, dswap, xerbla, idamax, dlamch, dnrm2);
            DGERQ2 dgerq2 = new DGERQ2(dlarf, dlarfg, xerbla);
            DLACPY dlacpy = new DLACPY(lsame);
            DLASET dlaset = new DLASET(lsame);
            DORG2R dorg2r = new DORG2R(dlarf, dscal, xerbla);
            DORMR2 dormr2 = new DORMR2(lsame, dlarf, xerbla);
            DGGSVP dggsvp = new DGGSVP(lsame, dgeqpf, dgeqr2, dgerq2, dlacpy, dlapmt, dlaset, dorg2r, dorm2r, dormr2
                                       , xerbla);
            DLARTG dlartg = new DLARTG(dlamch);
            DLASV2 dlasv2 = new DLASV2(dlamch);
            DLAGS2 dlags2 = new DLAGS2(dlartg, dlasv2);
            DLAPLL dlapll = new DLAPLL(ddot, daxpy, dlarfg, dlas2);
            DTGSJA dtgsja = new DTGSJA(lsame, dcopy, dlags2, dlapll, dlartg, dlaset, drot, dscal, xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dlamch = dlamch; this._dlange = dlange; this._dcopy = dcopy; this._dggsvp = dggsvp; 
            this._dtgsja = dtgsja;this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGGSVD computes the generalized singular value decomposition (GSVD)
        /// of an M-by-N real matrix A and P-by-N real matrix B:
        /// 
        /// U'*A*Q = D1*( 0 R ),    V'*B*Q = D2*( 0 R )
        /// 
        /// where U, V and Q are orthogonal matrices, and Z' is the transpose
        /// of Z.  Let K+L = the effective numerical rank of the matrix (A',B')',
        /// then R is a K+L-by-K+L nonsingular upper triangular matrix, D1 and
        /// D2 are M-by-(K+L) and P-by-(K+L) "diagonal" matrices and of the
        /// following structures, respectively:
        /// 
        /// If M-K-L .GE. 0,
        /// 
        /// K  L
        /// D1 =     K ( I  0 )
        /// L ( 0  C )
        /// M-K-L ( 0  0 )
        /// 
        /// K  L
        /// D2 =   L ( 0  S )
        /// P-L ( 0  0 )
        /// 
        /// N-K-L  K    L
        /// ( 0 R ) = K (  0   R11  R12 )
        /// L (  0    0   R22 )
        /// 
        /// where
        /// 
        /// C = diag( ALPHA(K+1), ... , ALPHA(K+L) ),
        /// S = diag( BETA(K+1),  ... , BETA(K+L) ),
        /// C**2 + S**2 = I.
        /// 
        /// R is stored in A(1:K+L,N-K-L+1:N) on exit.
        /// 
        /// If M-K-L .LT. 0,
        /// 
        /// K M-K K+L-M
        /// D1 =   K ( I  0    0   )
        /// M-K ( 0  C    0   )
        /// 
        /// K M-K K+L-M
        /// D2 =   M-K ( 0  S    0  )
        /// K+L-M ( 0  0    I  )
        /// P-L ( 0  0    0  )
        /// 
        /// N-K-L  K   M-K  K+L-M
        /// ( 0 R ) =     K ( 0    R11  R12  R13  )
        /// M-K ( 0     0   R22  R23  )
        /// K+L-M ( 0     0    0   R33  )
        /// 
        /// where
        /// 
        /// C = diag( ALPHA(K+1), ... , ALPHA(M) ),
        /// S = diag( BETA(K+1),  ... , BETA(M) ),
        /// C**2 + S**2 = I.
        /// 
        /// (R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N), and R33 is stored
        /// ( 0  R22 R23 )
        /// in B(M-K+1:L,N+M-K-L+1:N) on exit.
        /// 
        /// The routine computes C, S, R, and optionally the orthogonal
        /// transformation matrices U, V and Q.
        /// 
        /// In particular, if B is an N-by-N nonsingular matrix, then the GSVD of
        /// A and B implicitly gives the SVD of A*inv(B):
        /// A*inv(B) = U*(D1*inv(D2))*V'.
        /// If ( A',B')' has orthonormal columns, then the GSVD of A and B is
        /// also equal to the CS decomposition of A and B. Furthermore, the GSVD
        /// can be used to derive the solution of the eigenvalue problem:
        /// A'*A x = lambda* B'*B x.
        /// In some literature, the GSVD of A and B is presented in the form
        /// U'*A*X = ( 0 D1 ),   V'*B*X = ( 0 D2 )
        /// where U and V are orthogonal and X is nonsingular, D1 and D2 are
        /// ``diagonal''.  The former GSVD form can be converted to the latter
        /// form by taking the nonsingular matrix X as
        /// 
        /// X = Q*( I   0    )
        /// ( 0 inv(R) ).
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
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrices A and B.  N .GE. 0.
        ///</param>
        /// <param name="P">
        /// (input) INTEGER
        /// The number of rows of the matrix B.  P .GE. 0.
        ///</param>
        /// <param name="K">
        /// L
        ///</param>
        /// <param name="L">
        /// ( 0  C )
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the M-by-N matrix A.
        /// On exit, A contains the triangular matrix R, or part of R.
        /// See Purpose for details.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A. LDA .GE. max(1,M).
        ///</param>
        /// <param name="B">
        /// (input/output) DOUBLE PRECISION array, dimension (LDB,N)
        /// On entry, the P-by-N matrix B.
        /// On exit, B contains the triangular matrix R if M-K-L .LT. 0.
        /// See Purpose for details.
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of the array B. LDB .GE. max(1,P).
        ///</param>
        /// <param name="ALPHA">
        /// (output) DOUBLE PRECISION array, dimension (N)
        ///</param>
        /// <param name="BETA">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// On exit, ALPHA and BETA contain the generalized singular
        /// value pairs of A and B;
        /// ALPHA(1:K) = 1,
        /// BETA(1:K)  = 0,
        /// and if M-K-L .GE. 0,
        /// ALPHA(K+1:K+L) = C,
        /// BETA(K+1:K+L)  = S,
        /// or if M-K-L .LT. 0,
        /// ALPHA(K+1:M)=C, ALPHA(M+1:K+L)=0
        /// BETA(K+1:M) =S, BETA(M+1:K+L) =1
        /// and
        /// ALPHA(K+L+1:N) = 0
        /// BETA(K+L+1:N)  = 0
        ///</param>
        /// <param name="U">
        /// (output) DOUBLE PRECISION array, dimension (LDU,M)
        /// If JOBU = 'U', U contains the M-by-M orthogonal matrix U.
        /// If JOBU = 'N', U is not referenced.
        ///</param>
        /// <param name="LDU">
        /// (input) INTEGER
        /// The leading dimension of the array U. LDU .GE. max(1,M) if
        /// JOBU = 'U'; LDU .GE. 1 otherwise.
        ///</param>
        /// <param name="V">
        /// (output) DOUBLE PRECISION array, dimension (LDV,P)
        /// If JOBV = 'V', V contains the P-by-P orthogonal matrix V.
        /// If JOBV = 'N', V is not referenced.
        ///</param>
        /// <param name="LDV">
        /// (input) INTEGER
        /// The leading dimension of the array V. LDV .GE. max(1,P) if
        /// JOBV = 'V'; LDV .GE. 1 otherwise.
        ///</param>
        /// <param name="Q">
        /// (output) DOUBLE PRECISION array, dimension (LDQ,N)
        /// If JOBQ = 'Q', Q contains the N-by-N orthogonal matrix Q.
        /// If JOBQ = 'N', Q is not referenced.
        ///</param>
        /// <param name="LDQ">
        /// (input) INTEGER
        /// The leading dimension of the array Q. LDQ .GE. max(1,N) if
        /// JOBQ = 'Q'; LDQ .GE. 1 otherwise.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array,
        /// dimension (max(3*N,M,P)+N)
        ///</param>
        /// <param name="IWORK">
        /// (workspace/output) INTEGER array, dimension (N)
        /// On exit, IWORK stores the sorting information. More
        /// precisely, the following loop will sort ALPHA
        /// for I = K+1, min(M,K+L)
        /// swap ALPHA(I) and ALPHA(IWORK(I))
        /// endfor
        /// such that ALPHA(1) .GE. ALPHA(2) .GE. ... .GE. ALPHA(N).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// .GT. 0:  if INFO = 1, the Jacobi-type procedure failed to
        /// converge.  For further details, see subroutine DTGSJA.
        ///</param>
        public void Run(string JOBU, string JOBV, string JOBQ, int M, int N, int P
                         , ref int K, ref int L, ref double[] A, int offset_a, int LDA, ref double[] B, int offset_b, int LDB
                         , ref double[] ALPHA, int offset_alpha, ref double[] BETA, int offset_beta, ref double[] U, int offset_u, int LDU, ref double[] V, int offset_v, int LDV
                         , ref double[] Q, int offset_q, int LDQ, ref double[] WORK, int offset_work, ref int[] IWORK, int offset_iwork, ref int INFO)
        {

            #region Variables
            
            bool WANTQ = false; bool WANTU = false; bool WANTV = false; int I = 0; int IBND = 0; int ISUB = 0; int J = 0; 
            int NCYCLE = 0;double ANORM = 0; double BNORM = 0; double SMAX = 0; double TEMP = 0; double TOLA = 0; double TOLB = 0; 
            double ULP = 0;double UNFL = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_b = -1 - LDB + offset_b;  int o_alpha = -1 + offset_alpha; 
             int o_beta = -1 + offset_beta; int o_u = -1 - LDU + offset_u;  int o_v = -1 - LDV + offset_v; 
             int o_q = -1 - LDQ + offset_q; int o_work = -1 + offset_work;  int o_iwork = -1 + offset_iwork; 

            #endregion


            #region Strings
            
            JOBU = JOBU.Substring(0, 1);  JOBV = JOBV.Substring(0, 1);  JOBQ = JOBQ.Substring(0, 1);  

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
            // *  DGGSVD computes the generalized singular value decomposition (GSVD)
            // *  of an M-by-N real matrix A and P-by-N real matrix B:
            // *
            // *      U'*A*Q = D1*( 0 R ),    V'*B*Q = D2*( 0 R )
            // *
            // *  where U, V and Q are orthogonal matrices, and Z' is the transpose
            // *  of Z.  Let K+L = the effective numerical rank of the matrix (A',B')',
            // *  then R is a K+L-by-K+L nonsingular upper triangular matrix, D1 and
            // *  D2 are M-by-(K+L) and P-by-(K+L) "diagonal" matrices and of the
            // *  following structures, respectively:
            // *
            // *  If M-K-L >= 0,
            // *
            // *                      K  L
            // *         D1 =     K ( I  0 )
            // *                  L ( 0  C )
            // *              M-K-L ( 0  0 )
            // *
            // *                    K  L
            // *         D2 =   L ( 0  S )
            // *              P-L ( 0  0 )
            // *
            // *                  N-K-L  K    L
            // *    ( 0 R ) = K (  0   R11  R12 )
            // *              L (  0    0   R22 )
            // *
            // *  where
            // *
            // *    C = diag( ALPHA(K+1), ... , ALPHA(K+L) ),
            // *    S = diag( BETA(K+1),  ... , BETA(K+L) ),
            // *    C**2 + S**2 = I.
            // *
            // *    R is stored in A(1:K+L,N-K-L+1:N) on exit.
            // *
            // *  If M-K-L < 0,
            // *
            // *                    K M-K K+L-M
            // *         D1 =   K ( I  0    0   )
            // *              M-K ( 0  C    0   )
            // *
            // *                      K M-K K+L-M
            // *         D2 =   M-K ( 0  S    0  )
            // *              K+L-M ( 0  0    I  )
            // *                P-L ( 0  0    0  )
            // *
            // *                     N-K-L  K   M-K  K+L-M
            // *    ( 0 R ) =     K ( 0    R11  R12  R13  )
            // *                M-K ( 0     0   R22  R23  )
            // *              K+L-M ( 0     0    0   R33  )
            // *
            // *  where
            // *
            // *    C = diag( ALPHA(K+1), ... , ALPHA(M) ),
            // *    S = diag( BETA(K+1),  ... , BETA(M) ),
            // *    C**2 + S**2 = I.
            // *
            // *    (R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N), and R33 is stored
            // *    ( 0  R22 R23 )
            // *    in B(M-K+1:L,N+M-K-L+1:N) on exit.
            // *
            // *  The routine computes C, S, R, and optionally the orthogonal
            // *  transformation matrices U, V and Q.
            // *
            // *  In particular, if B is an N-by-N nonsingular matrix, then the GSVD of
            // *  A and B implicitly gives the SVD of A*inv(B):
            // *                       A*inv(B) = U*(D1*inv(D2))*V'.
            // *  If ( A',B')' has orthonormal columns, then the GSVD of A and B is
            // *  also equal to the CS decomposition of A and B. Furthermore, the GSVD
            // *  can be used to derive the solution of the eigenvalue problem:
            // *                       A'*A x = lambda* B'*B x.
            // *  In some literature, the GSVD of A and B is presented in the form
            // *                   U'*A*X = ( 0 D1 ),   V'*B*X = ( 0 D2 )
            // *  where U and V are orthogonal and X is nonsingular, D1 and D2 are
            // *  ``diagonal''.  The former GSVD form can be converted to the latter
            // *  form by taking the nonsingular matrix X as
            // *
            // *                       X = Q*( I   0    )
            // *                             ( 0 inv(R) ).
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
            // *  N       (input) INTEGER
            // *          The number of columns of the matrices A and B.  N >= 0.
            // *
            // *  P       (input) INTEGER
            // *          The number of rows of the matrix B.  P >= 0.
            // *
            // *  K       (output) INTEGER
            // *  L       (output) INTEGER
            // *          On exit, K and L specify the dimension of the subblocks
            // *          described in the Purpose section.
            // *          K + L = effective numerical rank of (A',B')'.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the M-by-N matrix A.
            // *          On exit, A contains the triangular matrix R, or part of R.
            // *          See Purpose for details.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A. LDA >= max(1,M).
            // *
            // *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,N)
            // *          On entry, the P-by-N matrix B.
            // *          On exit, B contains the triangular matrix R if M-K-L < 0.
            // *          See Purpose for details.
            // *
            // *  LDB     (input) INTEGER
            // *          The leading dimension of the array B. LDB >= max(1,P).
            // *
            // *  ALPHA   (output) DOUBLE PRECISION array, dimension (N)
            // *  BETA    (output) DOUBLE PRECISION array, dimension (N)
            // *          On exit, ALPHA and BETA contain the generalized singular
            // *          value pairs of A and B;
            // *            ALPHA(1:K) = 1,
            // *            BETA(1:K)  = 0,
            // *          and if M-K-L >= 0,
            // *            ALPHA(K+1:K+L) = C,
            // *            BETA(K+1:K+L)  = S,
            // *          or if M-K-L < 0,
            // *            ALPHA(K+1:M)=C, ALPHA(M+1:K+L)=0
            // *            BETA(K+1:M) =S, BETA(M+1:K+L) =1
            // *          and
            // *            ALPHA(K+L+1:N) = 0
            // *            BETA(K+L+1:N)  = 0
            // *
            // *  U       (output) DOUBLE PRECISION array, dimension (LDU,M)
            // *          If JOBU = 'U', U contains the M-by-M orthogonal matrix U.
            // *          If JOBU = 'N', U is not referenced.
            // *
            // *  LDU     (input) INTEGER
            // *          The leading dimension of the array U. LDU >= max(1,M) if
            // *          JOBU = 'U'; LDU >= 1 otherwise.
            // *
            // *  V       (output) DOUBLE PRECISION array, dimension (LDV,P)
            // *          If JOBV = 'V', V contains the P-by-P orthogonal matrix V.
            // *          If JOBV = 'N', V is not referenced.
            // *
            // *  LDV     (input) INTEGER
            // *          The leading dimension of the array V. LDV >= max(1,P) if
            // *          JOBV = 'V'; LDV >= 1 otherwise.
            // *
            // *  Q       (output) DOUBLE PRECISION array, dimension (LDQ,N)
            // *          If JOBQ = 'Q', Q contains the N-by-N orthogonal matrix Q.
            // *          If JOBQ = 'N', Q is not referenced.
            // *
            // *  LDQ     (input) INTEGER
            // *          The leading dimension of the array Q. LDQ >= max(1,N) if
            // *          JOBQ = 'Q'; LDQ >= 1 otherwise.
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array,
            // *                      dimension (max(3*N,M,P)+N)
            // *
            // *  IWORK   (workspace/output) INTEGER array, dimension (N)
            // *          On exit, IWORK stores the sorting information. More
            // *          precisely, the following loop will sort ALPHA
            // *             for I = K+1, min(M,K+L)
            // *                 swap ALPHA(I) and ALPHA(IWORK(I))
            // *             endfor
            // *          such that ALPHA(1) >= ALPHA(2) >= ... >= ALPHA(N).
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *          > 0:  if INFO = 1, the Jacobi-type procedure failed to
            // *                converge.  For further details, see subroutine DTGSJA.
            // *
            // *  Internal Parameters
            // *  ===================
            // *
            // *  TOLA    DOUBLE PRECISION
            // *  TOLB    DOUBLE PRECISION
            // *          TOLA and TOLB are the thresholds to determine the effective
            // *          rank of (A',B')'. Generally, they are set to
            // *                   TOLA = MAX(M,N)*norm(A)*MAZHEPS,
            // *                   TOLB = MAX(P,N)*norm(B)*MAZHEPS.
            // *          The size of TOLA and TOLB may affect the size of backward
            // *          errors of the decomposition.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  2-96 Based on modifications by
            // *     Ming Gu and Huan Ren, Computer Science Division, University of
            // *     California at Berkeley, USA
            // *
            // *  =====================================================================
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MAX, MIN;
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
                            if (N < 0)
                            {
                                INFO =  - 5;
                            }
                            else
                            {
                                if (P < 0)
                                {
                                    INFO =  - 6;
                                }
                                else
                                {
                                    if (LDA < Math.Max(1, M))
                                    {
                                        INFO =  - 10;
                                    }
                                    else
                                    {
                                        if (LDB < Math.Max(1, P))
                                        {
                                            INFO =  - 12;
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
                this._xerbla.Run("DGGSVD",  - INFO);
                return;
            }
            // *
            // *     Compute the Frobenius norm of matrices A and B
            // *
            ANORM = this._dlange.Run("1", M, N, A, offset_a, LDA, ref WORK, offset_work);
            BNORM = this._dlange.Run("1", P, N, B, offset_b, LDB, ref WORK, offset_work);
            // *
            // *     Get machine precision and set up threshold for determining
            // *     the effective numerical rank of the matrices A and B.
            // *
            ULP = this._dlamch.Run("Precision");
            UNFL = this._dlamch.Run("Safe Minimum");
            TOLA = Math.Max(M, N) * Math.Max(ANORM, UNFL) * ULP;
            TOLB = Math.Max(P, N) * Math.Max(BNORM, UNFL) * ULP;
            // *
            // *     Preprocessing
            // *
            this._dggsvp.Run(JOBU, JOBV, JOBQ, M, P, N
                             , ref A, offset_a, LDA, ref B, offset_b, LDB, TOLA, TOLB
                             , ref K, ref L, ref U, offset_u, LDU, ref V, offset_v, LDV
                             , ref Q, offset_q, LDQ, ref IWORK, offset_iwork, ref WORK, offset_work, ref WORK, N + 1 + o_work, ref INFO);
            // *
            // *     Compute the GSVD of two upper "triangular" matrices
            // *
            this._dtgsja.Run(JOBU, JOBV, JOBQ, M, P, N
                             , K, L, ref A, offset_a, LDA, ref B, offset_b, LDB
                             , TOLA, TOLB, ref ALPHA, offset_alpha, ref BETA, offset_beta, ref U, offset_u, LDU
                             , ref V, offset_v, LDV, ref Q, offset_q, LDQ, ref WORK, offset_work, ref NCYCLE
                             , ref INFO);
            // *
            // *     Sort the singular values and store the pivot indices in IWORK
            // *     Copy ALPHA to WORK, then sort ALPHA in WORK
            // *
            this._dcopy.Run(N, ALPHA, offset_alpha, 1, ref WORK, offset_work, 1);
            IBND = Math.Min(L, M - K);
            for (I = 1; I <= IBND; I++)
            {
                // *
                // *        Scan for largest ALPHA(K+I)
                // *
                ISUB = I;
                SMAX = WORK[K + I + o_work];
                for (J = I + 1; J <= IBND; J++)
                {
                    TEMP = WORK[K + J + o_work];
                    if (TEMP > SMAX)
                    {
                        ISUB = J;
                        SMAX = TEMP;
                    }
                }
                if (ISUB != I)
                {
                    WORK[K + ISUB + o_work] = WORK[K + I + o_work];
                    WORK[K + I + o_work] = SMAX;
                    IWORK[K + I + o_iwork] = K + ISUB;
                }
                else
                {
                    IWORK[K + I + o_iwork] = K + I;
                }
            }
            // *
            return;
            // *
            // *     End of DGGSVD
            // *

            #endregion

        }
    }
}
