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
    /// DTGSJA computes the generalized singular value decomposition (GSVD)
    /// of two real upper triangular (or trapezoidal) matrices A and B.
    /// 
    /// On entry, it is assumed that matrices A and B have the following
    /// forms, which may be obtained by the preprocessing subroutine DGGSVP
    /// from a general M-by-N matrix A and P-by-N matrix B:
    /// 
    /// N-K-L  K    L
    /// A =    K ( 0    A12  A13 ) if M-K-L .GE. 0;
    /// L ( 0     0   A23 )
    /// M-K-L ( 0     0    0  )
    /// 
    /// N-K-L  K    L
    /// A =  K ( 0    A12  A13 ) if M-K-L .LT. 0;
    /// M-K ( 0     0   A23 )
    /// 
    /// N-K-L  K    L
    /// B =  L ( 0     0   B13 )
    /// P-L ( 0     0    0  )
    /// 
    /// where the K-by-K matrix A12 and L-by-L matrix B13 are nonsingular
    /// upper triangular; A23 is L-by-L upper triangular if M-K-L .GE. 0,
    /// otherwise A23 is (M-K)-by-L upper trapezoidal.
    /// 
    /// On exit,
    /// 
    /// U'*A*Q = D1*( 0 R ),    V'*B*Q = D2*( 0 R ),
    /// 
    /// where U, V and Q are orthogonal matrices, Z' denotes the transpose
    /// of Z, R is a nonsingular upper triangular matrix, and D1 and D2 are
    /// ``diagonal'' matrices, which are of the following structures:
    /// 
    /// If M-K-L .GE. 0,
    /// 
    /// K  L
    /// D1 =     K ( I  0 )
    /// L ( 0  C )
    /// M-K-L ( 0  0 )
    /// 
    /// K  L
    /// D2 = L   ( 0  S )
    /// P-L ( 0  0 )
    /// 
    /// N-K-L  K    L
    /// ( 0 R ) = K (  0   R11  R12 ) K
    /// L (  0    0   R22 ) L
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
    /// D2 =   M-K ( 0  S    0   )
    /// K+L-M ( 0  0    I   )
    /// P-L ( 0  0    0   )
    /// 
    /// N-K-L  K   M-K  K+L-M
    /// ( 0 R ) =    K ( 0    R11  R12  R13  )
    /// M-K ( 0     0   R22  R23  )
    /// K+L-M ( 0     0    0   R33  )
    /// 
    /// where
    /// C = diag( ALPHA(K+1), ... , ALPHA(M) ),
    /// S = diag( BETA(K+1),  ... , BETA(M) ),
    /// C**2 + S**2 = I.
    /// 
    /// R = ( R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N) and R33 is stored
    /// (  0  R22 R23 )
    /// in B(M-K+1:L,N+M-K-L+1:N) on exit.
    /// 
    /// The computation of the orthogonal transformation matrices U, V or Q
    /// is optional.  These matrices may either be formed explicitly, or they
    /// may be postmultiplied into input matrices U1, V1, or Q1.
    /// 
    ///</summary>
    public class DTGSJA
    {
    

        #region Dependencies
        
        LSAME _lsame; DCOPY _dcopy; DLAGS2 _dlags2; DLAPLL _dlapll; DLARTG _dlartg; DLASET _dlaset; DROT _drot; DSCAL _dscal; 
        XERBLA _xerbla;

        #endregion


        #region Variables
        
        const int MAXIT = 40; const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 

        #endregion

        public DTGSJA(LSAME lsame, DCOPY dcopy, DLAGS2 dlags2, DLAPLL dlapll, DLARTG dlartg, DLASET dlaset, DROT drot, DSCAL dscal, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dcopy = dcopy; this._dlags2 = dlags2; this._dlapll = dlapll; this._dlartg = dlartg; 
            this._dlaset = dlaset;this._drot = drot; this._dscal = dscal; this._xerbla = xerbla; 

            #endregion

        }
    
        public DTGSJA()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DCOPY dcopy = new DCOPY();
            DLAMC3 dlamc3 = new DLAMC3();
            DDOT ddot = new DDOT();
            DAXPY daxpy = new DAXPY();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DLAS2 dlas2 = new DLAS2();
            DROT drot = new DROT();
            XERBLA xerbla = new XERBLA();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARTG dlartg = new DLARTG(dlamch);
            DLASV2 dlasv2 = new DLASV2(dlamch);
            DLAGS2 dlags2 = new DLAGS2(dlartg, dlasv2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DLAPLL dlapll = new DLAPLL(ddot, daxpy, dlarfg, dlas2);
            DLASET dlaset = new DLASET(lsame);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dcopy = dcopy; this._dlags2 = dlags2; this._dlapll = dlapll; this._dlartg = dlartg; 
            this._dlaset = dlaset;this._drot = drot; this._dscal = dscal; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DTGSJA computes the generalized singular value decomposition (GSVD)
        /// of two real upper triangular (or trapezoidal) matrices A and B.
        /// 
        /// On entry, it is assumed that matrices A and B have the following
        /// forms, which may be obtained by the preprocessing subroutine DGGSVP
        /// from a general M-by-N matrix A and P-by-N matrix B:
        /// 
        /// N-K-L  K    L
        /// A =    K ( 0    A12  A13 ) if M-K-L .GE. 0;
        /// L ( 0     0   A23 )
        /// M-K-L ( 0     0    0  )
        /// 
        /// N-K-L  K    L
        /// A =  K ( 0    A12  A13 ) if M-K-L .LT. 0;
        /// M-K ( 0     0   A23 )
        /// 
        /// N-K-L  K    L
        /// B =  L ( 0     0   B13 )
        /// P-L ( 0     0    0  )
        /// 
        /// where the K-by-K matrix A12 and L-by-L matrix B13 are nonsingular
        /// upper triangular; A23 is L-by-L upper triangular if M-K-L .GE. 0,
        /// otherwise A23 is (M-K)-by-L upper trapezoidal.
        /// 
        /// On exit,
        /// 
        /// U'*A*Q = D1*( 0 R ),    V'*B*Q = D2*( 0 R ),
        /// 
        /// where U, V and Q are orthogonal matrices, Z' denotes the transpose
        /// of Z, R is a nonsingular upper triangular matrix, and D1 and D2 are
        /// ``diagonal'' matrices, which are of the following structures:
        /// 
        /// If M-K-L .GE. 0,
        /// 
        /// K  L
        /// D1 =     K ( I  0 )
        /// L ( 0  C )
        /// M-K-L ( 0  0 )
        /// 
        /// K  L
        /// D2 = L   ( 0  S )
        /// P-L ( 0  0 )
        /// 
        /// N-K-L  K    L
        /// ( 0 R ) = K (  0   R11  R12 ) K
        /// L (  0    0   R22 ) L
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
        /// D2 =   M-K ( 0  S    0   )
        /// K+L-M ( 0  0    I   )
        /// P-L ( 0  0    0   )
        /// 
        /// N-K-L  K   M-K  K+L-M
        /// ( 0 R ) =    K ( 0    R11  R12  R13  )
        /// M-K ( 0     0   R22  R23  )
        /// K+L-M ( 0     0    0   R33  )
        /// 
        /// where
        /// C = diag( ALPHA(K+1), ... , ALPHA(M) ),
        /// S = diag( BETA(K+1),  ... , BETA(M) ),
        /// C**2 + S**2 = I.
        /// 
        /// R = ( R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N) and R33 is stored
        /// (  0  R22 R23 )
        /// in B(M-K+1:L,N+M-K-L+1:N) on exit.
        /// 
        /// The computation of the orthogonal transformation matrices U, V or Q
        /// is optional.  These matrices may either be formed explicitly, or they
        /// may be postmultiplied into input matrices U1, V1, or Q1.
        /// 
        ///</summary>
        /// <param name="JOBU">
        /// (input) CHARACTER*1
        /// = 'U':  U must contain an orthogonal matrix U1 on entry, and
        /// the product U1*U is returned;
        /// = 'I':  U is initialized to the unit matrix, and the
        /// orthogonal matrix U is returned;
        /// = 'N':  U is not computed.
        ///</param>
        /// <param name="JOBV">
        /// (input) CHARACTER*1
        /// = 'V':  V must contain an orthogonal matrix V1 on entry, and
        /// the product V1*V is returned;
        /// = 'I':  V is initialized to the unit matrix, and the
        /// orthogonal matrix V is returned;
        /// = 'N':  V is not computed.
        ///</param>
        /// <param name="JOBQ">
        /// (input) CHARACTER*1
        /// = 'Q':  Q must contain an orthogonal matrix Q1 on entry, and
        /// the product Q1*Q is returned;
        /// = 'I':  Q is initialized to the unit matrix, and the
        /// orthogonal matrix Q is returned;
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
        /// <param name="K">
        /// L
        ///</param>
        /// <param name="L">
        /// ( 0     0   A23 )
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the M-by-N matrix A.
        /// On exit, A(N-K+1:N,1:MIN(K+L,M) ) contains the triangular
        /// matrix R or part of R.  See Purpose for details.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A. LDA .GE. max(1,M).
        ///</param>
        /// <param name="B">
        /// (input/output) DOUBLE PRECISION array, dimension (LDB,N)
        /// On entry, the P-by-N matrix B.
        /// On exit, if necessary, B(M-K+1:L,N+M-K-L+1:N) contains
        /// a part of R.  See Purpose for details.
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
        /// TOLA and TOLB are the convergence criteria for the Jacobi-
        /// Kogbetliantz iteration procedure. Generally, they are the
        /// same as used in the preprocessing step, say
        /// TOLA = max(M,N)*norm(A)*MAZHEPS,
        /// TOLB = max(P,N)*norm(B)*MAZHEPS.
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
        /// ALPHA(K+1:K+L) = diag(C),
        /// BETA(K+1:K+L)  = diag(S),
        /// or if M-K-L .LT. 0,
        /// ALPHA(K+1:M)= C, ALPHA(M+1:K+L)= 0
        /// BETA(K+1:M) = S, BETA(M+1:K+L) = 1.
        /// Furthermore, if K+L .LT. N,
        /// ALPHA(K+L+1:N) = 0 and
        /// BETA(K+L+1:N)  = 0.
        ///</param>
        /// <param name="U">
        /// (input/output) DOUBLE PRECISION array, dimension (LDU,M)
        /// On entry, if JOBU = 'U', U must contain a matrix U1 (usually
        /// the orthogonal matrix returned by DGGSVP).
        /// On exit,
        /// if JOBU = 'I', U contains the orthogonal matrix U;
        /// if JOBU = 'U', U contains the product U1*U.
        /// If JOBU = 'N', U is not referenced.
        ///</param>
        /// <param name="LDU">
        /// (input) INTEGER
        /// The leading dimension of the array U. LDU .GE. max(1,M) if
        /// JOBU = 'U'; LDU .GE. 1 otherwise.
        ///</param>
        /// <param name="V">
        /// (input/output) DOUBLE PRECISION array, dimension (LDV,P)
        /// On entry, if JOBV = 'V', V must contain a matrix V1 (usually
        /// the orthogonal matrix returned by DGGSVP).
        /// On exit,
        /// if JOBV = 'I', V contains the orthogonal matrix V;
        /// if JOBV = 'V', V contains the product V1*V.
        /// If JOBV = 'N', V is not referenced.
        ///</param>
        /// <param name="LDV">
        /// (input) INTEGER
        /// The leading dimension of the array V. LDV .GE. max(1,P) if
        /// JOBV = 'V'; LDV .GE. 1 otherwise.
        ///</param>
        /// <param name="Q">
        /// (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
        /// On entry, if JOBQ = 'Q', Q must contain a matrix Q1 (usually
        /// the orthogonal matrix returned by DGGSVP).
        /// On exit,
        /// if JOBQ = 'I', Q contains the orthogonal matrix Q;
        /// if JOBQ = 'Q', Q contains the product Q1*Q.
        /// If JOBQ = 'N', Q is not referenced.
        ///</param>
        /// <param name="LDQ">
        /// (input) INTEGER
        /// The leading dimension of the array Q. LDQ .GE. max(1,N) if
        /// JOBQ = 'Q'; LDQ .GE. 1 otherwise.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (2*N)
        ///</param>
        /// <param name="NCYCLE">
        /// (output) INTEGER
        /// The number of cycles required for convergence.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// = 1:  the procedure does not converge after MAXIT cycles.
        ///</param>
        public void Run(string JOBU, string JOBV, string JOBQ, int M, int P, int N
                         , int K, int L, ref double[] A, int offset_a, int LDA, ref double[] B, int offset_b, int LDB
                         , double TOLA, double TOLB, ref double[] ALPHA, int offset_alpha, ref double[] BETA, int offset_beta, ref double[] U, int offset_u, int LDU
                         , ref double[] V, int offset_v, int LDV, ref double[] Q, int offset_q, int LDQ, ref double[] WORK, int offset_work, ref int NCYCLE
                         , ref int INFO)
        {

            #region Variables
            
            bool INITQ = false; bool INITU = false; bool INITV = false; bool UPPER = false; bool WANTQ = false; 
            bool WANTU = false;bool WANTV = false; int I = 0; int J = 0; int KCYCLE = 0; double A1 = 0; double A2 = 0; 
            double A3 = 0;double B1 = 0; double B2 = 0; double B3 = 0; double CSQ = 0; double CSU = 0; double CSV = 0; 
            double ERROR = 0;double GAMMA = 0; double RWK = 0; double SNQ = 0; double SNU = 0; double SNV = 0; double SSMIN = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_b = -1 - LDB + offset_b;  int o_alpha = -1 + offset_alpha; 
             int o_beta = -1 + offset_beta; int o_u = -1 - LDU + offset_u;  int o_v = -1 - LDV + offset_v; 
             int o_q = -1 - LDQ + offset_q; int o_work = -1 + offset_work; 

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
            // *  DTGSJA computes the generalized singular value decomposition (GSVD)
            // *  of two real upper triangular (or trapezoidal) matrices A and B.
            // *
            // *  On entry, it is assumed that matrices A and B have the following
            // *  forms, which may be obtained by the preprocessing subroutine DGGSVP
            // *  from a general M-by-N matrix A and P-by-N matrix B:
            // *
            // *               N-K-L  K    L
            // *     A =    K ( 0    A12  A13 ) if M-K-L >= 0;
            // *            L ( 0     0   A23 )
            // *        M-K-L ( 0     0    0  )
            // *
            // *             N-K-L  K    L
            // *     A =  K ( 0    A12  A13 ) if M-K-L < 0;
            // *        M-K ( 0     0   A23 )
            // *
            // *             N-K-L  K    L
            // *     B =  L ( 0     0   B13 )
            // *        P-L ( 0     0    0  )
            // *
            // *  where the K-by-K matrix A12 and L-by-L matrix B13 are nonsingular
            // *  upper triangular; A23 is L-by-L upper triangular if M-K-L >= 0,
            // *  otherwise A23 is (M-K)-by-L upper trapezoidal.
            // *
            // *  On exit,
            // *
            // *              U'*A*Q = D1*( 0 R ),    V'*B*Q = D2*( 0 R ),
            // *
            // *  where U, V and Q are orthogonal matrices, Z' denotes the transpose
            // *  of Z, R is a nonsingular upper triangular matrix, and D1 and D2 are
            // *  ``diagonal'' matrices, which are of the following structures:
            // *
            // *  If M-K-L >= 0,
            // *
            // *                      K  L
            // *         D1 =     K ( I  0 )
            // *                  L ( 0  C )
            // *              M-K-L ( 0  0 )
            // *
            // *                    K  L
            // *         D2 = L   ( 0  S )
            // *              P-L ( 0  0 )
            // *
            // *                 N-K-L  K    L
            // *    ( 0 R ) = K (  0   R11  R12 ) K
            // *              L (  0    0   R22 ) L
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
            // *                 K M-K K+L-M
            // *      D1 =   K ( I  0    0   )
            // *           M-K ( 0  C    0   )
            // *
            // *                   K M-K K+L-M
            // *      D2 =   M-K ( 0  S    0   )
            // *           K+L-M ( 0  0    I   )
            // *             P-L ( 0  0    0   )
            // *
            // *                 N-K-L  K   M-K  K+L-M
            // * ( 0 R ) =    K ( 0    R11  R12  R13  )
            // *            M-K ( 0     0   R22  R23  )
            // *          K+L-M ( 0     0    0   R33  )
            // *
            // *  where
            // *  C = diag( ALPHA(K+1), ... , ALPHA(M) ),
            // *  S = diag( BETA(K+1),  ... , BETA(M) ),
            // *  C**2 + S**2 = I.
            // *
            // *  R = ( R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N) and R33 is stored
            // *      (  0  R22 R23 )
            // *  in B(M-K+1:L,N+M-K-L+1:N) on exit.
            // *
            // *  The computation of the orthogonal transformation matrices U, V or Q
            // *  is optional.  These matrices may either be formed explicitly, or they
            // *  may be postmultiplied into input matrices U1, V1, or Q1.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  JOBU    (input) CHARACTER*1
            // *          = 'U':  U must contain an orthogonal matrix U1 on entry, and
            // *                  the product U1*U is returned;
            // *          = 'I':  U is initialized to the unit matrix, and the
            // *                  orthogonal matrix U is returned;
            // *          = 'N':  U is not computed.
            // *
            // *  JOBV    (input) CHARACTER*1
            // *          = 'V':  V must contain an orthogonal matrix V1 on entry, and
            // *                  the product V1*V is returned;
            // *          = 'I':  V is initialized to the unit matrix, and the
            // *                  orthogonal matrix V is returned;
            // *          = 'N':  V is not computed.
            // *
            // *  JOBQ    (input) CHARACTER*1
            // *          = 'Q':  Q must contain an orthogonal matrix Q1 on entry, and
            // *                  the product Q1*Q is returned;
            // *          = 'I':  Q is initialized to the unit matrix, and the
            // *                  orthogonal matrix Q is returned;
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
            // *  K       (input) INTEGER
            // *  L       (input) INTEGER
            // *          K and L specify the subblocks in the input matrices A and B:
            // *          A23 = A(K+1:MIN(K+L,M),N-L+1:N) and B13 = B(1:L,N-L+1:N)
            // *          of A and B, whose GSVD is going to be computed by DTGSJA.
            // *          See Further details.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the M-by-N matrix A.
            // *          On exit, A(N-K+1:N,1:MIN(K+L,M) ) contains the triangular
            // *          matrix R or part of R.  See Purpose for details.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A. LDA >= max(1,M).
            // *
            // *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,N)
            // *          On entry, the P-by-N matrix B.
            // *          On exit, if necessary, B(M-K+1:L,N+M-K-L+1:N) contains
            // *          a part of R.  See Purpose for details.
            // *
            // *  LDB     (input) INTEGER
            // *          The leading dimension of the array B. LDB >= max(1,P).
            // *
            // *  TOLA    (input) DOUBLE PRECISION
            // *  TOLB    (input) DOUBLE PRECISION
            // *          TOLA and TOLB are the convergence criteria for the Jacobi-
            // *          Kogbetliantz iteration procedure. Generally, they are the
            // *          same as used in the preprocessing step, say
            // *              TOLA = max(M,N)*norm(A)*MAZHEPS,
            // *              TOLB = max(P,N)*norm(B)*MAZHEPS.
            // *
            // *  ALPHA   (output) DOUBLE PRECISION array, dimension (N)
            // *  BETA    (output) DOUBLE PRECISION array, dimension (N)
            // *          On exit, ALPHA and BETA contain the generalized singular
            // *          value pairs of A and B;
            // *            ALPHA(1:K) = 1,
            // *            BETA(1:K)  = 0,
            // *          and if M-K-L >= 0,
            // *            ALPHA(K+1:K+L) = diag(C),
            // *            BETA(K+1:K+L)  = diag(S),
            // *          or if M-K-L < 0,
            // *            ALPHA(K+1:M)= C, ALPHA(M+1:K+L)= 0
            // *            BETA(K+1:M) = S, BETA(M+1:K+L) = 1.
            // *          Furthermore, if K+L < N,
            // *            ALPHA(K+L+1:N) = 0 and
            // *            BETA(K+L+1:N)  = 0.
            // *
            // *  U       (input/output) DOUBLE PRECISION array, dimension (LDU,M)
            // *          On entry, if JOBU = 'U', U must contain a matrix U1 (usually
            // *          the orthogonal matrix returned by DGGSVP).
            // *          On exit,
            // *          if JOBU = 'I', U contains the orthogonal matrix U;
            // *          if JOBU = 'U', U contains the product U1*U.
            // *          If JOBU = 'N', U is not referenced.
            // *
            // *  LDU     (input) INTEGER
            // *          The leading dimension of the array U. LDU >= max(1,M) if
            // *          JOBU = 'U'; LDU >= 1 otherwise.
            // *
            // *  V       (input/output) DOUBLE PRECISION array, dimension (LDV,P)
            // *          On entry, if JOBV = 'V', V must contain a matrix V1 (usually
            // *          the orthogonal matrix returned by DGGSVP).
            // *          On exit,
            // *          if JOBV = 'I', V contains the orthogonal matrix V;
            // *          if JOBV = 'V', V contains the product V1*V.
            // *          If JOBV = 'N', V is not referenced.
            // *
            // *  LDV     (input) INTEGER
            // *          The leading dimension of the array V. LDV >= max(1,P) if
            // *          JOBV = 'V'; LDV >= 1 otherwise.
            // *
            // *  Q       (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
            // *          On entry, if JOBQ = 'Q', Q must contain a matrix Q1 (usually
            // *          the orthogonal matrix returned by DGGSVP).
            // *          On exit,
            // *          if JOBQ = 'I', Q contains the orthogonal matrix Q;
            // *          if JOBQ = 'Q', Q contains the product Q1*Q.
            // *          If JOBQ = 'N', Q is not referenced.
            // *
            // *  LDQ     (input) INTEGER
            // *          The leading dimension of the array Q. LDQ >= max(1,N) if
            // *          JOBQ = 'Q'; LDQ >= 1 otherwise.
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
            // *
            // *  NCYCLE  (output) INTEGER
            // *          The number of cycles required for convergence.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *          = 1:  the procedure does not converge after MAXIT cycles.
            // *
            // *  Internal Parameters
            // *  ===================
            // *
            // *  MAXIT   INTEGER
            // *          MAXIT specifies the total loops that the iterative procedure
            // *          may take. If after MAXIT cycles, the routine fails to
            // *          converge, we return INFO = 1.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  DTGSJA essentially uses a variant of Kogbetliantz algorithm to reduce
            // *  min(L,M-K)-by-L triangular (or trapezoidal) matrix A23 and L-by-L
            // *  matrix B13 to the form:
            // *
            // *           U1'*A13*Q1 = C1*R1; V1'*B13*Q1 = S1*R1,
            // *
            // *  where U1, V1 and Q1 are orthogonal matrix, and Z' is the transpose
            // *  of Z.  C1 and S1 are diagonal matrices satisfying
            // *
            // *                C1**2 + S1**2 = I,
            // *
            // *  and R1 is an L-by-L nonsingular upper triangular matrix.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *
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
            // *     Decode and test the input parameters
            // *

            #endregion


            #region Body
            
            INITU = this._lsame.Run(JOBU, "I");
            WANTU = INITU || this._lsame.Run(JOBU, "U");
            // *
            INITV = this._lsame.Run(JOBV, "I");
            WANTV = INITV || this._lsame.Run(JOBV, "V");
            // *
            INITQ = this._lsame.Run(JOBQ, "I");
            WANTQ = INITQ || this._lsame.Run(JOBQ, "Q");
            // *
            INFO = 0;
            if (!(INITU || WANTU || this._lsame.Run(JOBU, "N")))
            {
                INFO =  - 1;
            }
            else
            {
                if (!(INITV || WANTV || this._lsame.Run(JOBV, "N")))
                {
                    INFO =  - 2;
                }
                else
                {
                    if (!(INITQ || WANTQ || this._lsame.Run(JOBQ, "N")))
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
                                                INFO =  - 18;
                                            }
                                            else
                                            {
                                                if (LDV < 1 || (WANTV && LDV < P))
                                                {
                                                    INFO =  - 20;
                                                }
                                                else
                                                {
                                                    if (LDQ < 1 || (WANTQ && LDQ < N))
                                                    {
                                                        INFO =  - 22;
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
                this._xerbla.Run("DTGSJA",  - INFO);
                return;
            }
            // *
            // *     Initialize U, V and Q, if necessary
            // *
            if (INITU)
            {
                this._dlaset.Run("Full", M, M, ZERO, ONE, ref U, offset_u
                                 , LDU);
            }
            if (INITV)
            {
                this._dlaset.Run("Full", P, P, ZERO, ONE, ref V, offset_v
                                 , LDV);
            }
            if (INITQ)
            {
                this._dlaset.Run("Full", N, N, ZERO, ONE, ref Q, offset_q
                                 , LDQ);
            }
            // *
            // *     Loop until convergence
            // *
            UPPER = false;
            for (KCYCLE = 1; KCYCLE <= MAXIT; KCYCLE++)
            {
                // *
                UPPER = !UPPER;
                // *
                for (I = 1; I <= L - 1; I++)
                {
                    for (J = I + 1; J <= L; J++)
                    {
                        // *
                        A1 = ZERO;
                        A2 = ZERO;
                        A3 = ZERO;
                        if (K + I <= M) A1 = A[K + I+(N - L + I) * LDA + o_a];
                        if (K + J <= M) A3 = A[K + J+(N - L + J) * LDA + o_a];
                        // *
                        B1 = B[I+(N - L + I) * LDB + o_b];
                        B3 = B[J+(N - L + J) * LDB + o_b];
                        // *
                        if (UPPER)
                        {
                            if (K + I <= M) A2 = A[K + I+(N - L + J) * LDA + o_a];
                            B2 = B[I+(N - L + J) * LDB + o_b];
                        }
                        else
                        {
                            if (K + J <= M) A2 = A[K + J+(N - L + I) * LDA + o_a];
                            B2 = B[J+(N - L + I) * LDB + o_b];
                        }
                        // *
                        this._dlags2.Run(UPPER, A1, A2, A3, B1, B2
                                         , B3, ref CSU, ref SNU, ref CSV, ref SNV, ref CSQ
                                         , ref SNQ);
                        // *
                        // *              Update (K+I)-th and (K+J)-th rows of matrix A: U'*A
                        // *
                        if (K + J <= M)
                        {
                            this._drot.Run(L, ref A, K + J+(N - L + 1) * LDA + o_a, LDA, ref A, K + I+(N - L + 1) * LDA + o_a, LDA, CSU
                                           , SNU);
                        }
                        // *
                        // *              Update I-th and J-th rows of matrix B: V'*B
                        // *
                        this._drot.Run(L, ref B, J+(N - L + 1) * LDB + o_b, LDB, ref B, I+(N - L + 1) * LDB + o_b, LDB, CSV
                                       , SNV);
                        // *
                        // *              Update (N-L+I)-th and (N-L+J)-th columns of matrices
                        // *              A and B: A*Q and B*Q
                        // *
                        this._drot.Run(Math.Min(K + L, M), ref A, 1+(N - L + J) * LDA + o_a, 1, ref A, 1+(N - L + I) * LDA + o_a, 1, CSQ
                                       , SNQ);
                        // *
                        this._drot.Run(L, ref B, 1+(N - L + J) * LDB + o_b, 1, ref B, 1+(N - L + I) * LDB + o_b, 1, CSQ
                                       , SNQ);
                        // *
                        if (UPPER)
                        {
                            if (K + I <= M) A[K + I+(N - L + J) * LDA + o_a] = ZERO;
                            B[I+(N - L + J) * LDB + o_b] = ZERO;
                        }
                        else
                        {
                            if (K + J <= M) A[K + J+(N - L + I) * LDA + o_a] = ZERO;
                            B[J+(N - L + I) * LDB + o_b] = ZERO;
                        }
                        // *
                        // *              Update orthogonal matrices U, V, Q, if desired.
                        // *
                        if (WANTU && K + J <= M)
                        {
                            this._drot.Run(M, ref U, 1+(K + J) * LDU + o_u, 1, ref U, 1+(K + I) * LDU + o_u, 1, CSU
                                           , SNU);
                        }
                        // *
                        if (WANTV)
                        {
                            this._drot.Run(P, ref V, 1+J * LDV + o_v, 1, ref V, 1+I * LDV + o_v, 1, CSV
                                           , SNV);
                        }
                        // *
                        if (WANTQ)
                        {
                            this._drot.Run(N, ref Q, 1+(N - L + J) * LDQ + o_q, 1, ref Q, 1+(N - L + I) * LDQ + o_q, 1, CSQ
                                           , SNQ);
                        }
                        // *
                    }
                }
                // *
                if (!UPPER)
                {
                    // *
                    // *           The matrices A13 and B13 were lower triangular at the start
                    // *           of the cycle, and are now upper triangular.
                    // *
                    // *           Convergence test: test the parallelism of the corresponding
                    // *           rows of A and B.
                    // *
                    ERROR = ZERO;
                    for (I = 1; I <= Math.Min(L, M - K); I++)
                    {
                        this._dcopy.Run(L - I + 1, A, K + I+(N - L + I) * LDA + o_a, LDA, ref WORK, offset_work, 1);
                        this._dcopy.Run(L - I + 1, B, I+(N - L + I) * LDB + o_b, LDB, ref WORK, L + 1 + o_work, 1);
                        this._dlapll.Run(L - I + 1, ref WORK, offset_work, 1, ref WORK, L + 1 + o_work, 1, ref SSMIN);
                        ERROR = Math.Max(ERROR, SSMIN);
                    }
                    // *
                    if (Math.Abs(ERROR) <= Math.Min(TOLA, TOLB)) goto LABEL50;
                }
                // *
                // *        End of cycle loop
                // *
            }
            // *
            // *     The algorithm has not converged after MAXIT cycles.
            // *
            INFO = 1;
            goto LABEL100;
            // *
        LABEL50:;
            // *
            // *     If ERROR <= MIN(TOLA,TOLB), then the algorithm has converged.
            // *     Compute the generalized singular value pairs (ALPHA, BETA), and
            // *     set the triangular matrix R to array A.
            // *
            for (I = 1; I <= K; I++)
            {
                ALPHA[I + o_alpha] = ONE;
                BETA[I + o_beta] = ZERO;
            }
            // *
            for (I = 1; I <= Math.Min(L, M - K); I++)
            {
                // *
                A1 = A[K + I+(N - L + I) * LDA + o_a];
                B1 = B[I+(N - L + I) * LDB + o_b];
                // *
                if (A1 != ZERO)
                {
                    GAMMA = B1 / A1;
                    // *
                    // *           change sign if necessary
                    // *
                    if (GAMMA < ZERO)
                    {
                        this._dscal.Run(L - I + 1,  - ONE, ref B, I+(N - L + I) * LDB + o_b, LDB);
                        if (WANTV) this._dscal.Run(P,  - ONE, ref V, 1+I * LDV + o_v, 1);
                    }
                    // *
                    this._dlartg.Run(Math.Abs(GAMMA), ONE, ref BETA[K + I + o_beta], ref ALPHA[K + I + o_alpha], ref RWK);
                    // *
                    if (ALPHA[K + I + o_alpha] >= BETA[K + I + o_beta])
                    {
                        this._dscal.Run(L - I + 1, ONE / ALPHA[K + I + o_alpha], ref A, K + I+(N - L + I) * LDA + o_a, LDA);
                    }
                    else
                    {
                        this._dscal.Run(L - I + 1, ONE / BETA[K + I + o_beta], ref B, I+(N - L + I) * LDB + o_b, LDB);
                        this._dcopy.Run(L - I + 1, B, I+(N - L + I) * LDB + o_b, LDB, ref A, K + I+(N - L + I) * LDA + o_a, LDA);
                    }
                    // *
                }
                else
                {
                    // *
                    ALPHA[K + I + o_alpha] = ZERO;
                    BETA[K + I + o_beta] = ONE;
                    this._dcopy.Run(L - I + 1, B, I+(N - L + I) * LDB + o_b, LDB, ref A, K + I+(N - L + I) * LDA + o_a, LDA);
                    // *
                }
                // *
            }
            // *
            // *     Post-assignment
            // *
            for (I = M + 1; I <= K + L; I++)
            {
                ALPHA[I + o_alpha] = ZERO;
                BETA[I + o_beta] = ONE;
            }
            // *
            if (K + L < N)
            {
                for (I = K + L + 1; I <= N; I++)
                {
                    ALPHA[I + o_alpha] = ZERO;
                    BETA[I + o_beta] = ZERO;
                }
            }
            // *
        LABEL100:;
            NCYCLE = KCYCLE;
            return;
            // *
            // *     End of DTGSJA
            // *

            #endregion

        }
    }
}
