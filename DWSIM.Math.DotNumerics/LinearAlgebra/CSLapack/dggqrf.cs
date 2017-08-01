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
    /// DGGQRF computes a generalized QR factorization of an N-by-M matrix A
    /// and an N-by-P matrix B:
    /// 
    /// A = Q*R,        B = Q*T*Z,
    /// 
    /// where Q is an N-by-N orthogonal matrix, Z is a P-by-P orthogonal
    /// matrix, and R and T assume one of the forms:
    /// 
    /// if N .GE. M,  R = ( R11 ) M  ,   or if N .LT. M,  R = ( R11  R12 ) N,
    /// (  0  ) N-M                         N   M-N
    /// M
    /// 
    /// where R11 is upper triangular, and
    /// 
    /// if N .LE. P,  T = ( 0  T12 ) N,   or if N .GT. P,  T = ( T11 ) N-P,
    /// P-N  N                           ( T21 ) P
    /// P
    /// 
    /// where T12 or T21 is upper triangular.
    /// 
    /// In particular, if B is square and nonsingular, the GQR factorization
    /// of A and B implicitly gives the QR factorization of inv(B)*A:
    /// 
    /// inv(B)*A = Z'*(inv(T)*R)
    /// 
    /// where inv(B) denotes the inverse of the matrix B, and Z' denotes the
    /// transpose of the matrix Z.
    /// 
    ///</summary>
    public class DGGQRF
    {
    

        #region Dependencies
        
        DGEQRF _dgeqrf; DGERQF _dgerqf; DORMQR _dormqr; XERBLA _xerbla; ILAENV _ilaenv; 

        #endregion

        public DGGQRF(DGEQRF dgeqrf, DGERQF dgerqf, DORMQR dormqr, XERBLA xerbla, ILAENV ilaenv)
        {
    

            #region Set Dependencies
            
            this._dgeqrf = dgeqrf; this._dgerqf = dgerqf; this._dormqr = dormqr; this._xerbla = xerbla; this._ilaenv = ilaenv; 

            #endregion

        }
    
        public DGGQRF()
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
            DGEQRF dgeqrf = new DGEQRF(dgeqr2, dlarfb, dlarft, xerbla, ilaenv);
            DGERQ2 dgerq2 = new DGERQ2(dlarf, dlarfg, xerbla);
            DGERQF dgerqf = new DGERQF(dgerq2, dlarfb, dlarft, xerbla, ilaenv);
            DORM2R dorm2r = new DORM2R(lsame, dlarf, xerbla);
            DORMQR dormqr = new DORMQR(lsame, ilaenv, dlarfb, dlarft, dorm2r, xerbla);

            #endregion


            #region Set Dependencies
            
            this._dgeqrf = dgeqrf; this._dgerqf = dgerqf; this._dormqr = dormqr; this._xerbla = xerbla; this._ilaenv = ilaenv; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGGQRF computes a generalized QR factorization of an N-by-M matrix A
        /// and an N-by-P matrix B:
        /// 
        /// A = Q*R,        B = Q*T*Z,
        /// 
        /// where Q is an N-by-N orthogonal matrix, Z is a P-by-P orthogonal
        /// matrix, and R and T assume one of the forms:
        /// 
        /// if N .GE. M,  R = ( R11 ) M  ,   or if N .LT. M,  R = ( R11  R12 ) N,
        /// (  0  ) N-M                         N   M-N
        /// M
        /// 
        /// where R11 is upper triangular, and
        /// 
        /// if N .LE. P,  T = ( 0  T12 ) N,   or if N .GT. P,  T = ( T11 ) N-P,
        /// P-N  N                           ( T21 ) P
        /// P
        /// 
        /// where T12 or T21 is upper triangular.
        /// 
        /// In particular, if B is square and nonsingular, the GQR factorization
        /// of A and B implicitly gives the QR factorization of inv(B)*A:
        /// 
        /// inv(B)*A = Z'*(inv(T)*R)
        /// 
        /// where inv(B) denotes the inverse of the matrix B, and Z' denotes the
        /// transpose of the matrix Z.
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of rows of the matrices A and B. N .GE. 0.
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of columns of the matrix A.  M .GE. 0.
        ///</param>
        /// <param name="P">
        /// (input) INTEGER
        /// The number of columns of the matrix B.  P .GE. 0.
        ///</param>
        /// <param name="A">
        /// = Q*R,        B = Q*T*Z,
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A. LDA .GE. max(1,N).
        ///</param>
        /// <param name="TAUA">
        /// (output) DOUBLE PRECISION array, dimension (min(N,M))
        /// The scalar factors of the elementary reflectors which
        /// represent the orthogonal matrix Q (see Further Details).
        ///</param>
        /// <param name="B">
        /// (input/output) DOUBLE PRECISION array, dimension (LDB,P)
        /// On entry, the N-by-P matrix B.
        /// On exit, if N .LE. P, the upper triangle of the subarray
        /// B(1:N,P-N+1:P) contains the N-by-N upper triangular matrix T;
        /// if N .GT. P, the elements on and above the (N-P)-th subdiagonal
        /// contain the N-by-P upper trapezoidal matrix T; the remaining
        /// elements, with the array TAUB, represent the orthogonal
        /// matrix Z as a product of elementary reflectors (see Further
        /// Details).
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of the array B. LDB .GE. max(1,N).
        ///</param>
        /// <param name="TAUB">
        /// (output) DOUBLE PRECISION array, dimension (min(N,P))
        /// The scalar factors of the elementary reflectors which
        /// represent the orthogonal matrix Z (see Further Details).
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK. LWORK .GE. max(1,N,M,P).
        /// For optimum performance LWORK .GE. max(N,M,P)*max(NB1,NB2,NB3),
        /// where NB1 is the optimal blocksize for the QR factorization
        /// of an N-by-M matrix, NB2 is the optimal blocksize for the
        /// RQ factorization of an N-by-P matrix, and NB3 is the optimal
        /// blocksize for a call of DORMQR.
        /// 
        /// If LWORK = -1, then a workspace query is assumed; the routine
        /// only calculates the optimal size of the WORK array, returns
        /// this value as the first entry of the WORK array, and no error
        /// message related to LWORK is issued by XERBLA.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(int N, int M, int P, ref double[] A, int offset_a, int LDA, ref double[] TAUA, int offset_taua
                         , ref double[] B, int offset_b, int LDB, ref double[] TAUB, int offset_taub, ref double[] WORK, int offset_work, int LWORK, ref int INFO)
        {

            #region Variables
            
            bool LQUERY = false; int LOPT = 0; int LWKOPT = 0; int NB = 0; int NB1 = 0; int NB2 = 0; int NB3 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_taua = -1 + offset_taua;  int o_b = -1 - LDB + offset_b; 
             int o_taub = -1 + offset_taub; int o_work = -1 + offset_work; 

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
            // *  DGGQRF computes a generalized QR factorization of an N-by-M matrix A
            // *  and an N-by-P matrix B:
            // *
            // *              A = Q*R,        B = Q*T*Z,
            // *
            // *  where Q is an N-by-N orthogonal matrix, Z is a P-by-P orthogonal
            // *  matrix, and R and T assume one of the forms:
            // *
            // *  if N >= M,  R = ( R11 ) M  ,   or if N < M,  R = ( R11  R12 ) N,
            // *                  (  0  ) N-M                         N   M-N
            // *                     M
            // *
            // *  where R11 is upper triangular, and
            // *
            // *  if N <= P,  T = ( 0  T12 ) N,   or if N > P,  T = ( T11 ) N-P,
            // *                   P-N  N                           ( T21 ) P
            // *                                                       P
            // *
            // *  where T12 or T21 is upper triangular.
            // *
            // *  In particular, if B is square and nonsingular, the GQR factorization
            // *  of A and B implicitly gives the QR factorization of inv(B)*A:
            // *
            // *               inv(B)*A = Z'*(inv(T)*R)
            // *
            // *  where inv(B) denotes the inverse of the matrix B, and Z' denotes the
            // *  transpose of the matrix Z.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N       (input) INTEGER
            // *          The number of rows of the matrices A and B. N >= 0.
            // *
            // *  M       (input) INTEGER
            // *          The number of columns of the matrix A.  M >= 0.
            // *
            // *  P       (input) INTEGER
            // *          The number of columns of the matrix B.  P >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,M)
            // *          On entry, the N-by-M matrix A.
            // *          On exit, the elements on and above the diagonal of the array
            // *          contain the min(N,M)-by-M upper trapezoidal matrix R (R is
            // *          upper triangular if N >= M); the elements below the diagonal,
            // *          with the array TAUA, represent the orthogonal matrix Q as a
            // *          product of min(N,M) elementary reflectors (see Further
            // *          Details).
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A. LDA >= max(1,N).
            // *
            // *  TAUA    (output) DOUBLE PRECISION array, dimension (min(N,M))
            // *          The scalar factors of the elementary reflectors which
            // *          represent the orthogonal matrix Q (see Further Details).
            // *
            // *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,P)
            // *          On entry, the N-by-P matrix B.
            // *          On exit, if N <= P, the upper triangle of the subarray
            // *          B(1:N,P-N+1:P) contains the N-by-N upper triangular matrix T;
            // *          if N > P, the elements on and above the (N-P)-th subdiagonal
            // *          contain the N-by-P upper trapezoidal matrix T; the remaining
            // *          elements, with the array TAUB, represent the orthogonal
            // *          matrix Z as a product of elementary reflectors (see Further
            // *          Details).
            // *
            // *  LDB     (input) INTEGER
            // *          The leading dimension of the array B. LDB >= max(1,N).
            // *
            // *  TAUB    (output) DOUBLE PRECISION array, dimension (min(N,P))
            // *          The scalar factors of the elementary reflectors which
            // *          represent the orthogonal matrix Z (see Further Details).
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK. LWORK >= max(1,N,M,P).
            // *          For optimum performance LWORK >= max(N,M,P)*max(NB1,NB2,NB3),
            // *          where NB1 is the optimal blocksize for the QR factorization
            // *          of an N-by-M matrix, NB2 is the optimal blocksize for the
            // *          RQ factorization of an N-by-P matrix, and NB3 is the optimal
            // *          blocksize for a call of DORMQR.
            // *
            // *          If LWORK = -1, then a workspace query is assumed; the routine
            // *          only calculates the optimal size of the WORK array, returns
            // *          this value as the first entry of the WORK array, and no error
            // *          message related to LWORK is issued by XERBLA.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  The matrix Q is represented as a product of elementary reflectors
            // *
            // *     Q = H(1) H(2) . . . H(k), where k = min(n,m).
            // *
            // *  Each H(i) has the form
            // *
            // *     H(i) = I - taua * v * v'
            // *
            // *  where taua is a real scalar, and v is a real vector with
            // *  v(1:i-1) = 0 and v(i) = 1; v(i+1:n) is stored on exit in A(i+1:n,i),
            // *  and taua in TAUA(i).
            // *  To form Q explicitly, use LAPACK subroutine DORGQR.
            // *  To use Q to update another matrix, use LAPACK subroutine DORMQR.
            // *
            // *  The matrix Z is represented as a product of elementary reflectors
            // *
            // *     Z = H(1) H(2) . . . H(k), where k = min(n,p).
            // *
            // *  Each H(i) has the form
            // *
            // *     H(i) = I - taub * v * v'
            // *
            // *  where taub is a real scalar, and v is a real vector with
            // *  v(p-k+i+1:p) = 0 and v(p-k+i) = 1; v(1:p-k+i-1) is stored on exit in
            // *  B(n-k+i,1:p-k+i-1), and taub in TAUB(i).
            // *  To form Z explicitly, use LAPACK subroutine DORGRQ.
            // *  To use Z to update another matrix, use LAPACK subroutine DORMRQ.
            // *
            // *  =====================================================================
            // *
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
            NB1 = this._ilaenv.Run(1, "DGEQRF", " ", N, M,  - 1,  - 1);
            NB2 = this._ilaenv.Run(1, "DGERQF", " ", N, P,  - 1,  - 1);
            NB3 = this._ilaenv.Run(1, "DORMQR", " ", N, M, P,  - 1);
            NB = Math.Max(NB1, Math.Max(NB2, NB3));
            LWKOPT = Math.Max(N, Math.Max(M, P)) * NB;
            WORK[1 + o_work] = LWKOPT;
            LQUERY = (LWORK ==  - 1);
            if (N < 0)
            {
                INFO =  - 1;
            }
            else
            {
                if (M < 0)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (P < 0)
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
                                INFO =  - 8;
                            }
                            else
                            {
                                if (LWORK < Math.Max(1, Math.Max(N, Math.Max(M, P))) && !LQUERY)
                                {
                                    INFO =  - 11;
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGGQRF",  - INFO);
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
            // *     QR factorization of N-by-M matrix A: A = Q*R
            // *
            this._dgeqrf.Run(N, M, ref A, offset_a, LDA, ref TAUA, offset_taua, ref WORK, offset_work
                             , LWORK, ref INFO);
            LOPT = (int)WORK[1 + o_work];
            // *
            // *     Update B := Q'*B.
            // *
            this._dormqr.Run("Left", "Transpose", N, P, Math.Min(N, M), ref A, offset_a
                             , LDA, TAUA, offset_taua, ref B, offset_b, LDB, ref WORK, offset_work, LWORK
                             , ref INFO);
            LOPT = Math.Max(LOPT, Convert.ToInt32(Math.Truncate(WORK[1 + o_work])));
            // *
            // *     RQ factorization of N-by-P matrix B: B = T*Z.
            // *
            this._dgerqf.Run(N, P, ref B, offset_b, LDB, ref TAUB, offset_taub, ref WORK, offset_work
                             , LWORK, ref INFO);
            WORK[1 + o_work] = Math.Max(LOPT, Convert.ToInt32(Math.Truncate(WORK[1 + o_work])));
            // *
            return;
            // *
            // *     End of DGGQRF
            // *

            #endregion

        }
    }
}
