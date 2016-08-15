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
    /// DGGRQF computes a generalized RQ factorization of an M-by-N matrix A
    /// and a P-by-N matrix B:
    /// 
    /// A = R*Q,        B = Z*T*Q,
    /// 
    /// where Q is an N-by-N orthogonal matrix, Z is a P-by-P orthogonal
    /// matrix, and R and T assume one of the forms:
    /// 
    /// if M .LE. N,  R = ( 0  R12 ) M,   or if M .GT. N,  R = ( R11 ) M-N,
    /// N-M  M                           ( R21 ) N
    /// N
    /// 
    /// where R12 or R21 is upper triangular, and
    /// 
    /// if P .GE. N,  T = ( T11 ) N  ,   or if P .LT. N,  T = ( T11  T12 ) P,
    /// (  0  ) P-N                         P   N-P
    /// N
    /// 
    /// where T11 is upper triangular.
    /// 
    /// In particular, if B is square and nonsingular, the GRQ factorization
    /// of A and B implicitly gives the RQ factorization of A*inv(B):
    /// 
    /// A*inv(B) = (R*inv(T))*Z'
    /// 
    /// where inv(B) denotes the inverse of the matrix B, and Z' denotes the
    /// transpose of the matrix Z.
    /// 
    ///</summary>
    public class DGGRQF
    {
    

        #region Dependencies
        
        DGEQRF _dgeqrf; DGERQF _dgerqf; DORMRQ _dormrq; XERBLA _xerbla; ILAENV _ilaenv; 

        #endregion

        public DGGRQF(DGEQRF dgeqrf, DGERQF dgerqf, DORMRQ dormrq, XERBLA xerbla, ILAENV ilaenv)
        {
    

            #region Set Dependencies
            
            this._dgeqrf = dgeqrf; this._dgerqf = dgerqf; this._dormrq = dormrq; this._xerbla = xerbla; this._ilaenv = ilaenv; 

            #endregion

        }
    
        public DGGRQF()
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
            DORMR2 dormr2 = new DORMR2(lsame, dlarf, xerbla);
            DORMRQ dormrq = new DORMRQ(lsame, ilaenv, dlarfb, dlarft, dormr2, xerbla);

            #endregion


            #region Set Dependencies
            
            this._dgeqrf = dgeqrf; this._dgerqf = dgerqf; this._dormrq = dormrq; this._xerbla = xerbla; this._ilaenv = ilaenv; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGGRQF computes a generalized RQ factorization of an M-by-N matrix A
        /// and a P-by-N matrix B:
        /// 
        /// A = R*Q,        B = Z*T*Q,
        /// 
        /// where Q is an N-by-N orthogonal matrix, Z is a P-by-P orthogonal
        /// matrix, and R and T assume one of the forms:
        /// 
        /// if M .LE. N,  R = ( 0  R12 ) M,   or if M .GT. N,  R = ( R11 ) M-N,
        /// N-M  M                           ( R21 ) N
        /// N
        /// 
        /// where R12 or R21 is upper triangular, and
        /// 
        /// if P .GE. N,  T = ( T11 ) N  ,   or if P .LT. N,  T = ( T11  T12 ) P,
        /// (  0  ) P-N                         P   N-P
        /// N
        /// 
        /// where T11 is upper triangular.
        /// 
        /// In particular, if B is square and nonsingular, the GRQ factorization
        /// of A and B implicitly gives the RQ factorization of A*inv(B):
        /// 
        /// A*inv(B) = (R*inv(T))*Z'
        /// 
        /// where inv(B) denotes the inverse of the matrix B, and Z' denotes the
        /// transpose of the matrix Z.
        /// 
        ///</summary>
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
        /// The number of columns of the matrices A and B. N .GE. 0.
        ///</param>
        /// <param name="A">
        /// = R*Q,        B = Z*T*Q,
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A. LDA .GE. max(1,M).
        ///</param>
        /// <param name="TAUA">
        /// (output) DOUBLE PRECISION array, dimension (min(M,N))
        /// The scalar factors of the elementary reflectors which
        /// represent the orthogonal matrix Q (see Further Details).
        ///</param>
        /// <param name="B">
        /// (input/output) DOUBLE PRECISION array, dimension (LDB,N)
        /// On entry, the P-by-N matrix B.
        /// On exit, the elements on and above the diagonal of the array
        /// contain the min(P,N)-by-N upper trapezoidal matrix T (T is
        /// upper triangular if P .GE. N); the elements below the diagonal,
        /// with the array TAUB, represent the orthogonal matrix Z as a
        /// product of elementary reflectors (see Further Details).
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of the array B. LDB .GE. max(1,P).
        ///</param>
        /// <param name="TAUB">
        /// (output) DOUBLE PRECISION array, dimension (min(P,N))
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
        /// where NB1 is the optimal blocksize for the RQ factorization
        /// of an M-by-N matrix, NB2 is the optimal blocksize for the
        /// QR factorization of a P-by-N matrix, and NB3 is the optimal
        /// blocksize for a call of DORMRQ.
        /// 
        /// If LWORK = -1, then a workspace query is assumed; the routine
        /// only calculates the optimal size of the WORK array, returns
        /// this value as the first entry of the WORK array, and no error
        /// message related to LWORK is issued by XERBLA.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INF0= -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(int M, int P, int N, ref double[] A, int offset_a, int LDA, ref double[] TAUA, int offset_taua
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
            // *  DGGRQF computes a generalized RQ factorization of an M-by-N matrix A
            // *  and a P-by-N matrix B:
            // *
            // *              A = R*Q,        B = Z*T*Q,
            // *
            // *  where Q is an N-by-N orthogonal matrix, Z is a P-by-P orthogonal
            // *  matrix, and R and T assume one of the forms:
            // *
            // *  if M <= N,  R = ( 0  R12 ) M,   or if M > N,  R = ( R11 ) M-N,
            // *                   N-M  M                           ( R21 ) N
            // *                                                       N
            // *
            // *  where R12 or R21 is upper triangular, and
            // *
            // *  if P >= N,  T = ( T11 ) N  ,   or if P < N,  T = ( T11  T12 ) P,
            // *                  (  0  ) P-N                         P   N-P
            // *                     N
            // *
            // *  where T11 is upper triangular.
            // *
            // *  In particular, if B is square and nonsingular, the GRQ factorization
            // *  of A and B implicitly gives the RQ factorization of A*inv(B):
            // *
            // *               A*inv(B) = (R*inv(T))*Z'
            // *
            // *  where inv(B) denotes the inverse of the matrix B, and Z' denotes the
            // *  transpose of the matrix Z.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix A.  M >= 0.
            // *
            // *  P       (input) INTEGER
            // *          The number of rows of the matrix B.  P >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrices A and B. N >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the M-by-N matrix A.
            // *          On exit, if M <= N, the upper triangle of the subarray
            // *          A(1:M,N-M+1:N) contains the M-by-M upper triangular matrix R;
            // *          if M > N, the elements on and above the (M-N)-th subdiagonal
            // *          contain the M-by-N upper trapezoidal matrix R; the remaining
            // *          elements, with the array TAUA, represent the orthogonal
            // *          matrix Q as a product of elementary reflectors (see Further
            // *          Details).
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A. LDA >= max(1,M).
            // *
            // *  TAUA    (output) DOUBLE PRECISION array, dimension (min(M,N))
            // *          The scalar factors of the elementary reflectors which
            // *          represent the orthogonal matrix Q (see Further Details).
            // *
            // *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,N)
            // *          On entry, the P-by-N matrix B.
            // *          On exit, the elements on and above the diagonal of the array
            // *          contain the min(P,N)-by-N upper trapezoidal matrix T (T is
            // *          upper triangular if P >= N); the elements below the diagonal,
            // *          with the array TAUB, represent the orthogonal matrix Z as a
            // *          product of elementary reflectors (see Further Details).
            // *
            // *  LDB     (input) INTEGER
            // *          The leading dimension of the array B. LDB >= max(1,P).
            // *
            // *  TAUB    (output) DOUBLE PRECISION array, dimension (min(P,N))
            // *          The scalar factors of the elementary reflectors which
            // *          represent the orthogonal matrix Z (see Further Details).
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK. LWORK >= max(1,N,M,P).
            // *          For optimum performance LWORK >= max(N,M,P)*max(NB1,NB2,NB3),
            // *          where NB1 is the optimal blocksize for the RQ factorization
            // *          of an M-by-N matrix, NB2 is the optimal blocksize for the
            // *          QR factorization of a P-by-N matrix, and NB3 is the optimal
            // *          blocksize for a call of DORMRQ.
            // *
            // *          If LWORK = -1, then a workspace query is assumed; the routine
            // *          only calculates the optimal size of the WORK array, returns
            // *          this value as the first entry of the WORK array, and no error
            // *          message related to LWORK is issued by XERBLA.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INF0= -i, the i-th argument had an illegal value.
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
            // *     H(i) = I - taua * v * v'
            // *
            // *  where taua is a real scalar, and v is a real vector with
            // *  v(n-k+i+1:n) = 0 and v(n-k+i) = 1; v(1:n-k+i-1) is stored on exit in
            // *  A(m-k+i,1:n-k+i-1), and taua in TAUA(i).
            // *  To form Q explicitly, use LAPACK subroutine DORGRQ.
            // *  To use Q to update another matrix, use LAPACK subroutine DORMRQ.
            // *
            // *  The matrix Z is represented as a product of elementary reflectors
            // *
            // *     Z = H(1) H(2) . . . H(k), where k = min(p,n).
            // *
            // *  Each H(i) has the form
            // *
            // *     H(i) = I - taub * v * v'
            // *
            // *  where taub is a real scalar, and v is a real vector with
            // *  v(1:i-1) = 0 and v(i) = 1; v(i+1:p) is stored on exit in B(i+1:p,i),
            // *  and taub in TAUB(i).
            // *  To form Z explicitly, use LAPACK subroutine DORGQR.
            // *  To use Z to update another matrix, use LAPACK subroutine DORMQR.
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
            NB1 = this._ilaenv.Run(1, "DGERQF", " ", M, N,  - 1,  - 1);
            NB2 = this._ilaenv.Run(1, "DGEQRF", " ", P, N,  - 1,  - 1);
            NB3 = this._ilaenv.Run(1, "DORMRQ", " ", M, N, P,  - 1);
            NB = Math.Max(NB1, Math.Max(NB2, NB3));
            LWKOPT = Math.Max(N, Math.Max(M, P)) * NB;
            WORK[1 + o_work] = LWKOPT;
            LQUERY = (LWORK ==  - 1);
            if (M < 0)
            {
                INFO =  - 1;
            }
            else
            {
                if (P < 0)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (N < 0)
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
                                INFO =  - 8;
                            }
                            else
                            {
                                if (LWORK < Math.Max(1, Math.Max(M, Math.Max(P, N))) && !LQUERY)
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
                this._xerbla.Run("DGGRQF",  - INFO);
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
            // *     RQ factorization of M-by-N matrix A: A = R*Q
            // *
            this._dgerqf.Run(M, N, ref A, offset_a, LDA, ref TAUA, offset_taua, ref WORK, offset_work
                             , LWORK, ref INFO);
            LOPT = (int)WORK[1 + o_work];
            // *
            // *     Update B := B*Q'
            // *
            this._dormrq.Run("Right", "Transpose", P, N, Math.Min(M, N), ref A, Math.Max(1, M - N + 1)+1 * LDA + o_a
                             , LDA, TAUA, offset_taua, ref B, offset_b, LDB, ref WORK, offset_work, LWORK
                             , ref INFO);
            LOPT = Math.Max(LOPT, Convert.ToInt32(Math.Truncate(WORK[1 + o_work])));
            // *
            // *     QR factorization of P-by-N matrix B: B = Z*T
            // *
            this._dgeqrf.Run(P, N, ref B, offset_b, LDB, ref TAUB, offset_taub, ref WORK, offset_work
                             , LWORK, ref INFO);
            WORK[1 + o_work] = Math.Max(LOPT, Convert.ToInt32(Math.Truncate(WORK[1 + o_work])));
            // *
            return;
            // *
            // *     End of DGGRQF
            // *

            #endregion

        }
    }
}
