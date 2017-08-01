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
    /// DORGTR generates a real orthogonal matrix Q which is defined as the
    /// product of n-1 elementary reflectors of order N, as returned by
    /// DSYTRD:
    /// 
    /// if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),
    /// 
    /// if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).
    /// 
    ///</summary>
    public class DORGTR
    {
    

        #region Dependencies
        
        LSAME _lsame; ILAENV _ilaenv; DORGQL _dorgql; DORGQR _dorgqr; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 

        #endregion

        public DORGTR(LSAME lsame, ILAENV ilaenv, DORGQL dorgql, DORGQR dorgqr, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dorgql = dorgql; this._dorgqr = dorgqr; this._xerbla = xerbla; 

            #endregion

        }
    
        public DORGTR()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DCOPY dcopy = new DCOPY();
            XERBLA xerbla = new XERBLA();
            DSCAL dscal = new DSCAL();
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);
            DLARFB dlarfb = new DLARFB(lsame, dcopy, dgemm, dtrmm);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DTRMV dtrmv = new DTRMV(lsame, xerbla);
            DLARFT dlarft = new DLARFT(dgemv, dtrmv, lsame);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);
            DORG2L dorg2l = new DORG2L(dlarf, dscal, xerbla);
            DORGQL dorgql = new DORGQL(dlarfb, dlarft, dorg2l, xerbla, ilaenv);
            DORG2R dorg2r = new DORG2R(dlarf, dscal, xerbla);
            DORGQR dorgqr = new DORGQR(dlarfb, dlarft, dorg2r, xerbla, ilaenv);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dorgql = dorgql; this._dorgqr = dorgqr; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DORGTR generates a real orthogonal matrix Q which is defined as the
        /// product of n-1 elementary reflectors of order N, as returned by
        /// DSYTRD:
        /// 
        /// if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),
        /// 
        /// if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).
        /// 
        ///</summary>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// = 'U': Upper triangle of A contains elementary reflectors
        /// from DSYTRD;
        /// = 'L': Lower triangle of A contains elementary reflectors
        /// from DSYTRD.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix Q. N .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the vectors which define the elementary reflectors,
        /// as returned by DSYTRD.
        /// On exit, the N-by-N orthogonal matrix Q.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A. LDA .GE. max(1,N).
        ///</param>
        /// <param name="TAU">
        /// (input) DOUBLE PRECISION array, dimension (N-1)
        /// TAU(i) must contain the scalar factor of the elementary
        /// reflector H(i), as returned by DSYTRD.
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK. LWORK .GE. max(1,N-1).
        /// For optimum performance LWORK .GE. (N-1)*NB, where NB is
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
        public void Run(string UPLO, int N, ref double[] A, int offset_a, int LDA, double[] TAU, int offset_tau, ref double[] WORK, int offset_work
                         , int LWORK, ref int INFO)
        {

            #region Variables
            
            bool LQUERY = false; bool UPPER = false; int I = 0; int IINFO = 0; int J = 0; int LWKOPT = 0; int NB = 0; 

            #endregion


            #region Implicit Variables
            
            int A_J = 0; int A_0 = 0; int A_N = 0; int A_1 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_tau = -1 + offset_tau;  int o_work = -1 + offset_work; 

            #endregion


            #region Strings
            
            UPLO = UPLO.Substring(0, 1);  

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
            // *  DORGTR generates a real orthogonal matrix Q which is defined as the
            // *  product of n-1 elementary reflectors of order N, as returned by
            // *  DSYTRD:
            // *
            // *  if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),
            // *
            // *  if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).
            // *
            // *  Arguments
            // *  =========
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          = 'U': Upper triangle of A contains elementary reflectors
            // *                 from DSYTRD;
            // *          = 'L': Lower triangle of A contains elementary reflectors
            // *                 from DSYTRD.
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix Q. N >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the vectors which define the elementary reflectors,
            // *          as returned by DSYTRD.
            // *          On exit, the N-by-N orthogonal matrix Q.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A. LDA >= max(1,N).
            // *
            // *  TAU     (input) DOUBLE PRECISION array, dimension (N-1)
            // *          TAU(i) must contain the scalar factor of the elementary
            // *          reflector H(i), as returned by DSYTRD.
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK. LWORK >= max(1,N-1).
            // *          For optimum performance LWORK >= (N-1)*NB, where NB is
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
            //      INTRINSIC          MAX;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input arguments
            // *

            #endregion


            #region Body
            
            INFO = 0;
            LQUERY = (LWORK ==  - 1);
            UPPER = this._lsame.Run(UPLO, "U");
            if (!UPPER && !this._lsame.Run(UPLO, "L"))
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
                    if (LDA < Math.Max(1, N))
                    {
                        INFO =  - 4;
                    }
                    else
                    {
                        if (LWORK < Math.Max(1, N - 1) && !LQUERY)
                        {
                            INFO =  - 7;
                        }
                    }
                }
            }
            // *
            if (INFO == 0)
            {
                if (UPPER)
                {
                    NB = this._ilaenv.Run(1, "DORGQL", " ", N - 1, N - 1, N - 1,  - 1);
                }
                else
                {
                    NB = this._ilaenv.Run(1, "DORGQR", " ", N - 1, N - 1, N - 1,  - 1);
                }
                LWKOPT = Math.Max(1, N - 1) * NB;
                WORK[1 + o_work] = LWKOPT;
            }
            // *
            if (INFO != 0)
            {
                this._xerbla.Run("DORGTR",  - INFO);
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
            if (N == 0)
            {
                WORK[1 + o_work] = 1;
                return;
            }
            // *
            if (UPPER)
            {
                // *
                // *        Q was determined by a call to DSYTRD with UPLO = 'U'
                // *
                // *        Shift the vectors which define the elementary reflectors one
                // *        column to the left, and set the last row and column of Q to
                // *        those of the unit matrix
                // *
                for (J = 1; J <= N - 1; J++)
                {
                    A_J = J * LDA + o_a;
                    A_0 = (J + 1) * LDA + o_a;
                    for (I = 1; I <= J - 1; I++)
                    {
                        A[I + A_J] = A[I + A_0];
                    }
                    A[N+J * LDA + o_a] = ZERO;
                }
                A_N = N * LDA + o_a;
                for (I = 1; I <= N - 1; I++)
                {
                    A[I + A_N] = ZERO;
                }
                A[N+N * LDA + o_a] = ONE;
                // *
                // *        Generate Q(1:n-1,1:n-1)
                // *
                this._dorgql.Run(N - 1, N - 1, N - 1, ref A, offset_a, LDA, TAU, offset_tau
                                 , ref WORK, offset_work, LWORK, ref IINFO);
                // *
            }
            else
            {
                // *
                // *        Q was determined by a call to DSYTRD with UPLO = 'L'.
                // *
                // *        Shift the vectors which define the elementary reflectors one
                // *        column to the right, and set the first row and column of Q to
                // *        those of the unit matrix
                // *
                for (J = N; J >= 2; J +=  - 1)
                {
                    A[1+J * LDA + o_a] = ZERO;
                    A_J = J * LDA + o_a;
                    A_1 = (J - 1) * LDA + o_a;
                    for (I = J + 1; I <= N; I++)
                    {
                        A[I + A_J] = A[I + A_1];
                    }
                }
                A[1+1 * LDA + o_a] = ONE;
                A_1 = 1 * LDA + o_a;
                for (I = 2; I <= N; I++)
                {
                    A[I + A_1] = ZERO;
                }
                if (N > 1)
                {
                    // *
                    // *           Generate Q(2:n,2:n)
                    // *
                    this._dorgqr.Run(N - 1, N - 1, N - 1, ref A, 2+2 * LDA + o_a, LDA, TAU, offset_tau
                                     , ref WORK, offset_work, LWORK, ref IINFO);
                }
            }
            WORK[1 + o_work] = LWKOPT;
            return;
            // *
            // *     End of DORGTR
            // *

            #endregion

        }
    }
}
