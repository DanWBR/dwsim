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
    /// DTZRZF reduces the M-by-N ( M.LE.N ) real upper trapezoidal matrix A
    /// to upper triangular form by means of orthogonal transformations.
    /// 
    /// The upper trapezoidal matrix A is factored as
    /// 
    /// A = ( R  0 ) * Z,
    /// 
    /// where Z is an N-by-N orthogonal matrix and R is an M-by-M upper
    /// triangular matrix.
    /// 
    ///</summary>
    public class DTZRZF
    {
    

        #region Dependencies
        
        DLARZB _dlarzb; DLARZT _dlarzt; DLATRZ _dlatrz; XERBLA _xerbla; ILAENV _ilaenv; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; 

        #endregion

        public DTZRZF(DLARZB dlarzb, DLARZT dlarzt, DLATRZ dlatrz, XERBLA xerbla, ILAENV ilaenv)
        {
    

            #region Set Dependencies
            
            this._dlarzb = dlarzb; this._dlarzt = dlarzt; this._dlatrz = dlatrz; this._xerbla = xerbla; this._ilaenv = ilaenv; 

            #endregion

        }
    
        public DTZRZF()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DCOPY dcopy = new DCOPY();
            XERBLA xerbla = new XERBLA();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DAXPY daxpy = new DAXPY();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);
            DLARZB dlarzb = new DLARZB(lsame, dcopy, dgemm, dtrmm, xerbla);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DTRMV dtrmv = new DTRMV(lsame, xerbla);
            DLARZT dlarzt = new DLARZT(dgemv, dtrmv, xerbla, lsame);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DGER dger = new DGER(xerbla);
            DLARZ dlarz = new DLARZ(daxpy, dcopy, dgemv, dger, lsame);
            DLATRZ dlatrz = new DLATRZ(dlarfg, dlarz);
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);

            #endregion


            #region Set Dependencies
            
            this._dlarzb = dlarzb; this._dlarzt = dlarzt; this._dlatrz = dlatrz; this._xerbla = xerbla; this._ilaenv = ilaenv; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DTZRZF reduces the M-by-N ( M.LE.N ) real upper trapezoidal matrix A
        /// to upper triangular form by means of orthogonal transformations.
        /// 
        /// The upper trapezoidal matrix A is factored as
        /// 
        /// A = ( R  0 ) * Z,
        /// 
        /// where Z is an N-by-N orthogonal matrix and R is an M-by-M upper
        /// triangular matrix.
        /// 
        ///</summary>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix A.  M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix A.  N .GE. M.
        ///</param>
        /// <param name="A">
        /// = ( R  0 ) * Z,
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,M).
        ///</param>
        /// <param name="TAU">
        /// (output) DOUBLE PRECISION array, dimension (M)
        /// The scalar factors of the elementary reflectors.
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK.  LWORK .GE. max(1,M).
        /// For optimum performance LWORK .GE. M*NB, where NB is
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
            
            bool LQUERY = false; int I = 0; int IB = 0; int IWS = 0; int KI = 0; int KK = 0; int LDWORK = 0; int LWKOPT = 0; 
            int M1 = 0;int MU = 0; int NB = 0; int NBMIN = 0; int NX = 0; 

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
            // *  DTZRZF reduces the M-by-N ( M<=N ) real upper trapezoidal matrix A
            // *  to upper triangular form by means of orthogonal transformations.
            // *
            // *  The upper trapezoidal matrix A is factored as
            // *
            // *     A = ( R  0 ) * Z,
            // *
            // *  where Z is an N-by-N orthogonal matrix and R is an M-by-M upper
            // *  triangular matrix.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix A.  M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix A.  N >= M.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the leading M-by-N upper trapezoidal part of the
            // *          array A must contain the matrix to be factorized.
            // *          On exit, the leading M-by-M upper triangular part of A
            // *          contains the upper triangular matrix R, and elements M+1 to
            // *          N of the first M rows of A, with the array TAU, represent the
            // *          orthogonal matrix Z as a product of M elementary reflectors.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,M).
            // *
            // *  TAU     (output) DOUBLE PRECISION array, dimension (M)
            // *          The scalar factors of the elementary reflectors.
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK.  LWORK >= max(1,M).
            // *          For optimum performance LWORK >= M*NB, where NB is
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
            // *  Based on contributions by
            // *    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
            // *
            // *  The factorization is obtained by Householder's method.  The kth
            // *  transformation matrix, Z( k ), which is used to introduce zeros into
            // *  the ( m - k + 1 )th row of A, is given in the form
            // *
            // *     Z( k ) = ( I     0   ),
            // *              ( 0  T( k ) )
            // *
            // *  where
            // *
            // *     T( k ) = I - tau*u( k )*u( k )',   u( k ) = (   1    ),
            // *                                                 (   0    )
            // *                                                 ( z( k ) )
            // *
            // *  tau is a scalar and z( k ) is an ( n - m ) element vector.
            // *  tau and z( k ) are chosen to annihilate the elements of the kth row
            // *  of X.
            // *
            // *  The scalar tau is returned in the kth element of TAU and the vector
            // *  u( k ) in the kth row of A, such that the elements of z( k ) are
            // *  in  a( k, m + 1 ), ..., a( k, n ). The elements of R are returned in
            // *  the upper triangular part of A.
            // *
            // *  Z is given by
            // *
            // *     Z =  Z( 1 ) * Z( 2 ) * ... * Z( m ).
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
            // *     .. External Functions ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input arguments
            // *

            #endregion


            #region Body
            
            INFO = 0;
            LQUERY = (LWORK ==  - 1);
            if (M < 0)
            {
                INFO =  - 1;
            }
            else
            {
                if (N < M)
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
            // *
            if (INFO == 0)
            {
                if (M == 0 || M == N)
                {
                    LWKOPT = 1;
                }
                else
                {
                    // *
                    // *           Determine the block size.
                    // *
                    NB = this._ilaenv.Run(1, "DGERQF", " ", M, N,  - 1,  - 1);
                    LWKOPT = M * NB;
                }
                WORK[1 + o_work] = LWKOPT;
                // *
                if (LWORK < Math.Max(1, M) && !LQUERY)
                {
                    INFO =  - 7;
                }
            }
            // *
            if (INFO != 0)
            {
                this._xerbla.Run("DTZRZF",  - INFO);
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
            if (M == 0)
            {
                return;
            }
            else
            {
                if (M == N)
                {
                    for (I = 1; I <= N; I++)
                    {
                        TAU[I + o_tau] = ZERO;
                    }
                    return;
                }
            }
            // *
            NBMIN = 2;
            NX = 1;
            IWS = M;
            if (NB > 1 && NB < M)
            {
                // *
                // *        Determine when to cross over from blocked to unblocked code.
                // *
                NX = Math.Max(0, this._ilaenv.Run(3, "DGERQF", " ", M, N,  - 1,  - 1));
                if (NX < M)
                {
                    // *
                    // *           Determine if workspace is large enough for blocked code.
                    // *
                    LDWORK = M;
                    IWS = LDWORK * NB;
                    if (LWORK < IWS)
                    {
                        // *
                        // *              Not enough workspace to use optimal NB:  reduce NB and
                        // *              determine the minimum value of NB.
                        // *
                        NB = LWORK / LDWORK;
                        NBMIN = Math.Max(2, this._ilaenv.Run(2, "DGERQF", " ", M, N,  - 1,  - 1));
                    }
                }
            }
            // *
            if (NB >= NBMIN && NB < M && NX < M)
            {
                // *
                // *        Use blocked code initially.
                // *        The last kk rows are handled by the block method.
                // *
                M1 = Math.Min(M + 1, N);
                KI = ((M - NX - 1) / NB) * NB;
                KK = Math.Min(M, KI + NB);
                // *
                for (I = M - KK + KI + 1; ( - NB >= 0) ? (I <= M - KK + 1) : (I >= M - KK + 1); I +=  - NB)
                {
                    IB = Math.Min(M - I + 1, NB);
                    // *
                    // *           Compute the TZ factorization of the current block
                    // *           A(i:i+ib-1,i:n)
                    // *
                    this._dlatrz.Run(IB, N - I + 1, N - M, ref A, I+I * LDA + o_a, LDA, ref TAU, I + o_tau
                                     , ref WORK, offset_work);
                    if (I > 1)
                    {
                        // *
                        // *              Form the triangular factor of the block reflector
                        // *              H = H(i+ib-1) . . . H(i+1) H(i)
                        // *
                        this._dlarzt.Run("Backward", "Rowwise", N - M, IB, A, I+M1 * LDA + o_a, LDA
                                         , TAU, I + o_tau, ref WORK, offset_work, LDWORK);
                        // *
                        // *              Apply H to A(1:i-1,i:n) from the right
                        // *
                        this._dlarzb.Run("Right", "No transpose", "Backward", "Rowwise", I - 1, N - I + 1
                                         , IB, N - M, A, I+M1 * LDA + o_a, LDA, WORK, offset_work, LDWORK
                                         , ref A, 1+I * LDA + o_a, LDA, ref WORK, IB + 1 + o_work, LDWORK);
                    }
                }
                MU = I + NB - 1;
            }
            else
            {
                MU = M;
            }
            // *
            // *     Use unblocked code to factor the last or only block
            // *
            if (MU > 0)
            {
                this._dlatrz.Run(MU, N, N - M, ref A, offset_a, LDA, ref TAU, offset_tau
                                 , ref WORK, offset_work);
            }
            // *
            WORK[1 + o_work] = LWKOPT;
            // *
            return;
            // *
            // *     End of DTZRZF
            // *

            #endregion

        }
    }
}
