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
    /// DORGQL generates an M-by-N real matrix Q with orthonormal columns,
    /// which is defined as the last N columns of a product of K elementary
    /// reflectors of order M
    /// 
    /// Q  =  H(k) . . . H(2) H(1)
    /// 
    /// as returned by DGEQLF.
    /// 
    ///</summary>
    public class DORGQL
    {
    

        #region Dependencies
        
        DLARFB _dlarfb; DLARFT _dlarft; DORG2L _dorg2l; XERBLA _xerbla; ILAENV _ilaenv; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; 

        #endregion

        public DORGQL(DLARFB dlarfb, DLARFT dlarft, DORG2L dorg2l, XERBLA xerbla, ILAENV ilaenv)
        {
    

            #region Set Dependencies
            
            this._dlarfb = dlarfb; this._dlarft = dlarft; this._dorg2l = dorg2l; this._xerbla = xerbla; this._ilaenv = ilaenv; 

            #endregion

        }
    
        public DORGQL()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DCOPY dcopy = new DCOPY();
            XERBLA xerbla = new XERBLA();
            DSCAL dscal = new DSCAL();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);
            DLARFB dlarfb = new DLARFB(lsame, dcopy, dgemm, dtrmm);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DTRMV dtrmv = new DTRMV(lsame, xerbla);
            DLARFT dlarft = new DLARFT(dgemv, dtrmv, lsame);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);
            DORG2L dorg2l = new DORG2L(dlarf, dscal, xerbla);
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);

            #endregion


            #region Set Dependencies
            
            this._dlarfb = dlarfb; this._dlarft = dlarft; this._dorg2l = dorg2l; this._xerbla = xerbla; this._ilaenv = ilaenv; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DORGQL generates an M-by-N real matrix Q with orthonormal columns,
        /// which is defined as the last N columns of a product of K elementary
        /// reflectors of order M
        /// 
        /// Q  =  H(k) . . . H(2) H(1)
        /// 
        /// as returned by DGEQLF.
        /// 
        ///</summary>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix Q. M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix Q. M .GE. N .GE. 0.
        ///</param>
        /// <param name="K">
        /// (input) INTEGER
        /// The number of elementary reflectors whose product defines the
        /// matrix Q. N .GE. K .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the (n-k+i)-th column must contain the vector which
        /// defines the elementary reflector H(i), for i = 1,2,...,k, as
        /// returned by DGEQLF in the last k columns of its array
        /// argument A.
        /// On exit, the M-by-N matrix Q.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The first dimension of the array A. LDA .GE. max(1,M).
        ///</param>
        /// <param name="TAU">
        /// (input) DOUBLE PRECISION array, dimension (K)
        /// TAU(i) must contain the scalar factor of the elementary
        /// reflector H(i), as returned by DGEQLF.
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK. LWORK .GE. max(1,N).
        /// For optimum performance LWORK .GE. N*NB, where NB is the
        /// optimal blocksize.
        /// 
        /// If LWORK = -1, then a workspace query is assumed; the routine
        /// only calculates the optimal size of the WORK array, returns
        /// this value as the first entry of the WORK array, and no error
        /// message related to LWORK is issued by XERBLA.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument has an illegal value
        ///</param>
        public void Run(int M, int N, int K, ref double[] A, int offset_a, int LDA, double[] TAU, int offset_tau
                         , ref double[] WORK, int offset_work, int LWORK, ref int INFO)
        {

            #region Variables
            
            bool LQUERY = false; int I = 0; int IB = 0; int IINFO = 0; int IWS = 0; int J = 0; int KK = 0; int L = 0; 
            int LDWORK = 0;int LWKOPT = 0; int NB = 0; int NBMIN = 0; int NX = 0; 

            #endregion


            #region Implicit Variables
            
            int A_J = 0; 

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
            // *  DORGQL generates an M-by-N real matrix Q with orthonormal columns,
            // *  which is defined as the last N columns of a product of K elementary
            // *  reflectors of order M
            // *
            // *        Q  =  H(k) . . . H(2) H(1)
            // *
            // *  as returned by DGEQLF.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix Q. M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix Q. M >= N >= 0.
            // *
            // *  K       (input) INTEGER
            // *          The number of elementary reflectors whose product defines the
            // *          matrix Q. N >= K >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the (n-k+i)-th column must contain the vector which
            // *          defines the elementary reflector H(i), for i = 1,2,...,k, as
            // *          returned by DGEQLF in the last k columns of its array
            // *          argument A.
            // *          On exit, the M-by-N matrix Q.
            // *
            // *  LDA     (input) INTEGER
            // *          The first dimension of the array A. LDA >= max(1,M).
            // *
            // *  TAU     (input) DOUBLE PRECISION array, dimension (K)
            // *          TAU(i) must contain the scalar factor of the elementary
            // *          reflector H(i), as returned by DGEQLF.
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK. LWORK >= max(1,N).
            // *          For optimum performance LWORK >= N*NB, where NB is the
            // *          optimal blocksize.
            // *
            // *          If LWORK = -1, then a workspace query is assumed; the routine
            // *          only calculates the optimal size of the WORK array, returns
            // *          this value as the first entry of the WORK array, and no error
            // *          message related to LWORK is issued by XERBLA.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument has an illegal value
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
                if (N < 0 || N > M)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (K < 0 || K > N)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (LDA < Math.Max(1, M))
                        {
                            INFO =  - 5;
                        }
                    }
                }
            }
            // *
            if (INFO == 0)
            {
                if (N == 0)
                {
                    LWKOPT = 1;
                }
                else
                {
                    NB = this._ilaenv.Run(1, "DORGQL", " ", M, N, K,  - 1);
                    LWKOPT = N * NB;
                }
                WORK[1 + o_work] = LWKOPT;
                // *
                if (LWORK < Math.Max(1, N) && !LQUERY)
                {
                    INFO =  - 8;
                }
            }
            // *
            if (INFO != 0)
            {
                this._xerbla.Run("DORGQL",  - INFO);
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
            if (N <= 0)
            {
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
                NX = Math.Max(0, this._ilaenv.Run(3, "DORGQL", " ", M, N, K,  - 1));
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
                        NBMIN = Math.Max(2, this._ilaenv.Run(2, "DORGQL", " ", M, N, K,  - 1));
                    }
                }
            }
            // *
            if (NB >= NBMIN && NB < K && NX < K)
            {
                // *
                // *        Use blocked code after the first block.
                // *        The last kk columns are handled by the block method.
                // *
                KK = Math.Min(K, ((K - NX + NB - 1) / NB) * NB);
                // *
                // *        Set A(m-kk+1:m,1:n-kk) to zero.
                // *
                for (J = 1; J <= N - KK; J++)
                {
                    A_J = J * LDA + o_a;
                    for (I = M - KK + 1; I <= M; I++)
                    {
                        A[I + A_J] = ZERO;
                    }
                }
            }
            else
            {
                KK = 0;
            }
            // *
            // *     Use unblocked code for the first or only block.
            // *
            this._dorg2l.Run(M - KK, N - KK, K - KK, ref A, offset_a, LDA, TAU, offset_tau
                             , ref WORK, offset_work, ref IINFO);
            // *
            if (KK > 0)
            {
                // *
                // *        Use blocked code
                // *
                for (I = K - KK + 1; (NB >= 0) ? (I <= K) : (I >= K); I += NB)
                {
                    IB = Math.Min(NB, K - I + 1);
                    if (N - K + I > 1)
                    {
                        // *
                        // *              Form the triangular factor of the block reflector
                        // *              H = H(i+ib-1) . . . H(i+1) H(i)
                        // *
                        this._dlarft.Run("Backward", "Columnwise", M - K + I + IB - 1, IB, ref A, 1+(N - K + I) * LDA + o_a, LDA
                                         , TAU, I + o_tau, ref WORK, offset_work, LDWORK);
                        // *
                        // *              Apply H to A(1:m-k+i+ib-1,1:n-k+i-1) from the left
                        // *
                        this._dlarfb.Run("Left", "No transpose", "Backward", "Columnwise", M - K + I + IB - 1, N - K + I - 1
                                         , IB, A, 1+(N - K + I) * LDA + o_a, LDA, WORK, offset_work, LDWORK, ref A, offset_a
                                         , LDA, ref WORK, IB + 1 + o_work, LDWORK);
                    }
                    // *
                    // *           Apply H to rows 1:m-k+i+ib-1 of current block
                    // *
                    this._dorg2l.Run(M - K + I + IB - 1, IB, IB, ref A, 1+(N - K + I) * LDA + o_a, LDA, TAU, I + o_tau
                                     , ref WORK, offset_work, ref IINFO);
                    // *
                    // *           Set rows m-k+i+ib:m of current block to zero
                    // *
                    for (J = N - K + I; J <= N - K + I + IB - 1; J++)
                    {
                        A_J = J * LDA + o_a;
                        for (L = M - K + I + IB; L <= M; L++)
                        {
                            A[L + A_J] = ZERO;
                        }
                    }
                }
            }
            // *
            WORK[1 + o_work] = IWS;
            return;
            // *
            // *     End of DORGQL
            // *

            #endregion

        }
    }
}
