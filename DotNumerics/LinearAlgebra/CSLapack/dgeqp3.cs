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
    /// DGEQP3 computes a QR factorization with column pivoting of a
    /// matrix A:  A*P = Q*R  using Level 3 BLAS.
    /// 
    ///</summary>
    public class DGEQP3
    {
    

        #region Dependencies
        
        DGEQRF _dgeqrf; DLAQP2 _dlaqp2; DLAQPS _dlaqps; DORMQR _dormqr; DSWAP _dswap; XERBLA _xerbla; ILAENV _ilaenv; 
        DNRM2 _dnrm2;

        #endregion


        #region Variables
        
        const int INB = 1; const int INBMIN = 2; const int IXOVER = 3; 

        #endregion

        public DGEQP3(DGEQRF dgeqrf, DLAQP2 dlaqp2, DLAQPS dlaqps, DORMQR dormqr, DSWAP dswap, XERBLA xerbla, ILAENV ilaenv, DNRM2 dnrm2)
        {
    

            #region Set Dependencies
            
            this._dgeqrf = dgeqrf; this._dlaqp2 = dlaqp2; this._dlaqps = dlaqps; this._dormqr = dormqr; this._dswap = dswap; 
            this._xerbla = xerbla;this._ilaenv = ilaenv; this._dnrm2 = dnrm2; 

            #endregion

        }
    
        public DGEQP3()
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
            DSWAP dswap = new DSWAP();
            IDAMAX idamax = new IDAMAX();
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
            DLAQP2 dlaqp2 = new DLAQP2(dlarf, dlarfg, dswap, idamax, dlamch, dnrm2);
            DLAQPS dlaqps = new DLAQPS(dgemm, dgemv, dlarfg, dswap, idamax, dlamch, dnrm2);
            DORM2R dorm2r = new DORM2R(lsame, dlarf, xerbla);
            DORMQR dormqr = new DORMQR(lsame, ilaenv, dlarfb, dlarft, dorm2r, xerbla);

            #endregion


            #region Set Dependencies
            
            this._dgeqrf = dgeqrf; this._dlaqp2 = dlaqp2; this._dlaqps = dlaqps; this._dormqr = dormqr; this._dswap = dswap; 
            this._xerbla = xerbla;this._ilaenv = ilaenv; this._dnrm2 = dnrm2; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGEQP3 computes a QR factorization with column pivoting of a
        /// matrix A:  A*P = Q*R  using Level 3 BLAS.
        /// 
        ///</summary>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix A. M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the M-by-N matrix A.
        /// On exit, the upper triangle of the array contains the
        /// min(M,N)-by-N upper trapezoidal matrix R; the elements below
        /// the diagonal, together with the array TAU, represent the
        /// orthogonal matrix Q as a product of min(M,N) elementary
        /// reflectors.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A. LDA .GE. max(1,M).
        ///</param>
        /// <param name="JPVT">
        /// (input/output) INTEGER array, dimension (N)
        /// On entry, if JPVT(J).ne.0, the J-th column of A is permuted
        /// to the front of A*P (a leading column); if JPVT(J)=0,
        /// the J-th column of A is a free column.
        /// On exit, if JPVT(J)=K, then the J-th column of A*P was the
        /// the K-th column of A.
        ///</param>
        /// <param name="TAU">
        /// (output) DOUBLE PRECISION array, dimension (min(M,N))
        /// The scalar factors of the elementary reflectors.
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// On exit, if INFO=0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK. LWORK .GE. 3*N+1.
        /// For optimal performance LWORK .GE. 2*N+( N+1 )*NB, where NB
        /// is the optimal blocksize.
        /// 
        /// If LWORK = -1, then a workspace query is assumed; the routine
        /// only calculates the optimal size of the WORK array, returns
        /// this value as the first entry of the WORK array, and no error
        /// message related to LWORK is issued by XERBLA.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0: successful exit.
        /// .LT. 0: if INFO = -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(int M, int N, ref double[] A, int offset_a, int LDA, ref int[] JPVT, int offset_jpvt, ref double[] TAU, int offset_tau
                         , ref double[] WORK, int offset_work, int LWORK, ref int INFO)
        {

            #region Variables
            
            bool LQUERY = false; int FJB = 0; int IWS = 0; int J = 0; int JB = 0; int LWKOPT = 0; int MINMN = 0; int MINWS = 0; 
            int NA = 0;int NB = 0; int NBMIN = 0; int NFXD = 0; int NX = 0; int SM = 0; int SMINMN = 0; int SN = 0; 
            int TOPBMN = 0;

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_jpvt = -1 + offset_jpvt;  int o_tau = -1 + offset_tau; 
             int o_work = -1 + offset_work;

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
            // *  DGEQP3 computes a QR factorization with column pivoting of a
            // *  matrix A:  A*P = Q*R  using Level 3 BLAS.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix A. M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix A.  N >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the M-by-N matrix A.
            // *          On exit, the upper triangle of the array contains the
            // *          min(M,N)-by-N upper trapezoidal matrix R; the elements below
            // *          the diagonal, together with the array TAU, represent the
            // *          orthogonal matrix Q as a product of min(M,N) elementary
            // *          reflectors.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A. LDA >= max(1,M).
            // *
            // *  JPVT    (input/output) INTEGER array, dimension (N)
            // *          On entry, if JPVT(J).ne.0, the J-th column of A is permuted
            // *          to the front of A*P (a leading column); if JPVT(J)=0,
            // *          the J-th column of A is a free column.
            // *          On exit, if JPVT(J)=K, then the J-th column of A*P was the
            // *          the K-th column of A.
            // *
            // *  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
            // *          The scalar factors of the elementary reflectors.
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO=0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK. LWORK >= 3*N+1.
            // *          For optimal performance LWORK >= 2*N+( N+1 )*NB, where NB
            // *          is the optimal blocksize.
            // *
            // *          If LWORK = -1, then a workspace query is assumed; the routine
            // *          only calculates the optimal size of the WORK array, returns
            // *          this value as the first entry of the WORK array, and no error
            // *          message related to LWORK is issued by XERBLA.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0: successful exit.
            // *          < 0: if INFO = -i, the i-th argument had an illegal value.
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
            // *     H(i) = I - tau * v * v'
            // *
            // *  where tau is a real/complex scalar, and v is a real/complex vector
            // *  with v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in
            // *  A(i+1:m,i), and tau in TAU(i).
            // *
            // *  Based on contributions by
            // *    G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
            // *    X. Sun, Computer Science Dept., Duke University, USA
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
            // *     Test input arguments
            // *     ====================
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
                if (N < 0)
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
                MINMN = Math.Min(M, N);
                if (MINMN == 0)
                {
                    IWS = 1;
                    LWKOPT = 1;
                }
                else
                {
                    IWS = 3 * N + 1;
                    NB = this._ilaenv.Run(INB, "DGEQRF", " ", M, N,  - 1,  - 1);
                    LWKOPT = 2 * N + (N + 1) * NB;
                }
                WORK[1 + o_work] = LWKOPT;
                // *
                if ((LWORK < IWS) && !LQUERY)
                {
                    INFO =  - 8;
                }
            }
            // *
            if (INFO != 0)
            {
                this._xerbla.Run("DGEQP3",  - INFO);
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
            // *     Quick return if possible.
            // *
            if (MINMN == 0)
            {
                return;
            }
            // *
            // *     Move initial columns up front.
            // *
            NFXD = 1;
            for (J = 1; J <= N; J++)
            {
                if (JPVT[J + o_jpvt] != 0)
                {
                    if (J != NFXD)
                    {
                        this._dswap.Run(M, ref A, 1+J * LDA + o_a, 1, ref A, 1+NFXD * LDA + o_a, 1);
                        JPVT[J + o_jpvt] = JPVT[NFXD + o_jpvt];
                        JPVT[NFXD + o_jpvt] = J;
                    }
                    else
                    {
                        JPVT[J + o_jpvt] = J;
                    }
                    NFXD += 1;
                }
                else
                {
                    JPVT[J + o_jpvt] = J;
                }
            }
            NFXD -= 1;
            // *
            // *     Factorize fixed columns
            // *     =======================
            // *
            // *     Compute the QR factorization of fixed columns and update
            // *     remaining columns.
            // *
            if (NFXD > 0)
            {
                NA = Math.Min(M, NFXD);
                // *CC      CALL DGEQR2( M, NA, A, LDA, TAU, WORK, INFO )
                this._dgeqrf.Run(M, NA, ref A, offset_a, LDA, ref TAU, offset_tau, ref WORK, offset_work
                                 , LWORK, ref INFO);
                IWS = Math.Max(IWS, Convert.ToInt32(Math.Truncate(WORK[1 + o_work])));
                if (NA < N)
                {
                    // *CC         CALL DORM2R( 'Left', 'Transpose', M, N-NA, NA, A, LDA,
                    // *CC  $                   TAU, A( 1, NA+1 ), LDA, WORK, INFO )
                    this._dormqr.Run("Left", "Transpose", M, N - NA, NA, ref A, offset_a
                                     , LDA, TAU, offset_tau, ref A, 1+(NA + 1) * LDA + o_a, LDA, ref WORK, offset_work, LWORK
                                     , ref INFO);
                    IWS = Math.Max(IWS, Convert.ToInt32(Math.Truncate(WORK[1 + o_work])));
                }
            }
            // *
            // *     Factorize free columns
            // *     ======================
            // *
            if (NFXD < MINMN)
            {
                // *
                SM = M - NFXD;
                SN = N - NFXD;
                SMINMN = MINMN - NFXD;
                // *
                // *        Determine the block size.
                // *
                NB = this._ilaenv.Run(INB, "DGEQRF", " ", SM, SN,  - 1,  - 1);
                NBMIN = 2;
                NX = 0;
                // *
                if ((NB > 1) && (NB < SMINMN))
                {
                    // *
                    // *           Determine when to cross over from blocked to unblocked code.
                    // *
                    NX = Math.Max(0, this._ilaenv.Run(IXOVER, "DGEQRF", " ", SM, SN,  - 1,  - 1));
                    // *
                    // *
                    if (NX < SMINMN)
                    {
                        // *
                        // *              Determine if workspace is large enough for blocked code.
                        // *
                        MINWS = 2 * SN + (SN + 1) * NB;
                        IWS = Math.Max(IWS, MINWS);
                        if (LWORK < MINWS)
                        {
                            // *
                            // *                 Not enough workspace to use optimal NB: Reduce NB and
                            // *                 determine the minimum value of NB.
                            // *
                            NB = (LWORK - 2 * SN) / (SN + 1);
                            NBMIN = Math.Max(2, this._ilaenv.Run(INBMIN, "DGEQRF", " ", SM, SN,  - 1,  - 1));
                            // *
                            // *
                        }
                    }
                }
                // *
                // *        Initialize partial column norms. The first N elements of work
                // *        store the exact column norms.
                // *
                for (J = NFXD + 1; J <= N; J++)
                {
                    WORK[J + o_work] = this._dnrm2.Run(SM, A, NFXD + 1+J * LDA + o_a, 1);
                    WORK[N + J + o_work] = WORK[J + o_work];
                }
                // *
                if ((NB >= NBMIN) && (NB < SMINMN) && (NX < SMINMN))
                {
                    // *
                    // *           Use blocked code initially.
                    // *
                    J = NFXD + 1;
                    // *
                    // *           Compute factorization: while loop.
                    // *
                    // *
                    TOPBMN = MINMN - NX;
                LABEL30:;
                    if (J <= TOPBMN)
                    {
                        JB = Math.Min(NB, TOPBMN - J + 1);
                        // *
                        // *              Factorize JB columns among columns J:N.
                        // *
                        this._dlaqps.Run(M, N - J + 1, J - 1, JB, ref FJB, ref A, 1+J * LDA + o_a
                                         , LDA, ref JPVT, J + o_jpvt, ref TAU, J + o_tau, ref WORK, J + o_work, ref WORK, N + J + o_work, ref WORK, 2 * N + 1 + o_work
                                         , ref WORK, 2 * N + JB + 1 + o_work, N - J + 1);
                        // *
                        J += FJB;
                        goto LABEL30;
                    }
                }
                else
                {
                    J = NFXD + 1;
                }
                // *
                // *        Use unblocked code to factor the last or only block.
                // *
                // *
                if (J <= MINMN)
                {
                    this._dlaqp2.Run(M, N - J + 1, J - 1, ref A, 1+J * LDA + o_a, LDA, ref JPVT, J + o_jpvt
                                     , ref TAU, J + o_tau, ref WORK, J + o_work, ref WORK, N + J + o_work, ref WORK, 2 * N + 1 + o_work);
                }
                // *
            }
            // *
            WORK[1 + o_work] = IWS;
            return;
            // *
            // *     End of DGEQP3
            // *

            #endregion

        }
    }
}
