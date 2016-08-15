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
    /// DGEHRD reduces a real general matrix A to upper Hessenberg form H by
    /// an orthogonal similarity transformation:  Q' * A * Q = H .
    /// 
    ///</summary>
    public class DGEHRD
    {
    

        #region Dependencies
        
        DAXPY _daxpy; DGEHD2 _dgehd2; DGEMM _dgemm; DLAHR2 _dlahr2; DLARFB _dlarfb; DTRMM _dtrmm; XERBLA _xerbla; ILAENV _ilaenv; 

        #endregion


        #region Variables
        
        const int NBMAX = 64; const int LDT = NBMAX + 1; const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 
        double[] T = new double[LDT * NBMAX];

        #endregion

        public DGEHRD(DAXPY daxpy, DGEHD2 dgehd2, DGEMM dgemm, DLAHR2 dlahr2, DLARFB dlarfb, DTRMM dtrmm, XERBLA xerbla, ILAENV ilaenv)
        {
    

            #region Set Dependencies
            
            this._daxpy = daxpy; this._dgehd2 = dgehd2; this._dgemm = dgemm; this._dlahr2 = dlahr2; this._dlarfb = dlarfb; 
            this._dtrmm = dtrmm;this._xerbla = xerbla; this._ilaenv = ilaenv; 

            #endregion

        }
    
        public DGEHRD()
        {
    

            #region Dependencies (Initialization)
            
            DAXPY daxpy = new DAXPY();
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
            DGEHD2 dgehd2 = new DGEHD2(dlarf, dlarfg, xerbla);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DLACPY dlacpy = new DLACPY(lsame);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);
            DTRMV dtrmv = new DTRMV(lsame, xerbla);
            DLAHR2 dlahr2 = new DLAHR2(daxpy, dcopy, dgemm, dgemv, dlacpy, dlarfg, dscal, dtrmm, dtrmv);
            DLARFB dlarfb = new DLARFB(lsame, dcopy, dgemm, dtrmm);
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);

            #endregion


            #region Set Dependencies
            
            this._daxpy = daxpy; this._dgehd2 = dgehd2; this._dgemm = dgemm; this._dlahr2 = dlahr2; this._dlarfb = dlarfb; 
            this._dtrmm = dtrmm;this._xerbla = xerbla; this._ilaenv = ilaenv; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGEHRD reduces a real general matrix A to upper Hessenberg form H by
        /// an orthogonal similarity transformation:  Q' * A * Q = H .
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="ILO">
        /// (input) INTEGER
        ///</param>
        /// <param name="IHI">
        /// (input) INTEGER
        /// It is assumed that A is already upper triangular in rows
        /// and columns 1:ILO-1 and IHI+1:N. ILO and IHI are normally
        /// set by a previous call to DGEBAL; otherwise they should be
        /// set to 1 and N respectively. See Further Details.
        /// 1 .LE. ILO .LE. IHI .LE. N, if N .GT. 0; ILO=1 and IHI=0, if N=0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the N-by-N general matrix to be reduced.
        /// On exit, the upper triangle and the first subdiagonal of A
        /// are overwritten with the upper Hessenberg matrix H, and the
        /// elements below the first subdiagonal, with the array TAU,
        /// represent the orthogonal matrix Q as a product of elementary
        /// reflectors. See Further Details.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,N).
        ///</param>
        /// <param name="TAU">
        /// (output) DOUBLE PRECISION array, dimension (N-1)
        /// The scalar factors of the elementary reflectors (see Further
        /// Details). Elements 1:ILO-1 and IHI:N-1 of TAU are set to
        /// zero.
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The length of the array WORK.  LWORK .GE. max(1,N).
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
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(int N, int ILO, int IHI, ref double[] A, int offset_a, int LDA, ref double[] TAU, int offset_tau
                         , ref double[] WORK, int offset_work, int LWORK, ref int INFO)
        {

            #region Variables
            
            bool LQUERY = false; int I = 0; int IB = 0; int IINFO = 0; int IWS = 0; int J = 0; int LDWORK = 0; int LWKOPT = 0; 
            int NB = 0;int NBMIN = 0; int NH = 0; int NX = 0; double EI = 0; int offset_t = 0;

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
            // *  DGEHRD reduces a real general matrix A to upper Hessenberg form H by
            // *  an orthogonal similarity transformation:  Q' * A * Q = H .
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.
            // *
            // *  ILO     (input) INTEGER
            // *  IHI     (input) INTEGER
            // *          It is assumed that A is already upper triangular in rows
            // *          and columns 1:ILO-1 and IHI+1:N. ILO and IHI are normally
            // *          set by a previous call to DGEBAL; otherwise they should be
            // *          set to 1 and N respectively. See Further Details.
            // *          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the N-by-N general matrix to be reduced.
            // *          On exit, the upper triangle and the first subdiagonal of A
            // *          are overwritten with the upper Hessenberg matrix H, and the
            // *          elements below the first subdiagonal, with the array TAU,
            // *          represent the orthogonal matrix Q as a product of elementary
            // *          reflectors. See Further Details.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,N).
            // *
            // *  TAU     (output) DOUBLE PRECISION array, dimension (N-1)
            // *          The scalar factors of the elementary reflectors (see Further
            // *          Details). Elements 1:ILO-1 and IHI:N-1 of TAU are set to
            // *          zero.
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The length of the array WORK.  LWORK >= max(1,N).
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
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  The matrix Q is represented as a product of (ihi-ilo) elementary
            // *  reflectors
            // *
            // *     Q = H(ilo) H(ilo+1) . . . H(ihi-1).
            // *
            // *  Each H(i) has the form
            // *
            // *     H(i) = I - tau * v * v'
            // *
            // *  where tau is a real scalar, and v is a real vector with
            // *  v(1:i) = 0, v(i+1) = 1 and v(ihi+1:n) = 0; v(i+2:ihi) is stored on
            // *  exit in A(i+2:ihi,i), and tau in TAU(i).
            // *
            // *  The contents of A are illustrated by the following example, with
            // *  n = 7, ilo = 2 and ihi = 6:
            // *
            // *  on entry,                        on exit,
            // *
            // *  ( a   a   a   a   a   a   a )    (  a   a   h   h   h   h   a )
            // *  (     a   a   a   a   a   a )    (      a   h   h   h   h   a )
            // *  (     a   a   a   a   a   a )    (      h   h   h   h   h   h )
            // *  (     a   a   a   a   a   a )    (      v2  h   h   h   h   h )
            // *  (     a   a   a   a   a   a )    (      v2  v3  h   h   h   h )
            // *  (     a   a   a   a   a   a )    (      v2  v3  v4  h   h   h )
            // *  (                         a )    (                          a )
            // *
            // *  where a denotes an element of the original matrix A, h denotes a
            // *  modified element of the upper Hessenberg matrix H, and vi denotes an
            // *  element of the vector defining H(i).
            // *
            // *  This file is a slight modification of LAPACK-3.0's DGEHRD
            // *  subroutine incorporating improvements proposed by Quintana-Orti and
            // *  Van de Geijn (2005). 
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Local Arrays ..
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
            // *     Test the input parameters
            // *

            #endregion


            #region Body
            
            INFO = 0;
            NB = Math.Min(NBMAX, this._ilaenv.Run(1, "DGEHRD", " ", N, ILO, IHI,  - 1));
            LWKOPT = N * NB;
            WORK[1 + o_work] = LWKOPT;
            LQUERY = (LWORK ==  - 1);
            if (N < 0)
            {
                INFO =  - 1;
            }
            else
            {
                if (ILO < 1 || ILO > Math.Max(1, N))
                {
                    INFO =  - 2;
                }
                else
                {
                    if (IHI < Math.Min(ILO, N) || IHI > N)
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
                            if (LWORK < Math.Max(1, N) && !LQUERY)
                            {
                                INFO =  - 8;
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGEHRD",  - INFO);
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
            // *     Set elements 1:ILO-1 and IHI:N-1 of TAU to zero
            // *
            for (I = 1; I <= ILO - 1; I++)
            {
                TAU[I + o_tau] = ZERO;
            }
            for (I = Math.Max(1, IHI); I <= N - 1; I++)
            {
                TAU[I + o_tau] = ZERO;
            }
            // *
            // *     Quick return if possible
            // *
            NH = IHI - ILO + 1;
            if (NH <= 1)
            {
                WORK[1 + o_work] = 1;
                return;
            }
            // *
            // *     Determine the block size
            // *
            NB = Math.Min(NBMAX, this._ilaenv.Run(1, "DGEHRD", " ", N, ILO, IHI,  - 1));
            NBMIN = 2;
            IWS = 1;
            if (NB > 1 && NB < NH)
            {
                // *
                // *        Determine when to cross over from blocked to unblocked code
                // *        (last block is always handled by unblocked code)
                // *
                NX = Math.Max(NB, this._ilaenv.Run(3, "DGEHRD", " ", N, ILO, IHI,  - 1));
                if (NX < NH)
                {
                    // *
                    // *           Determine if workspace is large enough for blocked code
                    // *
                    IWS = N * NB;
                    if (LWORK < IWS)
                    {
                        // *
                        // *              Not enough workspace to use optimal NB:  determine the
                        // *              minimum value of NB, and reduce NB or force use of
                        // *              unblocked code
                        // *
                        NBMIN = Math.Max(2, this._ilaenv.Run(2, "DGEHRD", " ", N, ILO, IHI,  - 1));
                        if (LWORK >= N * NBMIN)
                        {
                            NB = LWORK / N;
                        }
                        else
                        {
                            NB = 1;
                        }
                    }
                }
            }
            LDWORK = N;
            // *
            if (NB < NBMIN || NB >= NH)
            {
                // *
                // *        Use unblocked code below
                // *
                I = ILO;
                // *
            }
            else
            {
                // *
                // *        Use blocked code
                // *
                for (I = ILO; (NB >= 0) ? (I <= IHI - 1 - NX) : (I >= IHI - 1 - NX); I += NB)
                {
                    IB = Math.Min(NB, IHI - I);
                    // *
                    // *           Reduce columns i:i+ib-1 to Hessenberg form, returning the
                    // *           matrices V and T of the block reflector H = I - V*T*V'
                    // *           which performs the reduction, and also the matrix Y = A*V*T
                    // *
                    this._dlahr2.Run(IHI, I, IB, ref A, 1+I * LDA + o_a, LDA, ref TAU, I + o_tau
                                     , ref T, offset_t, LDT, ref WORK, offset_work, LDWORK);
                    // *
                    // *           Apply the block reflector H to A(1:ihi,i+ib:ihi) from the
                    // *           right, computing  A := A - Y * V'. V(i+ib,ib-1) must be set
                    // *           to 1
                    // *
                    EI = A[I + IB+(I + IB - 1) * LDA + o_a];
                    A[I + IB+(I + IB - 1) * LDA + o_a] = ONE;
                    this._dgemm.Run("No transpose", "Transpose", IHI, IHI - I - IB + 1, IB,  - ONE
                                    , WORK, offset_work, LDWORK, A, I + IB+I * LDA + o_a, LDA, ONE, ref A, 1+(I + IB) * LDA + o_a
                                    , LDA);
                    A[I + IB+(I + IB - 1) * LDA + o_a] = EI;
                    // *
                    // *           Apply the block reflector H to A(1:i,i+1:i+ib-1) from the
                    // *           right
                    // *
                    this._dtrmm.Run("Right", "Lower", "Transpose", "Unit", I, IB - 1
                                    , ONE, A, I + 1+I * LDA + o_a, LDA, ref WORK, offset_work, LDWORK);
                    for (J = 0; J <= IB - 2; J++)
                    {
                        this._daxpy.Run(I,  - ONE, WORK, LDWORK * J + 1 + o_work, 1, ref A, 1+(I + J + 1) * LDA + o_a, 1);
                    }
                    // *
                    // *           Apply the block reflector H to A(i+1:ihi,i+ib:n) from the
                    // *           left
                    // *
                    this._dlarfb.Run("Left", "Transpose", "Forward", "Columnwise", IHI - I, N - I - IB + 1
                                     , IB, A, I + 1+I * LDA + o_a, LDA, T, offset_t, LDT, ref A, I + 1+(I + IB) * LDA + o_a
                                     , LDA, ref WORK, offset_work, LDWORK);
                }
            }
            // *
            // *     Use unblocked code to reduce the rest of the matrix
            // *
            this._dgehd2.Run(N, I, IHI, ref A, offset_a, LDA, ref TAU, offset_tau
                             , ref WORK, offset_work, ref IINFO);
            WORK[1 + o_work] = IWS;
            // *
            return;
            // *
            // *     End of DGEHRD
            // *

            #endregion

        }
    }
}
