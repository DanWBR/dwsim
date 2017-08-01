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
    /// -- LAPACK deprecated driver routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// This routine is deprecated and has been replaced by routine DGEQP3.
    /// 
    /// DGEQPF computes a QR factorization with column pivoting of a
    /// real M-by-N matrix A: A*P = Q*R.
    /// 
    ///</summary>
    public class DGEQPF
    {
    

        #region Dependencies
        
        DGEQR2 _dgeqr2; DLARF _dlarf; DLARFG _dlarfg; DORM2R _dorm2r; DSWAP _dswap; XERBLA _xerbla; IDAMAX _idamax; 
        DLAMCH _dlamch;DNRM2 _dnrm2; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 

        #endregion

        public DGEQPF(DGEQR2 dgeqr2, DLARF dlarf, DLARFG dlarfg, DORM2R dorm2r, DSWAP dswap, XERBLA xerbla, IDAMAX idamax, DLAMCH dlamch, DNRM2 dnrm2)
        {
    

            #region Set Dependencies
            
            this._dgeqr2 = dgeqr2; this._dlarf = dlarf; this._dlarfg = dlarfg; this._dorm2r = dorm2r; this._dswap = dswap; 
            this._xerbla = xerbla;this._idamax = idamax; this._dlamch = dlamch; this._dnrm2 = dnrm2; 

            #endregion

        }
    
        public DGEQPF()
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

            #endregion


            #region Set Dependencies
            
            this._dgeqr2 = dgeqr2; this._dlarf = dlarf; this._dlarfg = dlarfg; this._dorm2r = dorm2r; this._dswap = dswap; 
            this._xerbla = xerbla;this._idamax = idamax; this._dlamch = dlamch; this._dnrm2 = dnrm2; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// This routine is deprecated and has been replaced by routine DGEQP3.
        /// 
        /// DGEQPF computes a QR factorization with column pivoting of a
        /// real M-by-N matrix A: A*P = Q*R.
        /// 
        ///</summary>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix A. M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix A. N .GE. 0
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the M-by-N matrix A.
        /// On exit, the upper triangle of the array contains the
        /// min(M,N)-by-N upper triangular matrix R; the elements
        /// below the diagonal, together with the array TAU,
        /// represent the orthogonal matrix Q as a product of
        /// min(m,n) elementary reflectors.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A. LDA .GE. max(1,M).
        ///</param>
        /// <param name="JPVT">
        /// (input/output) INTEGER array, dimension (N)
        /// On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted
        /// to the front of A*P (a leading column); if JPVT(i) = 0,
        /// the i-th column of A is a free column.
        /// On exit, if JPVT(i) = k, then the i-th column of A*P
        /// was the k-th column of A.
        ///</param>
        /// <param name="TAU">
        /// (output) DOUBLE PRECISION array, dimension (min(M,N))
        /// The scalar factors of the elementary reflectors.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (3*N)
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
        ///</param>
        public void Run(int M, int N, ref double[] A, int offset_a, int LDA, ref int[] JPVT, int offset_jpvt, ref double[] TAU, int offset_tau
                         , ref double[] WORK, int offset_work, ref int INFO)
        {

            #region Variables
            
            int I = 0; int ITEMP = 0; int J = 0; int MA = 0; int MN = 0; int PVT = 0; double AII = 0; double TEMP = 0; 
            double TEMP2 = 0;double TOL3Z = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_jpvt = -1 + offset_jpvt;  int o_tau = -1 + offset_tau; 
             int o_work = -1 + offset_work;

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK deprecated driver routine (version 3.1) --
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
            // *  This routine is deprecated and has been replaced by routine DGEQP3.
            // *
            // *  DGEQPF computes a QR factorization with column pivoting of a
            // *  real M-by-N matrix A: A*P = Q*R.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix A. M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix A. N >= 0
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the M-by-N matrix A.
            // *          On exit, the upper triangle of the array contains the
            // *          min(M,N)-by-N upper triangular matrix R; the elements
            // *          below the diagonal, together with the array TAU,
            // *          represent the orthogonal matrix Q as a product of
            // *          min(m,n) elementary reflectors.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A. LDA >= max(1,M).
            // *
            // *  JPVT    (input/output) INTEGER array, dimension (N)
            // *          On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted
            // *          to the front of A*P (a leading column); if JPVT(i) = 0,
            // *          the i-th column of A is a free column.
            // *          On exit, if JPVT(i) = k, then the i-th column of A*P
            // *          was the k-th column of A.
            // *
            // *  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
            // *          The scalar factors of the elementary reflectors.
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (3*N)
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  The matrix Q is represented as a product of elementary reflectors
            // *
            // *     Q = H(1) H(2) . . . H(n)
            // *
            // *  Each H(i) has the form
            // *
            // *     H = I - tau * v * v'
            // *
            // *  where tau is a real scalar, and v is a real vector with
            // *  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i).
            // *
            // *  The matrix P is represented in jpvt as follows: If
            // *     jpvt(j) = i
            // *  then the jth column of P is the ith canonical unit vector.
            // *
            // *  Partial column norm updating strategy modified by
            // *    Z. Drmac and Z. Bujanovic, Dept. of Mathematics,
            // *    University of Zagreb, Croatia.
            // *    June 2006.
            // *  For more details see LAPACK Working Note 176.
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
            //      INTRINSIC          ABS, MAX, MIN, SQRT;
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
            if (INFO != 0)
            {
                this._xerbla.Run("DGEQPF",  - INFO);
                return;
            }
            // *
            MN = Math.Min(M, N);
            TOL3Z = Math.Sqrt(this._dlamch.Run("Epsilon"));
            // *
            // *     Move initial columns up front
            // *
            ITEMP = 1;
            for (I = 1; I <= N; I++)
            {
                if (JPVT[I + o_jpvt] != 0)
                {
                    if (I != ITEMP)
                    {
                        this._dswap.Run(M, ref A, 1+I * LDA + o_a, 1, ref A, 1+ITEMP * LDA + o_a, 1);
                        JPVT[I + o_jpvt] = JPVT[ITEMP + o_jpvt];
                        JPVT[ITEMP + o_jpvt] = I;
                    }
                    else
                    {
                        JPVT[I + o_jpvt] = I;
                    }
                    ITEMP += 1;
                }
                else
                {
                    JPVT[I + o_jpvt] = I;
                }
            }
            ITEMP -= 1;
            // *
            // *     Compute the QR factorization and update remaining columns
            // *
            if (ITEMP > 0)
            {
                MA = Math.Min(ITEMP, M);
                this._dgeqr2.Run(M, MA, ref A, offset_a, LDA, ref TAU, offset_tau, ref WORK, offset_work
                                 , ref INFO);
                if (MA < N)
                {
                    this._dorm2r.Run("Left", "Transpose", M, N - MA, MA, ref A, offset_a
                                     , LDA, TAU, offset_tau, ref A, 1+(MA + 1) * LDA + o_a, LDA, ref WORK, offset_work, ref INFO);
                }
            }
            // *
            if (ITEMP < MN)
            {
                // *
                // *        Initialize partial column norms. The first n elements of
                // *        work store the exact column norms.
                // *
                for (I = ITEMP + 1; I <= N; I++)
                {
                    WORK[I + o_work] = this._dnrm2.Run(M - ITEMP, A, ITEMP + 1+I * LDA + o_a, 1);
                    WORK[N + I + o_work] = WORK[I + o_work];
                }
                // *
                // *        Compute factorization
                // *
                for (I = ITEMP + 1; I <= MN; I++)
                {
                    // *
                    // *           Determine ith pivot column and swap if necessary
                    // *
                    PVT = (I - 1) + this._idamax.Run(N - I + 1, WORK, I + o_work, 1);
                    // *
                    if (PVT != I)
                    {
                        this._dswap.Run(M, ref A, 1+PVT * LDA + o_a, 1, ref A, 1+I * LDA + o_a, 1);
                        ITEMP = JPVT[PVT + o_jpvt];
                        JPVT[PVT + o_jpvt] = JPVT[I + o_jpvt];
                        JPVT[I + o_jpvt] = ITEMP;
                        WORK[PVT + o_work] = WORK[I + o_work];
                        WORK[N + PVT + o_work] = WORK[N + I + o_work];
                    }
                    // *
                    // *           Generate elementary reflector H(i)
                    // *
                    if (I < M)
                    {
                        this._dlarfg.Run(M - I + 1, ref A[I+I * LDA + o_a], ref A, I + 1+I * LDA + o_a, 1, ref TAU[I + o_tau]);
                    }
                    else
                    {
                        this._dlarfg.Run(1, ref A[M+M * LDA + o_a], ref A, M+M * LDA + o_a, 1, ref TAU[M + o_tau]);
                    }
                    // *
                    if (I < N)
                    {
                        // *
                        // *              Apply H(i) to A(i:m,i+1:n) from the left
                        // *
                        AII = A[I+I * LDA + o_a];
                        A[I+I * LDA + o_a] = ONE;
                        this._dlarf.Run("LEFT", M - I + 1, N - I, A, I+I * LDA + o_a, 1, TAU[I + o_tau]
                                        , ref A, I+(I + 1) * LDA + o_a, LDA, ref WORK, 2 * N + 1 + o_work);
                        A[I+I * LDA + o_a] = AII;
                    }
                    // *
                    // *           Update partial column norms
                    // *
                    for (J = I + 1; J <= N; J++)
                    {
                        if (WORK[J + o_work] != ZERO)
                        {
                            // *
                            // *                 NOTE: The following 4 lines follow from the analysis in
                            // *                 Lapack Working Note 176.
                            // *                 
                            TEMP = Math.Abs(A[I+J * LDA + o_a]) / WORK[J + o_work];
                            TEMP = Math.Max(ZERO, (ONE + TEMP) * (ONE - TEMP));
                            TEMP2 = TEMP * Math.Pow(WORK[J + o_work] / WORK[N + J + o_work],2);
                            if (TEMP2 <= TOL3Z)
                            {
                                if (M - I > 0)
                                {
                                    WORK[J + o_work] = this._dnrm2.Run(M - I, A, I + 1+J * LDA + o_a, 1);
                                    WORK[N + J + o_work] = WORK[J + o_work];
                                }
                                else
                                {
                                    WORK[J + o_work] = ZERO;
                                    WORK[N + J + o_work] = ZERO;
                                }
                            }
                            else
                            {
                                WORK[J + o_work] *= Math.Sqrt(TEMP);
                            }
                        }
                    }
                    // *
                }
            }
            return;
            // *
            // *     End of DGEQPF
            // *

            #endregion

        }
    }
}
