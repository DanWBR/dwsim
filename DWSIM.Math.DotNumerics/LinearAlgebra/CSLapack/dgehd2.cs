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
    /// DGEHD2 reduces a real general matrix A to upper Hessenberg form H by
    /// an orthogonal similarity transformation:  Q' * A * Q = H .
    /// 
    ///</summary>
    public class DGEHD2
    {
    

        #region Dependencies
        
        DLARF _dlarf; DLARFG _dlarfg; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; 

        #endregion

        public DGEHD2(DLARF dlarf, DLARFG dlarfg, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dlarf = dlarf; this._dlarfg = dlarfg; this._xerbla = xerbla; 

            #endregion

        }
    
        public DGEHD2()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);

            #endregion


            #region Set Dependencies
            
            this._dlarf = dlarf; this._dlarfg = dlarfg; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGEHD2 reduces a real general matrix A to upper Hessenberg form H by
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
        /// 1 .LE. ILO .LE. IHI .LE. max(1,N).
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the n by n general matrix to be reduced.
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
        /// Details).
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (N)
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(int N, int ILO, int IHI, ref double[] A, int offset_a, int LDA, ref double[] TAU, int offset_tau
                         , ref double[] WORK, int offset_work, ref int INFO)
        {

            #region Variables
            
            int I = 0; double AII = 0; 

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
            // *  DGEHD2 reduces a real general matrix A to upper Hessenberg form H by
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
            // *          1 <= ILO <= IHI <= max(1,N).
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the n by n general matrix to be reduced.
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
            // *          Details).
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (N)
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit.
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
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters
            // *

            #endregion


            #region Body
            
            INFO = 0;
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
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGEHD2",  - INFO);
                return;
            }
            // *
            for (I = ILO; I <= IHI - 1; I++)
            {
                // *
                // *        Compute elementary reflector H(i) to annihilate A(i+2:ihi,i)
                // *
                this._dlarfg.Run(IHI - I, ref A[I + 1+I * LDA + o_a], ref A, Math.Min(I + 2, N)+I * LDA + o_a, 1, ref TAU[I + o_tau]);
                AII = A[I + 1+I * LDA + o_a];
                A[I + 1+I * LDA + o_a] = ONE;
                // *
                // *        Apply H(i) to A(1:ihi,i+1:ihi) from the right
                // *
                this._dlarf.Run("Right", IHI, IHI - I, A, I + 1+I * LDA + o_a, 1, TAU[I + o_tau]
                                , ref A, 1+(I + 1) * LDA + o_a, LDA, ref WORK, offset_work);
                // *
                // *        Apply H(i) to A(i+1:ihi,i+1:n) from the left
                // *
                this._dlarf.Run("Left", IHI - I, N - I, A, I + 1+I * LDA + o_a, 1, TAU[I + o_tau]
                                , ref A, I + 1+(I + 1) * LDA + o_a, LDA, ref WORK, offset_work);
                // *
                A[I + 1+I * LDA + o_a] = AII;
            }
            // *
            return;
            // *
            // *     End of DGEHD2
            // *

            #endregion

        }
    }
}
