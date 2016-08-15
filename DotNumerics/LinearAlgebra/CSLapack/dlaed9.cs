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
    /// DLAED9 finds the roots of the secular equation, as defined by the
    /// values in D, Z, and RHO, between KSTART and KSTOP.  It makes the
    /// appropriate calls to DLAED4 and then stores the new matrix of
    /// eigenvectors for use in calculating the next level of Z vectors.
    /// 
    ///</summary>
    public class DLAED9
    {
    

        #region Dependencies
        
        DLAMC3 _dlamc3; DNRM2 _dnrm2; DCOPY _dcopy; DLAED4 _dlaed4; XERBLA _xerbla; 

        #endregion

        public DLAED9(DLAMC3 dlamc3, DNRM2 dnrm2, DCOPY dcopy, DLAED4 dlaed4, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dlamc3 = dlamc3; this._dnrm2 = dnrm2; this._dcopy = dcopy; this._dlaed4 = dlaed4; this._xerbla = xerbla; 

            #endregion

        }
    
        public DLAED9()
        {
    

            #region Dependencies (Initialization)
            
            DLAMC3 dlamc3 = new DLAMC3();
            DNRM2 dnrm2 = new DNRM2();
            DCOPY dcopy = new DCOPY();
            LSAME lsame = new LSAME();
            DLAED5 dlaed5 = new DLAED5();
            XERBLA xerbla = new XERBLA();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLAED6 dlaed6 = new DLAED6(dlamch);
            DLAED4 dlaed4 = new DLAED4(dlamch, dlaed5, dlaed6);

            #endregion


            #region Set Dependencies
            
            this._dlamc3 = dlamc3; this._dnrm2 = dnrm2; this._dcopy = dcopy; this._dlaed4 = dlaed4; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAED9 finds the roots of the secular equation, as defined by the
        /// values in D, Z, and RHO, between KSTART and KSTOP.  It makes the
        /// appropriate calls to DLAED4 and then stores the new matrix of
        /// eigenvectors for use in calculating the next level of Z vectors.
        /// 
        ///</summary>
        /// <param name="K">
        /// (input) INTEGER
        /// The number of terms in the rational function to be solved by
        /// DLAED4.  K .GE. 0.
        ///</param>
        /// <param name="KSTART">
        /// (input) INTEGER
        ///</param>
        /// <param name="KSTOP">
        /// (input) INTEGER
        /// The updated eigenvalues Lambda(I), KSTART .LE. I .LE. KSTOP
        /// are to be computed.  1 .LE. KSTART .LE. KSTOP .LE. K.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of rows and columns in the Q matrix.
        /// N .GE. K (delation may result in N .GT. K).
        ///</param>
        /// <param name="D">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// D(I) contains the updated eigenvalues
        /// for KSTART .LE. I .LE. KSTOP.
        ///</param>
        /// <param name="Q">
        /// (workspace) DOUBLE PRECISION array, dimension (LDQ,N)
        ///</param>
        /// <param name="LDQ">
        /// (input) INTEGER
        /// The leading dimension of the array Q.  LDQ .GE. max( 1, N ).
        ///</param>
        /// <param name="RHO">
        /// (input) DOUBLE PRECISION
        /// The value of the parameter in the rank one update equation.
        /// RHO .GE. 0 required.
        ///</param>
        /// <param name="DLAMDA">
        /// (input) DOUBLE PRECISION array, dimension (K)
        /// The first K elements of this array contain the old roots
        /// of the deflated updating problem.  These are the poles
        /// of the secular equation.
        ///</param>
        /// <param name="W">
        /// (input) DOUBLE PRECISION array, dimension (K)
        /// The first K elements of this array contain the components
        /// of the deflation-adjusted updating vector.
        ///</param>
        /// <param name="S">
        /// (output) DOUBLE PRECISION array, dimension (LDS, K)
        /// Will contain the eigenvectors of the repaired matrix which
        /// will be stored for subsequent Z vector calculation and
        /// multiplied by the previously accumulated eigenvectors
        /// to update the system.
        ///</param>
        /// <param name="LDS">
        /// (input) INTEGER
        /// The leading dimension of S.  LDS .GE. max( 1, K ).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// .GT. 0:  if INFO = 1, an eigenvalue did not converge
        ///</param>
        public void Run(int K, int KSTART, int KSTOP, int N, ref double[] D, int offset_d, ref double[] Q, int offset_q
                         , int LDQ, double RHO, ref double[] DLAMDA, int offset_dlamda, ref double[] W, int offset_w, ref double[] S, int offset_s, int LDS
                         , ref int INFO)
        {

            #region Variables
            
            int I = 0; int J = 0; double TEMP = 0; 

            #endregion


            #region Implicit Variables
            
            int S_I = 0; int Q_I = 0; int Q_J = 0; int S_J = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_q = -1 - LDQ + offset_q;  int o_dlamda = -1 + offset_dlamda; 
             int o_w = -1 + offset_w; int o_s = -1 - LDS + offset_s; 

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
            // *  DLAED9 finds the roots of the secular equation, as defined by the
            // *  values in D, Z, and RHO, between KSTART and KSTOP.  It makes the
            // *  appropriate calls to DLAED4 and then stores the new matrix of
            // *  eigenvectors for use in calculating the next level of Z vectors.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  K       (input) INTEGER
            // *          The number of terms in the rational function to be solved by
            // *          DLAED4.  K >= 0.
            // *
            // *  KSTART  (input) INTEGER
            // *  KSTOP   (input) INTEGER
            // *          The updated eigenvalues Lambda(I), KSTART <= I <= KSTOP
            // *          are to be computed.  1 <= KSTART <= KSTOP <= K.
            // *
            // *  N       (input) INTEGER
            // *          The number of rows and columns in the Q matrix.
            // *          N >= K (delation may result in N > K).
            // *
            // *  D       (output) DOUBLE PRECISION array, dimension (N)
            // *          D(I) contains the updated eigenvalues
            // *          for KSTART <= I <= KSTOP.
            // *
            // *  Q       (workspace) DOUBLE PRECISION array, dimension (LDQ,N)
            // *
            // *  LDQ     (input) INTEGER
            // *          The leading dimension of the array Q.  LDQ >= max( 1, N ).
            // *
            // *  RHO     (input) DOUBLE PRECISION
            // *          The value of the parameter in the rank one update equation.
            // *          RHO >= 0 required.
            // *
            // *  DLAMDA  (input) DOUBLE PRECISION array, dimension (K)
            // *          The first K elements of this array contain the old roots
            // *          of the deflated updating problem.  These are the poles
            // *          of the secular equation.
            // *
            // *  W       (input) DOUBLE PRECISION array, dimension (K)
            // *          The first K elements of this array contain the components
            // *          of the deflation-adjusted updating vector.
            // *
            // *  S       (output) DOUBLE PRECISION array, dimension (LDS, K)
            // *          Will contain the eigenvectors of the repaired matrix which
            // *          will be stored for subsequent Z vector calculation and
            // *          multiplied by the previously accumulated eigenvectors
            // *          to update the system.
            // *
            // *  LDS     (input) INTEGER
            // *          The leading dimension of S.  LDS >= max( 1, K ).
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit.
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *          > 0:  if INFO = 1, an eigenvalue did not converge
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *     Jeff Rutter, Computer Science Division, University of California
            // *     at Berkeley, USA
            // *
            // *  =====================================================================
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MAX, SIGN, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            // *
            if (K < 0)
            {
                INFO =  - 1;
            }
            else
            {
                if (KSTART < 1 || KSTART > Math.Max(1, K))
                {
                    INFO =  - 2;
                }
                else
                {
                    if (Math.Max(1, KSTOP) < KSTART || KSTOP > Math.Max(1, K))
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (N < K)
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (LDQ < Math.Max(1, K))
                            {
                                INFO =  - 7;
                            }
                            else
                            {
                                if (LDS < Math.Max(1, K))
                                {
                                    INFO =  - 12;
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLAED9",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (K == 0) return;
            // *
            // *     Modify values DLAMDA(i) to make sure all DLAMDA(i)-DLAMDA(j) can
            // *     be computed with high relative accuracy (barring over/underflow).
            // *     This is a problem on machines without a guard digit in
            // *     add/subtract (Cray XMP, Cray YMP, Cray C 90 and Cray 2).
            // *     The following code replaces DLAMDA(I) by 2*DLAMDA(I)-DLAMDA(I),
            // *     which on any of these machines zeros out the bottommost
            // *     bit of DLAMDA(I) if it is 1; this makes the subsequent
            // *     subtractions DLAMDA(I)-DLAMDA(J) unproblematic when cancellation
            // *     occurs. On binary machines with a guard digit (almost all
            // *     machines) it does not change DLAMDA(I) at all. On hexadecimal
            // *     and decimal machines with a guard digit, it slightly
            // *     changes the bottommost bits of DLAMDA(I). It does not account
            // *     for hexadecimal or decimal machines without guard digits
            // *     (we know of none). We use a subroutine call to compute
            // *     2*DLAMBDA(I) to prevent optimizing compilers from eliminating
            // *     this code.
            // *
            for (I = 1; I <= N; I++)
            {
                DLAMDA[I + o_dlamda] = this._dlamc3.Run(DLAMDA[I + o_dlamda], DLAMDA[I + o_dlamda]) - DLAMDA[I + o_dlamda];
            }
            // *
            for (J = KSTART; J <= KSTOP; J++)
            {
                this._dlaed4.Run(K, J, DLAMDA, offset_dlamda, W, offset_w, ref Q, 1+J * LDQ + o_q, RHO
                                 , ref D[J + o_d], ref INFO);
                // *
                // *        If the zero finder fails, the computation is terminated.
                // *
                if (INFO != 0) goto LABEL120;
            }
            // *
            if (K == 1 || K == 2)
            {
                for (I = 1; I <= K; I++)
                {
                    S_I = I * LDS + o_s;
                    Q_I = I * LDQ + o_q;
                    for (J = 1; J <= K; J++)
                    {
                        S[J + S_I] = Q[J + Q_I];
                    }
                }
                goto LABEL120;
            }
            // *
            // *     Compute updated W.
            // *
            this._dcopy.Run(K, W, offset_w, 1, ref S, offset_s, 1);
            // *
            // *     Initialize W(I) = Q(I,I)
            // *
            this._dcopy.Run(K, Q, offset_q, LDQ + 1, ref W, offset_w, 1);
            for (J = 1; J <= K; J++)
            {
                Q_J = J * LDQ + o_q;
                for (I = 1; I <= J - 1; I++)
                {
                    W[I + o_w] = W[I + o_w] * (Q[I + Q_J] / (DLAMDA[I + o_dlamda] - DLAMDA[J + o_dlamda]));
                }
                Q_J = J * LDQ + o_q;
                for (I = J + 1; I <= K; I++)
                {
                    W[I + o_w] = W[I + o_w] * (Q[I + Q_J] / (DLAMDA[I + o_dlamda] - DLAMDA[J + o_dlamda]));
                }
            }
            for (I = 1; I <= K; I++)
            {
                W[I + o_w] = FortranLib.Sign(Math.Sqrt( - W[I + o_w]),S[I+1 * LDS + o_s]);
            }
            // *
            // *     Compute eigenvectors of the modified rank-1 modification.
            // *
            for (J = 1; J <= K; J++)
            {
                Q_J = J * LDQ + o_q;
                for (I = 1; I <= K; I++)
                {
                    Q[I + Q_J] = W[I + o_w] / Q[I + Q_J];
                }
                TEMP = this._dnrm2.Run(K, Q, 1+J * LDQ + o_q, 1);
                S_J = J * LDS + o_s;
                Q_J = J * LDQ + o_q;
                for (I = 1; I <= K; I++)
                {
                    S[I + S_J] = Q[I + Q_J] / TEMP;
                }
            }
            // *
        LABEL120:;
            return;
            // *
            // *     End of DLAED9
            // *

            #endregion

        }
    }
}
