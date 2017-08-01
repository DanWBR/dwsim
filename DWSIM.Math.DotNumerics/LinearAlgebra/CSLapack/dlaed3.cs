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
    /// DLAED3 finds the roots of the secular equation, as defined by the
    /// values in D, W, and RHO, between 1 and K.  It makes the
    /// appropriate calls to DLAED4 and then updates the eigenvectors by
    /// multiplying the matrix of eigenvectors of the pair of eigensystems
    /// being combined by the matrix of eigenvectors of the K-by-K system
    /// which is solved here.
    /// 
    /// This code makes very mild assumptions about floating point
    /// arithmetic. It will work on machines with a guard digit in
    /// add/subtract, or on those binary machines without guard digits
    /// which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
    /// It could conceivably fail on hexadecimal or decimal machines
    /// without guard digits, but we know of none.
    /// 
    ///</summary>
    public class DLAED3
    {
    

        #region Dependencies
        
        DLAMC3 _dlamc3; DNRM2 _dnrm2; DCOPY _dcopy; DGEMM _dgemm; DLACPY _dlacpy; DLAED4 _dlaed4; DLASET _dlaset; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E0; const double ZERO = 0.0E0; 

        #endregion

        public DLAED3(DLAMC3 dlamc3, DNRM2 dnrm2, DCOPY dcopy, DGEMM dgemm, DLACPY dlacpy, DLAED4 dlaed4, DLASET dlaset, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dlamc3 = dlamc3; this._dnrm2 = dnrm2; this._dcopy = dcopy; this._dgemm = dgemm; this._dlacpy = dlacpy; 
            this._dlaed4 = dlaed4;this._dlaset = dlaset; this._xerbla = xerbla; 

            #endregion

        }
    
        public DLAED3()
        {
    

            #region Dependencies (Initialization)
            
            DLAMC3 dlamc3 = new DLAMC3();
            DNRM2 dnrm2 = new DNRM2();
            DCOPY dcopy = new DCOPY();
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DLAED5 dlaed5 = new DLAED5();
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DLACPY dlacpy = new DLACPY(lsame);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLAED6 dlaed6 = new DLAED6(dlamch);
            DLAED4 dlaed4 = new DLAED4(dlamch, dlaed5, dlaed6);
            DLASET dlaset = new DLASET(lsame);

            #endregion


            #region Set Dependencies
            
            this._dlamc3 = dlamc3; this._dnrm2 = dnrm2; this._dcopy = dcopy; this._dgemm = dgemm; this._dlacpy = dlacpy; 
            this._dlaed4 = dlaed4;this._dlaset = dlaset; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAED3 finds the roots of the secular equation, as defined by the
        /// values in D, W, and RHO, between 1 and K.  It makes the
        /// appropriate calls to DLAED4 and then updates the eigenvectors by
        /// multiplying the matrix of eigenvectors of the pair of eigensystems
        /// being combined by the matrix of eigenvectors of the K-by-K system
        /// which is solved here.
        /// 
        /// This code makes very mild assumptions about floating point
        /// arithmetic. It will work on machines with a guard digit in
        /// add/subtract, or on those binary machines without guard digits
        /// which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
        /// It could conceivably fail on hexadecimal or decimal machines
        /// without guard digits, but we know of none.
        /// 
        ///</summary>
        /// <param name="K">
        /// (input) INTEGER
        /// The number of terms in the rational function to be solved by
        /// DLAED4.  K .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of rows and columns in the Q matrix.
        /// N .GE. K (deflation may result in N.GT.K).
        ///</param>
        /// <param name="N1">
        /// (input) INTEGER
        /// The location of the last eigenvalue in the leading submatrix.
        /// min(1,N) .LE. N1 .LE. N/2.
        ///</param>
        /// <param name="D">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// D(I) contains the updated eigenvalues for
        /// 1 .LE. I .LE. K.
        ///</param>
        /// <param name="Q">
        /// (output) DOUBLE PRECISION array, dimension (LDQ,N)
        /// Initially the first K columns are used as workspace.
        /// On output the columns 1 to K contain
        /// the updated eigenvectors.
        ///</param>
        /// <param name="LDQ">
        /// (input) INTEGER
        /// The leading dimension of the array Q.  LDQ .GE. max(1,N).
        ///</param>
        /// <param name="RHO">
        /// (input) DOUBLE PRECISION
        /// The value of the parameter in the rank one update equation.
        /// RHO .GE. 0 required.
        ///</param>
        /// <param name="DLAMDA">
        /// (input/output) DOUBLE PRECISION array, dimension (K)
        /// The first K elements of this array contain the old roots
        /// of the deflated updating problem.  These are the poles
        /// of the secular equation. May be changed on output by
        /// having lowest order bit set to zero on Cray X-MP, Cray Y-MP,
        /// Cray-2, or Cray C-90, as described above.
        ///</param>
        /// <param name="Q2">
        /// (input) DOUBLE PRECISION array, dimension (LDQ2, N)
        /// The first K columns of this matrix contain the non-deflated
        /// eigenvectors for the split problem.
        ///</param>
        /// <param name="INDX">
        /// (input) INTEGER array, dimension (N)
        /// The permutation used to arrange the columns of the deflated
        /// Q matrix into three groups (see DLAED2).
        /// The rows of the eigenvectors found by DLAED4 must be likewise
        /// permuted before the matrix multiply can take place.
        ///</param>
        /// <param name="CTOT">
        /// (input) INTEGER array, dimension (4)
        /// A count of the total number of the various types of columns
        /// in Q, as described in INDX.  The fourth column type is any
        /// column which has been deflated.
        ///</param>
        /// <param name="W">
        /// (input/output) DOUBLE PRECISION array, dimension (K)
        /// The first K elements of this array contain the components
        /// of the deflation-adjusted updating vector. Destroyed on
        /// output.
        ///</param>
        /// <param name="S">
        /// (workspace) DOUBLE PRECISION array, dimension (N1 + 1)*K
        /// Will contain the eigenvectors of the repaired matrix which
        /// will be multiplied by the previously accumulated eigenvectors
        /// to update the system.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// .GT. 0:  if INFO = 1, an eigenvalue did not converge
        ///</param>
        public void Run(int K, int N, int N1, ref double[] D, int offset_d, ref double[] Q, int offset_q, int LDQ
                         , double RHO, ref double[] DLAMDA, int offset_dlamda, double[] Q2, int offset_q2, int[] INDX, int offset_indx, int[] CTOT, int offset_ctot, ref double[] W, int offset_w
                         , ref double[] S, int offset_s, ref int INFO)
        {

            #region Variables
            
            int I = 0; int II = 0; int IQ2 = 0; int J = 0; int N12 = 0; int N2 = 0; int N23 = 0; double TEMP = 0; 

            #endregion


            #region Implicit Variables
            
            int Q_J = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_q = -1 - LDQ + offset_q;  int o_dlamda = -1 + offset_dlamda; 
             int o_q2 = -1 + offset_q2; int o_indx = -1 + offset_indx;  int o_ctot = -1 + offset_ctot;  int o_w = -1 + offset_w; 
             int o_s = -1 + offset_s;

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
            // *  DLAED3 finds the roots of the secular equation, as defined by the
            // *  values in D, W, and RHO, between 1 and K.  It makes the
            // *  appropriate calls to DLAED4 and then updates the eigenvectors by
            // *  multiplying the matrix of eigenvectors of the pair of eigensystems
            // *  being combined by the matrix of eigenvectors of the K-by-K system
            // *  which is solved here.
            // *
            // *  This code makes very mild assumptions about floating point
            // *  arithmetic. It will work on machines with a guard digit in
            // *  add/subtract, or on those binary machines without guard digits
            // *  which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
            // *  It could conceivably fail on hexadecimal or decimal machines
            // *  without guard digits, but we know of none.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  K       (input) INTEGER
            // *          The number of terms in the rational function to be solved by
            // *          DLAED4.  K >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of rows and columns in the Q matrix.
            // *          N >= K (deflation may result in N>K).
            // *
            // *  N1      (input) INTEGER
            // *          The location of the last eigenvalue in the leading submatrix.
            // *          min(1,N) <= N1 <= N/2.
            // *
            // *  D       (output) DOUBLE PRECISION array, dimension (N)
            // *          D(I) contains the updated eigenvalues for
            // *          1 <= I <= K.
            // *
            // *  Q       (output) DOUBLE PRECISION array, dimension (LDQ,N)
            // *          Initially the first K columns are used as workspace.
            // *          On output the columns 1 to K contain
            // *          the updated eigenvectors.
            // *
            // *  LDQ     (input) INTEGER
            // *          The leading dimension of the array Q.  LDQ >= max(1,N).
            // *
            // *  RHO     (input) DOUBLE PRECISION
            // *          The value of the parameter in the rank one update equation.
            // *          RHO >= 0 required.
            // *
            // *  DLAMDA  (input/output) DOUBLE PRECISION array, dimension (K)
            // *          The first K elements of this array contain the old roots
            // *          of the deflated updating problem.  These are the poles
            // *          of the secular equation. May be changed on output by
            // *          having lowest order bit set to zero on Cray X-MP, Cray Y-MP,
            // *          Cray-2, or Cray C-90, as described above.
            // *
            // *  Q2      (input) DOUBLE PRECISION array, dimension (LDQ2, N)
            // *          The first K columns of this matrix contain the non-deflated
            // *          eigenvectors for the split problem.
            // *
            // *  INDX    (input) INTEGER array, dimension (N)
            // *          The permutation used to arrange the columns of the deflated
            // *          Q matrix into three groups (see DLAED2).
            // *          The rows of the eigenvectors found by DLAED4 must be likewise
            // *          permuted before the matrix multiply can take place.
            // *
            // *  CTOT    (input) INTEGER array, dimension (4)
            // *          A count of the total number of the various types of columns
            // *          in Q, as described in INDX.  The fourth column type is any
            // *          column which has been deflated.
            // *
            // *  W       (input/output) DOUBLE PRECISION array, dimension (K)
            // *          The first K elements of this array contain the components
            // *          of the deflation-adjusted updating vector. Destroyed on
            // *          output.
            // *
            // *  S       (workspace) DOUBLE PRECISION array, dimension (N1 + 1)*K
            // *          Will contain the eigenvectors of the repaired matrix which
            // *          will be multiplied by the previously accumulated eigenvectors
            // *          to update the system.
            // *
            // *  LDS     (input) INTEGER
            // *          The leading dimension of S.  LDS >= max(1,K).
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
            // *  Modified by Francoise Tisseur, University of Tennessee.
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
                if (N < K)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (LDQ < Math.Max(1, N))
                    {
                        INFO =  - 6;
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLAED3",  - INFO);
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
            for (I = 1; I <= K; I++)
            {
                DLAMDA[I + o_dlamda] = this._dlamc3.Run(DLAMDA[I + o_dlamda], DLAMDA[I + o_dlamda]) - DLAMDA[I + o_dlamda];
            }
            // *
            for (J = 1; J <= K; J++)
            {
                this._dlaed4.Run(K, J, DLAMDA, offset_dlamda, W, offset_w, ref Q, 1+J * LDQ + o_q, RHO
                                 , ref D[J + o_d], ref INFO);
                // *
                // *        If the zero finder fails, the computation is terminated.
                // *
                if (INFO != 0) goto LABEL120;
            }
            // *
            if (K == 1) goto LABEL110;
            if (K == 2)
            {
                for (J = 1; J <= K; J++)
                {
                    W[1 + o_w] = Q[1+J * LDQ + o_q];
                    W[2 + o_w] = Q[2+J * LDQ + o_q];
                    II = INDX[1 + o_indx];
                    Q[1+J * LDQ + o_q] = W[II + o_w];
                    II = INDX[2 + o_indx];
                    Q[2+J * LDQ + o_q] = W[II + o_w];
                }
                goto LABEL110;
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
                W[I + o_w] = FortranLib.Sign(Math.Sqrt( - W[I + o_w]),S[I + o_s]);
            }
            // *
            // *     Compute eigenvectors of the modified rank-1 modification.
            // *
            for (J = 1; J <= K; J++)
            {
                Q_J = J * LDQ + o_q;
                for (I = 1; I <= K; I++)
                {
                    S[I + o_s] = W[I + o_w] / Q[I + Q_J];
                }
                TEMP = this._dnrm2.Run(K, S, offset_s, 1);
                Q_J = J * LDQ + o_q;
                for (I = 1; I <= K; I++)
                {
                    II = INDX[I + o_indx];
                    Q[I + Q_J] = S[II + o_s] / TEMP;
                }
            }
            // *
            // *     Compute the updated eigenvectors.
            // *
        LABEL110:;
            // *
            N2 = N - N1;
            N12 = CTOT[1 + o_ctot] + CTOT[2 + o_ctot];
            N23 = CTOT[2 + o_ctot] + CTOT[3 + o_ctot];
            // *
            this._dlacpy.Run("A", N23, K, Q, CTOT[1 + o_ctot] + 1+1 * LDQ + o_q, LDQ, ref S, offset_s
                             , N23);
            IQ2 = N1 * N12 + 1;
            if (N23 != 0)
            {
                this._dgemm.Run("N", "N", N2, K, N23, ONE
                                , Q2, IQ2 + o_q2, N2, S, offset_s, N23, ZERO, ref Q, N1 + 1+1 * LDQ + o_q
                                , LDQ);
            }
            else
            {
                this._dlaset.Run("A", N2, K, ZERO, ZERO, ref Q, N1 + 1+1 * LDQ + o_q
                                 , LDQ);
            }
            // *
            this._dlacpy.Run("A", N12, K, Q, offset_q, LDQ, ref S, offset_s
                             , N12);
            if (N12 != 0)
            {
                this._dgemm.Run("N", "N", N1, K, N12, ONE
                                , Q2, offset_q2, N1, S, offset_s, N12, ZERO, ref Q, offset_q
                                , LDQ);
            }
            else
            {
                this._dlaset.Run("A", N1, K, ZERO, ZERO, ref Q, 1+1 * LDQ + o_q
                                 , LDQ);
            }
            // *
            // *
        LABEL120:;
            return;
            // *
            // *     End of DLAED3
            // *

            #endregion

        }
    }
}
