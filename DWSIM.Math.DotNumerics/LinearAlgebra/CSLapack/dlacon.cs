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
    /// -- LAPACK auxiliary routine (version 3.0) --
    /// Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
    /// Courant Institute, Argonne National Lab, and Rice University
    /// February 29, 1992
    /// Purpose
    /// =======
    /// 
    /// DLACON estimates the 1-norm of a square, real matrix A.
    /// Reverse communication is used for evaluating matrix-vector products.
    /// 
    ///</summary>
    public class DLACON
    {
    

        #region Dependencies
        
        IDAMAX _idamax; DASUM _dasum; DCOPY _dcopy; 

        #endregion


        #region Variables
        
        const int ITMAX = 5; const double ZERO = 0.0E+0; const double ONE = 1.0E+0; const double TWO = 2.0E+0; int I = 0; 
        int ITER = 0;int J = 0; int JLAST = 0; int JUMP = 0; double ALTSGN = 0; double ESTOLD = 0; double TEMP = 0; 

        #endregion

        public DLACON(IDAMAX idamax, DASUM dasum, DCOPY dcopy)
        {
    

            #region Set Dependencies
            
            this._idamax = idamax; this._dasum = dasum; this._dcopy = dcopy; 

            #endregion

        }
    
        public DLACON()
        {
    

            #region Dependencies (Initialization)
            
            IDAMAX idamax = new IDAMAX();
            DASUM dasum = new DASUM();
            DCOPY dcopy = new DCOPY();

            #endregion


            #region Set Dependencies
            
            this._idamax = idamax; this._dasum = dasum; this._dcopy = dcopy; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLACON estimates the 1-norm of a square, real matrix A.
        /// Reverse communication is used for evaluating matrix-vector products.
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix.  N .GE. 1.
        ///</param>
        /// <param name="V">
        /// (workspace) DOUBLE PRECISION array, dimension (N)
        /// On the final return, V = A*W,  where  EST = norm(V)/norm(W)
        /// (W is not returned).
        ///</param>
        /// <param name="X">
        /// (input/output) DOUBLE PRECISION array, dimension (N)
        /// On an intermediate return, X should be overwritten by
        /// A * X,   if KASE=1,
        /// A' * X,  if KASE=2,
        /// and DLACON must be re-called with all the other parameters
        /// unchanged.
        ///</param>
        /// <param name="ISGN">
        /// (workspace) INTEGER array, dimension (N)
        ///</param>
        /// <param name="EST">
        /// (output) DOUBLE PRECISION
        /// An estimate (a lower bound) for norm(A).
        ///</param>
        /// <param name="KASE">
        /// (input/output) INTEGER
        /// On the initial call to DLACON, KASE should be 0.
        /// On an intermediate return, KASE will be 1 or 2, indicating
        /// whether X should be overwritten by A * X  or A' * X.
        /// On the final return from DLACON, KASE will again be 0.
        ///</param>
        public void Run(int N, ref double[] V, int offset_v, ref double[] X, int offset_x, ref int[] ISGN, int offset_isgn, ref double EST, ref int KASE)
        {

            #region Array Index Correction
            
             int o_v = -1 + offset_v;  int o_x = -1 + offset_x;  int o_isgn = -1 + offset_isgn; 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.0) --
            // *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
            // *     Courant Institute, Argonne National Lab, and Rice University
            // *     February 29, 1992
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLACON estimates the 1-norm of a square, real matrix A.
            // *  Reverse communication is used for evaluating matrix-vector products.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N      (input) INTEGER
            // *         The order of the matrix.  N >= 1.
            // *
            // *  V      (workspace) DOUBLE PRECISION array, dimension (N)
            // *         On the final return, V = A*W,  where  EST = norm(V)/norm(W)
            // *         (W is not returned).
            // *
            // *  X      (input/output) DOUBLE PRECISION array, dimension (N)
            // *         On an intermediate return, X should be overwritten by
            // *               A * X,   if KASE=1,
            // *               A' * X,  if KASE=2,
            // *         and DLACON must be re-called with all the other parameters
            // *         unchanged.
            // *
            // *  ISGN   (workspace) INTEGER array, dimension (N)
            // *
            // *  EST    (output) DOUBLE PRECISION
            // *         An estimate (a lower bound) for norm(A).
            // *
            // *  KASE   (input/output) INTEGER
            // *         On the initial call to DLACON, KASE should be 0.
            // *         On an intermediate return, KASE will be 1 or 2, indicating
            // *         whether X should be overwritten by A * X  or A' * X.
            // *         On the final return from DLACON, KASE will again be 0.
            // *
            // *  Further Details
            // *  ======= =======
            // *
            // *  Contributed by Nick Higham, University of Manchester.
            // *  Originally named SONEST, dated March 16, 1988.
            // *
            // *  Reference: N.J. Higham, "FORTRAN codes for estimating the one-norm of
            // *  a real or complex matrix, with applications to condition estimation",
            // *  ACM Trans. Math. Soft., vol. 14, no. 4, pp. 381-396, December 1988.
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
            //      INTRINSIC          ABS, DBLE, NINT, SIGN;
            // *     ..
            // *     .. Save statement ..
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            if (KASE == 0)
            {
                for (I = 1; I <= N; I++)
                {
                    X[I + o_x] = ONE / Convert.ToDouble(N);
                }
                KASE = 1;
                JUMP = 1;
                return;
            }
            // *
            switch (JUMP)
            {
                case 1: goto LABEL20;
                case 2: goto LABEL40;
                case 3: goto LABEL70;
                case 4: goto LABEL110;
                case 5: goto LABEL140;
            }
            // *
            // *     ................ ENTRY   (JUMP = 1)
            // *     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY A*X.
            // *
        LABEL20:;
            if (N == 1)
            {
                V[1 + o_v] = X[1 + o_x];
                EST = Math.Abs(V[1 + o_v]);
                // *        ... QUIT
                goto LABEL150;
            }
            EST = this._dasum.Run(N, X, offset_x, 1);
            // *
            for (I = 1; I <= N; I++)
            {
                X[I + o_x] = FortranLib.Sign(ONE,X[I + o_x]);
                ISGN[I + o_isgn] = (int)Math.Round(X[I + o_x]);
            }
            KASE = 2;
            JUMP = 2;
            return;
            // *
            // *     ................ ENTRY   (JUMP = 2)
            // *     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY TRANDPOSE(A)*X.
            // *
        LABEL40:;
            J = this._idamax.Run(N, X, offset_x, 1);
            ITER = 2;
            // *
            // *     MAIN LOOP - ITERATIONS 2,3,...,ITMAX.
            // *
        LABEL50:;
            for (I = 1; I <= N; I++)
            {
                X[I + o_x] = ZERO;
            }
            X[J + o_x] = ONE;
            KASE = 1;
            JUMP = 3;
            return;
            // *
            // *     ................ ENTRY   (JUMP = 3)
            // *     X HAS BEEN OVERWRITTEN BY A*X.
            // *
        LABEL70:;
            this._dcopy.Run(N, X, offset_x, 1, ref V, offset_v, 1);
            ESTOLD = EST;
            EST = this._dasum.Run(N, V, offset_v, 1);
            for (I = 1; I <= N; I++)
            {
                if (Math.Round(FortranLib.Sign(ONE,X[I + o_x])) != ISGN[I + o_isgn]) goto LABEL90;
            }
            // *     REPEATED SIGN VECTOR DETECTED, HENCE ALGORITHM HAS CONVERGED.
            goto LABEL120;
            // *
        LABEL90:;
            // *     TEST FOR CYCLING.
            if (EST <= ESTOLD) goto LABEL120;
            // *
            for (I = 1; I <= N; I++)
            {
                X[I + o_x] = FortranLib.Sign(ONE,X[I + o_x]);
                ISGN[I + o_isgn] = (int)Math.Round(X[I + o_x]);
            }
            KASE = 2;
            JUMP = 4;
            return;
            // *
            // *     ................ ENTRY   (JUMP = 4)
            // *     X HAS BEEN OVERWRITTEN BY TRANDPOSE(A)*X.
            // *
        LABEL110:;
            JLAST = J;
            J = this._idamax.Run(N, X, offset_x, 1);
            if ((X[JLAST + o_x] != Math.Abs(X[J + o_x])) && (ITER < ITMAX))
            {
                ITER += 1;
                goto LABEL50;
            }
            // *
            // *     ITERATION COMPLETE.  FINAL STAGE.
            // *
        LABEL120:;
            ALTSGN = ONE;
            for (I = 1; I <= N; I++)
            {
                X[I + o_x] = ALTSGN * (ONE + Convert.ToDouble(I - 1) / Convert.ToDouble(N - 1));
                ALTSGN =  - ALTSGN;
            }
            KASE = 1;
            JUMP = 5;
            return;
            // *
            // *     ................ ENTRY   (JUMP = 5)
            // *     X HAS BEEN OVERWRITTEN BY A*X.
            // *
        LABEL140:;
            TEMP = TWO * (this._dasum.Run(N, X, offset_x, 1) / Convert.ToDouble(3 * N));
            if (TEMP > EST)
            {
                this._dcopy.Run(N, X, offset_x, 1, ref V, offset_v, 1);
                EST = TEMP;
            }
            // *
        LABEL150:;
            KASE = 0;
            return;
            // *
            // *     End of DLACON
            // *

            #endregion

        }
    }
}
