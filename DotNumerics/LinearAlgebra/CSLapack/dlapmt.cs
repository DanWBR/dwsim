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
    /// -- LAPACK auxiliary routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DLAPMT rearranges the columns of the M by N matrix X as specified
    /// by the permutation K(1),K(2),...,K(N) of the integers 1,...,N.
    /// If FORWRD = .TRUE.,  forward permutation:
    /// 
    /// X(*,K(J)) is moved X(*,J) for J = 1,2,...,N.
    /// 
    /// If FORWRD = .FALSE., backward permutation:
    /// 
    /// X(*,J) is moved to X(*,K(J)) for J = 1,2,...,N.
    /// 
    ///</summary>
    public class DLAPMT
    {
    
        public DLAPMT()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAPMT rearranges the columns of the M by N matrix X as specified
        /// by the permutation K(1),K(2),...,K(N) of the integers 1,...,N.
        /// If FORWRD = .TRUE.,  forward permutation:
        /// 
        /// X(*,K(J)) is moved X(*,J) for J = 1,2,...,N.
        /// 
        /// If FORWRD = .FALSE., backward permutation:
        /// 
        /// X(*,J) is moved to X(*,K(J)) for J = 1,2,...,N.
        /// 
        ///</summary>
        /// <param name="FORWRD">
        /// (input) LOGICAL
        /// = .TRUE., forward permutation
        /// = .FALSE., backward permutation
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix X. M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix X. N .GE. 0.
        ///</param>
        /// <param name="X">
        /// (input/output) DOUBLE PRECISION array, dimension (LDX,N)
        /// On entry, the M by N matrix X.
        /// On exit, X contains the permuted matrix X.
        ///</param>
        /// <param name="LDX">
        /// (input) INTEGER
        /// The leading dimension of the array X, LDX .GE. MAX(1,M).
        ///</param>
        /// <param name="K">
        /// (input/output) INTEGER array, dimension (N)
        /// On entry, K contains the permutation vector. K is used as
        /// internal workspace, but reset to its original value on
        /// output.
        ///</param>
        public void Run(bool FORWRD, int M, int N, ref double[] X, int offset_x, int LDX, ref int[] K, int offset_k)
        {

            #region Variables
            
            int I = 0; int II = 0; int IN = 0; int J = 0; double TEMP = 0; 

            #endregion


            #region Implicit Variables
            
            int X_J = 0; int X_IN = 0; int X_I = 0; 

            #endregion


            #region Array Index Correction
            
             int o_x = -1 - LDX + offset_x;  int o_k = -1 + offset_k; 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.1) --
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
            // *  DLAPMT rearranges the columns of the M by N matrix X as specified
            // *  by the permutation K(1),K(2),...,K(N) of the integers 1,...,N.
            // *  If FORWRD = .TRUE.,  forward permutation:
            // *
            // *       X(*,K(J)) is moved X(*,J) for J = 1,2,...,N.
            // *
            // *  If FORWRD = .FALSE., backward permutation:
            // *
            // *       X(*,J) is moved to X(*,K(J)) for J = 1,2,...,N.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  FORWRD  (input) LOGICAL
            // *          = .TRUE., forward permutation
            // *          = .FALSE., backward permutation
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix X. M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix X. N >= 0.
            // *
            // *  X       (input/output) DOUBLE PRECISION array, dimension (LDX,N)
            // *          On entry, the M by N matrix X.
            // *          On exit, X contains the permuted matrix X.
            // *
            // *  LDX     (input) INTEGER
            // *          The leading dimension of the array X, LDX >= MAX(1,M).
            // *
            // *  K       (input/output) INTEGER array, dimension (N)
            // *          On entry, K contains the permutation vector. K is used as
            // *          internal workspace, but reset to its original value on
            // *          output.
            // *
            // *  =====================================================================
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            if (N <= 1) return;
            // *
            for (I = 1; I <= N; I++)
            {
                K[I + o_k] =  - K[I + o_k];
            }
            // *
            if (FORWRD)
            {
                // *
                // *        Forward permutation
                // *
                for (I = 1; I <= N; I++)
                {
                    // *
                    if (K[I + o_k] > 0) goto LABEL40;
                    // *
                    J = I;
                    K[J + o_k] =  - K[J + o_k];
                    IN = K[J + o_k];
                    // *
                LABEL20:;
                    if (K[IN + o_k] > 0) goto LABEL40;
                    // *
                    X_J = J * LDX + o_x;
                    X_IN = IN * LDX + o_x;
                    for (II = 1; II <= M; II++)
                    {
                        TEMP = X[II + X_J];
                        X[II + X_J] = X[II + X_IN];
                        X[II + X_IN] = TEMP;
                    }
                    // *
                    K[IN + o_k] =  - K[IN + o_k];
                    J = IN;
                    IN = K[IN + o_k];
                    goto LABEL20;
                    // *
                LABEL40:;
                    // *
                }
                // *
            }
            else
            {
                // *
                // *        Backward permutation
                // *
                for (I = 1; I <= N; I++)
                {
                    // *
                    if (K[I + o_k] > 0) goto LABEL80;
                    // *
                    K[I + o_k] =  - K[I + o_k];
                    J = K[I + o_k];
                LABEL60:;
                    if (J == I) goto LABEL80;
                    // *
                    X_I = I * LDX + o_x;
                    X_J = J * LDX + o_x;
                    for (II = 1; II <= M; II++)
                    {
                        TEMP = X[II + X_I];
                        X[II + X_I] = X[II + X_J];
                        X[II + X_J] = TEMP;
                    }
                    // *
                    K[J + o_k] =  - K[J + o_k];
                    J = K[J + o_k];
                    goto LABEL60;
                    // *
                LABEL80:;
                    // *
                }
                // *
            }
            // *
            return;
            // *
            // *     End of DLAPMT
            // *

            #endregion

        }
    }
}
