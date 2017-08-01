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
    /// DGEBAK forms the right or left eigenvectors of a real general matrix
    /// by backward transformation on the computed eigenvectors of the
    /// balanced matrix output by DGEBAL.
    /// 
    ///</summary>
    public class DGEBAK
    {
    

        #region Dependencies
        
        LSAME _lsame; DSCAL _dscal; DSWAP _dswap; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; 

        #endregion

        public DGEBAK(LSAME lsame, DSCAL dscal, DSWAP dswap, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dscal = dscal; this._dswap = dswap; this._xerbla = xerbla; 

            #endregion

        }
    
        public DGEBAK()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DSCAL dscal = new DSCAL();
            DSWAP dswap = new DSWAP();
            XERBLA xerbla = new XERBLA();

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dscal = dscal; this._dswap = dswap; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGEBAK forms the right or left eigenvectors of a real general matrix
        /// by backward transformation on the computed eigenvectors of the
        /// balanced matrix output by DGEBAL.
        /// 
        ///</summary>
        /// <param name="JOB">
        /// (input) CHARACTER*1
        /// Specifies the type of backward transformation required:
        /// = 'N', do nothing, return immediately;
        /// = 'P', do backward transformation for permutation only;
        /// = 'S', do backward transformation for scaling only;
        /// = 'B', do backward transformations for both permutation and
        /// scaling.
        /// JOB must be the same as the argument JOB supplied to DGEBAL.
        ///</param>
        /// <param name="SIDE">
        /// (input) CHARACTER*1
        /// = 'R':  V contains right eigenvectors;
        /// = 'L':  V contains left eigenvectors.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of rows of the matrix V.  N .GE. 0.
        ///</param>
        /// <param name="ILO">
        /// (input) INTEGER
        ///</param>
        /// <param name="IHI">
        /// (input) INTEGER
        /// The integers ILO and IHI determined by DGEBAL.
        /// 1 .LE. ILO .LE. IHI .LE. N, if N .GT. 0; ILO=1 and IHI=0, if N=0.
        ///</param>
        /// <param name="SCALE">
        /// (input) DOUBLE PRECISION array, dimension (N)
        /// Details of the permutation and scaling factors, as returned
        /// by DGEBAL.
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of columns of the matrix V.  M .GE. 0.
        ///</param>
        /// <param name="V">
        /// (input/output) DOUBLE PRECISION array, dimension (LDV,M)
        /// On entry, the matrix of right or left eigenvectors to be
        /// transformed, as returned by DHSEIN or DTREVC.
        /// On exit, V is overwritten by the transformed eigenvectors.
        ///</param>
        /// <param name="LDV">
        /// (input) INTEGER
        /// The leading dimension of the array V. LDV .GE. max(1,N).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(string JOB, string SIDE, int N, int ILO, int IHI, double[] SCALE, int offset_scale
                         , int M, ref double[] V, int offset_v, int LDV, ref int INFO)
        {

            #region Variables
            
            bool LEFTV = false; bool RIGHTV = false; int I = 0; int II = 0; int K = 0; double S = 0; 

            #endregion


            #region Array Index Correction
            
             int o_scale = -1 + offset_scale;  int o_v = -1 - LDV + offset_v; 

            #endregion


            #region Strings
            
            JOB = JOB.Substring(0, 1);  SIDE = SIDE.Substring(0, 1);  

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
            // *  DGEBAK forms the right or left eigenvectors of a real general matrix
            // *  by backward transformation on the computed eigenvectors of the
            // *  balanced matrix output by DGEBAL.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  JOB     (input) CHARACTER*1
            // *          Specifies the type of backward transformation required:
            // *          = 'N', do nothing, return immediately;
            // *          = 'P', do backward transformation for permutation only;
            // *          = 'S', do backward transformation for scaling only;
            // *          = 'B', do backward transformations for both permutation and
            // *                 scaling.
            // *          JOB must be the same as the argument JOB supplied to DGEBAL.
            // *
            // *  SIDE    (input) CHARACTER*1
            // *          = 'R':  V contains right eigenvectors;
            // *          = 'L':  V contains left eigenvectors.
            // *
            // *  N       (input) INTEGER
            // *          The number of rows of the matrix V.  N >= 0.
            // *
            // *  ILO     (input) INTEGER
            // *  IHI     (input) INTEGER
            // *          The integers ILO and IHI determined by DGEBAL.
            // *          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
            // *
            // *  SCALE   (input) DOUBLE PRECISION array, dimension (N)
            // *          Details of the permutation and scaling factors, as returned
            // *          by DGEBAL.
            // *
            // *  M       (input) INTEGER
            // *          The number of columns of the matrix V.  M >= 0.
            // *
            // *  V       (input/output) DOUBLE PRECISION array, dimension (LDV,M)
            // *          On entry, the matrix of right or left eigenvectors to be
            // *          transformed, as returned by DHSEIN or DTREVC.
            // *          On exit, V is overwritten by the transformed eigenvectors.
            // *
            // *  LDV     (input) INTEGER
            // *          The leading dimension of the array V. LDV >= max(1,N).
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
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
            //      INTRINSIC          MAX, MIN;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Decode and Test the input parameters
            // *

            #endregion


            #region Body
            
            RIGHTV = this._lsame.Run(SIDE, "R");
            LEFTV = this._lsame.Run(SIDE, "L");
            // *
            INFO = 0;
            if (!this._lsame.Run(JOB, "N") && !this._lsame.Run(JOB, "P") && !this._lsame.Run(JOB, "S") && !this._lsame.Run(JOB, "B"))
            {
                INFO =  - 1;
            }
            else
            {
                if (!RIGHTV && !LEFTV)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (N < 0)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (ILO < 1 || ILO > Math.Max(1, N))
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (IHI < Math.Min(ILO, N) || IHI > N)
                            {
                                INFO =  - 5;
                            }
                            else
                            {
                                if (M < 0)
                                {
                                    INFO =  - 7;
                                }
                                else
                                {
                                    if (LDV < Math.Max(1, N))
                                    {
                                        INFO =  - 9;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGEBAK",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N == 0) return;
            if (M == 0) return;
            if (this._lsame.Run(JOB, "N")) return;
            // *
            if (ILO == IHI) goto LABEL30;
            // *
            // *     Backward balance
            // *
            if (this._lsame.Run(JOB, "S") || this._lsame.Run(JOB, "B"))
            {
                // *
                if (RIGHTV)
                {
                    for (I = ILO; I <= IHI; I++)
                    {
                        S = SCALE[I + o_scale];
                        this._dscal.Run(M, S, ref V, I+1 * LDV + o_v, LDV);
                    }
                }
                // *
                if (LEFTV)
                {
                    for (I = ILO; I <= IHI; I++)
                    {
                        S = ONE / SCALE[I + o_scale];
                        this._dscal.Run(M, S, ref V, I+1 * LDV + o_v, LDV);
                    }
                }
                // *
            }
            // *
            // *     Backward permutation
            // *
            // *     For  I = ILO-1 step -1 until 1,
            // *              IHI+1 step 1 until N do --
            // *
        LABEL30:;
            if (this._lsame.Run(JOB, "P") || this._lsame.Run(JOB, "B"))
            {
                if (RIGHTV)
                {
                    for (II = 1; II <= N; II++)
                    {
                        I = II;
                        if (I >= ILO && I <= IHI) goto LABEL40;
                        if (I < ILO) I = ILO - II;
                        K = (int)SCALE[I + o_scale];
                        if (K == I) goto LABEL40;
                        this._dswap.Run(M, ref V, I+1 * LDV + o_v, LDV, ref V, K+1 * LDV + o_v, LDV);
                    LABEL40:;
                    }
                }
                // *
                if (LEFTV)
                {
                    for (II = 1; II <= N; II++)
                    {
                        I = II;
                        if (I >= ILO && I <= IHI) goto LABEL50;
                        if (I < ILO) I = ILO - II;
                        K = (int)SCALE[I + o_scale];
                        if (K == I) goto LABEL50;
                        this._dswap.Run(M, ref V, I+1 * LDV + o_v, LDV, ref V, K+1 * LDV + o_v, LDV);
                    LABEL50:;
                    }
                }
            }
            // *
            return;
            // *
            // *     End of DGEBAK
            // *

            #endregion

        }
    }
}
