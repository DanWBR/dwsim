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
    /// DLARZT forms the triangular factor T of a real block reflector
    /// H of order .GT. n, which is defined as a product of k elementary
    /// reflectors.
    /// 
    /// If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
    /// 
    /// If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
    /// 
    /// If STOREV = 'C', the vector which defines the elementary reflector
    /// H(i) is stored in the i-th column of the array V, and
    /// 
    /// H  =  I - V * T * V'
    /// 
    /// If STOREV = 'R', the vector which defines the elementary reflector
    /// H(i) is stored in the i-th row of the array V, and
    /// 
    /// H  =  I - V' * T * V
    /// 
    /// Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
    /// 
    ///</summary>
    public class DLARZT
    {
    

        #region Dependencies
        
        DGEMV _dgemv; DTRMV _dtrmv; XERBLA _xerbla; LSAME _lsame; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; 

        #endregion

        public DLARZT(DGEMV dgemv, DTRMV dtrmv, XERBLA xerbla, LSAME lsame)
        {
    

            #region Set Dependencies
            
            this._dgemv = dgemv; this._dtrmv = dtrmv; this._xerbla = xerbla; this._lsame = lsame; 

            #endregion

        }
    
        public DLARZT()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DTRMV dtrmv = new DTRMV(lsame, xerbla);

            #endregion


            #region Set Dependencies
            
            this._dgemv = dgemv; this._dtrmv = dtrmv; this._xerbla = xerbla; this._lsame = lsame; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLARZT forms the triangular factor T of a real block reflector
        /// H of order .GT. n, which is defined as a product of k elementary
        /// reflectors.
        /// 
        /// If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
        /// 
        /// If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
        /// 
        /// If STOREV = 'C', the vector which defines the elementary reflector
        /// H(i) is stored in the i-th column of the array V, and
        /// 
        /// H  =  I - V * T * V'
        /// 
        /// If STOREV = 'R', the vector which defines the elementary reflector
        /// H(i) is stored in the i-th row of the array V, and
        /// 
        /// H  =  I - V' * T * V
        /// 
        /// Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
        /// 
        ///</summary>
        /// <param name="DIRECT">
        /// (input) CHARACTER*1
        /// Specifies the order in which the elementary reflectors are
        /// multiplied to form the block reflector:
        /// = 'F': H = H(1) H(2) . . . H(k) (Forward, not supported yet)
        /// = 'B': H = H(k) . . . H(2) H(1) (Backward)
        ///</param>
        /// <param name="STOREV">
        /// (input) CHARACTER*1
        /// Specifies how the vectors which define the elementary
        /// reflectors are stored (see also Further Details):
        /// = 'C': columnwise                        (not supported yet)
        /// = 'R': rowwise
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the block reflector H. N .GE. 0.
        ///</param>
        /// <param name="K">
        /// (input) INTEGER
        /// The order of the triangular factor T (= the number of
        /// elementary reflectors). K .GE. 1.
        ///</param>
        /// <param name="V">
        /// (input/output) DOUBLE PRECISION array, dimension
        /// (LDV,K) if STOREV = 'C'
        /// (LDV,N) if STOREV = 'R'
        /// The matrix V. See further details.
        ///</param>
        /// <param name="LDV">
        /// (input) INTEGER
        /// The leading dimension of the array V.
        /// If STOREV = 'C', LDV .GE. max(1,N); if STOREV = 'R', LDV .GE. K.
        ///</param>
        /// <param name="TAU">
        /// (input) DOUBLE PRECISION array, dimension (K)
        /// TAU(i) must contain the scalar factor of the elementary
        /// reflector H(i).
        ///</param>
        /// <param name="T">
        /// (output) DOUBLE PRECISION array, dimension (LDT,K)
        /// The k by k triangular factor T of the block reflector.
        /// If DIRECT = 'F', T is upper triangular; if DIRECT = 'B', T is
        /// lower triangular. The rest of the array is not used.
        ///</param>
        /// <param name="LDT">
        /// (input) INTEGER
        /// The leading dimension of the array T. LDT .GE. K.
        ///</param>
        public void Run(string DIRECT, string STOREV, int N, int K, double[] V, int offset_v, int LDV
                         , double[] TAU, int offset_tau, ref double[] T, int offset_t, int LDT)
        {

            #region Variables
            
            int I = 0; int INFO = 0; int J = 0; 

            #endregion


            #region Implicit Variables
            
            int T_I = 0; 

            #endregion


            #region Array Index Correction
            
             int o_v = -1 - LDV + offset_v;  int o_tau = -1 + offset_tau;  int o_t = -1 - LDT + offset_t; 

            #endregion


            #region Strings
            
            DIRECT = DIRECT.Substring(0, 1);  STOREV = STOREV.Substring(0, 1);  

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
            // *  DLARZT forms the triangular factor T of a real block reflector
            // *  H of order > n, which is defined as a product of k elementary
            // *  reflectors.
            // *
            // *  If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
            // *
            // *  If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
            // *
            // *  If STOREV = 'C', the vector which defines the elementary reflector
            // *  H(i) is stored in the i-th column of the array V, and
            // *
            // *     H  =  I - V * T * V'
            // *
            // *  If STOREV = 'R', the vector which defines the elementary reflector
            // *  H(i) is stored in the i-th row of the array V, and
            // *
            // *     H  =  I - V' * T * V
            // *
            // *  Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  DIRECT  (input) CHARACTER*1
            // *          Specifies the order in which the elementary reflectors are
            // *          multiplied to form the block reflector:
            // *          = 'F': H = H(1) H(2) . . . H(k) (Forward, not supported yet)
            // *          = 'B': H = H(k) . . . H(2) H(1) (Backward)
            // *
            // *  STOREV  (input) CHARACTER*1
            // *          Specifies how the vectors which define the elementary
            // *          reflectors are stored (see also Further Details):
            // *          = 'C': columnwise                        (not supported yet)
            // *          = 'R': rowwise
            // *
            // *  N       (input) INTEGER
            // *          The order of the block reflector H. N >= 0.
            // *
            // *  K       (input) INTEGER
            // *          The order of the triangular factor T (= the number of
            // *          elementary reflectors). K >= 1.
            // *
            // *  V       (input/output) DOUBLE PRECISION array, dimension
            // *                               (LDV,K) if STOREV = 'C'
            // *                               (LDV,N) if STOREV = 'R'
            // *          The matrix V. See further details.
            // *
            // *  LDV     (input) INTEGER
            // *          The leading dimension of the array V.
            // *          If STOREV = 'C', LDV >= max(1,N); if STOREV = 'R', LDV >= K.
            // *
            // *  TAU     (input) DOUBLE PRECISION array, dimension (K)
            // *          TAU(i) must contain the scalar factor of the elementary
            // *          reflector H(i).
            // *
            // *  T       (output) DOUBLE PRECISION array, dimension (LDT,K)
            // *          The k by k triangular factor T of the block reflector.
            // *          If DIRECT = 'F', T is upper triangular; if DIRECT = 'B', T is
            // *          lower triangular. The rest of the array is not used.
            // *
            // *  LDT     (input) INTEGER
            // *          The leading dimension of the array T. LDT >= K.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
            // *
            // *  The shape of the matrix V and the storage of the vectors which define
            // *  the H(i) is best illustrated by the following example with n = 5 and
            // *  k = 3. The elements equal to 1 are not stored; the corresponding
            // *  array elements are modified but restored on exit. The rest of the
            // *  array is not used.
            // *
            // *  DIRECT = 'F' and STOREV = 'C':         DIRECT = 'F' and STOREV = 'R':
            // *
            // *                                              ______V_____
            // *         ( v1 v2 v3 )                        /            \
            // *         ( v1 v2 v3 )                      ( v1 v1 v1 v1 v1 . . . . 1 )
            // *     V = ( v1 v2 v3 )                      ( v2 v2 v2 v2 v2 . . . 1   )
            // *         ( v1 v2 v3 )                      ( v3 v3 v3 v3 v3 . . 1     )
            // *         ( v1 v2 v3 )
            // *            .  .  .
            // *            .  .  .
            // *            1  .  .
            // *               1  .
            // *                  1
            // *
            // *  DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R':
            // *
            // *                                                        ______V_____
            // *            1                                          /            \
            // *            .  1                           ( 1 . . . . v1 v1 v1 v1 v1 )
            // *            .  .  1                        ( . 1 . . . v2 v2 v2 v2 v2 )
            // *            .  .  .                        ( . . 1 . . v3 v3 v3 v3 v3 )
            // *            .  .  .
            // *         ( v1 v2 v3 )
            // *         ( v1 v2 v3 )
            // *     V = ( v1 v2 v3 )
            // *         ( v1 v2 v3 )
            // *         ( v1 v2 v3 )
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
            // *     .. Executable Statements ..
            // *
            // *     Check for currently supported options
            // *

            #endregion


            #region Body
            
            INFO = 0;
            if (!this._lsame.Run(DIRECT, "B"))
            {
                INFO =  - 1;
            }
            else
            {
                if (!this._lsame.Run(STOREV, "R"))
                {
                    INFO =  - 2;
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLARZT",  - INFO);
                return;
            }
            // *
            for (I = K; I >= 1; I +=  - 1)
            {
                if (TAU[I + o_tau] == ZERO)
                {
                    // *
                    // *           H(i)  =  I
                    // *
                    T_I = I * LDT + o_t;
                    for (J = I; J <= K; J++)
                    {
                        T[J + T_I] = ZERO;
                    }
                }
                else
                {
                    // *
                    // *           general case
                    // *
                    if (I < K)
                    {
                        // *
                        // *              T(i+1:k,i) = - tau(i) * V(i+1:k,1:n) * V(i,1:n)'
                        // *
                        this._dgemv.Run("No transpose", K - I, N,  - TAU[I + o_tau], V, I + 1+1 * LDV + o_v, LDV
                                        , V, I+1 * LDV + o_v, LDV, ZERO, ref T, I + 1+I * LDT + o_t, 1);
                        // *
                        // *              T(i+1:k,i) = T(i+1:k,i+1:k) * T(i+1:k,i)
                        // *
                        this._dtrmv.Run("Lower", "No transpose", "Non-unit", K - I, T, I + 1+(I + 1) * LDT + o_t, LDT
                                        , ref T, I + 1+I * LDT + o_t, 1);
                    }
                    T[I+I * LDT + o_t] = TAU[I + o_tau];
                }
            }
            return;
            // *
            // *     End of DLARZT
            // *

            #endregion

        }
    }
}
