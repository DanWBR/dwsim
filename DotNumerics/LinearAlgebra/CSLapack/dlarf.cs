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
    /// DLARF applies a real elementary reflector H to a real m by n matrix
    /// C, from either the left or the right. H is represented in the form
    /// 
    /// H = I - tau * v * v'
    /// 
    /// where tau is a real scalar and v is a real vector.
    /// 
    /// If tau = 0, then H is taken to be the unit matrix.
    /// 
    ///</summary>
    public class DLARF
    {
    

        #region Dependencies
        
        DGEMV _dgemv; DGER _dger; LSAME _lsame; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DLARF(DGEMV dgemv, DGER dger, LSAME lsame)
        {
    

            #region Set Dependencies
            
            this._dgemv = dgemv; this._dger = dger; this._lsame = lsame; 

            #endregion

        }
    
        public DLARF()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);

            #endregion


            #region Set Dependencies
            
            this._dgemv = dgemv; this._dger = dger; this._lsame = lsame; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLARF applies a real elementary reflector H to a real m by n matrix
        /// C, from either the left or the right. H is represented in the form
        /// 
        /// H = I - tau * v * v'
        /// 
        /// where tau is a real scalar and v is a real vector.
        /// 
        /// If tau = 0, then H is taken to be the unit matrix.
        /// 
        ///</summary>
        /// <param name="SIDE">
        /// (input) CHARACTER*1
        /// = 'L': form  H * C
        /// = 'R': form  C * H
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix C.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix C.
        ///</param>
        /// <param name="V">
        /// (input) DOUBLE PRECISION array, dimension
        /// (1 + (M-1)*abs(INCV)) if SIDE = 'L'
        /// or (1 + (N-1)*abs(INCV)) if SIDE = 'R'
        /// The vector v in the representation of H. V is not used if
        /// TAU = 0.
        ///</param>
        /// <param name="INCV">
        /// (input) INTEGER
        /// The increment between elements of v. INCV .LT..GT. 0.
        ///</param>
        /// <param name="TAU">
        /// (input) DOUBLE PRECISION
        /// The value tau in the representation of H.
        ///</param>
        /// <param name="C">
        /// (input/output) DOUBLE PRECISION array, dimension (LDC,N)
        /// On entry, the m by n matrix C.
        /// On exit, C is overwritten by the matrix H * C if SIDE = 'L',
        /// or C * H if SIDE = 'R'.
        ///</param>
        /// <param name="LDC">
        /// (input) INTEGER
        /// The leading dimension of the array C. LDC .GE. max(1,M).
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension
        /// (N) if SIDE = 'L'
        /// or (M) if SIDE = 'R'
        ///</param>
        public void Run(string SIDE, int M, int N, double[] V, int offset_v, int INCV, double TAU
                         , ref double[] C, int offset_c, int LDC, ref double[] WORK, int offset_work)
        {

            #region Array Index Correction
            
             int o_v = -1 + offset_v;  int o_c = -1 - LDC + offset_c;  int o_work = -1 + offset_work; 

            #endregion


            #region Strings
            
            SIDE = SIDE.Substring(0, 1);  

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
            // *  DLARF applies a real elementary reflector H to a real m by n matrix
            // *  C, from either the left or the right. H is represented in the form
            // *
            // *        H = I - tau * v * v'
            // *
            // *  where tau is a real scalar and v is a real vector.
            // *
            // *  If tau = 0, then H is taken to be the unit matrix.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  SIDE    (input) CHARACTER*1
            // *          = 'L': form  H * C
            // *          = 'R': form  C * H
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix C.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix C.
            // *
            // *  V       (input) DOUBLE PRECISION array, dimension
            // *                     (1 + (M-1)*abs(INCV)) if SIDE = 'L'
            // *                  or (1 + (N-1)*abs(INCV)) if SIDE = 'R'
            // *          The vector v in the representation of H. V is not used if
            // *          TAU = 0.
            // *
            // *  INCV    (input) INTEGER
            // *          The increment between elements of v. INCV <> 0.
            // *
            // *  TAU     (input) DOUBLE PRECISION
            // *          The value tau in the representation of H.
            // *
            // *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
            // *          On entry, the m by n matrix C.
            // *          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
            // *          or C * H if SIDE = 'R'.
            // *
            // *  LDC     (input) INTEGER
            // *          The leading dimension of the array C. LDC >= max(1,M).
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension
            // *                         (N) if SIDE = 'L'
            // *                      or (M) if SIDE = 'R'
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            if (this._lsame.Run(SIDE, "L"))
            {
                // *
                // *        Form  H * C
                // *
                if (TAU != ZERO)
                {
                    // *
                    // *           w := C' * v
                    // *
                    this._dgemv.Run("Transpose", M, N, ONE, C, offset_c, LDC
                                    , V, offset_v, INCV, ZERO, ref WORK, offset_work, 1);
                    // *
                    // *           C := C - v * w'
                    // *
                    this._dger.Run(M, N,  - TAU, V, offset_v, INCV, WORK, offset_work
                                   , 1, ref C, offset_c, LDC);
                }
            }
            else
            {
                // *
                // *        Form  C * H
                // *
                if (TAU != ZERO)
                {
                    // *
                    // *           w := C * v
                    // *
                    this._dgemv.Run("No transpose", M, N, ONE, C, offset_c, LDC
                                    , V, offset_v, INCV, ZERO, ref WORK, offset_work, 1);
                    // *
                    // *           C := C - w * v'
                    // *
                    this._dger.Run(M, N,  - TAU, WORK, offset_work, 1, V, offset_v
                                   , INCV, ref C, offset_c, LDC);
                }
            }
            return;
            // *
            // *     End of DLARF
            // *

            #endregion

        }
    }
}
