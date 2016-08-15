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
    /// DLARZB applies a real block reflector H or its transpose H**T to
    /// a real distributed M-by-N  C from the left or the right.
    /// 
    /// Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
    /// 
    ///</summary>
    public class DLARZB
    {
    

        #region Dependencies
        
        LSAME _lsame; DCOPY _dcopy; DGEMM _dgemm; DTRMM _dtrmm; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; 

        #endregion

        public DLARZB(LSAME lsame, DCOPY dcopy, DGEMM dgemm, DTRMM dtrmm, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dcopy = dcopy; this._dgemm = dgemm; this._dtrmm = dtrmm; this._xerbla = xerbla; 

            #endregion

        }
    
        public DLARZB()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DCOPY dcopy = new DCOPY();
            XERBLA xerbla = new XERBLA();
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dcopy = dcopy; this._dgemm = dgemm; this._dtrmm = dtrmm; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLARZB applies a real block reflector H or its transpose H**T to
        /// a real distributed M-by-N  C from the left or the right.
        /// 
        /// Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
        /// 
        ///</summary>
        /// <param name="SIDE">
        /// (input) CHARACTER*1
        /// = 'L': apply H or H' from the Left
        /// = 'R': apply H or H' from the Right
        ///</param>
        /// <param name="TRANS">
        /// (input) CHARACTER*1
        /// = 'N': apply H (No transpose)
        /// = 'C': apply H' (Transpose)
        ///</param>
        /// <param name="DIRECT">
        /// (input) CHARACTER*1
        /// Indicates how H is formed from a product of elementary
        /// reflectors
        /// = 'F': H = H(1) H(2) . . . H(k) (Forward, not supported yet)
        /// = 'B': H = H(k) . . . H(2) H(1) (Backward)
        ///</param>
        /// <param name="STOREV">
        /// (input) CHARACTER*1
        /// Indicates how the vectors which define the elementary
        /// reflectors are stored:
        /// = 'C': Columnwise                        (not supported yet)
        /// = 'R': Rowwise
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix C.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix C.
        ///</param>
        /// <param name="K">
        /// (input) INTEGER
        /// The order of the matrix T (= the number of elementary
        /// reflectors whose product defines the block reflector).
        ///</param>
        /// <param name="L">
        /// (input) INTEGER
        /// The number of columns of the matrix V containing the
        /// meaningful part of the Householder reflectors.
        /// If SIDE = 'L', M .GE. L .GE. 0, if SIDE = 'R', N .GE. L .GE. 0.
        ///</param>
        /// <param name="V">
        /// (input) DOUBLE PRECISION array, dimension (LDV,NV).
        /// If STOREV = 'C', NV = K; if STOREV = 'R', NV = L.
        ///</param>
        /// <param name="LDV">
        /// (input) INTEGER
        /// The leading dimension of the array V.
        /// If STOREV = 'C', LDV .GE. L; if STOREV = 'R', LDV .GE. K.
        ///</param>
        /// <param name="T">
        /// (input) DOUBLE PRECISION array, dimension (LDT,K)
        /// The triangular K-by-K matrix T in the representation of the
        /// block reflector.
        ///</param>
        /// <param name="LDT">
        /// (input) INTEGER
        /// The leading dimension of the array T. LDT .GE. K.
        ///</param>
        /// <param name="C">
        /// (input/output) DOUBLE PRECISION array, dimension (LDC,N)
        /// On entry, the M-by-N matrix C.
        /// On exit, C is overwritten by H*C or H'*C or C*H or C*H'.
        ///</param>
        /// <param name="LDC">
        /// (input) INTEGER
        /// The leading dimension of the array C. LDC .GE. max(1,M).
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (LDWORK,K)
        ///</param>
        /// <param name="LDWORK">
        /// (input) INTEGER
        /// The leading dimension of the array WORK.
        /// If SIDE = 'L', LDWORK .GE. max(1,N);
        /// if SIDE = 'R', LDWORK .GE. max(1,M).
        ///</param>
        public void Run(string SIDE, string TRANS, string DIRECT, string STOREV, int M, int N
                         , int K, int L, double[] V, int offset_v, int LDV, double[] T, int offset_t, int LDT
                         , ref double[] C, int offset_c, int LDC, ref double[] WORK, int offset_work, int LDWORK)
        {

            #region Variables
            
            string TRANST = new string(' ', 1); int I = 0; int INFO = 0; int J = 0; 

            #endregion


            #region Implicit Variables
            
            int C_J = 0; int WORK_J = 0; 

            #endregion


            #region Array Index Correction
            
             int o_v = -1 - LDV + offset_v;  int o_t = -1 - LDT + offset_t;  int o_c = -1 - LDC + offset_c; 
             int o_work = -1 - LDWORK + offset_work;

            #endregion


            #region Strings
            
            SIDE = SIDE.Substring(0, 1);  TRANS = TRANS.Substring(0, 1);  DIRECT = DIRECT.Substring(0, 1);  
            STOREV = STOREV.Substring(0, 1); 

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
            // *  DLARZB applies a real block reflector H or its transpose H**T to
            // *  a real distributed M-by-N  C from the left or the right.
            // *
            // *  Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  SIDE    (input) CHARACTER*1
            // *          = 'L': apply H or H' from the Left
            // *          = 'R': apply H or H' from the Right
            // *
            // *  TRANS   (input) CHARACTER*1
            // *          = 'N': apply H (No transpose)
            // *          = 'C': apply H' (Transpose)
            // *
            // *  DIRECT  (input) CHARACTER*1
            // *          Indicates how H is formed from a product of elementary
            // *          reflectors
            // *          = 'F': H = H(1) H(2) . . . H(k) (Forward, not supported yet)
            // *          = 'B': H = H(k) . . . H(2) H(1) (Backward)
            // *
            // *  STOREV  (input) CHARACTER*1
            // *          Indicates how the vectors which define the elementary
            // *          reflectors are stored:
            // *          = 'C': Columnwise                        (not supported yet)
            // *          = 'R': Rowwise
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix C.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix C.
            // *
            // *  K       (input) INTEGER
            // *          The order of the matrix T (= the number of elementary
            // *          reflectors whose product defines the block reflector).
            // *
            // *  L       (input) INTEGER
            // *          The number of columns of the matrix V containing the
            // *          meaningful part of the Householder reflectors.
            // *          If SIDE = 'L', M >= L >= 0, if SIDE = 'R', N >= L >= 0.
            // *
            // *  V       (input) DOUBLE PRECISION array, dimension (LDV,NV).
            // *          If STOREV = 'C', NV = K; if STOREV = 'R', NV = L.
            // *
            // *  LDV     (input) INTEGER
            // *          The leading dimension of the array V.
            // *          If STOREV = 'C', LDV >= L; if STOREV = 'R', LDV >= K.
            // *
            // *  T       (input) DOUBLE PRECISION array, dimension (LDT,K)
            // *          The triangular K-by-K matrix T in the representation of the
            // *          block reflector.
            // *
            // *  LDT     (input) INTEGER
            // *          The leading dimension of the array T. LDT >= K.
            // *
            // *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
            // *          On entry, the M-by-N matrix C.
            // *          On exit, C is overwritten by H*C or H'*C or C*H or C*H'.
            // *
            // *  LDC     (input) INTEGER
            // *          The leading dimension of the array C. LDC >= max(1,M).
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (LDWORK,K)
            // *
            // *  LDWORK  (input) INTEGER
            // *          The leading dimension of the array WORK.
            // *          If SIDE = 'L', LDWORK >= max(1,N);
            // *          if SIDE = 'R', LDWORK >= max(1,M).
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
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
            // *     .. Executable Statements ..
            // *
            // *     Quick return if possible
            // *

            #endregion


            #region Body
            
            if (M <= 0 || N <= 0) return;
            // *
            // *     Check for currently supported options
            // *
            INFO = 0;
            if (!this._lsame.Run(DIRECT, "B"))
            {
                INFO =  - 3;
            }
            else
            {
                if (!this._lsame.Run(STOREV, "R"))
                {
                    INFO =  - 4;
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLARZB",  - INFO);
                return;
            }
            // *
            if (this._lsame.Run(TRANS, "N"))
            {
                FortranLib.Copy(ref TRANST , "T");
            }
            else
            {
                FortranLib.Copy(ref TRANST , "N");
            }
            // *
            if (this._lsame.Run(SIDE, "L"))
            {
                // *
                // *        Form  H * C  or  H' * C
                // *
                // *        W( 1:n, 1:k ) = C( 1:k, 1:n )'
                // *
                for (J = 1; J <= K; J++)
                {
                    this._dcopy.Run(N, C, J+1 * LDC + o_c, LDC, ref WORK, 1+J * LDWORK + o_work, 1);
                }
                // *
                // *        W( 1:n, 1:k ) = W( 1:n, 1:k ) + ...
                // *                        C( m-l+1:m, 1:n )' * V( 1:k, 1:l )'
                // *
                if (L > 0)
                {
                    this._dgemm.Run("Transpose", "Transpose", N, K, L, ONE
                                    , C, M - L + 1+1 * LDC + o_c, LDC, V, offset_v, LDV, ONE, ref WORK, offset_work
                                    , LDWORK);
                }
                // *
                // *        W( 1:n, 1:k ) = W( 1:n, 1:k ) * T'  or  W( 1:m, 1:k ) * T
                // *
                this._dtrmm.Run("Right", "Lower", TRANST, "Non-unit", N, K
                                , ONE, T, offset_t, LDT, ref WORK, offset_work, LDWORK);
                // *
                // *        C( 1:k, 1:n ) = C( 1:k, 1:n ) - W( 1:n, 1:k )'
                // *
                for (J = 1; J <= N; J++)
                {
                    C_J = J * LDC + o_c;
                    for (I = 1; I <= K; I++)
                    {
                        C[I + C_J] -= WORK[J+I * LDWORK + o_work];
                    }
                }
                // *
                // *        C( m-l+1:m, 1:n ) = C( m-l+1:m, 1:n ) - ...
                // *                            V( 1:k, 1:l )' * W( 1:n, 1:k )'
                // *
                if (L > 0)
                {
                    this._dgemm.Run("Transpose", "Transpose", L, N, K,  - ONE
                                    , V, offset_v, LDV, WORK, offset_work, LDWORK, ONE, ref C, M - L + 1+1 * LDC + o_c
                                    , LDC);
                }
                // *
            }
            else
            {
                if (this._lsame.Run(SIDE, "R"))
                {
                    // *
                    // *        Form  C * H  or  C * H'
                    // *
                    // *        W( 1:m, 1:k ) = C( 1:m, 1:k )
                    // *
                    for (J = 1; J <= K; J++)
                    {
                        this._dcopy.Run(M, C, 1+J * LDC + o_c, 1, ref WORK, 1+J * LDWORK + o_work, 1);
                    }
                    // *
                    // *        W( 1:m, 1:k ) = W( 1:m, 1:k ) + ...
                    // *                        C( 1:m, n-l+1:n ) * V( 1:k, 1:l )'
                    // *
                    if (L > 0)
                    {
                        this._dgemm.Run("No transpose", "Transpose", M, K, L, ONE
                                        , C, 1+(N - L + 1) * LDC + o_c, LDC, V, offset_v, LDV, ONE, ref WORK, offset_work
                                        , LDWORK);
                    }
                    // *
                    // *        W( 1:m, 1:k ) = W( 1:m, 1:k ) * T  or  W( 1:m, 1:k ) * T'
                    // *
                    this._dtrmm.Run("Right", "Lower", TRANS, "Non-unit", M, K
                                    , ONE, T, offset_t, LDT, ref WORK, offset_work, LDWORK);
                    // *
                    // *        C( 1:m, 1:k ) = C( 1:m, 1:k ) - W( 1:m, 1:k )
                    // *
                    for (J = 1; J <= K; J++)
                    {
                        C_J = J * LDC + o_c;
                        WORK_J = J * LDWORK + o_work;
                        for (I = 1; I <= M; I++)
                        {
                            C[I + C_J] -= WORK[I + WORK_J];
                        }
                    }
                    // *
                    // *        C( 1:m, n-l+1:n ) = C( 1:m, n-l+1:n ) - ...
                    // *                            W( 1:m, 1:k ) * V( 1:k, 1:l )
                    // *
                    if (L > 0)
                    {
                        this._dgemm.Run("No transpose", "No transpose", M, L, K,  - ONE
                                        , WORK, offset_work, LDWORK, V, offset_v, LDV, ONE, ref C, 1+(N - L + 1) * LDC + o_c
                                        , LDC);
                    }
                    // *
                }
            }
            // *
            return;
            // *
            // *     End of DLARZB
            // *

            #endregion

        }
    }
}
