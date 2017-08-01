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
    /// DLARFB applies a real block reflector H or its transpose H' to a
    /// real m by n matrix C, from either the left or the right.
    /// 
    ///</summary>
    public class DLARFB
    {
    

        #region Dependencies
        
        LSAME _lsame; DCOPY _dcopy; DGEMM _dgemm; DTRMM _dtrmm; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; 

        #endregion

        public DLARFB(LSAME lsame, DCOPY dcopy, DGEMM dgemm, DTRMM dtrmm)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dcopy = dcopy; this._dgemm = dgemm; this._dtrmm = dtrmm; 

            #endregion

        }
    
        public DLARFB()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DCOPY dcopy = new DCOPY();
            XERBLA xerbla = new XERBLA();
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dcopy = dcopy; this._dgemm = dgemm; this._dtrmm = dtrmm; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLARFB applies a real block reflector H or its transpose H' to a
        /// real m by n matrix C, from either the left or the right.
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
        /// = 'T': apply H' (Transpose)
        ///</param>
        /// <param name="DIRECT">
        /// (input) CHARACTER*1
        /// Indicates how H is formed from a product of elementary
        /// reflectors
        /// = 'F': H = H(1) H(2) . . . H(k) (Forward)
        /// = 'B': H = H(k) . . . H(2) H(1) (Backward)
        ///</param>
        /// <param name="STOREV">
        /// (input) CHARACTER*1
        /// Indicates how the vectors which define the elementary
        /// reflectors are stored:
        /// = 'C': Columnwise
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
        /// <param name="V">
        /// (input) DOUBLE PRECISION array, dimension
        /// (LDV,K) if STOREV = 'C'
        /// (LDV,M) if STOREV = 'R' and SIDE = 'L'
        /// (LDV,N) if STOREV = 'R' and SIDE = 'R'
        /// The matrix V. See further details.
        ///</param>
        /// <param name="LDV">
        /// (input) INTEGER
        /// The leading dimension of the array V.
        /// If STOREV = 'C' and SIDE = 'L', LDV .GE. max(1,M);
        /// if STOREV = 'C' and SIDE = 'R', LDV .GE. max(1,N);
        /// if STOREV = 'R', LDV .GE. K.
        ///</param>
        /// <param name="T">
        /// (input) DOUBLE PRECISION array, dimension (LDT,K)
        /// The triangular k by k matrix T in the representation of the
        /// block reflector.
        ///</param>
        /// <param name="LDT">
        /// (input) INTEGER
        /// The leading dimension of the array T. LDT .GE. K.
        ///</param>
        /// <param name="C">
        /// (input/output) DOUBLE PRECISION array, dimension (LDC,N)
        /// On entry, the m by n matrix C.
        /// On exit, C is overwritten by H*C or H'*C or C*H or C*H'.
        ///</param>
        /// <param name="LDC">
        /// (input) INTEGER
        /// The leading dimension of the array C. LDA .GE. max(1,M).
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
                         , int K, double[] V, int offset_v, int LDV, double[] T, int offset_t, int LDT, ref double[] C, int offset_c
                         , int LDC, ref double[] WORK, int offset_work, int LDWORK)
        {

            #region Variables
            
            string TRANST = new string(' ', 1); int I = 0; int J = 0; 

            #endregion


            #region Implicit Variables
            
            int WORK_J = 0; int C_J = 0; int C_0 = 0; int C_1 = 0; 

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
            // *  DLARFB applies a real block reflector H or its transpose H' to a
            // *  real m by n matrix C, from either the left or the right.
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
            // *          = 'T': apply H' (Transpose)
            // *
            // *  DIRECT  (input) CHARACTER*1
            // *          Indicates how H is formed from a product of elementary
            // *          reflectors
            // *          = 'F': H = H(1) H(2) . . . H(k) (Forward)
            // *          = 'B': H = H(k) . . . H(2) H(1) (Backward)
            // *
            // *  STOREV  (input) CHARACTER*1
            // *          Indicates how the vectors which define the elementary
            // *          reflectors are stored:
            // *          = 'C': Columnwise
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
            // *  V       (input) DOUBLE PRECISION array, dimension
            // *                                (LDV,K) if STOREV = 'C'
            // *                                (LDV,M) if STOREV = 'R' and SIDE = 'L'
            // *                                (LDV,N) if STOREV = 'R' and SIDE = 'R'
            // *          The matrix V. See further details.
            // *
            // *  LDV     (input) INTEGER
            // *          The leading dimension of the array V.
            // *          If STOREV = 'C' and SIDE = 'L', LDV >= max(1,M);
            // *          if STOREV = 'C' and SIDE = 'R', LDV >= max(1,N);
            // *          if STOREV = 'R', LDV >= K.
            // *
            // *  T       (input) DOUBLE PRECISION array, dimension (LDT,K)
            // *          The triangular k by k matrix T in the representation of the
            // *          block reflector.
            // *
            // *  LDT     (input) INTEGER
            // *          The leading dimension of the array T. LDT >= K.
            // *
            // *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
            // *          On entry, the m by n matrix C.
            // *          On exit, C is overwritten by H*C or H'*C or C*H or C*H'.
            // *
            // *  LDC     (input) INTEGER
            // *          The leading dimension of the array C. LDA >= max(1,M).
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (LDWORK,K)
            // *
            // *  LDWORK  (input) INTEGER
            // *          The leading dimension of the array WORK.
            // *          If SIDE = 'L', LDWORK >= max(1,N);
            // *          if SIDE = 'R', LDWORK >= max(1,M).
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
            if (this._lsame.Run(TRANS, "N"))
            {
                FortranLib.Copy(ref TRANST , "T");
            }
            else
            {
                FortranLib.Copy(ref TRANST , "N");
            }
            // *
            if (this._lsame.Run(STOREV, "C"))
            {
                // *
                if (this._lsame.Run(DIRECT, "F"))
                {
                    // *
                    // *           Let  V =  ( V1 )    (first K rows)
                    // *                     ( V2 )
                    // *           where  V1  is unit lower triangular.
                    // *
                    if (this._lsame.Run(SIDE, "L"))
                    {
                        // *
                        // *              Form  H * C  or  H' * C  where  C = ( C1 )
                        // *                                                  ( C2 )
                        // *
                        // *              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
                        // *
                        // *              W := C1'
                        // *
                        for (J = 1; J <= K; J++)
                        {
                            this._dcopy.Run(N, C, J+1 * LDC + o_c, LDC, ref WORK, 1+J * LDWORK + o_work, 1);
                        }
                        // *
                        // *              W := W * V1
                        // *
                        this._dtrmm.Run("Right", "Lower", "No transpose", "Unit", N, K
                                        , ONE, V, offset_v, LDV, ref WORK, offset_work, LDWORK);
                        if (M > K)
                        {
                            // *
                            // *                 W := W + C2'*V2
                            // *
                            this._dgemm.Run("Transpose", "No transpose", N, K, M - K, ONE
                                            , C, K + 1+1 * LDC + o_c, LDC, V, K + 1+1 * LDV + o_v, LDV, ONE, ref WORK, offset_work
                                            , LDWORK);
                        }
                        // *
                        // *              W := W * T'  or  W * T
                        // *
                        this._dtrmm.Run("Right", "Upper", TRANST, "Non-unit", N, K
                                        , ONE, T, offset_t, LDT, ref WORK, offset_work, LDWORK);
                        // *
                        // *              C := C - V * W'
                        // *
                        if (M > K)
                        {
                            // *
                            // *                 C2 := C2 - V2 * W'
                            // *
                            this._dgemm.Run("No transpose", "Transpose", M - K, N, K,  - ONE
                                            , V, K + 1+1 * LDV + o_v, LDV, WORK, offset_work, LDWORK, ONE, ref C, K + 1+1 * LDC + o_c
                                            , LDC);
                        }
                        // *
                        // *              W := W * V1'
                        // *
                        this._dtrmm.Run("Right", "Lower", "Transpose", "Unit", N, K
                                        , ONE, V, offset_v, LDV, ref WORK, offset_work, LDWORK);
                        // *
                        // *              C1 := C1 - W'
                        // *
                        for (J = 1; J <= K; J++)
                        {
                            WORK_J = J * LDWORK + o_work;
                            for (I = 1; I <= N; I++)
                            {
                                C[J+I * LDC + o_c] -= WORK[I + WORK_J];
                            }
                        }
                        // *
                    }
                    else
                    {
                        if (this._lsame.Run(SIDE, "R"))
                        {
                            // *
                            // *              Form  C * H  or  C * H'  where  C = ( C1  C2 )
                            // *
                            // *              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
                            // *
                            // *              W := C1
                            // *
                            for (J = 1; J <= K; J++)
                            {
                                this._dcopy.Run(M, C, 1+J * LDC + o_c, 1, ref WORK, 1+J * LDWORK + o_work, 1);
                            }
                            // *
                            // *              W := W * V1
                            // *
                            this._dtrmm.Run("Right", "Lower", "No transpose", "Unit", M, K
                                            , ONE, V, offset_v, LDV, ref WORK, offset_work, LDWORK);
                            if (N > K)
                            {
                                // *
                                // *                 W := W + C2 * V2
                                // *
                                this._dgemm.Run("No transpose", "No transpose", M, K, N - K, ONE
                                                , C, 1+(K + 1) * LDC + o_c, LDC, V, K + 1+1 * LDV + o_v, LDV, ONE, ref WORK, offset_work
                                                , LDWORK);
                            }
                            // *
                            // *              W := W * T  or  W * T'
                            // *
                            this._dtrmm.Run("Right", "Upper", TRANS, "Non-unit", M, K
                                            , ONE, T, offset_t, LDT, ref WORK, offset_work, LDWORK);
                            // *
                            // *              C := C - W * V'
                            // *
                            if (N > K)
                            {
                                // *
                                // *                 C2 := C2 - W * V2'
                                // *
                                this._dgemm.Run("No transpose", "Transpose", M, N - K, K,  - ONE
                                                , WORK, offset_work, LDWORK, V, K + 1+1 * LDV + o_v, LDV, ONE, ref C, 1+(K + 1) * LDC + o_c
                                                , LDC);
                            }
                            // *
                            // *              W := W * V1'
                            // *
                            this._dtrmm.Run("Right", "Lower", "Transpose", "Unit", M, K
                                            , ONE, V, offset_v, LDV, ref WORK, offset_work, LDWORK);
                            // *
                            // *              C1 := C1 - W
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
                        }
                    }
                    // *
                }
                else
                {
                    // *
                    // *           Let  V =  ( V1 )
                    // *                     ( V2 )    (last K rows)
                    // *           where  V2  is unit upper triangular.
                    // *
                    if (this._lsame.Run(SIDE, "L"))
                    {
                        // *
                        // *              Form  H * C  or  H' * C  where  C = ( C1 )
                        // *                                                  ( C2 )
                        // *
                        // *              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
                        // *
                        // *              W := C2'
                        // *
                        for (J = 1; J <= K; J++)
                        {
                            this._dcopy.Run(N, C, M - K + J+1 * LDC + o_c, LDC, ref WORK, 1+J * LDWORK + o_work, 1);
                        }
                        // *
                        // *              W := W * V2
                        // *
                        this._dtrmm.Run("Right", "Upper", "No transpose", "Unit", N, K
                                        , ONE, V, M - K + 1+1 * LDV + o_v, LDV, ref WORK, offset_work, LDWORK);
                        if (M > K)
                        {
                            // *
                            // *                 W := W + C1'*V1
                            // *
                            this._dgemm.Run("Transpose", "No transpose", N, K, M - K, ONE
                                            , C, offset_c, LDC, V, offset_v, LDV, ONE, ref WORK, offset_work
                                            , LDWORK);
                        }
                        // *
                        // *              W := W * T'  or  W * T
                        // *
                        this._dtrmm.Run("Right", "Lower", TRANST, "Non-unit", N, K
                                        , ONE, T, offset_t, LDT, ref WORK, offset_work, LDWORK);
                        // *
                        // *              C := C - V * W'
                        // *
                        if (M > K)
                        {
                            // *
                            // *                 C1 := C1 - V1 * W'
                            // *
                            this._dgemm.Run("No transpose", "Transpose", M - K, N, K,  - ONE
                                            , V, offset_v, LDV, WORK, offset_work, LDWORK, ONE, ref C, offset_c
                                            , LDC);
                        }
                        // *
                        // *              W := W * V2'
                        // *
                        this._dtrmm.Run("Right", "Upper", "Transpose", "Unit", N, K
                                        , ONE, V, M - K + 1+1 * LDV + o_v, LDV, ref WORK, offset_work, LDWORK);
                        // *
                        // *              C2 := C2 - W'
                        // *
                        for (J = 1; J <= K; J++)
                        {
                            WORK_J = J * LDWORK + o_work;
                            for (I = 1; I <= N; I++)
                            {
                                C[M - K + J+I * LDC + o_c] -= WORK[I + WORK_J];
                            }
                        }
                        // *
                    }
                    else
                    {
                        if (this._lsame.Run(SIDE, "R"))
                        {
                            // *
                            // *              Form  C * H  or  C * H'  where  C = ( C1  C2 )
                            // *
                            // *              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
                            // *
                            // *              W := C2
                            // *
                            for (J = 1; J <= K; J++)
                            {
                                this._dcopy.Run(M, C, 1+(N - K + J) * LDC + o_c, 1, ref WORK, 1+J * LDWORK + o_work, 1);
                            }
                            // *
                            // *              W := W * V2
                            // *
                            this._dtrmm.Run("Right", "Upper", "No transpose", "Unit", M, K
                                            , ONE, V, N - K + 1+1 * LDV + o_v, LDV, ref WORK, offset_work, LDWORK);
                            if (N > K)
                            {
                                // *
                                // *                 W := W + C1 * V1
                                // *
                                this._dgemm.Run("No transpose", "No transpose", M, K, N - K, ONE
                                                , C, offset_c, LDC, V, offset_v, LDV, ONE, ref WORK, offset_work
                                                , LDWORK);
                            }
                            // *
                            // *              W := W * T  or  W * T'
                            // *
                            this._dtrmm.Run("Right", "Lower", TRANS, "Non-unit", M, K
                                            , ONE, T, offset_t, LDT, ref WORK, offset_work, LDWORK);
                            // *
                            // *              C := C - W * V'
                            // *
                            if (N > K)
                            {
                                // *
                                // *                 C1 := C1 - W * V1'
                                // *
                                this._dgemm.Run("No transpose", "Transpose", M, N - K, K,  - ONE
                                                , WORK, offset_work, LDWORK, V, offset_v, LDV, ONE, ref C, offset_c
                                                , LDC);
                            }
                            // *
                            // *              W := W * V2'
                            // *
                            this._dtrmm.Run("Right", "Upper", "Transpose", "Unit", M, K
                                            , ONE, V, N - K + 1+1 * LDV + o_v, LDV, ref WORK, offset_work, LDWORK);
                            // *
                            // *              C2 := C2 - W
                            // *
                            for (J = 1; J <= K; J++)
                            {
                                C_0 = (N - K + J) * LDC + o_c;
                                WORK_J = J * LDWORK + o_work;
                                for (I = 1; I <= M; I++)
                                {
                                    C[I + C_0] -= WORK[I + WORK_J];
                                }
                            }
                        }
                    }
                }
                // *
            }
            else
            {
                if (this._lsame.Run(STOREV, "R"))
                {
                    // *
                    if (this._lsame.Run(DIRECT, "F"))
                    {
                        // *
                        // *           Let  V =  ( V1  V2 )    (V1: first K columns)
                        // *           where  V1  is unit upper triangular.
                        // *
                        if (this._lsame.Run(SIDE, "L"))
                        {
                            // *
                            // *              Form  H * C  or  H' * C  where  C = ( C1 )
                            // *                                                  ( C2 )
                            // *
                            // *              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
                            // *
                            // *              W := C1'
                            // *
                            for (J = 1; J <= K; J++)
                            {
                                this._dcopy.Run(N, C, J+1 * LDC + o_c, LDC, ref WORK, 1+J * LDWORK + o_work, 1);
                            }
                            // *
                            // *              W := W * V1'
                            // *
                            this._dtrmm.Run("Right", "Upper", "Transpose", "Unit", N, K
                                            , ONE, V, offset_v, LDV, ref WORK, offset_work, LDWORK);
                            if (M > K)
                            {
                                // *
                                // *                 W := W + C2'*V2'
                                // *
                                this._dgemm.Run("Transpose", "Transpose", N, K, M - K, ONE
                                                , C, K + 1+1 * LDC + o_c, LDC, V, 1+(K + 1) * LDV + o_v, LDV, ONE, ref WORK, offset_work
                                                , LDWORK);
                            }
                            // *
                            // *              W := W * T'  or  W * T
                            // *
                            this._dtrmm.Run("Right", "Upper", TRANST, "Non-unit", N, K
                                            , ONE, T, offset_t, LDT, ref WORK, offset_work, LDWORK);
                            // *
                            // *              C := C - V' * W'
                            // *
                            if (M > K)
                            {
                                // *
                                // *                 C2 := C2 - V2' * W'
                                // *
                                this._dgemm.Run("Transpose", "Transpose", M - K, N, K,  - ONE
                                                , V, 1+(K + 1) * LDV + o_v, LDV, WORK, offset_work, LDWORK, ONE, ref C, K + 1+1 * LDC + o_c
                                                , LDC);
                            }
                            // *
                            // *              W := W * V1
                            // *
                            this._dtrmm.Run("Right", "Upper", "No transpose", "Unit", N, K
                                            , ONE, V, offset_v, LDV, ref WORK, offset_work, LDWORK);
                            // *
                            // *              C1 := C1 - W'
                            // *
                            for (J = 1; J <= K; J++)
                            {
                                WORK_J = J * LDWORK + o_work;
                                for (I = 1; I <= N; I++)
                                {
                                    C[J+I * LDC + o_c] -= WORK[I + WORK_J];
                                }
                            }
                            // *
                        }
                        else
                        {
                            if (this._lsame.Run(SIDE, "R"))
                            {
                                // *
                                // *              Form  C * H  or  C * H'  where  C = ( C1  C2 )
                                // *
                                // *              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
                                // *
                                // *              W := C1
                                // *
                                for (J = 1; J <= K; J++)
                                {
                                    this._dcopy.Run(M, C, 1+J * LDC + o_c, 1, ref WORK, 1+J * LDWORK + o_work, 1);
                                }
                                // *
                                // *              W := W * V1'
                                // *
                                this._dtrmm.Run("Right", "Upper", "Transpose", "Unit", M, K
                                                , ONE, V, offset_v, LDV, ref WORK, offset_work, LDWORK);
                                if (N > K)
                                {
                                    // *
                                    // *                 W := W + C2 * V2'
                                    // *
                                    this._dgemm.Run("No transpose", "Transpose", M, K, N - K, ONE
                                                    , C, 1+(K + 1) * LDC + o_c, LDC, V, 1+(K + 1) * LDV + o_v, LDV, ONE, ref WORK, offset_work
                                                    , LDWORK);
                                }
                                // *
                                // *              W := W * T  or  W * T'
                                // *
                                this._dtrmm.Run("Right", "Upper", TRANS, "Non-unit", M, K
                                                , ONE, T, offset_t, LDT, ref WORK, offset_work, LDWORK);
                                // *
                                // *              C := C - W * V
                                // *
                                if (N > K)
                                {
                                    // *
                                    // *                 C2 := C2 - W * V2
                                    // *
                                    this._dgemm.Run("No transpose", "No transpose", M, N - K, K,  - ONE
                                                    , WORK, offset_work, LDWORK, V, 1+(K + 1) * LDV + o_v, LDV, ONE, ref C, 1+(K + 1) * LDC + o_c
                                                    , LDC);
                                }
                                // *
                                // *              W := W * V1
                                // *
                                this._dtrmm.Run("Right", "Upper", "No transpose", "Unit", M, K
                                                , ONE, V, offset_v, LDV, ref WORK, offset_work, LDWORK);
                                // *
                                // *              C1 := C1 - W
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
                            }
                        }
                        // *
                    }
                    else
                    {
                        // *
                        // *           Let  V =  ( V1  V2 )    (V2: last K columns)
                        // *           where  V2  is unit lower triangular.
                        // *
                        if (this._lsame.Run(SIDE, "L"))
                        {
                            // *
                            // *              Form  H * C  or  H' * C  where  C = ( C1 )
                            // *                                                  ( C2 )
                            // *
                            // *              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
                            // *
                            // *              W := C2'
                            // *
                            for (J = 1; J <= K; J++)
                            {
                                this._dcopy.Run(N, C, M - K + J+1 * LDC + o_c, LDC, ref WORK, 1+J * LDWORK + o_work, 1);
                            }
                            // *
                            // *              W := W * V2'
                            // *
                            this._dtrmm.Run("Right", "Lower", "Transpose", "Unit", N, K
                                            , ONE, V, 1+(M - K + 1) * LDV + o_v, LDV, ref WORK, offset_work, LDWORK);
                            if (M > K)
                            {
                                // *
                                // *                 W := W + C1'*V1'
                                // *
                                this._dgemm.Run("Transpose", "Transpose", N, K, M - K, ONE
                                                , C, offset_c, LDC, V, offset_v, LDV, ONE, ref WORK, offset_work
                                                , LDWORK);
                            }
                            // *
                            // *              W := W * T'  or  W * T
                            // *
                            this._dtrmm.Run("Right", "Lower", TRANST, "Non-unit", N, K
                                            , ONE, T, offset_t, LDT, ref WORK, offset_work, LDWORK);
                            // *
                            // *              C := C - V' * W'
                            // *
                            if (M > K)
                            {
                                // *
                                // *                 C1 := C1 - V1' * W'
                                // *
                                this._dgemm.Run("Transpose", "Transpose", M - K, N, K,  - ONE
                                                , V, offset_v, LDV, WORK, offset_work, LDWORK, ONE, ref C, offset_c
                                                , LDC);
                            }
                            // *
                            // *              W := W * V2
                            // *
                            this._dtrmm.Run("Right", "Lower", "No transpose", "Unit", N, K
                                            , ONE, V, 1+(M - K + 1) * LDV + o_v, LDV, ref WORK, offset_work, LDWORK);
                            // *
                            // *              C2 := C2 - W'
                            // *
                            for (J = 1; J <= K; J++)
                            {
                                WORK_J = J * LDWORK + o_work;
                                for (I = 1; I <= N; I++)
                                {
                                    C[M - K + J+I * LDC + o_c] -= WORK[I + WORK_J];
                                }
                            }
                            // *
                        }
                        else
                        {
                            if (this._lsame.Run(SIDE, "R"))
                            {
                                // *
                                // *              Form  C * H  or  C * H'  where  C = ( C1  C2 )
                                // *
                                // *              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
                                // *
                                // *              W := C2
                                // *
                                for (J = 1; J <= K; J++)
                                {
                                    this._dcopy.Run(M, C, 1+(N - K + J) * LDC + o_c, 1, ref WORK, 1+J * LDWORK + o_work, 1);
                                }
                                // *
                                // *              W := W * V2'
                                // *
                                this._dtrmm.Run("Right", "Lower", "Transpose", "Unit", M, K
                                                , ONE, V, 1+(N - K + 1) * LDV + o_v, LDV, ref WORK, offset_work, LDWORK);
                                if (N > K)
                                {
                                    // *
                                    // *                 W := W + C1 * V1'
                                    // *
                                    this._dgemm.Run("No transpose", "Transpose", M, K, N - K, ONE
                                                    , C, offset_c, LDC, V, offset_v, LDV, ONE, ref WORK, offset_work
                                                    , LDWORK);
                                }
                                // *
                                // *              W := W * T  or  W * T'
                                // *
                                this._dtrmm.Run("Right", "Lower", TRANS, "Non-unit", M, K
                                                , ONE, T, offset_t, LDT, ref WORK, offset_work, LDWORK);
                                // *
                                // *              C := C - W * V
                                // *
                                if (N > K)
                                {
                                    // *
                                    // *                 C1 := C1 - W * V1
                                    // *
                                    this._dgemm.Run("No transpose", "No transpose", M, N - K, K,  - ONE
                                                    , WORK, offset_work, LDWORK, V, offset_v, LDV, ONE, ref C, offset_c
                                                    , LDC);
                                }
                                // *
                                // *              W := W * V2
                                // *
                                this._dtrmm.Run("Right", "Lower", "No transpose", "Unit", M, K
                                                , ONE, V, 1+(N - K + 1) * LDV + o_v, LDV, ref WORK, offset_work, LDWORK);
                                // *
                                // *              C1 := C1 - W
                                // *
                                for (J = 1; J <= K; J++)
                                {
                                    C_1 = (N - K + J) * LDC + o_c;
                                    WORK_J = J * LDWORK + o_work;
                                    for (I = 1; I <= M; I++)
                                    {
                                        C[I + C_1] -= WORK[I + WORK_J];
                                    }
                                }
                                // *
                            }
                        }
                        // *
                    }
                }
            }
            // *
            return;
            // *
            // *     End of DLARFB
            // *

            #endregion

        }
    }
}
