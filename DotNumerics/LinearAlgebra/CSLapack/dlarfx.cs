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
    /// DLARFX applies a real elementary reflector H to a real m by n
    /// matrix C, from either the left or the right. H is represented in the
    /// form
    /// 
    /// H = I - tau * v * v'
    /// 
    /// where tau is a real scalar and v is a real vector.
    /// 
    /// If tau = 0, then H is taken to be the unit matrix
    /// 
    /// This version uses inline code if H has order .LT. 11.
    /// 
    ///</summary>
    public class DLARFX
    {
    

        #region Dependencies
        
        LSAME _lsame; DGEMV _dgemv; DGER _dger; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 

        #endregion

        public DLARFX(LSAME lsame, DGEMV dgemv, DGER dger)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dgemv = dgemv; this._dger = dger; 

            #endregion

        }
    
        public DLARFX()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dgemv = dgemv; this._dger = dger; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLARFX applies a real elementary reflector H to a real m by n
        /// matrix C, from either the left or the right. H is represented in the
        /// form
        /// 
        /// H = I - tau * v * v'
        /// 
        /// where tau is a real scalar and v is a real vector.
        /// 
        /// If tau = 0, then H is taken to be the unit matrix
        /// 
        /// This version uses inline code if H has order .LT. 11.
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
        /// (input) DOUBLE PRECISION array, dimension (M) if SIDE = 'L'
        /// or (N) if SIDE = 'R'
        /// The vector v in the representation of H.
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
        /// The leading dimension of the array C. LDA .GE. (1,M).
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension
        /// (N) if SIDE = 'L'
        /// or (M) if SIDE = 'R'
        /// WORK is not referenced if H has order .LT. 11.
        ///</param>
        public void Run(string SIDE, int M, int N, double[] V, int offset_v, double TAU, ref double[] C, int offset_c
                         , int LDC, ref double[] WORK, int offset_work)
        {

            #region Variables
            
            int J = 0; double SUM = 0; double T1 = 0; double T10 = 0; double T2 = 0; double T3 = 0; double T4 = 0; double T5 = 0; 
            double T6 = 0;double T7 = 0; double T8 = 0; double T9 = 0; double V1 = 0; double V10 = 0; double V2 = 0; 
            double V3 = 0;double V4 = 0; double V5 = 0; double V6 = 0; double V7 = 0; double V8 = 0; double V9 = 0; 

            #endregion


            #region Implicit Variables
            
            int C_1 = 0; int C_2 = 0; int C_3 = 0; int C_4 = 0; int C_5 = 0; int C_6 = 0; int C_7 = 0; int C_8 = 0; int C_9 = 0; 
            int C_10 = 0;

            #endregion


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
            // *  DLARFX applies a real elementary reflector H to a real m by n
            // *  matrix C, from either the left or the right. H is represented in the
            // *  form
            // *
            // *        H = I - tau * v * v'
            // *
            // *  where tau is a real scalar and v is a real vector.
            // *
            // *  If tau = 0, then H is taken to be the unit matrix
            // *
            // *  This version uses inline code if H has order < 11.
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
            // *  V       (input) DOUBLE PRECISION array, dimension (M) if SIDE = 'L'
            // *                                     or (N) if SIDE = 'R'
            // *          The vector v in the representation of H.
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
            // *          The leading dimension of the array C. LDA >= (1,M).
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension
            // *                      (N) if SIDE = 'L'
            // *                      or (M) if SIDE = 'R'
            // *          WORK is not referenced if H has order < 11.
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

            #endregion


            #region Body
            
            if (TAU == ZERO) return;
            if (this._lsame.Run(SIDE, "L"))
            {
                // *
                // *        Form  H * C, where H has order m.
                // *
                switch (M)
                {
                    case 1: goto LABEL10;
                    case 2: goto LABEL30;
                    case 3: goto LABEL50;
                    case 4: goto LABEL70;
                    case 5: goto LABEL90;
                    case 6: goto LABEL110;
                    case 7: goto LABEL130;
                    case 8: goto LABEL150;
                    case 9: goto LABEL170;
                    case 10: goto LABEL190;
                }
                // *
                // *        Code for general M
                // *
                // *        w := C'*v
                // *
                this._dgemv.Run("Transpose", M, N, ONE, C, offset_c, LDC
                                , V, offset_v, 1, ZERO, ref WORK, offset_work, 1);
                // *
                // *        C := C - tau * v * w'
                // *
                this._dger.Run(M, N,  - TAU, V, offset_v, 1, WORK, offset_work
                               , 1, ref C, offset_c, LDC);
                goto LABEL410;
            LABEL10:;
                // *
                // *        Special code for 1 x 1 Householder
                // *
                T1 = ONE - TAU * V[1 + o_v] * V[1 + o_v];
                for (J = 1; J <= N; J++)
                {
                    C[1+J * LDC + o_c] *= T1;
                }
                goto LABEL410;
            LABEL30:;
                // *
                // *        Special code for 2 x 2 Householder
                // *
                V1 = V[1 + o_v];
                T1 = TAU * V1;
                V2 = V[2 + o_v];
                T2 = TAU * V2;
                for (J = 1; J <= N; J++)
                {
                    SUM = V1 * C[1+J * LDC + o_c] + V2 * C[2+J * LDC + o_c];
                    C[1+J * LDC + o_c] +=  - SUM * T1;
                    C[2+J * LDC + o_c] +=  - SUM * T2;
                }
                goto LABEL410;
            LABEL50:;
                // *
                // *        Special code for 3 x 3 Householder
                // *
                V1 = V[1 + o_v];
                T1 = TAU * V1;
                V2 = V[2 + o_v];
                T2 = TAU * V2;
                V3 = V[3 + o_v];
                T3 = TAU * V3;
                for (J = 1; J <= N; J++)
                {
                    SUM = V1 * C[1+J * LDC + o_c] + V2 * C[2+J * LDC + o_c] + V3 * C[3+J * LDC + o_c];
                    C[1+J * LDC + o_c] +=  - SUM * T1;
                    C[2+J * LDC + o_c] +=  - SUM * T2;
                    C[3+J * LDC + o_c] +=  - SUM * T3;
                }
                goto LABEL410;
            LABEL70:;
                // *
                // *        Special code for 4 x 4 Householder
                // *
                V1 = V[1 + o_v];
                T1 = TAU * V1;
                V2 = V[2 + o_v];
                T2 = TAU * V2;
                V3 = V[3 + o_v];
                T3 = TAU * V3;
                V4 = V[4 + o_v];
                T4 = TAU * V4;
                for (J = 1; J <= N; J++)
                {
                    SUM = V1 * C[1+J * LDC + o_c] + V2 * C[2+J * LDC + o_c] + V3 * C[3+J * LDC + o_c] + V4 * C[4+J * LDC + o_c];
                    C[1+J * LDC + o_c] +=  - SUM * T1;
                    C[2+J * LDC + o_c] +=  - SUM * T2;
                    C[3+J * LDC + o_c] +=  - SUM * T3;
                    C[4+J * LDC + o_c] +=  - SUM * T4;
                }
                goto LABEL410;
            LABEL90:;
                // *
                // *        Special code for 5 x 5 Householder
                // *
                V1 = V[1 + o_v];
                T1 = TAU * V1;
                V2 = V[2 + o_v];
                T2 = TAU * V2;
                V3 = V[3 + o_v];
                T3 = TAU * V3;
                V4 = V[4 + o_v];
                T4 = TAU * V4;
                V5 = V[5 + o_v];
                T5 = TAU * V5;
                for (J = 1; J <= N; J++)
                {
                    SUM = V1 * C[1+J * LDC + o_c] + V2 * C[2+J * LDC + o_c] + V3 * C[3+J * LDC + o_c] + V4 * C[4+J * LDC + o_c] + V5 * C[5+J * LDC + o_c];
                    C[1+J * LDC + o_c] +=  - SUM * T1;
                    C[2+J * LDC + o_c] +=  - SUM * T2;
                    C[3+J * LDC + o_c] +=  - SUM * T3;
                    C[4+J * LDC + o_c] +=  - SUM * T4;
                    C[5+J * LDC + o_c] +=  - SUM * T5;
                }
                goto LABEL410;
            LABEL110:;
                // *
                // *        Special code for 6 x 6 Householder
                // *
                V1 = V[1 + o_v];
                T1 = TAU * V1;
                V2 = V[2 + o_v];
                T2 = TAU * V2;
                V3 = V[3 + o_v];
                T3 = TAU * V3;
                V4 = V[4 + o_v];
                T4 = TAU * V4;
                V5 = V[5 + o_v];
                T5 = TAU * V5;
                V6 = V[6 + o_v];
                T6 = TAU * V6;
                for (J = 1; J <= N; J++)
                {
                    SUM = V1 * C[1+J * LDC + o_c] + V2 * C[2+J * LDC + o_c] + V3 * C[3+J * LDC + o_c] + V4 * C[4+J * LDC + o_c] + V5 * C[5+J * LDC + o_c] + V6 * C[6+J * LDC + o_c];
                    C[1+J * LDC + o_c] +=  - SUM * T1;
                    C[2+J * LDC + o_c] +=  - SUM * T2;
                    C[3+J * LDC + o_c] +=  - SUM * T3;
                    C[4+J * LDC + o_c] +=  - SUM * T4;
                    C[5+J * LDC + o_c] +=  - SUM * T5;
                    C[6+J * LDC + o_c] +=  - SUM * T6;
                }
                goto LABEL410;
            LABEL130:;
                // *
                // *        Special code for 7 x 7 Householder
                // *
                V1 = V[1 + o_v];
                T1 = TAU * V1;
                V2 = V[2 + o_v];
                T2 = TAU * V2;
                V3 = V[3 + o_v];
                T3 = TAU * V3;
                V4 = V[4 + o_v];
                T4 = TAU * V4;
                V5 = V[5 + o_v];
                T5 = TAU * V5;
                V6 = V[6 + o_v];
                T6 = TAU * V6;
                V7 = V[7 + o_v];
                T7 = TAU * V7;
                for (J = 1; J <= N; J++)
                {
                    SUM = V1 * C[1+J * LDC + o_c] + V2 * C[2+J * LDC + o_c] + V3 * C[3+J * LDC + o_c] + V4 * C[4+J * LDC + o_c] + V5 * C[5+J * LDC + o_c] + V6 * C[6+J * LDC + o_c] + V7 * C[7+J * LDC + o_c];
                    C[1+J * LDC + o_c] +=  - SUM * T1;
                    C[2+J * LDC + o_c] +=  - SUM * T2;
                    C[3+J * LDC + o_c] +=  - SUM * T3;
                    C[4+J * LDC + o_c] +=  - SUM * T4;
                    C[5+J * LDC + o_c] +=  - SUM * T5;
                    C[6+J * LDC + o_c] +=  - SUM * T6;
                    C[7+J * LDC + o_c] +=  - SUM * T7;
                }
                goto LABEL410;
            LABEL150:;
                // *
                // *        Special code for 8 x 8 Householder
                // *
                V1 = V[1 + o_v];
                T1 = TAU * V1;
                V2 = V[2 + o_v];
                T2 = TAU * V2;
                V3 = V[3 + o_v];
                T3 = TAU * V3;
                V4 = V[4 + o_v];
                T4 = TAU * V4;
                V5 = V[5 + o_v];
                T5 = TAU * V5;
                V6 = V[6 + o_v];
                T6 = TAU * V6;
                V7 = V[7 + o_v];
                T7 = TAU * V7;
                V8 = V[8 + o_v];
                T8 = TAU * V8;
                for (J = 1; J <= N; J++)
                {
                    SUM = V1 * C[1+J * LDC + o_c] + V2 * C[2+J * LDC + o_c] + V3 * C[3+J * LDC + o_c] + V4 * C[4+J * LDC + o_c] + V5 * C[5+J * LDC + o_c] + V6 * C[6+J * LDC + o_c] + V7 * C[7+J * LDC + o_c] + V8 * C[8+J * LDC + o_c];
                    C[1+J * LDC + o_c] +=  - SUM * T1;
                    C[2+J * LDC + o_c] +=  - SUM * T2;
                    C[3+J * LDC + o_c] +=  - SUM * T3;
                    C[4+J * LDC + o_c] +=  - SUM * T4;
                    C[5+J * LDC + o_c] +=  - SUM * T5;
                    C[6+J * LDC + o_c] +=  - SUM * T6;
                    C[7+J * LDC + o_c] +=  - SUM * T7;
                    C[8+J * LDC + o_c] +=  - SUM * T8;
                }
                goto LABEL410;
            LABEL170:;
                // *
                // *        Special code for 9 x 9 Householder
                // *
                V1 = V[1 + o_v];
                T1 = TAU * V1;
                V2 = V[2 + o_v];
                T2 = TAU * V2;
                V3 = V[3 + o_v];
                T3 = TAU * V3;
                V4 = V[4 + o_v];
                T4 = TAU * V4;
                V5 = V[5 + o_v];
                T5 = TAU * V5;
                V6 = V[6 + o_v];
                T6 = TAU * V6;
                V7 = V[7 + o_v];
                T7 = TAU * V7;
                V8 = V[8 + o_v];
                T8 = TAU * V8;
                V9 = V[9 + o_v];
                T9 = TAU * V9;
                for (J = 1; J <= N; J++)
                {
                    SUM = V1 * C[1+J * LDC + o_c] + V2 * C[2+J * LDC + o_c] + V3 * C[3+J * LDC + o_c] + V4 * C[4+J * LDC + o_c] + V5 * C[5+J * LDC + o_c] + V6 * C[6+J * LDC + o_c] + V7 * C[7+J * LDC + o_c] + V8 * C[8+J * LDC + o_c] + V9 * C[9+J * LDC + o_c];
                    C[1+J * LDC + o_c] +=  - SUM * T1;
                    C[2+J * LDC + o_c] +=  - SUM * T2;
                    C[3+J * LDC + o_c] +=  - SUM * T3;
                    C[4+J * LDC + o_c] +=  - SUM * T4;
                    C[5+J * LDC + o_c] +=  - SUM * T5;
                    C[6+J * LDC + o_c] +=  - SUM * T6;
                    C[7+J * LDC + o_c] +=  - SUM * T7;
                    C[8+J * LDC + o_c] +=  - SUM * T8;
                    C[9+J * LDC + o_c] +=  - SUM * T9;
                }
                goto LABEL410;
            LABEL190:;
                // *
                // *        Special code for 10 x 10 Householder
                // *
                V1 = V[1 + o_v];
                T1 = TAU * V1;
                V2 = V[2 + o_v];
                T2 = TAU * V2;
                V3 = V[3 + o_v];
                T3 = TAU * V3;
                V4 = V[4 + o_v];
                T4 = TAU * V4;
                V5 = V[5 + o_v];
                T5 = TAU * V5;
                V6 = V[6 + o_v];
                T6 = TAU * V6;
                V7 = V[7 + o_v];
                T7 = TAU * V7;
                V8 = V[8 + o_v];
                T8 = TAU * V8;
                V9 = V[9 + o_v];
                T9 = TAU * V9;
                V10 = V[10 + o_v];
                T10 = TAU * V10;
                for (J = 1; J <= N; J++)
                {
                    SUM = V1 * C[1+J * LDC + o_c] + V2 * C[2+J * LDC + o_c] + V3 * C[3+J * LDC + o_c] + V4 * C[4+J * LDC + o_c] + V5 * C[5+J * LDC + o_c] + V6 * C[6+J * LDC + o_c] + V7 * C[7+J * LDC + o_c] + V8 * C[8+J * LDC + o_c] + V9 * C[9+J * LDC + o_c] + V10 * C[10+J * LDC + o_c];
                    C[1+J * LDC + o_c] +=  - SUM * T1;
                    C[2+J * LDC + o_c] +=  - SUM * T2;
                    C[3+J * LDC + o_c] +=  - SUM * T3;
                    C[4+J * LDC + o_c] +=  - SUM * T4;
                    C[5+J * LDC + o_c] +=  - SUM * T5;
                    C[6+J * LDC + o_c] +=  - SUM * T6;
                    C[7+J * LDC + o_c] +=  - SUM * T7;
                    C[8+J * LDC + o_c] +=  - SUM * T8;
                    C[9+J * LDC + o_c] +=  - SUM * T9;
                    C[10+J * LDC + o_c] +=  - SUM * T10;
                }
                goto LABEL410;
            }
            else
            {
                // *
                // *        Form  C * H, where H has order n.
                // *
                switch (N)
                {
                    case 1: goto LABEL210;
                    case 2: goto LABEL230;
                    case 3: goto LABEL250;
                    case 4: goto LABEL270;
                    case 5: goto LABEL290;
                    case 6: goto LABEL310;
                    case 7: goto LABEL330;
                    case 8: goto LABEL350;
                    case 9: goto LABEL370;
                    case 10: goto LABEL390;
                }
                // *
                // *        Code for general N
                // *
                // *        w := C * v
                // *
                this._dgemv.Run("No transpose", M, N, ONE, C, offset_c, LDC
                                , V, offset_v, 1, ZERO, ref WORK, offset_work, 1);
                // *
                // *        C := C - tau * w * v'
                // *
                this._dger.Run(M, N,  - TAU, WORK, offset_work, 1, V, offset_v
                               , 1, ref C, offset_c, LDC);
                goto LABEL410;
            LABEL210:;
                // *
                // *        Special code for 1 x 1 Householder
                // *
                T1 = ONE - TAU * V[1 + o_v] * V[1 + o_v];
                C_1 = 1 * LDC + o_c;
                for (J = 1; J <= M; J++)
                {
                    C[J + C_1] *= T1;
                }
                goto LABEL410;
            LABEL230:;
                // *
                // *        Special code for 2 x 2 Householder
                // *
                V1 = V[1 + o_v];
                T1 = TAU * V1;
                V2 = V[2 + o_v];
                T2 = TAU * V2;
                C_1 = 1 * LDC + o_c;
                C_2 = 2 * LDC + o_c;
                for (J = 1; J <= M; J++)
                {
                    SUM = V1 * C[J + C_1] + V2 * C[J + C_2];
                    C[J + C_1] +=  - SUM * T1;
                    C[J + C_2] +=  - SUM * T2;
                }
                goto LABEL410;
            LABEL250:;
                // *
                // *        Special code for 3 x 3 Householder
                // *
                V1 = V[1 + o_v];
                T1 = TAU * V1;
                V2 = V[2 + o_v];
                T2 = TAU * V2;
                V3 = V[3 + o_v];
                T3 = TAU * V3;
                C_1 = 1 * LDC + o_c;
                C_2 = 2 * LDC + o_c;
                C_3 = 3 * LDC + o_c;
                for (J = 1; J <= M; J++)
                {
                    SUM = V1 * C[J + C_1] + V2 * C[J + C_2] + V3 * C[J + C_3];
                    C[J + C_1] +=  - SUM * T1;
                    C[J + C_2] +=  - SUM * T2;
                    C[J + C_3] +=  - SUM * T3;
                }
                goto LABEL410;
            LABEL270:;
                // *
                // *        Special code for 4 x 4 Householder
                // *
                V1 = V[1 + o_v];
                T1 = TAU * V1;
                V2 = V[2 + o_v];
                T2 = TAU * V2;
                V3 = V[3 + o_v];
                T3 = TAU * V3;
                V4 = V[4 + o_v];
                T4 = TAU * V4;
                C_1 = 1 * LDC + o_c;
                C_2 = 2 * LDC + o_c;
                C_3 = 3 * LDC + o_c;
                C_4 = 4 * LDC + o_c;
                for (J = 1; J <= M; J++)
                {
                    SUM = V1 * C[J + C_1] + V2 * C[J + C_2] + V3 * C[J + C_3] + V4 * C[J + C_4];
                    C[J + C_1] +=  - SUM * T1;
                    C[J + C_2] +=  - SUM * T2;
                    C[J + C_3] +=  - SUM * T3;
                    C[J + C_4] +=  - SUM * T4;
                }
                goto LABEL410;
            LABEL290:;
                // *
                // *        Special code for 5 x 5 Householder
                // *
                V1 = V[1 + o_v];
                T1 = TAU * V1;
                V2 = V[2 + o_v];
                T2 = TAU * V2;
                V3 = V[3 + o_v];
                T3 = TAU * V3;
                V4 = V[4 + o_v];
                T4 = TAU * V4;
                V5 = V[5 + o_v];
                T5 = TAU * V5;
                C_1 = 1 * LDC + o_c;
                C_2 = 2 * LDC + o_c;
                C_3 = 3 * LDC + o_c;
                C_4 = 4 * LDC + o_c;
                C_5 = 5 * LDC + o_c;
                for (J = 1; J <= M; J++)
                {
                    SUM = V1 * C[J + C_1] + V2 * C[J + C_2] + V3 * C[J + C_3] + V4 * C[J + C_4] + V5 * C[J + C_5];
                    C[J + C_1] +=  - SUM * T1;
                    C[J + C_2] +=  - SUM * T2;
                    C[J + C_3] +=  - SUM * T3;
                    C[J + C_4] +=  - SUM * T4;
                    C[J + C_5] +=  - SUM * T5;
                }
                goto LABEL410;
            LABEL310:;
                // *
                // *        Special code for 6 x 6 Householder
                // *
                V1 = V[1 + o_v];
                T1 = TAU * V1;
                V2 = V[2 + o_v];
                T2 = TAU * V2;
                V3 = V[3 + o_v];
                T3 = TAU * V3;
                V4 = V[4 + o_v];
                T4 = TAU * V4;
                V5 = V[5 + o_v];
                T5 = TAU * V5;
                V6 = V[6 + o_v];
                T6 = TAU * V6;
                C_1 = 1 * LDC + o_c;
                C_2 = 2 * LDC + o_c;
                C_3 = 3 * LDC + o_c;
                C_4 = 4 * LDC + o_c;
                C_5 = 5 * LDC + o_c;
                C_6 = 6 * LDC + o_c;
                for (J = 1; J <= M; J++)
                {
                    SUM = V1 * C[J + C_1] + V2 * C[J + C_2] + V3 * C[J + C_3] + V4 * C[J + C_4] + V5 * C[J + C_5] + V6 * C[J + C_6];
                    C[J + C_1] +=  - SUM * T1;
                    C[J + C_2] +=  - SUM * T2;
                    C[J + C_3] +=  - SUM * T3;
                    C[J + C_4] +=  - SUM * T4;
                    C[J + C_5] +=  - SUM * T5;
                    C[J + C_6] +=  - SUM * T6;
                }
                goto LABEL410;
            LABEL330:;
                // *
                // *        Special code for 7 x 7 Householder
                // *
                V1 = V[1 + o_v];
                T1 = TAU * V1;
                V2 = V[2 + o_v];
                T2 = TAU * V2;
                V3 = V[3 + o_v];
                T3 = TAU * V3;
                V4 = V[4 + o_v];
                T4 = TAU * V4;
                V5 = V[5 + o_v];
                T5 = TAU * V5;
                V6 = V[6 + o_v];
                T6 = TAU * V6;
                V7 = V[7 + o_v];
                T7 = TAU * V7;
                C_1 = 1 * LDC + o_c;
                C_2 = 2 * LDC + o_c;
                C_3 = 3 * LDC + o_c;
                C_4 = 4 * LDC + o_c;
                C_5 = 5 * LDC + o_c;
                C_6 = 6 * LDC + o_c;
                C_7 = 7 * LDC + o_c;
                for (J = 1; J <= M; J++)
                {
                    SUM = V1 * C[J + C_1] + V2 * C[J + C_2] + V3 * C[J + C_3] + V4 * C[J + C_4] + V5 * C[J + C_5] + V6 * C[J + C_6] + V7 * C[J + C_7];
                    C[J + C_1] +=  - SUM * T1;
                    C[J + C_2] +=  - SUM * T2;
                    C[J + C_3] +=  - SUM * T3;
                    C[J + C_4] +=  - SUM * T4;
                    C[J + C_5] +=  - SUM * T5;
                    C[J + C_6] +=  - SUM * T6;
                    C[J + C_7] +=  - SUM * T7;
                }
                goto LABEL410;
            LABEL350:;
                // *
                // *        Special code for 8 x 8 Householder
                // *
                V1 = V[1 + o_v];
                T1 = TAU * V1;
                V2 = V[2 + o_v];
                T2 = TAU * V2;
                V3 = V[3 + o_v];
                T3 = TAU * V3;
                V4 = V[4 + o_v];
                T4 = TAU * V4;
                V5 = V[5 + o_v];
                T5 = TAU * V5;
                V6 = V[6 + o_v];
                T6 = TAU * V6;
                V7 = V[7 + o_v];
                T7 = TAU * V7;
                V8 = V[8 + o_v];
                T8 = TAU * V8;
                C_1 = 1 * LDC + o_c;
                C_2 = 2 * LDC + o_c;
                C_3 = 3 * LDC + o_c;
                C_4 = 4 * LDC + o_c;
                C_5 = 5 * LDC + o_c;
                C_6 = 6 * LDC + o_c;
                C_7 = 7 * LDC + o_c;
                C_8 = 8 * LDC + o_c;
                for (J = 1; J <= M; J++)
                {
                    SUM = V1 * C[J + C_1] + V2 * C[J + C_2] + V3 * C[J + C_3] + V4 * C[J + C_4] + V5 * C[J + C_5] + V6 * C[J + C_6] + V7 * C[J + C_7] + V8 * C[J + C_8];
                    C[J + C_1] +=  - SUM * T1;
                    C[J + C_2] +=  - SUM * T2;
                    C[J + C_3] +=  - SUM * T3;
                    C[J + C_4] +=  - SUM * T4;
                    C[J + C_5] +=  - SUM * T5;
                    C[J + C_6] +=  - SUM * T6;
                    C[J + C_7] +=  - SUM * T7;
                    C[J + C_8] +=  - SUM * T8;
                }
                goto LABEL410;
            LABEL370:;
                // *
                // *        Special code for 9 x 9 Householder
                // *
                V1 = V[1 + o_v];
                T1 = TAU * V1;
                V2 = V[2 + o_v];
                T2 = TAU * V2;
                V3 = V[3 + o_v];
                T3 = TAU * V3;
                V4 = V[4 + o_v];
                T4 = TAU * V4;
                V5 = V[5 + o_v];
                T5 = TAU * V5;
                V6 = V[6 + o_v];
                T6 = TAU * V6;
                V7 = V[7 + o_v];
                T7 = TAU * V7;
                V8 = V[8 + o_v];
                T8 = TAU * V8;
                V9 = V[9 + o_v];
                T9 = TAU * V9;
                C_1 = 1 * LDC + o_c;
                C_2 = 2 * LDC + o_c;
                C_3 = 3 * LDC + o_c;
                C_4 = 4 * LDC + o_c;
                C_5 = 5 * LDC + o_c;
                C_6 = 6 * LDC + o_c;
                C_7 = 7 * LDC + o_c;
                C_8 = 8 * LDC + o_c;
                C_9 = 9 * LDC + o_c;
                for (J = 1; J <= M; J++)
                {
                    SUM = V1 * C[J + C_1] + V2 * C[J + C_2] + V3 * C[J + C_3] + V4 * C[J + C_4] + V5 * C[J + C_5] + V6 * C[J + C_6] + V7 * C[J + C_7] + V8 * C[J + C_8] + V9 * C[J + C_9];
                    C[J + C_1] +=  - SUM * T1;
                    C[J + C_2] +=  - SUM * T2;
                    C[J + C_3] +=  - SUM * T3;
                    C[J + C_4] +=  - SUM * T4;
                    C[J + C_5] +=  - SUM * T5;
                    C[J + C_6] +=  - SUM * T6;
                    C[J + C_7] +=  - SUM * T7;
                    C[J + C_8] +=  - SUM * T8;
                    C[J + C_9] +=  - SUM * T9;
                }
                goto LABEL410;
            LABEL390:;
                // *
                // *        Special code for 10 x 10 Householder
                // *
                V1 = V[1 + o_v];
                T1 = TAU * V1;
                V2 = V[2 + o_v];
                T2 = TAU * V2;
                V3 = V[3 + o_v];
                T3 = TAU * V3;
                V4 = V[4 + o_v];
                T4 = TAU * V4;
                V5 = V[5 + o_v];
                T5 = TAU * V5;
                V6 = V[6 + o_v];
                T6 = TAU * V6;
                V7 = V[7 + o_v];
                T7 = TAU * V7;
                V8 = V[8 + o_v];
                T8 = TAU * V8;
                V9 = V[9 + o_v];
                T9 = TAU * V9;
                V10 = V[10 + o_v];
                T10 = TAU * V10;
                C_1 = 1 * LDC + o_c;
                C_2 = 2 * LDC + o_c;
                C_3 = 3 * LDC + o_c;
                C_4 = 4 * LDC + o_c;
                C_5 = 5 * LDC + o_c;
                C_6 = 6 * LDC + o_c;
                C_7 = 7 * LDC + o_c;
                C_8 = 8 * LDC + o_c;
                C_9 = 9 * LDC + o_c;
                C_10 = 10 * LDC + o_c;
                for (J = 1; J <= M; J++)
                {
                    SUM = V1 * C[J + C_1] + V2 * C[J + C_2] + V3 * C[J + C_3] + V4 * C[J + C_4] + V5 * C[J + C_5] + V6 * C[J + C_6] + V7 * C[J + C_7] + V8 * C[J + C_8] + V9 * C[J + C_9] + V10 * C[J + C_10];
                    C[J + C_1] +=  - SUM * T1;
                    C[J + C_2] +=  - SUM * T2;
                    C[J + C_3] +=  - SUM * T3;
                    C[J + C_4] +=  - SUM * T4;
                    C[J + C_5] +=  - SUM * T5;
                    C[J + C_6] +=  - SUM * T6;
                    C[J + C_7] +=  - SUM * T7;
                    C[J + C_8] +=  - SUM * T8;
                    C[J + C_9] +=  - SUM * T9;
                    C[J + C_10] +=  - SUM * T10;
                }
                goto LABEL410;
            }
        LABEL410:;
            return;
            // *
            // *     End of DLARFX
            // *

            #endregion

        }
    }
}
