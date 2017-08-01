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
    /// DLAR2V applies a vector of real plane rotations from both sides to
    /// a sequence of 2-by-2 real symmetric matrices, defined by the elements
    /// of the vectors x, y and z. For i = 1,2,...,n
    /// 
    /// ( x(i)  z(i) ) := (  c(i)  s(i) ) ( x(i)  z(i) ) ( c(i) -s(i) )
    /// ( z(i)  y(i) )    ( -s(i)  c(i) ) ( z(i)  y(i) ) ( s(i)  c(i) )
    /// 
    ///</summary>
    public class DLAR2V
    {
    
        public DLAR2V()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAR2V applies a vector of real plane rotations from both sides to
        /// a sequence of 2-by-2 real symmetric matrices, defined by the elements
        /// of the vectors x, y and z. For i = 1,2,...,n
        /// 
        /// ( x(i)  z(i) ) := (  c(i)  s(i) ) ( x(i)  z(i) ) ( c(i) -s(i) )
        /// ( z(i)  y(i) )    ( -s(i)  c(i) ) ( z(i)  y(i) ) ( s(i)  c(i) )
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of plane rotations to be applied.
        ///</param>
        /// <param name="X">
        /// (input/output) DOUBLE PRECISION array,
        /// dimension (1+(N-1)*INCX)
        /// The vector x.
        ///</param>
        /// <param name="Y">
        /// (input/output) DOUBLE PRECISION array,
        /// dimension (1+(N-1)*INCX)
        /// The vector y.
        ///</param>
        /// <param name="Z">
        /// (input/output) DOUBLE PRECISION array,
        /// dimension (1+(N-1)*INCX)
        /// The vector z.
        ///</param>
        /// <param name="INCX">
        /// (input) INTEGER
        /// The increment between elements of X, Y and Z. INCX .GT. 0.
        ///</param>
        /// <param name="C">
        /// (input) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
        /// The cosines of the plane rotations.
        ///</param>
        /// <param name="S">
        /// (input) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
        /// The sines of the plane rotations.
        ///</param>
        /// <param name="INCC">
        /// (input) INTEGER
        /// The increment between elements of C and S. INCC .GT. 0.
        ///</param>
        public void Run(int N, ref double[] X, int offset_x, ref double[] Y, int offset_y, ref double[] Z, int offset_z, int INCX, double[] C, int offset_c
                         , double[] S, int offset_s, int INCC)
        {

            #region Variables
            
            int I = 0; int IC = 0; int IX = 0; double CI = 0; double SI = 0; double T1 = 0; double T2 = 0; double T3 = 0; 
            double T4 = 0;double T5 = 0; double T6 = 0; double XI = 0; double YI = 0; double ZI = 0; 

            #endregion


            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_y = -1 + offset_y;  int o_z = -1 + offset_z;  int o_c = -1 + offset_c; 
             int o_s = -1 + offset_s;

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
            // *  DLAR2V applies a vector of real plane rotations from both sides to
            // *  a sequence of 2-by-2 real symmetric matrices, defined by the elements
            // *  of the vectors x, y and z. For i = 1,2,...,n
            // *
            // *     ( x(i)  z(i) ) := (  c(i)  s(i) ) ( x(i)  z(i) ) ( c(i) -s(i) )
            // *     ( z(i)  y(i) )    ( -s(i)  c(i) ) ( z(i)  y(i) ) ( s(i)  c(i) )
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N       (input) INTEGER
            // *          The number of plane rotations to be applied.
            // *
            // *  X       (input/output) DOUBLE PRECISION array,
            // *                         dimension (1+(N-1)*INCX)
            // *          The vector x.
            // *
            // *  Y       (input/output) DOUBLE PRECISION array,
            // *                         dimension (1+(N-1)*INCX)
            // *          The vector y.
            // *
            // *  Z       (input/output) DOUBLE PRECISION array,
            // *                         dimension (1+(N-1)*INCX)
            // *          The vector z.
            // *
            // *  INCX    (input) INTEGER
            // *          The increment between elements of X, Y and Z. INCX > 0.
            // *
            // *  C       (input) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
            // *          The cosines of the plane rotations.
            // *
            // *  S       (input) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
            // *          The sines of the plane rotations.
            // *
            // *  INCC    (input) INTEGER
            // *          The increment between elements of C and S. INCC > 0.
            // *
            // *  =====================================================================
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            IX = 1;
            IC = 1;
            for (I = 1; I <= N; I++)
            {
                XI = X[IX + o_x];
                YI = Y[IX + o_y];
                ZI = Z[IX + o_z];
                CI = C[IC + o_c];
                SI = S[IC + o_s];
                T1 = SI * ZI;
                T2 = CI * ZI;
                T3 = T2 - SI * XI;
                T4 = T2 + SI * YI;
                T5 = CI * XI + T1;
                T6 = CI * YI - T1;
                X[IX + o_x] = CI * T5 + SI * T4;
                Y[IX + o_y] = CI * T6 - SI * T3;
                Z[IX + o_z] = CI * T4 - SI * T5;
                IX += INCX;
                IC += INCC;
            }
            // *
            // *     End of DLAR2V
            // *
            return;

            #endregion

        }
    }
}
