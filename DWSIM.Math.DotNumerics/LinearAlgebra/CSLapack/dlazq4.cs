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
    /// DLAZQ4 computes an approximation TAU to the smallest eigenvalue 
    /// using values of d from the previous transform.
    /// 
    /// I0    (input) INTEGER
    /// First index.
    /// 
    /// N0    (input) INTEGER
    /// Last index.
    /// 
    /// Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
    /// Z holds the qd array.
    /// 
    /// PP    (input) INTEGER
    /// PP=0 for ping, PP=1 for pong.
    /// 
    /// N0IN  (input) INTEGER
    /// The value of N0 at start of EIGTEST.
    /// 
    /// DMIN  (input) DOUBLE PRECISION
    /// Minimum value of d.
    /// 
    /// DMIN1 (input) DOUBLE PRECISION
    /// Minimum value of d, excluding D( N0 ).
    /// 
    /// DMIN2 (input) DOUBLE PRECISION
    /// Minimum value of d, excluding D( N0 ) and D( N0-1 ).
    /// 
    /// DN    (input) DOUBLE PRECISION
    /// d(N)
    /// 
    /// DN1   (input) DOUBLE PRECISION
    /// d(N-1)
    /// 
    /// DN2   (input) DOUBLE PRECISION
    /// d(N-2)
    /// 
    /// TAU   (output) DOUBLE PRECISION
    /// This is the shift.
    /// 
    /// TTYPE (output) INTEGER
    /// Shift type.
    /// 
    /// G     (input/output) DOUBLE PRECISION
    /// G is passed as an argument in order to save its value between
    /// calls to DLAZQ4
    /// 
    ///</summary>
    public class DLAZQ4
    {
    

        #region Variables
        
        const double CNST1 = 0.5630E0; const double CNST2 = 1.010E0; const double CNST3 = 1.050E0; const double QURTR = 0.250E0; 
        const double THIRD = 0.3330E0;const double HALF = 0.50E0; const double ZERO = 0.0E0; const double ONE = 1.0E0; 
        const double TWO = 2.0E0;const double HUNDRD = 100.0E0; 

        #endregion

        public DLAZQ4()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAZQ4 computes an approximation TAU to the smallest eigenvalue 
        /// using values of d from the previous transform.
        /// 
        /// I0    (input) INTEGER
        /// First index.
        /// 
        /// N0    (input) INTEGER
        /// Last index.
        /// 
        /// Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
        /// Z holds the qd array.
        /// 
        /// PP    (input) INTEGER
        /// PP=0 for ping, PP=1 for pong.
        /// 
        /// N0IN  (input) INTEGER
        /// The value of N0 at start of EIGTEST.
        /// 
        /// DMIN  (input) DOUBLE PRECISION
        /// Minimum value of d.
        /// 
        /// DMIN1 (input) DOUBLE PRECISION
        /// Minimum value of d, excluding D( N0 ).
        /// 
        /// DMIN2 (input) DOUBLE PRECISION
        /// Minimum value of d, excluding D( N0 ) and D( N0-1 ).
        /// 
        /// DN    (input) DOUBLE PRECISION
        /// d(N)
        /// 
        /// DN1   (input) DOUBLE PRECISION
        /// d(N-1)
        /// 
        /// DN2   (input) DOUBLE PRECISION
        /// d(N-2)
        /// 
        /// TAU   (output) DOUBLE PRECISION
        /// This is the shift.
        /// 
        /// TTYPE (output) INTEGER
        /// Shift type.
        /// 
        /// G     (input/output) DOUBLE PRECISION
        /// G is passed as an argument in order to save its value between
        /// calls to DLAZQ4
        /// 
        ///</summary>
        /// <param name="I0">
        /// (input) INTEGER
        /// First index.
        ///</param>
        /// <param name="N0">
        /// (input) INTEGER
        /// Last index.
        ///</param>
        /// <param name="Z">
        /// (input) DOUBLE PRECISION array, dimension ( 4*N )
        /// Z holds the qd array.
        ///</param>
        /// <param name="PP">
        /// (input) INTEGER
        /// PP=0 for ping, PP=1 for pong.
        ///</param>
        /// <param name="N0IN">
        /// (input) INTEGER
        /// The value of N0 at start of EIGTEST.
        ///</param>
        /// <param name="DMIN">
        /// (input) DOUBLE PRECISION
        /// Minimum value of d.
        ///</param>
        /// <param name="DMIN1">
        /// (input) DOUBLE PRECISION
        /// Minimum value of d, excluding D( N0 ).
        ///</param>
        /// <param name="DMIN2">
        /// (input) DOUBLE PRECISION
        /// Minimum value of d, excluding D( N0 ) and D( N0-1 ).
        ///</param>
        /// <param name="DN">
        /// (input) DOUBLE PRECISION
        /// d(N)
        ///</param>
        /// <param name="DN1">
        /// (input) DOUBLE PRECISION
        /// d(N-1)
        ///</param>
        /// <param name="DN2">
        /// (input) DOUBLE PRECISION
        /// d(N-2)
        ///</param>
        /// <param name="TAU">
        /// (output) DOUBLE PRECISION
        /// This is the shift.
        ///</param>
        /// <param name="TTYPE">
        /// (output) INTEGER
        /// Shift type.
        ///</param>
        /// <param name="G">
        /// (input/output) DOUBLE PRECISION
        /// G is passed as an argument in order to save its value between
        /// calls to DLAZQ4
        ///</param>
        public void Run(int I0, int N0, double[] Z, int offset_z, int PP, int N0IN, double DMIN
                         , double DMIN1, double DMIN2, double DN, double DN1, double DN2, ref double TAU
                         , ref int TTYPE, ref double G)
        {

            #region Variables
            
            int I4 = 0; int NN = 0; int NP = 0; double A2 = 0; double B1 = 0; double B2 = 0; double GAM = 0; double GAP1 = 0; 
            double GAP2 = 0;double S = 0; 

            #endregion


            #region Array Index Correction
            
             int o_z = -1 + offset_z; 

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
            // *  DLAZQ4 computes an approximation TAU to the smallest eigenvalue 
            // *  using values of d from the previous transform.
            // *
            // *  I0    (input) INTEGER
            // *        First index.
            // *
            // *  N0    (input) INTEGER
            // *        Last index.
            // *
            // *  Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
            // *        Z holds the qd array.
            // *
            // *  PP    (input) INTEGER
            // *        PP=0 for ping, PP=1 for pong.
            // *
            // *  N0IN  (input) INTEGER
            // *        The value of N0 at start of EIGTEST.
            // *
            // *  DMIN  (input) DOUBLE PRECISION
            // *        Minimum value of d.
            // *
            // *  DMIN1 (input) DOUBLE PRECISION
            // *        Minimum value of d, excluding D( N0 ).
            // *
            // *  DMIN2 (input) DOUBLE PRECISION
            // *        Minimum value of d, excluding D( N0 ) and D( N0-1 ).
            // *
            // *  DN    (input) DOUBLE PRECISION
            // *        d(N)
            // *
            // *  DN1   (input) DOUBLE PRECISION
            // *        d(N-1)
            // *
            // *  DN2   (input) DOUBLE PRECISION
            // *        d(N-2)
            // *
            // *  TAU   (output) DOUBLE PRECISION
            // *        This is the shift.
            // *
            // *  TTYPE (output) INTEGER
            // *        Shift type.
            // *
            // *  G     (input/output) DOUBLE PRECISION
            // *        G is passed as an argument in order to save its value between
            // *        calls to DLAZQ4
            // *
            // *  Further Details
            // *  ===============
            // *  CNST1 = 9/16
            // *
            // *  This is a thread safe version of DLASQ4, which passes G through the
            // *  argument list in place of declaring G in a SAVE statment.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MAX, MIN, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     A negative DMIN forces the shift to take that absolute value
            // *     TTYPE records the type of shift.
            // *

            #endregion


            #region Body
            
            if (DMIN <= ZERO)
            {
                TAU =  - DMIN;
                TTYPE =  - 1;
                return;
            }
            // *       
            NN = 4 * N0 + PP;
            if (N0IN == N0)
            {
                // *
                // *        No eigenvalues deflated.
                // *
                if (DMIN == DN || DMIN == DN1)
                {
                    // *
                    B1 = Math.Sqrt(Z[NN - 3 + o_z]) * Math.Sqrt(Z[NN - 5 + o_z]);
                    B2 = Math.Sqrt(Z[NN - 7 + o_z]) * Math.Sqrt(Z[NN - 9 + o_z]);
                    A2 = Z[NN - 7 + o_z] + Z[NN - 5 + o_z];
                    // *
                    // *           Cases 2 and 3.
                    // *
                    if (DMIN == DN && DMIN1 == DN1)
                    {
                        GAP2 = DMIN2 - A2 - DMIN2 * QURTR;
                        if (GAP2 > ZERO && GAP2 > B2)
                        {
                            GAP1 = A2 - DN - (B2 / GAP2) * B2;
                        }
                        else
                        {
                            GAP1 = A2 - DN - (B1 + B2);
                        }
                        if (GAP1 > ZERO && GAP1 > B1)
                        {
                            S = Math.Max(DN - (B1 / GAP1) * B1, HALF * DMIN);
                            TTYPE =  - 2;
                        }
                        else
                        {
                            S = ZERO;
                            if (DN > B1) S = DN - B1;
                            if (A2 > (B1 + B2)) S = Math.Min(S, A2 - (B1 + B2));
                            S = Math.Max(S, THIRD * DMIN);
                            TTYPE =  - 3;
                        }
                    }
                    else
                    {
                        // *
                        // *              Case 4.
                        // *
                        TTYPE =  - 4;
                        S = QURTR * DMIN;
                        if (DMIN == DN)
                        {
                            GAM = DN;
                            A2 = ZERO;
                            if (Z[NN - 5 + o_z] > Z[NN - 7 + o_z]) return;
                            B2 = Z[NN - 5 + o_z] / Z[NN - 7 + o_z];
                            NP = NN - 9;
                        }
                        else
                        {
                            NP = NN - 2 * PP;
                            B2 = Z[NP - 2 + o_z];
                            GAM = DN1;
                            if (Z[NP - 4 + o_z] > Z[NP - 2 + o_z]) return;
                            A2 = Z[NP - 4 + o_z] / Z[NP - 2 + o_z];
                            if (Z[NN - 9 + o_z] > Z[NN - 11 + o_z]) return;
                            B2 = Z[NN - 9 + o_z] / Z[NN - 11 + o_z];
                            NP = NN - 13;
                        }
                        // *
                        // *              Approximate contribution to norm squared from I < NN-1.
                        // *
                        A2 += B2;
                        for (I4 = NP; I4 >= 4 * I0 - 1 + PP; I4 +=  - 4)
                        {
                            if (B2 == ZERO) goto LABEL20;
                            B1 = B2;
                            if (Z[I4 + o_z] > Z[I4 - 2 + o_z]) return;
                            B2 = B2 * (Z[I4 + o_z] / Z[I4 - 2 + o_z]);
                            A2 += B2;
                            if (HUNDRD * Math.Max(B2, B1) < A2 || CNST1 < A2) goto LABEL20;
                        }
                    LABEL20:;
                        A2 *= CNST3;
                        // *
                        // *              Rayleigh quotient residual bound.
                        // *
                        if (A2 < CNST1) S = GAM * (ONE - Math.Sqrt(A2)) / (ONE + A2);
                    }
                }
                else
                {
                    if (DMIN == DN2)
                    {
                        // *
                        // *           Case 5.
                        // *
                        TTYPE =  - 5;
                        S = QURTR * DMIN;
                        // *
                        // *           Compute contribution to norm squared from I > NN-2.
                        // *
                        NP = NN - 2 * PP;
                        B1 = Z[NP - 2 + o_z];
                        B2 = Z[NP - 6 + o_z];
                        GAM = DN2;
                        if (Z[NP - 8 + o_z] > B2 || Z[NP - 4 + o_z] > B1) return;
                        A2 = (Z[NP - 8 + o_z] / B2) * (ONE + Z[NP - 4 + o_z] / B1);
                        // *
                        // *           Approximate contribution to norm squared from I < NN-2.
                        // *
                        if (N0 - I0 > 2)
                        {
                            B2 = Z[NN - 13 + o_z] / Z[NN - 15 + o_z];
                            A2 += B2;
                            for (I4 = NN - 17; I4 >= 4 * I0 - 1 + PP; I4 +=  - 4)
                            {
                                if (B2 == ZERO) goto LABEL40;
                                B1 = B2;
                                if (Z[I4 + o_z] > Z[I4 - 2 + o_z]) return;
                                B2 = B2 * (Z[I4 + o_z] / Z[I4 - 2 + o_z]);
                                A2 += B2;
                                if (HUNDRD * Math.Max(B2, B1) < A2 || CNST1 < A2) goto LABEL40;
                            }
                        LABEL40:;
                            A2 *= CNST3;
                        }
                        // *
                        if (A2 < CNST1) S = GAM * (ONE - Math.Sqrt(A2)) / (ONE + A2);
                    }
                    else
                    {
                        // *
                        // *           Case 6, no information to guide us.
                        // *
                        if (TTYPE ==  - 6)
                        {
                            G += THIRD * (ONE - G);
                        }
                        else
                        {
                            if (TTYPE ==  - 18)
                            {
                                G = QURTR * THIRD;
                            }
                            else
                            {
                                G = QURTR;
                            }
                        }
                        S = G * DMIN;
                        TTYPE =  - 6;
                    }
                }
                // *
            }
            else
            {
                if (N0IN == (N0 + 1))
                {
                    // *
                    // *        One eigenvalue just deflated. Use DMIN1, DN1 for DMIN and DN.
                    // *
                    if (DMIN1 == DN1 && DMIN2 == DN2)
                    {
                        // *
                        // *           Cases 7 and 8.
                        // *
                        TTYPE =  - 7;
                        S = THIRD * DMIN1;
                        if (Z[NN - 5 + o_z] > Z[NN - 7 + o_z]) return;
                        B1 = Z[NN - 5 + o_z] / Z[NN - 7 + o_z];
                        B2 = B1;
                        if (B2 == ZERO) goto LABEL60;
                        for (I4 = 4 * N0 - 9 + PP; I4 >= 4 * I0 - 1 + PP; I4 +=  - 4)
                        {
                            A2 = B1;
                            if (Z[I4 + o_z] > Z[I4 - 2 + o_z]) return;
                            B1 = B1 * (Z[I4 + o_z] / Z[I4 - 2 + o_z]);
                            B2 += B1;
                            if (HUNDRD * Math.Max(B1, A2) < B2) goto LABEL60;
                        }
                    LABEL60:;
                        B2 = Math.Sqrt(CNST3 * B2);
                        A2 = DMIN1 / (ONE + Math.Pow(B2,2));
                        GAP2 = HALF * DMIN2 - A2;
                        if (GAP2 > ZERO && GAP2 > B2 * A2)
                        {
                            S = Math.Max(S, A2 * (ONE - CNST2 * A2 * (B2 / GAP2) * B2));
                        }
                        else
                        {
                            S = Math.Max(S, A2 * (ONE - CNST2 * B2));
                            TTYPE =  - 8;
                        }
                    }
                    else
                    {
                        // *
                        // *           Case 9.
                        // *
                        S = QURTR * DMIN1;
                        if (DMIN1 == DN1) S = HALF * DMIN1;
                        TTYPE =  - 9;
                    }
                    // *
                }
                else
                {
                    if (N0IN == (N0 + 2))
                    {
                        // *
                        // *        Two eigenvalues deflated. Use DMIN2, DN2 for DMIN and DN.
                        // *
                        // *        Cases 10 and 11.
                        // *
                        if (DMIN2 == DN2 && TWO * Z[NN - 5 + o_z] < Z[NN - 7 + o_z])
                        {
                            TTYPE =  - 10;
                            S = THIRD * DMIN2;
                            if (Z[NN - 5 + o_z] > Z[NN - 7 + o_z]) return;
                            B1 = Z[NN - 5 + o_z] / Z[NN - 7 + o_z];
                            B2 = B1;
                            if (B2 == ZERO) goto LABEL80;
                            for (I4 = 4 * N0 - 9 + PP; I4 >= 4 * I0 - 1 + PP; I4 +=  - 4)
                            {
                                if (Z[I4 + o_z] > Z[I4 - 2 + o_z]) return;
                                B1 = B1 * (Z[I4 + o_z] / Z[I4 - 2 + o_z]);
                                B2 += B1;
                                if (HUNDRD * B1 < B2) goto LABEL80;
                            }
                        LABEL80:;
                            B2 = Math.Sqrt(CNST3 * B2);
                            A2 = DMIN2 / (ONE + Math.Pow(B2,2));
                            GAP2 = Z[NN - 7 + o_z] + Z[NN - 9 + o_z] - Math.Sqrt(Z[NN - 11 + o_z]) * Math.Sqrt(Z[NN - 9 + o_z]) - A2;
                            if (GAP2 > ZERO && GAP2 > B2 * A2)
                            {
                                S = Math.Max(S, A2 * (ONE - CNST2 * A2 * (B2 / GAP2) * B2));
                            }
                            else
                            {
                                S = Math.Max(S, A2 * (ONE - CNST2 * B2));
                            }
                        }
                        else
                        {
                            S = QURTR * DMIN2;
                            TTYPE =  - 11;
                        }
                    }
                    else
                    {
                        if (N0IN > (N0 + 2))
                        {
                            // *
                            // *        Case 12, more than two eigenvalues deflated. No information.
                            // *
                            S = ZERO;
                            TTYPE =  - 12;
                        }
                    }
                }
            }
            // *
            TAU = S;
            return;
            // *
            // *     End of DLAZQ4
            // *

            #endregion

        }
    }
}
