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
    /// DLASQ6 computes one dqd (shift equal to zero) transform in
    /// ping-pong form, with protection against underflow and overflow.
    /// 
    ///</summary>
    public class DLASQ6
    {
    

        #region Dependencies
        
        DLAMCH _dlamch; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; 

        #endregion

        public DLASQ6(DLAMCH dlamch)
        {
    

            #region Set Dependencies
            
            this._dlamch = dlamch; 

            #endregion

        }
    
        public DLASQ6()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);

            #endregion


            #region Set Dependencies
            
            this._dlamch = dlamch; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLASQ6 computes one dqd (shift equal to zero) transform in
        /// ping-pong form, with protection against underflow and overflow.
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
        /// Z holds the qd array. EMIN is stored in Z(4*N0) to avoid
        /// an extra argument.
        ///</param>
        /// <param name="PP">
        /// (input) INTEGER
        /// PP=0 for ping, PP=1 for pong.
        ///</param>
        /// <param name="DMIN">
        /// (output) DOUBLE PRECISION
        /// Minimum value of d.
        ///</param>
        /// <param name="DMIN1">
        /// (output) DOUBLE PRECISION
        /// Minimum value of d, excluding D( N0 ).
        ///</param>
        /// <param name="DMIN2">
        /// (output) DOUBLE PRECISION
        /// Minimum value of d, excluding D( N0 ) and D( N0-1 ).
        ///</param>
        /// <param name="DN">
        /// (output) DOUBLE PRECISION
        /// d(N0), the last value of d.
        ///</param>
        /// <param name="DNM1">
        /// (output) DOUBLE PRECISION
        /// d(N0-1).
        ///</param>
        /// <param name="DNM2">
        /// (output) DOUBLE PRECISION
        /// d(N0-2).
        ///</param>
        public void Run(int I0, int N0, ref double[] Z, int offset_z, int PP, ref double DMIN, ref double DMIN1
                         , ref double DMIN2, ref double DN, ref double DNM1, ref double DNM2)
        {

            #region Variables
            
            int J4 = 0; int J4P2 = 0; double D = 0; double EMIN = 0; double SAFMIN = 0; double TEMP = 0; 

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
            // *  DLASQ6 computes one dqd (shift equal to zero) transform in
            // *  ping-pong form, with protection against underflow and overflow.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  I0    (input) INTEGER
            // *        First index.
            // *
            // *  N0    (input) INTEGER
            // *        Last index.
            // *
            // *  Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
            // *        Z holds the qd array. EMIN is stored in Z(4*N0) to avoid
            // *        an extra argument.
            // *
            // *  PP    (input) INTEGER
            // *        PP=0 for ping, PP=1 for pong.
            // *
            // *  DMIN  (output) DOUBLE PRECISION
            // *        Minimum value of d.
            // *
            // *  DMIN1 (output) DOUBLE PRECISION
            // *        Minimum value of d, excluding D( N0 ).
            // *
            // *  DMIN2 (output) DOUBLE PRECISION
            // *        Minimum value of d, excluding D( N0 ) and D( N0-1 ).
            // *
            // *  DN    (output) DOUBLE PRECISION
            // *        d(N0), the last value of d.
            // *
            // *  DNM1  (output) DOUBLE PRECISION
            // *        d(N0-1).
            // *
            // *  DNM2  (output) DOUBLE PRECISION
            // *        d(N0-2).
            // *
            // *  =====================================================================
            // *
            // *     .. Parameter ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Function ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MIN;
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            if ((N0 - I0 - 1) <= 0) return;
            // *
            SAFMIN = this._dlamch.Run("Safe minimum");
            J4 = 4 * I0 + PP - 3;
            EMIN = Z[J4 + 4 + o_z];
            D = Z[J4 + o_z];
            DMIN = D;
            // *
            if (PP == 0)
            {
                for (J4 = 4 * I0; J4 <= 4 * (N0 - 3); J4 += 4)
                {
                    Z[J4 - 2 + o_z] = D + Z[J4 - 1 + o_z];
                    if (Z[J4 - 2 + o_z] == ZERO)
                    {
                        Z[J4 + o_z] = ZERO;
                        D = Z[J4 + 1 + o_z];
                        DMIN = D;
                        EMIN = ZERO;
                    }
                    else
                    {
                        if (SAFMIN * Z[J4 + 1 + o_z] < Z[J4 - 2 + o_z] && SAFMIN * Z[J4 - 2 + o_z] < Z[J4 + 1 + o_z])
                        {
                            TEMP = Z[J4 + 1 + o_z] / Z[J4 - 2 + o_z];
                            Z[J4 + o_z] = Z[J4 - 1 + o_z] * TEMP;
                            D *= TEMP;
                        }
                        else
                        {
                            Z[J4 + o_z] = Z[J4 + 1 + o_z] * (Z[J4 - 1 + o_z] / Z[J4 - 2 + o_z]);
                            D = Z[J4 + 1 + o_z] * (D / Z[J4 - 2 + o_z]);
                        }
                    }
                    DMIN = Math.Min(DMIN, D);
                    EMIN = Math.Min(EMIN, Z[J4 + o_z]);
                }
            }
            else
            {
                for (J4 = 4 * I0; J4 <= 4 * (N0 - 3); J4 += 4)
                {
                    Z[J4 - 3 + o_z] = D + Z[J4 + o_z];
                    if (Z[J4 - 3 + o_z] == ZERO)
                    {
                        Z[J4 - 1 + o_z] = ZERO;
                        D = Z[J4 + 2 + o_z];
                        DMIN = D;
                        EMIN = ZERO;
                    }
                    else
                    {
                        if (SAFMIN * Z[J4 + 2 + o_z] < Z[J4 - 3 + o_z] && SAFMIN * Z[J4 - 3 + o_z] < Z[J4 + 2 + o_z])
                        {
                            TEMP = Z[J4 + 2 + o_z] / Z[J4 - 3 + o_z];
                            Z[J4 - 1 + o_z] = Z[J4 + o_z] * TEMP;
                            D *= TEMP;
                        }
                        else
                        {
                            Z[J4 - 1 + o_z] = Z[J4 + 2 + o_z] * (Z[J4 + o_z] / Z[J4 - 3 + o_z]);
                            D = Z[J4 + 2 + o_z] * (D / Z[J4 - 3 + o_z]);
                        }
                    }
                    DMIN = Math.Min(DMIN, D);
                    EMIN = Math.Min(EMIN, Z[J4 - 1 + o_z]);
                }
            }
            // *
            // *     Unroll last two steps. 
            // *
            DNM2 = D;
            DMIN2 = DMIN;
            J4 = 4 * (N0 - 2) - PP;
            J4P2 = J4 + 2 * PP - 1;
            Z[J4 - 2 + o_z] = DNM2 + Z[J4P2 + o_z];
            if (Z[J4 - 2 + o_z] == ZERO)
            {
                Z[J4 + o_z] = ZERO;
                DNM1 = Z[J4P2 + 2 + o_z];
                DMIN = DNM1;
                EMIN = ZERO;
            }
            else
            {
                if (SAFMIN * Z[J4P2 + 2 + o_z] < Z[J4 - 2 + o_z] && SAFMIN * Z[J4 - 2 + o_z] < Z[J4P2 + 2 + o_z])
                {
                    TEMP = Z[J4P2 + 2 + o_z] / Z[J4 - 2 + o_z];
                    Z[J4 + o_z] = Z[J4P2 + o_z] * TEMP;
                    DNM1 = DNM2 * TEMP;
                }
                else
                {
                    Z[J4 + o_z] = Z[J4P2 + 2 + o_z] * (Z[J4P2 + o_z] / Z[J4 - 2 + o_z]);
                    DNM1 = Z[J4P2 + 2 + o_z] * (DNM2 / Z[J4 - 2 + o_z]);
                }
            }
            DMIN = Math.Min(DMIN, DNM1);
            // *
            DMIN1 = DMIN;
            J4 += 4;
            J4P2 = J4 + 2 * PP - 1;
            Z[J4 - 2 + o_z] = DNM1 + Z[J4P2 + o_z];
            if (Z[J4 - 2 + o_z] == ZERO)
            {
                Z[J4 + o_z] = ZERO;
                DN = Z[J4P2 + 2 + o_z];
                DMIN = DN;
                EMIN = ZERO;
            }
            else
            {
                if (SAFMIN * Z[J4P2 + 2 + o_z] < Z[J4 - 2 + o_z] && SAFMIN * Z[J4 - 2 + o_z] < Z[J4P2 + 2 + o_z])
                {
                    TEMP = Z[J4P2 + 2 + o_z] / Z[J4 - 2 + o_z];
                    Z[J4 + o_z] = Z[J4P2 + o_z] * TEMP;
                    DN = DNM1 * TEMP;
                }
                else
                {
                    Z[J4 + o_z] = Z[J4P2 + 2 + o_z] * (Z[J4P2 + o_z] / Z[J4 - 2 + o_z]);
                    DN = Z[J4P2 + 2 + o_z] * (DNM1 / Z[J4 - 2 + o_z]);
                }
            }
            DMIN = Math.Min(DMIN, DN);
            // *
            Z[J4 + 2 + o_z] = DN;
            Z[4 * N0 - PP + o_z] = EMIN;
            return;
            // *
            // *     End of DLASQ6
            // *

            #endregion

        }
    }
}
