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

namespace DotNumerics.Optimization.LBFGSB
{
    public class DPMEPS
    {
    

        #region Variables
        
        double ZERO = 0; double ONE = 0; double TWO = 0; 

        #endregion

        public DPMEPS()
        {
    

            #region Data Initialization
            
            //ZERO,ONE,TWO/0.0D0,1.0D0,2.0D0
            ZERO = 0.0E0;
            ONE = 1.0E0;
            TWO = 2.0E0;

            #endregion

        }
    
        public double Run()
        {
        double dpmeps = 0;

            #region Variables
            
            int I = 0; int IBETA = 0; int IRND = 0; int IT = 0; int ITEMP = 0; int NEGEP = 0; double A = 0; double B = 0; 
            double BETA = 0;double BETAIN = 0; double BETAH = 0; double TEMP = 0; double TEMPA = 0; double TEMP1 = 0; 

            #endregion


            #region Prolog
            
            // c     **********
            // c
            // c     Subroutine dpeps
            // c
            // c     This subroutine computes the machine precision parameter
            // c     dpmeps as the smallest floating point number such that
            // c     1 + dpmeps differs from 1.
            // c
            // c     This subroutine is based on the subroutine machar described in
            // c
            // c     W. J. Cody,
            // c     MACHAR: A subroutine to dynamically determine machine parameters,
            // c     ACM Transactions on Mathematical Software, 14, 1988, pages 303-311.
            // c
            // c     The subroutine statement is:
            // c
            // c       subroutine dpeps(dpmeps)
            // c
            // c     where
            // c
            // c       dpmeps is a double precision variable.
            // c         On entry dpmeps need not be specified.
            // c         On exit dpmeps is the machine precision.
            // c
            // c     MINPACK-2 Project. February 1991.
            // c     Argonne National Laboratory and University of Minnesota.
            // c     Brett M. Averick.
            // c
            // c     *******
            
            // c     determine ibeta, beta ala malcolm.
            

            #endregion


            #region Body
            
            A = ONE;
            B = ONE;
        LABEL10:;
            A += A;
            TEMP = A + ONE;
            TEMP1 = TEMP - A;
            if (TEMP1 - ONE == ZERO) goto LABEL10;
        LABEL20:;
            B += B;
            TEMP = A + B;
            ITEMP = Convert.ToInt32(Math.Truncate(TEMP - A));
            if (ITEMP == 0) goto LABEL20;
            IBETA = ITEMP;
            BETA = Convert.ToDouble(IBETA);
            
            // c     determine it, irnd.
            
            IT = 0;
            B = ONE;
        LABEL30:;
            IT += 1;
            B *= BETA;
            TEMP = B + ONE;
            TEMP1 = TEMP - B;
            if (TEMP1 - ONE == ZERO) goto LABEL30;
            IRND = 0;
            BETAH = BETA / TWO;
            TEMP = A + BETAH;
            if (TEMP - A != ZERO) IRND = 1;
            TEMPA = A + BETA;
            TEMP = TEMPA + BETAH;
            if ((IRND == 0) && (TEMP - TEMPA != ZERO)) IRND = 2;
            
            // c     determine dpmeps.
            
            NEGEP = IT + 3;
            BETAIN = ONE / BETA;
            A = ONE;
            for (I = 1; I <= NEGEP; I++)
            {
                A *= BETAIN;
            }
        LABEL50:;
            TEMP = ONE + A;
            if (TEMP - ONE != ZERO) goto LABEL60;
            A *= BETA;
            goto LABEL50;
        LABEL60:;
            dpmeps = A;
            if ((IBETA == 2) || (IRND == 0)) goto LABEL70;
            A = (A * (ONE + A)) / TWO;
            TEMP = ONE + A;
            if (TEMP - ONE != ZERO) dpmeps = A;
            
        LABEL70:  return dpmeps;
            
        return dpmeps;

            #endregion

        }
    }
    
    // c====================== The end of dpmeps ==============================
}
