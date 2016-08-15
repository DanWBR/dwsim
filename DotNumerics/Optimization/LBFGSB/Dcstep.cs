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
    public class DCSTEP
    {
    

        #region Variables
        
        const double ZERO = 0.0E0; const double P66 = 0.66E0; const double TWO = 2.0E0; const double THREE = 3.0E0; 

        #endregion

        public DCSTEP()
        {
    
        }
    
        /// <param name="STX">
        /// is a double precision variable.
        /// On entry stx is the best step obtained so far and is an
        /// endpoint of the interval that contains the minimizer. 
        /// On exit stx is the updated best step.
        ///</param>
        /// <param name="FX">
        /// is a double precision variable.
        /// On entry fx is the function at stx.
        /// On exit fx is the function at stx.
        ///</param>
        /// <param name="DX">
        /// is a double precision variable.
        /// On entry dx is the derivative of the function at 
        /// stx. The derivative must be negative in the direction of 
        /// the step, that is, dx and stp - stx must have opposite 
        /// signs.
        /// On exit dx is the derivative of the function at stx.
        ///</param>
        /// <param name="STY">
        /// is a double precision variable.
        /// On entry sty is the second endpoint of the interval that 
        /// contains the minimizer.
        /// On exit sty is the updated endpoint of the interval that 
        /// contains the minimizer.
        ///</param>
        /// <param name="FY">
        /// is a double precision variable.
        /// On entry fy is the function at sty.
        /// On exit fy is the function at sty.
        ///</param>
        /// <param name="DY">
        /// is a double precision variable.
        /// On entry dy is the derivative of the function at sty.
        /// On exit dy is the derivative of the function at the exit sty.
        ///</param>
        /// <param name="STP">
        /// is a double precision variable.
        /// On entry stp is the current step. If brackt is set to .true.
        /// then on input stp must be between stx and sty. 
        /// On exit stp is a new trial step.
        ///</param>
        /// <param name="FP">
        /// is a double precision variable.
        /// On entry fp is the function at stp
        /// On exit fp is unchanged.
        ///</param>
        /// <param name="DP">
        /// is a double precision variable.
        /// On entry dp is the the derivative of the function at stp.
        /// On exit dp is unchanged.
        ///</param>
        /// <param name="BRACKT">
        /// is an logical variable.
        /// On entry brackt specifies if a minimizer has been bracketed.
        /// Initially brackt must be set to .false.
        /// On exit brackt specifies if a minimizer has been bracketed.
        /// When a minimizer is bracketed brackt is set to .true.
        ///</param>
        /// <param name="STPMIN">
        /// is a double precision variable.
        /// On entry stpmin is a lower bound for the step.
        /// On exit stpmin is unchanged.
        ///</param>
        /// <param name="STPMAX">
        /// is a double precision variable.
        /// On entry stpmax is an upper bound for the step.
        /// On exit stpmax is unchanged.
        ///</param>
        public void Run(ref double STX, ref double FX, ref double DX, ref double STY, ref double FY, ref double DY
                         , ref double STP, double FP, double DP, ref bool BRACKT, double STPMIN, double STPMAX)
        {

            #region Variables
            
            double GAMMA = 0; double P = 0; double Q = 0; double R = 0; double S = 0; double SGND = 0; double STPC = 0; 
            double STPF = 0;double STPQ = 0; double THETA = 0; 

            #endregion


            #region Prolog
            
            // c     **********
            // c
            // c     Subroutine dcstep
            // c
            // c     This subroutine computes a safeguarded step for a search
            // c     procedure and updates an interval that contains a step that
            // c     satisfies a sufficient decrease and a curvature condition.
            // c
            // c     The parameter stx contains the step with the least function
            // c     value. If brackt is set to .true. then a minimizer has
            // c     been bracketed in an interval with endpoints stx and sty.
            // c     The parameter stp contains the current step. 
            // c     The subroutine assumes that if brackt is set to .true. then
            // c
            // c           min(stx,sty) < stp < max(stx,sty),
            // c
            // c     and that the derivative at stx is negative in the direction 
            // c     of the step.
            // c
            // c     The subroutine statement is
            // c
            // c       subroutine dcstep(stx,fx,dx,sty,fy,dy,stp,fp,dp,brackt,
            // c                         stpmin,stpmax)
            // c
            // c     where
            // c
            // c       stx is a double precision variable.
            // c         On entry stx is the best step obtained so far and is an
            // c            endpoint of the interval that contains the minimizer. 
            // c         On exit stx is the updated best step.
            // c
            // c       fx is a double precision variable.
            // c         On entry fx is the function at stx.
            // c         On exit fx is the function at stx.
            // c
            // c       dx is a double precision variable.
            // c         On entry dx is the derivative of the function at 
            // c            stx. The derivative must be negative in the direction of 
            // c            the step, that is, dx and stp - stx must have opposite 
            // c            signs.
            // c         On exit dx is the derivative of the function at stx.
            // c
            // c       sty is a double precision variable.
            // c         On entry sty is the second endpoint of the interval that 
            // c            contains the minimizer.
            // c         On exit sty is the updated endpoint of the interval that 
            // c            contains the minimizer.
            // c
            // c       fy is a double precision variable.
            // c         On entry fy is the function at sty.
            // c         On exit fy is the function at sty.
            // c
            // c       dy is a double precision variable.
            // c         On entry dy is the derivative of the function at sty.
            // c         On exit dy is the derivative of the function at the exit sty.
            // c
            // c       stp is a double precision variable.
            // c         On entry stp is the current step. If brackt is set to .true.
            // c            then on input stp must be between stx and sty. 
            // c         On exit stp is a new trial step.
            // c
            // c       fp is a double precision variable.
            // c         On entry fp is the function at stp
            // c         On exit fp is unchanged.
            // c
            // c       dp is a double precision variable.
            // c         On entry dp is the the derivative of the function at stp.
            // c         On exit dp is unchanged.
            // c
            // c       brackt is an logical variable.
            // c         On entry brackt specifies if a minimizer has been bracketed.
            // c            Initially brackt must be set to .false.
            // c         On exit brackt specifies if a minimizer has been bracketed.
            // c            When a minimizer is bracketed brackt is set to .true.
            // c
            // c       stpmin is a double precision variable.
            // c         On entry stpmin is a lower bound for the step.
            // c         On exit stpmin is unchanged.
            // c
            // c       stpmax is a double precision variable.
            // c         On entry stpmax is an upper bound for the step.
            // c         On exit stpmax is unchanged.
            // c
            // c     MINPACK-1 Project. June 1983
            // c     Argonne National Laboratory. 
            // c     Jorge J. More' and David J. Thuente.
            // c
            // c     MINPACK-2 Project. October 1993.
            // c     Argonne National Laboratory and University of Minnesota. 
            // c     Brett M. Averick and Jorge J. More'.
            // c
            // c     **********
            
            

            #endregion


            #region Body
            
            SGND = DP * (DX / Math.Abs(DX));
            
            // c     First case: A higher function value. The minimum is bracketed. 
            // c     If the cubic step is closer to stx than the quadratic step, the 
            // c     cubic step is taken, otherwise the average of the cubic and 
            // c     quadratic steps is taken.
            
            if (FP > FX)
            {
                THETA = THREE * (FX - FP) / (STP - STX) + DX + DP;
                S = Math.Max(Math.Abs(THETA), Math.Max(Math.Abs(DX), Math.Abs(DP)));
                GAMMA = S * Math.Sqrt(Math.Pow(THETA / S,2) - (DX / S) * (DP / S));
                if (STP < STX) GAMMA =  - GAMMA;
                P = (GAMMA - DX) + THETA;
                Q = ((GAMMA - DX) + GAMMA) + DP;
                R = P / Q;
                STPC = STX + R * (STP - STX);
                STPQ = STX + ((DX / ((FX - FP) / (STP - STX) + DX)) / TWO) * (STP - STX);
                if (Math.Abs(STPC - STX) < Math.Abs(STPQ - STX))
                {
                    STPF = STPC;
                }
                else
                {
                    STPF = STPC + (STPQ - STPC) / TWO;
                }
                BRACKT = true;
                
                // c     Second case: A lower function value and derivatives of opposite 
                // c     sign. The minimum is bracketed. If the cubic step is farther from 
                // c     stp than the secant step, the cubic step is taken, otherwise the 
                // c     secant step is taken.
                
            }
            else
            {
                if (SGND < ZERO)
                {
                    THETA = THREE * (FX - FP) / (STP - STX) + DX + DP;
                    S = Math.Max(Math.Abs(THETA), Math.Max(Math.Abs(DX), Math.Abs(DP)));
                    GAMMA = S * Math.Sqrt(Math.Pow(THETA / S,2) - (DX / S) * (DP / S));
                    if (STP > STX) GAMMA =  - GAMMA;
                    P = (GAMMA - DP) + THETA;
                    Q = ((GAMMA - DP) + GAMMA) + DX;
                    R = P / Q;
                    STPC = STP + R * (STX - STP);
                    STPQ = STP + (DP / (DP - DX)) * (STX - STP);
                    if (Math.Abs(STPC - STP) > Math.Abs(STPQ - STP))
                    {
                        STPF = STPC;
                    }
                    else
                    {
                        STPF = STPQ;
                    }
                    BRACKT = true;
                    
                    // c     Third case: A lower function value, derivatives of the same sign,
                    // c     and the magnitude of the derivative decreases.
                    
                }
                else
                {
                    if (Math.Abs(DP) < Math.Abs(DX))
                    {
                        
                        // c        The cubic step is computed only if the cubic tends to infinity 
                        // c        in the direction of the step or if the minimum of the cubic
                        // c        is beyond stp. Otherwise the cubic step is defined to be the 
                        // c        secant step.
                        
                        THETA = THREE * (FX - FP) / (STP - STX) + DX + DP;
                        S = Math.Max(Math.Abs(THETA), Math.Max(Math.Abs(DX), Math.Abs(DP)));
                        
                        // c        The case gamma = 0 only arises if the cubic does not tend
                        // c        to infinity in the direction of the step.
                        
                        GAMMA = S * Math.Sqrt(Math.Max(ZERO, Math.Pow(THETA / S,2) - (DX / S) * (DP / S)));
                        if (STP > STX) GAMMA =  - GAMMA;
                        P = (GAMMA - DP) + THETA;
                        Q = (GAMMA + (DX - DP)) + GAMMA;
                        R = P / Q;
                        if (R < ZERO && GAMMA != ZERO)
                        {
                            STPC = STP + R * (STX - STP);
                        }
                        else
                        {
                            if (STP > STX)
                            {
                                STPC = STPMAX;
                            }
                            else
                            {
                                STPC = STPMIN;
                            }
                        }
                        STPQ = STP + (DP / (DP - DX)) * (STX - STP);
                        
                        if (BRACKT)
                        {
                            
                            // c           A minimizer has been bracketed. If the cubic step is 
                            // c           closer to stp than the secant step, the cubic step is 
                            // c           taken, otherwise the secant step is taken.
                            
                            if (Math.Abs(STPC - STP) < Math.Abs(STPQ - STP))
                            {
                                STPF = STPC;
                            }
                            else
                            {
                                STPF = STPQ;
                            }
                            if (STP > STX)
                            {
                                STPF = Math.Min(STP + P66 * (STY - STP), STPF);
                            }
                            else
                            {
                                STPF = Math.Max(STP + P66 * (STY - STP), STPF);
                            }
                        }
                        else
                        {
                            
                            // c           A minimizer has not been bracketed. If the cubic step is 
                            // c           farther from stp than the secant step, the cubic step is 
                            // c           taken, otherwise the secant step is taken.
                            
                            if (Math.Abs(STPC - STP) > Math.Abs(STPQ - STP))
                            {
                                STPF = STPC;
                            }
                            else
                            {
                                STPF = STPQ;
                            }
                            STPF = Math.Min(STPMAX, STPF);
                            STPF = Math.Max(STPMIN, STPF);
                        }
                        
                        // c     Fourth case: A lower function value, derivatives of the same sign, 
                        // c     and the magnitude of the derivative does not decrease. If the 
                        // c     minimum is not bracketed, the step is either stpmin or stpmax, 
                        // c     otherwise the cubic step is taken.
                        
                    }
                    else
                    {
                        if (BRACKT)
                        {
                            THETA = THREE * (FP - FY) / (STY - STP) + DY + DP;
                            S = Math.Max(Math.Abs(THETA), Math.Max(Math.Abs(DY), Math.Abs(DP)));
                            GAMMA = S * Math.Sqrt(Math.Pow(THETA / S,2) - (DY / S) * (DP / S));
                            if (STP > STY) GAMMA =  - GAMMA;
                            P = (GAMMA - DP) + THETA;
                            Q = ((GAMMA - DP) + GAMMA) + DY;
                            R = P / Q;
                            STPC = STP + R * (STY - STP);
                            STPF = STPC;
                        }
                        else
                        {
                            if (STP > STX)
                            {
                                STPF = STPMAX;
                            }
                            else
                            {
                                STPF = STPMIN;
                            }
                        }
                    }
                }
            }
            
            // c     Update the interval which contains a minimizer.
            
            if (FP > FX)
            {
                STY = STP;
                FY = FP;
                DY = DP;
            }
            else
            {
                if (SGND < ZERO)
                {
                    STY = STX;
                    FY = FX;
                    DY = DX;
                }
                STX = STP;
                FX = FP;
                DX = DP;
            }
            
            // c     Compute the new step.
            
            STP = STPF;
            

            #endregion

        }
    }
    
    // c====================== The end of dcstep ==============================
}
