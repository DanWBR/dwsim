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
    public class DCSRCH
    {
    

        #region Dependencies
        
        DCSTEP _dcstep; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double P5 = 0.5E0; const double P66 = 0.66E0; const double XTRAPL = 1.1E0; 
        const double XTRAPU = 4.0E0;

        #endregion

        public DCSRCH(DCSTEP dcstep)
        {
    

            #region Set Dependencies
            
            this._dcstep = dcstep; 

            #endregion

        }
    
        public DCSRCH()
        {
    

            #region Dependencies (Initialization)
            
            DCSTEP dcstep = new DCSTEP();

            #endregion


            #region Set Dependencies
            
            this._dcstep = dcstep; 

            #endregion

        }
        /// <param name="F">
        /// is a double precision variable.
        /// On initial entry f is the value of the function at 0.
        /// On subsequent entries f is the value of the 
        /// function at stp.
        /// On exit f is the value of the function at stp.
        ///</param>
        /// <param name="G">
        /// is a double precision variable.
        /// On initial entry g is the derivative of the function at 0.
        /// On subsequent entries g is the derivative of the 
        /// function at stp.
        /// On exit g is the derivative of the function at stp.
        /// 
        /// stp is a double precision variable. 
        /// On entry stp is the current estimate of a satisfactory 
        /// step. On initial entry, a positive initial estimate 
        /// must be provided. 
        /// On exit stp is the current estimate of a satisfactory step
        /// if task = 'FG'. If task = 'CONV' then stp satisfies
        /// the sufficient decrease and curvature condition.
        /// 
        /// ftol is a double precision variable.
        /// On entry ftol specifies a nonnegative tolerance for the 
        /// sufficient decrease condition.
        /// On exit ftol is unchanged.
        /// 
        /// gtol is a double precision variable.
        /// On entry gtol specifies a nonnegative tolerance for the 
        /// curvature condition. 
        /// On exit gtol is unchanged.
        /// 
        /// xtol is a double precision variable.
        /// On entry xtol specifies a nonnegative relative tolerance
        /// for an acceptable step. The subroutine exits with a
        /// warning if the relative difference between sty and stx
        /// is less than xtol.
        /// On exit xtol is unchanged.
        /// 
        /// stpmin is a double precision variable.
        /// On entry stpmin is a nonnegative lower bound for the step.
        /// On exit stpmin is unchanged.
        /// 
        /// stpmax is a double precision variable.
        /// On entry stpmax is a nonnegative upper bound for the step.
        /// On exit stpmax is unchanged.
        /// 
        /// task is a character variable of length at least 60.
        /// On initial entry task must be set to 'START'.
        /// On exit task indicates the required action:
        /// 
        /// If task(1:2) = 'FG' then evaluate the function and 
        /// derivative at stp and call dcsrch again.
        /// 
        /// If task(1:4) = 'CONV' then the search is successful.
        /// 
        /// If task(1:4) = 'WARN' then the subroutine is not able
        /// to satisfy the convergence conditions. The exit value of
        /// stp contains the best point found during the search.
        /// 
        /// If task(1:5) = 'ERROR' then there is an error in the
        /// input arguments.
        /// 
        /// On exit with convergence, a warning or an error, the
        /// variable task contains additional information.
        /// 
        /// isave is an integer work array of dimension 2.
        /// 
        /// dsave is a double precision work array of dimension 13.
        /// 
        /// Subprograms called
        /// 
        /// MINPACK-2 ... dcstep
        /// 
        /// MINPACK-1 Project. June 1983.
        /// Argonne National Laboratory. 
        /// Jorge J. More' and David J. Thuente.
        /// 
        /// MINPACK-2 Project. October 1993.
        /// Argonne National Laboratory and University of Minnesota. 
        /// Brett M. Averick, Richard G. Carter, and Jorge J. More'. 
        /// 
        /// **********
        /// 
        /// 
        /// 
        /// 
        /// Initialization block.
        ///</param>
        /// <param name="STP">
        /// is a double precision variable. 
        /// On entry stp is the current estimate of a satisfactory 
        /// step. On initial entry, a positive initial estimate 
        /// must be provided. 
        /// On exit stp is the current estimate of a satisfactory step
        /// if task = 'FG'. If task = 'CONV' then stp satisfies
        /// the sufficient decrease and curvature condition.
        /// 
        /// ftol is a double precision variable.
        /// On entry ftol specifies a nonnegative tolerance for the 
        /// sufficient decrease condition.
        /// On exit ftol is unchanged.
        /// 
        /// gtol is a double precision variable.
        /// On entry gtol specifies a nonnegative tolerance for the 
        /// curvature condition. 
        /// On exit gtol is unchanged.
        /// 
        /// xtol is a double precision variable.
        /// On entry xtol specifies a nonnegative relative tolerance
        /// for an acceptable step. The subroutine exits with a
        /// warning if the relative difference between sty and stx
        /// is less than xtol.
        /// On exit xtol is unchanged.
        /// 
        /// stpmin is a double precision variable.
        /// On entry stpmin is a nonnegative lower bound for the step.
        /// On exit stpmin is unchanged.
        /// 
        /// stpmax is a double precision variable.
        /// On entry stpmax is a nonnegative upper bound for the step.
        /// On exit stpmax is unchanged.
        /// 
        /// task is a character variable of length at least 60.
        /// On initial entry task must be set to 'START'.
        /// On exit task indicates the required action:
        /// 
        /// If task(1:2) = 'FG' then evaluate the function and 
        /// derivative at stp and call dcsrch again.
        /// 
        /// If task(1:4) = 'CONV' then the search is successful.
        /// 
        /// If task(1:4) = 'WARN' then the subroutine is not able
        /// to satisfy the convergence conditions. The exit value of
        /// stp contains the best point found during the search.
        /// 
        /// If task(1:5) = 'ERROR' then there is an error in the
        /// input arguments.
        /// 
        /// On exit with convergence, a warning or an error, the
        /// variable task contains additional information.
        /// 
        /// isave is an integer work array of dimension 2.
        /// 
        /// dsave is a double precision work array of dimension 13.
        /// 
        /// Subprograms called
        /// 
        /// MINPACK-2 ... dcstep
        /// 
        /// MINPACK-1 Project. June 1983.
        /// Argonne National Laboratory. 
        /// Jorge J. More' and David J. Thuente.
        /// 
        /// MINPACK-2 Project. October 1993.
        /// Argonne National Laboratory and University of Minnesota. 
        /// Brett M. Averick, Richard G. Carter, and Jorge J. More'. 
        /// 
        /// **********
        /// 
        /// 
        /// 
        /// 
        /// Initialization block.
        ///</param>
        /// <param name="FTOL">
        /// is a double precision variable.
        /// On entry ftol specifies a nonnegative tolerance for the 
        /// sufficient decrease condition.
        /// On exit ftol is unchanged.
        ///</param>
        /// <param name="GTOL">
        /// is a double precision variable.
        /// On entry gtol specifies a nonnegative tolerance for the 
        /// curvature condition. 
        /// On exit gtol is unchanged.
        ///</param>
        /// <param name="XTOL">
        /// is a double precision variable.
        /// On entry xtol specifies a nonnegative relative tolerance
        /// for an acceptable step. The subroutine exits with a
        /// warning if the relative difference between sty and stx
        /// is less than xtol.
        /// On exit xtol is unchanged.
        /// 
        /// stpmin is a double precision variable.
        /// On entry stpmin is a nonnegative lower bound for the step.
        /// On exit stpmin is unchanged.
        /// 
        /// stpmax is a double precision variable.
        /// On entry stpmax is a nonnegative upper bound for the step.
        /// On exit stpmax is unchanged.
        /// 
        /// task is a character variable of length at least 60.
        /// On initial entry task must be set to 'START'.
        /// On exit task indicates the required action:
        /// 
        /// If task(1:2) = 'FG' then evaluate the function and 
        /// derivative at stp and call dcsrch again.
        /// 
        /// If task(1:4) = 'CONV' then the search is successful.
        /// 
        /// If task(1:4) = 'WARN' then the subroutine is not able
        /// to satisfy the convergence conditions. The exit value of
        /// stp contains the best point found during the search.
        /// 
        /// If task(1:5) = 'ERROR' then there is an error in the
        /// input arguments.
        /// 
        /// On exit with convergence, a warning or an error, the
        /// variable task contains additional information.
        /// 
        /// isave is an integer work array of dimension 2.
        /// 
        /// dsave is a double precision work array of dimension 13.
        /// 
        /// Subprograms called
        /// 
        /// MINPACK-2 ... dcstep
        /// 
        /// MINPACK-1 Project. June 1983.
        /// Argonne National Laboratory. 
        /// Jorge J. More' and David J. Thuente.
        /// 
        /// MINPACK-2 Project. October 1993.
        /// Argonne National Laboratory and University of Minnesota. 
        /// Brett M. Averick, Richard G. Carter, and Jorge J. More'. 
        /// 
        /// **********
        /// 
        /// 
        /// 
        /// 
        /// Initialization block.
        ///</param>
        /// <param name="STPMIN">
        /// is a double precision variable.
        /// On entry stpmin is a nonnegative lower bound for the step.
        /// On exit stpmin is unchanged.
        /// 
        /// stpmax is a double precision variable.
        /// On entry stpmax is a nonnegative upper bound for the step.
        /// On exit stpmax is unchanged.
        /// 
        /// task is a character variable of length at least 60.
        /// On initial entry task must be set to 'START'.
        /// On exit task indicates the required action:
        /// 
        /// If task(1:2) = 'FG' then evaluate the function and 
        /// derivative at stp and call dcsrch again.
        /// 
        /// If task(1:4) = 'CONV' then the search is successful.
        /// 
        /// If task(1:4) = 'WARN' then the subroutine is not able
        /// to satisfy the convergence conditions. The exit value of
        /// stp contains the best point found during the search.
        /// 
        /// If task(1:5) = 'ERROR' then there is an error in the
        /// input arguments.
        /// 
        /// On exit with convergence, a warning or an error, the
        /// variable task contains additional information.
        /// 
        /// isave is an integer work array of dimension 2.
        /// 
        /// dsave is a double precision work array of dimension 13.
        /// 
        /// Subprograms called
        /// 
        /// MINPACK-2 ... dcstep
        /// 
        /// MINPACK-1 Project. June 1983.
        /// Argonne National Laboratory. 
        /// Jorge J. More' and David J. Thuente.
        /// 
        /// MINPACK-2 Project. October 1993.
        /// Argonne National Laboratory and University of Minnesota. 
        /// Brett M. Averick, Richard G. Carter, and Jorge J. More'. 
        /// 
        /// **********
        /// 
        /// 
        /// 
        /// 
        /// Initialization block.
        ///</param>
        /// <param name="STPMAX">
        /// is a double precision variable.
        /// On entry stpmax is a nonnegative upper bound for the step.
        /// On exit stpmax is unchanged.
        /// 
        /// task is a character variable of length at least 60.
        /// On initial entry task must be set to 'START'.
        /// On exit task indicates the required action:
        /// 
        /// If task(1:2) = 'FG' then evaluate the function and 
        /// derivative at stp and call dcsrch again.
        /// 
        /// If task(1:4) = 'CONV' then the search is successful.
        /// 
        /// If task(1:4) = 'WARN' then the subroutine is not able
        /// to satisfy the convergence conditions. The exit value of
        /// stp contains the best point found during the search.
        /// 
        /// If task(1:5) = 'ERROR' then there is an error in the
        /// input arguments.
        /// 
        /// On exit with convergence, a warning or an error, the
        /// variable task contains additional information.
        /// 
        /// isave is an integer work array of dimension 2.
        /// 
        /// dsave is a double precision work array of dimension 13.
        /// 
        /// Subprograms called
        /// 
        /// MINPACK-2 ... dcstep
        /// 
        /// MINPACK-1 Project. June 1983.
        /// Argonne National Laboratory. 
        /// Jorge J. More' and David J. Thuente.
        /// 
        /// MINPACK-2 Project. October 1993.
        /// Argonne National Laboratory and University of Minnesota. 
        /// Brett M. Averick, Richard G. Carter, and Jorge J. More'. 
        /// 
        /// **********
        /// 
        /// 
        /// 
        /// 
        /// Initialization block.
        ///</param>
        /// <param name="TASK">
        /// = 'START'
        ///</param>
        /// <param name="ISAVE">
        /// is an integer work array of dimension 2.
        ///</param>
        /// <param name="DSAVE">
        /// is a double precision work array of dimension 13.
        ///</param>
        public void Run(double F, double G, ref double STP, double FTOL, double GTOL, double XTOL
                         , double STPMIN, double STPMAX, ref BFGSTask TASK, ref int[] ISAVE, int offset_isave, ref double[] DSAVE, int offset_dsave)
        {

            #region Variables
            
            bool BRACKT = false; int STAGE = 0; double FINIT = 0; double FTEST = 0; double FM = 0; double FX = 0; double FXM = 0; 
            double FY = 0;double FYM = 0; double GINIT = 0; double GTEST = 0; double GM = 0; double GX = 0; double GXM = 0; 
            double GY = 0;double GYM = 0; double STX = 0; double STY = 0; double STMIN = 0; double STMAX = 0; double WIDTH = 0; 
            double WIDTH1 = 0;

            #endregion


            #region Array Index Correction
            
             int o_isave = -1 + offset_isave;  int o_dsave = -1 + offset_dsave; 

            #endregion


            #region Prolog
            
            // c     **********
            // c
            // c     Subroutine dcsrch
            // c
            // c     This subroutine finds a step that satisfies a sufficient
            // c     decrease condition and a curvature condition.
            // c
            // c     Each call of the subroutine updates an interval with 
            // c     endpoints stx and sty. The interval is initially chosen 
            // c     so that it contains a minimizer of the modified function
            // c
            // c           psi(stp) = f(stp) - f(0) - ftol*stp*f'(0).
            // c
            // c     If psi(stp) <= 0 and f'(stp) >= 0 for some step, then the
            // c     interval is chosen so that it contains a minimizer of f. 
            // c
            // c     The algorithm is designed to find a step that satisfies 
            // c     the sufficient decrease condition 
            // c
            // c           f(stp) <= f(0) + ftol*stp*f'(0),
            // c
            // c     and the curvature condition
            // c
            // c           abs(f'(stp)) <= gtol*abs(f'(0)).
            // c
            // c     If ftol is less than gtol and if, for example, the function
            // c     is bounded below, then there is always a step which satisfies
            // c     both conditions. 
            // c
            // c     If no step can be found that satisfies both conditions, then 
            // c     the algorithm stops with a warning. In this case stp only 
            // c     satisfies the sufficient decrease condition.
            // c
            // c     A typical invocation of dcsrch has the following outline:
            // c
            // c     task = 'START'
            // c  10 continue
            // c        call dcsrch( ... )
            // c        if (task .eq. 'FG') then
            // c           Evaluate the function and the gradient at stp 
            // c           goto 10
            // c           end if
            // c
            // c     NOTE: The user must no alter work arrays between calls.
            // c
            // c     The subroutine statement is
            // c
            // c        subroutine dcsrch(f,g,stp,ftol,gtol,xtol,stpmin,stpmax,
            // c                          task,isave,dsave)
            // c     where
            // c
            // c       f is a double precision variable.
            // c         On initial entry f is the value of the function at 0.
            // c            On subsequent entries f is the value of the 
            // c            function at stp.
            // c         On exit f is the value of the function at stp.
            // c
            // c	g is a double precision variable.
            // c         On initial entry g is the derivative of the function at 0.
            // c            On subsequent entries g is the derivative of the 
            // c            function at stp.
            // c         On exit g is the derivative of the function at stp.
            // c
            // c	stp is a double precision variable. 
            // c         On entry stp is the current estimate of a satisfactory 
            // c            step. On initial entry, a positive initial estimate 
            // c            must be provided. 
            // c         On exit stp is the current estimate of a satisfactory step
            // c            if task = 'FG'. If task = 'CONV' then stp satisfies
            // c            the sufficient decrease and curvature condition.
            // c
            // c       ftol is a double precision variable.
            // c         On entry ftol specifies a nonnegative tolerance for the 
            // c            sufficient decrease condition.
            // c         On exit ftol is unchanged.
            // c
            // c       gtol is a double precision variable.
            // c         On entry gtol specifies a nonnegative tolerance for the 
            // c            curvature condition. 
            // c         On exit gtol is unchanged.
            // c
            // c	xtol is a double precision variable.
            // c         On entry xtol specifies a nonnegative relative tolerance
            // c            for an acceptable step. The subroutine exits with a
            // c            warning if the relative difference between sty and stx
            // c            is less than xtol.
            // c         On exit xtol is unchanged.
            // c
            // c	stpmin is a double precision variable.
            // c         On entry stpmin is a nonnegative lower bound for the step.
            // c         On exit stpmin is unchanged.
            // c
            // c	stpmax is a double precision variable.
            // c         On entry stpmax is a nonnegative upper bound for the step.
            // c         On exit stpmax is unchanged.
            // c
            // c       task is a character variable of length at least 60.
            // c         On initial entry task must be set to 'START'.
            // c         On exit task indicates the required action:
            // c
            // c            If task(1:2) = 'FG' then evaluate the function and 
            // c            derivative at stp and call dcsrch again.
            // c
            // c            If task(1:4) = 'CONV' then the search is successful.
            // c
            // c            If task(1:4) = 'WARN' then the subroutine is not able
            // c            to satisfy the convergence conditions. The exit value of
            // c            stp contains the best point found during the search.
            // c
            // c            If task(1:5) = 'ERROR' then there is an error in the
            // c            input arguments.
            // c
            // c         On exit with convergence, a warning or an error, the
            // c            variable task contains additional information.
            // c
            // c       isave is an integer work array of dimension 2.
            // c         
            // c       dsave is a double precision work array of dimension 13.
            // c
            // c     Subprograms called
            // c
            // c	MINPACK-2 ... dcstep
            // c
            // c     MINPACK-1 Project. June 1983.
            // c     Argonne National Laboratory. 
            // c     Jorge J. More' and David J. Thuente.
            // c
            // c     MINPACK-2 Project. October 1993.
            // c     Argonne National Laboratory and University of Minnesota. 
            // c     Brett M. Averick, Richard G. Carter, and Jorge J. More'. 
            // c
            // c     **********
            
            
            
            
            // c     Initialization block.
            

            #endregion


            #region Body

             if (TASK == BFGSTask.START)
            {
                
                // c        Check the input arguments for errors.
                
                //if (STP < STPMIN) FortranLib.Copy(ref TASK , "ERROR: STP .LT. STPMIN");
                //if (STP > STPMAX) FortranLib.Copy(ref TASK , "ERROR: STP .GT. STPMAX");
                //if (G >= ZERO) FortranLib.Copy(ref TASK , "ERROR: INITIAL G .GE. ZERO");
                //if (FTOL < ZERO) FortranLib.Copy(ref TASK , "ERROR: FTOL .LT. ZERO");
                //if (GTOL < ZERO) FortranLib.Copy(ref TASK , "ERROR: GTOL .LT. ZERO");
                //if (XTOL < ZERO) FortranLib.Copy(ref TASK , "ERROR: XTOL .LT. ZERO");
                //if (STPMIN < ZERO) FortranLib.Copy(ref TASK , "ERROR: STPMIN .LT. ZERO");
                //if (STPMAX < STPMIN) FortranLib.Copy(ref TASK , "ERROR: STPMAX .LT. STPMIN");
                if (STP < STPMIN)
                {
                    TASK = BFGSTask.ERROR;;
                }
                if (STP > STPMAX)
                {
                    TASK = BFGSTask.ERROR;
                }
                if (G >= ZERO)
                {
                    TASK = BFGSTask.ERROR;
                }
                if (FTOL < ZERO)
                {
                    TASK = BFGSTask.ERROR;
                }
                if (GTOL < ZERO)
                {
                    TASK = BFGSTask.ERROR;
                }
                if (XTOL < ZERO)
                {
                    TASK = BFGSTask.ERROR;
                }
                if (STPMIN < ZERO)
                {
                    TASK = BFGSTask.ERROR;
                }
                if (STPMAX < STPMIN)
                {
                    TASK = BFGSTask.ERROR;
                }
                
                // c        Exit if there are errors on input.

                if (TASK == BFGSTask.ERROR) return;
                
                // c        Initialize local variables.
                
                BRACKT = false;
                STAGE = 1;
                FINIT = F;
                GINIT = G;
                GTEST = FTOL * GINIT;
                WIDTH = STPMAX - STPMIN;
                WIDTH1 = WIDTH / P5;
                
                // c        The variables stx, fx, gx contain the values of the step, 
                // c        function, and derivative at the best step. 
                // c        The variables sty, fy, gy contain the value of the step, 
                // c        function, and derivative at sty.
                // c        The variables stp, f, g contain the values of the step, 
                // c        function, and derivative at stp.
                
                STX = ZERO;
                FX = FINIT;
                GX = GINIT;
                STY = ZERO;
                FY = FINIT;
                GY = GINIT;
                STMIN = ZERO;
                STMAX = STP + XTRAPU * STP;
                TASK = BFGSTask.FG;
                
                goto LABEL1000;
                
            }
            else
            {
                
                // c        Restore local variables.
                
                if (ISAVE[1 + o_isave] == 1)
                {
                    BRACKT = true;
                }
                else
                {
                    BRACKT = false;
                }
                STAGE = ISAVE[2 + o_isave];
                GINIT = DSAVE[1 + o_dsave];
                GTEST = DSAVE[2 + o_dsave];
                GX = DSAVE[3 + o_dsave];
                GY = DSAVE[4 + o_dsave];
                FINIT = DSAVE[5 + o_dsave];
                FX = DSAVE[6 + o_dsave];
                FY = DSAVE[7 + o_dsave];
                STX = DSAVE[8 + o_dsave];
                STY = DSAVE[9 + o_dsave];
                STMIN = DSAVE[10 + o_dsave];
                STMAX = DSAVE[11 + o_dsave];
                WIDTH = DSAVE[12 + o_dsave];
                WIDTH1 = DSAVE[13 + o_dsave];
                
            }
            
            // c     If psi(stp) <= 0 and f'(stp) >= 0 for some step, then the
            // c     algorithm enters the second stage.
            
            FTEST = FINIT + STP * GTEST;
            if (STAGE == 1 && F <= FTEST && G >= ZERO) STAGE = 2;
            
            // c     Test for warnings.
            
            //if (BRACKT && (STP <= STMIN || STP >= STMAX)) FortranLib.Copy(ref TASK , "WARNING: ROUNDING ERRORS PREVENT PROGRESS");
            //if (BRACKT && STMAX - STMIN <= XTOL * STMAX) FortranLib.Copy(ref TASK , "WARNING: XTOL TEST SATISFIED");
            //if (STP == STPMAX && F <= FTEST && G <= GTEST) FortranLib.Copy(ref TASK , "WARNING: STP = STPMAX");
            //if (STP == STPMIN && (F > FTEST || G >= GTEST)) FortranLib.Copy(ref TASK , "WARNING: STP = STPMIN");
            if (BRACKT && (STP <= STMIN || STP >= STMAX))
            {
                TASK = BFGSTask.WARNING;
            }
            if (BRACKT && STMAX - STMIN <= XTOL * STMAX)
            {
                TASK = BFGSTask.WARNING;
            }
            if (STP == STPMAX && F <= FTEST && G <= GTEST)
            {
                TASK = BFGSTask.WARNING;
            }
            if (STP == STPMIN && (F > FTEST || G >= GTEST))
            {
                TASK = BFGSTask.WARNING;
            }
            // c     Test for convergence.

            if (F <= FTEST && Math.Abs(G) <= GTOL * (-GINIT)) TASK = BFGSTask.CONV;
            
            // c     Test for termination.

            if (TASK == BFGSTask.WARNING || TASK == BFGSTask.CONV) goto LABEL1000;
            
            // c     A modified function is used to predict the step during the
            // c     first stage if a lower function value has been obtained but 
            // c     the decrease is not sufficient.
            
            if (STAGE == 1 && F <= FX && F > FTEST)
            {
                
                // c        Define the modified function and derivative values.
                
                FM = F - STP * GTEST;
                FXM = FX - STX * GTEST;
                FYM = FY - STY * GTEST;
                GM = G - GTEST;
                GXM = GX - GTEST;
                GYM = GY - GTEST;
                
                // c        Call dcstep to update stx, sty, and to compute the new step.
                
                this._dcstep.Run(ref STX, ref FXM, ref GXM, ref STY, ref FYM, ref GYM
                                 , ref STP, FM, GM, ref BRACKT, STMIN, STMAX);
                
                // c        Reset the function and derivative values for f.
                
                FX = FXM + STX * GTEST;
                FY = FYM + STY * GTEST;
                GX = GXM + GTEST;
                GY = GYM + GTEST;
                
            }
            else
            {
                
                // c       Call dcstep to update stx, sty, and to compute the new step.
                
                this._dcstep.Run(ref STX, ref FX, ref GX, ref STY, ref FY, ref GY
                                 , ref STP, F, G, ref BRACKT, STMIN, STMAX);
                
            }
            
            // c     Decide if a bisection step is needed.
            
            if (BRACKT)
            {
                if (Math.Abs(STY - STX) >= P66 * WIDTH1) STP = STX + P5 * (STY - STX);
                WIDTH1 = WIDTH;
                WIDTH = Math.Abs(STY - STX);
            }
            
            // c     Set the minimum and maximum steps allowed for stp.
            
            if (BRACKT)
            {
                STMIN = Math.Min(STX, STY);
                STMAX = Math.Max(STX, STY);
            }
            else
            {
                STMIN = STP + XTRAPL * (STP - STX);
                STMAX = STP + XTRAPU * (STP - STX);
            }
            
            // c     Force the step to be within the bounds stpmax and stpmin.
            
            STP = Math.Max(STP, STPMIN);
            STP = Math.Min(STP, STPMAX);
            
            // c     If further progress is not possible, let stp be the best
            // c     point obtained during the search.
            
            if (BRACKT && (STP <= STMIN || STP >= STMAX) || (BRACKT && STMAX - STMIN <= XTOL * STMAX)) STP = STX;
            
            // c     Obtain another function and derivative.

            TASK = BFGSTask.FG;
            
        LABEL1000:;
            
            // c     Save local variables.
            
            if (BRACKT)
            {
                ISAVE[1 + o_isave] = 1;
            }
            else
            {
                ISAVE[1 + o_isave] = 0;
            }
            ISAVE[2 + o_isave] = STAGE;
            DSAVE[1 + o_dsave] = GINIT;
            DSAVE[2 + o_dsave] = GTEST;
            DSAVE[3 + o_dsave] = GX;
            DSAVE[4 + o_dsave] = GY;
            DSAVE[5 + o_dsave] = FINIT;
            DSAVE[6 + o_dsave] = FX;
            DSAVE[7 + o_dsave] = FY;
            DSAVE[8 + o_dsave] = STX;
            DSAVE[9 + o_dsave] = STY;
            DSAVE[10 + o_dsave] = STMIN;
            DSAVE[11 + o_dsave] = STMAX;
            DSAVE[12 + o_dsave] = WIDTH;
            DSAVE[13 + o_dsave] = WIDTH1;
            

            #endregion

        }
    }
    
    // c====================== The end of dcsrch ==============================
}
