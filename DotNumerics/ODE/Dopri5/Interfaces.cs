using System;
using System.Collections.Generic;
using System.Text;

namespace DotNumerics.ODE.Dopri5
{
    #region Interface

    public interface IFAREN
    {
        void Run(int N, double X, double[] Y, int offset_y, ref double[] F, int offset_f, double[] RPAR, int offset_rpar, int IPAR);
    }
    public interface ISOLOUT
    {
        void Run(int NR, double XOLD, double X, double[] Y, int offset_y, int N, double[] CON, int offset_con
                 , int[] ICOMP, int offset_icomp, int ND, double[] RPAR, int offset_rpar, int IPAR, int IRTRN);
    }

    #endregion

    #region                                                The Class: SOLOUT
    ////----------------------------------------------------------------------------------------------------------------------------
    ////                                                     The Class: SOLOUT
    ////----------------------------------------------------------------------------------------------------------------------------

    //// C
    //// C


    ///// <summary>
    ///// NAME (EXTERNAL) OF SUBROUTINE PROVIDING THE
    ///// NUMERICAL SOLUTION DURING INTEGRATION. 
    ///// IF IOUT.GE.1, IT IS CALLED AFTER EVERY SUCCESSFUL STEP.
    ///// SUPPLY A DUMMY SUBROUTINE IF IOUT=0. 
    ///// IT MUST HAVE THE FORM
    ///// SUBROUTINE SOLOUTR (NR,XOLD,X,Y,N,CON,ICOMP,ND,
    ///// RPAR,IPAR,IRTRN)
    ///// DIMENSION Y(N),CON(5*ND),ICOMP(ND)
    ///// ....  
    ///// SOLOUTR FURNISHES THE SOLUTION "Y" AT THE NR-TH
    ///// GRID-POINT "X" (THEREBY THE INITIAL VALUE IS
    ///// THE FIRST GRID-POINT).
    ///// "XOLD" IS THE PRECEEDING GRID-POINT.
    ///// "IRTRN" SERVES TO INTERRUPT THE INTEGRATION. IF IRTRN
    ///// IS SET .LT.0, DOPRI5 WILL RETURN TO THE CALLING PROGRAM.
    ///// IF THE NUMERICAL SOLUTION IS ALTERED IN SOLOUTR,
    ///// SET  IRTRN = 2
    ///// 
    ///// -----  CONTINUOUS OUTPUT: -----
    ///// DURING CALLS TO "SOLOUTR", A CONTINUOUS SOLUTION
    ///// FOR THE INTERVAL [XOLD,X] IS AVAILABLE THROUGH
    ///// THE FUNCTION
    ///// .GT..GT..GT.   CONTD5(I,S,CON,ICOMP,ND)   .LT..LT..LT.
    ///// WHICH PROVIDES AN APPROXIMATION TO THE I-TH
    ///// COMPONENT OF THE SOLUTION AT THE POINT S. THE VALUE
    ///// S SHOULD LIE IN THE INTERVAL [XOLD,X].
    ///// </summary>
    //internal class SOLOUT : RKSolOut, ISOLOUT
    //{


    //    CONTD5 Contd5;

    //    internal SOLOUT(double[] y0, double t0, double deltaT, double tEnd, out double[,] solutionArray, CONTD5 contd5)
    //    {
    //        base.Initialize(y0, t0, deltaT, tEnd, out solutionArray);

    //        this.Contd5 = contd5;

    //    }

    //    internal SOLOUT(double[] y0, double[] tspan, out double[,] solutionArray, CONTD5 contd5)
    //    {
    //        base.Initialize(y0, tspan, out solutionArray);

    //        this.Contd5 = contd5;

    //    }

    //    internal SOLOUT(double[] y0, double t0, double deltaT, double tEnd, OdeSolution solution, CONTD5 contd5)
    //    {
    //        base.Initialize(y0, t0, deltaT, tEnd, solution);

    //        this.Contd5 = contd5;

    //    }

    //    internal SOLOUT(double[] y0, double[] tspan, OdeSolution solution, CONTD5 contd5)
    //    {
    //        base.Initialize(y0, tspan, solution);

    //        this.Contd5 = contd5;

    //    }


    //    public void solout(int NR, double XOLD, double X, double[] Y, int o_y, int N, double[] CON, int o_con
    //                        , int[] ICOMP, int o_icomp, int ND, double[] RPAR, int o_rpar, int IPAR, int IRTRN)
    //    {

    //        #region                                         Array Index Correction
    //        //--------------------------------------------------------------------------------------------------------------------
    //        //                                              Array Index Correction
    //        //--------------------------------------------------------------------------------------------------------------------

    //        int c_y = -1 + o_y; int c_con = -1 + o_con; int c_icomp = -1 + o_icomp; int c_rpar = -1 + o_rpar;

    //        #endregion

    //        if (NR == 1)
    //        {
    //            if (this.MeSolutionOutType == SolutionOutType.Array)
    //            {
    //                this.MeSolutionArray[0, 0] = this.MeT0;
    //                for (int j = 1; j <= this.MeNEquations; j++)
    //                {
    //                    this.MeSolutionArray[0, j] = this.MeY0[j - 1];
    //                }
    //            }
    //            else
    //            {
    //                this.MeSolutionOut(this.MeT0, this.MeY0);
    //            }

    //            this.MeIndex = 1;
    //        }


    //        bool MyContinue = true;

    //        while (MyContinue && this.MeIndex < this.MeMaxIndex)
    //        {
    //            this.MeT = base.GetTime(this.MeIndex);

    //            if ((this.isDeltaPositive && X >= this.MeT) || (this.isDeltaPositive == false && X <= this.MeT))
    //            {
    //                if (this.MeSolutionOutType == SolutionOutType.Array)
    //                {
    //                    this.MeSolutionArray[this.MeIndex, 0] = this.MeT;
    //                    for (int j = 1; j <= this.MeNEquations; j++)
    //                    {
    //                        this.MeSolutionArray[this.MeIndex, j] = this.Contd5.contd5(j, this.MeT, CON, o_con, ICOMP, o_icomp, ND);
    //                    }
    //                }
    //                else
    //                {
    //                    for (int j = 0; j < this.MeNEquations; j++)
    //                    {
    //                        this.MeTemporalSolution[j] = this.Contd5.contd5(j, this.MeT, CON, o_con, ICOMP, o_icomp, ND);
    //                    }
    //                    this.MeSolutionOut(this.MeT, this.MeTemporalSolution);
    //                }

    //                this.MeIndex++;
    //            }
    //            else MyContinue = false;
    //        }
    //        return;
    //    }




    //}

    #endregion

    #region                                                The Class: FAREN
    //----------------------------------------------------------------------------------------------------------------------------
    //                                                     The Class: FAREN
    //----------------------------------------------------------------------------------------------------------------------------

    // C
    internal class FAREN : IFAREN
    {

        #region Fields

        private OdeFunction MeFunction;

        private double[] MeY;
        private double[] MeYDot;
        private int MeNEq;

        #endregion

        #region Constructor

        internal FAREN(int NEq, OdeFunction Func)
        {
            this.MeNEq = NEq;
            this.MeY = new double[NEq];
            //this.MeYDot = new double[NEq];
            this.MeFunction = Func;
        }
        #endregion

        #region Properties


        #endregion


        public void Run(int N, double X, double[] Y, int o_y, ref double[] F, int o_f, double[] RPAR, int o_rpar, int IPAR)
        {

            #region                                         Array Index Correction
            //--------------------------------------------------------------------------------------------------------------------
            //                                              Array Index Correction
            //--------------------------------------------------------------------------------------------------------------------

            int c_y = -1 + o_y; int c_f = -1 + o_f; int c_rpar = -1 + o_rpar;

            #endregion

            for (int i = 0; i < this.MeNEq; i++)
            {
                this.MeY[i] = Y[i + o_y];
            }

            this.MeYDot = this.MeFunction(X, this.MeY);

            if (this.MeYDot.Length == this.MeNEq)
            {
                for (int i = 0; i < this.MeNEq; i++)
                {
                    F[i + o_f] = this.MeYDot[i];
                }
            }
            else
            {
                //Erroe
            }
            return;

        }
    }

    #endregion
}
