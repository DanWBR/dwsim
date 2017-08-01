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

namespace DotNumerics.Optimization.Cobyla
{

    public delegate void CALCFC(int N, int M, double[] X, int o_x, ref double F, ref double[] CON, int o_con);

    #region The Class: COBYLA
    
    // C------------------------------------------------------------------------ 
    // C
    public class COBYLA
    {
    
        #region Dependencies
        
        COBYLB _cobylb; 
        #endregion
        public COBYLA(COBYLB cobylb)
        {
    
            #region Set Dependencies
            
            this._cobylb = cobylb; 
            #endregion
        }

        public COBYLA(CALCFC calcfc)
        {
    
            #region Initialization Common Blocks
            
            CommonBlock Default = new CommonBlock(0, 1, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            TRSTLP trstlp = new TRSTLP();
            //CALCFC calcfc = new CALCFC(Default);
            COBYLB cobylb = new COBYLB(calcfc, trstlp);
            #endregion
            #region Set Dependencies
            
            this._cobylb = cobylb; 
            #endregion
        }
        /// <param name="N">
        /// components. The algorithm employs linear approximations to the
        ///</param>
        /// <param name="X">
        /// is now the current vector of variables. The subroutine should return
        ///</param>
        /// <param name="RHOEND">
        /// should be set to reasonable initial changes to and the required   
        ///</param>
        /// <param name="IPRINT">
        /// should be set to 0, 1, 2 or 3, which controls the amount of
        ///</param>
        public void Run(int N, int M, ref double[] X, int offset_x, double RHOBEG, double RHOEND, int IPRINT
                         , ref int MAXFUN, ref double[] W, int offset_w, ref int[] IACT, int offset_iact)
        {
            #region Implicit Variables
            
            int MPP = 0; int ICON = 0; int ISIM = 0; int ISIMI = 0; int IDATM = 0; int IA = 0; int IVSIG = 0; int IVETA = 0; 
            int ISIGB = 0;int IDX = 0; int IWORK = 0; 
            #endregion
            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_w = -1 + offset_w;  int o_iact = -1 + offset_iact; 
            #endregion
            #region Prolog
            
            // C
            // C     This subroutine minimizes an objective function F(X) subject to M
            // C     inequality constraints on X, where X is a vector of variables that has
            // C     N components. The algorithm employs linear approximations to the
            // C     objective and constraint functions, the approximations being formed by
            // C     linear interpolation at N+1 points in the space of the variables.
            // C     We regard these interpolation points as vertices of a simplex. The
            // C     parameter RHO controls the size of the simplex and it is reduced
            // C     automatically from RHOBEG to RHOEND. For each RHO the subroutine tries
            // C     to achieve a good vector of variables for the current size, and then
            // C     RHO is reduced until the value RHOEND is reached. Therefore RHOBEG and
            // C     RHOEND should be set to reasonable initial changes to and the required   
            // C     accuracy in the variables respectively, but this accuracy should be
            // C     viewed as a subject for experimentation because it is not guaranteed.
            // C     The subroutine has an advantage over many of its competitors, however,
            // C     which is that it treats each constraint individually when calculating
            // C     a change to the variables, instead of lumping the constraints together
            // C     into a single penalty function. The name of the subroutine is derived
            // C     from the phrase Constrained Optimization BY Linear Approximations.
            // C
            // C     The user must set the values of N, M, RHOBEG and RHOEND, and must
            // C     provide an initial vector of variables in X. Further, the value of
            // C     IPRINT should be set to 0, 1, 2 or 3, which controls the amount of
            // C     printing during the calculation. Specifically, there is no output if
            // C     IPRINT=0 and there is output only at the end of the calculation if
            // C     IPRINT=1. Otherwise each new value of RHO and SIGMA is printed.
            // C     Further, the vector of variables and some function information are
            // C     given either when RHO is reduced or when each new value of F(X) is
            // C     computed in the cases IPRINT=2 or IPRINT=3 respectively. Here SIGMA
            // C     is a penalty parameter, it being assumed that a change to X is an
            // C     improvement if it reduces the merit function
            // C                F(X)+SIGMA*MAX(0.0,-C1(X),-C2(X),...,-CM(X)),
            // C     where C1,C2,...,CM denote the constraint functions that should become
            // C     nonnegative eventually, at least to the precision of RHOEND. In the
            // C     printed output the displayed term that is multiplied by SIGMA is
            // C     called MAXCV, which stands for 'MAXimum Constraint Violation'. The
            // C     argument MAXFUN is an integer variable that must be set by the user to a
            // C     limit on the number of calls of CALCFC, the purpose of this routine being
            // C     given below. The value of MAXFUN will be altered to the number of calls
            // C     of CALCFC that are made. The arguments W and IACT provide real and
            // C     integer arrays that are used as working space. Their lengths must be at
            // C     least N*(3*N+2*M+11)+4*M+6 and M+1 respectively.
            // C
            // C     In order to define the objective and constraint functions, we require
            // C     a subroutine that has the name and arguments
            // C                SUBROUTINE CALCFC (N,M,X,F,CON)
            // C                DIMENSION X(*),CON(*)  .
            // C     The values of N and M are fixed and have been defined already, while
            // C     X is now the current vector of variables. The subroutine should return
            // C     the objective and constraint functions at X in F and CON(1),CON(2),
            // C     ...,CON(M). Note that we are trying to adjust X so that F(X) is as
            // C     small as possible subject to the constraint functions being nonnegative.
            // C
            // C     Partition the working space array W to provide the storage that is needed
            // C     for the main calculation.
            // C
            #endregion
            MPP = M + 2;
            ICON = 1;
            ISIM = ICON + MPP;
            ISIMI = ISIM + N * N + N;
            IDATM = ISIMI + N * N;
            IA = IDATM + N * MPP + MPP;
            IVSIG = IA + M * N + N;
            IVETA = IVSIG + N;
            ISIGB = IVETA + N;
            IDX = ISIGB + N;
            IWORK = IDX + N;
            this._cobylb.Run(N, M, MPP, ref X, offset_x, RHOBEG, RHOEND
                             , IPRINT, ref MAXFUN, ref W, ICON + o_w, ref W, ISIM + o_w, ref W, ISIMI + o_w, ref W, IDATM + o_w
                             , ref W, IA + o_w, ref W, IVSIG + o_w, ref W, IVETA + o_w, ref W, ISIGB + o_w, ref W, IDX + o_w, ref W, IWORK + o_w
                             , ref IACT, offset_iact);
            return;
        }
    }

    #endregion


    #region The Class: COBYLB
    
    // C------------------------------------------------------------------------------
    public class COBYLB
    {
    
        #region Dependencies
        
        CALCFC _calcfc; TRSTLP _trstlp; 
        #endregion
        public COBYLB(CALCFC calcfc, TRSTLP trstlp)
        {
    
            #region Set Dependencies
            
            this._calcfc = calcfc; this._trstlp = trstlp; 
            #endregion
        }


        public void Run(int N, int M, int MPP, ref double[] X, int offset_x, double RHOBEG, double RHOEND
                         , int IPRINT, ref int MAXFUN, ref double[] CON, int offset_con, ref double[] SIM, int offset_sim, ref double[] SIMI, int offset_simi, ref double[] DATMAT, int offset_datmat
                         , ref double[] A, int offset_a, ref double[] VSIG, int offset_vsig, ref double[] VETA, int offset_veta, ref double[] SIGBAR, int offset_sigbar, ref double[] DX, int offset_dx, ref double[] W, int offset_w
                         , ref int[] IACT, int offset_iact)
        {
            #region Implicit Variables
            
            int IPTEM = 0; int IPTEMP = 0; int NP = 0; int MP = 0; double ALPHA = 0; double BETA = 0; double GAMMA = 0; 
            double DELTA = 0;double RHO = 0; double PARMU = 0; int NFVALS = 0; double TEMP = 0; int I = 0; int SIM_NP = 0; 
            int J = 0;int JDROP = 0; int IBRNCH = 0; double RESMAX = 0; int K = 0; double F = 0; int DATMAT_JDROP = 0; 
            int DATMAT_NP = 0;double PHIMIN = 0; int NBEST = 0; int DATMAT_NBEST = 0; int SIM_NBEST = 0; double TEMPA = 0; 
            double ERROR = 0;int IFLAG = 0; double PARSIG = 0; double PARETA = 0; double WSIG = 0; double WETA = 0; 
            double CVMAXP = 0;double CVMAXM = 0; double SUM = 0; double DXSIGN = 0; int SIM_JDROP = 0; int IZ = 0; int IZDOTA = 0; 
            int IVMC = 0;int ISDIRN = 0; int IDXNEW = 0; int IVMD = 0; int IFULL = 0; double RESNEW = 0; double BARMU = 0; 
            double PREREC = 0;double PHI = 0; double PREREM = 0; double VMOLD = 0; double VMNEW = 0; double TRURED = 0; 
            double RATIO = 0;double EDGMAX = 0; int L = 0; double DENOM = 0; double CMIN = 0; double CMAX = 0; 
            #endregion
            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_con = -1 + offset_con;  int o_sim = -1 - N + offset_sim; 
             int o_simi = -1 - N + offset_simi; int o_datmat = -1 - MPP + offset_datmat;  int o_a = -1 - N + offset_a; 
             int o_vsig = -1 + offset_vsig; int o_veta = -1 + offset_veta;  int o_sigbar = -1 + offset_sigbar; 
             int o_dx = -1 + offset_dx; int o_w = -1 + offset_w;  int o_iact = -1 + offset_iact; 
            #endregion
            // C
            // C     Set the initial values of some parameters. The last column of SIM holds
            // C     the optimal vertex of the current simplex, and the preceding N columns
            // C     hold the displacements from the optimal vertex to the other vertices.
            // C     Further, SIMI holds the inverse of the matrix that is contained in the
            // C     first N columns of SIM.
            // C
            #region Body
            
            IPTEM = Math.Min(N, 5);
            IPTEMP = IPTEM + 1;
            NP = N + 1;
            MP = M + 1;
            ALPHA = 0.25E0;
            BETA = 2.1E0;
            GAMMA = 0.5E0;
            DELTA = 1.1E0;
            RHO = RHOBEG;
            PARMU = 0.0E0;
            if (IPRINT >= 2) ;//ERROR-ERRORPRINT10,RHO
            NFVALS = 0;
            TEMP = 1.0E0 / RHO;
            SIM_NP = NP * N + o_sim;
            for (I = 1; I <= N; I++)
            {
                SIM[I + SIM_NP] = X[I + o_x];
                for (J = 1; J <= N; J++)
                {
                    SIM[I+J * N + o_sim] = 0.0E0;
                    SIMI[I+J * N + o_simi] = 0.0E0;
                }
                SIM[I+I * N + o_sim] = RHO;
                SIMI[I+I * N + o_simi] = TEMP;
            }
            JDROP = NP;
            IBRNCH = 0;
            // C
            // C     Make the next call of the user-supplied subroutine CALCFC. These
            // C     instructions are also used for calling CALCFC during the iterations of
            // C     the algorithm.
            // C
        LABEL40:  
            if (NFVALS >= MAXFUN && NFVALS > 0)
            {
                if (IPRINT >= 1) ;//ERROR-ERRORPRINT50
                goto LABEL600;
            }
            NFVALS += 1;
            this._calcfc(N, M, X, offset_x, ref F, ref CON, offset_con);
            RESMAX = 0.0E0;
            if (M > 0)
            {
                for (K = 1; K <= M; K++)
                {
                    RESMAX = Math.Max(RESMAX,  - CON[K + o_con]);
                }
            }
            if (NFVALS == IPRINT - 1 || IPRINT == 3)
            {
                //ERROR-ERROR          PRINT 70, NFVALS,F,RESMAX,(X(I),I=1,IPTEM);
                if (IPTEM < N) ;//ERROR-ERRORPRINT80,(X(I),I=IPTEMP,N)
            }
            CON[MP + o_con] = F;
            CON[MPP + o_con] = RESMAX;
            if (IBRNCH == 1) goto LABEL440;
            // C
            // C     Set the recently calculated function values in a column of DATMAT. This
            // C     array has a column for each vertex of the current simplex, the entries of
            // C     each column being the values of the constraint functions (if any)
            // C     followed by the objective function and the greatest constraint violation
            // C     at the vertex.
            // C
            for (K = 1; K <= MPP; K++)
            {
                DATMAT[K+JDROP * MPP + o_datmat] = CON[K + o_con];
            }
            if (NFVALS > NP) goto LABEL130;
            // C
            // C     Exchange the new vertex of the initial simplex with the optimal vertex if
            // C     necessary. Then, if the initial simplex is not complete, pick its next
            // C     vertex and calculate the function values there.
            // C
            if (JDROP <= N)
            {
                if (DATMAT[MP+NP * MPP + o_datmat] <= F)
                {
                    X[JDROP + o_x] = SIM[JDROP+NP * N + o_sim];
                }
                else
                {
                    SIM[JDROP+NP * N + o_sim] = X[JDROP + o_x];
                    DATMAT_JDROP = JDROP * MPP + o_datmat;
                    DATMAT_NP = NP * MPP + o_datmat;
                    for (K = 1; K <= MPP; K++)
                    {
                        DATMAT[K + DATMAT_JDROP] = DATMAT[K + DATMAT_NP];
                        DATMAT[K+NP * MPP + o_datmat] = CON[K + o_con];
                    }
                    for (K = 1; K <= JDROP; K++)
                    {
                        SIM[JDROP+K * N + o_sim] =  - RHO;
                        TEMP = 0.0;
                        for (I = K; I <= JDROP; I++)
                        {
                            TEMP -= SIMI[I+K * N + o_simi];
                        }
                        SIMI[JDROP+K * N + o_simi] = TEMP;
                    }
                }
            }
            if (NFVALS <= N)
            {
                JDROP = NFVALS;
                X[JDROP + o_x] += RHO;
                goto LABEL40;
            }
        LABEL130:  IBRNCH = 1;
            // C
            // C     Identify the optimal vertex of the current simplex.
            // C
        LABEL140:  PHIMIN = DATMAT[MP+NP * MPP + o_datmat] + PARMU * DATMAT[MPP+NP * MPP + o_datmat];
            NBEST = NP;
            for (J = 1; J <= N; J++)
            {
                TEMP = DATMAT[MP+J * MPP + o_datmat] + PARMU * DATMAT[MPP+J * MPP + o_datmat];
                if (TEMP < PHIMIN)
                {
                    NBEST = J;
                    PHIMIN = TEMP;
                }
                else
                {
                    if (TEMP == PHIMIN && PARMU == 0.0E0)
                    {
                        if (DATMAT[MPP+J * MPP + o_datmat] < DATMAT[MPP+NBEST * MPP + o_datmat]) NBEST = J;
                    }
                }
            }
            // C
            // C     Switch the best vertex into pole position if it is not there already,
            // C     and also update SIM, SIMI and DATMAT.
            // C
            if (NBEST <= N)
            {
                DATMAT_NP = NP * MPP + o_datmat;
                DATMAT_NBEST = NBEST * MPP + o_datmat;
                for (I = 1; I <= MPP; I++)
                {
                    TEMP = DATMAT[I + DATMAT_NP];
                    DATMAT[I + DATMAT_NP] = DATMAT[I + DATMAT_NBEST];
                    DATMAT[I+NBEST * MPP + o_datmat] = TEMP;
                }
                SIM_NBEST = NBEST * N + o_sim;
                SIM_NP = NP * N + o_sim;
                for (I = 1; I <= N; I++)
                {
                    TEMP = SIM[I + SIM_NBEST];
                    SIM[I + SIM_NBEST] = 0.0E0;
                    SIM[I + SIM_NP] += TEMP;
                    TEMPA = 0.0E0;
                    for (K = 1; K <= N; K++)
                    {
                        SIM[I+K * N + o_sim] -= TEMP;
                        TEMPA -= SIMI[K+I * N + o_simi];
                    }
                    SIMI[NBEST+I * N + o_simi] = TEMPA;
                }
            }
            // C
            // C     Make an error return if SIGI is a poor approximation to the inverse of
            // C     the leading N by N submatrix of SIG.
            // C
            ERROR = 0.0E0;
            for (I = 1; I <= N; I++)
            {
                for (J = 1; J <= N; J++)
                {
                    TEMP = 0.0E0;
                    if (I == J) TEMP -= 1.0E0;
                    for (K = 1; K <= N; K++)
                    {
                        TEMP += SIMI[I+K * N + o_simi] * SIM[K+J * N + o_sim];
                    }
                    ERROR = Math.Max(ERROR, Math.Abs(TEMP));
                }
            }
            if (ERROR > 0.1E0)
            {
                if (IPRINT >= 1) ;//ERROR-ERRORPRINT210
                goto LABEL600;
            }
            // C
            // C     Calculate the coefficients of the linear approximations to the objective
            // C     and constraint functions, placing minus the objective function gradient
            // C     after the constraint gradients in the array A. The vector W is used for
            // C     working space.
            // C
            DATMAT_NP = NP * MPP + o_datmat;
            for (K = 1; K <= MP; K++)
            {
                CON[K + o_con] =  - DATMAT[K + DATMAT_NP];
                for (J = 1; J <= N; J++)
                {
                    W[J + o_w] = DATMAT[K+J * MPP + o_datmat] + CON[K + o_con];
                }
                for (I = 1; I <= N; I++)
                {
                    TEMP = 0.0E0;
                    for (J = 1; J <= N; J++)
                    {
                        TEMP += W[J + o_w] * SIMI[J+I * N + o_simi];
                    }
                    if (K == MP) TEMP =  - TEMP;
                    A[I+K * N + o_a] = TEMP;
                }
            }
            // C
            // C     Calculate the values of sigma and eta, and set IFLAG=0 if the current
            // C     simplex is not acceptable.
            // C
            IFLAG = 1;
            PARSIG = ALPHA * RHO;
            PARETA = BETA * RHO;
            for (J = 1; J <= N; J++)
            {
                WSIG = 0.0E0;
                WETA = 0.0E0;
                for (I = 1; I <= N; I++)
                {
                    WSIG += Math.Pow(SIMI[J+I * N + o_simi],2);
                    WETA += Math.Pow(SIM[I+J * N + o_sim],2);
                }
                VSIG[J + o_vsig] = 1.0E0 / Math.Sqrt(WSIG);
                VETA[J + o_veta] = Math.Sqrt(WETA);
                if (VSIG[J + o_vsig] < PARSIG || VETA[J + o_veta] > PARETA) IFLAG = 0;
            }
            // C
            // C     If a new vertex is needed to improve acceptability, then decide which
            // C     vertex to drop from the simplex.
            // C
            if (IBRNCH == 1 || IFLAG == 1) goto LABEL370;
            JDROP = 0;
            TEMP = PARETA;
            for (J = 1; J <= N; J++)
            {
                if (VETA[J + o_veta] > TEMP)
                {
                    JDROP = J;
                    TEMP = VETA[J + o_veta];
                }
            }
            if (JDROP == 0)
            {
                for (J = 1; J <= N; J++)
                {
                    if (VSIG[J + o_vsig] < TEMP)
                    {
                        JDROP = J;
                        TEMP = VSIG[J + o_vsig];
                    }
                }
            }
            // C
            // C     Calculate the step to the new vertex and its sign.
            // C
            TEMP = GAMMA * RHO * VSIG[JDROP + o_vsig];
            for (I = 1; I <= N; I++)
            {
                DX[I + o_dx] = TEMP * SIMI[JDROP+I * N + o_simi];
            }
            CVMAXP = 0.0E0;
            CVMAXM = 0.0E0;
            for (K = 1; K <= MP; K++)
            {
                SUM = 0.0E0;
                for (I = 1; I <= N; I++)
                {
                    SUM += A[I+K * N + o_a] * DX[I + o_dx];
                }
                if (K < MP)
                {
                    TEMP = DATMAT[K+NP * MPP + o_datmat];
                    CVMAXP = Math.Max(CVMAXP,  - SUM - TEMP);
                    CVMAXM = Math.Max(CVMAXM, SUM - TEMP);
                }
            }
            DXSIGN = 1.0E0;
            if (PARMU * (CVMAXP - CVMAXM) > SUM + SUM) DXSIGN =  - 1.0E0;
            // C
            // C     Update the elements of SIM and SIMI, and set the next X.
            // C
            TEMP = 0.0E0;
            SIM_JDROP = JDROP * N + o_sim;
            for (I = 1; I <= N; I++)
            {
                DX[I + o_dx] *= DXSIGN;
                SIM[I + SIM_JDROP] = DX[I + o_dx];
                TEMP += SIMI[JDROP+I * N + o_simi] * DX[I + o_dx];
            }
            for (I = 1; I <= N; I++)
            {
                SIMI[JDROP+I * N + o_simi] /= TEMP;
            }
            for (J = 1; J <= N; J++)
            {
                if (J != JDROP)
                {
                    TEMP = 0.0E0;
                    for (I = 1; I <= N; I++)
                    {
                        TEMP += SIMI[J+I * N + o_simi] * DX[I + o_dx];
                    }
                    for (I = 1; I <= N; I++)
                    {
                        SIMI[J+I * N + o_simi] +=  - TEMP * SIMI[JDROP+I * N + o_simi];
                    }
                }
                X[J + o_x] = SIM[J+NP * N + o_sim] + DX[J + o_dx];
            }
            goto LABEL40;
            // C
            // C     Calculate DX=x(*)-x(0). Branch if the length of DX is less than 0.5*RHO.
            // C
        LABEL370:  IZ = 1;
            IZDOTA = IZ + N * N;
            IVMC = IZDOTA + N;
            ISDIRN = IVMC + MP;
            IDXNEW = ISDIRN + N;
            IVMD = IDXNEW + N;
            this._trstlp.Run(N, M, A, offset_a, CON, offset_con, RHO, ref DX, offset_dx
                             , ref IFULL, ref IACT, offset_iact, ref W, IZ + o_w, ref W, IZDOTA + o_w, ref W, IVMC + o_w, ref W, ISDIRN + o_w
                             , ref W, IDXNEW + o_w, ref W, IVMD + o_w);
            if (IFULL == 0)
            {
                TEMP = 0.0E0;
                for (I = 1; I <= N; I++)
                {
                    TEMP += Math.Pow(DX[I + o_dx],2);
                }
                if (TEMP < 0.25E0 * RHO * RHO)
                {
                    IBRNCH = 1;
                    goto LABEL550;
                }
            }
            // C
            // C     Predict the change to F and the new maximum constraint violation if the
            // C     variables are altered from x(0) to x(0)+DX.
            // C
            RESNEW = 0.0E0;
            CON[MP + o_con] = 0.0E0;
            for (K = 1; K <= MP; K++)
            {
                SUM = CON[K + o_con];
                for (I = 1; I <= N; I++)
                {
                    SUM +=  - A[I+K * N + o_a] * DX[I + o_dx];
                }
                if (K < MP) RESNEW = Math.Max(RESNEW, SUM);
            }
            // C
            // C     Increase PARMU if necessary and branch back if this change alters the
            // C     optimal vertex. Otherwise PREREM and PREREC will be set to the predicted
            // C     reductions in the merit function and the maximum constraint violation
            // C     respectively.
            // C
            BARMU = 0.0E0;
            PREREC = DATMAT[MPP+NP * MPP + o_datmat] - RESNEW;
            if (PREREC > 0.0E0) BARMU = SUM / PREREC;
            if (PARMU < 1.5E0 * BARMU)
            {
                PARMU = 2.0E0 * BARMU;
                if (IPRINT >= 2) ;//ERROR-ERRORPRINT410,PARMU
                PHI = DATMAT[MP+NP * MPP + o_datmat] + PARMU * DATMAT[MPP+NP * MPP + o_datmat];
                for (J = 1; J <= N; J++)
                {
                    TEMP = DATMAT[MP+J * MPP + o_datmat] + PARMU * DATMAT[MPP+J * MPP + o_datmat];
                    if (TEMP < PHI) goto LABEL140;
                    if (TEMP == PHI && PARMU == 0.0)
                    {
                        if (DATMAT[MPP+J * MPP + o_datmat] < DATMAT[MPP+NP * MPP + o_datmat]) goto LABEL140;
                    }
                }
            }
            PREREM = PARMU * PREREC - SUM;
            // C
            // C     Calculate the constraint and objective functions at x(*). Then find the
            // C     actual reduction in the merit function.
            // C
            for (I = 1; I <= N; I++)
            {
                X[I + o_x] = SIM[I+NP * N + o_sim] + DX[I + o_dx];
            }
            IBRNCH = 1;
            goto LABEL40;
        LABEL440:  VMOLD = DATMAT[MP+NP * MPP + o_datmat] + PARMU * DATMAT[MPP+NP * MPP + o_datmat];
            VMNEW = F + PARMU * RESMAX;
            TRURED = VMOLD - VMNEW;
            if (PARMU == 0.0E0 && F == DATMAT[MP+NP * MPP + o_datmat])
            {
                PREREM = PREREC;
                TRURED = DATMAT[MPP+NP * MPP + o_datmat] - RESMAX;
            }
            // C
            // C     Begin the operations that decide whether x(*) should replace one of the
            // C     vertices of the current simplex, the change being mandatory if TRURED is
            // C     positive. Firstly, JDROP is set to the index of the vertex that is to be
            // C     replaced.
            // C
            RATIO = 0.0E0;
            if (TRURED <= 0.0) RATIO = 1.0;
            JDROP = 0;
            for (J = 1; J <= N; J++)
            {
                TEMP = 0.0E0;
                for (I = 1; I <= N; I++)
                {
                    TEMP += SIMI[J+I * N + o_simi] * DX[I + o_dx];
                }
                TEMP = Math.Abs(TEMP);
                if (TEMP > RATIO)
                {
                    JDROP = J;
                    RATIO = TEMP;
                }
                SIGBAR[J + o_sigbar] = TEMP * VSIG[J + o_vsig];
            }
            // C
            // C     Calculate the value of ell.
            // C
            EDGMAX = DELTA * RHO;
            L = 0;
            for (J = 1; J <= N; J++)
            {
                if (SIGBAR[J + o_sigbar] >= PARSIG || SIGBAR[J + o_sigbar] >= VSIG[J + o_vsig])
                {
                    TEMP = VETA[J + o_veta];
                    if (TRURED > 0.0E0)
                    {
                        TEMP = 0.0E0;
                        for (I = 1; I <= N; I++)
                        {
                            TEMP += Math.Pow(DX[I + o_dx] - SIM[I+J * N + o_sim],2);
                        }
                        TEMP = Math.Sqrt(TEMP);
                    }
                    if (TEMP > EDGMAX)
                    {
                        L = J;
                        EDGMAX = TEMP;
                    }
                }
            }
            if (L > 0) JDROP = L;
            if (JDROP == 0) goto LABEL550;
            // C
            // C     Revise the simplex by updating the elements of SIM, SIMI and DATMAT.
            // C
            TEMP = 0.0E0;
            SIM_JDROP = JDROP * N + o_sim;
            for (I = 1; I <= N; I++)
            {
                SIM[I + SIM_JDROP] = DX[I + o_dx];
                TEMP += SIMI[JDROP+I * N + o_simi] * DX[I + o_dx];
            }
            for (I = 1; I <= N; I++)
            {
                SIMI[JDROP+I * N + o_simi] /= TEMP;
            }
            for (J = 1; J <= N; J++)
            {
                if (J != JDROP)
                {
                    TEMP = 0.0E0;
                    for (I = 1; I <= N; I++)
                    {
                        TEMP += SIMI[J+I * N + o_simi] * DX[I + o_dx];
                    }
                    for (I = 1; I <= N; I++)
                    {
                        SIMI[J+I * N + o_simi] +=  - TEMP * SIMI[JDROP+I * N + o_simi];
                    }
                }
            }
            for (K = 1; K <= MPP; K++)
            {
                DATMAT[K+JDROP * MPP + o_datmat] = CON[K + o_con];
            }
            // C
            // C     Branch back for further iterations with the current RHO.
            // C
            if (TRURED > 0.0E0 && TRURED >= 0.1E0 * PREREM) goto LABEL140;
        LABEL550:  
            if (IFLAG == 0)
            {
                IBRNCH = 0;
                goto LABEL140;
            }
            // C
            // C     Otherwise reduce RHO if it is not at its least value and reset PARMU.
            // C
            if (RHO > RHOEND)
            {
                RHO *= 0.5E0;
                if (RHO <= 1.5E0 * RHOEND) RHO = RHOEND;
                if (PARMU > 0.0E0)
                {
                    DENOM = 0.0E0;
                    DATMAT_NP = NP * MPP + o_datmat;
                    for (K = 1; K <= MP; K++)
                    {
                        CMIN = DATMAT[K + DATMAT_NP];
                        CMAX = CMIN;
                        for (I = 1; I <= N; I++)
                        {
                            CMIN = Math.Min(CMIN, DATMAT[K+I * MPP + o_datmat]);
                            CMAX = Math.Max(CMAX, DATMAT[K+I * MPP + o_datmat]);
                        }
                        if (K <= M && CMIN < 0.5E0 * CMAX)
                        {
                            TEMP = Math.Max(CMAX, 0.0E0) - CMIN;
                            if (DENOM <= 0.0E0)
                            {
                                DENOM = TEMP;
                            }
                            else
                            {
                                DENOM = Math.Min(DENOM, TEMP);
                            }
                        }
                    }
                    if (DENOM == 0.0E0)
                    {
                        PARMU = 0.0E0;
                    }
                    else
                    {
                        if (CMAX - CMIN < PARMU * DENOM)
                        {
                            PARMU = (CMAX - CMIN) / DENOM;
                        }
                    }
                }
                if (IPRINT >= 2) ;//ERROR-ERRORPRINT580,RHO,PARMU
                if (IPRINT == 2)
                {
                    //ERROR-ERROR              PRINT 70, NFVALS,DATMAT(MP,NP),DATMAT(MPP,NP),(SIM(I,NP),I=1,IPTEM);
                    if (IPTEM < N) ;//ERROR-ERRORPRINT80,(X(I),I=IPTEMP,N)
                }
                goto LABEL140;
            }
            // C
            // C     Return the best calculated values of the variables.
            // C
            if (IPRINT >= 1) ;//ERROR-ERRORPRINT590
            if (IFULL == 1) goto LABEL620;
        LABEL600:  
            for (I = 1; I <= N; I++)
            {
                X[I + o_x] = SIM[I+NP * N + o_sim];
            }
            F = DATMAT[MP+NP * MPP + o_datmat];
            RESMAX = DATMAT[MPP+NP * MPP + o_datmat];
        LABEL620:  
            if (IPRINT >= 1)
            {
                //ERROR-ERROR          PRINT 70, NFVALS,F,RESMAX,(X(I),I=1,IPTEM);
                if (IPTEM < N) ;//ERROR-ERRORPRINT80,(X(I),I=IPTEMP,N)
            }
            MAXFUN = NFVALS;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: TRSTLP
    
    // C------------------------------------------------------------------------------
    public class TRSTLP
    {
    
        public TRSTLP()
        {
    
        }
    
        public void Run(int N, int M, double[] A, int offset_a, double[] B, int offset_b, double RHO, ref double[] DX, int offset_dx
                         , ref int IFULL, ref int[] IACT, int offset_iact, ref double[] Z, int offset_z, ref double[] ZDOTA, int offset_zdota, ref double[] VMULTC, int offset_vmultc, ref double[] SDIRN, int offset_sdirn
                         , ref double[] DXNEW, int offset_dxnew, ref double[] VMULTD, int offset_vmultd)
        {
            #region Implicit Variables
            
            int MCON = 0; int NACT = 0; double RESMAX = 0; int I = 0; int J = 0; int K = 0; int ICON = 0; double OPTOLD = 0; 
            int ICOUNT = 0;double OPTNEW = 0; int NACTX = 0; int KK = 0; double TOT = 0; double SP = 0; double SPABS = 0; 
            double TEMP = 0;int Z_K = 0; double ACCA = 0; double ACCB = 0; int KP = 0; double ALPHA = 0; double BETA = 0; 
            int Z_KP = 0;double RATIO = 0; double ZDOTV = 0; double ZDVABS = 0; double TEMPA = 0; int IOUT = 0; int KW = 0; 
            int ISAVE = 0;double VSAVE = 0; int Z_NACT = 0; double DD = 0; double SD = 0; double SS = 0; double STPFUL = 0; 
            double STEP = 0;double RESOLD = 0; double ZDOTW = 0; double ZDWABS = 0; int KL = 0; double SUM = 0; double SUMABS = 0; 
            int A_KK = 0;
            #endregion
            #region Array Index Correction
            
             int o_a = -1 - N + offset_a;  int o_b = -1 + offset_b;  int o_dx = -1 + offset_dx;  int o_iact = -1 + offset_iact; 
             int o_z = -1 - N + offset_z; int o_zdota = -1 + offset_zdota;  int o_vmultc = -1 + offset_vmultc; 
             int o_sdirn = -1 + offset_sdirn; int o_dxnew = -1 + offset_dxnew;  int o_vmultd = -1 + offset_vmultd; 
            #endregion
            #region Prolog
            
            // C
            // C     This subroutine calculates an N-component vector DX by applying the
            // C     following two stages. In the first stage, DX is set to the shortest
            // C     vector that minimizes the greatest violation of the constraints
            // C       A(1,K)*DX(1)+A(2,K)*DX(2)+...+A(N,K)*DX(N) .GE. B(K), K=2,3,...,M,
            // C     subject to the Euclidean length of DX being at most RHO. If its length is
            // C     strictly less than RHO, then we use the resultant freedom in DX to
            // C     minimize the objective function
            // C              -A(1,M+1)*DX(1)-A(2,M+1)*DX(2)-...-A(N,M+1)*DX(N)
            // C     subject to no increase in any greatest constraint violation. This
            // C     notation allows the gradient of the objective function to be regarded as
            // C     the gradient of a constraint. Therefore the two stages are distinguished
            // C     by MCON .EQ. M and MCON .GT. M respectively. It is possible that a
            // C     degeneracy may prevent DX from attaining the target length RHO. Then the
            // C     value IFULL=0 would be set, but usually IFULL=1 on return.
            // C
            // C     In general NACT is the number of constraints in the active set and
            // C     IACT(1),...,IACT(NACT) are their indices, while the remainder of IACT
            // C     contains a permutation of the remaining constraint indices. Further, Z is
            // C     an orthogonal matrix whose first NACT columns can be regarded as the
            // C     result of Gram-Schmidt applied to the active constraint gradients. For
            // C     J=1,2,...,NACT, the number ZDOTA(J) is the scalar product of the J-th
            // C     column of Z with the gradient of the J-th active constraint. DX is the
            // C     current vector of variables and here the residuals of the active
            // C     constraints should be zero. Further, the active constraints have
            // C     nonnegative Lagrange multipliers that are held at the beginning of
            // C     VMULTC. The remainder of this vector holds the residuals of the inactive
            // C     constraints at DX, the ordering of the components of VMULTC being in
            // C     agreement with the permutation of the indices of the constraints that is
            // C     in IACT. All these residuals are nonnegative, which is achieved by the
            // C     shift RESMAX that makes the least residual zero.
            // C
            // C     Initialize Z and some other variables. The value of RESMAX will be
            // C     appropriate to DX=0, while ICON will be the index of a most violated
            // C     constraint if RESMAX is positive. Usually during the first stage the
            // C     vector SDIRN gives a search direction that reduces all the active
            // C     constraint violations by one simultaneously.
            // C
            #endregion
            #region Body
            
            IFULL = 1;
            MCON = M;
            NACT = 0;
            RESMAX = 0.0E0;
            for (I = 1; I <= N; I++)
            {
                for (J = 1; J <= N; J++)
                {
                    Z[I+J * N + o_z] = 0.0E0;
                }
                Z[I+I * N + o_z] = 1.0E0;
                DX[I + o_dx] = 0.0E0;
            }
            if (M >= 1)
            {
                for (K = 1; K <= M; K++)
                {
                    if (B[K + o_b] > RESMAX)
                    {
                        RESMAX = B[K + o_b];
                        ICON = K;
                    }
                }
                for (K = 1; K <= M; K++)
                {
                    IACT[K + o_iact] = K;
                    VMULTC[K + o_vmultc] = RESMAX - B[K + o_b];
                }
            }
            if (RESMAX == 0.0E0) goto LABEL480;
            for (I = 1; I <= N; I++)
            {
                SDIRN[I + o_sdirn] = 0.0E0;
            }
            // C
            // C     End the current stage of the calculation if 3 consecutive iterations
            // C     have either failed to reduce the best calculated value of the objective
            // C     function or to increase the number of active constraints since the best
            // C     value was calculated. This strategy prevents cycling, but there is a
            // C     remote possibility that it will cause premature termination.
            // C
        LABEL60:  OPTOLD = 0.0E0;
            ICOUNT = 0;
        LABEL70:  
            if (MCON == M)
            {
                OPTNEW = RESMAX;
            }
            else
            {
                OPTNEW = 0.0E0;
                for (I = 1; I <= N; I++)
                {
                    OPTNEW +=  - DX[I + o_dx] * A[I+MCON * N + o_a];
                }
            }
            if (ICOUNT == 0 || OPTNEW < OPTOLD)
            {
                OPTOLD = OPTNEW;
                NACTX = NACT;
                ICOUNT = 3;
            }
            else
            {
                if (NACT > NACTX)
                {
                    NACTX = NACT;
                    ICOUNT = 3;
                }
                else
                {
                    ICOUNT -= 1;
                    if (ICOUNT == 0) goto LABEL490;
                }
            }
            // C
            // C     If ICON exceeds NACT, then we add the constraint with index IACT(ICON) to
            // C     the active set. Apply Givens rotations so that the last N-NACT-1 columns
            // C     of Z are orthogonal to the gradient of the new constraint, a scalar
            // C     product being set to zero if its nonzero value could be due to computer
            // C     rounding errors. The array DXNEW is used for working space.
            // C
            if (ICON <= NACT) goto LABEL260;
            KK = IACT[ICON + o_iact];
            for (I = 1; I <= N; I++)
            {
                DXNEW[I + o_dxnew] = A[I+KK * N + o_a];
            }
            TOT = 0.0E0;
            K = N;
        LABEL100:  
            if (K > NACT)
            {
                SP = 0.0E0;
                SPABS = 0.0E0;
                Z_K = K * N + o_z;
                for (I = 1; I <= N; I++)
                {
                    TEMP = Z[I + Z_K] * DXNEW[I + o_dxnew];
                    SP += TEMP;
                    SPABS += Math.Abs(TEMP);
                }
                ACCA = SPABS + 0.1E0 * Math.Abs(SP);
                ACCB = SPABS + 0.2E0 * Math.Abs(SP);
                if (SPABS >= ACCA || ACCA >= ACCB) SP = 0.0E0;
                if (TOT == 0.0E0)
                {
                    TOT = SP;
                }
                else
                {
                    KP = K + 1;
                    TEMP = Math.Sqrt(SP * SP + TOT * TOT);
                    ALPHA = SP / TEMP;
                    BETA = TOT / TEMP;
                    TOT = TEMP;
                    Z_K = K * N + o_z;
                    Z_KP = KP * N + o_z;
                    for (I = 1; I <= N; I++)
                    {
                        TEMP = ALPHA * Z[I + Z_K] + BETA * Z[I + Z_KP];
                        Z[I + Z_KP] = ALPHA * Z[I + Z_KP] - BETA * Z[I + Z_K];
                        Z[I+K * N + o_z] = TEMP;
                    }
                }
                K -= 1;
                goto LABEL100;
            }
            // C
            // C     Add the new constraint if this can be done without a deletion from the
            // C     active set.
            // C
            if (TOT != 0.0E0)
            {
                NACT += 1;
                ZDOTA[NACT + o_zdota] = TOT;
                VMULTC[ICON + o_vmultc] = VMULTC[NACT + o_vmultc];
                VMULTC[NACT + o_vmultc] = 0.0E0;
                goto LABEL210;
            }
            // C
            // C     The next instruction is reached if a deletion has to be made from the
            // C     active set in order to make room for the new active constraint, because
            // C     the new constraint gradient is a linear combination of the gradients of
            // C     the old active constraints. Set the elements of VMULTD to the multipliers
            // C     of the linear combination. Further, set IOUT to the index of the
            // C     constraint to be deleted, but branch if no suitable index can be found.
            // C
            RATIO =  - 1.0E0;
            K = NACT;
        LABEL130:  ZDOTV = 0.0E0;
            ZDVABS = 0.0E0;
            Z_K = K * N + o_z;
            for (I = 1; I <= N; I++)
            {
                TEMP = Z[I + Z_K] * DXNEW[I + o_dxnew];
                ZDOTV += TEMP;
                ZDVABS += Math.Abs(TEMP);
            }
            ACCA = ZDVABS + 0.1E0 * Math.Abs(ZDOTV);
            ACCB = ZDVABS + 0.2E0 * Math.Abs(ZDOTV);
            if (ZDVABS < ACCA && ACCA < ACCB)
            {
                TEMP = ZDOTV / ZDOTA[K + o_zdota];
                if (TEMP > 0.0E0 && IACT[K + o_iact] <= M)
                {
                    TEMPA = VMULTC[K + o_vmultc] / TEMP;
                    if (RATIO < 0.0E0 || TEMPA < RATIO)
                    {
                        RATIO = TEMPA;
                        IOUT = K;
                    }
                }
                if (K >= 2)
                {
                    KW = IACT[K + o_iact];
                    for (I = 1; I <= N; I++)
                    {
                        DXNEW[I + o_dxnew] +=  - TEMP * A[I+KW * N + o_a];
                    }
                }
                VMULTD[K + o_vmultd] = TEMP;
            }
            else
            {
                VMULTD[K + o_vmultd] = 0.0E0;
            }
            K -= 1;
            if (K > 0) goto LABEL130;
            if (RATIO < 0.0E0) goto LABEL490;
            // C
            // C     Revise the Lagrange multipliers and reorder the active constraints so
            // C     that the one to be replaced is at the end of the list. Also calculate the
            // C     new value of ZDOTA(NACT) and branch if it is not acceptable.
            // C
            for (K = 1; K <= NACT; K++)
            {
                VMULTC[K + o_vmultc] = Math.Max(0.0E0, VMULTC[K + o_vmultc] - RATIO * VMULTD[K + o_vmultd]);
            }
            if (ICON < NACT)
            {
                ISAVE = IACT[ICON + o_iact];
                VSAVE = VMULTC[ICON + o_vmultc];
                K = ICON;
            LABEL170:  KP = K + 1;
                KW = IACT[KP + o_iact];
                SP = 0.0E0;
                for (I = 1; I <= N; I++)
                {
                    SP += Z[I+K * N + o_z] * A[I+KW * N + o_a];
                }
                TEMP = Math.Sqrt(SP * SP + Math.Pow(ZDOTA[KP + o_zdota],2));
                ALPHA = ZDOTA[KP + o_zdota] / TEMP;
                BETA = SP / TEMP;
                ZDOTA[KP + o_zdota] = ALPHA * ZDOTA[K + o_zdota];
                ZDOTA[K + o_zdota] = TEMP;
                Z_KP = KP * N + o_z;
                Z_K = K * N + o_z;
                for (I = 1; I <= N; I++)
                {
                    TEMP = ALPHA * Z[I + Z_KP] + BETA * Z[I + Z_K];
                    Z[I + Z_KP] = ALPHA * Z[I + Z_K] - BETA * Z[I + Z_KP];
                    Z[I+K * N + o_z] = TEMP;
                }
                IACT[K + o_iact] = KW;
                VMULTC[K + o_vmultc] = VMULTC[KP + o_vmultc];
                K = KP;
                if (K < NACT) goto LABEL170;
                IACT[K + o_iact] = ISAVE;
                VMULTC[K + o_vmultc] = VSAVE;
            }
            TEMP = 0.0E0;
            for (I = 1; I <= N; I++)
            {
                TEMP += Z[I+NACT * N + o_z] * A[I+KK * N + o_a];
            }
            if (TEMP == 0.0E0) goto LABEL490;
            ZDOTA[NACT + o_zdota] = TEMP;
            VMULTC[ICON + o_vmultc] = 0.0E0;
            VMULTC[NACT + o_vmultc] = RATIO;
            // C
            // C     Update IACT and ensure that the objective function continues to be
            // C     treated as the last active constraint when MCON>M.
            // C
        LABEL210:  IACT[ICON + o_iact] = IACT[NACT + o_iact];
            IACT[NACT + o_iact] = KK;
            if (MCON > M && KK != MCON)
            {
                K = NACT - 1;
                SP = 0.0E0;
                for (I = 1; I <= N; I++)
                {
                    SP += Z[I+K * N + o_z] * A[I+KK * N + o_a];
                }
                TEMP = Math.Sqrt(SP * SP + Math.Pow(ZDOTA[NACT + o_zdota],2));
                ALPHA = ZDOTA[NACT + o_zdota] / TEMP;
                BETA = SP / TEMP;
                ZDOTA[NACT + o_zdota] = ALPHA * ZDOTA[K + o_zdota];
                ZDOTA[K + o_zdota] = TEMP;
                Z_NACT = NACT * N + o_z;
                Z_K = K * N + o_z;
                for (I = 1; I <= N; I++)
                {
                    TEMP = ALPHA * Z[I + Z_NACT] + BETA * Z[I + Z_K];
                    Z[I + Z_NACT] = ALPHA * Z[I + Z_K] - BETA * Z[I + Z_NACT];
                    Z[I+K * N + o_z] = TEMP;
                }
                IACT[NACT + o_iact] = IACT[K + o_iact];
                IACT[K + o_iact] = KK;
                TEMP = VMULTC[K + o_vmultc];
                VMULTC[K + o_vmultc] = VMULTC[NACT + o_vmultc];
                VMULTC[NACT + o_vmultc] = TEMP;
            }
            // C
            // C     If stage one is in progress, then set SDIRN to the direction of the next
            // C     change to the current vector of variables.
            // C
            if (MCON > M) goto LABEL320;
            KK = IACT[NACT + o_iact];
            TEMP = 0.0E0;
            for (I = 1; I <= N; I++)
            {
                TEMP += SDIRN[I + o_sdirn] * A[I+KK * N + o_a];
            }
            TEMP -= 1.0E0;
            TEMP /= ZDOTA[NACT + o_zdota];
            for (I = 1; I <= N; I++)
            {
                SDIRN[I + o_sdirn] +=  - TEMP * Z[I+NACT * N + o_z];
            }
            goto LABEL340;
            // C
            // C     Delete the constraint that has the index IACT(ICON) from the active set.
            // C
        LABEL260:  
            if (ICON < NACT)
            {
                ISAVE = IACT[ICON + o_iact];
                VSAVE = VMULTC[ICON + o_vmultc];
                K = ICON;
            LABEL270:  KP = K + 1;
                KK = IACT[KP + o_iact];
                SP = 0.0E0;
                for (I = 1; I <= N; I++)
                {
                    SP += Z[I+K * N + o_z] * A[I+KK * N + o_a];
                }
                TEMP = Math.Sqrt(SP * SP + Math.Pow(ZDOTA[KP + o_zdota],2));
                ALPHA = ZDOTA[KP + o_zdota] / TEMP;
                BETA = SP / TEMP;
                ZDOTA[KP + o_zdota] = ALPHA * ZDOTA[K + o_zdota];
                ZDOTA[K + o_zdota] = TEMP;
                Z_KP = KP * N + o_z;
                Z_K = K * N + o_z;
                for (I = 1; I <= N; I++)
                {
                    TEMP = ALPHA * Z[I + Z_KP] + BETA * Z[I + Z_K];
                    Z[I + Z_KP] = ALPHA * Z[I + Z_K] - BETA * Z[I + Z_KP];
                    Z[I+K * N + o_z] = TEMP;
                }
                IACT[K + o_iact] = KK;
                VMULTC[K + o_vmultc] = VMULTC[KP + o_vmultc];
                K = KP;
                if (K < NACT) goto LABEL270;
                IACT[K + o_iact] = ISAVE;
                VMULTC[K + o_vmultc] = VSAVE;
            }
            NACT -= 1;
            // C
            // C     If stage one is in progress, then set SDIRN to the direction of the next
            // C     change to the current vector of variables.
            // C
            if (MCON > M) goto LABEL320;
            TEMP = 0.0E0;
            for (I = 1; I <= N; I++)
            {
                TEMP += SDIRN[I + o_sdirn] * Z[I+(NACT + 1) * N + o_z];
            }
            for (I = 1; I <= N; I++)
            {
                SDIRN[I + o_sdirn] +=  - TEMP * Z[I+(NACT + 1) * N + o_z];
            }
            goto LABEL340;
            // C
            // C     Pick the next search direction of stage two.
            // C
        LABEL320:  TEMP = 1.0E0 / ZDOTA[NACT + o_zdota];
            for (I = 1; I <= N; I++)
            {
                SDIRN[I + o_sdirn] = TEMP * Z[I+NACT * N + o_z];
            }
            // C
            // C     Calculate the step to the boundary of the trust region or take the step
            // C     that reduces RESMAX to zero. The two statements below that include the
            // C     factor 1.0E-6 prevent some harmless underflows that occurred in a test
            // C     calculation. Further, we skip the step if it could be zero within a
            // C     reasonable tolerance for computer rounding errors.
            // C
        LABEL340:  DD = RHO * RHO;
            SD = 0.0E0;
            SS = 0.0E0;
            for (I = 1; I <= N; I++)
            {
                if (Math.Abs(DX[I + o_dx]) >= 1.0E-6) DD -= Math.Pow(DX[I + o_dx],2);
                SD += DX[I + o_dx] * SDIRN[I + o_sdirn];
                SS += Math.Pow(SDIRN[I + o_sdirn],2);
            }
            if (DD <= 0.0E0) goto LABEL490;
            TEMP = Math.Sqrt(SS * DD);
            if (Math.Abs(SD) >= 1.0E-6) TEMP = Math.Sqrt(SS * DD + SD * SD);
            STPFUL = DD / (TEMP + SD);
            STEP = STPFUL;
            if (MCON == M)
            {
                ACCA = STEP + 0.1E0 * RESMAX;
                ACCB = STEP + 0.2E0 * RESMAX;
                if (STEP >= ACCA || ACCA >= ACCB) goto LABEL480;
                STEP = Math.Min(STEP, RESMAX);
            }
            // C
            // C     Set DXNEW to the new variables if STEP is the steplength, and reduce
            // C     RESMAX to the corresponding maximum residual if stage one is being done.
            // C     Because DXNEW will be changed during the calculation of some Lagrange
            // C     multipliers, it will be restored to the following value later.
            // C
            for (I = 1; I <= N; I++)
            {
                DXNEW[I + o_dxnew] = DX[I + o_dx] + STEP * SDIRN[I + o_sdirn];
            }
            if (MCON == M)
            {
                RESOLD = RESMAX;
                RESMAX = 0.0E0;
                for (K = 1; K <= NACT; K++)
                {
                    KK = IACT[K + o_iact];
                    TEMP = B[KK + o_b];
                    for (I = 1; I <= N; I++)
                    {
                        TEMP +=  - A[I+KK * N + o_a] * DXNEW[I + o_dxnew];
                    }
                    RESMAX = Math.Max(RESMAX, TEMP);
                }
            }
            // C
            // C     Set VMULTD to the VMULTC vector that would occur if DX became DXNEW. A
            // C     device is included to force VMULTD(K)=0.0 if deviations from this value
            // C     can be attributed to computer rounding errors. First calculate the new
            // C     Lagrange multipliers.
            // C
            K = NACT;
        LABEL390:  ZDOTW = 0.0E0;
            ZDWABS = 0.0E0;
            Z_K = K * N + o_z;
            for (I = 1; I <= N; I++)
            {
                TEMP = Z[I + Z_K] * DXNEW[I + o_dxnew];
                ZDOTW += TEMP;
                ZDWABS += Math.Abs(TEMP);
            }
            ACCA = ZDWABS + 0.1E0 * Math.Abs(ZDOTW);
            ACCB = ZDWABS + 0.2E0 * Math.Abs(ZDOTW);
            if (ZDWABS >= ACCA || ACCA >= ACCB) ZDOTW = 0.0E0;
            VMULTD[K + o_vmultd] = ZDOTW / ZDOTA[K + o_zdota];
            if (K >= 2)
            {
                KK = IACT[K + o_iact];
                for (I = 1; I <= N; I++)
                {
                    DXNEW[I + o_dxnew] +=  - VMULTD[K + o_vmultd] * A[I+KK * N + o_a];
                }
                K -= 1;
                goto LABEL390;
            }
            if (MCON > M) VMULTD[NACT + o_vmultd] = Math.Max(0.0E0, VMULTD[NACT + o_vmultd]);
            // C
            // C     Complete VMULTC by finding the new constraint residuals.
            // C
            for (I = 1; I <= N; I++)
            {
                DXNEW[I + o_dxnew] = DX[I + o_dx] + STEP * SDIRN[I + o_sdirn];
            }
            if (MCON > NACT)
            {
                KL = NACT + 1;
                for (K = KL; K <= MCON; K++)
                {
                    KK = IACT[K + o_iact];
                    SUM = RESMAX - B[KK + o_b];
                    SUMABS = RESMAX + Math.Abs(B[KK + o_b]);
                    A_KK = KK * N + o_a;
                    for (I = 1; I <= N; I++)
                    {
                        TEMP = A[I + A_KK] * DXNEW[I + o_dxnew];
                        SUM += TEMP;
                        SUMABS += Math.Abs(TEMP);
                    }
                    ACCA = SUMABS + 0.1 * Math.Abs(SUM);
                    ACCB = SUMABS + 0.2 * Math.Abs(SUM);
                    if (SUMABS >= ACCA || ACCA >= ACCB) SUM = 0.0;
                    VMULTD[K + o_vmultd] = SUM;
                }
            }
            // C
            // C     Calculate the fraction of the step from DX to DXNEW that will be taken.
            // C
            RATIO = 1.0E0;
            ICON = 0;
            for (K = 1; K <= MCON; K++)
            {
                if (VMULTD[K + o_vmultd] < 0.0E0)
                {
                    TEMP = VMULTC[K + o_vmultc] / (VMULTC[K + o_vmultc] - VMULTD[K + o_vmultd]);
                    if (TEMP < RATIO)
                    {
                        RATIO = TEMP;
                        ICON = K;
                    }
                }
            }
            // C
            // C     Update DX, VMULTC and RESMAX.
            // C
            TEMP = 1.0E0 - RATIO;
            for (I = 1; I <= N; I++)
            {
                DX[I + o_dx] = TEMP * DX[I + o_dx] + RATIO * DXNEW[I + o_dxnew];
            }
            for (K = 1; K <= MCON; K++)
            {
                VMULTC[K + o_vmultc] = Math.Max(0.0E0, TEMP * VMULTC[K + o_vmultc] + RATIO * VMULTD[K + o_vmultd]);
            }
            if (MCON == M) RESMAX = RESOLD + RATIO * (RESMAX - RESOLD);
            // C
            // C     If the full step is not acceptable then begin another iteration.
            // C     Otherwise switch to stage two or end the calculation.
            // C
            if (ICON > 0) goto LABEL70;
            if (STEP == STPFUL) goto LABEL500;
        LABEL480:  MCON = M + 1;
            ICON = MCON;
            IACT[MCON + o_iact] = MCON;
            VMULTC[MCON + o_vmultc] = 0.0E0;
            goto LABEL60;
            // C
            // C     We employ any freedom that may be available to reduce the objective
            // C     function before returning a DX whose length is less than RHO.
            // C
        LABEL490:  
            if (MCON == M) goto LABEL480;
            IFULL = 0;
        LABEL500:  return;
            #endregion
        }
    }

    #endregion

    
    
    
}
