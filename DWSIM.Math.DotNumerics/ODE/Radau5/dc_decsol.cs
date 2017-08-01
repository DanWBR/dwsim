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

namespace DotNumerics.ODE.Radau5
{

    #region The Class: DECOMR
    
    // C ******************************************
    // C     VERSION OF SEPTEMBER 18, 1995      
    // C ******************************************
    // C
    public class DECOMR
    {
    
        #region Dependencies
        
        DEC _dec; DECB _decb; ELMHES _elmhes; DECH _dech; 
        #endregion
        #region Common variables
        
        #region Common Block: LINAL Declaration
        
        CommonBlock _linal;
        Oint MLE; Oint MUE; Oint MBJAC; Oint MBB; Oint MDIAG; Oint MDIFF; Oint MBDIAG; 
        #endregion
        #endregion
        public DECOMR(DEC dec, DECB decb, ELMHES elmhes, DECH dech, CommonBlock LINAL)
        {
    
            #region Set Dependencies
            
            this._dec = dec; this._decb = decb; this._elmhes = elmhes; this._dech = dech; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
    
        public DECOMR()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock LINAL = new CommonBlock(0, 7, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            DEC dec = new DEC();
            DECB decb = new DECB();
            ELMHES elmhes = new ELMHES();
            DECH dech = new DECH();
            #endregion
            #region Set Dependencies
            
            this._dec = dec; this._decb = decb; this._elmhes = elmhes; this._dech = dech; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
        public void Run(int N, ref double[] FJAC, int offset_fjac, int LDJAC, double[] FMAS, int offset_fmas, int LDMAS, int MLMAS
                         , int MUMAS, int M1, int M2, int NM1, double FAC1, ref double[] E1, int offset_e1
                         , int LDE1, ref int[] IP1, int offset_ip1, ref int IER, int IJOB, ref bool CALHES, ref int[] IPHES, int offset_iphes)
        {
            #region Implicit Variables
            
            int I = 0; int J = 0; int E1_J = 0; int FJAC_J = 0; int JM1 = 0; int FJAC_JM1 = 0; int MM = 0; double SUM = 0; 
            int K = 0;int FMAS_J = 0; int IB = 0; int J1 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_fjac = -1 - LDJAC + offset_fjac;  int o_fmas = -1 - LDMAS + offset_fmas;  int o_e1 = -1 - LDE1 + offset_e1; 
             int o_ip1 = -1 + offset_ip1; int o_iphes = -1 + offset_iphes; 
            #endregion
            // C
            #region Body
            
            switch (IJOB)
            {
                case 1: goto LABEL1;
                case 2: goto LABEL2;
                case 3: goto LABEL3;
                case 4: goto LABEL4;
                case 5: goto LABEL5;
                case 6: goto LABEL6;
                case 7: goto LABEL7;
                case 8: goto LABEL55;
                case 9: goto LABEL55;
                case 10: goto LABEL55;
                case 11: goto LABEL11;
                case 12: goto LABEL12;
                case 13: goto LABEL13;
                case 14: goto LABEL14;
                case 15: goto LABEL15;
            }
            // C
            // C -----------------------------------------------------------
            // C
        LABEL1:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX
            for (J = 1; J <= N; J++)
            {
                E1_J = J * LDE1 + o_e1;
                FJAC_J = J * LDJAC + o_fjac;
                for (I = 1; I <= N; I++)
                {
                    E1[I + E1_J] =  - FJAC[I + FJAC_J];
                }
                E1[J+J * LDE1 + o_e1] += FAC1;
            }
            this._dec.Run(N, LDE1, ref E1, offset_e1, ref IP1, offset_ip1, ref IER);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL11:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (J = 1; J <= NM1; J++)
            {
                JM1 = J + M1;
                E1_J = J * LDE1 + o_e1;
                FJAC_JM1 = JM1 * LDJAC + o_fjac;
                for (I = 1; I <= NM1; I++)
                {
                    E1[I + E1_J] =  - FJAC[I + FJAC_JM1];
                }
                E1[J+J * LDE1 + o_e1] += FAC1;
            }
        LABEL45:  MM = M1 / M2;
            for (J = 1; J <= M2; J++)
            {
                E1_J = J * LDE1 + o_e1;
                for (I = 1; I <= NM1; I++)
                {
                    SUM = 0.0E0;
                    for (K = 0; K <= MM - 1; K++)
                    {
                        SUM = (SUM + FJAC[I+(J + K * M2) * LDJAC + o_fjac]) / FAC1;
                    }
                    E1[I + E1_J] -= SUM;
                }
            }
            this._dec.Run(NM1, LDE1, ref E1, offset_e1, ref IP1, offset_ip1, ref IER);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL2:;
            // C ---  B=IDENTITY, JACOBIAN A BANDED MATRIX
            for (J = 1; J <= N; J++)
            {
                E1_J = J * LDE1 + o_e1;
                FJAC_J = J * LDJAC + o_fjac;
                for (I = 1; I <= MBJAC.v; I++)
                {
                    E1[I + MLE.v + E1_J] =  - FJAC[I + FJAC_J];
                }
                E1[MDIAG.v+J * LDE1 + o_e1] += FAC1;
            }
            this._decb.Run(N, LDE1, ref E1, offset_e1, MLE.v, MUE.v, ref IP1, offset_ip1
                           , ref IER);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL12:;
            // C ---  B=IDENTITY, JACOBIAN A BANDED MATRIX, SECOND ORDER
            for (J = 1; J <= NM1; J++)
            {
                JM1 = J + M1;
                E1_J = J * LDE1 + o_e1;
                FJAC_JM1 = JM1 * LDJAC + o_fjac;
                for (I = 1; I <= MBJAC.v; I++)
                {
                    E1[I + MLE.v + E1_J] =  - FJAC[I + FJAC_JM1];
                }
                E1[MDIAG.v+J * LDE1 + o_e1] += FAC1;
            }
        LABEL46:  MM = M1 / M2;
            for (J = 1; J <= M2; J++)
            {
                E1_J = J * LDE1 + o_e1;
                for (I = 1; I <= MBJAC.v; I++)
                {
                    SUM = 0.0E0;
                    for (K = 0; K <= MM - 1; K++)
                    {
                        SUM = (SUM + FJAC[I+(J + K * M2) * LDJAC + o_fjac]) / FAC1;
                    }
                    E1[I + MLE.v + E1_J] -= SUM;
                }
            }
            this._decb.Run(NM1, LDE1, ref E1, offset_e1, MLE.v, MUE.v, ref IP1, offset_ip1
                           , ref IER);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL3:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX
            for (J = 1; J <= N; J++)
            {
                E1_J = J * LDE1 + o_e1;
                FJAC_J = J * LDJAC + o_fjac;
                for (I = 1; I <= N; I++)
                {
                    E1[I + E1_J] =  - FJAC[I + FJAC_J];
                }
                E1_J = J * LDE1 + o_e1;
                FMAS_J = J * LDMAS + o_fmas;
                for (I = Math.Max(1, J - MUMAS); I <= Math.Min(N, J + MLMAS); I++)
                {
                    E1[I + E1_J] += FAC1 * FMAS[I - J + MBDIAG.v + FMAS_J];
                }
            }
            this._dec.Run(N, LDE1, ref E1, offset_e1, ref IP1, offset_ip1, ref IER);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL13:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (J = 1; J <= NM1; J++)
            {
                JM1 = J + M1;
                E1_J = J * LDE1 + o_e1;
                FJAC_JM1 = JM1 * LDJAC + o_fjac;
                for (I = 1; I <= NM1; I++)
                {
                    E1[I + E1_J] =  - FJAC[I + FJAC_JM1];
                }
                E1_J = J * LDE1 + o_e1;
                FMAS_J = J * LDMAS + o_fmas;
                for (I = Math.Max(1, J - MUMAS); I <= Math.Min(NM1, J + MLMAS); I++)
                {
                    E1[I + E1_J] += FAC1 * FMAS[I - J + MBDIAG.v + FMAS_J];
                }
            }
            goto LABEL45;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL4:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX
            for (J = 1; J <= N; J++)
            {
                E1_J = J * LDE1 + o_e1;
                FJAC_J = J * LDJAC + o_fjac;
                for (I = 1; I <= MBJAC.v; I++)
                {
                    E1[I + MLE.v + E1_J] =  - FJAC[I + FJAC_J];
                }
                FMAS_J = J * LDMAS + o_fmas;
                for (I = 1; I <= MBB.v; I++)
                {
                    IB = I + MDIFF.v;
                    E1[IB+J * LDE1 + o_e1] += FAC1 * FMAS[I + FMAS_J];
                }
            }
            this._decb.Run(N, LDE1, ref E1, offset_e1, MLE.v, MUE.v, ref IP1, offset_ip1
                           , ref IER);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL14:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX, SECOND ORDER
            for (J = 1; J <= NM1; J++)
            {
                JM1 = J + M1;
                E1_J = J * LDE1 + o_e1;
                FJAC_JM1 = JM1 * LDJAC + o_fjac;
                for (I = 1; I <= MBJAC.v; I++)
                {
                    E1[I + MLE.v + E1_J] =  - FJAC[I + FJAC_JM1];
                }
                FMAS_J = J * LDMAS + o_fmas;
                for (I = 1; I <= MBB.v; I++)
                {
                    IB = I + MDIFF.v;
                    E1[IB+J * LDE1 + o_e1] += FAC1 * FMAS[I + FMAS_J];
                }
            }
            goto LABEL46;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL5:;
            // C ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX
            for (J = 1; J <= N; J++)
            {
                E1_J = J * LDE1 + o_e1;
                FMAS_J = J * LDMAS + o_fmas;
                FJAC_J = J * LDJAC + o_fjac;
                for (I = 1; I <= N; I++)
                {
                    E1[I + E1_J] = FMAS[I + FMAS_J] * FAC1 - FJAC[I + FJAC_J];
                }
            }
            this._dec.Run(N, LDE1, ref E1, offset_e1, ref IP1, offset_ip1, ref IER);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL15:;
            // C ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (J = 1; J <= NM1; J++)
            {
                JM1 = J + M1;
                E1_J = J * LDE1 + o_e1;
                FMAS_J = J * LDMAS + o_fmas;
                FJAC_JM1 = JM1 * LDJAC + o_fjac;
                for (I = 1; I <= NM1; I++)
                {
                    E1[I + E1_J] = FMAS[I + FMAS_J] * FAC1 - FJAC[I + FJAC_JM1];
                }
            }
            goto LABEL45;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL6:;
            // C ---  B IS A FULL MATRIX, JACOBIAN A BANDED MATRIX
            // C ---  THIS OPTION IS NOT PROVIDED
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL7:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX, HESSENBERG-OPTION
            if (CALHES) this._elmhes.Run(LDJAC, N, 1, N, ref FJAC, offset_fjac, ref IPHES, offset_iphes);
            CALHES = false;
            for (J = 1; J <= N - 1; J++)
            {
                J1 = J + 1;
                E1[J1+J * LDE1 + o_e1] =  - FJAC[J1+J * LDJAC + o_fjac];
            }
            for (J = 1; J <= N; J++)
            {
                E1_J = J * LDE1 + o_e1;
                FJAC_J = J * LDJAC + o_fjac;
                for (I = 1; I <= J; I++)
                {
                    E1[I + E1_J] =  - FJAC[I + FJAC_J];
                }
                E1[J+J * LDE1 + o_e1] += FAC1;
            }
            this._dech.Run(N, LDE1, ref E1, offset_e1, 1, ref IP1, offset_ip1, ref IER);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL55:;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: DECOMC
    
    // C
    // C     END OF SUBROUTINE DECOMR
    // C
    // C ***********************************************************
    // C
    public class DECOMC
    {
    
        #region Dependencies
        
        DECC _decc; DECBC _decbc; DECHC _dechc; 
        #endregion
        #region Common variables
        
        #region Common Block: LINAL Declaration
        
        CommonBlock _linal;
        Oint MLE; Oint MUE; Oint MBJAC; Oint MBB; Oint MDIAG; Oint MDIFF; Oint MBDIAG; 
        #endregion
        #endregion
        public DECOMC(DECC decc, DECBC decbc, DECHC dechc, CommonBlock LINAL)
        {
    
            #region Set Dependencies
            
            this._decc = decc; this._decbc = decbc; this._dechc = dechc; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
    
        public DECOMC()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock LINAL = new CommonBlock(0, 7, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            DECC decc = new DECC();
            DECBC decbc = new DECBC();
            DECHC dechc = new DECHC();
            #endregion
            #region Set Dependencies
            
            this._decc = decc; this._decbc = decbc; this._dechc = dechc; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
        public void Run(int N, double[] FJAC, int offset_fjac, int LDJAC, double[] FMAS, int offset_fmas, int LDMAS, int MLMAS
                         , int MUMAS, int M1, int M2, int NM1, double ALPHN, double BETAN
                         , ref double[] E2R, int offset_e2r, ref double[] E2I, int offset_e2i, int LDE1, ref int[] IP2, int offset_ip2, ref int IER, int IJOB)
        {
            #region Implicit Variables
            
            int I = 0; int J = 0; int E2R_J = 0; int FJAC_J = 0; int E2I_J = 0; int JM1 = 0; int FJAC_JM1 = 0; int MM = 0; 
            double ABNO = 0;double ALP = 0; double BET = 0; double SUMR = 0; double SUMI = 0; double SUMS = 0; int K = 0; 
            int IMLE = 0;double BB = 0; int FMAS_J = 0; double FFMA = 0; int IB = 0; int J1 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_fjac = -1 - LDJAC + offset_fjac;  int o_fmas = -1 - LDMAS + offset_fmas;  int o_e2r = -1 - LDE1 + offset_e2r; 
             int o_e2i = -1 - LDE1 + offset_e2i; int o_ip2 = -1 + offset_ip2; 
            #endregion
            // C
            #region Body
            
            switch (IJOB)
            {
                case 1: goto LABEL1;
                case 2: goto LABEL2;
                case 3: goto LABEL3;
                case 4: goto LABEL4;
                case 5: goto LABEL5;
                case 6: goto LABEL6;
                case 7: goto LABEL7;
                case 8: goto LABEL55;
                case 9: goto LABEL55;
                case 10: goto LABEL55;
                case 11: goto LABEL11;
                case 12: goto LABEL12;
                case 13: goto LABEL13;
                case 14: goto LABEL14;
                case 15: goto LABEL15;
            }
            // C
            // C -----------------------------------------------------------
            // C
        LABEL1:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX
            for (J = 1; J <= N; J++)
            {
                E2R_J = J * LDE1 + o_e2r;
                FJAC_J = J * LDJAC + o_fjac;
                E2I_J = J * LDE1 + o_e2i;
                for (I = 1; I <= N; I++)
                {
                    E2R[I + E2R_J] =  - FJAC[I + FJAC_J];
                    E2I[I + E2I_J] = 0.0E0;
                }
                E2R[J+J * LDE1 + o_e2r] += ALPHN;
                E2I[J+J * LDE1 + o_e2i] = BETAN;
            }
            this._decc.Run(N, LDE1, ref E2R, offset_e2r, ref E2I, offset_e2i, ref IP2, offset_ip2, ref IER);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL11:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (J = 1; J <= NM1; J++)
            {
                JM1 = J + M1;
                E2R_J = J * LDE1 + o_e2r;
                FJAC_JM1 = JM1 * LDJAC + o_fjac;
                E2I_J = J * LDE1 + o_e2i;
                for (I = 1; I <= NM1; I++)
                {
                    E2R[I + E2R_J] =  - FJAC[I + FJAC_JM1];
                    E2I[I + E2I_J] = 0.0E0;
                }
                E2R[J+J * LDE1 + o_e2r] += ALPHN;
                E2I[J+J * LDE1 + o_e2i] = BETAN;
            }
        LABEL45:  MM = M1 / M2;
            ABNO = Math.Pow(ALPHN,2) + Math.Pow(BETAN,2);
            ALP = ALPHN / ABNO;
            BET = BETAN / ABNO;
            for (J = 1; J <= M2; J++)
            {
                E2R_J = J * LDE1 + o_e2r;
                E2I_J = J * LDE1 + o_e2i;
                for (I = 1; I <= NM1; I++)
                {
                    SUMR = 0.0E0;
                    SUMI = 0.0E0;
                    for (K = 0; K <= MM - 1; K++)
                    {
                        SUMS = SUMR + FJAC[I+(J + K * M2) * LDJAC + o_fjac];
                        SUMR = SUMS * ALP + SUMI * BET;
                        SUMI = SUMI * ALP - SUMS * BET;
                    }
                    E2R[I + E2R_J] -= SUMR;
                    E2I[I + E2I_J] -= SUMI;
                }
            }
            this._decc.Run(NM1, LDE1, ref E2R, offset_e2r, ref E2I, offset_e2i, ref IP2, offset_ip2, ref IER);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL2:;
            // C ---  B=IDENTITY, JACOBIAN A BANDED MATRIX
            for (J = 1; J <= N; J++)
            {
                FJAC_J = J * LDJAC + o_fjac;
                for (I = 1; I <= MBJAC.v; I++)
                {
                    IMLE = I + MLE.v;
                    E2R[IMLE+J * LDE1 + o_e2r] =  - FJAC[I + FJAC_J];
                    E2I[IMLE+J * LDE1 + o_e2i] = 0.0E0;
                }
                E2R[MDIAG.v+J * LDE1 + o_e2r] += ALPHN;
                E2I[MDIAG.v+J * LDE1 + o_e2i] = BETAN;
            }
            this._decbc.Run(N, LDE1, ref E2R, offset_e2r, ref E2I, offset_e2i, MLE.v, MUE.v
                            , ref IP2, offset_ip2, ref IER);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL12:;
            // C ---  B=IDENTITY, JACOBIAN A BANDED MATRIX, SECOND ORDER
            for (J = 1; J <= NM1; J++)
            {
                JM1 = J + M1;
                E2R_J = J * LDE1 + o_e2r;
                FJAC_JM1 = JM1 * LDJAC + o_fjac;
                E2I_J = J * LDE1 + o_e2i;
                for (I = 1; I <= MBJAC.v; I++)
                {
                    E2R[I + MLE.v + E2R_J] =  - FJAC[I + FJAC_JM1];
                    E2I[I + MLE.v + E2I_J] = 0.0E0;
                }
                E2R[MDIAG.v+J * LDE1 + o_e2r] += ALPHN;
                E2I[MDIAG.v+J * LDE1 + o_e2i] += BETAN;
            }
        LABEL46:  MM = M1 / M2;
            ABNO = Math.Pow(ALPHN,2) + Math.Pow(BETAN,2);
            ALP = ALPHN / ABNO;
            BET = BETAN / ABNO;
            for (J = 1; J <= M2; J++)
            {
                for (I = 1; I <= MBJAC.v; I++)
                {
                    SUMR = 0.0E0;
                    SUMI = 0.0E0;
                    for (K = 0; K <= MM - 1; K++)
                    {
                        SUMS = SUMR + FJAC[I+(J + K * M2) * LDJAC + o_fjac];
                        SUMR = SUMS * ALP + SUMI * BET;
                        SUMI = SUMI * ALP - SUMS * BET;
                    }
                    IMLE = I + MLE.v;
                    E2R[IMLE+J * LDE1 + o_e2r] -= SUMR;
                    E2I[IMLE+J * LDE1 + o_e2i] -= SUMI;
                }
            }
            this._decbc.Run(NM1, LDE1, ref E2R, offset_e2r, ref E2I, offset_e2i, MLE.v, MUE.v
                            , ref IP2, offset_ip2, ref IER);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL3:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX
            for (J = 1; J <= N; J++)
            {
                E2R_J = J * LDE1 + o_e2r;
                FJAC_J = J * LDJAC + o_fjac;
                E2I_J = J * LDE1 + o_e2i;
                for (I = 1; I <= N; I++)
                {
                    E2R[I + E2R_J] =  - FJAC[I + FJAC_J];
                    E2I[I + E2I_J] = 0.0E0;
                }
            }
            for (J = 1; J <= N; J++)
            {
                FMAS_J = J * LDMAS + o_fmas;
                E2R_J = J * LDE1 + o_e2r;
                E2I_J = J * LDE1 + o_e2i;
                for (I = Math.Max(1, J - MUMAS); I <= Math.Min(N, J + MLMAS); I++)
                {
                    BB = FMAS[I - J + MBDIAG.v + FMAS_J];
                    E2R[I + E2R_J] += ALPHN * BB;
                    E2I[I + E2I_J] = BETAN * BB;
                }
            }
            this._decc.Run(N, LDE1, ref E2R, offset_e2r, ref E2I, offset_e2i, ref IP2, offset_ip2, ref IER);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL13:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (J = 1; J <= NM1; J++)
            {
                JM1 = J + M1;
                E2R_J = J * LDE1 + o_e2r;
                FJAC_JM1 = JM1 * LDJAC + o_fjac;
                E2I_J = J * LDE1 + o_e2i;
                for (I = 1; I <= NM1; I++)
                {
                    E2R[I + E2R_J] =  - FJAC[I + FJAC_JM1];
                    E2I[I + E2I_J] = 0.0E0;
                }
                FMAS_J = J * LDMAS + o_fmas;
                E2R_J = J * LDE1 + o_e2r;
                E2I_J = J * LDE1 + o_e2i;
                for (I = Math.Max(1, J - MUMAS); I <= Math.Min(NM1, J + MLMAS); I++)
                {
                    FFMA = FMAS[I - J + MBDIAG.v + FMAS_J];
                    E2R[I + E2R_J] += ALPHN * FFMA;
                    E2I[I + E2I_J] += BETAN * FFMA;
                }
            }
            goto LABEL45;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL4:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX
            for (J = 1; J <= N; J++)
            {
                FJAC_J = J * LDJAC + o_fjac;
                for (I = 1; I <= MBJAC.v; I++)
                {
                    IMLE = I + MLE.v;
                    E2R[IMLE+J * LDE1 + o_e2r] =  - FJAC[I + FJAC_J];
                    E2I[IMLE+J * LDE1 + o_e2i] = 0.0E0;
                }
                FMAS_J = J * LDMAS + o_fmas;
                for (I = Math.Max(1, MUMAS + 2 - J); I <= Math.Min(MBB.v, MUMAS + 1 - J + N); I++)
                {
                    IB = I + MDIFF.v;
                    BB = FMAS[I + FMAS_J];
                    E2R[IB+J * LDE1 + o_e2r] += ALPHN * BB;
                    E2I[IB+J * LDE1 + o_e2i] = BETAN * BB;
                }
            }
            this._decbc.Run(N, LDE1, ref E2R, offset_e2r, ref E2I, offset_e2i, MLE.v, MUE.v
                            , ref IP2, offset_ip2, ref IER);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL14:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX, SECOND ORDER
            for (J = 1; J <= NM1; J++)
            {
                JM1 = J + M1;
                E2R_J = J * LDE1 + o_e2r;
                FJAC_JM1 = JM1 * LDJAC + o_fjac;
                E2I_J = J * LDE1 + o_e2i;
                for (I = 1; I <= MBJAC.v; I++)
                {
                    E2R[I + MLE.v + E2R_J] =  - FJAC[I + FJAC_JM1];
                    E2I[I + MLE.v + E2I_J] = 0.0E0;
                }
                FMAS_J = J * LDMAS + o_fmas;
                for (I = 1; I <= MBB.v; I++)
                {
                    IB = I + MDIFF.v;
                    FFMA = FMAS[I + FMAS_J];
                    E2R[IB+J * LDE1 + o_e2r] += ALPHN * FFMA;
                    E2I[IB+J * LDE1 + o_e2i] += BETAN * FFMA;
                }
            }
            goto LABEL46;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL5:;
            // C ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX
            for (J = 1; J <= N; J++)
            {
                FMAS_J = J * LDMAS + o_fmas;
                E2R_J = J * LDE1 + o_e2r;
                FJAC_J = J * LDJAC + o_fjac;
                E2I_J = J * LDE1 + o_e2i;
                for (I = 1; I <= N; I++)
                {
                    BB = FMAS[I + FMAS_J];
                    E2R[I + E2R_J] = BB * ALPHN - FJAC[I + FJAC_J];
                    E2I[I + E2I_J] = BB * BETAN;
                }
            }
            this._decc.Run(N, LDE1, ref E2R, offset_e2r, ref E2I, offset_e2i, ref IP2, offset_ip2, ref IER);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL15:;
            // C ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (J = 1; J <= NM1; J++)
            {
                JM1 = J + M1;
                E2R_J = J * LDE1 + o_e2r;
                FMAS_J = J * LDMAS + o_fmas;
                FJAC_JM1 = JM1 * LDJAC + o_fjac;
                E2I_J = J * LDE1 + o_e2i;
                for (I = 1; I <= NM1; I++)
                {
                    E2R[I + E2R_J] = ALPHN * FMAS[I + FMAS_J] - FJAC[I + FJAC_JM1];
                    E2I[I + E2I_J] = BETAN * FMAS[I + FMAS_J];
                }
            }
            goto LABEL45;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL6:;
            // C ---  B IS A FULL MATRIX, JACOBIAN A BANDED MATRIX
            // C ---  THIS OPTION IS NOT PROVIDED
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL7:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX, HESSENBERG-OPTION
            for (J = 1; J <= N - 1; J++)
            {
                J1 = J + 1;
                E2R[J1+J * LDE1 + o_e2r] =  - FJAC[J1+J * LDJAC + o_fjac];
                E2I[J1+J * LDE1 + o_e2i] = 0.0E0;
            }
            for (J = 1; J <= N; J++)
            {
                E2I_J = J * LDE1 + o_e2i;
                E2R_J = J * LDE1 + o_e2r;
                FJAC_J = J * LDJAC + o_fjac;
                for (I = 1; I <= J; I++)
                {
                    E2I[I + E2I_J] = 0.0E0;
                    E2R[I + E2R_J] =  - FJAC[I + FJAC_J];
                }
                E2R[J+J * LDE1 + o_e2r] += ALPHN;
                E2I[J+J * LDE1 + o_e2i] = BETAN;
            }
            this._dechc.Run(N, LDE1, ref E2R, offset_e2r, ref E2I, offset_e2i, 1, ref IP2, offset_ip2
                            , ref IER);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL55:;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: SLVRAR
    
    // C
    // C     END OF SUBROUTINE DECOMC
    // C
    // C ***********************************************************
    // C
    public class SLVRAR
    {
    
        #region Dependencies
        
        SOL _sol; SOLB _solb; SOLH _solh; 
        #endregion
        #region Common variables
        
        #region Common Block: LINAL Declaration
        
        CommonBlock _linal;
        Oint MLE; Oint MUE; Oint MBJAC; Oint MBB; Oint MDIAG; Oint MDIFF; Oint MBDIAG; 
        #endregion
        #endregion
        public SLVRAR(SOL sol, SOLB solb, SOLH solh, CommonBlock LINAL)
        {
    
            #region Set Dependencies
            
            this._sol = sol; this._solb = solb; this._solh = solh; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
    
        public SLVRAR()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock LINAL = new CommonBlock(0, 7, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            SOL sol = new SOL();
            SOLB solb = new SOLB();
            SOLH solh = new SOLH();
            #endregion
            #region Set Dependencies
            
            this._sol = sol; this._solb = solb; this._solh = solh; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
        public void Run(int N, double[] FJAC, int offset_fjac, int LDJAC, int MLJAC, int MUJAC, double[] FMAS, int offset_fmas
                         , int LDMAS, int MLMAS, int MUMAS, int M1, int M2, int NM1
                         , double FAC1, double[] E1, int offset_e1, int LDE1, ref double[] Z1, int offset_z1, double[] F1, int offset_f1, int[] IP1, int offset_ip1
                         , int[] IPHES, int offset_iphes, int IER, int IJOB)
        {
            #region Implicit Variables
            
            int I = 0; int MM = 0; double SUM1 = 0; int JKM = 0; int J = 0; int K = 0; int IM1 = 0; int FJAC_JKM = 0; 
            double S1 = 0;int MP = 0; int MP1 = 0; double ZSAFE = 0; int FJAC_MP1 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_fjac = -1 - LDJAC + offset_fjac;  int o_fmas = -1 - LDMAS + offset_fmas;  int o_e1 = -1 - LDE1 + offset_e1; 
             int o_z1 = -1 + offset_z1; int o_f1 = -1 + offset_f1;  int o_ip1 = -1 + offset_ip1;  int o_iphes = -1 + offset_iphes; 
            #endregion
            // C
            #region Body
            
            switch (IJOB)
            {
                case 1: goto LABEL1;
                case 2: goto LABEL2;
                case 3: goto LABEL3;
                case 4: goto LABEL4;
                case 5: goto LABEL5;
                case 6: goto LABEL6;
                case 7: goto LABEL7;
                case 8: goto LABEL55;
                case 9: goto LABEL55;
                case 10: goto LABEL55;
                case 11: goto LABEL11;
                case 12: goto LABEL12;
                case 13: goto LABEL13;
                case 14: goto LABEL13;
                case 15: goto LABEL15;
            }
            // C
            // C -----------------------------------------------------------
            // C
        LABEL1:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX
            for (I = 1; I <= N; I++)
            {
                Z1[I + o_z1] +=  - F1[I + o_f1] * FAC1;
            }
            this._sol.Run(N, LDE1, E1, offset_e1, ref Z1, offset_z1, IP1, offset_ip1);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL11:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (I = 1; I <= N; I++)
            {
                Z1[I + o_z1] +=  - F1[I + o_f1] * FAC1;
            }
        LABEL48:;
            MM = M1 / M2;
            for (J = 1; J <= M2; J++)
            {
                SUM1 = 0.0E0;
                for (K = MM - 1; K >= 0; K +=  - 1)
                {
                    JKM = J + K * M2;
                    SUM1 = (Z1[JKM + o_z1] + SUM1) / FAC1;
                    FJAC_JKM = JKM * LDJAC + o_fjac;
                    for (I = 1; I <= NM1; I++)
                    {
                        IM1 = I + M1;
                        Z1[IM1 + o_z1] += FJAC[I + FJAC_JKM] * SUM1;
                    }
                }
            }
            this._sol.Run(NM1, LDE1, E1, offset_e1, ref Z1, M1 + 1 + o_z1, IP1, offset_ip1);
        LABEL49:;
            for (I = M1; I >= 1; I +=  - 1)
            {
                Z1[I + o_z1] = (Z1[I + o_z1] + Z1[M2 + I + o_z1]) / FAC1;
            }
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL2:;
            // C ---  B=IDENTITY, JACOBIAN A BANDED MATRIX
            for (I = 1; I <= N; I++)
            {
                Z1[I + o_z1] +=  - F1[I + o_f1] * FAC1;
            }
            this._solb.Run(N, LDE1, E1, offset_e1, MLE.v, MUE.v, ref Z1, offset_z1
                           , IP1, offset_ip1);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL12:;
            // C ---  B=IDENTITY, JACOBIAN A BANDED MATRIX, SECOND ORDER
            for (I = 1; I <= N; I++)
            {
                Z1[I + o_z1] +=  - F1[I + o_f1] * FAC1;
            }
        LABEL45:;
            MM = M1 / M2;
            for (J = 1; J <= M2; J++)
            {
                SUM1 = 0.0E0;
                for (K = MM - 1; K >= 0; K +=  - 1)
                {
                    JKM = J + K * M2;
                    SUM1 = (Z1[JKM + o_z1] + SUM1) / FAC1;
                    FJAC_JKM = JKM * LDJAC + o_fjac;
                    for (I = Math.Max(1, J - MUJAC); I <= Math.Min(NM1, J + MLJAC); I++)
                    {
                        IM1 = I + M1;
                        Z1[IM1 + o_z1] += FJAC[I + MUJAC + 1 - J + FJAC_JKM] * SUM1;
                    }
                }
            }
            this._solb.Run(NM1, LDE1, E1, offset_e1, MLE.v, MUE.v, ref Z1, M1 + 1 + o_z1
                           , IP1, offset_ip1);
            goto LABEL49;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL3:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX
            for (I = 1; I <= N; I++)
            {
                S1 = 0.0E0;
                for (J = Math.Max(1, I - MLMAS); J <= Math.Min(N, I + MUMAS); J++)
                {
                    S1 +=  - FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas] * F1[J + o_f1];
                }
                Z1[I + o_z1] += S1 * FAC1;
            }
            this._sol.Run(N, LDE1, E1, offset_e1, ref Z1, offset_z1, IP1, offset_ip1);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL13:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (I = 1; I <= M1; I++)
            {
                Z1[I + o_z1] +=  - F1[I + o_f1] * FAC1;
            }
            for (I = 1; I <= NM1; I++)
            {
                IM1 = I + M1;
                S1 = 0.0E0;
                for (J = Math.Max(1, I - MLMAS); J <= Math.Min(NM1, I + MUMAS); J++)
                {
                    S1 +=  - FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas] * F1[J + M1 + o_f1];
                }
                Z1[IM1 + o_z1] += S1 * FAC1;
            }
            if (IJOB == 14) goto LABEL45;
            goto LABEL48;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL4:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX
            for (I = 1; I <= N; I++)
            {
                S1 = 0.0E0;
                for (J = Math.Max(1, I - MLMAS); J <= Math.Min(N, I + MUMAS); J++)
                {
                    S1 +=  - FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas] * F1[J + o_f1];
                }
                Z1[I + o_z1] += S1 * FAC1;
            }
            this._solb.Run(N, LDE1, E1, offset_e1, MLE.v, MUE.v, ref Z1, offset_z1
                           , IP1, offset_ip1);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL5:;
            // C ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX
            for (I = 1; I <= N; I++)
            {
                S1 = 0.0E0;
                for (J = 1; J <= N; J++)
                {
                    S1 +=  - FMAS[I+J * LDMAS + o_fmas] * F1[J + o_f1];
                }
                Z1[I + o_z1] += S1 * FAC1;
            }
            this._sol.Run(N, LDE1, E1, offset_e1, ref Z1, offset_z1, IP1, offset_ip1);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL15:;
            // C ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (I = 1; I <= M1; I++)
            {
                Z1[I + o_z1] +=  - F1[I + o_f1] * FAC1;
            }
            for (I = 1; I <= NM1; I++)
            {
                IM1 = I + M1;
                S1 = 0.0E0;
                for (J = 1; J <= NM1; J++)
                {
                    S1 +=  - FMAS[I+J * LDMAS + o_fmas] * F1[J + M1 + o_f1];
                }
                Z1[IM1 + o_z1] += S1 * FAC1;
            }
            goto LABEL48;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL6:;
            // C ---  B IS A FULL MATRIX, JACOBIAN A BANDED MATRIX
            // C ---  THIS OPTION IS NOT PROVIDED
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL7:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX, HESSENBERG-OPTION
            for (I = 1; I <= N; I++)
            {
                Z1[I + o_z1] +=  - F1[I + o_f1] * FAC1;
            }
            for (MM = N - 2; MM >= 1; MM +=  - 1)
            {
                MP = N - MM;
                MP1 = MP - 1;
                I = IPHES[MP + o_iphes];
                if (I == MP) goto LABEL746;
                ZSAFE = Z1[MP + o_z1];
                Z1[MP + o_z1] = Z1[I + o_z1];
                Z1[I + o_z1] = ZSAFE;
            LABEL746:;
                FJAC_MP1 = MP1 * LDJAC + o_fjac;
                for (I = MP + 1; I <= N; I++)
                {
                    Z1[I + o_z1] +=  - FJAC[I + FJAC_MP1] * Z1[MP + o_z1];
                }
            }
            this._solh.Run(N, LDE1, E1, offset_e1, 1, ref Z1, offset_z1, IP1, offset_ip1);
            for (MM = 1; MM <= N - 2; MM++)
            {
                MP = N - MM;
                MP1 = MP - 1;
                FJAC_MP1 = MP1 * LDJAC + o_fjac;
                for (I = MP + 1; I <= N; I++)
                {
                    Z1[I + o_z1] += FJAC[I + FJAC_MP1] * Z1[MP + o_z1];
                }
                I = IPHES[MP + o_iphes];
                if (I == MP) goto LABEL750;
                ZSAFE = Z1[MP + o_z1];
                Z1[MP + o_z1] = Z1[I + o_z1];
                Z1[I + o_z1] = ZSAFE;
            LABEL750:;
            }
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL55:;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: SLVRAI
    
    // C
    // C     END OF SUBROUTINE SLVRAR
    // C
    // C ***********************************************************
    // C
    public class SLVRAI
    {
    
        #region Dependencies
        
        SOLC _solc; SOLBC _solbc; SOLHC _solhc; 
        #endregion
        #region Common variables
        
        #region Common Block: LINAL Declaration
        
        CommonBlock _linal;
        Oint MLE; Oint MUE; Oint MBJAC; Oint MBB; Oint MDIAG; Oint MDIFF; Oint MBDIAG; 
        #endregion
        #endregion
        public SLVRAI(SOLC solc, SOLBC solbc, SOLHC solhc, CommonBlock LINAL)
        {
    
            #region Set Dependencies
            
            this._solc = solc; this._solbc = solbc; this._solhc = solhc; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
    
        public SLVRAI()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock LINAL = new CommonBlock(0, 7, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            SOLC solc = new SOLC();
            SOLBC solbc = new SOLBC();
            SOLHC solhc = new SOLHC();
            #endregion
            #region Set Dependencies
            
            this._solc = solc; this._solbc = solbc; this._solhc = solhc; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
        public void Run(int N, double[] FJAC, int offset_fjac, int LDJAC, int MLJAC, int MUJAC, double[] FMAS, int offset_fmas
                         , int LDMAS, int MLMAS, int MUMAS, int M1, int M2, int NM1
                         , double ALPHN, double BETAN, double[] E2R, int offset_e2r, double[] E2I, int offset_e2i, int LDE1, ref double[] Z2, int offset_z2
                         , ref double[] Z3, int offset_z3, double[] F2, int offset_f2, double[] F3, int offset_f3, double CONT, int[] IP2, int offset_ip2, int[] IPHES, int offset_iphes
                         , int IER, int IJOB)
        {
            #region Implicit Variables
            
            double S2 = 0; int I = 0; double S3 = 0; double ABNO = 0; int MM = 0; double SUM2 = 0; double SUM3 = 0; int JKM = 0; 
            int J = 0;int K = 0; double SUMH = 0; int IM1 = 0; int FJAC_JKM = 0; int MPI = 0; double Z2I = 0; double Z3I = 0; 
            int IIMU = 0;double BB = 0; int JM1 = 0; int MP = 0; int MP1 = 0; double ZSAFE = 0; double E1IMP = 0; 
            int FJAC_MP1 = 0;
            #endregion
            #region Array Index Correction
            
             int o_fjac = -1 - LDJAC + offset_fjac;  int o_fmas = -1 - LDMAS + offset_fmas;  int o_e2r = -1 - LDE1 + offset_e2r; 
             int o_e2i = -1 - LDE1 + offset_e2i; int o_z2 = -1 + offset_z2;  int o_z3 = -1 + offset_z3; 
             int o_f2 = -1 + offset_f2; int o_f3 = -1 + offset_f3;  int o_ip2 = -1 + offset_ip2;  int o_iphes = -1 + offset_iphes; 
            #endregion
            // C
            #region Body
            
            switch (IJOB)
            {
                case 1: goto LABEL1;
                case 2: goto LABEL2;
                case 3: goto LABEL3;
                case 4: goto LABEL4;
                case 5: goto LABEL5;
                case 6: goto LABEL6;
                case 7: goto LABEL7;
                case 8: goto LABEL55;
                case 9: goto LABEL55;
                case 10: goto LABEL55;
                case 11: goto LABEL11;
                case 12: goto LABEL12;
                case 13: goto LABEL13;
                case 14: goto LABEL13;
                case 15: goto LABEL15;
            }
            // C
            // C -----------------------------------------------------------
            // C
        LABEL1:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX
            for (I = 1; I <= N; I++)
            {
                S2 =  - F2[I + o_f2];
                S3 =  - F3[I + o_f3];
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            this._solc.Run(N, LDE1, E2R, offset_e2r, E2I, offset_e2i, ref Z2, offset_z2, ref Z3, offset_z3
                           , IP2, offset_ip2);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL11:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (I = 1; I <= N; I++)
            {
                S2 =  - F2[I + o_f2];
                S3 =  - F3[I + o_f3];
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
        LABEL48:  ABNO = Math.Pow(ALPHN,2) + Math.Pow(BETAN,2);
            MM = M1 / M2;
            for (J = 1; J <= M2; J++)
            {
                SUM2 = 0.0E0;
                SUM3 = 0.0E0;
                for (K = MM - 1; K >= 0; K +=  - 1)
                {
                    JKM = J + K * M2;
                    SUMH = (Z2[JKM + o_z2] + SUM2) / ABNO;
                    SUM3 = (Z3[JKM + o_z3] + SUM3) / ABNO;
                    SUM2 = SUMH * ALPHN + SUM3 * BETAN;
                    SUM3 = SUM3 * ALPHN - SUMH * BETAN;
                    FJAC_JKM = JKM * LDJAC + o_fjac;
                    for (I = 1; I <= NM1; I++)
                    {
                        IM1 = I + M1;
                        Z2[IM1 + o_z2] += FJAC[I + FJAC_JKM] * SUM2;
                        Z3[IM1 + o_z3] += FJAC[I + FJAC_JKM] * SUM3;
                    }
                }
            }
            this._solc.Run(NM1, LDE1, E2R, offset_e2r, E2I, offset_e2i, ref Z2, M1 + 1 + o_z2, ref Z3, M1 + 1 + o_z3
                           , IP2, offset_ip2);
        LABEL49:;
            for (I = M1; I >= 1; I +=  - 1)
            {
                MPI = M2 + I;
                Z2I = Z2[I + o_z2] + Z2[MPI + o_z2];
                Z3I = Z3[I + o_z3] + Z3[MPI + o_z3];
                Z3[I + o_z3] = (Z3I * ALPHN - Z2I * BETAN) / ABNO;
                Z2[I + o_z2] = (Z2I * ALPHN + Z3I * BETAN) / ABNO;
            }
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL2:;
            // C ---  B=IDENTITY, JACOBIAN A BANDED MATRIX
            for (I = 1; I <= N; I++)
            {
                S2 =  - F2[I + o_f2];
                S3 =  - F3[I + o_f3];
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            this._solbc.Run(N, LDE1, E2R, offset_e2r, E2I, offset_e2i, MLE.v, MUE.v
                            , ref Z2, offset_z2, ref Z3, offset_z3, IP2, offset_ip2);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL12:;
            // C ---  B=IDENTITY, JACOBIAN A BANDED MATRIX, SECOND ORDER
            for (I = 1; I <= N; I++)
            {
                S2 =  - F2[I + o_f2];
                S3 =  - F3[I + o_f3];
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
        LABEL45:  ABNO = Math.Pow(ALPHN,2) + Math.Pow(BETAN,2);
            MM = M1 / M2;
            for (J = 1; J <= M2; J++)
            {
                SUM2 = 0.0E0;
                SUM3 = 0.0E0;
                for (K = MM - 1; K >= 0; K +=  - 1)
                {
                    JKM = J + K * M2;
                    SUMH = (Z2[JKM + o_z2] + SUM2) / ABNO;
                    SUM3 = (Z3[JKM + o_z3] + SUM3) / ABNO;
                    SUM2 = SUMH * ALPHN + SUM3 * BETAN;
                    SUM3 = SUM3 * ALPHN - SUMH * BETAN;
                    for (I = Math.Max(1, J - MUJAC); I <= Math.Min(NM1, J + MLJAC); I++)
                    {
                        IM1 = I + M1;
                        IIMU = I + MUJAC + 1 - J;
                        Z2[IM1 + o_z2] += FJAC[IIMU+JKM * LDJAC + o_fjac] * SUM2;
                        Z3[IM1 + o_z3] += FJAC[IIMU+JKM * LDJAC + o_fjac] * SUM3;
                    }
                }
            }
            this._solbc.Run(NM1, LDE1, E2R, offset_e2r, E2I, offset_e2i, MLE.v, MUE.v
                            , ref Z2, M1 + 1 + o_z2, ref Z3, M1 + 1 + o_z3, IP2, offset_ip2);
            goto LABEL49;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL3:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX
            for (I = 1; I <= N; I++)
            {
                S2 = 0.0E0;
                S3 = 0.0E0;
                for (J = Math.Max(1, I - MLMAS); J <= Math.Min(N, I + MUMAS); J++)
                {
                    BB = FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas];
                    S2 +=  - BB * F2[J + o_f2];
                    S3 +=  - BB * F3[J + o_f3];
                }
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            this._solc.Run(N, LDE1, E2R, offset_e2r, E2I, offset_e2i, ref Z2, offset_z2, ref Z3, offset_z3
                           , IP2, offset_ip2);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL13:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (I = 1; I <= M1; I++)
            {
                S2 =  - F2[I + o_f2];
                S3 =  - F3[I + o_f3];
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            for (I = 1; I <= NM1; I++)
            {
                IM1 = I + M1;
                S2 = 0.0E0;
                S3 = 0.0E0;
                for (J = Math.Max(1, I - MLMAS); J <= Math.Min(NM1, I + MUMAS); J++)
                {
                    JM1 = J + M1;
                    BB = FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas];
                    S2 +=  - BB * F2[JM1 + o_f2];
                    S3 +=  - BB * F3[JM1 + o_f3];
                }
                Z2[IM1 + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[IM1 + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            if (IJOB == 14) goto LABEL45;
            goto LABEL48;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL4:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX
            for (I = 1; I <= N; I++)
            {
                S2 = 0.0E0;
                S3 = 0.0E0;
                for (J = Math.Max(1, I - MLMAS); J <= Math.Min(N, I + MUMAS); J++)
                {
                    BB = FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas];
                    S2 +=  - BB * F2[J + o_f2];
                    S3 +=  - BB * F3[J + o_f3];
                }
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            this._solbc.Run(N, LDE1, E2R, offset_e2r, E2I, offset_e2i, MLE.v, MUE.v
                            , ref Z2, offset_z2, ref Z3, offset_z3, IP2, offset_ip2);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL5:;
            // C ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX
            for (I = 1; I <= N; I++)
            {
                S2 = 0.0E0;
                S3 = 0.0E0;
                for (J = 1; J <= N; J++)
                {
                    BB = FMAS[I+J * LDMAS + o_fmas];
                    S2 +=  - BB * F2[J + o_f2];
                    S3 +=  - BB * F3[J + o_f3];
                }
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            this._solc.Run(N, LDE1, E2R, offset_e2r, E2I, offset_e2i, ref Z2, offset_z2, ref Z3, offset_z3
                           , IP2, offset_ip2);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL15:;
            // C ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (I = 1; I <= M1; I++)
            {
                S2 =  - F2[I + o_f2];
                S3 =  - F3[I + o_f3];
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            for (I = 1; I <= NM1; I++)
            {
                IM1 = I + M1;
                S2 = 0.0E0;
                S3 = 0.0E0;
                for (J = 1; J <= NM1; J++)
                {
                    JM1 = J + M1;
                    BB = FMAS[I+J * LDMAS + o_fmas];
                    S2 +=  - BB * F2[JM1 + o_f2];
                    S3 +=  - BB * F3[JM1 + o_f3];
                }
                Z2[IM1 + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[IM1 + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            goto LABEL48;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL6:;
            // C ---  B IS A FULL MATRIX, JACOBIAN A BANDED MATRIX
            // C ---  THIS OPTION IS NOT PROVIDED
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL7:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX, HESSENBERG-OPTION
            for (I = 1; I <= N; I++)
            {
                S2 =  - F2[I + o_f2];
                S3 =  - F3[I + o_f3];
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            for (MM = N - 2; MM >= 1; MM +=  - 1)
            {
                MP = N - MM;
                MP1 = MP - 1;
                I = IPHES[MP + o_iphes];
                if (I == MP) goto LABEL746;
                ZSAFE = Z2[MP + o_z2];
                Z2[MP + o_z2] = Z2[I + o_z2];
                Z2[I + o_z2] = ZSAFE;
                ZSAFE = Z3[MP + o_z3];
                Z3[MP + o_z3] = Z3[I + o_z3];
                Z3[I + o_z3] = ZSAFE;
            LABEL746:;
                FJAC_MP1 = MP1 * LDJAC + o_fjac;
                for (I = MP + 1; I <= N; I++)
                {
                    E1IMP = FJAC[I + FJAC_MP1];
                    Z2[I + o_z2] +=  - E1IMP * Z2[MP + o_z2];
                    Z3[I + o_z3] +=  - E1IMP * Z3[MP + o_z3];
                }
            }
            this._solhc.Run(N, LDE1, E2R, offset_e2r, E2I, offset_e2i, 1, ref Z2, offset_z2
                            , ref Z3, offset_z3, IP2, offset_ip2);
            for (MM = 1; MM <= N - 2; MM++)
            {
                MP = N - MM;
                MP1 = MP - 1;
                FJAC_MP1 = MP1 * LDJAC + o_fjac;
                for (I = MP + 1; I <= N; I++)
                {
                    E1IMP = FJAC[I + FJAC_MP1];
                    Z2[I + o_z2] += E1IMP * Z2[MP + o_z2];
                    Z3[I + o_z3] += E1IMP * Z3[MP + o_z3];
                }
                I = IPHES[MP + o_iphes];
                if (I == MP) goto LABEL750;
                ZSAFE = Z2[MP + o_z2];
                Z2[MP + o_z2] = Z2[I + o_z2];
                Z2[I + o_z2] = ZSAFE;
                ZSAFE = Z3[MP + o_z3];
                Z3[MP + o_z3] = Z3[I + o_z3];
                Z3[I + o_z3] = ZSAFE;
            LABEL750:;
            }
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL55:;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: SLVRAD
    
    // C
    // C     END OF SUBROUTINE SLVRAI
    // C
    // C ***********************************************************
    // C
    public class SLVRAD
    {
    
        #region Dependencies
        
        SOL _sol; SOLC _solc; SOLB _solb; SOLBC _solbc; SOLH _solh; SOLHC _solhc; 
        #endregion
        #region Common variables
        
        #region Common Block: LINAL Declaration
        
        CommonBlock _linal;
        Oint MLE; Oint MUE; Oint MBJAC; Oint MBB; Oint MDIAG; Oint MDIFF; Oint MBDIAG; 
        #endregion
        #endregion
        public SLVRAD(SOL sol, SOLC solc, SOLB solb, SOLBC solbc, SOLH solh, SOLHC solhc, CommonBlock LINAL)
        {
    
            #region Set Dependencies
            
            this._sol = sol; this._solc = solc; this._solb = solb; this._solbc = solbc; this._solh = solh; this._solhc = solhc; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
    
        public SLVRAD()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock LINAL = new CommonBlock(0, 7, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            SOL sol = new SOL();
            SOLC solc = new SOLC();
            SOLB solb = new SOLB();
            SOLBC solbc = new SOLBC();
            SOLH solh = new SOLH();
            SOLHC solhc = new SOLHC();
            #endregion
            #region Set Dependencies
            
            this._sol = sol; this._solc = solc; this._solb = solb; this._solbc = solbc; this._solh = solh; this._solhc = solhc; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
        public void Run(int N, double[] FJAC, int offset_fjac, int LDJAC, int MLJAC, int MUJAC, double[] FMAS, int offset_fmas
                         , int LDMAS, int MLMAS, int MUMAS, int M1, int M2, int NM1
                         , double FAC1, double ALPHN, double BETAN, double[] E1, int offset_e1, double[] E2R, int offset_e2r, double[] E2I, int offset_e2i
                         , int LDE1, ref double[] Z1, int offset_z1, ref double[] Z2, int offset_z2, ref double[] Z3, int offset_z3, double[] F1, int offset_f1, double[] F2, int offset_f2
                         , double[] F3, int offset_f3, double CONT, int[] IP1, int offset_ip1, int[] IP2, int offset_ip2, int[] IPHES, int offset_iphes, int IER
                         , int IJOB)
        {
            #region Implicit Variables
            
            double S2 = 0; int I = 0; double S3 = 0; double ABNO = 0; int MM = 0; double SUM1 = 0; double SUM2 = 0; 
            double SUM3 = 0;int JKM = 0; int J = 0; int K = 0; double SUMH = 0; int IM1 = 0; int FJAC_JKM = 0; int MPI = 0; 
            double Z2I = 0;double Z3I = 0; double FFJA = 0; double S1 = 0; double BB = 0; int J1B = 0; int J2B = 0; int JM1 = 0; 
            int MP = 0;int MP1 = 0; double ZSAFE = 0; double E1IMP = 0; int FJAC_MP1 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_fjac = -1 - LDJAC + offset_fjac;  int o_fmas = -1 - LDMAS + offset_fmas;  int o_e1 = -1 - LDE1 + offset_e1; 
             int o_e2r = -1 - LDE1 + offset_e2r; int o_e2i = -1 - LDE1 + offset_e2i;  int o_z1 = -1 + offset_z1; 
             int o_z2 = -1 + offset_z2; int o_z3 = -1 + offset_z3;  int o_f1 = -1 + offset_f1;  int o_f2 = -1 + offset_f2; 
             int o_f3 = -1 + offset_f3; int o_ip1 = -1 + offset_ip1;  int o_ip2 = -1 + offset_ip2; 
             int o_iphes = -1 + offset_iphes;
            #endregion
            // C
            #region Body
            
            switch (IJOB)
            {
                case 1: goto LABEL1;
                case 2: goto LABEL2;
                case 3: goto LABEL3;
                case 4: goto LABEL4;
                case 5: goto LABEL5;
                case 6: goto LABEL6;
                case 7: goto LABEL7;
                case 8: goto LABEL55;
                case 9: goto LABEL55;
                case 10: goto LABEL55;
                case 11: goto LABEL11;
                case 12: goto LABEL12;
                case 13: goto LABEL13;
                case 14: goto LABEL13;
                case 15: goto LABEL15;
            }
            // C
            // C -----------------------------------------------------------
            // C
        LABEL1:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX
            for (I = 1; I <= N; I++)
            {
                S2 =  - F2[I + o_f2];
                S3 =  - F3[I + o_f3];
                Z1[I + o_z1] +=  - F1[I + o_f1] * FAC1;
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            this._sol.Run(N, LDE1, E1, offset_e1, ref Z1, offset_z1, IP1, offset_ip1);
            this._solc.Run(N, LDE1, E2R, offset_e2r, E2I, offset_e2i, ref Z2, offset_z2, ref Z3, offset_z3
                           , IP2, offset_ip2);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL11:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (I = 1; I <= N; I++)
            {
                S2 =  - F2[I + o_f2];
                S3 =  - F3[I + o_f3];
                Z1[I + o_z1] +=  - F1[I + o_f1] * FAC1;
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
        LABEL48:  ABNO = Math.Pow(ALPHN,2) + Math.Pow(BETAN,2);
            MM = M1 / M2;
            for (J = 1; J <= M2; J++)
            {
                SUM1 = 0.0E0;
                SUM2 = 0.0E0;
                SUM3 = 0.0E0;
                for (K = MM - 1; K >= 0; K +=  - 1)
                {
                    JKM = J + K * M2;
                    SUM1 = (Z1[JKM + o_z1] + SUM1) / FAC1;
                    SUMH = (Z2[JKM + o_z2] + SUM2) / ABNO;
                    SUM3 = (Z3[JKM + o_z3] + SUM3) / ABNO;
                    SUM2 = SUMH * ALPHN + SUM3 * BETAN;
                    SUM3 = SUM3 * ALPHN - SUMH * BETAN;
                    FJAC_JKM = JKM * LDJAC + o_fjac;
                    for (I = 1; I <= NM1; I++)
                    {
                        IM1 = I + M1;
                        Z1[IM1 + o_z1] += FJAC[I + FJAC_JKM] * SUM1;
                        Z2[IM1 + o_z2] += FJAC[I + FJAC_JKM] * SUM2;
                        Z3[IM1 + o_z3] += FJAC[I + FJAC_JKM] * SUM3;
                    }
                }
            }
            this._sol.Run(NM1, LDE1, E1, offset_e1, ref Z1, M1 + 1 + o_z1, IP1, offset_ip1);
            this._solc.Run(NM1, LDE1, E2R, offset_e2r, E2I, offset_e2i, ref Z2, M1 + 1 + o_z2, ref Z3, M1 + 1 + o_z3
                           , IP2, offset_ip2);
        LABEL49:;
            for (I = M1; I >= 1; I +=  - 1)
            {
                MPI = M2 + I;
                Z1[I + o_z1] = (Z1[I + o_z1] + Z1[MPI + o_z1]) / FAC1;
                Z2I = Z2[I + o_z2] + Z2[MPI + o_z2];
                Z3I = Z3[I + o_z3] + Z3[MPI + o_z3];
                Z3[I + o_z3] = (Z3I * ALPHN - Z2I * BETAN) / ABNO;
                Z2[I + o_z2] = (Z2I * ALPHN + Z3I * BETAN) / ABNO;
            }
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL2:;
            // C ---  B=IDENTITY, JACOBIAN A BANDED MATRIX
            for (I = 1; I <= N; I++)
            {
                S2 =  - F2[I + o_f2];
                S3 =  - F3[I + o_f3];
                Z1[I + o_z1] +=  - F1[I + o_f1] * FAC1;
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            this._solb.Run(N, LDE1, E1, offset_e1, MLE.v, MUE.v, ref Z1, offset_z1
                           , IP1, offset_ip1);
            this._solbc.Run(N, LDE1, E2R, offset_e2r, E2I, offset_e2i, MLE.v, MUE.v
                            , ref Z2, offset_z2, ref Z3, offset_z3, IP2, offset_ip2);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL12:;
            // C ---  B=IDENTITY, JACOBIAN A BANDED MATRIX, SECOND ORDER
            for (I = 1; I <= N; I++)
            {
                S2 =  - F2[I + o_f2];
                S3 =  - F3[I + o_f3];
                Z1[I + o_z1] +=  - F1[I + o_f1] * FAC1;
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
        LABEL45:  ABNO = Math.Pow(ALPHN,2) + Math.Pow(BETAN,2);
            MM = M1 / M2;
            for (J = 1; J <= M2; J++)
            {
                SUM1 = 0.0E0;
                SUM2 = 0.0E0;
                SUM3 = 0.0E0;
                for (K = MM - 1; K >= 0; K +=  - 1)
                {
                    JKM = J + K * M2;
                    SUM1 = (Z1[JKM + o_z1] + SUM1) / FAC1;
                    SUMH = (Z2[JKM + o_z2] + SUM2) / ABNO;
                    SUM3 = (Z3[JKM + o_z3] + SUM3) / ABNO;
                    SUM2 = SUMH * ALPHN + SUM3 * BETAN;
                    SUM3 = SUM3 * ALPHN - SUMH * BETAN;
                    FJAC_JKM = JKM * LDJAC + o_fjac;
                    for (I = Math.Max(1, J - MUJAC); I <= Math.Min(NM1, J + MLJAC); I++)
                    {
                        IM1 = I + M1;
                        FFJA = FJAC[I + MUJAC + 1 - J + FJAC_JKM];
                        Z1[IM1 + o_z1] += FFJA * SUM1;
                        Z2[IM1 + o_z2] += FFJA * SUM2;
                        Z3[IM1 + o_z3] += FFJA * SUM3;
                    }
                }
            }
            this._solb.Run(NM1, LDE1, E1, offset_e1, MLE.v, MUE.v, ref Z1, M1 + 1 + o_z1
                           , IP1, offset_ip1);
            this._solbc.Run(NM1, LDE1, E2R, offset_e2r, E2I, offset_e2i, MLE.v, MUE.v
                            , ref Z2, M1 + 1 + o_z2, ref Z3, M1 + 1 + o_z3, IP2, offset_ip2);
            goto LABEL49;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL3:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX
            for (I = 1; I <= N; I++)
            {
                S1 = 0.0E0;
                S2 = 0.0E0;
                S3 = 0.0E0;
                for (J = Math.Max(1, I - MLMAS); J <= Math.Min(N, I + MUMAS); J++)
                {
                    BB = FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas];
                    S1 +=  - BB * F1[J + o_f1];
                    S2 +=  - BB * F2[J + o_f2];
                    S3 +=  - BB * F3[J + o_f3];
                }
                Z1[I + o_z1] += S1 * FAC1;
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            this._sol.Run(N, LDE1, E1, offset_e1, ref Z1, offset_z1, IP1, offset_ip1);
            this._solc.Run(N, LDE1, E2R, offset_e2r, E2I, offset_e2i, ref Z2, offset_z2, ref Z3, offset_z3
                           , IP2, offset_ip2);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL13:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (I = 1; I <= M1; I++)
            {
                S2 =  - F2[I + o_f2];
                S3 =  - F3[I + o_f3];
                Z1[I + o_z1] +=  - F1[I + o_f1] * FAC1;
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            for (I = 1; I <= NM1; I++)
            {
                IM1 = I + M1;
                S1 = 0.0E0;
                S2 = 0.0E0;
                S3 = 0.0E0;
                J1B = Math.Max(1, I - MLMAS);
                J2B = Math.Min(NM1, I + MUMAS);
                for (J = J1B; J <= J2B; J++)
                {
                    JM1 = J + M1;
                    BB = FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas];
                    S1 +=  - BB * F1[JM1 + o_f1];
                    S2 +=  - BB * F2[JM1 + o_f2];
                    S3 +=  - BB * F3[JM1 + o_f3];
                }
                Z1[IM1 + o_z1] += S1 * FAC1;
                Z2[IM1 + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[IM1 + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            if (IJOB == 14) goto LABEL45;
            goto LABEL48;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL4:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX
            for (I = 1; I <= N; I++)
            {
                S1 = 0.0E0;
                S2 = 0.0E0;
                S3 = 0.0E0;
                for (J = Math.Max(1, I - MLMAS); J <= Math.Min(N, I + MUMAS); J++)
                {
                    BB = FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas];
                    S1 +=  - BB * F1[J + o_f1];
                    S2 +=  - BB * F2[J + o_f2];
                    S3 +=  - BB * F3[J + o_f3];
                }
                Z1[I + o_z1] += S1 * FAC1;
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            this._solb.Run(N, LDE1, E1, offset_e1, MLE.v, MUE.v, ref Z1, offset_z1
                           , IP1, offset_ip1);
            this._solbc.Run(N, LDE1, E2R, offset_e2r, E2I, offset_e2i, MLE.v, MUE.v
                            , ref Z2, offset_z2, ref Z3, offset_z3, IP2, offset_ip2);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL5:;
            // C ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX
            for (I = 1; I <= N; I++)
            {
                S1 = 0.0E0;
                S2 = 0.0E0;
                S3 = 0.0E0;
                for (J = 1; J <= N; J++)
                {
                    BB = FMAS[I+J * LDMAS + o_fmas];
                    S1 +=  - BB * F1[J + o_f1];
                    S2 +=  - BB * F2[J + o_f2];
                    S3 +=  - BB * F3[J + o_f3];
                }
                Z1[I + o_z1] += S1 * FAC1;
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            this._sol.Run(N, LDE1, E1, offset_e1, ref Z1, offset_z1, IP1, offset_ip1);
            this._solc.Run(N, LDE1, E2R, offset_e2r, E2I, offset_e2i, ref Z2, offset_z2, ref Z3, offset_z3
                           , IP2, offset_ip2);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL15:;
            // C ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (I = 1; I <= M1; I++)
            {
                S2 =  - F2[I + o_f2];
                S3 =  - F3[I + o_f3];
                Z1[I + o_z1] +=  - F1[I + o_f1] * FAC1;
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            for (I = 1; I <= NM1; I++)
            {
                IM1 = I + M1;
                S1 = 0.0E0;
                S2 = 0.0E0;
                S3 = 0.0E0;
                for (J = 1; J <= NM1; J++)
                {
                    JM1 = J + M1;
                    BB = FMAS[I+J * LDMAS + o_fmas];
                    S1 +=  - BB * F1[JM1 + o_f1];
                    S2 +=  - BB * F2[JM1 + o_f2];
                    S3 +=  - BB * F3[JM1 + o_f3];
                }
                Z1[IM1 + o_z1] += S1 * FAC1;
                Z2[IM1 + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[IM1 + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            goto LABEL48;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL6:;
            // C ---  B IS A FULL MATRIX, JACOBIAN A BANDED MATRIX
            // C ---  THIS OPTION IS NOT PROVIDED
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL7:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX, HESSENBERG-OPTION
            for (I = 1; I <= N; I++)
            {
                S2 =  - F2[I + o_f2];
                S3 =  - F3[I + o_f3];
                Z1[I + o_z1] +=  - F1[I + o_f1] * FAC1;
                Z2[I + o_z2] += S2 * ALPHN - S3 * BETAN;
                Z3[I + o_z3] += S3 * ALPHN + S2 * BETAN;
            }
            for (MM = N - 2; MM >= 1; MM +=  - 1)
            {
                MP = N - MM;
                MP1 = MP - 1;
                I = IPHES[MP + o_iphes];
                if (I == MP) goto LABEL746;
                ZSAFE = Z1[MP + o_z1];
                Z1[MP + o_z1] = Z1[I + o_z1];
                Z1[I + o_z1] = ZSAFE;
                ZSAFE = Z2[MP + o_z2];
                Z2[MP + o_z2] = Z2[I + o_z2];
                Z2[I + o_z2] = ZSAFE;
                ZSAFE = Z3[MP + o_z3];
                Z3[MP + o_z3] = Z3[I + o_z3];
                Z3[I + o_z3] = ZSAFE;
            LABEL746:;
                FJAC_MP1 = MP1 * LDJAC + o_fjac;
                for (I = MP + 1; I <= N; I++)
                {
                    E1IMP = FJAC[I + FJAC_MP1];
                    Z1[I + o_z1] +=  - E1IMP * Z1[MP + o_z1];
                    Z2[I + o_z2] +=  - E1IMP * Z2[MP + o_z2];
                    Z3[I + o_z3] +=  - E1IMP * Z3[MP + o_z3];
                }
            }
            this._solh.Run(N, LDE1, E1, offset_e1, 1, ref Z1, offset_z1, IP1, offset_ip1);
            this._solhc.Run(N, LDE1, E2R, offset_e2r, E2I, offset_e2i, 1, ref Z2, offset_z2
                            , ref Z3, offset_z3, IP2, offset_ip2);
            for (MM = 1; MM <= N - 2; MM++)
            {
                MP = N - MM;
                MP1 = MP - 1;
                FJAC_MP1 = MP1 * LDJAC + o_fjac;
                for (I = MP + 1; I <= N; I++)
                {
                    E1IMP = FJAC[I + FJAC_MP1];
                    Z1[I + o_z1] += E1IMP * Z1[MP + o_z1];
                    Z2[I + o_z2] += E1IMP * Z2[MP + o_z2];
                    Z3[I + o_z3] += E1IMP * Z3[MP + o_z3];
                }
                I = IPHES[MP + o_iphes];
                if (I == MP) goto LABEL750;
                ZSAFE = Z1[MP + o_z1];
                Z1[MP + o_z1] = Z1[I + o_z1];
                Z1[I + o_z1] = ZSAFE;
                ZSAFE = Z2[MP + o_z2];
                Z2[MP + o_z2] = Z2[I + o_z2];
                Z2[I + o_z2] = ZSAFE;
                ZSAFE = Z3[MP + o_z3];
                Z3[MP + o_z3] = Z3[I + o_z3];
                Z3[I + o_z3] = ZSAFE;
            LABEL750:;
            }
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL55:;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: ESTRAD
    
    // C
    // C     END OF SUBROUTINE SLVRAD
    // C
    // C ***********************************************************
    // C
    public class ESTRAD
    {
    
        #region Dependencies
        
        SOL _sol; SOLB _solb; SOLH _solh; 
        #endregion
        #region Common variables
        
        #region Common Block: LINAL Declaration
        
        CommonBlock _linal;
        Oint MLE; Oint MUE; Oint MBJAC; Oint MBB; Oint MDIAG; Oint MDIFF; Oint MBDIAG; 
        #endregion
        #endregion
        public ESTRAD(SOL sol, SOLB solb, SOLH solh, CommonBlock LINAL)
        {
    
            #region Set Dependencies
            
            this._sol = sol; this._solb = solb; this._solh = solh; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
    
        public ESTRAD()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock LINAL = new CommonBlock(0, 7, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            SOL sol = new SOL();
            SOLB solb = new SOLB();
            SOLH solh = new SOLH();
            #endregion
            #region Set Dependencies
            
            this._sol = sol; this._solb = solb; this._solh = solh; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
        public void Run(int N, double[] FJAC, int offset_fjac, int LDJAC, int MLJAC, int MUJAC, double[] FMAS, int offset_fmas
                         , int LDMAS, int MLMAS, int MUMAS, double H, double DD1, double DD2
                         , double DD3, IFVPOL FCN, ref int NFCN, double[] Y0, int offset_y0, double[] Y, int offset_y, int IJOB
                         , double X, int M1, int M2, int NM1, double[] E1, int offset_e1, int LDE1
                         , double[] Z1, int offset_z1, double[] Z2, int offset_z2, double[] Z3, int offset_z3, ref double[] CONT, int offset_cont, ref double[] F1, int offset_f1, ref double[] F2, int offset_f2
                         , int[] IP1, int offset_ip1, int[] IPHES, int offset_iphes, double[] SCAL, int offset_scal, ref double ERR, bool FIRST, bool REJECT
                         , double FAC1, double[] RPAR, int offset_rpar, int[] IPAR, int offset_ipar)
        {
            #region Implicit Variables
            
            double HEE1 = 0; double HEE2 = 0; double HEE3 = 0; int I = 0; int MM = 0; double SUM1 = 0; int J = 0; int K = 0; 
            int IM1 = 0;int FJAC_0 = 0; int FJAC_1 = 0; double SUM = 0; int MP = 0; double ZSAFE = 0; int FJAC_2 = 0; 
            int FJAC_3 = 0;int FJAC_4 = 0; int FJAC_5 = 0; int FJAC_6 = 0; int FJAC_7 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_fjac = -1 - LDJAC + offset_fjac;  int o_fmas = -1 - LDMAS + offset_fmas;  int o_y0 = -1 + offset_y0; 
             int o_y = -1 + offset_y; int o_e1 = -1 - LDE1 + offset_e1;  int o_z1 = -1 + offset_z1;  int o_z2 = -1 + offset_z2; 
             int o_z3 = -1 + offset_z3; int o_cont = -1 + offset_cont;  int o_f1 = -1 + offset_f1;  int o_f2 = -1 + offset_f2; 
             int o_ip1 = -1 + offset_ip1; int o_iphes = -1 + offset_iphes;  int o_scal = -1 + offset_scal; 
             int o_rpar = -1 + offset_rpar; int o_ipar = -1 + offset_ipar; 
            #endregion
            #region Body
            
            HEE1 = DD1 / H;
            HEE2 = DD2 / H;
            HEE3 = DD3 / H;
            switch (IJOB)
            {
                case 1: goto LABEL1;
                case 2: goto LABEL2;
                case 3: goto LABEL3;
                case 4: goto LABEL4;
                case 5: goto LABEL5;
                case 6: goto LABEL6;
                case 7: goto LABEL7;
                case 8: goto LABEL55;
                case 9: goto LABEL55;
                case 10: goto LABEL55;
                case 11: goto LABEL11;
                case 12: goto LABEL12;
                case 13: goto LABEL13;
                case 14: goto LABEL14;
                case 15: goto LABEL15;
            }
            // C
        LABEL1:;
            // C ------  B=IDENTITY, JACOBIAN A FULL MATRIX
            for (I = 1; I <= N; I++)
            {
                F2[I + o_f2] = HEE1 * Z1[I + o_z1] + HEE2 * Z2[I + o_z2] + HEE3 * Z3[I + o_z3];
                CONT[I + o_cont] = F2[I + o_f2] + Y0[I + o_y0];
            }
            this._sol.Run(N, LDE1, E1, offset_e1, ref CONT, offset_cont, IP1, offset_ip1);
            goto LABEL77;
            // C
        LABEL11:;
            // C ------  B=IDENTITY, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (I = 1; I <= N; I++)
            {
                F2[I + o_f2] = HEE1 * Z1[I + o_z1] + HEE2 * Z2[I + o_z2] + HEE3 * Z3[I + o_z3];
                CONT[I + o_cont] = F2[I + o_f2] + Y0[I + o_y0];
            }
        LABEL48:  MM = M1 / M2;
            for (J = 1; J <= M2; J++)
            {
                SUM1 = 0.0E0;
                for (K = MM - 1; K >= 0; K +=  - 1)
                {
                    SUM1 = (CONT[J + K * M2 + o_cont] + SUM1) / FAC1;
                    FJAC_0 = (J + K * M2) * LDJAC + o_fjac;
                    for (I = 1; I <= NM1; I++)
                    {
                        IM1 = I + M1;
                        CONT[IM1 + o_cont] += FJAC[I + FJAC_0] * SUM1;
                    }
                }
            }
            this._sol.Run(NM1, LDE1, E1, offset_e1, ref CONT, M1 + 1 + o_cont, IP1, offset_ip1);
            for (I = M1; I >= 1; I +=  - 1)
            {
                CONT[I + o_cont] = (CONT[I + o_cont] + CONT[M2 + I + o_cont]) / FAC1;
            }
            goto LABEL77;
            // C
        LABEL2:;
            // C ------  B=IDENTITY, JACOBIAN A BANDED MATRIX
            for (I = 1; I <= N; I++)
            {
                F2[I + o_f2] = HEE1 * Z1[I + o_z1] + HEE2 * Z2[I + o_z2] + HEE3 * Z3[I + o_z3];
                CONT[I + o_cont] = F2[I + o_f2] + Y0[I + o_y0];
            }
            this._solb.Run(N, LDE1, E1, offset_e1, MLE.v, MUE.v, ref CONT, offset_cont
                           , IP1, offset_ip1);
            goto LABEL77;
            // C
        LABEL12:;
            // C ------  B=IDENTITY, JACOBIAN A BANDED MATRIX, SECOND ORDER
            for (I = 1; I <= N; I++)
            {
                F2[I + o_f2] = HEE1 * Z1[I + o_z1] + HEE2 * Z2[I + o_z2] + HEE3 * Z3[I + o_z3];
                CONT[I + o_cont] = F2[I + o_f2] + Y0[I + o_y0];
            }
        LABEL45:  MM = M1 / M2;
            for (J = 1; J <= M2; J++)
            {
                SUM1 = 0.0E0;
                for (K = MM - 1; K >= 0; K +=  - 1)
                {
                    SUM1 = (CONT[J + K * M2 + o_cont] + SUM1) / FAC1;
                    FJAC_1 = (J + K * M2) * LDJAC + o_fjac;
                    for (I = Math.Max(1, J - MUJAC); I <= Math.Min(NM1, J + MLJAC); I++)
                    {
                        IM1 = I + M1;
                        CONT[IM1 + o_cont] += FJAC[I + MUJAC + 1 - J + FJAC_1] * SUM1;
                    }
                }
            }
            this._solb.Run(NM1, LDE1, E1, offset_e1, MLE.v, MUE.v, ref CONT, M1 + 1 + o_cont
                           , IP1, offset_ip1);
            for (I = M1; I >= 1; I +=  - 1)
            {
                CONT[I + o_cont] = (CONT[I + o_cont] + CONT[M2 + I + o_cont]) / FAC1;
            }
            goto LABEL77;
            // C
        LABEL3:;
            // C ------  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX
            for (I = 1; I <= N; I++)
            {
                F1[I + o_f1] = HEE1 * Z1[I + o_z1] + HEE2 * Z2[I + o_z2] + HEE3 * Z3[I + o_z3];
            }
            for (I = 1; I <= N; I++)
            {
                SUM = 0.0E0;
                for (J = Math.Max(1, I - MLMAS); J <= Math.Min(N, I + MUMAS); J++)
                {
                    SUM += FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas] * F1[J + o_f1];
                }
                F2[I + o_f2] = SUM;
                CONT[I + o_cont] = SUM + Y0[I + o_y0];
            }
            this._sol.Run(N, LDE1, E1, offset_e1, ref CONT, offset_cont, IP1, offset_ip1);
            goto LABEL77;
            // C
        LABEL13:;
            // C ------  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (I = 1; I <= M1; I++)
            {
                F2[I + o_f2] = HEE1 * Z1[I + o_z1] + HEE2 * Z2[I + o_z2] + HEE3 * Z3[I + o_z3];
                CONT[I + o_cont] = F2[I + o_f2] + Y0[I + o_y0];
            }
            for (I = M1 + 1; I <= N; I++)
            {
                F1[I + o_f1] = HEE1 * Z1[I + o_z1] + HEE2 * Z2[I + o_z2] + HEE3 * Z3[I + o_z3];
            }
            for (I = 1; I <= NM1; I++)
            {
                SUM = 0.0E0;
                for (J = Math.Max(1, I - MLMAS); J <= Math.Min(NM1, I + MUMAS); J++)
                {
                    SUM += FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas] * F1[J + M1 + o_f1];
                }
                IM1 = I + M1;
                F2[IM1 + o_f2] = SUM;
                CONT[IM1 + o_cont] = SUM + Y0[IM1 + o_y0];
            }
            goto LABEL48;
            // C
        LABEL4:;
            // C ------  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX
            for (I = 1; I <= N; I++)
            {
                F1[I + o_f1] = HEE1 * Z1[I + o_z1] + HEE2 * Z2[I + o_z2] + HEE3 * Z3[I + o_z3];
            }
            for (I = 1; I <= N; I++)
            {
                SUM = 0.0E0;
                for (J = Math.Max(1, I - MLMAS); J <= Math.Min(N, I + MUMAS); J++)
                {
                    SUM += FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas] * F1[J + o_f1];
                }
                F2[I + o_f2] = SUM;
                CONT[I + o_cont] = SUM + Y0[I + o_y0];
            }
            this._solb.Run(N, LDE1, E1, offset_e1, MLE.v, MUE.v, ref CONT, offset_cont
                           , IP1, offset_ip1);
            goto LABEL77;
            // C
        LABEL14:;
            // C ------  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX, SECOND ORDER
            for (I = 1; I <= M1; I++)
            {
                F2[I + o_f2] = HEE1 * Z1[I + o_z1] + HEE2 * Z2[I + o_z2] + HEE3 * Z3[I + o_z3];
                CONT[I + o_cont] = F2[I + o_f2] + Y0[I + o_y0];
            }
            for (I = M1 + 1; I <= N; I++)
            {
                F1[I + o_f1] = HEE1 * Z1[I + o_z1] + HEE2 * Z2[I + o_z2] + HEE3 * Z3[I + o_z3];
            }
            for (I = 1; I <= NM1; I++)
            {
                SUM = 0.0E0;
                for (J = Math.Max(1, I - MLMAS); J <= Math.Min(NM1, I + MUMAS); J++)
                {
                    SUM += FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas] * F1[J + M1 + o_f1];
                }
                IM1 = I + M1;
                F2[IM1 + o_f2] = SUM;
                CONT[IM1 + o_cont] = SUM + Y0[IM1 + o_y0];
            }
            goto LABEL45;
            // C
        LABEL5:;
            // C ------  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX
            for (I = 1; I <= N; I++)
            {
                F1[I + o_f1] = HEE1 * Z1[I + o_z1] + HEE2 * Z2[I + o_z2] + HEE3 * Z3[I + o_z3];
            }
            for (I = 1; I <= N; I++)
            {
                SUM = 0.0E0;
                for (J = 1; J <= N; J++)
                {
                    SUM += FMAS[I+J * LDMAS + o_fmas] * F1[J + o_f1];
                }
                F2[I + o_f2] = SUM;
                CONT[I + o_cont] = SUM + Y0[I + o_y0];
            }
            this._sol.Run(N, LDE1, E1, offset_e1, ref CONT, offset_cont, IP1, offset_ip1);
            goto LABEL77;
            // C
        LABEL15:;
            // C ------  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (I = 1; I <= M1; I++)
            {
                F2[I + o_f2] = HEE1 * Z1[I + o_z1] + HEE2 * Z2[I + o_z2] + HEE3 * Z3[I + o_z3];
                CONT[I + o_cont] = F2[I + o_f2] + Y0[I + o_y0];
            }
            for (I = M1 + 1; I <= N; I++)
            {
                F1[I + o_f1] = HEE1 * Z1[I + o_z1] + HEE2 * Z2[I + o_z2] + HEE3 * Z3[I + o_z3];
            }
            for (I = 1; I <= NM1; I++)
            {
                SUM = 0.0E0;
                for (J = 1; J <= NM1; J++)
                {
                    SUM += FMAS[I+J * LDMAS + o_fmas] * F1[J + M1 + o_f1];
                }
                IM1 = I + M1;
                F2[IM1 + o_f2] = SUM;
                CONT[IM1 + o_cont] = SUM + Y0[IM1 + o_y0];
            }
            goto LABEL48;
            // C
        LABEL6:;
            // C ------  B IS A FULL MATRIX, JACOBIAN A BANDED MATRIX
            // C ------  THIS OPTION IS NOT PROVIDED
            return;
            // C
        LABEL7:;
            // C ------  B=IDENTITY, JACOBIAN A FULL MATRIX, HESSENBERG-OPTION
            for (I = 1; I <= N; I++)
            {
                F2[I + o_f2] = HEE1 * Z1[I + o_z1] + HEE2 * Z2[I + o_z2] + HEE3 * Z3[I + o_z3];
                CONT[I + o_cont] = F2[I + o_f2] + Y0[I + o_y0];
            }
            for (MM = N - 2; MM >= 1; MM +=  - 1)
            {
                MP = N - MM;
                I = IPHES[MP + o_iphes];
                if (I == MP) goto LABEL310;
                ZSAFE = CONT[MP + o_cont];
                CONT[MP + o_cont] = CONT[I + o_cont];
                CONT[I + o_cont] = ZSAFE;
            LABEL310:;
                FJAC_2 = (MP - 1) * LDJAC + o_fjac;
                for (I = MP + 1; I <= N; I++)
                {
                    CONT[I + o_cont] +=  - FJAC[I + FJAC_2] * CONT[MP + o_cont];
                }
            }
            this._solh.Run(N, LDE1, E1, offset_e1, 1, ref CONT, offset_cont, IP1, offset_ip1);
            for (MM = 1; MM <= N - 2; MM++)
            {
                MP = N - MM;
                FJAC_3 = (MP - 1) * LDJAC + o_fjac;
                for (I = MP + 1; I <= N; I++)
                {
                    CONT[I + o_cont] += FJAC[I + FJAC_3] * CONT[MP + o_cont];
                }
                I = IPHES[MP + o_iphes];
                if (I == MP) goto LABEL440;
                ZSAFE = CONT[MP + o_cont];
                CONT[MP + o_cont] = CONT[I + o_cont];
                CONT[I + o_cont] = ZSAFE;
            LABEL440:;
            }
            // C
            // C --------------------------------------
            // C
        LABEL77:;
            ERR = 0.0E0;
            for (I = 1; I <= N; I++)
            {
                ERR += Math.Pow(CONT[I + o_cont] / SCAL[I + o_scal],2);
            }
            ERR = Math.Max(Math.Sqrt(ERR / N), 1.0E-10);
            // C
            if (ERR < 1.0E0) return;
            if (FIRST || REJECT)
            {
                for (I = 1; I <= N; I++)
                {
                    CONT[I + o_cont] += Y[I + o_y];
                }
                FCN.Run(N, X, CONT, offset_cont, ref F1, offset_f1, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
                NFCN += 1;
                for (I = 1; I <= N; I++)
                {
                    CONT[I + o_cont] = F1[I + o_f1] + F2[I + o_f2];
                }
                switch (IJOB)
                {
                    case 1: goto LABEL31;
                    case 2: goto LABEL32;
                    case 3: goto LABEL31;
                    case 4: goto LABEL32;
                    case 5: goto LABEL31;
                    case 6: goto LABEL32;
                    case 7: goto LABEL33;
                    case 8: goto LABEL55;
                    case 9: goto LABEL55;
                    case 10: goto LABEL55;
                    case 11: goto LABEL41;
                    case 12: goto LABEL42;
                    case 13: goto LABEL41;
                    case 14: goto LABEL42;
                    case 15: goto LABEL41;
                }
                // C ------ FULL MATRIX OPTION
            LABEL31:;
                this._sol.Run(N, LDE1, E1, offset_e1, ref CONT, offset_cont, IP1, offset_ip1);
                goto LABEL88;
                // C ------ FULL MATRIX OPTION, SECOND ORDER
            LABEL41:;
                for (J = 1; J <= M2; J++)
                {
                    SUM1 = 0.0E0;
                    for (K = MM - 1; K >= 0; K +=  - 1)
                    {
                        SUM1 = (CONT[J + K * M2 + o_cont] + SUM1) / FAC1;
                        FJAC_4 = (J + K * M2) * LDJAC + o_fjac;
                        for (I = 1; I <= NM1; I++)
                        {
                            IM1 = I + M1;
                            CONT[IM1 + o_cont] += FJAC[I + FJAC_4] * SUM1;
                        }
                    }
                }
                this._sol.Run(NM1, LDE1, E1, offset_e1, ref CONT, M1 + 1 + o_cont, IP1, offset_ip1);
                for (I = M1; I >= 1; I +=  - 1)
                {
                    CONT[I + o_cont] = (CONT[I + o_cont] + CONT[M2 + I + o_cont]) / FAC1;
                }
                goto LABEL88;
                // C ------ BANDED MATRIX OPTION
            LABEL32:;
                this._solb.Run(N, LDE1, E1, offset_e1, MLE.v, MUE.v, ref CONT, offset_cont
                               , IP1, offset_ip1);
                goto LABEL88;
                // C ------ BANDED MATRIX OPTION, SECOND ORDER
            LABEL42:;
                for (J = 1; J <= M2; J++)
                {
                    SUM1 = 0.0E0;
                    for (K = MM - 1; K >= 0; K +=  - 1)
                    {
                        SUM1 = (CONT[J + K * M2 + o_cont] + SUM1) / FAC1;
                        FJAC_5 = (J + K * M2) * LDJAC + o_fjac;
                        for (I = Math.Max(1, J - MUJAC); I <= Math.Min(NM1, J + MLJAC); I++)
                        {
                            IM1 = I + M1;
                            CONT[IM1 + o_cont] += FJAC[I + MUJAC + 1 - J + FJAC_5] * SUM1;
                        }
                    }
                }
                this._solb.Run(NM1, LDE1, E1, offset_e1, MLE.v, MUE.v, ref CONT, M1 + 1 + o_cont
                               , IP1, offset_ip1);
                for (I = M1; I >= 1; I +=  - 1)
                {
                    CONT[I + o_cont] = (CONT[I + o_cont] + CONT[M2 + I + o_cont]) / FAC1;
                }
                goto LABEL88;
                // C ------ HESSENBERG MATRIX OPTION
            LABEL33:;
                for (MM = N - 2; MM >= 1; MM +=  - 1)
                {
                    MP = N - MM;
                    I = IPHES[MP + o_iphes];
                    if (I == MP) goto LABEL510;
                    ZSAFE = CONT[MP + o_cont];
                    CONT[MP + o_cont] = CONT[I + o_cont];
                    CONT[I + o_cont] = ZSAFE;
                LABEL510:;
                    FJAC_6 = (MP - 1) * LDJAC + o_fjac;
                    for (I = MP + 1; I <= N; I++)
                    {
                        CONT[I + o_cont] +=  - FJAC[I + FJAC_6] * CONT[MP + o_cont];
                    }
                }
                this._solh.Run(N, LDE1, E1, offset_e1, 1, ref CONT, offset_cont, IP1, offset_ip1);
                for (MM = 1; MM <= N - 2; MM++)
                {
                    MP = N - MM;
                    FJAC_7 = (MP - 1) * LDJAC + o_fjac;
                    for (I = MP + 1; I <= N; I++)
                    {
                        CONT[I + o_cont] += FJAC[I + FJAC_7] * CONT[MP + o_cont];
                    }
                    I = IPHES[MP + o_iphes];
                    if (I == MP) goto LABEL640;
                    ZSAFE = CONT[MP + o_cont];
                    CONT[MP + o_cont] = CONT[I + o_cont];
                    CONT[I + o_cont] = ZSAFE;
                LABEL640:;
                }
                // C -----------------------------------
            LABEL88:;
                ERR = 0.0E0;
                for (I = 1; I <= N; I++)
                {
                    ERR += Math.Pow(CONT[I + o_cont] / SCAL[I + o_scal],2);
                }
                ERR = Math.Max(Math.Sqrt(ERR / N), 1.0E-10);
            }
            return;
            // C -----------------------------------------------------------
        LABEL55:;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: ESTRAV
    
    // C
    // C     END OF SUBROUTINE ESTRAD
    // C
    // C ***********************************************************
    // C
    public class ESTRAV
    {
    
        #region Dependencies
        
        SOL _sol; SOLB _solb; SOLH _solh; 
        #endregion
        #region Common variables
        
        #region Common Block: LINAL Declaration
        
        CommonBlock _linal;
        Oint MLE; Oint MUE; Oint MBJAC; Oint MBB; Oint MDIAG; Oint MDIFF; Oint MBDIAG; 
        #endregion
        #endregion
        public ESTRAV(SOL sol, SOLB solb, SOLH solh, CommonBlock LINAL)
        {
    
            #region Set Dependencies
            
            this._sol = sol; this._solb = solb; this._solh = solh; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
    
        public ESTRAV()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock LINAL = new CommonBlock(0, 7, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            SOL sol = new SOL();
            SOLB solb = new SOLB();
            SOLH solh = new SOLH();
            #endregion
            #region Set Dependencies
            
            this._sol = sol; this._solb = solb; this._solh = solh; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
        public void Run(int N, double[] FJAC, int offset_fjac, int LDJAC, int MLJAC, int MUJAC, double[] FMAS, int offset_fmas
                         , int LDMAS, int MLMAS, int MUMAS, double H, double[] DD, int offset_dd, IFVPOL FCN
                         , ref int NFCN, double[] Y0, int offset_y0, double[] Y, int offset_y, int IJOB, double X, int M1
                         , int M2, int NM1, int NS, int NNS, double[] E1, int offset_e1, int LDE1
                         , double[] ZZ, int offset_zz, ref double[] CONT, int offset_cont, ref double[] FF, int offset_ff, int[] IP1, int offset_ip1, int[] IPHES, int offset_iphes, double[] SCAL, int offset_scal
                         , ref double ERR, bool FIRST, bool REJECT, double FAC1, double[] RPAR, int offset_rpar, int[] IPAR, int offset_ipar)
        {
            #region Implicit Variables
            
            double SUM = 0; int K = 0; int I = 0; int MM = 0; double SUM1 = 0; int J = 0; int IM1 = 0; int FJAC_0 = 0; 
            int FJAC_1 = 0;int MP = 0; double ZSAFE = 0; int FJAC_2 = 0; int FJAC_3 = 0; int FJAC_4 = 0; int FJAC_5 = 0; 
            int FJAC_6 = 0;int FJAC_7 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_fjac = -1 - LDJAC + offset_fjac;  int o_fmas = -1 - LDMAS + offset_fmas;  int o_dd = -1 + offset_dd; 
             int o_y0 = -1 + offset_y0; int o_y = -1 + offset_y;  int o_e1 = -1 - LDE1 + offset_e1;  int o_zz = -1 + offset_zz; 
             int o_cont = -1 + offset_cont; int o_ff = -1 + offset_ff;  int o_ip1 = -1 + offset_ip1; 
             int o_iphes = -1 + offset_iphes; int o_scal = -1 + offset_scal;  int o_rpar = -1 + offset_rpar; 
             int o_ipar = -1 + offset_ipar;
            #endregion
            #region Body
            
            switch (IJOB)
            {
                case 1: goto LABEL1;
                case 2: goto LABEL2;
                case 3: goto LABEL3;
                case 4: goto LABEL4;
                case 5: goto LABEL5;
                case 6: goto LABEL6;
                case 7: goto LABEL7;
                case 8: goto LABEL55;
                case 9: goto LABEL55;
                case 10: goto LABEL55;
                case 11: goto LABEL11;
                case 12: goto LABEL12;
                case 13: goto LABEL13;
                case 14: goto LABEL14;
                case 15: goto LABEL15;
            }
            // C
        LABEL1:;
            // C ------  B=IDENTITY, JACOBIAN A FULL MATRIX
            for (I = 1; I <= N; I++)
            {
                SUM = 0.0E0;
                for (K = 1; K <= NS; K++)
                {
                    SUM += DD[K + o_dd] * ZZ[I + (K - 1) * N + o_zz];
                }
                FF[I + N + o_ff] = SUM / H;
                CONT[I + o_cont] = FF[I + N + o_ff] + Y0[I + o_y0];
            }
            this._sol.Run(N, LDE1, E1, offset_e1, ref CONT, offset_cont, IP1, offset_ip1);
            goto LABEL77;
            // C
        LABEL11:;
            // C ------  B=IDENTITY, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (I = 1; I <= N; I++)
            {
                SUM = 0.0E0;
                for (K = 1; K <= NS; K++)
                {
                    SUM += DD[K + o_dd] * ZZ[I + (K - 1) * N + o_zz];
                }
                FF[I + N + o_ff] = SUM / H;
                CONT[I + o_cont] = FF[I + N + o_ff] + Y0[I + o_y0];
            }
        LABEL48:  MM = M1 / M2;
            for (J = 1; J <= M2; J++)
            {
                SUM1 = 0.0E0;
                for (K = MM - 1; K >= 0; K +=  - 1)
                {
                    SUM1 = (CONT[J + K * M2 + o_cont] + SUM1) / FAC1;
                    FJAC_0 = (J + K * M2) * LDJAC + o_fjac;
                    for (I = 1; I <= NM1; I++)
                    {
                        IM1 = I + M1;
                        CONT[IM1 + o_cont] += FJAC[I + FJAC_0] * SUM1;
                    }
                }
            }
            this._sol.Run(NM1, LDE1, E1, offset_e1, ref CONT, M1 + 1 + o_cont, IP1, offset_ip1);
            for (I = M1; I >= 1; I +=  - 1)
            {
                CONT[I + o_cont] = (CONT[I + o_cont] + CONT[M2 + I + o_cont]) / FAC1;
            }
            goto LABEL77;
            // C
        LABEL2:;
            // C ------  B=IDENTITY, JACOBIAN A BANDED MATRIX
            for (I = 1; I <= N; I++)
            {
                SUM = 0.0E0;
                for (K = 1; K <= NS; K++)
                {
                    SUM += DD[K + o_dd] * ZZ[I + (K - 1) * N + o_zz];
                }
                FF[I + N + o_ff] = SUM / H;
                CONT[I + o_cont] = FF[I + N + o_ff] + Y0[I + o_y0];
            }
            this._solb.Run(N, LDE1, E1, offset_e1, MLE.v, MUE.v, ref CONT, offset_cont
                           , IP1, offset_ip1);
            goto LABEL77;
            // C
        LABEL12:;
            // C ------  B=IDENTITY, JACOBIAN A BANDED MATRIX, SECOND ORDER
            for (I = 1; I <= N; I++)
            {
                SUM = 0.0E0;
                for (K = 1; K <= NS; K++)
                {
                    SUM += DD[K + o_dd] * ZZ[I + (K - 1) * N + o_zz];
                }
                FF[I + N + o_ff] = SUM / H;
                CONT[I + o_cont] = FF[I + N + o_ff] + Y0[I + o_y0];
            }
        LABEL45:  MM = M1 / M2;
            for (J = 1; J <= M2; J++)
            {
                SUM1 = 0.0E0;
                for (K = MM - 1; K >= 0; K +=  - 1)
                {
                    SUM1 = (CONT[J + K * M2 + o_cont] + SUM1) / FAC1;
                    FJAC_1 = (J + K * M2) * LDJAC + o_fjac;
                    for (I = Math.Max(1, J - MUJAC); I <= Math.Min(NM1, J + MLJAC); I++)
                    {
                        IM1 = I + M1;
                        CONT[IM1 + o_cont] += FJAC[I + MUJAC + 1 - J + FJAC_1] * SUM1;
                    }
                }
            }
            this._solb.Run(NM1, LDE1, E1, offset_e1, MLE.v, MUE.v, ref CONT, M1 + 1 + o_cont
                           , IP1, offset_ip1);
            for (I = M1; I >= 1; I +=  - 1)
            {
                CONT[I + o_cont] = (CONT[I + o_cont] + CONT[M2 + I + o_cont]) / FAC1;
            }
            goto LABEL77;
            // C
        LABEL3:;
            // C ------  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX
            for (I = 1; I <= N; I++)
            {
                SUM = 0.0E0;
                for (K = 1; K <= NS; K++)
                {
                    SUM += DD[K + o_dd] * ZZ[I + (K - 1) * N + o_zz];
                }
                FF[I + o_ff] = SUM / H;
            }
            for (I = 1; I <= N; I++)
            {
                SUM = 0.0E0;
                for (J = Math.Max(1, I - MLMAS); J <= Math.Min(N, I + MUMAS); J++)
                {
                    SUM += FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas] * FF[J + o_ff];
                }
                FF[I + N + o_ff] = SUM;
                CONT[I + o_cont] = SUM + Y0[I + o_y0];
            }
            this._sol.Run(N, LDE1, E1, offset_e1, ref CONT, offset_cont, IP1, offset_ip1);
            goto LABEL77;
            // C
        LABEL13:;
            // C ------  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (I = 1; I <= M1; I++)
            {
                SUM = 0.0E0;
                for (K = 1; K <= NS; K++)
                {
                    SUM += DD[K + o_dd] * ZZ[I + (K - 1) * N + o_zz];
                }
                FF[I + N + o_ff] = SUM / H;
                CONT[I + o_cont] = FF[I + N + o_ff] + Y0[I + o_y0];
            }
            for (I = M1 + 1; I <= N; I++)
            {
                SUM = 0.0E0;
                for (K = 1; K <= NS; K++)
                {
                    SUM += DD[K + o_dd] * ZZ[I + (K - 1) * N + o_zz];
                }
                FF[I + o_ff] = SUM / H;
            }
            for (I = 1; I <= NM1; I++)
            {
                SUM = 0.0E0;
                for (J = Math.Max(1, I - MLMAS); J <= Math.Min(NM1, I + MUMAS); J++)
                {
                    SUM += FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas] * FF[J + M1 + o_ff];
                }
                IM1 = I + M1;
                FF[IM1 + N + o_ff] = SUM;
                CONT[IM1 + o_cont] = SUM + Y0[IM1 + o_y0];
            }
            goto LABEL48;
            // C
        LABEL4:;
            // C ------  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX
            for (I = 1; I <= N; I++)
            {
                SUM = 0.0E0;
                for (K = 1; K <= NS; K++)
                {
                    SUM += DD[K + o_dd] * ZZ[I + (K - 1) * N + o_zz];
                }
                FF[I + o_ff] = SUM / H;
            }
            for (I = 1; I <= N; I++)
            {
                SUM = 0.0E0;
                for (J = Math.Max(1, I - MLMAS); J <= Math.Min(N, I + MUMAS); J++)
                {
                    SUM += FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas] * FF[J + o_ff];
                }
                FF[I + N + o_ff] = SUM;
                CONT[I + o_cont] = SUM + Y0[I + o_y0];
            }
            this._solb.Run(N, LDE1, E1, offset_e1, MLE.v, MUE.v, ref CONT, offset_cont
                           , IP1, offset_ip1);
            goto LABEL77;
            // C
        LABEL14:;
            // C ------  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX, SECOND ORDER
            for (I = 1; I <= M1; I++)
            {
                SUM = 0.0E0;
                for (K = 1; K <= NS; K++)
                {
                    SUM += DD[K + o_dd] * ZZ[I + (K - 1) * N + o_zz];
                }
                FF[I + N + o_ff] = SUM / H;
                CONT[I + o_cont] = FF[I + N + o_ff] + Y0[I + o_y0];
            }
            for (I = M1 + 1; I <= N; I++)
            {
                SUM = 0.0E0;
                for (K = 1; K <= NS; K++)
                {
                    SUM += DD[K + o_dd] * ZZ[I + (K - 1) * N + o_zz];
                }
                FF[I + o_ff] = SUM / H;
            }
            for (I = 1; I <= NM1; I++)
            {
                SUM = 0.0E0;
                for (J = Math.Max(1, I - MLMAS); J <= Math.Min(NM1, I + MUMAS); J++)
                {
                    SUM += FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas] * FF[J + M1 + o_ff];
                }
                IM1 = I + M1;
                FF[IM1 + N + o_ff] = SUM;
                CONT[IM1 + o_cont] = SUM + Y0[IM1 + o_y0];
            }
            goto LABEL45;
            // C
        LABEL5:;
            // C ------  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX
            for (I = 1; I <= N; I++)
            {
                SUM = 0.0E0;
                for (K = 1; K <= NS; K++)
                {
                    SUM += DD[K + o_dd] * ZZ[I + (K - 1) * N + o_zz];
                }
                FF[I + o_ff] = SUM / H;
            }
            for (I = 1; I <= N; I++)
            {
                SUM = 0.0E0;
                for (J = 1; J <= N; J++)
                {
                    SUM += FMAS[I+J * LDMAS + o_fmas] * FF[J + o_ff];
                }
                FF[I + N + o_ff] = SUM;
                CONT[I + o_cont] = SUM + Y0[I + o_y0];
            }
            this._sol.Run(N, LDE1, E1, offset_e1, ref CONT, offset_cont, IP1, offset_ip1);
            goto LABEL77;
            // C
        LABEL15:;
            // C ------  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER
            for (I = 1; I <= M1; I++)
            {
                SUM = 0.0E0;
                for (K = 1; K <= NS; K++)
                {
                    SUM += DD[K + o_dd] * ZZ[I + (K - 1) * N + o_zz];
                }
                FF[I + N + o_ff] = SUM / H;
                CONT[I + o_cont] = FF[I + N + o_ff] + Y0[I + o_y0];
            }
            for (I = M1 + 1; I <= N; I++)
            {
                SUM = 0.0E0;
                for (K = 1; K <= NS; K++)
                {
                    SUM += DD[K + o_dd] * ZZ[I + (K - 1) * N + o_zz];
                }
                FF[I + o_ff] = SUM / H;
            }
            for (I = 1; I <= NM1; I++)
            {
                SUM = 0.0E0;
                for (J = 1; J <= NM1; J++)
                {
                    SUM += FMAS[I+J * LDMAS + o_fmas] * FF[J + M1 + o_ff];
                }
                IM1 = I + M1;
                FF[IM1 + N + o_ff] = SUM;
                CONT[IM1 + o_cont] = SUM + Y0[IM1 + o_y0];
            }
            goto LABEL48;
            // C
        LABEL6:;
            // C ------  B IS A FULL MATRIX, JACOBIAN A BANDED MATRIX
            // C ------  THIS OPTION IS NOT PROVIDED
            return;
            // C
        LABEL7:;
            // C ------  B=IDENTITY, JACOBIAN A FULL MATRIX, HESSENBERG-OPTION
            for (I = 1; I <= N; I++)
            {
                SUM = 0.0E0;
                for (K = 1; K <= NS; K++)
                {
                    SUM += DD[K + o_dd] * ZZ[I + (K - 1) * N + o_zz];
                }
                FF[I + N + o_ff] = SUM / H;
                CONT[I + o_cont] = FF[I + N + o_ff] + Y0[I + o_y0];
            }
            for (MM = N - 2; MM >= 1; MM +=  - 1)
            {
                MP = N - MM;
                I = IPHES[MP + o_iphes];
                if (I == MP) goto LABEL310;
                ZSAFE = CONT[MP + o_cont];
                CONT[MP + o_cont] = CONT[I + o_cont];
                CONT[I + o_cont] = ZSAFE;
            LABEL310:;
                FJAC_2 = (MP - 1) * LDJAC + o_fjac;
                for (I = MP + 1; I <= N; I++)
                {
                    CONT[I + o_cont] +=  - FJAC[I + FJAC_2] * CONT[MP + o_cont];
                }
            }
            this._solh.Run(N, LDE1, E1, offset_e1, 1, ref CONT, offset_cont, IP1, offset_ip1);
            for (MM = 1; MM <= N - 2; MM++)
            {
                MP = N - MM;
                FJAC_3 = (MP - 1) * LDJAC + o_fjac;
                for (I = MP + 1; I <= N; I++)
                {
                    CONT[I + o_cont] += FJAC[I + FJAC_3] * CONT[MP + o_cont];
                }
                I = IPHES[MP + o_iphes];
                if (I == MP) goto LABEL440;
                ZSAFE = CONT[MP + o_cont];
                CONT[MP + o_cont] = CONT[I + o_cont];
                CONT[I + o_cont] = ZSAFE;
            LABEL440:;
            }
            // C
            // C --------------------------------------
            // C
        LABEL77:;
            ERR = 0.0E0;
            for (I = 1; I <= N; I++)
            {
                ERR += Math.Pow(CONT[I + o_cont] / SCAL[I + o_scal],2);
            }
            ERR = Math.Max(Math.Sqrt(ERR / N), 1.0E-10);
            // C
            if (ERR < 1.0E0) return;
            if (FIRST || REJECT)
            {
                for (I = 1; I <= N; I++)
                {
                    CONT[I + o_cont] += Y[I + o_y];
                }
                FCN.Run(N, X, CONT, offset_cont, ref FF, offset_ff, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
                NFCN += 1;
                for (I = 1; I <= N; I++)
                {
                    CONT[I + o_cont] = FF[I + o_ff] + FF[I + N + o_ff];
                }
                switch (IJOB)
                {
                    case 1: goto LABEL31;
                    case 2: goto LABEL32;
                    case 3: goto LABEL31;
                    case 4: goto LABEL32;
                    case 5: goto LABEL31;
                    case 6: goto LABEL32;
                    case 7: goto LABEL33;
                    case 8: goto LABEL55;
                    case 9: goto LABEL55;
                    case 10: goto LABEL55;
                    case 11: goto LABEL41;
                    case 12: goto LABEL42;
                    case 13: goto LABEL41;
                    case 14: goto LABEL42;
                    case 15: goto LABEL41;
                }
                // C ------ FULL MATRIX OPTION
            LABEL31:;
                this._sol.Run(N, LDE1, E1, offset_e1, ref CONT, offset_cont, IP1, offset_ip1);
                goto LABEL88;
                // C ------ FULL MATRIX OPTION, SECOND ORDER
            LABEL41:;
                for (J = 1; J <= M2; J++)
                {
                    SUM1 = 0.0E0;
                    for (K = MM - 1; K >= 0; K +=  - 1)
                    {
                        SUM1 = (CONT[J + K * M2 + o_cont] + SUM1) / FAC1;
                        FJAC_4 = (J + K * M2) * LDJAC + o_fjac;
                        for (I = 1; I <= NM1; I++)
                        {
                            IM1 = I + M1;
                            CONT[IM1 + o_cont] += FJAC[I + FJAC_4] * SUM1;
                        }
                    }
                }
                this._sol.Run(NM1, LDE1, E1, offset_e1, ref CONT, M1 + 1 + o_cont, IP1, offset_ip1);
                for (I = M1; I >= 1; I +=  - 1)
                {
                    CONT[I + o_cont] = (CONT[I + o_cont] + CONT[M2 + I + o_cont]) / FAC1;
                }
                goto LABEL88;
                // C ------ BANDED MATRIX OPTION
            LABEL32:;
                this._solb.Run(N, LDE1, E1, offset_e1, MLE.v, MUE.v, ref CONT, offset_cont
                               , IP1, offset_ip1);
                goto LABEL88;
                // C ------ BANDED MATRIX OPTION, SECOND ORDER
            LABEL42:;
                for (J = 1; J <= M2; J++)
                {
                    SUM1 = 0.0E0;
                    for (K = MM - 1; K >= 0; K +=  - 1)
                    {
                        SUM1 = (CONT[J + K * M2 + o_cont] + SUM1) / FAC1;
                        FJAC_5 = (J + K * M2) * LDJAC + o_fjac;
                        for (I = Math.Max(1, J - MUJAC); I <= Math.Min(NM1, J + MLJAC); I++)
                        {
                            IM1 = I + M1;
                            CONT[IM1 + o_cont] += FJAC[I + MUJAC + 1 - J + FJAC_5] * SUM1;
                        }
                    }
                }
                this._solb.Run(NM1, LDE1, E1, offset_e1, MLE.v, MUE.v, ref CONT, M1 + 1 + o_cont
                               , IP1, offset_ip1);
                for (I = M1; I >= 1; I +=  - 1)
                {
                    CONT[I + o_cont] = (CONT[I + o_cont] + CONT[M2 + I + o_cont]) / FAC1;
                }
                goto LABEL88;
                // C ------ HESSENBERG MATRIX OPTION
            LABEL33:;
                for (MM = N - 2; MM >= 1; MM +=  - 1)
                {
                    MP = N - MM;
                    I = IPHES[MP + o_iphes];
                    if (I == MP) goto LABEL510;
                    ZSAFE = CONT[MP + o_cont];
                    CONT[MP + o_cont] = CONT[I + o_cont];
                    CONT[I + o_cont] = ZSAFE;
                LABEL510:;
                    FJAC_6 = (MP - 1) * LDJAC + o_fjac;
                    for (I = MP + 1; I <= N; I++)
                    {
                        CONT[I + o_cont] +=  - FJAC[I + FJAC_6] * CONT[MP + o_cont];
                    }
                }
                this._solh.Run(N, LDE1, E1, offset_e1, 1, ref CONT, offset_cont, IP1, offset_ip1);
                for (MM = 1; MM <= N - 2; MM++)
                {
                    MP = N - MM;
                    FJAC_7 = (MP - 1) * LDJAC + o_fjac;
                    for (I = MP + 1; I <= N; I++)
                    {
                        CONT[I + o_cont] += FJAC[I + FJAC_7] * CONT[MP + o_cont];
                    }
                    I = IPHES[MP + o_iphes];
                    if (I == MP) goto LABEL640;
                    ZSAFE = CONT[MP + o_cont];
                    CONT[MP + o_cont] = CONT[I + o_cont];
                    CONT[I + o_cont] = ZSAFE;
                LABEL640:;
                }
                // C -----------------------------------
            LABEL88:;
                ERR = 0.0E0;
                for (I = 1; I <= N; I++)
                {
                    ERR += Math.Pow(CONT[I + o_cont] / SCAL[I + o_scal],2);
                }
                ERR = Math.Max(Math.Sqrt(ERR / N), 1.0E-10);
            }
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL55:;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: SLVROD
    
    // C
    // C     END OF SUBROUTINE ESTRAV
    // C
    // C ***********************************************************
    // C
    public class SLVROD
    {
    
        #region Dependencies
        
        SOL _sol; SOLB _solb; 
        #endregion
        #region Common variables
        
        #region Common Block: LINAL Declaration
        
        CommonBlock _linal;
        Oint MLE; Oint MUE; Oint MBJAC; Oint MBB; Oint MDIAG; Oint MDIFF; Oint MBDIAG; 
        #endregion
        #endregion
        public SLVROD(SOL sol, SOLB solb, CommonBlock LINAL)
        {
    
            #region Set Dependencies
            
            this._sol = sol; this._solb = solb; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
    
        public SLVROD()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock LINAL = new CommonBlock(0, 7, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            SOL sol = new SOL();
            SOLB solb = new SOLB();
            #endregion
            #region Set Dependencies
            
            this._sol = sol; this._solb = solb; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
        public void Run(int N, double[] FJAC, int offset_fjac, int LDJAC, int MLJAC, int MUJAC, double[] FMAS, int offset_fmas
                         , int LDMAS, int MLMAS, int MUMAS, int M1, int M2, int NM1
                         , double FAC1, double[] E, int offset_e, int LDE, int[] IP, int offset_ip, double[] DY, int offset_dy, ref double[] AK, int offset_ak
                         , double[] FX, int offset_fx, double[] YNEW, int offset_ynew, double HD, int IJOB, bool STAGE1)
        {
            #region Implicit Variables
            
            int I = 0; int MM = 0; double SUM = 0; int JKM = 0; int J = 0; int K = 0; int IM1 = 0; int FJAC_JKM = 0; 
            #endregion
            #region Array Index Correction
            
             int o_fjac = -1 - LDJAC + offset_fjac;  int o_fmas = -1 - LDMAS + offset_fmas;  int o_e = -1 - LDE + offset_e; 
             int o_ip = -1 + offset_ip; int o_dy = -1 + offset_dy;  int o_ak = -1 + offset_ak;  int o_fx = -1 + offset_fx; 
             int o_ynew = -1 + offset_ynew;
            #endregion
            // C
            #region Body
            
            if (HD == 0.0E0)
            {
                for (I = 1; I <= N; I++)
                {
                    AK[I + o_ak] = DY[I + o_dy];
                }
            }
            else
            {
                for (I = 1; I <= N; I++)
                {
                    AK[I + o_ak] = DY[I + o_dy] + HD * FX[I + o_fx];
                }
            }
            // C
            switch (IJOB)
            {
                case 1: goto LABEL1;
                case 2: goto LABEL2;
                case 3: goto LABEL3;
                case 4: goto LABEL4;
                case 5: goto LABEL5;
                case 6: goto LABEL6;
                case 7: goto LABEL55;
                case 8: goto LABEL55;
                case 9: goto LABEL55;
                case 10: goto LABEL55;
                case 11: goto LABEL11;
                case 12: goto LABEL12;
                case 13: goto LABEL13;
                case 14: goto LABEL13;
                case 15: goto LABEL15;
            }
            // C
            // C -----------------------------------------------------------
            // C
        LABEL1:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX
            if (STAGE1)
            {
                for (I = 1; I <= N; I++)
                {
                    AK[I + o_ak] += YNEW[I + o_ynew];
                }
            }
            this._sol.Run(N, LDE, E, offset_e, ref AK, offset_ak, IP, offset_ip);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL11:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX, SECOND ORDER
            if (STAGE1)
            {
                for (I = 1; I <= N; I++)
                {
                    AK[I + o_ak] += YNEW[I + o_ynew];
                }
            }
        LABEL48:  MM = M1 / M2;
            for (J = 1; J <= M2; J++)
            {
                SUM = 0.0E0;
                for (K = MM - 1; K >= 0; K +=  - 1)
                {
                    JKM = J + K * M2;
                    SUM = (AK[JKM + o_ak] + SUM) / FAC1;
                    FJAC_JKM = JKM * LDJAC + o_fjac;
                    for (I = 1; I <= NM1; I++)
                    {
                        IM1 = I + M1;
                        AK[IM1 + o_ak] += FJAC[I + FJAC_JKM] * SUM;
                    }
                }
            }
            this._sol.Run(NM1, LDE, E, offset_e, ref AK, M1 + 1 + o_ak, IP, offset_ip);
            for (I = M1; I >= 1; I +=  - 1)
            {
                AK[I + o_ak] = (AK[I + o_ak] + AK[M2 + I + o_ak]) / FAC1;
            }
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL2:;
            // C ---  B=IDENTITY, JACOBIAN A BANDED MATRIX
            if (STAGE1)
            {
                for (I = 1; I <= N; I++)
                {
                    AK[I + o_ak] += YNEW[I + o_ynew];
                }
            }
            this._solb.Run(N, LDE, E, offset_e, MLE.v, MUE.v, ref AK, offset_ak
                           , IP, offset_ip);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL12:;
            // C ---  B=IDENTITY, JACOBIAN A BANDED MATRIX, SECOND ORDER
            if (STAGE1)
            {
                for (I = 1; I <= N; I++)
                {
                    AK[I + o_ak] += YNEW[I + o_ynew];
                }
            }
        LABEL45:  MM = M1 / M2;
            for (J = 1; J <= M2; J++)
            {
                SUM = 0.0E0;
                for (K = MM - 1; K >= 0; K +=  - 1)
                {
                    JKM = J + K * M2;
                    SUM = (AK[JKM + o_ak] + SUM) / FAC1;
                    FJAC_JKM = JKM * LDJAC + o_fjac;
                    for (I = Math.Max(1, J - MUJAC); I <= Math.Min(NM1, J + MLJAC); I++)
                    {
                        IM1 = I + M1;
                        AK[IM1 + o_ak] += FJAC[I + MUJAC + 1 - J + FJAC_JKM] * SUM;
                    }
                }
            }
            this._solb.Run(NM1, LDE, E, offset_e, MLE.v, MUE.v, ref AK, M1 + 1 + o_ak
                           , IP, offset_ip);
            for (I = M1; I >= 1; I +=  - 1)
            {
                AK[I + o_ak] = (AK[I + o_ak] + AK[M2 + I + o_ak]) / FAC1;
            }
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL3:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX
            if (STAGE1)
            {
                for (I = 1; I <= N; I++)
                {
                    SUM = 0.0E0;
                    for (J = Math.Max(1, I - MLMAS); J <= Math.Min(N, I + MUMAS); J++)
                    {
                        SUM += FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas] * YNEW[J + o_ynew];
                    }
                    AK[I + o_ak] += SUM;
                }
            }
            this._sol.Run(N, LDE, E, offset_e, ref AK, offset_ak, IP, offset_ip);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL13:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER
            if (STAGE1)
            {
                for (I = 1; I <= M1; I++)
                {
                    AK[I + o_ak] += YNEW[I + o_ynew];
                }
                for (I = 1; I <= NM1; I++)
                {
                    SUM = 0.0E0;
                    for (J = Math.Max(1, I - MLMAS); J <= Math.Min(NM1, I + MUMAS); J++)
                    {
                        SUM += FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas] * YNEW[J + M1 + o_ynew];
                    }
                    IM1 = I + M1;
                    AK[IM1 + o_ak] += SUM;
                }
            }
            if (IJOB == 14) goto LABEL45;
            goto LABEL48;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL4:;
            // C ---  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX
            if (STAGE1)
            {
                for (I = 1; I <= N; I++)
                {
                    SUM = 0.0E0;
                    for (J = Math.Max(1, I - MLMAS); J <= Math.Min(N, I + MUMAS); J++)
                    {
                        SUM += FMAS[I - J + MBDIAG.v+J * LDMAS + o_fmas] * YNEW[J + o_ynew];
                    }
                    AK[I + o_ak] += SUM;
                }
            }
            this._solb.Run(N, LDE, E, offset_e, MLE.v, MUE.v, ref AK, offset_ak
                           , IP, offset_ip);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL5:;
            // C ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX
            if (STAGE1)
            {
                for (I = 1; I <= N; I++)
                {
                    SUM = 0.0E0;
                    for (J = 1; J <= N; J++)
                    {
                        SUM += FMAS[I+J * LDMAS + o_fmas] * YNEW[J + o_ynew];
                    }
                    AK[I + o_ak] += SUM;
                }
            }
            this._sol.Run(N, LDE, E, offset_e, ref AK, offset_ak, IP, offset_ip);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL15:;
            // C ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER
            if (STAGE1)
            {
                for (I = 1; I <= M1; I++)
                {
                    AK[I + o_ak] += YNEW[I + o_ynew];
                }
                for (I = 1; I <= NM1; I++)
                {
                    SUM = 0.0E0;
                    for (J = 1; J <= NM1; J++)
                    {
                        SUM += FMAS[I+J * LDMAS + o_fmas] * YNEW[J + M1 + o_ynew];
                    }
                    IM1 = I + M1;
                    AK[IM1 + o_ak] += SUM;
                }
            }
            goto LABEL48;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL6:;
            // C ---  B IS A FULL MATRIX, JACOBIAN A BANDED MATRIX
            // C ---  THIS OPTION IS NOT PROVIDED
            if (STAGE1)
            {
                for (I = 1; I <= N; I++)
                {
                    SUM = 0.0E0;
                    for (J = 1; J <= N; J++)
                    {
                        SUM += FMAS[I+J * LDMAS + o_fmas] * YNEW[J + o_ynew];
                    }
                    AK[I + o_ak] += SUM;
                }
                this._solb.Run(N, LDE, E, offset_e, MLE.v, MUE.v, ref AK, offset_ak
                               , IP, offset_ip);
            }
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL55:;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: SLVSEU
    
    // C
    // C     END OF SUBROUTINE SLVROD
    // C
    // C
    // C ***********************************************************
    // C
    public class SLVSEU
    {
    
        #region Dependencies
        
        SOL _sol; SOLB _solb; SOLH _solh; 
        #endregion
        #region Common variables
        
        #region Common Block: LINAL Declaration
        
        CommonBlock _linal;
        Oint MLE; Oint MUE; Oint MBJAC; Oint MBB; Oint MDIAG; Oint MDIFF; Oint MBDIAG; 
        #endregion
        #endregion
        public SLVSEU(SOL sol, SOLB solb, SOLH solh, CommonBlock LINAL)
        {
    
            #region Set Dependencies
            
            this._sol = sol; this._solb = solb; this._solh = solh; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
    
        public SLVSEU()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock LINAL = new CommonBlock(0, 7, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            SOL sol = new SOL();
            SOLB solb = new SOLB();
            SOLH solh = new SOLH();
            #endregion
            #region Set Dependencies
            
            this._sol = sol; this._solb = solb; this._solh = solh; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
        public void Run(int N, double[] FJAC, int offset_fjac, int LDJAC, int MLJAC, int MUJAC, double[] FMAS, int offset_fmas
                         , int LDMAS, int MLMAS, int MUMAS, int M1, int M2, int NM1
                         , double FAC1, double[] E, int offset_e, int LDE, int[] IP, int offset_ip, int[] IPHES, int offset_iphes, ref double[] DEL, int offset_del
                         , int IJOB)
        {
            #region Implicit Variables
            
            int MM = 0; double SUM = 0; int JKM = 0; int J = 0; int K = 0; int IM1 = 0; int I = 0; int FJAC_JKM = 0; int MP = 0; 
            int MMM = 0;int MP1 = 0; double ZSAFE = 0; int FJAC_MP1 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_fjac = -1 - LDJAC + offset_fjac;  int o_fmas = -1 - LDMAS + offset_fmas;  int o_e = -1 - LDE + offset_e; 
             int o_ip = -1 + offset_ip; int o_iphes = -1 + offset_iphes;  int o_del = -1 + offset_del; 
            #endregion
            // C
            #region Body
            
            switch (IJOB)
            {
                case 1: goto LABEL1;
                case 2: goto LABEL2;
                case 3: goto LABEL1;
                case 4: goto LABEL2;
                case 5: goto LABEL1;
                case 6: goto LABEL55;
                case 7: goto LABEL7;
                case 8: goto LABEL55;
                case 9: goto LABEL55;
                case 10: goto LABEL55;
                case 11: goto LABEL11;
                case 12: goto LABEL12;
                case 13: goto LABEL11;
                case 14: goto LABEL12;
                case 15: goto LABEL11;
            }
            // C
            // C -----------------------------------------------------------
            // C
        LABEL1:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX
            this._sol.Run(N, LDE, E, offset_e, ref DEL, offset_del, IP, offset_ip);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL11:;
            // C ---  B=IDENTITY, JACOBIAN A FULL MATRIX, SECOND ORDER
            MM = M1 / M2;
            for (J = 1; J <= M2; J++)
            {
                SUM = 0.0E0;
                for (K = MM - 1; K >= 0; K +=  - 1)
                {
                    JKM = J + K * M2;
                    SUM = (DEL[JKM + o_del] + SUM) / FAC1;
                    FJAC_JKM = JKM * LDJAC + o_fjac;
                    for (I = 1; I <= NM1; I++)
                    {
                        IM1 = I + M1;
                        DEL[IM1 + o_del] += FJAC[I + FJAC_JKM] * SUM;
                    }
                }
            }
            this._sol.Run(NM1, LDE, E, offset_e, ref DEL, M1 + 1 + o_del, IP, offset_ip);
            for (I = M1; I >= 1; I +=  - 1)
            {
                DEL[I + o_del] = (DEL[I + o_del] + DEL[M2 + I + o_del]) / FAC1;
            }
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL2:;
            // C ---  B=IDENTITY, JACOBIAN A BANDED MATRIX
            this._solb.Run(N, LDE, E, offset_e, MLE.v, MUE.v, ref DEL, offset_del
                           , IP, offset_ip);
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL12:;
            // C ---  B=IDENTITY, JACOBIAN A BANDED MATRIX, SECOND ORDER
            MM = M1 / M2;
            for (J = 1; J <= M2; J++)
            {
                SUM = 0.0E0;
                for (K = MM - 1; K >= 0; K +=  - 1)
                {
                    JKM = J + K * M2;
                    SUM = (DEL[JKM + o_del] + SUM) / FAC1;
                    FJAC_JKM = JKM * LDJAC + o_fjac;
                    for (I = Math.Max(1, J - MUJAC); I <= Math.Min(NM1, J + MLJAC); I++)
                    {
                        IM1 = I + M1;
                        DEL[IM1 + o_del] += FJAC[I + MUJAC + 1 - J + FJAC_JKM] * SUM;
                    }
                }
            }
            this._solb.Run(NM1, LDE, E, offset_e, MLE.v, MUE.v, ref DEL, M1 + 1 + o_del
                           , IP, offset_ip);
            for (I = M1; I >= 1; I +=  - 1)
            {
                DEL[I + o_del] = (DEL[I + o_del] + DEL[M2 + I + o_del]) / FAC1;
            }
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL7:;
            // C ---  HESSENBERG OPTION
            for (MMM = N - 2; MMM >= 1; MMM +=  - 1)
            {
                MP = N - MMM;
                MP1 = MP - 1;
                I = IPHES[MP + o_iphes];
                if (I == MP) goto LABEL110;
                ZSAFE = DEL[MP + o_del];
                DEL[MP + o_del] = DEL[I + o_del];
                DEL[I + o_del] = ZSAFE;
            LABEL110:;
                FJAC_MP1 = MP1 * LDJAC + o_fjac;
                for (I = MP + 1; I <= N; I++)
                {
                    DEL[I + o_del] +=  - FJAC[I + FJAC_MP1] * DEL[MP + o_del];
                }
            }
            this._solh.Run(N, LDE, E, offset_e, 1, ref DEL, offset_del, IP, offset_ip);
            for (MMM = 1; MMM <= N - 2; MMM++)
            {
                MP = N - MMM;
                MP1 = MP - 1;
                FJAC_MP1 = MP1 * LDJAC + o_fjac;
                for (I = MP + 1; I <= N; I++)
                {
                    DEL[I + o_del] += FJAC[I + FJAC_MP1] * DEL[MP + o_del];
                }
                I = IPHES[MP + o_iphes];
                if (I == MP) goto LABEL240;
                ZSAFE = DEL[MP + o_del];
                DEL[MP + o_del] = DEL[I + o_del];
                DEL[I + o_del] = ZSAFE;
            LABEL240:;
            }
            return;
            // C
            // C -----------------------------------------------------------
            // C
        LABEL55:;
            return;
            #endregion
        }
    }

    #endregion

    // C
    // C     END OF SUBROUTINE SLVSEU
    // C
}
