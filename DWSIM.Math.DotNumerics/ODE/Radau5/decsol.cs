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

    #region The Class: DEC
    
    public class DEC
    {
    
        public DEC()
        {
    
        }
    
        /// <param name="N">
        /// = ORDER OF MATRIX.
        ///</param>
        /// <param name="NDIM">
        /// = DECLARED DIMENSION OF ARRAY  A .
        ///</param>
        /// <param name="A">
        /// = MATRIX TO BE TRIANGULARIZED.
        ///</param>
        /// <param name="IER">
        /// = 0 IF MATRIX A IS NONSINGULAR, OR K IF FOUND TO BE
        /// SINGULAR AT STAGE K.
        ///</param>
        public void Run(int N, int NDIM, ref double[] A, int offset_a, ref int[] IP, int offset_ip, ref int IER)
        {
            #region Variables
            
            int NM1 = 0; int K = 0; int KP1 = 0; int M = 0; int I = 0; int J = 0; double T = 0; 
            #endregion
            #region Array Index Correction
            
             int o_a = -1 - NDIM + offset_a;  int o_ip = -1 + offset_ip; 
            #endregion
            #region Prolog
            
            // C VERSION REAL DOUBLE PRECISION
            // C-----------------------------------------------------------------------
            // C  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION.
            // C  INPUT..
            // C     N = ORDER OF MATRIX.
            // C     NDIM = DECLARED DIMENSION OF ARRAY  A .
            // C     A = MATRIX TO BE TRIANGULARIZED.
            // C  OUTPUT..
            // C     A(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U .
            // C     A(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L.
            // C     IP(K), K.LT.N = INDEX OF K-TH PIVOT ROW.
            // C     IP(N) = (-1)**(NUMBER OF INTERCHANGES) OR O .
            // C     IER = 0 IF MATRIX A IS NONSINGULAR, OR K IF FOUND TO BE
            // C           SINGULAR AT STAGE K.
            // C  USE  SOL  TO OBTAIN SOLUTION OF LINEAR SYSTEM.
            // C  DETERM(A) = IP(N)*A(1,1)*A(2,2)*...*A(N,N).
            // C  IF IP(N)=O, A IS SINGULAR, SOL WILL DIVIDE BY ZERO.
            // C
            // C  REFERENCE..
            // C     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER,
            // C     C.A.C.M. 15 (1972), P. 274.
            // C-----------------------------------------------------------------------
            #endregion
            #region Body
            
            IER = 0;
            IP[N + o_ip] = 1;
            if (N == 1) goto LABEL70;
            NM1 = N - 1;
            for (K = 1; K <= NM1; K++)
            {
                KP1 = K + 1;
                M = K;
                for (I = KP1; I <= N; I++)
                {
                    if (Math.Abs(A[I+K * NDIM + o_a]) > Math.Abs(A[M+K * NDIM + o_a])) M = I;
                }
                IP[K + o_ip] = M;
                T = A[M+K * NDIM + o_a];
                if (M == K) goto LABEL20;
                IP[N + o_ip] =  - IP[N + o_ip];
                A[M+K * NDIM + o_a] = A[K+K * NDIM + o_a];
                A[K+K * NDIM + o_a] = T;
            LABEL20:;
                if (T == 0.0E0) goto LABEL80;
                T = 1.0E0 / T;
                for (I = KP1; I <= N; I++)
                {
                    A[I+K * NDIM + o_a] =  - A[I+K * NDIM + o_a] * T;
                }
                for (J = KP1; J <= N; J++)
                {
                    T = A[M+J * NDIM + o_a];
                    A[M+J * NDIM + o_a] = A[K+J * NDIM + o_a];
                    A[K+J * NDIM + o_a] = T;
                    if (T == 0.0E0) goto LABEL45;
                    for (I = KP1; I <= N; I++)
                    {
                        A[I+J * NDIM + o_a] += A[I+K * NDIM + o_a] * T;
                    }
                LABEL45:;
                }
            }
        LABEL70:  K = N;
            if (A[N+N * NDIM + o_a] == 0.0E0) goto LABEL80;
            return;
        LABEL80:  IER = K;
            IP[N + o_ip] = 0;
            return;
            // C----------------------- END OF SUBROUTINE DEC -------------------------
            #endregion
        }
    }

    #endregion


    #region The Class: SOL
    
    // C
    // C
    public class SOL
    {
    
        public SOL()
        {
    
        }
    
        /// <param name="N">
        /// = ORDER OF MATRIX.
        ///</param>
        /// <param name="NDIM">
        /// = DECLARED DIMENSION OF ARRAY  A .
        ///</param>
        /// <param name="A">
        /// = TRIANGULARIZED MATRIX OBTAINED FROM DEC.
        ///</param>
        /// <param name="B">
        /// = RIGHT HAND SIDE VECTOR.
        ///</param>
        /// <param name="IP">
        /// = PIVOT VECTOR OBTAINED FROM DEC.
        ///</param>
        public void Run(int N, int NDIM, double[] A, int offset_a, ref double[] B, int offset_b, int[] IP, int offset_ip)
        {
            #region Variables
            
            int NM1 = 0; int K = 0; int KP1 = 0; int M = 0; int I = 0; int KB = 0; int KM1 = 0; double T = 0; 
            #endregion
            #region Array Index Correction
            
             int o_a = -1 - NDIM + offset_a;  int o_b = -1 + offset_b;  int o_ip = -1 + offset_ip; 
            #endregion
            #region Prolog
            
            // C VERSION REAL DOUBLE PRECISION
            // C-----------------------------------------------------------------------
            // C  SOLUTION OF LINEAR SYSTEM, A*X = B .
            // C  INPUT..
            // C    N = ORDER OF MATRIX.
            // C    NDIM = DECLARED DIMENSION OF ARRAY  A .
            // C    A = TRIANGULARIZED MATRIX OBTAINED FROM DEC.
            // C    B = RIGHT HAND SIDE VECTOR.
            // C    IP = PIVOT VECTOR OBTAINED FROM DEC.
            // C  DO NOT USE IF DEC HAS SET IER .NE. 0.
            // C  OUTPUT..
            // C    B = SOLUTION VECTOR, X .
            // C-----------------------------------------------------------------------
            #endregion
            #region Body
            
            if (N == 1) goto LABEL50;
            NM1 = N - 1;
            for (K = 1; K <= NM1; K++)
            {
                KP1 = K + 1;
                M = IP[K + o_ip];
                T = B[M + o_b];
                B[M + o_b] = B[K + o_b];
                B[K + o_b] = T;
                for (I = KP1; I <= N; I++)
                {
                    B[I + o_b] += A[I+K * NDIM + o_a] * T;
                }
            }
            for (KB = 1; KB <= NM1; KB++)
            {
                KM1 = N - KB;
                K = KM1 + 1;
                B[K + o_b] /= A[K+K * NDIM + o_a];
                T =  - B[K + o_b];
                for (I = 1; I <= KM1; I++)
                {
                    B[I + o_b] += A[I+K * NDIM + o_a] * T;
                }
            }
        LABEL50:  B[1 + o_b] /= A[1+1 * NDIM + o_a];
            return;
            // C----------------------- END OF SUBROUTINE SOL -------------------------
            #endregion
        }
    }

    #endregion


    #region The Class: DECH
    
    // c
    // c
    public class DECH
    {
    
        public DECH()
        {
    
        }
    
        /// <param name="N">
        /// = ORDER OF MATRIX A.
        ///</param>
        /// <param name="NDIM">
        /// = DECLARED DIMENSION OF ARRAY  A .
        ///</param>
        /// <param name="A">
        /// = MATRIX TO BE TRIANGULARIZED.
        ///</param>
        /// <param name="LB">
        /// = LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED, LB.GE.1).
        ///</param>
        /// <param name="IER">
        /// = 0 IF MATRIX A IS NONSINGULAR, OR K IF FOUND TO BE
        /// SINGULAR AT STAGE K.
        ///</param>
        public void Run(int N, int NDIM, ref double[] A, int offset_a, int LB, ref int[] IP, int offset_ip, ref int IER)
        {
            #region Variables
            
            int NM1 = 0; int K = 0; int KP1 = 0; int M = 0; int I = 0; int J = 0; int NA = 0; double T = 0; 
            #endregion
            #region Array Index Correction
            
             int o_a = -1 - NDIM + offset_a;  int o_ip = -1 + offset_ip; 
            #endregion
            #region Prolog
            
            // C VERSION REAL DOUBLE PRECISION
            // C-----------------------------------------------------------------------
            // C  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION OF A HESSENBERG
            // C  MATRIX WITH LOWER BANDWIDTH LB
            // C  INPUT..
            // C     N = ORDER OF MATRIX A.
            // C     NDIM = DECLARED DIMENSION OF ARRAY  A .
            // C     A = MATRIX TO BE TRIANGULARIZED.
            // C     LB = LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED, LB.GE.1).
            // C  OUTPUT..
            // C     A(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U .
            // C     A(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L.
            // C     IP(K), K.LT.N = INDEX OF K-TH PIVOT ROW.
            // C     IP(N) = (-1)**(NUMBER OF INTERCHANGES) OR O .
            // C     IER = 0 IF MATRIX A IS NONSINGULAR, OR K IF FOUND TO BE
            // C           SINGULAR AT STAGE K.
            // C  USE  SOLH  TO OBTAIN SOLUTION OF LINEAR SYSTEM.
            // C  DETERM(A) = IP(N)*A(1,1)*A(2,2)*...*A(N,N).
            // C  IF IP(N)=O, A IS SINGULAR, SOL WILL DIVIDE BY ZERO.
            // C
            // C  REFERENCE..
            // C     THIS IS A SLIGHT MODIFICATION OF
            // C     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER,
            // C     C.A.C.M. 15 (1972), P. 274.
            // C-----------------------------------------------------------------------
            #endregion
            #region Body
            
            IER = 0;
            IP[N + o_ip] = 1;
            if (N == 1) goto LABEL70;
            NM1 = N - 1;
            for (K = 1; K <= NM1; K++)
            {
                KP1 = K + 1;
                M = K;
                NA = Math.Min(N, LB + K);
                for (I = KP1; I <= NA; I++)
                {
                    if (Math.Abs(A[I+K * NDIM + o_a]) > Math.Abs(A[M+K * NDIM + o_a])) M = I;
                }
                IP[K + o_ip] = M;
                T = A[M+K * NDIM + o_a];
                if (M == K) goto LABEL20;
                IP[N + o_ip] =  - IP[N + o_ip];
                A[M+K * NDIM + o_a] = A[K+K * NDIM + o_a];
                A[K+K * NDIM + o_a] = T;
            LABEL20:;
                if (T == 0.0E0) goto LABEL80;
                T = 1.0E0 / T;
                for (I = KP1; I <= NA; I++)
                {
                    A[I+K * NDIM + o_a] =  - A[I+K * NDIM + o_a] * T;
                }
                for (J = KP1; J <= N; J++)
                {
                    T = A[M+J * NDIM + o_a];
                    A[M+J * NDIM + o_a] = A[K+J * NDIM + o_a];
                    A[K+J * NDIM + o_a] = T;
                    if (T == 0.0E0) goto LABEL45;
                    for (I = KP1; I <= NA; I++)
                    {
                        A[I+J * NDIM + o_a] += A[I+K * NDIM + o_a] * T;
                    }
                LABEL45:;
                }
            }
        LABEL70:  K = N;
            if (A[N+N * NDIM + o_a] == 0.0E0) goto LABEL80;
            return;
        LABEL80:  IER = K;
            IP[N + o_ip] = 0;
            return;
            // C----------------------- END OF SUBROUTINE DECH ------------------------
            #endregion
        }
    }

    #endregion


    #region The Class: SOLH
    
    // C
    // C
    public class SOLH
    {
    
        public SOLH()
        {
    
        }
    
        /// <param name="N">
        /// = ORDER OF MATRIX A.
        ///</param>
        /// <param name="NDIM">
        /// = DECLARED DIMENSION OF ARRAY  A .
        ///</param>
        /// <param name="A">
        /// = TRIANGULARIZED MATRIX OBTAINED FROM DECH.
        ///</param>
        /// <param name="LB">
        /// = LOWER BANDWIDTH OF A.
        ///</param>
        /// <param name="B">
        /// = RIGHT HAND SIDE VECTOR.
        ///</param>
        /// <param name="IP">
        /// = PIVOT VECTOR OBTAINED FROM DEC.
        ///</param>
        public void Run(int N, int NDIM, double[] A, int offset_a, int LB, ref double[] B, int offset_b, int[] IP, int offset_ip)
        {
            #region Variables
            
            int NM1 = 0; int K = 0; int KP1 = 0; int M = 0; int I = 0; int KB = 0; int KM1 = 0; int NA = 0; double T = 0; 
            #endregion
            #region Array Index Correction
            
             int o_a = -1 - NDIM + offset_a;  int o_b = -1 + offset_b;  int o_ip = -1 + offset_ip; 
            #endregion
            #region Prolog
            
            // C VERSION REAL DOUBLE PRECISION
            // C-----------------------------------------------------------------------
            // C  SOLUTION OF LINEAR SYSTEM, A*X = B .
            // C  INPUT..
            // C    N = ORDER OF MATRIX A.
            // C    NDIM = DECLARED DIMENSION OF ARRAY  A .
            // C    A = TRIANGULARIZED MATRIX OBTAINED FROM DECH.
            // C    LB = LOWER BANDWIDTH OF A.
            // C    B = RIGHT HAND SIDE VECTOR.
            // C    IP = PIVOT VECTOR OBTAINED FROM DEC.
            // C  DO NOT USE IF DECH HAS SET IER .NE. 0.
            // C  OUTPUT..
            // C    B = SOLUTION VECTOR, X .
            // C-----------------------------------------------------------------------
            #endregion
            #region Body
            
            if (N == 1) goto LABEL50;
            NM1 = N - 1;
            for (K = 1; K <= NM1; K++)
            {
                KP1 = K + 1;
                M = IP[K + o_ip];
                T = B[M + o_b];
                B[M + o_b] = B[K + o_b];
                B[K + o_b] = T;
                NA = Math.Min(N, LB + K);
                for (I = KP1; I <= NA; I++)
                {
                    B[I + o_b] += A[I+K * NDIM + o_a] * T;
                }
            }
            for (KB = 1; KB <= NM1; KB++)
            {
                KM1 = N - KB;
                K = KM1 + 1;
                B[K + o_b] /= A[K+K * NDIM + o_a];
                T =  - B[K + o_b];
                for (I = 1; I <= KM1; I++)
                {
                    B[I + o_b] += A[I+K * NDIM + o_a] * T;
                }
            }
        LABEL50:  B[1 + o_b] /= A[1+1 * NDIM + o_a];
            return;
            // C----------------------- END OF SUBROUTINE SOLH ------------------------
            #endregion
        }
    }

    #endregion


    #region The Class: DECC
    
    // C
    public class DECC
    {
    
        public DECC()
        {
    
        }
    
        /// <param name="N">
        /// = ORDER OF MATRIX.
        ///</param>
        /// <param name="NDIM">
        /// = DECLARED DIMENSION OF ARRAYS  AR AND AI .
        ///</param>
        /// <param name="IER">
        /// = 0 IF MATRIX A IS NONSINGULAR, OR K IF FOUND TO BE
        /// SINGULAR AT STAGE K.
        ///</param>
        public void Run(int N, int NDIM, ref double[] AR, int offset_ar, ref double[] AI, int offset_ai, ref int[] IP, int offset_ip, ref int IER)
        {
            #region Variables
            
            int NM1 = 0; int K = 0; int KP1 = 0; int M = 0; int I = 0; int J = 0; 
            #endregion
            #region Implicit Variables
            
            double TR = 0; double TI = 0; double DEN = 0; double PRODR = 0; int AR_K = 0; int AI_K = 0; double PRODI = 0; 
            int AR_J = 0;int AI_J = 0; 
            #endregion
            #region Array Index Correction
            
             int o_ar = -1 - NDIM + offset_ar;  int o_ai = -1 - NDIM + offset_ai;  int o_ip = -1 + offset_ip; 
            #endregion
            #region Prolog
            
            // C VERSION COMPLEX DOUBLE PRECISION
            // C-----------------------------------------------------------------------
            // C  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION
            // C  ------ MODIFICATION FOR COMPLEX MATRICES --------
            // C  INPUT..
            // C     N = ORDER OF MATRIX.
            // C     NDIM = DECLARED DIMENSION OF ARRAYS  AR AND AI .
            // C     (AR, AI) = MATRIX TO BE TRIANGULARIZED.
            // C  OUTPUT..
            // C     AR(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U ; REAL PART.
            // C     AI(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U ; IMAGINARY PART.
            // C     AR(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L.
            // C                                                    REAL PART.
            // C     AI(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L.
            // C                                                    IMAGINARY PART.
            // C     IP(K), K.LT.N = INDEX OF K-TH PIVOT ROW.
            // C     IP(N) = (-1)**(NUMBER OF INTERCHANGES) OR O .
            // C     IER = 0 IF MATRIX A IS NONSINGULAR, OR K IF FOUND TO BE
            // C           SINGULAR AT STAGE K.
            // C  USE  SOL  TO OBTAIN SOLUTION OF LINEAR SYSTEM.
            // C  IF IP(N)=O, A IS SINGULAR, SOL WILL DIVIDE BY ZERO.
            // C
            // C  REFERENCE..
            // C     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER,
            // C     C.A.C.M. 15 (1972), P. 274.
            // C-----------------------------------------------------------------------
            #endregion
            #region Body
            
            IER = 0;
            IP[N + o_ip] = 1;
            if (N == 1) goto LABEL70;
            NM1 = N - 1;
            for (K = 1; K <= NM1; K++)
            {
                KP1 = K + 1;
                M = K;
                for (I = KP1; I <= N; I++)
                {
                    if (Math.Abs(AR[I+K * NDIM + o_ar]) + Math.Abs(AI[I+K * NDIM + o_ai]) > Math.Abs(AR[M+K * NDIM + o_ar]) + Math.Abs(AI[M+K * NDIM + o_ai])) M = I;
                }
                IP[K + o_ip] = M;
                TR = AR[M+K * NDIM + o_ar];
                TI = AI[M+K * NDIM + o_ai];
                if (M == K) goto LABEL20;
                IP[N + o_ip] =  - IP[N + o_ip];
                AR[M+K * NDIM + o_ar] = AR[K+K * NDIM + o_ar];
                AI[M+K * NDIM + o_ai] = AI[K+K * NDIM + o_ai];
                AR[K+K * NDIM + o_ar] = TR;
                AI[K+K * NDIM + o_ai] = TI;
            LABEL20:;
                if (Math.Abs(TR) + Math.Abs(TI) == 0.0E0) goto LABEL80;
                DEN = TR * TR + TI * TI;
                TR /= DEN;
                TI =  - TI / DEN;
                AR_K = K * NDIM + o_ar;
                AI_K = K * NDIM + o_ai;
                for (I = KP1; I <= N; I++)
                {
                    PRODR = AR[I + AR_K] * TR - AI[I + AI_K] * TI;
                    PRODI = AI[I + AI_K] * TR + AR[I + AR_K] * TI;
                    AR[I + AR_K] =  - PRODR;
                    AI[I + AI_K] =  - PRODI;
                }
                for (J = KP1; J <= N; J++)
                {
                    TR = AR[M+J * NDIM + o_ar];
                    TI = AI[M+J * NDIM + o_ai];
                    AR[M+J * NDIM + o_ar] = AR[K+J * NDIM + o_ar];
                    AI[M+J * NDIM + o_ai] = AI[K+J * NDIM + o_ai];
                    AR[K+J * NDIM + o_ar] = TR;
                    AI[K+J * NDIM + o_ai] = TI;
                    if (Math.Abs(TR) + Math.Abs(TI) == 0.0E0) goto LABEL48;
                    if (TI == 0.0E0)
                    {
                        AR_K = K * NDIM + o_ar;
                        AI_K = K * NDIM + o_ai;
                        AR_J = J * NDIM + o_ar;
                        AI_J = J * NDIM + o_ai;
                        for (I = KP1; I <= N; I++)
                        {
                            PRODR = AR[I + AR_K] * TR;
                            PRODI = AI[I + AI_K] * TR;
                            AR[I + AR_J] += PRODR;
                            AI[I + AI_J] += PRODI;
                        }
                        goto LABEL48;
                    }
                    if (TR == 0.0E0)
                    {
                        AI_K = K * NDIM + o_ai;
                        AR_K = K * NDIM + o_ar;
                        AR_J = J * NDIM + o_ar;
                        AI_J = J * NDIM + o_ai;
                        for (I = KP1; I <= N; I++)
                        {
                            PRODR =  - AI[I + AI_K] * TI;
                            PRODI = AR[I + AR_K] * TI;
                            AR[I + AR_J] += PRODR;
                            AI[I + AI_J] += PRODI;
                        }
                        goto LABEL48;
                    }
                    AR_K = K * NDIM + o_ar;
                    AI_K = K * NDIM + o_ai;
                    AR_J = J * NDIM + o_ar;
                    AI_J = J * NDIM + o_ai;
                    for (I = KP1; I <= N; I++)
                    {
                        PRODR = AR[I + AR_K] * TR - AI[I + AI_K] * TI;
                        PRODI = AI[I + AI_K] * TR + AR[I + AR_K] * TI;
                        AR[I + AR_J] += PRODR;
                        AI[I + AI_J] += PRODI;
                    }
                LABEL48:;
                }
            }
        LABEL70:  K = N;
            if (Math.Abs(AR[N+N * NDIM + o_ar]) + Math.Abs(AI[N+N * NDIM + o_ai]) == 0.0E0) goto LABEL80;
            return;
        LABEL80:  IER = K;
            IP[N + o_ip] = 0;
            return;
            // C----------------------- END OF SUBROUTINE DECC ------------------------
            #endregion
        }
    }

    #endregion


    #region The Class: SOLC
    
    // C
    // C
    public class SOLC
    {
    
        public SOLC()
        {
    
        }
    
        /// <param name="N">
        /// = ORDER OF MATRIX.
        ///</param>
        /// <param name="NDIM">
        /// = DECLARED DIMENSION OF ARRAYS  AR AND AI.
        ///</param>
        /// <param name="IP">
        /// = PIVOT VECTOR OBTAINED FROM DEC.
        ///</param>
        public void Run(int N, int NDIM, double[] AR, int offset_ar, double[] AI, int offset_ai, ref double[] BR, int offset_br, ref double[] BI, int offset_bi
                         , int[] IP, int offset_ip)
        {
            #region Variables
            
            int NM1 = 0; int K = 0; int KP1 = 0; int M = 0; int I = 0; int KB = 0; int KM1 = 0; 
            #endregion
            #region Implicit Variables
            
            double TR = 0; double TI = 0; double PRODR = 0; int AR_K = 0; int AI_K = 0; double PRODI = 0; double DEN = 0; 
            #endregion
            #region Array Index Correction
            
             int o_ar = -1 - NDIM + offset_ar;  int o_ai = -1 - NDIM + offset_ai;  int o_br = -1 + offset_br; 
             int o_bi = -1 + offset_bi; int o_ip = -1 + offset_ip; 
            #endregion
            #region Prolog
            
            // C VERSION COMPLEX DOUBLE PRECISION
            // C-----------------------------------------------------------------------
            // C  SOLUTION OF LINEAR SYSTEM, A*X = B .
            // C  INPUT..
            // C    N = ORDER OF MATRIX.
            // C    NDIM = DECLARED DIMENSION OF ARRAYS  AR AND AI.
            // C    (AR,AI) = TRIANGULARIZED MATRIX OBTAINED FROM DEC.
            // C    (BR,BI) = RIGHT HAND SIDE VECTOR.
            // C    IP = PIVOT VECTOR OBTAINED FROM DEC.
            // C  DO NOT USE IF DEC HAS SET IER .NE. 0.
            // C  OUTPUT..
            // C    (BR,BI) = SOLUTION VECTOR, X .
            // C-----------------------------------------------------------------------
            #endregion
            #region Body
            
            if (N == 1) goto LABEL50;
            NM1 = N - 1;
            for (K = 1; K <= NM1; K++)
            {
                KP1 = K + 1;
                M = IP[K + o_ip];
                TR = BR[M + o_br];
                TI = BI[M + o_bi];
                BR[M + o_br] = BR[K + o_br];
                BI[M + o_bi] = BI[K + o_bi];
                BR[K + o_br] = TR;
                BI[K + o_bi] = TI;
                AR_K = K * NDIM + o_ar;
                AI_K = K * NDIM + o_ai;
                for (I = KP1; I <= N; I++)
                {
                    PRODR = AR[I + AR_K] * TR - AI[I + AI_K] * TI;
                    PRODI = AI[I + AI_K] * TR + AR[I + AR_K] * TI;
                    BR[I + o_br] += PRODR;
                    BI[I + o_bi] += PRODI;
                }
            }
            for (KB = 1; KB <= NM1; KB++)
            {
                KM1 = N - KB;
                K = KM1 + 1;
                DEN = AR[K+K * NDIM + o_ar] * AR[K+K * NDIM + o_ar] + AI[K+K * NDIM + o_ai] * AI[K+K * NDIM + o_ai];
                PRODR = BR[K + o_br] * AR[K+K * NDIM + o_ar] + BI[K + o_bi] * AI[K+K * NDIM + o_ai];
                PRODI = BI[K + o_bi] * AR[K+K * NDIM + o_ar] - BR[K + o_br] * AI[K+K * NDIM + o_ai];
                BR[K + o_br] = PRODR / DEN;
                BI[K + o_bi] = PRODI / DEN;
                TR =  - BR[K + o_br];
                TI =  - BI[K + o_bi];
                AR_K = K * NDIM + o_ar;
                AI_K = K * NDIM + o_ai;
                for (I = 1; I <= KM1; I++)
                {
                    PRODR = AR[I + AR_K] * TR - AI[I + AI_K] * TI;
                    PRODI = AI[I + AI_K] * TR + AR[I + AR_K] * TI;
                    BR[I + o_br] += PRODR;
                    BI[I + o_bi] += PRODI;
                }
            }
        LABEL50:;
            DEN = AR[1+1 * NDIM + o_ar] * AR[1+1 * NDIM + o_ar] + AI[1+1 * NDIM + o_ai] * AI[1+1 * NDIM + o_ai];
            PRODR = BR[1 + o_br] * AR[1+1 * NDIM + o_ar] + BI[1 + o_bi] * AI[1+1 * NDIM + o_ai];
            PRODI = BI[1 + o_bi] * AR[1+1 * NDIM + o_ar] - BR[1 + o_br] * AI[1+1 * NDIM + o_ai];
            BR[1 + o_br] = PRODR / DEN;
            BI[1 + o_bi] = PRODI / DEN;
            return;
            // C----------------------- END OF SUBROUTINE SOLC ------------------------
            #endregion
        }
    }

    #endregion


    #region The Class: DECHC
    
    // C
    // C
    public class DECHC
    {
    
        public DECHC()
        {
    
        }
    
        /// <param name="N">
        /// = ORDER OF MATRIX.
        ///</param>
        /// <param name="NDIM">
        /// = DECLARED DIMENSION OF ARRAYS  AR AND AI .
        ///</param>
        /// <param name="LB">
        /// = LOWER BANDWIDTH OF A (DIAGONAL NOT COUNTED), LB.GE.1.
        ///</param>
        /// <param name="IER">
        /// = 0 IF MATRIX A IS NONSINGULAR, OR K IF FOUND TO BE
        /// SINGULAR AT STAGE K.
        ///</param>
        public void Run(int N, int NDIM, ref double[] AR, int offset_ar, ref double[] AI, int offset_ai, int LB, ref int[] IP, int offset_ip
                         , ref int IER)
        {
            #region Variables
            
            int NM1 = 0; int K = 0; int KP1 = 0; int M = 0; int I = 0; int J = 0; 
            #endregion
            #region Implicit Variables
            
            int NA = 0; double TR = 0; double TI = 0; double DEN = 0; double PRODR = 0; int AR_K = 0; int AI_K = 0; 
            double PRODI = 0;int AR_J = 0; int AI_J = 0; 
            #endregion
            #region Array Index Correction
            
             int o_ar = -1 - NDIM + offset_ar;  int o_ai = -1 - NDIM + offset_ai;  int o_ip = -1 + offset_ip; 
            #endregion
            #region Prolog
            
            // C VERSION COMPLEX DOUBLE PRECISION
            // C-----------------------------------------------------------------------
            // C  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION
            // C  ------ MODIFICATION FOR COMPLEX MATRICES --------
            // C  INPUT..
            // C     N = ORDER OF MATRIX.
            // C     NDIM = DECLARED DIMENSION OF ARRAYS  AR AND AI .
            // C     (AR, AI) = MATRIX TO BE TRIANGULARIZED.
            // C  OUTPUT..
            // C     AR(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U ; REAL PART.
            // C     AI(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U ; IMAGINARY PART.
            // C     AR(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L.
            // C                                                    REAL PART.
            // C     AI(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L.
            // C                                                    IMAGINARY PART.
            // C     LB = LOWER BANDWIDTH OF A (DIAGONAL NOT COUNTED), LB.GE.1.
            // C     IP(K), K.LT.N = INDEX OF K-TH PIVOT ROW.
            // C     IP(N) = (-1)**(NUMBER OF INTERCHANGES) OR O .
            // C     IER = 0 IF MATRIX A IS NONSINGULAR, OR K IF FOUND TO BE
            // C           SINGULAR AT STAGE K.
            // C  USE  SOL  TO OBTAIN SOLUTION OF LINEAR SYSTEM.
            // C  IF IP(N)=O, A IS SINGULAR, SOL WILL DIVIDE BY ZERO.
            // C
            // C  REFERENCE..
            // C     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER,
            // C     C.A.C.M. 15 (1972), P. 274.
            // C-----------------------------------------------------------------------
            #endregion
            #region Body
            
            IER = 0;
            IP[N + o_ip] = 1;
            if (LB == 0) goto LABEL70;
            if (N == 1) goto LABEL70;
            NM1 = N - 1;
            for (K = 1; K <= NM1; K++)
            {
                KP1 = K + 1;
                M = K;
                NA = Math.Min(N, LB + K);
                for (I = KP1; I <= NA; I++)
                {
                    if (Math.Abs(AR[I+K * NDIM + o_ar]) + Math.Abs(AI[I+K * NDIM + o_ai]) > Math.Abs(AR[M+K * NDIM + o_ar]) + Math.Abs(AI[M+K * NDIM + o_ai])) M = I;
                }
                IP[K + o_ip] = M;
                TR = AR[M+K * NDIM + o_ar];
                TI = AI[M+K * NDIM + o_ai];
                if (M == K) goto LABEL20;
                IP[N + o_ip] =  - IP[N + o_ip];
                AR[M+K * NDIM + o_ar] = AR[K+K * NDIM + o_ar];
                AI[M+K * NDIM + o_ai] = AI[K+K * NDIM + o_ai];
                AR[K+K * NDIM + o_ar] = TR;
                AI[K+K * NDIM + o_ai] = TI;
            LABEL20:;
                if (Math.Abs(TR) + Math.Abs(TI) == 0.0E0) goto LABEL80;
                DEN = TR * TR + TI * TI;
                TR /= DEN;
                TI =  - TI / DEN;
                AR_K = K * NDIM + o_ar;
                AI_K = K * NDIM + o_ai;
                for (I = KP1; I <= NA; I++)
                {
                    PRODR = AR[I + AR_K] * TR - AI[I + AI_K] * TI;
                    PRODI = AI[I + AI_K] * TR + AR[I + AR_K] * TI;
                    AR[I + AR_K] =  - PRODR;
                    AI[I + AI_K] =  - PRODI;
                }
                for (J = KP1; J <= N; J++)
                {
                    TR = AR[M+J * NDIM + o_ar];
                    TI = AI[M+J * NDIM + o_ai];
                    AR[M+J * NDIM + o_ar] = AR[K+J * NDIM + o_ar];
                    AI[M+J * NDIM + o_ai] = AI[K+J * NDIM + o_ai];
                    AR[K+J * NDIM + o_ar] = TR;
                    AI[K+J * NDIM + o_ai] = TI;
                    if (Math.Abs(TR) + Math.Abs(TI) == 0.0E0) goto LABEL48;
                    if (TI == 0.0E0)
                    {
                        AR_K = K * NDIM + o_ar;
                        AI_K = K * NDIM + o_ai;
                        AR_J = J * NDIM + o_ar;
                        AI_J = J * NDIM + o_ai;
                        for (I = KP1; I <= NA; I++)
                        {
                            PRODR = AR[I + AR_K] * TR;
                            PRODI = AI[I + AI_K] * TR;
                            AR[I + AR_J] += PRODR;
                            AI[I + AI_J] += PRODI;
                        }
                        goto LABEL48;
                    }
                    if (TR == 0.0E0)
                    {
                        AI_K = K * NDIM + o_ai;
                        AR_K = K * NDIM + o_ar;
                        AR_J = J * NDIM + o_ar;
                        AI_J = J * NDIM + o_ai;
                        for (I = KP1; I <= NA; I++)
                        {
                            PRODR =  - AI[I + AI_K] * TI;
                            PRODI = AR[I + AR_K] * TI;
                            AR[I + AR_J] += PRODR;
                            AI[I + AI_J] += PRODI;
                        }
                        goto LABEL48;
                    }
                    AR_K = K * NDIM + o_ar;
                    AI_K = K * NDIM + o_ai;
                    AR_J = J * NDIM + o_ar;
                    AI_J = J * NDIM + o_ai;
                    for (I = KP1; I <= NA; I++)
                    {
                        PRODR = AR[I + AR_K] * TR - AI[I + AI_K] * TI;
                        PRODI = AI[I + AI_K] * TR + AR[I + AR_K] * TI;
                        AR[I + AR_J] += PRODR;
                        AI[I + AI_J] += PRODI;
                    }
                LABEL48:;
                }
            }
        LABEL70:  K = N;
            if (Math.Abs(AR[N+N * NDIM + o_ar]) + Math.Abs(AI[N+N * NDIM + o_ai]) == 0.0E0) goto LABEL80;
            return;
        LABEL80:  IER = K;
            IP[N + o_ip] = 0;
            return;
            // C----------------------- END OF SUBROUTINE DECHC -----------------------
            #endregion
        }
    }

    #endregion


    #region The Class: SOLHC
    
    // C
    // C
    public class SOLHC
    {
    
        public SOLHC()
        {
    
        }
    
        /// <param name="N">
        /// = ORDER OF MATRIX.
        ///</param>
        /// <param name="NDIM">
        /// = DECLARED DIMENSION OF ARRAYS  AR AND AI.
        ///</param>
        /// <param name="LB">
        /// = LOWER BANDWIDTH OF A.
        ///</param>
        /// <param name="IP">
        /// = PIVOT VECTOR OBTAINED FROM DEC.
        ///</param>
        public void Run(int N, int NDIM, double[] AR, int offset_ar, double[] AI, int offset_ai, int LB, ref double[] BR, int offset_br
                         , ref double[] BI, int offset_bi, int[] IP, int offset_ip)
        {
            #region Variables
            
            int NM1 = 0; int K = 0; int KP1 = 0; int M = 0; int I = 0; int KB = 0; int KM1 = 0; 
            #endregion
            #region Implicit Variables
            
            double TR = 0; double TI = 0; double PRODR = 0; int AR_K = 0; int AI_K = 0; double PRODI = 0; double DEN = 0; 
            #endregion
            #region Array Index Correction
            
             int o_ar = -1 - NDIM + offset_ar;  int o_ai = -1 - NDIM + offset_ai;  int o_br = -1 + offset_br; 
             int o_bi = -1 + offset_bi; int o_ip = -1 + offset_ip; 
            #endregion
            #region Prolog
            
            // C VERSION COMPLEX DOUBLE PRECISION
            // C-----------------------------------------------------------------------
            // C  SOLUTION OF LINEAR SYSTEM, A*X = B .
            // C  INPUT..
            // C    N = ORDER OF MATRIX.
            // C    NDIM = DECLARED DIMENSION OF ARRAYS  AR AND AI.
            // C    (AR,AI) = TRIANGULARIZED MATRIX OBTAINED FROM DEC.
            // C    (BR,BI) = RIGHT HAND SIDE VECTOR.
            // C    LB = LOWER BANDWIDTH OF A.
            // C    IP = PIVOT VECTOR OBTAINED FROM DEC.
            // C  DO NOT USE IF DEC HAS SET IER .NE. 0.
            // C  OUTPUT..
            // C    (BR,BI) = SOLUTION VECTOR, X .
            // C-----------------------------------------------------------------------
            #endregion
            #region Body
            
            if (N == 1) goto LABEL50;
            NM1 = N - 1;
            if (LB == 0) goto LABEL25;
            for (K = 1; K <= NM1; K++)
            {
                KP1 = K + 1;
                M = IP[K + o_ip];
                TR = BR[M + o_br];
                TI = BI[M + o_bi];
                BR[M + o_br] = BR[K + o_br];
                BI[M + o_bi] = BI[K + o_bi];
                BR[K + o_br] = TR;
                BI[K + o_bi] = TI;
                AR_K = K * NDIM + o_ar;
                AI_K = K * NDIM + o_ai;
                for (I = KP1; I <= Math.Min(N, LB + K); I++)
                {
                    PRODR = AR[I + AR_K] * TR - AI[I + AI_K] * TI;
                    PRODI = AI[I + AI_K] * TR + AR[I + AR_K] * TI;
                    BR[I + o_br] += PRODR;
                    BI[I + o_bi] += PRODI;
                }
            }
        LABEL25:;
            for (KB = 1; KB <= NM1; KB++)
            {
                KM1 = N - KB;
                K = KM1 + 1;
                DEN = AR[K+K * NDIM + o_ar] * AR[K+K * NDIM + o_ar] + AI[K+K * NDIM + o_ai] * AI[K+K * NDIM + o_ai];
                PRODR = BR[K + o_br] * AR[K+K * NDIM + o_ar] + BI[K + o_bi] * AI[K+K * NDIM + o_ai];
                PRODI = BI[K + o_bi] * AR[K+K * NDIM + o_ar] - BR[K + o_br] * AI[K+K * NDIM + o_ai];
                BR[K + o_br] = PRODR / DEN;
                BI[K + o_bi] = PRODI / DEN;
                TR =  - BR[K + o_br];
                TI =  - BI[K + o_bi];
                AR_K = K * NDIM + o_ar;
                AI_K = K * NDIM + o_ai;
                for (I = 1; I <= KM1; I++)
                {
                    PRODR = AR[I + AR_K] * TR - AI[I + AI_K] * TI;
                    PRODI = AI[I + AI_K] * TR + AR[I + AR_K] * TI;
                    BR[I + o_br] += PRODR;
                    BI[I + o_bi] += PRODI;
                }
            }
        LABEL50:;
            DEN = AR[1+1 * NDIM + o_ar] * AR[1+1 * NDIM + o_ar] + AI[1+1 * NDIM + o_ai] * AI[1+1 * NDIM + o_ai];
            PRODR = BR[1 + o_br] * AR[1+1 * NDIM + o_ar] + BI[1 + o_bi] * AI[1+1 * NDIM + o_ai];
            PRODI = BI[1 + o_bi] * AR[1+1 * NDIM + o_ar] - BR[1 + o_br] * AI[1+1 * NDIM + o_ai];
            BR[1 + o_br] = PRODR / DEN;
            BI[1 + o_bi] = PRODI / DEN;
            return;
            // C----------------------- END OF SUBROUTINE SOLHC -----------------------
            #endregion
        }
    }

    #endregion


    #region The Class: DECB
    
    // C
    public class DECB
    {
    
        public DECB()
        {
    
        }
    
        /// <param name="N">
        /// ORDER OF THE ORIGINAL MATRIX A.
        ///</param>
        /// <param name="NDIM">
        /// DECLARED DIMENSION OF ARRAY  A.
        ///</param>
        /// <param name="A">
        /// CONTAINS THE MATRIX IN BAND STORAGE.   THE COLUMNS  
        /// OF THE MATRIX ARE STORED IN THE COLUMNS OF  A  AND
        /// THE DIAGONALS OF THE MATRIX ARE STORED IN ROWS 
        /// ML+1 THROUGH 2*ML+MU+1 OF  A.
        ///</param>
        /// <param name="ML">
        /// LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
        ///</param>
        /// <param name="MU">
        /// UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
        ///</param>
        /// <param name="IP">
        /// INDEX VECTOR OF PIVOT INDICES.
        ///</param>
        /// <param name="IER">
        /// = 0 IF MATRIX A IS NONSINGULAR, OR  = K IF FOUND TO BE
        /// SINGULAR AT STAGE K.
        ///</param>
        public void Run(int N, int NDIM, ref double[] A, int offset_a, int ML, int MU, ref int[] IP, int offset_ip
                         , ref int IER)
        {
            #region Variables
            
            double T = 0; 
            #endregion
            #region Implicit Variables
            
            int MD = 0; int MD1 = 0; int JU = 0; int I = 0; int J = 0; int NM1 = 0; int KP1 = 0; int K = 0; int M = 0; 
            int MDL = 0;int MM = 0; int JK = 0; int IJK = 0; 
            #endregion
            #region Array Index Correction
            
             int o_a = -1 - NDIM + offset_a;  int o_ip = -1 + offset_ip; 
            #endregion
            #region Prolog
            
            // C-----------------------------------------------------------------------
            // C  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION OF A BANDED
            // C  MATRIX WITH LOWER BANDWIDTH ML AND UPPER BANDWIDTH MU
            // C  INPUT..
            // C     N       ORDER OF THE ORIGINAL MATRIX A.
            // C     NDIM    DECLARED DIMENSION OF ARRAY  A.
            // C     A       CONTAINS THE MATRIX IN BAND STORAGE.   THE COLUMNS  
            // C                OF THE MATRIX ARE STORED IN THE COLUMNS OF  A  AND
            // C                THE DIAGONALS OF THE MATRIX ARE STORED IN ROWS 
            // C                ML+1 THROUGH 2*ML+MU+1 OF  A.
            // C     ML      LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
            // C     MU      UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
            // C  OUTPUT..
            // C     A       AN UPPER TRIANGULAR MATRIX IN BAND STORAGE AND 
            // C                THE MULTIPLIERS WHICH WERE USED TO OBTAIN IT.  
            // C     IP      INDEX VECTOR OF PIVOT INDICES.
            // C     IP(N)   (-1)**(NUMBER OF INTERCHANGES) OR O .
            // C     IER     = 0 IF MATRIX A IS NONSINGULAR, OR  = K IF FOUND TO BE
            // C                SINGULAR AT STAGE K.
            // C  USE  SOLB  TO OBTAIN SOLUTION OF LINEAR SYSTEM.
            // C  DETERM(A) = IP(N)*A(MD,1)*A(MD,2)*...*A(MD,N)  WITH MD=ML+MU+1.
            // C  IF IP(N)=O, A IS SINGULAR, SOLB WILL DIVIDE BY ZERO.
            // C
            // C  REFERENCE..
            // C     THIS IS A MODIFICATION OF
            // C     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER,
            // C     C.A.C.M. 15 (1972), P. 274.
            // C-----------------------------------------------------------------------
            #endregion
            #region Body
            
            IER = 0;
            IP[N + o_ip] = 1;
            MD = ML + MU + 1;
            MD1 = MD + 1;
            JU = 0;
            if (ML == 0) goto LABEL70;
            if (N == 1) goto LABEL70;
            if (N < MU + 2) goto LABEL7;
            for (J = MU + 2; J <= N; J++)
            {
                for (I = 1; I <= ML; I++)
                {
                    A[I+J * NDIM + o_a] = 0.0E0;
                }
            }
        LABEL7:  NM1 = N - 1;
            for (K = 1; K <= NM1; K++)
            {
                KP1 = K + 1;
                M = MD;
                MDL = Math.Min(ML, N - K) + MD;
                for (I = MD1; I <= MDL; I++)
                {
                    if (Math.Abs(A[I+K * NDIM + o_a]) > Math.Abs(A[M+K * NDIM + o_a])) M = I;
                }
                IP[K + o_ip] = M + K - MD;
                T = A[M+K * NDIM + o_a];
                if (M == MD) goto LABEL20;
                IP[N + o_ip] =  - IP[N + o_ip];
                A[M+K * NDIM + o_a] = A[MD+K * NDIM + o_a];
                A[MD+K * NDIM + o_a] = T;
            LABEL20:;
                if (T == 0.0E0) goto LABEL80;
                T = 1.0E0 / T;
                for (I = MD1; I <= MDL; I++)
                {
                    A[I+K * NDIM + o_a] =  - A[I+K * NDIM + o_a] * T;
                }
                JU = Math.Min(Math.Max(JU, MU + IP[K + o_ip]), N);
                MM = MD;
                if (JU < KP1) goto LABEL55;
                for (J = KP1; J <= JU; J++)
                {
                    M -= 1;
                    MM -= 1;
                    T = A[M+J * NDIM + o_a];
                    if (M == MM) goto LABEL35;
                    A[M+J * NDIM + o_a] = A[MM+J * NDIM + o_a];
                    A[MM+J * NDIM + o_a] = T;
                LABEL35:;
                    if (T == 0.0E0) goto LABEL45;
                    JK = J - K;
                    for (I = MD1; I <= MDL; I++)
                    {
                        IJK = I - JK;
                        A[IJK+J * NDIM + o_a] += A[I+K * NDIM + o_a] * T;
                    }
                LABEL45:;
                }
            LABEL55:;
            }
        LABEL70:  K = N;
            if (A[MD+N * NDIM + o_a] == 0.0E0) goto LABEL80;
            return;
        LABEL80:  IER = K;
            IP[N + o_ip] = 0;
            return;
            // C----------------------- END OF SUBROUTINE DECB ------------------------
            #endregion
        }
    }

    #endregion


    #region The Class: SOLB
    
    // C
    // C
    public class SOLB
    {
    
        public SOLB()
        {
    
        }
    
        /// <param name="N">
        /// ORDER OF MATRIX A.
        ///</param>
        /// <param name="NDIM">
        /// DECLARED DIMENSION OF ARRAY  A .
        ///</param>
        /// <param name="A">
        /// TRIANGULARIZED MATRIX OBTAINED FROM DECB.
        ///</param>
        /// <param name="ML">
        /// LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
        ///</param>
        /// <param name="MU">
        /// UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
        ///</param>
        /// <param name="B">
        /// RIGHT HAND SIDE VECTOR.
        ///</param>
        /// <param name="IP">
        /// PIVOT VECTOR OBTAINED FROM DECB.
        ///</param>
        public void Run(int N, int NDIM, double[] A, int offset_a, int ML, int MU, ref double[] B, int offset_b
                         , int[] IP, int offset_ip)
        {
            #region Variables
            
            double T = 0; 
            #endregion
            #region Implicit Variables
            
            int MD = 0; int MD1 = 0; int MDM = 0; int NM1 = 0; int M = 0; int K = 0; int MDL = 0; int IMD = 0; int I = 0; 
            int KB = 0;int KMD = 0; int LM = 0; 
            #endregion
            #region Array Index Correction
            
             int o_a = -1 - NDIM + offset_a;  int o_b = -1 + offset_b;  int o_ip = -1 + offset_ip; 
            #endregion
            #region Prolog
            
            // C-----------------------------------------------------------------------
            // C  SOLUTION OF LINEAR SYSTEM, A*X = B .
            // C  INPUT..
            // C    N      ORDER OF MATRIX A.
            // C    NDIM   DECLARED DIMENSION OF ARRAY  A .
            // C    A      TRIANGULARIZED MATRIX OBTAINED FROM DECB.
            // C    ML     LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
            // C    MU     UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
            // C    B      RIGHT HAND SIDE VECTOR.
            // C    IP     PIVOT VECTOR OBTAINED FROM DECB.
            // C  DO NOT USE IF DECB HAS SET IER .NE. 0.
            // C  OUTPUT..
            // C    B      SOLUTION VECTOR, X .
            // C-----------------------------------------------------------------------
            #endregion
            #region Body
            
            MD = ML + MU + 1;
            MD1 = MD + 1;
            MDM = MD - 1;
            NM1 = N - 1;
            if (ML == 0) goto LABEL25;
            if (N == 1) goto LABEL50;
            for (K = 1; K <= NM1; K++)
            {
                M = IP[K + o_ip];
                T = B[M + o_b];
                B[M + o_b] = B[K + o_b];
                B[K + o_b] = T;
                MDL = Math.Min(ML, N - K) + MD;
                for (I = MD1; I <= MDL; I++)
                {
                    IMD = I + K - MD;
                    B[IMD + o_b] += A[I+K * NDIM + o_a] * T;
                }
            }
        LABEL25:;
            for (KB = 1; KB <= NM1; KB++)
            {
                K = N + 1 - KB;
                B[K + o_b] /= A[MD+K * NDIM + o_a];
                T =  - B[K + o_b];
                KMD = MD - K;
                LM = Math.Max(1, KMD + 1);
                for (I = LM; I <= MDM; I++)
                {
                    IMD = I - KMD;
                    B[IMD + o_b] += A[I+K * NDIM + o_a] * T;
                }
            }
        LABEL50:  B[1 + o_b] /= A[MD+1 * NDIM + o_a];
            return;
            // C----------------------- END OF SUBROUTINE SOLB ------------------------
            #endregion
        }
    }

    #endregion


    #region The Class: DECBC
    
    // C
    public class DECBC
    {
    
        public DECBC()
        {
    
        }
    
        /// <param name="N">
        /// ORDER OF THE ORIGINAL MATRIX A.
        ///</param>
        /// <param name="NDIM">
        /// DECLARED DIMENSION OF ARRAY  A.
        ///</param>
        /// <param name="ML">
        /// LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
        ///</param>
        /// <param name="MU">
        /// UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
        ///</param>
        /// <param name="IP">
        /// INDEX VECTOR OF PIVOT INDICES.
        ///</param>
        /// <param name="IER">
        /// = 0 IF MATRIX A IS NONSINGULAR, OR  = K IF FOUND TO BE
        /// SINGULAR AT STAGE K.
        ///</param>
        public void Run(int N, int NDIM, ref double[] AR, int offset_ar, ref double[] AI, int offset_ai, int ML, int MU
                         , ref int[] IP, int offset_ip, ref int IER)
        {
            #region Implicit Variables
            
            int MD = 0; int MD1 = 0; int JU = 0; int I = 0; int J = 0; int AR_J = 0; int AI_J = 0; int NM1 = 0; int KP1 = 0; 
            int K = 0;int M = 0; int MDL = 0; double TR = 0; double TI = 0; double DEN = 0; double PRODR = 0; int AR_K = 0; 
            int AI_K = 0;double PRODI = 0; int MM = 0; int JK = 0; int IJK = 0; 
            #endregion
            #region Array Index Correction
            
             int o_ar = -1 - NDIM + offset_ar;  int o_ai = -1 - NDIM + offset_ai;  int o_ip = -1 + offset_ip; 
            #endregion
            #region Prolog
            
            // C-----------------------------------------------------------------------
            // C  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION OF A BANDED COMPLEX
            // C  MATRIX WITH LOWER BANDWIDTH ML AND UPPER BANDWIDTH MU
            // C  INPUT..
            // C     N       ORDER OF THE ORIGINAL MATRIX A.
            // C     NDIM    DECLARED DIMENSION OF ARRAY  A.
            // C     AR, AI     CONTAINS THE MATRIX IN BAND STORAGE.   THE COLUMNS  
            // C                OF THE MATRIX ARE STORED IN THE COLUMNS OF  AR (REAL
            // C                PART) AND AI (IMAGINARY PART)  AND
            // C                THE DIAGONALS OF THE MATRIX ARE STORED IN ROWS 
            // C                ML+1 THROUGH 2*ML+MU+1 OF  AR AND AI.
            // C     ML      LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
            // C     MU      UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
            // C  OUTPUT..
            // C     AR, AI  AN UPPER TRIANGULAR MATRIX IN BAND STORAGE AND 
            // C                THE MULTIPLIERS WHICH WERE USED TO OBTAIN IT.  
            // C     IP      INDEX VECTOR OF PIVOT INDICES.
            // C     IP(N)   (-1)**(NUMBER OF INTERCHANGES) OR O .
            // C     IER     = 0 IF MATRIX A IS NONSINGULAR, OR  = K IF FOUND TO BE
            // C                SINGULAR AT STAGE K.
            // C  USE  SOLBC  TO OBTAIN SOLUTION OF LINEAR SYSTEM.
            // C  DETERM(A) = IP(N)*A(MD,1)*A(MD,2)*...*A(MD,N)  WITH MD=ML+MU+1.
            // C  IF IP(N)=O, A IS SINGULAR, SOLBC WILL DIVIDE BY ZERO.
            // C
            // C  REFERENCE..
            // C     THIS IS A MODIFICATION OF
            // C     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER,
            // C     C.A.C.M. 15 (1972), P. 274.
            // C-----------------------------------------------------------------------
            #endregion
            #region Body
            
            IER = 0;
            IP[N + o_ip] = 1;
            MD = ML + MU + 1;
            MD1 = MD + 1;
            JU = 0;
            if (ML == 0) goto LABEL70;
            if (N == 1) goto LABEL70;
            if (N < MU + 2) goto LABEL7;
            for (J = MU + 2; J <= N; J++)
            {
                AR_J = J * NDIM + o_ar;
                AI_J = J * NDIM + o_ai;
                for (I = 1; I <= ML; I++)
                {
                    AR[I + AR_J] = 0.0E0;
                    AI[I + AI_J] = 0.0E0;
                }
            }
        LABEL7:  NM1 = N - 1;
            for (K = 1; K <= NM1; K++)
            {
                KP1 = K + 1;
                M = MD;
                MDL = Math.Min(ML, N - K) + MD;
                for (I = MD1; I <= MDL; I++)
                {
                    if (Math.Abs(AR[I+K * NDIM + o_ar]) + Math.Abs(AI[I+K * NDIM + o_ai]) > Math.Abs(AR[M+K * NDIM + o_ar]) + Math.Abs(AI[M+K * NDIM + o_ai])) M = I;
                }
                IP[K + o_ip] = M + K - MD;
                TR = AR[M+K * NDIM + o_ar];
                TI = AI[M+K * NDIM + o_ai];
                if (M == MD) goto LABEL20;
                IP[N + o_ip] =  - IP[N + o_ip];
                AR[M+K * NDIM + o_ar] = AR[MD+K * NDIM + o_ar];
                AI[M+K * NDIM + o_ai] = AI[MD+K * NDIM + o_ai];
                AR[MD+K * NDIM + o_ar] = TR;
                AI[MD+K * NDIM + o_ai] = TI;
            LABEL20:  
                if (Math.Abs(TR) + Math.Abs(TI) == 0.0E0) goto LABEL80;
                DEN = TR * TR + TI * TI;
                TR /= DEN;
                TI =  - TI / DEN;
                AR_K = K * NDIM + o_ar;
                AI_K = K * NDIM + o_ai;
                for (I = MD1; I <= MDL; I++)
                {
                    PRODR = AR[I + AR_K] * TR - AI[I + AI_K] * TI;
                    PRODI = AI[I + AI_K] * TR + AR[I + AR_K] * TI;
                    AR[I + AR_K] =  - PRODR;
                    AI[I + AI_K] =  - PRODI;
                }
                JU = Math.Min(Math.Max(JU, MU + IP[K + o_ip]), N);
                MM = MD;
                if (JU < KP1) goto LABEL55;
                for (J = KP1; J <= JU; J++)
                {
                    M -= 1;
                    MM -= 1;
                    TR = AR[M+J * NDIM + o_ar];
                    TI = AI[M+J * NDIM + o_ai];
                    if (M == MM) goto LABEL35;
                    AR[M+J * NDIM + o_ar] = AR[MM+J * NDIM + o_ar];
                    AI[M+J * NDIM + o_ai] = AI[MM+J * NDIM + o_ai];
                    AR[MM+J * NDIM + o_ar] = TR;
                    AI[MM+J * NDIM + o_ai] = TI;
                LABEL35:;
                    if (Math.Abs(TR) + Math.Abs(TI) == 0.0E0) goto LABEL48;
                    JK = J - K;
                    if (TI == 0.0E0)
                    {
                        AR_K = K * NDIM + o_ar;
                        AI_K = K * NDIM + o_ai;
                        for (I = MD1; I <= MDL; I++)
                        {
                            IJK = I - JK;
                            PRODR = AR[I + AR_K] * TR;
                            PRODI = AI[I + AI_K] * TR;
                            AR[IJK+J * NDIM + o_ar] += PRODR;
                            AI[IJK+J * NDIM + o_ai] += PRODI;
                        }
                        goto LABEL48;
                    }
                    if (TR == 0.0E0)
                    {
                        AI_K = K * NDIM + o_ai;
                        AR_K = K * NDIM + o_ar;
                        for (I = MD1; I <= MDL; I++)
                        {
                            IJK = I - JK;
                            PRODR =  - AI[I + AI_K] * TI;
                            PRODI = AR[I + AR_K] * TI;
                            AR[IJK+J * NDIM + o_ar] += PRODR;
                            AI[IJK+J * NDIM + o_ai] += PRODI;
                        }
                        goto LABEL48;
                    }
                    AR_K = K * NDIM + o_ar;
                    AI_K = K * NDIM + o_ai;
                    for (I = MD1; I <= MDL; I++)
                    {
                        IJK = I - JK;
                        PRODR = AR[I + AR_K] * TR - AI[I + AI_K] * TI;
                        PRODI = AI[I + AI_K] * TR + AR[I + AR_K] * TI;
                        AR[IJK+J * NDIM + o_ar] += PRODR;
                        AI[IJK+J * NDIM + o_ai] += PRODI;
                    }
                LABEL48:;
                }
            LABEL55:;
            }
        LABEL70:  K = N;
            if (Math.Abs(AR[MD+N * NDIM + o_ar]) + Math.Abs(AI[MD+N * NDIM + o_ai]) == 0.0E0) goto LABEL80;
            return;
        LABEL80:  IER = K;
            IP[N + o_ip] = 0;
            return;
            // C----------------------- END OF SUBROUTINE DECBC ------------------------
            #endregion
        }
    }

    #endregion


    #region The Class: SOLBC
    
    // C
    // C
    public class SOLBC
    {
    
        public SOLBC()
        {
    
        }
    
        /// <param name="N">
        /// ORDER OF MATRIX A.
        ///</param>
        /// <param name="NDIM">
        /// DECLARED DIMENSION OF ARRAY  A .
        ///</param>
        /// <param name="ML">
        /// LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
        ///</param>
        /// <param name="MU">
        /// UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
        ///</param>
        /// <param name="IP">
        /// PIVOT VECTOR OBTAINED FROM DECBC.
        ///</param>
        public void Run(int N, int NDIM, double[] AR, int offset_ar, double[] AI, int offset_ai, int ML, int MU
                         , ref double[] BR, int offset_br, ref double[] BI, int offset_bi, int[] IP, int offset_ip)
        {
            #region Implicit Variables
            
            int MD = 0; int MD1 = 0; int MDM = 0; int NM1 = 0; int M = 0; int K = 0; double TR = 0; double TI = 0; int MDL = 0; 
            int IMD = 0;int I = 0; double PRODR = 0; int AR_K = 0; int AI_K = 0; double PRODI = 0; int KB = 0; double DEN = 0; 
            int KMD = 0;int LM = 0; 
            #endregion
            #region Array Index Correction
            
             int o_ar = -1 - NDIM + offset_ar;  int o_ai = -1 - NDIM + offset_ai;  int o_br = -1 + offset_br; 
             int o_bi = -1 + offset_bi; int o_ip = -1 + offset_ip; 
            #endregion
            #region Prolog
            
            // C-----------------------------------------------------------------------
            // C  SOLUTION OF LINEAR SYSTEM, A*X = B ,
            // C                  VERSION BANDED AND COMPLEX-DOUBLE PRECISION.
            // C  INPUT..
            // C    N      ORDER OF MATRIX A.
            // C    NDIM   DECLARED DIMENSION OF ARRAY  A .
            // C    AR, AI TRIANGULARIZED MATRIX OBTAINED FROM DECB (REAL AND IMAG. PART).
            // C    ML     LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
            // C    MU     UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
            // C    BR, BI RIGHT HAND SIDE VECTOR (REAL AND IMAG. PART).
            // C    IP     PIVOT VECTOR OBTAINED FROM DECBC.
            // C  DO NOT USE IF DECB HAS SET IER .NE. 0.
            // C  OUTPUT..
            // C    BR, BI SOLUTION VECTOR, X (REAL AND IMAG. PART).
            // C-----------------------------------------------------------------------
            #endregion
            #region Body
            
            MD = ML + MU + 1;
            MD1 = MD + 1;
            MDM = MD - 1;
            NM1 = N - 1;
            if (ML == 0) goto LABEL25;
            if (N == 1) goto LABEL50;
            for (K = 1; K <= NM1; K++)
            {
                M = IP[K + o_ip];
                TR = BR[M + o_br];
                TI = BI[M + o_bi];
                BR[M + o_br] = BR[K + o_br];
                BI[M + o_bi] = BI[K + o_bi];
                BR[K + o_br] = TR;
                BI[K + o_bi] = TI;
                MDL = Math.Min(ML, N - K) + MD;
                AR_K = K * NDIM + o_ar;
                AI_K = K * NDIM + o_ai;
                for (I = MD1; I <= MDL; I++)
                {
                    IMD = I + K - MD;
                    PRODR = AR[I + AR_K] * TR - AI[I + AI_K] * TI;
                    PRODI = AI[I + AI_K] * TR + AR[I + AR_K] * TI;
                    BR[IMD + o_br] += PRODR;
                    BI[IMD + o_bi] += PRODI;
                }
            }
        LABEL25:;
            for (KB = 1; KB <= NM1; KB++)
            {
                K = N + 1 - KB;
                DEN = AR[MD+K * NDIM + o_ar] * AR[MD+K * NDIM + o_ar] + AI[MD+K * NDIM + o_ai] * AI[MD+K * NDIM + o_ai];
                PRODR = BR[K + o_br] * AR[MD+K * NDIM + o_ar] + BI[K + o_bi] * AI[MD+K * NDIM + o_ai];
                PRODI = BI[K + o_bi] * AR[MD+K * NDIM + o_ar] - BR[K + o_br] * AI[MD+K * NDIM + o_ai];
                BR[K + o_br] = PRODR / DEN;
                BI[K + o_bi] = PRODI / DEN;
                TR =  - BR[K + o_br];
                TI =  - BI[K + o_bi];
                KMD = MD - K;
                LM = Math.Max(1, KMD + 1);
                AR_K = K * NDIM + o_ar;
                AI_K = K * NDIM + o_ai;
                for (I = LM; I <= MDM; I++)
                {
                    IMD = I - KMD;
                    PRODR = AR[I + AR_K] * TR - AI[I + AI_K] * TI;
                    PRODI = AI[I + AI_K] * TR + AR[I + AR_K] * TI;
                    BR[IMD + o_br] += PRODR;
                    BI[IMD + o_bi] += PRODI;
                }
            }
            DEN = AR[MD+1 * NDIM + o_ar] * AR[MD+1 * NDIM + o_ar] + AI[MD+1 * NDIM + o_ai] * AI[MD+1 * NDIM + o_ai];
            PRODR = BR[1 + o_br] * AR[MD+1 * NDIM + o_ar] + BI[1 + o_bi] * AI[MD+1 * NDIM + o_ai];
            PRODI = BI[1 + o_bi] * AR[MD+1 * NDIM + o_ar] - BR[1 + o_br] * AI[MD+1 * NDIM + o_ai];
            BR[1 + o_br] = PRODR / DEN;
            BI[1 + o_bi] = PRODI / DEN;
        LABEL50:;
            return;
            // C----------------------- END OF SUBROUTINE SOLBC ------------------------
            #endregion
        }
    }

    #endregion


    #region The Class: ELMHES
    
    // c
    // C
    public class ELMHES
    {
    
        public ELMHES()
        {
    
        }
    
        /// <param name="NM">
        /// must be set to the row dimension of two-dimensional
        /// array parameters as declared in the calling program
        /// dimension statement;
        ///</param>
        /// <param name="N">
        /// is the order of the matrix;
        ///</param>
        /// <param name="LOW">
        /// and igh are integers determined by the balancing
        /// subroutine  balanc.      if  balanc  has not been used,
        /// set low=1, igh=n;
        ///</param>
        /// <param name="A">
        /// contains the input matrix.
        ///</param>
        /// <param name="INT">
        /// contains information on the rows and columns
        /// interchanged in the reduction.
        /// only elements low through igh are used.
        ///</param>
        public void Run(int NM, int N, int LOW, int IGH, ref double[] A, int offset_a, ref int[] INT, int offset_int)
        {
            #region Variables
            
            int I = 0; int J = 0; int M = 0; int LA = 0; int KP1 = 0; int MM1 = 0; int MP1 = 0; double X = 0; double Y = 0; 
            double DABS = 0;
            #endregion
            #region Implicit Variables
            
            int A_MM1 = 0; int A_I = 0; int A_M = 0; 
            #endregion
            #region Array Index Correction
            
             int o_a = -1 - NM + offset_a;  int o_int = -1 + offset_int; 
            #endregion
            #region Prolog
            
            // C
            // C
            // C     this subroutine is a translation of the algol procedure elmhes,
            // C     num. math. 12, 349-368(1968) by martin and wilkinson.
            // C     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971).
            // C
            // C     given a real general matrix, this subroutine
            // C     reduces a submatrix situated in rows and columns
            // C     low through igh to upper hessenberg form by
            // C     stabilized elementary similarity transformations.
            // C
            // C     on input:
            // C
            // C      nm must be set to the row dimension of two-dimensional
            // C        array parameters as declared in the calling program
            // C        dimension statement;
            // C
            // C      n is the order of the matrix;
            // C
            // C      low and igh are integers determined by the balancing
            // C        subroutine  balanc.      if  balanc  has not been used,
            // C        set low=1, igh=n;
            // C
            // C      a contains the input matrix.
            // C
            // C     on output:
            // C
            // C      a contains the hessenberg matrix.  the multipliers
            // C        which were used in the reduction are stored in the
            // C        remaining triangle under the hessenberg matrix;
            // C
            // C      int contains information on the rows and columns
            // C        interchanged in the reduction.
            // C        only elements low through igh are used.
            // C
            // C     questions and comments should be directed to b. s. garbow,
            // C     applied mathematics division, argonne national laboratory
            // C
            // C     ------------------------------------------------------------------
            // C
            #endregion
            #region Body
            
            LA = IGH - 1;
            KP1 = LOW + 1;
            if (LA < KP1) goto LABEL200;
            // C
            for (M = KP1; M <= LA; M++)
            {
                MM1 = M - 1;
                X = 0.0E0;
                I = M;
                // C
                A_MM1 = MM1 * NM + o_a;
                for (J = M; J <= IGH; J++)
                {
                    if (Math.Abs(A[J+MM1 * NM + o_a]) <= Math.Abs(X)) goto LABEL100;
                    X = A[J + A_MM1];
                    I = J;
                LABEL100:;
                }
                // C
                INT[M + o_int] = I;
                if (I == M) goto LABEL130;
                // C    :::::::::: interchange rows and columns of a ::::::::::
                for (J = MM1; J <= N; J++)
                {
                    Y = A[I+J * NM + o_a];
                    A[I+J * NM + o_a] = A[M+J * NM + o_a];
                    A[M+J * NM + o_a] = Y;
                }
                // C
                A_I = I * NM + o_a;
                A_M = M * NM + o_a;
                for (J = 1; J <= IGH; J++)
                {
                    Y = A[J + A_I];
                    A[J + A_I] = A[J + A_M];
                    A[J + A_M] = Y;
                }
                // C    :::::::::: end interchange ::::::::::
            LABEL130:  
                if (X == 0.0E0) goto LABEL180;
                MP1 = M + 1;
                // C
                A_MM1 = MM1 * NM + o_a;
                for (I = MP1; I <= IGH; I++)
                {
                    Y = A[I + A_MM1];
                    if (Y == 0.0E0) goto LABEL160;
                    Y /= X;
                    A[I + A_MM1] = Y;
                    // C
                    for (J = M; J <= N; J++)
                    {
                        A[I+J * NM + o_a] +=  - Y * A[M+J * NM + o_a];
                    }
                    // C
                    for (J = 1; J <= IGH; J++)
                    {
                        A[J+M * NM + o_a] += Y * A[J+I * NM + o_a];
                    }
                    // C
                LABEL160:;
                }
                // C
            LABEL180:;
            }
            // C
        LABEL200:  return;
            // C    :::::::::: last card of elmhes ::::::::::
            #endregion
        }
    }

    #endregion

    
}
