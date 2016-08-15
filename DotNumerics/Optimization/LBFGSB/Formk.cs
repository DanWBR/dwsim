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
    public class FORMK
    {
    

        #region Dependencies
        
        DCOPY _dcopy; DPOFA _dpofa; DTRSL _dtrsl; DDOT _ddot; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E0; const double ZERO = 0.0E0; 

        #endregion

        public FORMK(DCOPY dcopy, DPOFA dpofa, DTRSL dtrsl, DDOT ddot)
        {
    

            #region Set Dependencies
            
            this._dcopy = dcopy; this._dpofa = dpofa; this._dtrsl = dtrsl; this._ddot = ddot; 

            #endregion

        }
    
        public FORMK()
        {
    

            #region Dependencies (Initialization)
            
            DCOPY dcopy = new DCOPY();
            DDOT ddot = new DDOT();
            DAXPY daxpy = new DAXPY();
            DPOFA dpofa = new DPOFA(ddot);
            DTRSL dtrsl = new DTRSL(ddot, daxpy);

            #endregion


            #region Set Dependencies
            
            this._dcopy = dcopy; this._dpofa = dpofa; this._dtrsl = dtrsl; this._ddot = ddot; 

            #endregion

        }
        /// <param name="N">
        /// is an integer variable.
        /// On entry n is the dimension of the problem.
        /// On exit n is unchanged.
        ///</param>
        /// <param name="NSUB">
        /// is an integer variable
        /// On entry nsub is the number of subspace variables in free set.
        /// On exit nsub is not changed.
        ///</param>
        /// <param name="IND">
        /// is an integer array of dimension nsub.
        /// On entry ind specifies the indices of subspace variables.
        /// On exit ind is unchanged. 
        ///</param>
        /// <param name="NENTER">
        /// is an integer variable.
        /// On entry nenter is the number of variables entering the 
        /// free set.
        /// On exit nenter is unchanged. 
        ///</param>
        /// <param name="ILEAVE">
        /// is an integer variable.
        /// On entry indx2(ileave),...,indx2(n) are the variables leaving
        /// the free set.
        /// On exit ileave is unchanged. 
        ///</param>
        /// <param name="INDX2">
        /// is an integer array of dimension n.
        /// On entry indx2(1),...,indx2(nenter) are the variables entering
        /// the free set, while indx2(ileave),...,indx2(n) are the
        /// variables leaving the free set.
        /// On exit indx2 is unchanged. 
        ///</param>
        /// <param name="IUPDAT">
        /// is an integer variable.
        /// On entry iupdat is the total number of BFGS updates made so far.
        /// On exit iupdat is unchanged. 
        ///</param>
        /// <param name="UPDATD">
        /// is a logical variable.
        /// On entry 'updatd' is true if the L-BFGS matrix is updatd.
        /// On exit 'updatd' is unchanged. 
        ///</param>
        /// <param name="WN">
        /// is a double precision array of dimension 2m x 2m.
        /// On entry wn is unspecified.
        /// On exit the upper triangle of wn stores the LEL^T factorization
        /// of the 2*col x 2*col indefinite matrix
        /// [-D -Y'ZZ'Y/theta     L_a'-R_z'  ]
        /// [L_a -R_z           theta*S'AA'S ]
        ///</param>
        /// <param name="WN1">
        /// is a double precision array of dimension 2m x 2m.
        /// On entry wn1 stores the lower triangular part of 
        /// [Y' ZZ'Y   L_a'+R_z']
        /// [L_a+R_z   S'AA'S   ]
        /// in the previous iteration.
        /// On exit wn1 stores the corresponding updated matrices.
        /// The purpose of wn1 is just to store these inner products
        /// so they can be easily updated and inserted into wn.
        ///</param>
        /// <param name="M">
        /// is an integer variable.
        /// On entry m is the maximum number of variable metric corrections
        /// used to define the limited memory matrix.
        /// On exit m is unchanged.
        ///</param>
        /// <param name="THETA">
        /// is a double precision variable;
        ///</param>
        /// <param name="COL">
        /// is an integer variable;
        ///</param>
        /// <param name="HEAD">
        /// is an integer variable.
        /// On entry they store the information defining the
        /// limited memory BFGS matrix:
        /// ws(n,m) stores S, a set of s-vectors;
        /// wy(n,m) stores Y, a set of y-vectors;
        /// sy(m,m) stores S'Y;
        /// wtyy(m,m) stores the Cholesky factorization
        /// of (theta*S'S+LD^(-1)L')
        /// theta is the scaling factor specifying B_0 = theta I;
        /// col is the number of variable metric corrections stored;
        /// head is the location of the 1st s- (or y-) vector in S (or Y).
        /// On exit they are unchanged.
        ///</param>
        /// <param name="INFO">
        /// is an integer variable.
        /// On entry info is unspecified.
        /// On exit info =  0 for normal return;
        /// = -1 when the 1st Cholesky factorization failed;
        /// = -2 when the 2st Cholesky factorization failed.
        ///</param>
        public void Run(int N, int NSUB, int[] IND, int offset_ind, int NENTER, int ILEAVE, int[] INDX2, int offset_indx2
                         , int IUPDAT, bool UPDATD, ref double[] WN, int offset_wn, ref double[] WN1, int offset_wn1, int M, double[] WS, int offset_ws
                         , double[] WY, int offset_wy, double[] SY, int offset_sy, double THETA, int COL, int HEAD, ref int INFO)
        {

            #region Variables
            
            int M2 = 0; int IPNTR = 0; int JPNTR = 0; int IY = 0; int IS = 0; int JY = 0; int JS = 0; int IS1 = 0; int JS1 = 0; 
            int K1 = 0;int I = 0; int K = 0; int COL2 = 0; int PBEGIN = 0; int PEND = 0; int DBEGIN = 0; int DEND = 0; 
            int UPCL = 0;double TEMP1 = 0; double TEMP2 = 0; double TEMP3 = 0; double TEMP4 = 0; 

            #endregion


            #region Implicit Variables
            
            int WN_IY = 0; int WN_IS = 0; 

            #endregion


            #region Array Index Correction
            
             int o_ind = -1 + offset_ind;  int o_indx2 = -1 + offset_indx2;  int o_wn = -1 - (2*M) + offset_wn; 
             int o_wn1 = -1 - (2*M) + offset_wn1; int o_ws = -1 - N + offset_ws;  int o_wy = -1 - N + offset_wy; 
             int o_sy = -1 - M + offset_sy;

            #endregion


            #region Prolog
            
            
            
            // c     ************
            // c
            // c     Subroutine formk 
            // c
            // c     This subroutine forms  the LEL^T factorization of the indefinite
            // c
            // c       matrix    K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ]
            // c                     [L_a -R_z           theta*S'AA'S ]
            // c                                                    where E = [-I  0]
            // c                                                              [ 0  I]
            // c     The matrix K can be shown to be equal to the matrix M^[-1]N
            // c       occurring in section 5.1 of [1], as well as to the matrix
            // c       Mbar^[-1] Nbar in section 5.3.
            // c
            // c     n is an integer variable.
            // c       On entry n is the dimension of the problem.
            // c       On exit n is unchanged.
            // c
            // c     nsub is an integer variable
            // c       On entry nsub is the number of subspace variables in free set.
            // c       On exit nsub is not changed.
            // c
            // c     ind is an integer array of dimension nsub.
            // c       On entry ind specifies the indices of subspace variables.
            // c       On exit ind is unchanged. 
            // c
            // c     nenter is an integer variable.
            // c       On entry nenter is the number of variables entering the 
            // c         free set.
            // c       On exit nenter is unchanged. 
            // c
            // c     ileave is an integer variable.
            // c       On entry indx2(ileave),...,indx2(n) are the variables leaving
            // c         the free set.
            // c       On exit ileave is unchanged. 
            // c
            // c     indx2 is an integer array of dimension n.
            // c       On entry indx2(1),...,indx2(nenter) are the variables entering
            // c         the free set, while indx2(ileave),...,indx2(n) are the
            // c         variables leaving the free set.
            // c       On exit indx2 is unchanged. 
            // c
            // c     iupdat is an integer variable.
            // c       On entry iupdat is the total number of BFGS updates made so far.
            // c       On exit iupdat is unchanged. 
            // c
            // c     updatd is a logical variable.
            // c       On entry 'updatd' is true if the L-BFGS matrix is updatd.
            // c       On exit 'updatd' is unchanged. 
            // c
            // c     wn is a double precision array of dimension 2m x 2m.
            // c       On entry wn is unspecified.
            // c       On exit the upper triangle of wn stores the LEL^T factorization
            // c         of the 2*col x 2*col indefinite matrix
            // c                     [-D -Y'ZZ'Y/theta     L_a'-R_z'  ]
            // c                     [L_a -R_z           theta*S'AA'S ]
            // c
            // c     wn1 is a double precision array of dimension 2m x 2m.
            // c       On entry wn1 stores the lower triangular part of 
            // c                     [Y' ZZ'Y   L_a'+R_z']
            // c                     [L_a+R_z   S'AA'S   ]
            // c         in the previous iteration.
            // c       On exit wn1 stores the corresponding updated matrices.
            // c       The purpose of wn1 is just to store these inner products
            // c       so they can be easily updated and inserted into wn.
            // c
            // c     m is an integer variable.
            // c       On entry m is the maximum number of variable metric corrections
            // c         used to define the limited memory matrix.
            // c       On exit m is unchanged.
            // c
            // c     ws, wy, sy, and wtyy are double precision arrays;
            // c     theta is a double precision variable;
            // c     col is an integer variable;
            // c     head is an integer variable.
            // c       On entry they store the information defining the
            // c                                          limited memory BFGS matrix:
            // c         ws(n,m) stores S, a set of s-vectors;
            // c         wy(n,m) stores Y, a set of y-vectors;
            // c         sy(m,m) stores S'Y;
            // c         wtyy(m,m) stores the Cholesky factorization
            // c                                   of (theta*S'S+LD^(-1)L')
            // c         theta is the scaling factor specifying B_0 = theta I;
            // c         col is the number of variable metric corrections stored;
            // c         head is the location of the 1st s- (or y-) vector in S (or Y).
            // c       On exit they are unchanged.
            // c
            // c     info is an integer variable.
            // c       On entry info is unspecified.
            // c       On exit info =  0 for normal return;
            // c                    = -1 when the 1st Cholesky factorization failed;
            // c                    = -2 when the 2st Cholesky factorization failed.
            // c
            // c     Subprograms called:
            // c
            // c       Linpack ... dcopy, dpofa, dtrsl.
            // c
            // c
            // c     References:
            // c       [1] R. H. Byrd, P. Lu, J. Nocedal and C. Zhu, ``A limited
            // c       memory algorithm for bound constrained optimization'',
            // c       SIAM J. Scientific Computing 16 (1995), no. 5, pp. 1190--1208.
            // c
            // c       [2] C. Zhu, R.H. Byrd, P. Lu, J. Nocedal, ``L-BFGS-B: a
            // c       limited memory FORTRAN code for solving bound constrained
            // c       optimization problems'', Tech. Report, NAM-11, EECS Department,
            // c       Northwestern University, 1994.
            // c
            // c       (Postscript files of these papers are available via anonymous
            // c        ftp to eecs.nwu.edu in the directory pub/lbfgs/lbfgs_bcm.)
            // c
            // c                           *  *  *
            // c
            // c     NEOS, November 1994. (Latest revision June 1996.)
            // c     Optimization Technology Center.
            // c     Argonne National Laboratory and Northwestern University.
            // c     Written by
            // c                        Ciyou Zhu
            // c     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
            // c
            // c
            // c     ************
            
            
            
            
            // c     Form the lower triangular part of
            // c               WN1 = [Y' ZZ'Y   L_a'+R_z'] 
            // c                     [L_a+R_z   S'AA'S   ]
            // c        where L_a is the strictly lower triangular part of S'AA'Y
            // c              R_z is the upper triangular part of S'ZZ'Y.
            

            #endregion


            #region Body
            
            if (UPDATD)
            {
                if (IUPDAT > M)
                {
                    // c                                 shift old part of WN1.
                    for (JY = 1; JY <= M - 1; JY++)
                    {
                        JS = M + JY;
                        this._dcopy.Run(M - JY, WN1, JY + 1+(JY + 1) * (2*M) + o_wn1, 1, ref WN1, JY+JY * (2*M) + o_wn1, 1);
                        this._dcopy.Run(M - JY, WN1, JS + 1+(JS + 1) * (2*M) + o_wn1, 1, ref WN1, JS+JS * (2*M) + o_wn1, 1);
                        this._dcopy.Run(M - 1, WN1, M + 2+(JY + 1) * (2*M) + o_wn1, 1, ref WN1, M + 1+JY * (2*M) + o_wn1, 1);
                    }
                }
                
                // c          put new rows in blocks (1,1), (2,1) and (2,2).
                PBEGIN = 1;
                PEND = NSUB;
                DBEGIN = NSUB + 1;
                DEND = N;
                IY = COL;
                IS = M + COL;
                IPNTR = HEAD + COL - 1;
                if (IPNTR > M) IPNTR -= M;
                JPNTR = HEAD;
                for (JY = 1; JY <= COL; JY++)
                {
                    JS = M + JY;
                    TEMP1 = ZERO;
                    TEMP2 = ZERO;
                    TEMP3 = ZERO;
                    // c             compute element jy of row 'col' of Y'ZZ'Y
                    for (K = PBEGIN; K <= PEND; K++)
                    {
                        K1 = IND[K + o_ind];
                        TEMP1 += WY[K1+IPNTR * N + o_wy] * WY[K1+JPNTR * N + o_wy];
                    }
                    // c             compute elements jy of row 'col' of L_a and S'AA'S
                    for (K = DBEGIN; K <= DEND; K++)
                    {
                        K1 = IND[K + o_ind];
                        TEMP2 += WS[K1+IPNTR * N + o_ws] * WS[K1+JPNTR * N + o_ws];
                        TEMP3 += WS[K1+IPNTR * N + o_ws] * WY[K1+JPNTR * N + o_wy];
                    }
                    WN1[IY+JY * (2*M) + o_wn1] = TEMP1;
                    WN1[IS+JS * (2*M) + o_wn1] = TEMP2;
                    WN1[IS+JY * (2*M) + o_wn1] = TEMP3;
                    JPNTR = FortranLib.Mod(JPNTR,M) + 1;
                }
                
                // c          put new column in block (2,1).
                JY = COL;
                JPNTR = HEAD + COL - 1;
                if (JPNTR > M) JPNTR -= M;
                IPNTR = HEAD;
                for (I = 1; I <= COL; I++)
                {
                    IS = M + I;
                    TEMP3 = ZERO;
                    // c             compute element i of column 'col' of R_z
                    for (K = PBEGIN; K <= PEND; K++)
                    {
                        K1 = IND[K + o_ind];
                        TEMP3 += WS[K1+IPNTR * N + o_ws] * WY[K1+JPNTR * N + o_wy];
                    }
                    IPNTR = FortranLib.Mod(IPNTR,M) + 1;
                    WN1[IS+JY * (2*M) + o_wn1] = TEMP3;
                }
                UPCL = COL - 1;
            }
            else
            {
                UPCL = COL;
            }
            
            // c       modify the old parts in blocks (1,1) and (2,2) due to changes
            // c       in the set of free variables.
            IPNTR = HEAD;
            for (IY = 1; IY <= UPCL; IY++)
            {
                IS = M + IY;
                JPNTR = HEAD;
                for (JY = 1; JY <= IY; JY++)
                {
                    JS = M + JY;
                    TEMP1 = ZERO;
                    TEMP2 = ZERO;
                    TEMP3 = ZERO;
                    TEMP4 = ZERO;
                    for (K = 1; K <= NENTER; K++)
                    {
                        K1 = INDX2[K + o_indx2];
                        TEMP1 += WY[K1+IPNTR * N + o_wy] * WY[K1+JPNTR * N + o_wy];
                        TEMP2 += WS[K1+IPNTR * N + o_ws] * WS[K1+JPNTR * N + o_ws];
                    }
                    for (K = ILEAVE; K <= N; K++)
                    {
                        K1 = INDX2[K + o_indx2];
                        TEMP3 += WY[K1+IPNTR * N + o_wy] * WY[K1+JPNTR * N + o_wy];
                        TEMP4 += WS[K1+IPNTR * N + o_ws] * WS[K1+JPNTR * N + o_ws];
                    }
                    WN1[IY+JY * (2*M) + o_wn1] += TEMP1 - TEMP3;
                    WN1[IS+JS * (2*M) + o_wn1] +=  - TEMP2 + TEMP4;
                    JPNTR = FortranLib.Mod(JPNTR,M) + 1;
                }
                IPNTR = FortranLib.Mod(IPNTR,M) + 1;
            }
            
            // c       modify the old parts in block (2,1).
            IPNTR = HEAD;
            for (IS = M + 1; IS <= M + UPCL; IS++)
            {
                JPNTR = HEAD;
                for (JY = 1; JY <= UPCL; JY++)
                {
                    TEMP1 = ZERO;
                    TEMP3 = ZERO;
                    for (K = 1; K <= NENTER; K++)
                    {
                        K1 = INDX2[K + o_indx2];
                        TEMP1 += WS[K1+IPNTR * N + o_ws] * WY[K1+JPNTR * N + o_wy];
                    }
                    for (K = ILEAVE; K <= N; K++)
                    {
                        K1 = INDX2[K + o_indx2];
                        TEMP3 += WS[K1+IPNTR * N + o_ws] * WY[K1+JPNTR * N + o_wy];
                    }
                    if (IS <= JY + M)
                    {
                        WN1[IS+JY * (2*M) + o_wn1] += TEMP1 - TEMP3;
                    }
                    else
                    {
                        WN1[IS+JY * (2*M) + o_wn1] +=  - TEMP1 + TEMP3;
                    }
                    JPNTR = FortranLib.Mod(JPNTR,M) + 1;
                }
                IPNTR = FortranLib.Mod(IPNTR,M) + 1;
            }
            
            // c     Form the upper triangle of WN = [D+Y' ZZ'Y/theta   -L_a'+R_z' ] 
            // c                                     [-L_a +R_z        S'AA'S*theta]
            
            M2 = 2 * M;
            for (IY = 1; IY <= COL; IY++)
            {
                IS = COL + IY;
                IS1 = M + IY;
                WN_IY = IY * (2*M) + o_wn;
                for (JY = 1; JY <= IY; JY++)
                {
                    JS = COL + JY;
                    JS1 = M + JY;
                    WN[JY + WN_IY] = WN1[IY+JY * (2*M) + o_wn1] / THETA;
                    WN[JS+IS * (2*M) + o_wn] = WN1[IS1+JS1 * (2*M) + o_wn1] * THETA;
                }
                WN_IS = IS * (2*M) + o_wn;
                for (JY = 1; JY <= IY - 1; JY++)
                {
                    WN[JY + WN_IS] =  - WN1[IS1+JY * (2*M) + o_wn1];
                }
                WN_IS = IS * (2*M) + o_wn;
                for (JY = IY; JY <= COL; JY++)
                {
                    WN[JY + WN_IS] = WN1[IS1+JY * (2*M) + o_wn1];
                }
                WN[IY+IY * (2*M) + o_wn] += SY[IY+IY * M + o_sy];
            }
            
            // c     Form the upper triangle of WN= [  LL'            L^-1(-L_a'+R_z')] 
            // c                                    [(-L_a +R_z)L'^-1   S'AA'S*theta  ]
            
            // c        first Cholesky factor (1,1) block of wn to get LL'
            // c                          with L' stored in the upper triangle of wn.
            this._dpofa.Run(ref WN, offset_wn, M2, COL, ref INFO);
            if (INFO != 0)
            {
                INFO =  - 1;
                return;
            }
            // c        then form L^-1(-L_a'+R_z') in the (1,2) block.
            COL2 = 2 * COL;
            for (JS = COL + 1; JS <= COL2; JS++)
            {
                this._dtrsl.Run(WN, offset_wn, M2, COL, ref WN, 1+JS * (2*M) + o_wn, 11, ref INFO);
            }
            
            // c     Form S'AA'S*theta + (L^-1(-L_a'+R_z'))'L^-1(-L_a'+R_z') in the
            // c        upper triangle of (2,2) block of wn.
            
            
            for (IS = COL + 1; IS <= COL2; IS++)
            {
                for (JS = IS; JS <= COL2; JS++)
                {
                    WN[IS+JS * (2*M) + o_wn] += this._ddot.Run(COL, WN, 1+IS * (2*M) + o_wn, 1, WN, 1+JS * (2*M) + o_wn, 1);
                }
            }
            
            // c     Cholesky factorization of (2,2) block of wn.
            
            this._dpofa.Run(ref WN, COL + 1+(COL + 1) * (2*M) + o_wn, M2, COL, ref INFO);
            if (INFO != 0)
            {
                INFO =  - 2;
                return;
            }
            
            return;
            

            #endregion

        }
    }
    
    // c======================= The end of formk ==============================
}
