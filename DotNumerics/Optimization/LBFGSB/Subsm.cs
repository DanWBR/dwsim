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
    public class SUBSM
    {
    

        #region Dependencies
        
        DTRSL _dtrsl; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E0; const double ZERO = 0.0E0; 

        #endregion

        public SUBSM(DTRSL dtrsl)
        {
    

            #region Set Dependencies
            
            this._dtrsl = dtrsl; 

            #endregion

        }
    
        public SUBSM()
        {
    

            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            DAXPY daxpy = new DAXPY();
            DTRSL dtrsl = new DTRSL(ddot, daxpy);

            #endregion


            #region Set Dependencies
            
            this._dtrsl = dtrsl; 

            #endregion

        }
        /// <param name="N">
        /// is an integer variable.
        /// On entry n is the dimension of the problem.
        /// On exit n is unchanged.
        ///</param>
        /// <param name="M">
        /// is an integer variable.
        /// On entry m is the maximum number of variable metric corrections
        /// used to define the limited memory matrix.
        /// On exit m is unchanged.
        ///</param>
        /// <param name="NSUB">
        /// is an integer variable.
        /// On entry nsub is the number of free variables.
        /// On exit nsub is unchanged.
        ///</param>
        /// <param name="IND">
        /// is an integer array of dimension nsub.
        /// On entry ind specifies the coordinate indices of free variables.
        /// On exit ind is unchanged.
        ///</param>
        /// <param name="L">
        /// is a double precision array of dimension n.
        /// On entry l is the lower bound of x.
        /// On exit l is unchanged.
        ///</param>
        /// <param name="U">
        /// is a double precision array of dimension n.
        /// On entry u is the upper bound of x.
        /// On exit u is unchanged.
        ///</param>
        /// <param name="NBD">
        /// is a integer array of dimension n.
        /// On entry nbd represents the type of bounds imposed on the
        /// variables, and must be specified as follows:
        /// nbd(i)=0 if x(i) is unbounded,
        /// 1 if x(i) has only a lower bound,
        /// 2 if x(i) has both lower and upper bounds, and
        /// 3 if x(i) has only an upper bound.
        /// On exit nbd is unchanged.
        ///</param>
        /// <param name="X">
        /// is a double precision array of dimension n.
        /// On entry x specifies the Cauchy point xcp. 
        /// On exit x(i) is the minimizer of Q over the subspace of
        /// free variables. 
        ///</param>
        /// <param name="D">
        /// = -(Z'BZ)^(-1) r.
        /// 
        /// The formula for the Newton direction, given the L-BFGS matrix
        /// and the Sherman-Morrison formula, is
        /// 
        /// d = (1/theta)r + (1/theta*2) Z'WK^(-1)W'Z r.
        /// 
        /// where
        /// K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ]
        /// [L_a -R_z           theta*S'AA'S ]
        /// 
        /// Note that this procedure for computing d differs 
        /// from that described in [1]. One can show that the matrix K is
        /// equal to the matrix M^[-1]N in that paper.
        /// 
        /// n is an integer variable.
        /// On entry n is the dimension of the problem.
        /// On exit n is unchanged.
        /// 
        /// m is an integer variable.
        /// On entry m is the maximum number of variable metric corrections
        /// used to define the limited memory matrix.
        /// On exit m is unchanged.
        /// 
        /// nsub is an integer variable.
        /// On entry nsub is the number of free variables.
        /// On exit nsub is unchanged.
        /// 
        /// ind is an integer array of dimension nsub.
        /// On entry ind specifies the coordinate indices of free variables.
        /// On exit ind is unchanged.
        /// 
        /// l is a double precision array of dimension n.
        /// On entry l is the lower bound of x.
        /// On exit l is unchanged.
        /// 
        /// u is a double precision array of dimension n.
        /// On entry u is the upper bound of x.
        /// On exit u is unchanged.
        /// 
        /// nbd is a integer array of dimension n.
        /// On entry nbd represents the type of bounds imposed on the
        /// variables, and must be specified as follows:
        /// nbd(i)=0 if x(i) is unbounded,
        /// 1 if x(i) has only a lower bound,
        /// 2 if x(i) has both lower and upper bounds, and
        /// 3 if x(i) has only an upper bound.
        /// On exit nbd is unchanged.
        /// 
        /// x is a double precision array of dimension n.
        /// On entry x specifies the Cauchy point xcp. 
        /// On exit x(i) is the minimizer of Q over the subspace of
        /// free variables. 
        /// 
        /// d is a double precision array of dimension n.
        /// On entry d is the reduced gradient of Q at xcp.
        /// On exit d is the Newton direction of Q. 
        /// 
        /// ws and wy are double precision arrays;
        /// theta is a double precision variable;
        /// col is an integer variable;
        /// head is an integer variable.
        /// On entry they store the information defining the
        /// limited memory BFGS matrix:
        /// ws(n,m) stores S, a set of s-vectors;
        /// wy(n,m) stores Y, a set of y-vectors;
        /// theta is the scaling factor specifying B_0 = theta I;
        /// col is the number of variable metric corrections stored;
        /// head is the location of the 1st s- (or y-) vector in S (or Y).
        /// On exit they are unchanged.
        /// 
        /// iword is an integer variable.
        /// On entry iword is unspecified.
        /// On exit iword specifies the status of the subspace solution.
        /// iword = 0 if the solution is in the box,
        /// 1 if some bound is encountered.
        /// 
        /// wv is a double precision working array of dimension 2m.
        /// 
        /// wn is a double precision array of dimension 2m x 2m.
        /// On entry the upper triangle of wn stores the LEL^T factorization
        /// of the indefinite matrix
        /// 
        /// K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ]
        /// [L_a -R_z           theta*S'AA'S ]
        /// where E = [-I  0]
        /// [ 0  I]
        /// On exit wn is unchanged.
        /// 
        /// iprint is an INTEGER variable that must be set by the user.
        /// It controls the frequency and type of output generated:
        /// iprint.LT.0    no output is generated;
        /// iprint=0    print only one line at the last iteration;
        /// 0.LT.iprint.LT.99 print also f and |proj g| every iprint iterations;
        /// iprint=99   print details of every iteration except n-vectors;
        /// iprint=100  print also the changes of active set and final x;
        /// iprint.GT.100  print details of every iteration including x and g;
        /// When iprint .GT. 0, the file iterate.dat will be created to
        /// summarize the iteration.
        /// 
        /// info is an integer variable.
        /// On entry info is unspecified.
        /// On exit info = 0       for normal return,
        /// = nonzero for abnormal return 
        /// when the matrix K is ill-conditioned.
        /// 
        /// Subprograms called:
        /// 
        /// Linpack dtrsl.
        /// 
        /// 
        /// References:
        /// 
        /// [1] R. H. Byrd, P. Lu, J. Nocedal and C. Zhu, ``A limited
        /// memory algorithm for bound constrained optimization'',
        /// SIAM J. Scientific Computing 16 (1995), no. 5, pp. 1190--1208.
        /// 
        /// 
        /// 
        /// *  *  *
        /// 
        /// NEOS, November 1994. (Latest revision June 1996.)
        /// Optimization Technology Center.
        /// Argonne National Laboratory and Northwestern University.
        /// Written by
        /// Ciyou Zhu
        /// in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
        /// 
        /// 
        /// ************
        /// 
        /// 
        /// 
        /// 
        /// 
        ///</param>
        /// <param name="WS">
        /// and wy are double precision arrays;
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
        /// theta is the scaling factor specifying B_0 = theta I;
        /// col is the number of variable metric corrections stored;
        /// head is the location of the 1st s- (or y-) vector in S (or Y).
        /// On exit they are unchanged.
        ///</param>
        /// <param name="IWORD">
        /// is an integer variable.
        /// On entry iword is unspecified.
        /// On exit iword specifies the status of the subspace solution.
        /// iword = 0 if the solution is in the box,
        /// 1 if some bound is encountered.
        ///</param>
        /// <param name="WV">
        /// is a double precision working array of dimension 2m.
        ///</param>
        /// <param name="WN">
        /// is a double precision array of dimension 2m x 2m.
        /// On entry the upper triangle of wn stores the LEL^T factorization
        /// of the indefinite matrix
        /// 
        /// K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ]
        /// [L_a -R_z           theta*S'AA'S ]
        /// where E = [-I  0]
        /// [ 0  I]
        /// On exit wn is unchanged.
        ///</param>
        /// <param name="IPRINT">
        /// is an INTEGER variable that must be set by the user.
        /// It controls the frequency and type of output generated:
        /// iprint.LT.0    no output is generated;
        /// iprint=0    print only one line at the last iteration;
        /// 0.LT.iprint.LT.99 print also f and |proj g| every iprint iterations;
        /// iprint=99   print details of every iteration except n-vectors;
        /// iprint=100  print also the changes of active set and final x;
        /// iprint.GT.100  print details of every iteration including x and g;
        /// When iprint .GT. 0, the file iterate.dat will be created to
        /// summarize the iteration.
        ///</param>
        /// <param name="INFO">
        /// is an integer variable.
        /// On entry info is unspecified.
        /// On exit info = 0       for normal return,
        /// = nonzero for abnormal return 
        /// when the matrix K is ill-conditioned.
        ///</param>
        public void Run(int N, int M, int NSUB, int[] IND, int offset_ind, double[] L, int offset_l, double[] U, int offset_u
                         , int[] NBD, int offset_nbd, ref double[] X, int offset_x, ref double[] D, int offset_d, double[] WS, int offset_ws, double[] WY, int offset_wy, double THETA
                         , int COL, int HEAD, ref int IWORD, ref double[] WV, int offset_wv, double[] WN, int offset_wn, int IPRINT
                         , ref int INFO)
        {

            #region Variables
            
            int POINTR = 0; int M2 = 0; int COL2 = 0; int IBD = 0; int JY = 0; int JS = 0; int I = 0; int J = 0; int K = 0; 
            double ALPHA = 0;double DK = 0; double TEMP1 = 0; double TEMP2 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_ind = -1 + offset_ind;  int o_l = -1 + offset_l;  int o_u = -1 + offset_u;  int o_nbd = -1 + offset_nbd; 
             int o_x = -1 + offset_x; int o_d = -1 + offset_d;  int o_ws = -1 - N + offset_ws;  int o_wy = -1 - N + offset_wy; 
             int o_wv = -1 + offset_wv; int o_wn = -1 - (2*M) + offset_wn; 

            #endregion


            #region Prolog
            
            
            
            // c     ************
            // c
            // c     Subroutine subsm
            // c
            // c     Given xcp, l, u, r, an index set that specifies
            // c	the active set at xcp, and an l-BFGS matrix B 
            // c	(in terms of WY, WS, SY, WT, head, col, and theta), 
            // c	this subroutine computes an approximate solution
            // c	of the subspace problem
            // c
            // c     	(P)   min Q(x) = r'(x-xcp) + 1/2 (x-xcp)' B (x-xcp)
            // c
            // c             subject to l<=x<=u
            // c	  	        x_i=xcp_i for all i in A(xcp)
            // c	              
            // c	along the subspace unconstrained Newton direction 
            // c	
            // c	   d = -(Z'BZ)^(-1) r.
            // c
            // c       The formula for the Newton direction, given the L-BFGS matrix
            // c       and the Sherman-Morrison formula, is
            // c
            // c	   d = (1/theta)r + (1/theta*2) Z'WK^(-1)W'Z r.
            // c 
            // c       where
            // c                 K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ]
            // c                     [L_a -R_z           theta*S'AA'S ]
            // c
            // c     Note that this procedure for computing d differs 
            // c     from that described in [1]. One can show that the matrix K is
            // c     equal to the matrix M^[-1]N in that paper.
            // c
            // c     n is an integer variable.
            // c       On entry n is the dimension of the problem.
            // c       On exit n is unchanged.
            // c
            // c     m is an integer variable.
            // c       On entry m is the maximum number of variable metric corrections
            // c         used to define the limited memory matrix.
            // c       On exit m is unchanged.
            // c
            // c     nsub is an integer variable.
            // c       On entry nsub is the number of free variables.
            // c       On exit nsub is unchanged.
            // c
            // c     ind is an integer array of dimension nsub.
            // c       On entry ind specifies the coordinate indices of free variables.
            // c       On exit ind is unchanged.
            // c
            // c     l is a double precision array of dimension n.
            // c       On entry l is the lower bound of x.
            // c       On exit l is unchanged.
            // c
            // c     u is a double precision array of dimension n.
            // c       On entry u is the upper bound of x.
            // c       On exit u is unchanged.
            // c
            // c     nbd is a integer array of dimension n.
            // c       On entry nbd represents the type of bounds imposed on the
            // c         variables, and must be specified as follows:
            // c         nbd(i)=0 if x(i) is unbounded,
            // c                1 if x(i) has only a lower bound,
            // c                2 if x(i) has both lower and upper bounds, and
            // c                3 if x(i) has only an upper bound.
            // c       On exit nbd is unchanged.
            // c
            // c     x is a double precision array of dimension n.
            // c       On entry x specifies the Cauchy point xcp. 
            // c       On exit x(i) is the minimizer of Q over the subspace of
            // c                                                        free variables. 
            // c
            // c     d is a double precision array of dimension n.
            // c       On entry d is the reduced gradient of Q at xcp.
            // c       On exit d is the Newton direction of Q. 
            // c
            // c     ws and wy are double precision arrays;
            // c     theta is a double precision variable;
            // c     col is an integer variable;
            // c     head is an integer variable.
            // c       On entry they store the information defining the
            // c                                          limited memory BFGS matrix:
            // c         ws(n,m) stores S, a set of s-vectors;
            // c         wy(n,m) stores Y, a set of y-vectors;
            // c         theta is the scaling factor specifying B_0 = theta I;
            // c         col is the number of variable metric corrections stored;
            // c         head is the location of the 1st s- (or y-) vector in S (or Y).
            // c       On exit they are unchanged.
            // c
            // c     iword is an integer variable.
            // c       On entry iword is unspecified.
            // c       On exit iword specifies the status of the subspace solution.
            // c         iword = 0 if the solution is in the box,
            // c                 1 if some bound is encountered.
            // c
            // c     wv is a double precision working array of dimension 2m.
            // c
            // c     wn is a double precision array of dimension 2m x 2m.
            // c       On entry the upper triangle of wn stores the LEL^T factorization
            // c         of the indefinite matrix
            // c
            // c              K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ]
            // c                  [L_a -R_z           theta*S'AA'S ]
            // c                                                    where E = [-I  0]
            // c                                                              [ 0  I]
            // c       On exit wn is unchanged.
            // c
            // c     iprint is an INTEGER variable that must be set by the user.
            // c       It controls the frequency and type of output generated:
            // c        iprint<0    no output is generated;
            // c        iprint=0    print only one line at the last iteration;
            // c        0<iprint<99 print also f and |proj g| every iprint iterations;
            // c        iprint=99   print details of every iteration except n-vectors;
            // c        iprint=100  print also the changes of active set and final x;
            // c        iprint>100  print details of every iteration including x and g;
            // c       When iprint > 0, the file iterate.dat will be created to
            // c                        summarize the iteration.
            // c
            // c     info is an integer variable.
            // c       On entry info is unspecified.
            // c       On exit info = 0       for normal return,
            // c                    = nonzero for abnormal return 
            // c                                  when the matrix K is ill-conditioned.
            // c
            // c     Subprograms called:
            // c
            // c       Linpack dtrsl.
            // c
            // c
            // c     References:
            // c
            // c       [1] R. H. Byrd, P. Lu, J. Nocedal and C. Zhu, ``A limited
            // c       memory algorithm for bound constrained optimization'',
            // c       SIAM J. Scientific Computing 16 (1995), no. 5, pp. 1190--1208.
            // c
            // c
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
            
            
            
            
            
            

            #endregion


            #region Body
            
            if (NSUB <= 0) return;
            if (IPRINT >= 99) ;//ERROR-ERRORWRITE(6,1001)
            
            // c     Compute wv = W'Zd.
            
            POINTR = HEAD;
            for (I = 1; I <= COL; I++)
            {
                TEMP1 = ZERO;
                TEMP2 = ZERO;
                for (J = 1; J <= NSUB; J++)
                {
                    K = IND[J + o_ind];
                    TEMP1 += WY[K+POINTR * N + o_wy] * D[J + o_d];
                    TEMP2 += WS[K+POINTR * N + o_ws] * D[J + o_d];
                }
                WV[I + o_wv] = TEMP1;
                WV[COL + I + o_wv] = THETA * TEMP2;
                POINTR = FortranLib.Mod(POINTR,M) + 1;
            }
            
            // c     Compute wv:=K^(-1)wv.
            
            M2 = 2 * M;
            COL2 = 2 * COL;
            this._dtrsl.Run(WN, offset_wn, M2, COL2, ref WV, offset_wv, 11, ref INFO);
            if (INFO != 0) return;
            for (I = 1; I <= COL; I++)
            {
                WV[I + o_wv] =  - WV[I + o_wv];
            }
            this._dtrsl.Run(WN, offset_wn, M2, COL2, ref WV, offset_wv, 01, ref INFO);
            if (INFO != 0) return;
            
            // c     Compute d = (1/theta)d + (1/theta**2)Z'W wv.
            
            POINTR = HEAD;
            for (JY = 1; JY <= COL; JY++)
            {
                JS = COL + JY;
                for (I = 1; I <= NSUB; I++)
                {
                    K = IND[I + o_ind];
                    D[I + o_d] += WY[K+POINTR * N + o_wy] * WV[JY + o_wv] / THETA + WS[K+POINTR * N + o_ws] * WV[JS + o_wv];
                }
                POINTR = FortranLib.Mod(POINTR,M) + 1;
            }
            for (I = 1; I <= NSUB; I++)
            {
                D[I + o_d] /= THETA;
            }
            
            // c     Backtrack to the feasible region.
            
            ALPHA = ONE;
            TEMP1 = ALPHA;
            for (I = 1; I <= NSUB; I++)
            {
                K = IND[I + o_ind];
                DK = D[I + o_d];
                if (NBD[K + o_nbd] != 0)
                {
                    if (DK < ZERO && NBD[K + o_nbd] <= 2)
                    {
                        TEMP2 = L[K + o_l] - X[K + o_x];
                        if (TEMP2 >= ZERO)
                        {
                            TEMP1 = ZERO;
                        }
                        else
                        {
                            if (DK * ALPHA < TEMP2)
                            {
                                TEMP1 = TEMP2 / DK;
                            }
                        }
                    }
                    else
                    {
                        if (DK > ZERO && NBD[K + o_nbd] >= 2)
                        {
                            TEMP2 = U[K + o_u] - X[K + o_x];
                            if (TEMP2 <= ZERO)
                            {
                                TEMP1 = ZERO;
                            }
                            else
                            {
                                if (DK * ALPHA > TEMP2)
                                {
                                    TEMP1 = TEMP2 / DK;
                                }
                            }
                        }
                    }
                    if (TEMP1 < ALPHA)
                    {
                        ALPHA = TEMP1;
                        IBD = I;
                    }
                }
            }
            
            if (ALPHA < ONE)
            {
                DK = D[IBD + o_d];
                K = IND[IBD + o_ind];
                if (DK > ZERO)
                {
                    X[K + o_x] = U[K + o_u];
                    D[IBD + o_d] = ZERO;
                }
                else
                {
                    if (DK < ZERO)
                    {
                        X[K + o_x] = L[K + o_l];
                        D[IBD + o_d] = ZERO;
                    }
                }
            }
            for (I = 1; I <= NSUB; I++)
            {
                K = IND[I + o_ind];
                X[K + o_x] += ALPHA * D[I + o_d];
            }
            
            if (IPRINT >= 99)
            {
                if (ALPHA < ONE)
                {
                    //ERROR-ERROR            WRITE (6,1002) ALPHA;
                }
                else
                {
                    //ERROR-ERROR            WRITE (6,*) 'SM solution inside the box';
                }
                if (IPRINT > 100) ;//ERROR-ERRORWRITE(6,1003)(X(I),I=1,N)
            }
            
            if (ALPHA < ONE)
            {
                IWORD = 1;
            }
            else
            {
                IWORD = 0;
            }
            if (IPRINT >= 99) ;//ERROR-ERRORWRITE(6,1004)
            
            
            return;
            

            #endregion

        }
    }
    
    // c====================== The end of subsm ===============================
}
