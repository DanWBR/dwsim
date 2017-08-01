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
    public class CAUCHY
    {
    

        #region Dependencies
        
        HPSOLB _hpsolb; BMV _bmv; DSCAL _dscal; DCOPY _dcopy; DAXPY _daxpy; DDOT _ddot; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E0; const double ZERO = 0.0E0; 

        #endregion

        public CAUCHY(HPSOLB hpsolb, BMV bmv, DSCAL dscal, DCOPY dcopy, DAXPY daxpy, DDOT ddot)
        {
    

            #region Set Dependencies
            
            this._hpsolb = hpsolb; this._bmv = bmv; this._dscal = dscal; this._dcopy = dcopy; this._daxpy = daxpy; 
            this._ddot = ddot;

            #endregion

        }
    
        public CAUCHY()
        {
    

            #region Dependencies (Initialization)
            
            HPSOLB hpsolb = new HPSOLB();
            DDOT ddot = new DDOT();
            DAXPY daxpy = new DAXPY();
            DSCAL dscal = new DSCAL();
            DCOPY dcopy = new DCOPY();
            DTRSL dtrsl = new DTRSL(ddot, daxpy);
            BMV bmv = new BMV(dtrsl);

            #endregion


            #region Set Dependencies
            
            this._hpsolb = hpsolb; this._bmv = bmv; this._dscal = dscal; this._dcopy = dcopy; this._daxpy = daxpy; 
            this._ddot = ddot;

            #endregion

        }
        /// <param name="N">
        /// is an integer variable.
        /// On entry n is the dimension of the problem.
        /// On exit n is unchanged.
        ///</param>
        /// <param name="X">
        /// is a double precision array of dimension n.
        /// On entry x is the starting point for the GCP computation.
        /// On exit x is unchanged.
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
        /// is an integer array of dimension n.
        /// On entry nbd represents the type of bounds imposed on the
        /// variables, and must be specified as follows:
        /// nbd(i)=0 if x(i) is unbounded,
        /// 1 if x(i) has only a lower bound,
        /// 2 if x(i) has both lower and upper bounds, and
        /// 3 if x(i) has only an upper bound. 
        /// On exit nbd is unchanged.
        ///</param>
        /// <param name="G">
        /// is a double precision array of dimension n.
        /// On entry g is the gradient of f(x).  g must be a nonzero vector.
        /// On exit g is unchanged.
        ///</param>
        /// <param name="IORDER">
        /// is an integer working array of dimension n.
        /// iorder will be used to store the breakpoints in the piecewise
        /// linear path and free variables encountered. On exit,
        /// iorder(1),...,iorder(nleft) are indices of breakpoints
        /// which have not been encountered; 
        /// iorder(nleft+1),...,iorder(nbreak) are indices of
        /// encountered breakpoints; and
        /// iorder(nfree),...,iorder(n) are indices of variables which
        /// have no bound constraits along the search direction.
        ///</param>
        /// <param name="IWHERE">
        /// is an integer array of dimension n.
        /// On entry iwhere indicates only the permanently fixed (iwhere=3)
        /// or free (iwhere= -1) components of x.
        /// On exit iwhere records the status of the current x variables.
        /// iwhere(i)=-3  if x(i) is free and has bounds, but is not moved
        /// 0   if x(i) is free and has bounds, and is moved
        /// 1   if x(i) is fixed at l(i), and l(i) .ne. u(i)
        /// 2   if x(i) is fixed at u(i), and u(i) .ne. l(i)
        /// 3   if x(i) is always fixed, i.e.,  u(i)=x(i)=l(i)
        /// -1  if x(i) is always free, i.e., it has no bounds.
        ///</param>
        /// <param name="T">
        /// is a double precision working array of dimension n. 
        /// t will be used to store the break points.
        ///</param>
        /// <param name="D">
        /// is a double precision array of dimension n used to store
        /// the Cauchy direction P(x-tg)-x.
        ///</param>
        /// <param name="XCP">
        /// is a double precision array of dimension n used to return the
        /// GCP on exit.
        ///</param>
        /// <param name="M">
        /// is an integer variable.
        /// On entry m is the maximum number of variable metric corrections 
        /// used to define the limited memory matrix.
        /// On exit m is unchanged.
        ///</param>
        /// <param name="THETA">
        /// is a double precision variable.
        /// On entry theta is the scaling factor specifying B_0 = theta I.
        /// On exit theta is unchanged.
        ///</param>
        /// <param name="COL">
        /// is an integer variable.
        /// On entry col is the actual number of variable metric
        /// corrections stored so far.
        /// On exit col is unchanged.
        ///</param>
        /// <param name="HEAD">
        /// is an integer variable.
        /// On entry head is the location of the first s-vector (or y-vector)
        /// in S (or Y).
        /// On exit col is unchanged.
        ///</param>
        /// <param name="P">
        /// is a double precision working array of dimension 2m.
        /// p will be used to store the vector p = W^(T)d.
        ///</param>
        /// <param name="C">
        /// is a double precision working array of dimension 2m.
        /// c will be used to store the vector c = W^(T)(xcp-x).
        ///</param>
        /// <param name="WBP">
        /// is a double precision working array of dimension 2m.
        /// wbp will be used to store the row of W corresponding
        /// to a breakpoint.
        ///</param>
        /// <param name="V">
        /// is a double precision working array of dimension 2m.
        ///</param>
        /// <param name="NINT">
        /// is an integer variable.
        /// On exit nint records the number of quadratic segments explored
        /// in searching for the GCP.
        ///</param>
        /// <param name="SG">
        /// and yg are double precision arrays of dimension m.
        /// On entry sg  and yg store S'g and Y'g correspondingly.
        /// On exit they are unchanged. 
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
        /// <param name="SBGNRM">
        /// is a double precision variable.
        /// On entry sbgnrm is the norm of the projected gradient at x.
        /// On exit sbgnrm is unchanged.
        ///</param>
        /// <param name="INFO">
        /// is an integer variable.
        /// On entry info is 0.
        /// On exit info = 0       for normal return,
        /// = nonzero for abnormal return when the the system
        /// used in routine bmv is singular.
        ///</param>
        public void Run(int N, double[] X, int offset_x, double[] L, int offset_l, double[] U, int offset_u, int[] NBD, int offset_nbd, double[] G, int offset_g
                         , ref int[] IORDER, int offset_iorder, ref int[] IWHERE, int offset_iwhere, ref double[] T, int offset_t, ref double[] D, int offset_d, ref double[] XCP, int offset_xcp, int M
                         , double[] WY, int offset_wy, double[] WS, int offset_ws, double[] SY, int offset_sy, double[] WT, int offset_wt, double THETA, int COL
                         , int HEAD, ref double[] P, int offset_p, ref double[] C, int offset_c, ref double[] WBP, int offset_wbp, ref double[] V, int offset_v, ref int NINT
                         , double[] SG, int offset_sg, double[] YG, int offset_yg, int IPRINT, double SBGNRM, ref int INFO, double EPSMCH)
        {

            #region Variables
            
            bool XLOWER = false; bool XUPPER = false; bool BNDED = false; int I = 0; int J = 0; int COL2 = 0; int NFREE = 0; 
            int NBREAK = 0;int POINTR = 0; int IBP = 0; int NLEFT = 0; int IBKMIN = 0; int ITER = 0; double F1 = 0; double F2 = 0; 
            double DT = 0;double DTM = 0; double TSUM = 0; double DIBP = 0; double ZIBP = 0; double DIBP2 = 0; double BKMIN = 0; 
            double TU = 0;double TL = 0; double WMC = 0; double WMP = 0; double WMW = 0; double TJ = 0; double TJ0 = 0; 
            double NEGGI = 0;double F2_ORG = 0; 

            #endregion


            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_l = -1 + offset_l;  int o_u = -1 + offset_u;  int o_nbd = -1 + offset_nbd; 
             int o_g = -1 + offset_g; int o_iorder = -1 + offset_iorder;  int o_iwhere = -1 + offset_iwhere; 
             int o_t = -1 + offset_t; int o_d = -1 + offset_d;  int o_xcp = -1 + offset_xcp;  int o_wy = -1 - N + offset_wy; 
             int o_ws = -1 - N + offset_ws; int o_sy = -1 - M + offset_sy;  int o_wt = -1 - M + offset_wt; 
             int o_p = -1 + offset_p; int o_c = -1 + offset_c;  int o_wbp = -1 + offset_wbp;  int o_v = -1 + offset_v; 
             int o_sg = -1 + offset_sg; int o_yg = -1 + offset_yg; 

            #endregion


            #region Prolog
            
            
            
            // c     ************
            // c
            // c     Subroutine cauchy
            // c
            // c     For given x, l, u, g (with sbgnrm > 0), and a limited memory
            // c       BFGS matrix B defined in terms of matrices WY, WS, WT, and
            // c       scalars head, col, and theta, this subroutine computes the
            // c       generalized Cauchy point (GCP), defined as the first local
            // c       minimizer of the quadratic
            // c
            // c                  Q(x + s) = g's + 1/2 s'Bs
            // c
            // c       along the projected gradient direction P(x-tg,l,u).
            // c       The routine returns the GCP in xcp. 
            // c       
            // c     n is an integer variable.
            // c       On entry n is the dimension of the problem.
            // c       On exit n is unchanged.
            // c
            // c     x is a double precision array of dimension n.
            // c       On entry x is the starting point for the GCP computation.
            // c       On exit x is unchanged.
            // c
            // c     l is a double precision array of dimension n.
            // c       On entry l is the lower bound of x.
            // c       On exit l is unchanged.
            // c
            // c     u is a double precision array of dimension n.
            // c       On entry u is the upper bound of x.
            // c       On exit u is unchanged.
            // c
            // c     nbd is an integer array of dimension n.
            // c       On entry nbd represents the type of bounds imposed on the
            // c         variables, and must be specified as follows:
            // c         nbd(i)=0 if x(i) is unbounded,
            // c                1 if x(i) has only a lower bound,
            // c                2 if x(i) has both lower and upper bounds, and
            // c                3 if x(i) has only an upper bound. 
            // c       On exit nbd is unchanged.
            // c
            // c     g is a double precision array of dimension n.
            // c       On entry g is the gradient of f(x).  g must be a nonzero vector.
            // c       On exit g is unchanged.
            // c
            // c     iorder is an integer working array of dimension n.
            // c       iorder will be used to store the breakpoints in the piecewise
            // c       linear path and free variables encountered. On exit,
            // c         iorder(1),...,iorder(nleft) are indices of breakpoints
            // c                                which have not been encountered; 
            // c         iorder(nleft+1),...,iorder(nbreak) are indices of
            // c                                     encountered breakpoints; and
            // c         iorder(nfree),...,iorder(n) are indices of variables which
            // c                 have no bound constraits along the search direction.
            // c
            // c     iwhere is an integer array of dimension n.
            // c       On entry iwhere indicates only the permanently fixed (iwhere=3)
            // c       or free (iwhere= -1) components of x.
            // c       On exit iwhere records the status of the current x variables.
            // c       iwhere(i)=-3  if x(i) is free and has bounds, but is not moved
            // c                 0   if x(i) is free and has bounds, and is moved
            // c                 1   if x(i) is fixed at l(i), and l(i) .ne. u(i)
            // c                 2   if x(i) is fixed at u(i), and u(i) .ne. l(i)
            // c                 3   if x(i) is always fixed, i.e.,  u(i)=x(i)=l(i)
            // c                 -1  if x(i) is always free, i.e., it has no bounds.
            // c
            // c     t is a double precision working array of dimension n. 
            // c       t will be used to store the break points.
            // c
            // c     d is a double precision array of dimension n used to store
            // c       the Cauchy direction P(x-tg)-x.
            // c
            // c     xcp is a double precision array of dimension n used to return the
            // c       GCP on exit.
            // c
            // c     m is an integer variable.
            // c       On entry m is the maximum number of variable metric corrections 
            // c         used to define the limited memory matrix.
            // c       On exit m is unchanged.
            // c
            // c     ws, wy, sy, and wt are double precision arrays.
            // c       On entry they store information that defines the
            // c                             limited memory BFGS matrix:
            // c         ws(n,m) stores S, a set of s-vectors;
            // c         wy(n,m) stores Y, a set of y-vectors;
            // c         sy(m,m) stores S'Y;
            // c         wt(m,m) stores the
            // c                 Cholesky factorization of (theta*S'S+LD^(-1)L').
            // c       On exit these arrays are unchanged.
            // c
            // c     theta is a double precision variable.
            // c       On entry theta is the scaling factor specifying B_0 = theta I.
            // c       On exit theta is unchanged.
            // c
            // c     col is an integer variable.
            // c       On entry col is the actual number of variable metric
            // c         corrections stored so far.
            // c       On exit col is unchanged.
            // c
            // c     head is an integer variable.
            // c       On entry head is the location of the first s-vector (or y-vector)
            // c         in S (or Y).
            // c       On exit col is unchanged.
            // c
            // c     p is a double precision working array of dimension 2m.
            // c       p will be used to store the vector p = W^(T)d.
            // c
            // c     c is a double precision working array of dimension 2m.
            // c       c will be used to store the vector c = W^(T)(xcp-x).
            // c
            // c     wbp is a double precision working array of dimension 2m.
            // c       wbp will be used to store the row of W corresponding
            // c         to a breakpoint.
            // c
            // c     v is a double precision working array of dimension 2m.
            // c
            // c     nint is an integer variable.
            // c       On exit nint records the number of quadratic segments explored
            // c         in searching for the GCP.
            // c
            // c     sg and yg are double precision arrays of dimension m.
            // c       On entry sg  and yg store S'g and Y'g correspondingly.
            // c       On exit they are unchanged. 
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
            // c     sbgnrm is a double precision variable.
            // c       On entry sbgnrm is the norm of the projected gradient at x.
            // c       On exit sbgnrm is unchanged.
            // c
            // c     info is an integer variable.
            // c       On entry info is 0.
            // c       On exit info = 0       for normal return,
            // c                    = nonzero for abnormal return when the the system
            // c                              used in routine bmv is singular.
            // c
            // c     Subprograms called:
            // c 
            // c       L-BFGS-B Library ... hpsolb, bmv.
            // c
            // c       Linpack ... dscal dcopy, daxpy.
            // c
            // c
            // c     References:
            // c
            // c       [1] R. H. Byrd, P. Lu, J. Nocedal and C. Zhu, ``A limited
            // c       memory algorithm for bound constrained optimization'',
            // c       SIAM J. Scientific Computing 16 (1995), no. 5, pp. 1190--1208.
            // c
            // c       [2] C. Zhu, R.H. Byrd, P. Lu, J. Nocedal, ``L-BFGS-B: FORTRAN
            // c       Subroutines for Large Scale Bound Constrained Optimization''
            // c       Tech. Report, NAM-11, EECS Department, Northwestern University,
            // c       1994.
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
            
            
            
            
            
            // c     Check the status of the variables, reset iwhere(i) if necessary;
            // c       compute the Cauchy direction d and the breakpoints t; initialize
            // c       the derivative f1 and the vector p = W'd (for theta = 1).
            

            #endregion


            #region Body
            
            if (SBGNRM <= ZERO)
            {
                if (IPRINT >= 0) ;//ERROR-ERRORWRITE(6,*)'Subgnorm = 0.  GCP = X.'
                this._dcopy.Run(N, X, offset_x, 1, ref XCP, offset_xcp, 1);
                return;
            }
            BNDED = true;
            NFREE = N + 1;
            NBREAK = 0;
            IBKMIN = 0;
            BKMIN = ZERO;
            COL2 = 2 * COL;
            F1 = ZERO;
            if (IPRINT >= 99) ;//ERROR-ERRORWRITE(6,3010)
            
            // c     We set p to zero and build it up as we determine d.
            
            for (I = 1; I <= COL2; I++)
            {
                P[I + o_p] = ZERO;
            }
            
            // c     In the following loop we determine for each variable its bound
            // c        status and its breakpoint, and update p accordingly.
            // c        Smallest breakpoint is identified.
            
            for (I = 1; I <= N; I++)
            {
                NEGGI =  - G[I + o_g];
                if (IWHERE[I + o_iwhere] != 3 && IWHERE[I + o_iwhere] !=  - 1)
                {
                    // c             if x(i) is not a constant and has bounds,
                    // c             compute the difference between x(i) and its bounds.
                    if (NBD[I + o_nbd] <= 2) TL = X[I + o_x] - L[I + o_l];
                    if (NBD[I + o_nbd] >= 2) TU = U[I + o_u] - X[I + o_x];
                    
                    // c           If a variable is close enough to a bound
                    // c             we treat it as at bound.
                    XLOWER = NBD[I + o_nbd] <= 2 && TL <= ZERO;
                    XUPPER = NBD[I + o_nbd] >= 2 && TU <= ZERO;
                    
                    // c              reset iwhere(i).
                    IWHERE[I + o_iwhere] = 0;
                    if (XLOWER)
                    {
                        if (NEGGI <= ZERO) IWHERE[I + o_iwhere] = 1;
                    }
                    else
                    {
                        if (XUPPER)
                        {
                            if (NEGGI >= ZERO) IWHERE[I + o_iwhere] = 2;
                        }
                        else
                        {
                            if (Math.Abs(NEGGI) <= ZERO) IWHERE[I + o_iwhere] =  - 3;
                        }
                    }
                }
                POINTR = HEAD;
                if (IWHERE[I + o_iwhere] != 0 && IWHERE[I + o_iwhere] !=  - 1)
                {
                    D[I + o_d] = ZERO;
                }
                else
                {
                    D[I + o_d] = NEGGI;
                    F1 +=  - NEGGI * NEGGI;
                    // c             calculate p := p - W'e_i* (g_i).
                    for (J = 1; J <= COL; J++)
                    {
                        P[J + o_p] += WY[I+POINTR * N + o_wy] * NEGGI;
                        P[COL + J + o_p] += WS[I+POINTR * N + o_ws] * NEGGI;
                        POINTR = FortranLib.Mod(POINTR,M) + 1;
                    }
                    if (NBD[I + o_nbd] <= 2 && NBD[I + o_nbd] != 0 && NEGGI < ZERO)
                    {
                        // c                                 x(i) + d(i) is bounded; compute t(i).
                        NBREAK += 1;
                        IORDER[NBREAK + o_iorder] = I;
                        T[NBREAK + o_t] = TL / ( - NEGGI);
                        if (NBREAK == 1 || T[NBREAK + o_t] < BKMIN)
                        {
                            BKMIN = T[NBREAK + o_t];
                            IBKMIN = NBREAK;
                        }
                    }
                    else
                    {
                        if (NBD[I + o_nbd] >= 2 && NEGGI > ZERO)
                        {
                            // c                                 x(i) + d(i) is bounded; compute t(i).
                            NBREAK += 1;
                            IORDER[NBREAK + o_iorder] = I;
                            T[NBREAK + o_t] = TU / NEGGI;
                            if (NBREAK == 1 || T[NBREAK + o_t] < BKMIN)
                            {
                                BKMIN = T[NBREAK + o_t];
                                IBKMIN = NBREAK;
                            }
                        }
                        else
                        {
                            // c                x(i) + d(i) is not bounded.
                            NFREE -= 1;
                            IORDER[NFREE + o_iorder] = I;
                            if (Math.Abs(NEGGI) > ZERO) BNDED = false;
                        }
                    }
                }
            }
            
            // c     The indices of the nonzero components of d are now stored
            // c       in iorder(1),...,iorder(nbreak) and iorder(nfree),...,iorder(n).
            // c       The smallest of the nbreak breakpoints is in t(ibkmin)=bkmin.
            
            if (THETA != ONE)
            {
                // c                   complete the initialization of p for theta not= one.
                this._dscal.Run(COL, THETA, ref P, COL + 1 + o_p, 1);
            }
            
            // c     Initialize GCP xcp = x.
            
            this._dcopy.Run(N, X, offset_x, 1, ref XCP, offset_xcp, 1);
            
            if (NBREAK == 0 && NFREE == N + 1)
            {
                // c                  is a zero vector, return with the initial xcp as GCP.
                if (IPRINT > 100) ;//ERROR-ERRORWRITE(6,1010)(XCP(I),I=1,N)
                return;
            }
            
            // c     Initialize c = W'(xcp - x) = 0.
            
            for (J = 1; J <= COL2; J++)
            {
                C[J + o_c] = ZERO;
            }
            
            // c     Initialize derivative f2.
            
            F2 =  - THETA * F1;
            F2_ORG = F2;
            if (COL > 0)
            {
                this._bmv.Run(M, SY, offset_sy, WT, offset_wt, COL, P, offset_p, ref V, offset_v
                              , ref INFO);
                if (INFO != 0) return;
                F2 -= this._ddot.Run(COL2, V, offset_v, 1, P, offset_p, 1);
            }
            DTM =  - F1 / F2;
            TSUM = ZERO;
            NINT = 1;
            if (IPRINT >= 99) ;//ERROR-ERRORWRITE(6,*)'There are ',NBREAK,'  breakpoints '
            
            // c     If there are no breakpoints, locate the GCP and return. 
            
            if (NBREAK == 0) goto LABEL888;
            
            NLEFT = NBREAK;
            ITER = 1;
            
            
            TJ = ZERO;
            
            // c------------------- the beginning of the loop -------------------------
            
        LABEL777:;
            
            // c     Find the next smallest breakpoint;
            // c       compute dt = t(nleft) - t(nleft + 1).
            
            TJ0 = TJ;
            if (ITER == 1)
            {
                // c         Since we already have the smallest breakpoint we need not do
                // c         heapsort yet. Often only one breakpoint is used and the
                // c         cost of heapsort is avoided.
                TJ = BKMIN;
                IBP = IORDER[IBKMIN + o_iorder];
            }
            else
            {
                if (ITER == 2)
                {
                    // c             Replace the already used smallest breakpoint with the
                    // c             breakpoint numbered nbreak > nlast, before heapsort call.
                    if (IBKMIN != NBREAK)
                    {
                        T[IBKMIN + o_t] = T[NBREAK + o_t];
                        IORDER[IBKMIN + o_iorder] = IORDER[NBREAK + o_iorder];
                    }
                    // c        Update heap structure of breakpoints
                    // c           (if iter=2, initialize heap).
                }
                this._hpsolb.Run(NLEFT, ref T, offset_t, ref IORDER, offset_iorder, ITER - 2);
                TJ = T[NLEFT + o_t];
                IBP = IORDER[NLEFT + o_iorder];
            }
            
            DT = TJ - TJ0;
            
            if (DT != ZERO && IPRINT >= 100)
            {
                //ERROR-ERROR         WRITE (6,4011) NINT,F1,F2;
                //ERROR-ERROR         WRITE (6,5010) DT;
                //ERROR-ERROR         WRITE (6,6010) DTM;
            }
            
            // c     If a minimizer is within this interval, locate the GCP and return. 
            
            if (DTM < DT) goto LABEL888;
            
            // c     Otherwise fix one variable and
            // c       reset the corresponding component of d to zero.
            
            TSUM += DT;
            NLEFT -= 1;
            ITER += 1;
            DIBP = D[IBP + o_d];
            D[IBP + o_d] = ZERO;
            if (DIBP > ZERO)
            {
                ZIBP = U[IBP + o_u] - X[IBP + o_x];
                XCP[IBP + o_xcp] = U[IBP + o_u];
                IWHERE[IBP + o_iwhere] = 2;
            }
            else
            {
                ZIBP = L[IBP + o_l] - X[IBP + o_x];
                XCP[IBP + o_xcp] = L[IBP + o_l];
                IWHERE[IBP + o_iwhere] = 1;
            }
            if (IPRINT >= 100) ;//ERROR-ERRORWRITE(6,*)'Variable  ',IBP,'  is fixed.'
            if (NLEFT == 0 && NBREAK == N)
            {
                // c                                             all n variables are fixed,
                // c                                                return with xcp as GCP.
                DTM = DT;
                goto LABEL999;
            }
            
            // c     Update the derivative information.
            
            NINT += 1;
            DIBP2 = Math.Pow(DIBP,2);
            
            // c     Update f1 and f2.
            
            // c        temporarily set f1 and f2 for col=0.
            F1 += DT * F2 + DIBP2 - THETA * DIBP * ZIBP;
            F2 +=  - THETA * DIBP2;
            
            if (COL > 0)
            {
                // c                          update c = c + dt*p.
                this._daxpy.Run(COL2, DT, P, offset_p, 1, ref C, offset_c, 1);
                
                // c           choose wbp,
                // c           the row of W corresponding to the breakpoint encountered.
                POINTR = HEAD;
                for (J = 1; J <= COL; J++)
                {
                    WBP[J + o_wbp] = WY[IBP+POINTR * N + o_wy];
                    WBP[COL + J + o_wbp] = THETA * WS[IBP+POINTR * N + o_ws];
                    POINTR = FortranLib.Mod(POINTR,M) + 1;
                }
                
                // c           compute (wbp)Mc, (wbp)Mp, and (wbp)M(wbp)'.
                this._bmv.Run(M, SY, offset_sy, WT, offset_wt, COL, WBP, offset_wbp, ref V, offset_v
                              , ref INFO);
                if (INFO != 0) return;
                WMC = this._ddot.Run(COL2, C, offset_c, 1, V, offset_v, 1);
                WMP = this._ddot.Run(COL2, P, offset_p, 1, V, offset_v, 1);
                WMW = this._ddot.Run(COL2, WBP, offset_wbp, 1, V, offset_v, 1);
                
                // c           update p = p - dibp*wbp. 
                this._daxpy.Run(COL2,  - DIBP, WBP, offset_wbp, 1, ref P, offset_p, 1);
                
                // c           complete updating f1 and f2 while col > 0.
                F1 += DIBP * WMC;
                F2 += 2.0E0 * DIBP * WMP - DIBP2 * WMW;
            }
            
            F2 = Math.Max(EPSMCH * F2_ORG, F2);
            if (NLEFT > 0)
            {
                DTM =  - F1 / F2;
                goto LABEL777;
                // c                 to repeat the loop for unsearched intervals. 
            }
            else
            {
                if (BNDED)
                {
                    F1 = ZERO;
                    F2 = ZERO;
                    DTM = ZERO;
                }
                else
                {
                    DTM =  - F1 / F2;
                }
            }
            
            // c------------------- the end of the loop -------------------------------
            
        LABEL888:;
            if (IPRINT >= 99)
            {
                //ERROR-ERROR         WRITE (6,*);
                //ERROR-ERROR         WRITE (6,*) 'GCP found in this segment';
                //ERROR-ERROR         WRITE (6,4010) NINT,F1,F2;
                //ERROR-ERROR         WRITE (6,6010) DTM;
            }
            if (DTM <= ZERO) DTM = ZERO;
            TSUM += DTM;
            
            // c     Move free variables (i.e., the ones w/o breakpoints) and 
            // c       the variables whose breakpoints haven't been reached.
            
            this._daxpy.Run(N, TSUM, D, offset_d, 1, ref XCP, offset_xcp, 1);
            
        LABEL999:;
            
            // c     Update c = c + dtm*p = W'(x^c - x) 
            // c       which will be used in computing r = Z'(B(x^c - x) + g).
            
            if (COL > 0) this._daxpy.Run(COL2, DTM, P, offset_p, 1, ref C, offset_c, 1);
            if (IPRINT > 100) ;//ERROR-ERRORWRITE(6,1010)(XCP(I),I=1,N)
            if (IPRINT >= 99) ;//ERROR-ERRORWRITE(6,2010)
            
            
            return;
            

            #endregion

        }
    }
    
    // c====================== The end of cauchy ==============================
}
