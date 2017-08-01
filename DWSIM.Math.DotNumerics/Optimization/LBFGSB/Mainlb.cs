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
    public class MAINLB
    {
    

        #region Dependencies
        
        CAUCHY _cauchy; SUBSM _subsm; LNSRLB _lnsrlb; FORMK _formk; ERRCLB _errclb; PRN1LB _prn1lb; PRN2LB _prn2lb; 
        PRN3LB _prn3lb;ACTIVE _active; PROJGR _projgr; FREEV _freev; CMPRLB _cmprlb; MATUPD _matupd; FORMT _formt; TIMER _timer; 
        DPMEPS _dpmeps;DCOPY _dcopy; DDOT _ddot; DSCAL _dscal; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E0; const double ZERO = 0.0E0; 

        #endregion

        public MAINLB(CAUCHY cauchy, SUBSM subsm, LNSRLB lnsrlb, FORMK formk, ERRCLB errclb, PRN1LB prn1lb, PRN2LB prn2lb, PRN3LB prn3lb, ACTIVE active, PROJGR projgr
                      , FREEV freev, CMPRLB cmprlb, MATUPD matupd, FORMT formt, TIMER timer, DPMEPS dpmeps, DCOPY dcopy, DDOT ddot, DSCAL dscal)
        {
    

            #region Set Dependencies
            
            this._cauchy = cauchy; this._subsm = subsm; this._lnsrlb = lnsrlb; this._formk = formk; this._errclb = errclb; 
            this._prn1lb = prn1lb;this._prn2lb = prn2lb; this._prn3lb = prn3lb; this._active = active; this._projgr = projgr; 
            this._freev = freev;this._cmprlb = cmprlb; this._matupd = matupd; this._formt = formt; this._timer = timer; 
            this._dpmeps = dpmeps;this._dcopy = dcopy; this._ddot = ddot; this._dscal = dscal; 

            #endregion

        }
    
        public MAINLB()
        {
    

            #region Dependencies (Initialization)
            
            HPSOLB hpsolb = new HPSOLB();
            DDOT ddot = new DDOT();
            DAXPY daxpy = new DAXPY();
            DSCAL dscal = new DSCAL();
            DCOPY dcopy = new DCOPY();
            DCSTEP dcstep = new DCSTEP();
            ERRCLB errclb = new ERRCLB();
            PRN1LB prn1lb = new PRN1LB();
            PRN2LB prn2lb = new PRN2LB();
            PRN3LB prn3lb = new PRN3LB();
            ACTIVE active = new ACTIVE();
            PROJGR projgr = new PROJGR();
            FREEV freev = new FREEV();
            TIMER timer = new TIMER();
            DPMEPS dpmeps = new DPMEPS();
            DTRSL dtrsl = new DTRSL(ddot, daxpy);
            BMV bmv = new BMV(dtrsl);
            CAUCHY cauchy = new CAUCHY(hpsolb, bmv, dscal, dcopy, daxpy, ddot);
            SUBSM subsm = new SUBSM(dtrsl);
            DCSRCH dcsrch = new DCSRCH(dcstep);
            LNSRLB lnsrlb = new LNSRLB(dtrsl, ddot, dcsrch, dcopy);
            DPOFA dpofa = new DPOFA(ddot);
            FORMK formk = new FORMK(dcopy, dpofa, dtrsl, ddot);
            CMPRLB cmprlb = new CMPRLB(bmv);
            MATUPD matupd = new MATUPD(dcopy, ddot);
            FORMT formt = new FORMT(dpofa);

            #endregion


            #region Set Dependencies
            
            this._cauchy = cauchy; this._subsm = subsm; this._lnsrlb = lnsrlb; this._formk = formk; this._errclb = errclb; 
            this._prn1lb = prn1lb;this._prn2lb = prn2lb; this._prn3lb = prn3lb; this._active = active; this._projgr = projgr; 
            this._freev = freev;this._cmprlb = cmprlb; this._matupd = matupd; this._formt = formt; this._timer = timer; 
            this._dpmeps = dpmeps;this._dcopy = dcopy; this._ddot = ddot; this._dscal = dscal; 

            #endregion

        }
        /// <param name="N">
        /// is an integer variable.
        /// On entry n is the number of variables.
        /// On exit n is unchanged.
        ///</param>
        /// <param name="M">
        /// is an integer variable.
        /// On entry m is the maximum number of variable metric
        /// corrections allowed in the limited memory matrix.
        /// On exit m is unchanged.
        ///</param>
        /// <param name="X">
        /// is a double precision array of dimension n.
        /// On entry x is an approximation to the solution.
        /// On exit x is the current approximation.
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
        /// 2 if x(i) has both lower and upper bounds,
        /// 3 if x(i) has only an upper bound.
        /// On exit nbd is unchanged.
        ///</param>
        /// <param name="F">
        /// is a double precision variable.
        /// On first entry f is unspecified.
        /// On final exit f is the value of the function at x.
        ///</param>
        /// <param name="G">
        /// is a double precision array of dimension n.
        /// On first entry g is unspecified.
        /// On final exit g is the value of the gradient at x.
        ///</param>
        /// <param name="FACTR">
        /// is a double precision variable.
        /// On entry factr .GE. 0 is specified by the user.  The iteration
        /// will stop when
        /// 
        /// (f^k - f^{k+1})/max{|f^k|,|f^{k+1}|,1} .LE. factr*epsmch
        /// 
        /// where epsmch is the machine precision, which is automatically
        /// generated by the code.
        /// On exit factr is unchanged.
        ///</param>
        /// <param name="PGTOL">
        /// is a double precision variable.
        /// On entry pgtol .GE. 0 is specified by the user.  The iteration
        /// will stop when
        /// 
        /// max{|proj g_i | i = 1, ..., n} .LE. pgtol
        /// 
        /// where pg_i is the ith component of the projected gradient.
        /// On exit pgtol is unchanged.
        ///</param>
        /// <param name="WN">
        /// is a double precision working array of dimension 2m x 2m
        /// used to store the LEL^T factorization of the indefinite matrix
        /// K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ]
        /// [L_a -R_z           theta*S'AA'S ]
        /// 
        /// where     E = [-I  0]
        /// [ 0  I]
        ///</param>
        /// <param name="SND">
        /// is a double precision working array of dimension 2m x 2m
        /// used to store the lower triangular part of
        /// N = [Y' ZZ'Y   L_a'+R_z']
        /// [L_a +R_z  S'AA'S   ]
        ///</param>
        /// <param name="Z">
        /// is used at different times to store the Cauchy point and
        ///</param>
        /// <param name="INDEX">
        /// is an integer working array of dimension n.
        /// In subroutine freev, index is used to store the free and fixed
        /// variables at the Generalized Cauchy Point (GCP).
        ///</param>
        /// <param name="IWHERE">
        /// is an integer working array of dimension n used to record
        /// the status of the vector x for GCP computation.
        /// iwhere(i)=0 or -3 if x(i) is free and has bounds,
        /// 1       if x(i) is fixed at l(i), and l(i) .ne. u(i)
        /// 2       if x(i) is fixed at u(i), and u(i) .ne. l(i)
        /// 3       if x(i) is always fixed, i.e.,  u(i)=x(i)=l(i)
        /// -1       if x(i) is always free, i.e., no bounds on it.
        ///</param>
        /// <param name="INDX2">
        /// is an integer working array of dimension n.
        /// Within subroutine cauchy, indx2 corresponds to the array iorder.
        /// In subroutine freev, a list of variables entering and leaving
        /// the free set is stored in indx2, and it is passed on to
        /// subroutine formk with this information.
        ///</param>
        /// <param name="TASK">
        /// is a working string of characters of length 60 indicating
        /// the current job when entering and leaving this subroutine.
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
        /// <param name="CSAVE">
        /// is a working string of characters of length 60.
        ///</param>
        /// <param name="LSAVE">
        /// is a logical working array of dimension 4.
        ///</param>
        /// <param name="ISAVE">
        /// is an integer working array of dimension 23.
        ///</param>
        /// <param name="DSAVE">
        /// is a double precision working array of dimension 29.
        /// 
        ///</param>
        public void Run(int N, int M, ref double[] X, int offset_x, double[] L, int offset_l, double[] U, int offset_u, int[] NBD, int offset_nbd
                         , ref double F, ref double[] G, int offset_g, double FACTR, double PGTOL, ref double[] WS, int offset_ws, ref double[] WY, int offset_wy
                         , ref double[] SY, int offset_sy, ref double[] SS, int offset_ss, double[] YY, int offset_yy, ref double[] WT, int offset_wt, ref double[] WN, int offset_wn, ref double[] SND, int offset_snd
                         , ref double[] Z, int offset_z, ref double[] R, int offset_r, ref double[] D, int offset_d, ref double[] T, int offset_t, ref double[] WA, int offset_wa, double[] SG, int offset_sg
                         , double[] SGO, int offset_sgo, double[] YG, int offset_yg, double[] YGO, int offset_ygo, ref int[] INDEX, int offset_index, ref int[] IWHERE, int offset_iwhere, ref int[] INDX2, int offset_indx2
                         , ref BFGSTask TASK, int IPRINT, ref BFGSTask CSAVE, ref bool[] LSAVE, int offset_lsave, ref int[] ISAVE, int offset_isave, ref double[] DSAVE, int offset_dsave)
        {

            #region Variables

            BFGSWord WORD = BFGSWord.aaa;

            bool PRJCTD = false; bool CNSTND = false; bool BOXED = false; bool UPDATD = false; bool WRK = false;
            int I = 0; int K = 0; int NINTOL = 0; int ITFILE = 0; int IBACK = 0; int NSKIP = 0; 
            int HEAD = 0;int COL = 0; int ITER = 0; int ITAIL = 0; int IUPDAT = 0; int NINT = 0; int NFGV = 0; int INFO = 0; 
            int IFUN = 0;int IWORD = 0; int NFREE = 0; int NACT = 0; int ILEAVE = 0; int NENTER = 0; double THETA = 0; 
            double FOLD = 0;double DDOT = 0; double DR = 0; double RR = 0; double TOL = 0; double DPMEPS = 0; double XSTEP = 0; 
            double SBGNRM = 0;double DDUM = 0; double DNORM = 0; double DTD = 0; double EPSMCH = 0; double CPU1 = 0; 
            double CPU2 = 0;double CACHYT = 0; double SBTIME = 0; double LNSCHT = 0; double TIME1 = 0; double TIME2 = 0; 
            double GD = 0;double GDOLD = 0; double STP = 0; double STPMX = 0; double TIME = 0; 

            #endregion


            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_l = -1 + offset_l;  int o_u = -1 + offset_u;  int o_nbd = -1 + offset_nbd; 
             int o_g = -1 + offset_g; int o_ws = -1 - N + offset_ws;  int o_wy = -1 - N + offset_wy; 
             int o_sy = -1 - M + offset_sy; int o_ss = -1 - M + offset_ss;  int o_yy = -1 - M + offset_yy; 
             int o_wt = -1 - M + offset_wt; int o_wn = -1 - (2*M) + offset_wn;  int o_snd = -1 - (2*M) + offset_snd; 
             int o_z = -1 + offset_z; int o_r = -1 + offset_r;  int o_d = -1 + offset_d;  int o_t = -1 + offset_t; 
             int o_wa = -1 + offset_wa; int o_sg = -1 + offset_sg;  int o_sgo = -1 + offset_sgo;  int o_yg = -1 + offset_yg; 
             int o_ygo = -1 + offset_ygo; int o_index = -1 + offset_index;  int o_iwhere = -1 + offset_iwhere; 
             int o_indx2 = -1 + offset_indx2; int o_lsave = -1 + offset_lsave;  int o_isave = -1 + offset_isave; 
             int o_dsave = -1 + offset_dsave;

            #endregion


            #region Prolog
            
            
            
            // c     ************
            // c
            // c     Subroutine mainlb
            // c
            // c     This subroutine solves bound constrained optimization problems by
            // c       using the compact formula of the limited memory BFGS updates.
            // c       
            // c     n is an integer variable.
            // c       On entry n is the number of variables.
            // c       On exit n is unchanged.
            // c
            // c     m is an integer variable.
            // c       On entry m is the maximum number of variable metric
            // c          corrections allowed in the limited memory matrix.
            // c       On exit m is unchanged.
            // c
            // c     x is a double precision array of dimension n.
            // c       On entry x is an approximation to the solution.
            // c       On exit x is the current approximation.
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
            // c                2 if x(i) has both lower and upper bounds,
            // c                3 if x(i) has only an upper bound.
            // c       On exit nbd is unchanged.
            // c
            // c     f is a double precision variable.
            // c       On first entry f is unspecified.
            // c       On final exit f is the value of the function at x.
            // c
            // c     g is a double precision array of dimension n.
            // c       On first entry g is unspecified.
            // c       On final exit g is the value of the gradient at x.
            // c
            // c     factr is a double precision variable.
            // c       On entry factr >= 0 is specified by the user.  The iteration
            // c         will stop when
            // c
            // c         (f^k - f^{k+1})/max{|f^k|,|f^{k+1}|,1} <= factr*epsmch
            // c
            // c         where epsmch is the machine precision, which is automatically
            // c         generated by the code.
            // c       On exit factr is unchanged.
            // c
            // c     pgtol is a double precision variable.
            // c       On entry pgtol >= 0 is specified by the user.  The iteration
            // c         will stop when
            // c
            // c                 max{|proj g_i | i = 1, ..., n} <= pgtol
            // c
            // c         where pg_i is the ith component of the projected gradient.
            // c       On exit pgtol is unchanged.
            // c
            // c     ws, wy, sy, and wt are double precision working arrays used to
            // c       store the following information defining the limited memory
            // c          BFGS matrix:
            // c          ws, of dimension n x m, stores S, the matrix of s-vectors;
            // c          wy, of dimension n x m, stores Y, the matrix of y-vectors;
            // c          sy, of dimension m x m, stores S'Y;
            // c          ss, of dimension m x m, stores S'S;
            // c	   yy, of dimension m x m, stores Y'Y;
            // c          wt, of dimension m x m, stores the Cholesky factorization
            // c                                  of (theta*S'S+LD^(-1)L'); see eq.
            // c                                  (2.26) in [3].
            // c
            // c     wn is a double precision working array of dimension 2m x 2m
            // c       used to store the LEL^T factorization of the indefinite matrix
            // c                 K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ]
            // c                     [L_a -R_z           theta*S'AA'S ]
            // c
            // c       where     E = [-I  0]
            // c                     [ 0  I]
            // c
            // c     snd is a double precision working array of dimension 2m x 2m
            // c       used to store the lower triangular part of
            // c                 N = [Y' ZZ'Y   L_a'+R_z']
            // c                     [L_a +R_z  S'AA'S   ]
            // c	     
            // c     z(n),r(n),d(n),t(n),wa(8*m) are double precision working arrays.
            // c       z is used at different times to store the Cauchy point and
            // c       the Newton point.
            // c
            // c     sg(m),sgo(m),yg(m),ygo(m) are double precision working arrays. 
            // c
            // c     index is an integer working array of dimension n.
            // c       In subroutine freev, index is used to store the free and fixed
            // c          variables at the Generalized Cauchy Point (GCP).
            // c
            // c     iwhere is an integer working array of dimension n used to record
            // c       the status of the vector x for GCP computation.
            // c       iwhere(i)=0 or -3 if x(i) is free and has bounds,
            // c                 1       if x(i) is fixed at l(i), and l(i) .ne. u(i)
            // c                 2       if x(i) is fixed at u(i), and u(i) .ne. l(i)
            // c                 3       if x(i) is always fixed, i.e.,  u(i)=x(i)=l(i)
            // c                -1       if x(i) is always free, i.e., no bounds on it.
            // c
            // c     indx2 is an integer working array of dimension n.
            // c       Within subroutine cauchy, indx2 corresponds to the array iorder.
            // c       In subroutine freev, a list of variables entering and leaving
            // c       the free set is stored in indx2, and it is passed on to
            // c       subroutine formk with this information.
            // c
            // c     task is a working string of characters of length 60 indicating
            // c       the current job when entering and leaving this subroutine.
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
            // c     csave is a working string of characters of length 60.
            // c
            // c     lsave is a logical working array of dimension 4.
            // c
            // c     isave is an integer working array of dimension 23.
            // c
            // c     dsave is a double precision working array of dimension 29.
            // c
            // c
            // c     Subprograms called
            // c
            // c       L-BFGS-B Library ... cauchy, subsm, lnsrlb, formk, 
            // c
            // c        errclb, prn1lb, prn2lb, prn3lb, active, projgr,
            // c
            // c        freev, cmprlb, matupd, formt.
            // c
            // c       Minpack2 Library ... timer, dpmeps.
            // c
            // c       Linpack Library ... dcopy, ddot.
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
            // c       [3] R. Byrd, J. Nocedal and R. Schnabel "Representations of
            // c       Quasi-Newton Matrices and their use in Limited Memory Methods'',
            // c       Mathematical Programming 63 (1994), no. 4, pp. 129-156.
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
            
            
            
            
            

            #endregion


            #region Body

             if (TASK == BFGSTask.START)
            {
                
                this._timer.Run(ref TIME1);
                
                // c        Generate the current machine precision.
                
                EPSMCH = this._dpmeps.Run();
                
                // c        Initialize counters and scalars when task='START'.
                
                // c           for the limited memory BFGS matrices:
                COL = 0;
                HEAD = 1;
                THETA = ONE;
                IUPDAT = 0;
                UPDATD = false;
                
                // c           for operation counts:
                ITER = 0;
                NFGV = 0;
                NINT = 0;
                NINTOL = 0;
                NSKIP = 0;
                NFREE = N;
                
                // c           for stopping tolerance:
                TOL = FACTR * EPSMCH;
                
                // c           for measuring running time:
                CACHYT = 0;
                SBTIME = 0;
                LNSCHT = 0;
                
                // c           'word' records the status of subspace solutions.
                WORD = BFGSWord.aaa;
                
                // c           'info' records the termination information.
                INFO = 0;
                
                if (IPRINT >= 1)
                {
                    // c                                open a summary file 'iterate.dat'
                    //ERROR-ERROR            OPEN (8, FILE = 'iterate.dat', STATUS = 'unknown');
                    ITFILE = 8;
                }
                
                // c        Check the input arguments for errors.
                
                this._errclb.Run(N, M, FACTR, L, offset_l, U, offset_u, NBD, offset_nbd
                                 , ref TASK, ref INFO, ref K);
                if (TASK == BFGSTask.ERROR)
                {
                    this._prn3lb.Run(N, X, offset_x, F, TASK, IPRINT, INFO
                                     , ITFILE, ITER, NFGV, NINTOL, NSKIP, NACT
                                     , SBGNRM, ZERO, NINT, WORD, IBACK, STP
                                     , XSTEP, K, CACHYT, SBTIME, LNSCHT);
                    return;
                }
                
                this._prn1lb.Run(N, M, L, offset_l, U, offset_u, X, offset_x, IPRINT
                                 , ITFILE, EPSMCH);
                
                // c        Initialize iwhere & project x onto the feasible set.
                
                this._active.Run(N, L, offset_l, U, offset_u, NBD, offset_nbd, ref X, offset_x, ref IWHERE, offset_iwhere
                                 , IPRINT, ref PRJCTD, ref CNSTND, ref BOXED);
                
                // c        The end of the initialization.
                
            }
            else
            {
                // c          restore local variables.
                
                PRJCTD = LSAVE[1 + o_lsave];
                CNSTND = LSAVE[2 + o_lsave];
                BOXED = LSAVE[3 + o_lsave];
                UPDATD = LSAVE[4 + o_lsave];
                
                NINTOL = ISAVE[1 + o_isave];
                ITFILE = ISAVE[3 + o_isave];
                IBACK = ISAVE[4 + o_isave];
                NSKIP = ISAVE[5 + o_isave];
                HEAD = ISAVE[6 + o_isave];
                COL = ISAVE[7 + o_isave];
                ITAIL = ISAVE[8 + o_isave];
                ITER = ISAVE[9 + o_isave];
                IUPDAT = ISAVE[10 + o_isave];
                NINT = ISAVE[12 + o_isave];
                NFGV = ISAVE[13 + o_isave];
                INFO = ISAVE[14 + o_isave];
                IFUN = ISAVE[15 + o_isave];
                IWORD = ISAVE[16 + o_isave];
                NFREE = ISAVE[17 + o_isave];
                NACT = ISAVE[18 + o_isave];
                ILEAVE = ISAVE[19 + o_isave];
                NENTER = ISAVE[20 + o_isave];
                
                THETA = DSAVE[1 + o_dsave];
                FOLD = DSAVE[2 + o_dsave];
                TOL = DSAVE[3 + o_dsave];
                DNORM = DSAVE[4 + o_dsave];
                EPSMCH = DSAVE[5 + o_dsave];
                CPU1 = DSAVE[6 + o_dsave];
                CACHYT = DSAVE[7 + o_dsave];
                SBTIME = DSAVE[8 + o_dsave];
                LNSCHT = DSAVE[9 + o_dsave];
                TIME1 = DSAVE[10 + o_dsave];
                GD = DSAVE[11 + o_dsave];
                STPMX = DSAVE[12 + o_dsave];
                SBGNRM = DSAVE[13 + o_dsave];
                STP = DSAVE[14 + o_dsave];
                GDOLD = DSAVE[15 + o_dsave];
                DTD = DSAVE[16 + o_dsave];
                
                // c        After returning from the driver go to the point where execution
                // c        is to resume.

                if (TASK == BFGSTask.FG_LNSRCH) goto LABEL666;
                if (TASK == BFGSTask.NEW_X) goto LABEL777;
                if (TASK == BFGSTask.FG_ST || TASK == BFGSTask.FG_START) goto LABEL111;
                if (TASK == BFGSTask.STOP)
                {
                    if (TASK== BFGSTask.CPU)
                    {
                        // c                                          restore the previous iterate.
                        this._dcopy.Run(N, T, offset_t, 1, ref X, offset_x, 1);
                        this._dcopy.Run(N, R, offset_r, 1, ref G, offset_g, 1);
                        F = FOLD;
                    }
                    goto LABEL999;
                }
            }
            
            // c     Compute f0 and g0.

             TASK = BFGSTask.FG_START;
            // c          return to the driver to calculate f and g; reenter at 111.
            goto LABEL1000;
        LABEL111:;
            NFGV = 1;
            
            // c     Compute the infinity norm of the (-) projected gradient.
            
            this._projgr.Run(N, L, offset_l, U, offset_u, NBD, offset_nbd, X, offset_x, G, offset_g
                             , ref SBGNRM);
            
            if (IPRINT >= 1)
            {
                //ERROR-ERROR         WRITE (6,1002) ITER,F,SBGNRM;
                //ERROR-ERROR         WRITE (ITFILE,1003) ITER,NFGV,SBGNRM,F;
            }
            if (SBGNRM <= PGTOL)
            {
                // c                                terminate the algorithm.
                TASK = BFGSTask.CONV;
                goto LABEL999;
            }
            
            // c ----------------- the beginning of the loop --------------------------
            
        LABEL222:;
            if (IPRINT >= 99) ;//ERROR-ERRORWRITE(6,1001)ITER+1
            IWORD =  - 1;
            // c
            if (!CNSTND && COL > 0)
            {
                // c                                            skip the search for GCP.
                this._dcopy.Run(N, X, offset_x, 1, ref Z, offset_z, 1);
                WRK = UPDATD;
                NINT = 0;
                goto LABEL333;
            }
            
            // cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            // c
            // c     Compute the Generalized Cauchy Point (GCP).
            // c
            // cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            
            this._timer.Run(ref CPU1);
            this._cauchy.Run(N, X, offset_x, L, offset_l, U, offset_u, NBD, offset_nbd, G, offset_g
                             , ref INDX2, offset_indx2, ref IWHERE, offset_iwhere, ref T, offset_t, ref D, offset_d, ref Z, offset_z, M
                             , WY, offset_wy, WS, offset_ws, SY, offset_sy, WT, offset_wt, THETA, COL
                             , HEAD, ref WA, 1 + o_wa, ref WA, 2 * M + 1 + o_wa, ref WA, 4 * M + 1 + o_wa, ref WA, 6 * M + 1 + o_wa, ref NINT
                             , SG, offset_sg, YG, offset_yg, IPRINT, SBGNRM, ref INFO, EPSMCH);
            if (INFO != 0)
            {
                // c         singular triangular system detected; refresh the lbfgs memory.
                if (IPRINT >= 1) ;//ERROR-ERRORWRITE(6,1005)
                INFO = 0;
                COL = 0;
                HEAD = 1;
                THETA = ONE;
                IUPDAT = 0;
                UPDATD = false;
                this._timer.Run(ref CPU2);
                CACHYT += CPU2 - CPU1;
                goto LABEL222;
            }
            this._timer.Run(ref CPU2);
            CACHYT += CPU2 - CPU1;
            NINTOL += NINT;
            
            // c     Count the entering and leaving variables for iter > 0; 
            // c     find the index set of free and active variables at the GCP.
            
            this._freev.Run(N, ref NFREE, ref INDEX, offset_index, ref NENTER, ref ILEAVE, ref INDX2, offset_indx2
                            , IWHERE, offset_iwhere, ref WRK, UPDATD, CNSTND, IPRINT, ITER);
            
            NACT = N - NFREE;
            
        LABEL333:;
            
            // c     If there are no free variables or B=theta*I, then
            // c                                        skip the subspace minimization.
            
            if (NFREE == 0 || COL == 0) goto LABEL555;
            
            // cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            // c
            // c     Subspace minimization.
            // c
            // cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            
            this._timer.Run(ref CPU1);
            
            // c     Form  the LEL^T factorization of the indefinite
            // c       matrix    K = [-D -Y'ZZ'Y/theta     L_a'-R_z'  ]
            // c                     [L_a -R_z           theta*S'AA'S ]
            // c       where     E = [-I  0]
            // c                     [ 0  I]
            
            if (WRK)
            {
                this._formk.Run(N, NFREE, INDEX, offset_index, NENTER, ILEAVE, INDX2, offset_indx2
                                , IUPDAT, UPDATD, ref WN, offset_wn, ref SND, offset_snd, M, WS, offset_ws
                                , WY, offset_wy, SY, offset_sy, THETA, COL, HEAD, ref INFO);
            }
            if (INFO != 0)
            {
                // c          nonpositive definiteness in Cholesky factorization;
                // c          refresh the lbfgs memory and restart the iteration.
                if (IPRINT >= 1) ;//ERROR-ERRORWRITE(6,1006)
                INFO = 0;
                COL = 0;
                HEAD = 1;
                THETA = ONE;
                IUPDAT = 0;
                UPDATD = false;
                this._timer.Run(ref CPU2);
                SBTIME += CPU2 - CPU1;
                goto LABEL222;
            }
            
            // c        compute r=-Z'B(xcp-xk)-Z'g (using wa(2m+1)=W'(xcp-x)
            // c                                                   from 'cauchy').
            this._cmprlb.Run(N, M, X, offset_x, G, offset_g, WS, offset_ws, WY, offset_wy
                             , SY, offset_sy, WT, offset_wt, Z, offset_z, ref R, offset_r, ref WA, offset_wa, INDEX, offset_index
                             , THETA, COL, HEAD, NFREE, CNSTND, ref INFO);
            if (INFO != 0) goto LABEL444;
            // c       call the direct method.
            this._subsm.Run(N, M, NFREE, INDEX, offset_index, L, offset_l, U, offset_u
                            , NBD, offset_nbd, ref Z, offset_z, ref R, offset_r, WS, offset_ws, WY, offset_wy, THETA
                            , COL, HEAD, ref IWORD, ref WA, offset_wa, WN, offset_wn, IPRINT
                            , ref INFO);
        LABEL444:;
            if (INFO != 0)
            {
                // c          singular triangular system detected;
                // c          refresh the lbfgs memory and restart the iteration.
                if (IPRINT >= 1) ;//ERROR-ERRORWRITE(6,1005)
                INFO = 0;
                COL = 0;
                HEAD = 1;
                THETA = ONE;
                IUPDAT = 0;
                UPDATD = false;
                this._timer.Run(ref CPU2);
                SBTIME += CPU2 - CPU1;
                goto LABEL222;
            }
            
            this._timer.Run(ref CPU2);
            SBTIME += CPU2 - CPU1;
        LABEL555:;
            
            // cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            // c
            // c     Line search and optimality tests.
            // c
            // cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            
            // c     Generate the search direction d:=z-x.
            
            for (I = 1; I <= N; I++)
            {
                D[I + o_d] = Z[I + o_z] - X[I + o_x];
            }
            this._timer.Run(ref CPU1);
        LABEL666:;
            this._lnsrlb.Run(N, L, offset_l, U, offset_u, NBD, offset_nbd, ref X, offset_x, F
                             , ref FOLD, ref GD, ref GDOLD, G, offset_g, D, offset_d, ref R, offset_r
                             , ref T, offset_t, Z, offset_z, ref STP, ref DNORM, ref DTD, ref XSTEP
                             , ref STPMX, ITER, ref IFUN, ref IBACK, ref NFGV, ref INFO
                             , ref TASK, BOXED, CNSTND, ref CSAVE, ref ISAVE, 22 + o_isave, ref DSAVE, 17 + o_dsave);
            if (INFO != 0 || IBACK >= 20)
            {
                // c          restore the previous iterate.
                this._dcopy.Run(N, T, offset_t, 1, ref X, offset_x, 1);
                this._dcopy.Run(N, R, offset_r, 1, ref G, offset_g, 1);
                F = FOLD;
                if (COL == 0)
                {
                    // c             abnormal termination.
                    if (INFO == 0)
                    {
                        INFO =  - 9;
                        // c                restore the actual number of f and g evaluations etc.
                        NFGV -= 1;
                        IFUN -= 1;
                        IBACK -= 1;
                    }
                    TASK = BFGSTask.ABNO;
                    ITER += 1;
                    goto LABEL999;
                }
                else
                {
                    // c             refresh the lbfgs memory and restart the iteration.
                    if (IPRINT >= 1) ;//ERROR-ERRORWRITE(6,1008)
                    if (INFO == 0) NFGV -= 1;
                    INFO = 0;
                    COL = 0;
                    HEAD = 1;
                    THETA = ONE;
                    IUPDAT = 0;
                    UPDATD = false;
                    TASK = BFGSTask.RESTART;
                    this._timer.Run(ref CPU2);
                    LNSCHT += CPU2 - CPU1;
                    goto LABEL222;
                }
            }
            else
            {
                if (TASK == BFGSTask.FG_LNSRCH)
                {
                    // c          return to the driver for calculating f and g; reenter at 666.
                    goto LABEL1000;
                }
                else
                {
                    // c          calculate and print out the quantities related to the new X.
                    this._timer.Run(ref CPU2);
                    LNSCHT += CPU2 - CPU1;
                    ITER += 1;
                    
                    // c        Compute the infinity norm of the projected (-)gradient.
                    
                    this._projgr.Run(N, L, offset_l, U, offset_u, NBD, offset_nbd, X, offset_x, G, offset_g
                                     , ref SBGNRM);
                    
                    // c        Print iteration information.
                    
                    this._prn2lb.Run(N, X, offset_x, F, G, offset_g, IPRINT, ITFILE
                                     , ITER, NFGV, NACT, SBGNRM, NINT, ref WORD
                                     , IWORD, IBACK, STP, XSTEP);
                    goto LABEL1000;
                }
            }
        LABEL777:;
            
            // c     Test for termination.
            
            if (SBGNRM <= PGTOL)
            {
                // c                                terminate the algorithm.
                TASK = BFGSTask.CONV;
                goto LABEL999;
            }
            
            DDUM = Math.Max(Math.Abs(FOLD), Math.Max(Math.Abs(F), ONE));
            if ((FOLD - F) <= TOL * DDUM)
            {
                // c                                        terminate the algorithm.
                TASK = BFGSTask.CONV;
                if (IBACK >= 10) INFO =  - 5;
                // c           i.e., to issue a warning if iback>10 in the line search.
                goto LABEL999;
            }
            
            // c     Compute d=newx-oldx, r=newg-oldg, rr=y'y and dr=y's.
            
            for (I = 1; I <= N; I++)
            {
                R[I + o_r] = G[I + o_g] - R[I + o_r];
            }
            RR = this._ddot.Run(N, R, offset_r, 1, R, offset_r, 1);
            if (STP == ONE)
            {
                DR = GD - GDOLD;
                DDUM =  - GDOLD;
            }
            else
            {
                DR = (GD - GDOLD) * STP;
                this._dscal.Run(N, STP, ref D, offset_d, 1);
                DDUM =  - GDOLD * STP;
            }
            
            if (DR <= EPSMCH * DDUM)
            {
                // c                            skip the L-BFGS update.
                NSKIP += 1;
                UPDATD = false;
                if (IPRINT >= 1) ;//ERROR-ERRORWRITE(6,1004)DR,DDUM
                goto LABEL888;
            }
            
            // cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            // c
            // c     Update the L-BFGS matrix.
            // c
            // cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            
            UPDATD = true;
            IUPDAT += 1;
            
            // c     Update matrices WS and WY and form the middle matrix in B.
            
            this._matupd.Run(N, M, ref WS, offset_ws, ref WY, offset_wy, ref SY, offset_sy, ref SS, offset_ss
                             , D, offset_d, R, offset_r, ref ITAIL, IUPDAT, ref COL, ref HEAD
                             , ref THETA, RR, DR, STP, DTD);
            
            // c     Form the upper half of the pds T = theta*SS + L*D^(-1)*L';
            // c        Store T in the upper triangular of the array wt;
            // c        Cholesky factorize T to J*J' with
            // c           J' stored in the upper triangular of wt.
            
            this._formt.Run(M, ref WT, offset_wt, SY, offset_sy, SS, offset_ss, COL, THETA
                            , ref INFO);
            
            if (INFO != 0)
            {
                // c          nonpositive definiteness in Cholesky factorization;
                // c          refresh the lbfgs memory and restart the iteration.
                if (IPRINT >= 1) ;//ERROR-ERRORWRITE(6,1007)
                INFO = 0;
                COL = 0;
                HEAD = 1;
                THETA = ONE;
                IUPDAT = 0;
                UPDATD = false;
                goto LABEL222;
            }
            
            // c     Now the inverse of the middle matrix in B is
            
            // c       [  D^(1/2)      O ] [ -D^(1/2)  D^(-1/2)*L' ]
            // c       [ -L*D^(-1/2)   J ] [  0        J'          ]
            
        LABEL888:;
            
            // c -------------------- the end of the loop -----------------------------
            
            goto LABEL222;
        LABEL999:;
            this._timer.Run(ref TIME2);
            TIME = TIME2 - TIME1;
            this._prn3lb.Run(N, X, offset_x, F, TASK, IPRINT, INFO
                             , ITFILE, ITER, NFGV, NINTOL, NSKIP, NACT
                             , SBGNRM, TIME, NINT, WORD, IBACK, STP
                             , XSTEP, K, CACHYT, SBTIME, LNSCHT);
        LABEL1000:;
            
            // c     Save local variables.
            
            LSAVE[1 + o_lsave] = PRJCTD;
            LSAVE[2 + o_lsave] = CNSTND;
            LSAVE[3 + o_lsave] = BOXED;
            LSAVE[4 + o_lsave] = UPDATD;
            
            ISAVE[1 + o_isave] = NINTOL;
            ISAVE[3 + o_isave] = ITFILE;
            ISAVE[4 + o_isave] = IBACK;
            ISAVE[5 + o_isave] = NSKIP;
            ISAVE[6 + o_isave] = HEAD;
            ISAVE[7 + o_isave] = COL;
            ISAVE[8 + o_isave] = ITAIL;
            ISAVE[9 + o_isave] = ITER;
            ISAVE[10 + o_isave] = IUPDAT;
            ISAVE[12 + o_isave] = NINT;
            ISAVE[13 + o_isave] = NFGV;
            ISAVE[14 + o_isave] = INFO;
            ISAVE[15 + o_isave] = IFUN;
            ISAVE[16 + o_isave] = IWORD;
            ISAVE[17 + o_isave] = NFREE;
            ISAVE[18 + o_isave] = NACT;
            ISAVE[19 + o_isave] = ILEAVE;
            ISAVE[20 + o_isave] = NENTER;
            
            DSAVE[1 + o_dsave] = THETA;
            DSAVE[2 + o_dsave] = FOLD;
            DSAVE[3 + o_dsave] = TOL;
            DSAVE[4 + o_dsave] = DNORM;
            DSAVE[5 + o_dsave] = EPSMCH;
            DSAVE[6 + o_dsave] = CPU1;
            DSAVE[7 + o_dsave] = CACHYT;
            DSAVE[8 + o_dsave] = SBTIME;
            DSAVE[9 + o_dsave] = LNSCHT;
            DSAVE[10 + o_dsave] = TIME1;
            DSAVE[11 + o_dsave] = GD;
            DSAVE[12 + o_dsave] = STPMX;
            DSAVE[13 + o_dsave] = SBGNRM;
            DSAVE[14 + o_dsave] = STP;
            DSAVE[15 + o_dsave] = GDOLD;
            DSAVE[16 + o_dsave] = DTD;
            
            
            return;
            

            #endregion

        }
    }
    
    // c======================= The end of mainlb =============================
}
