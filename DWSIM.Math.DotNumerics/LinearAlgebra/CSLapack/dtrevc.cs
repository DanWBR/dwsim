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

namespace DotNumerics.LinearAlgebra.CSLapack
{
    /// <summary>
    /// -- LAPACK routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DTREVC computes some or all of the right and/or left eigenvectors of
    /// a real upper quasi-triangular matrix T.
    /// Matrices of this type are produced by the Schur factorization of
    /// a real general matrix:  A = Q*T*Q**T, as computed by DHSEQR.
    /// 
    /// The right eigenvector x and the left eigenvector y of T corresponding
    /// to an eigenvalue w are defined by:
    /// 
    /// T*x = w*x,     (y**H)*T = w*(y**H)
    /// 
    /// where y**H denotes the conjugate transpose of y.
    /// The eigenvalues are not input to this routine, but are read directly
    /// from the diagonal blocks of T.
    /// 
    /// This routine returns the matrices X and/or Y of right and left
    /// eigenvectors of T, or the products Q*X and/or Q*Y, where Q is an
    /// input matrix.  If Q is the orthogonal factor that reduces a matrix
    /// A to Schur form T, then Q*X and Q*Y are the matrices of right and
    /// left eigenvectors of A.
    /// 
    ///</summary>
    public class DTREVC
    {
    

        #region Dependencies
        
        LSAME _lsame; IDAMAX _idamax; DDOT _ddot; DLAMCH _dlamch; DAXPY _daxpy; DCOPY _dcopy; DGEMV _dgemv; DLALN2 _dlaln2; 
        DSCAL _dscal;XERBLA _xerbla; DLABAD _dlabad; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; double[] X = new double[2 * 2]; 

        #endregion

        public DTREVC(LSAME lsame, IDAMAX idamax, DDOT ddot, DLAMCH dlamch, DAXPY daxpy, DCOPY dcopy, DGEMV dgemv, DLALN2 dlaln2, DSCAL dscal, XERBLA xerbla
                      , DLABAD dlabad)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._idamax = idamax; this._ddot = ddot; this._dlamch = dlamch; this._daxpy = daxpy; 
            this._dcopy = dcopy;this._dgemv = dgemv; this._dlaln2 = dlaln2; this._dscal = dscal; this._xerbla = xerbla; 
            this._dlabad = dlabad;

            #endregion

        }
    
        public DTREVC()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            IDAMAX idamax = new IDAMAX();
            DDOT ddot = new DDOT();
            DLAMC3 dlamc3 = new DLAMC3();
            DAXPY daxpy = new DAXPY();
            DCOPY dcopy = new DCOPY();
            XERBLA xerbla = new XERBLA();
            DLADIV dladiv = new DLADIV();
            DSCAL dscal = new DSCAL();
            DLABAD dlabad = new DLABAD();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DLALN2 dlaln2 = new DLALN2(dlamch, dladiv);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._idamax = idamax; this._ddot = ddot; this._dlamch = dlamch; this._daxpy = daxpy; 
            this._dcopy = dcopy;this._dgemv = dgemv; this._dlaln2 = dlaln2; this._dscal = dscal; this._xerbla = xerbla; 
            this._dlabad = dlabad;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DTREVC computes some or all of the right and/or left eigenvectors of
        /// a real upper quasi-triangular matrix T.
        /// Matrices of this type are produced by the Schur factorization of
        /// a real general matrix:  A = Q*T*Q**T, as computed by DHSEQR.
        /// 
        /// The right eigenvector x and the left eigenvector y of T corresponding
        /// to an eigenvalue w are defined by:
        /// 
        /// T*x = w*x,     (y**H)*T = w*(y**H)
        /// 
        /// where y**H denotes the conjugate transpose of y.
        /// The eigenvalues are not input to this routine, but are read directly
        /// from the diagonal blocks of T.
        /// 
        /// This routine returns the matrices X and/or Y of right and left
        /// eigenvectors of T, or the products Q*X and/or Q*Y, where Q is an
        /// input matrix.  If Q is the orthogonal factor that reduces a matrix
        /// A to Schur form T, then Q*X and Q*Y are the matrices of right and
        /// left eigenvectors of A.
        /// 
        ///</summary>
        /// <param name="SIDE">
        /// (input) CHARACTER*1
        /// = 'R':  compute right eigenvectors only;
        /// = 'L':  compute left eigenvectors only;
        /// = 'B':  compute both right and left eigenvectors.
        ///</param>
        /// <param name="HOWMNY">
        /// (input) CHARACTER*1
        /// = 'A':  compute all right and/or left eigenvectors;
        /// = 'B':  compute all right and/or left eigenvectors,
        /// backtransformed by the matrices in VR and/or VL;
        /// = 'S':  compute selected right and/or left eigenvectors,
        /// as indicated by the logical array SELECT.
        ///</param>
        /// <param name="SELECT">
        /// (input/output) LOGICAL array, dimension (N)
        /// If HOWMNY = 'S', SELECT specifies the eigenvectors to be
        /// computed.
        /// If w(j) is a real eigenvalue, the corresponding real
        /// eigenvector is computed if SELECT(j) is .TRUE..
        /// If w(j) and w(j+1) are the real and imaginary parts of a
        /// complex eigenvalue, the corresponding complex eigenvector is
        /// computed if either SELECT(j) or SELECT(j+1) is .TRUE., and
        /// on exit SELECT(j) is set to .TRUE. and SELECT(j+1) is set to
        /// .FALSE..
        /// Not referenced if HOWMNY = 'A' or 'B'.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix T. N .GE. 0.
        ///</param>
        /// <param name="T">
        /// (input) DOUBLE PRECISION array, dimension (LDT,N)
        /// The upper quasi-triangular matrix T in Schur canonical form.
        ///</param>
        /// <param name="LDT">
        /// (input) INTEGER
        /// The leading dimension of the array T. LDT .GE. max(1,N).
        ///</param>
        /// <param name="VL">
        /// (input/output) DOUBLE PRECISION array, dimension (LDVL,MM)
        /// On entry, if SIDE = 'L' or 'B' and HOWMNY = 'B', VL must
        /// contain an N-by-N matrix Q (usually the orthogonal matrix Q
        /// of Schur vectors returned by DHSEQR).
        /// On exit, if SIDE = 'L' or 'B', VL contains:
        /// if HOWMNY = 'A', the matrix Y of left eigenvectors of T;
        /// if HOWMNY = 'B', the matrix Q*Y;
        /// if HOWMNY = 'S', the left eigenvectors of T specified by
        /// SELECT, stored consecutively in the columns
        /// of VL, in the same order as their
        /// eigenvalues.
        /// A complex eigenvector corresponding to a complex eigenvalue
        /// is stored in two consecutive columns, the first holding the
        /// real part, and the second the imaginary part.
        /// Not referenced if SIDE = 'R'.
        ///</param>
        /// <param name="LDVL">
        /// (input) INTEGER
        /// The leading dimension of the array VL.  LDVL .GE. 1, and if
        /// SIDE = 'L' or 'B', LDVL .GE. N.
        ///</param>
        /// <param name="VR">
        /// (input/output) DOUBLE PRECISION array, dimension (LDVR,MM)
        /// On entry, if SIDE = 'R' or 'B' and HOWMNY = 'B', VR must
        /// contain an N-by-N matrix Q (usually the orthogonal matrix Q
        /// of Schur vectors returned by DHSEQR).
        /// On exit, if SIDE = 'R' or 'B', VR contains:
        /// if HOWMNY = 'A', the matrix X of right eigenvectors of T;
        /// if HOWMNY = 'B', the matrix Q*X;
        /// if HOWMNY = 'S', the right eigenvectors of T specified by
        /// SELECT, stored consecutively in the columns
        /// of VR, in the same order as their
        /// eigenvalues.
        /// A complex eigenvector corresponding to a complex eigenvalue
        /// is stored in two consecutive columns, the first holding the
        /// real part and the second the imaginary part.
        /// Not referenced if SIDE = 'L'.
        ///</param>
        /// <param name="LDVR">
        /// (input) INTEGER
        /// The leading dimension of the array VR.  LDVR .GE. 1, and if
        /// SIDE = 'R' or 'B', LDVR .GE. N.
        ///</param>
        /// <param name="MM">
        /// (input) INTEGER
        /// The number of columns in the arrays VL and/or VR. MM .GE. M.
        ///</param>
        /// <param name="M">
        /// (output) INTEGER
        /// The number of columns in the arrays VL and/or VR actually
        /// used to store the eigenvectors.
        /// If HOWMNY = 'A' or 'B', M is set to N.
        /// Each selected real eigenvector occupies one column and each
        /// selected complex eigenvector occupies two columns.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (3*N)
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
        ///</param>
        public void Run(string SIDE, string HOWMNY, ref bool[] SELECT, int offset_select, int N, double[] T, int offset_t, int LDT
                         , ref double[] VL, int offset_vl, int LDVL, ref double[] VR, int offset_vr, int LDVR, int MM, ref int M
                         , ref double[] WORK, int offset_work, ref int INFO)
        {

            #region Variables
            
            bool ALLV = false; bool BOTHV = false; bool LEFTV = false; bool OVER = false; bool PAIR = false; bool RIGHTV = false; 
            bool SOMEV = false;int I = 0; int IERR = 0; int II = 0; int IP = 0; int IS = 0; int J = 0; int J1 = 0; int J2 = 0; 
            int JNXT = 0;int K = 0; int KI = 0; int N2 = 0; double BETA = 0; double BIGNUM = 0; double EMAX = 0; double OVFL = 0; 
            double REC = 0;double REMAX = 0; double SCALE = 0; double SMIN = 0; double SMLNUM = 0; double ULP = 0; 
            double UNFL = 0;double VCRIT = 0; double VMAX = 0; double WI = 0; double WR = 0; double XNORM = 0; 
            int offset_x = 0; int o_x = -3;

            #endregion


            #region Implicit Variables
            
            int VR_IS = 0; int VR_0 = 0; int VL_IS = 0; int VL_0 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_select = -1 + offset_select;  int o_t = -1 - LDT + offset_t;  int o_vl = -1 - LDVL + offset_vl; 
             int o_vr = -1 - LDVR + offset_vr; int o_work = -1 + offset_work; 

            #endregion


            #region Strings
            
            SIDE = SIDE.Substring(0, 1);  HOWMNY = HOWMNY.Substring(0, 1);  

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK routine (version 3.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     November 2006
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DTREVC computes some or all of the right and/or left eigenvectors of
            // *  a real upper quasi-triangular matrix T.
            // *  Matrices of this type are produced by the Schur factorization of
            // *  a real general matrix:  A = Q*T*Q**T, as computed by DHSEQR.
            // *  
            // *  The right eigenvector x and the left eigenvector y of T corresponding
            // *  to an eigenvalue w are defined by:
            // *  
            // *     T*x = w*x,     (y**H)*T = w*(y**H)
            // *  
            // *  where y**H denotes the conjugate transpose of y.
            // *  The eigenvalues are not input to this routine, but are read directly
            // *  from the diagonal blocks of T.
            // *  
            // *  This routine returns the matrices X and/or Y of right and left
            // *  eigenvectors of T, or the products Q*X and/or Q*Y, where Q is an
            // *  input matrix.  If Q is the orthogonal factor that reduces a matrix
            // *  A to Schur form T, then Q*X and Q*Y are the matrices of right and
            // *  left eigenvectors of A.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  SIDE    (input) CHARACTER*1
            // *          = 'R':  compute right eigenvectors only;
            // *          = 'L':  compute left eigenvectors only;
            // *          = 'B':  compute both right and left eigenvectors.
            // *
            // *  HOWMNY  (input) CHARACTER*1
            // *          = 'A':  compute all right and/or left eigenvectors;
            // *          = 'B':  compute all right and/or left eigenvectors,
            // *                  backtransformed by the matrices in VR and/or VL;
            // *          = 'S':  compute selected right and/or left eigenvectors,
            // *                  as indicated by the logical array SELECT.
            // *
            // *  SELECT  (input/output) LOGICAL array, dimension (N)
            // *          If HOWMNY = 'S', SELECT specifies the eigenvectors to be
            // *          computed.
            // *          If w(j) is a real eigenvalue, the corresponding real
            // *          eigenvector is computed if SELECT(j) is .TRUE..
            // *          If w(j) and w(j+1) are the real and imaginary parts of a
            // *          complex eigenvalue, the corresponding complex eigenvector is
            // *          computed if either SELECT(j) or SELECT(j+1) is .TRUE., and
            // *          on exit SELECT(j) is set to .TRUE. and SELECT(j+1) is set to
            // *          .FALSE..
            // *          Not referenced if HOWMNY = 'A' or 'B'.
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix T. N >= 0.
            // *
            // *  T       (input) DOUBLE PRECISION array, dimension (LDT,N)
            // *          The upper quasi-triangular matrix T in Schur canonical form.
            // *
            // *  LDT     (input) INTEGER
            // *          The leading dimension of the array T. LDT >= max(1,N).
            // *
            // *  VL      (input/output) DOUBLE PRECISION array, dimension (LDVL,MM)
            // *          On entry, if SIDE = 'L' or 'B' and HOWMNY = 'B', VL must
            // *          contain an N-by-N matrix Q (usually the orthogonal matrix Q
            // *          of Schur vectors returned by DHSEQR).
            // *          On exit, if SIDE = 'L' or 'B', VL contains:
            // *          if HOWMNY = 'A', the matrix Y of left eigenvectors of T;
            // *          if HOWMNY = 'B', the matrix Q*Y;
            // *          if HOWMNY = 'S', the left eigenvectors of T specified by
            // *                           SELECT, stored consecutively in the columns
            // *                           of VL, in the same order as their
            // *                           eigenvalues.
            // *          A complex eigenvector corresponding to a complex eigenvalue
            // *          is stored in two consecutive columns, the first holding the
            // *          real part, and the second the imaginary part.
            // *          Not referenced if SIDE = 'R'.
            // *
            // *  LDVL    (input) INTEGER
            // *          The leading dimension of the array VL.  LDVL >= 1, and if
            // *          SIDE = 'L' or 'B', LDVL >= N.
            // *
            // *  VR      (input/output) DOUBLE PRECISION array, dimension (LDVR,MM)
            // *          On entry, if SIDE = 'R' or 'B' and HOWMNY = 'B', VR must
            // *          contain an N-by-N matrix Q (usually the orthogonal matrix Q
            // *          of Schur vectors returned by DHSEQR).
            // *          On exit, if SIDE = 'R' or 'B', VR contains:
            // *          if HOWMNY = 'A', the matrix X of right eigenvectors of T;
            // *          if HOWMNY = 'B', the matrix Q*X;
            // *          if HOWMNY = 'S', the right eigenvectors of T specified by
            // *                           SELECT, stored consecutively in the columns
            // *                           of VR, in the same order as their
            // *                           eigenvalues.
            // *          A complex eigenvector corresponding to a complex eigenvalue
            // *          is stored in two consecutive columns, the first holding the
            // *          real part and the second the imaginary part.
            // *          Not referenced if SIDE = 'L'.
            // *
            // *  LDVR    (input) INTEGER
            // *          The leading dimension of the array VR.  LDVR >= 1, and if
            // *          SIDE = 'R' or 'B', LDVR >= N.
            // *
            // *  MM      (input) INTEGER
            // *          The number of columns in the arrays VL and/or VR. MM >= M.
            // *
            // *  M       (output) INTEGER
            // *          The number of columns in the arrays VL and/or VR actually
            // *          used to store the eigenvectors.
            // *          If HOWMNY = 'A' or 'B', M is set to N.
            // *          Each selected real eigenvector occupies one column and each
            // *          selected complex eigenvector occupies two columns.
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (3*N)
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  The algorithm used in this program is basically backward (forward)
            // *  substitution, with scaling to make the the code robust against
            // *  possible overflow.
            // *
            // *  Each eigenvector is normalized so that the element of largest
            // *  magnitude has magnitude 1; here the magnitude of a complex number
            // *  (x,y) is taken to be |x| + |y|.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, MAX, SQRT;
            // *     ..
            // *     .. Local Arrays ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Decode and test the input parameters
            // *

            #endregion


            #region Body
            
            BOTHV = this._lsame.Run(SIDE, "B");
            RIGHTV = this._lsame.Run(SIDE, "R") || BOTHV;
            LEFTV = this._lsame.Run(SIDE, "L") || BOTHV;
            // *
            ALLV = this._lsame.Run(HOWMNY, "A");
            OVER = this._lsame.Run(HOWMNY, "B");
            SOMEV = this._lsame.Run(HOWMNY, "S");
            // *
            INFO = 0;
            if (!RIGHTV && !LEFTV)
            {
                INFO =  - 1;
            }
            else
            {
                if (!ALLV && !OVER && !SOMEV)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (N < 0)
                    {
                        INFO =  - 4;
                    }
                    else
                    {
                        if (LDT < Math.Max(1, N))
                        {
                            INFO =  - 6;
                        }
                        else
                        {
                            if (LDVL < 1 || (LEFTV && LDVL < N))
                            {
                                INFO =  - 8;
                            }
                            else
                            {
                                if (LDVR < 1 || (RIGHTV && LDVR < N))
                                {
                                    INFO =  - 10;
                                }
                                else
                                {
                                    // *
                                    // *        Set M to the number of columns required to store the selected
                                    // *        eigenvectors, standardize the array SELECT if necessary, and
                                    // *        test MM.
                                    // *
                                    if (SOMEV)
                                    {
                                        M = 0;
                                        PAIR = false;
                                        for (J = 1; J <= N; J++)
                                        {
                                            if (PAIR)
                                            {
                                                PAIR = false;
                                                SELECT[J + o_select] = false;
                                            }
                                            else
                                            {
                                                if (J < N)
                                                {
                                                    if (T[J + 1+J * LDT + o_t] == ZERO)
                                                    {
                                                        if (SELECT[J + o_select]) M += 1;
                                                    }
                                                    else
                                                    {
                                                        PAIR = true;
                                                        if (SELECT[J + o_select] || SELECT[J + 1 + o_select])
                                                        {
                                                            SELECT[J + o_select] = true;
                                                            M += 2;
                                                        }
                                                    }
                                                }
                                                else
                                                {
                                                    if (SELECT[N + o_select]) M += 1;
                                                }
                                            }
                                        }
                                    }
                                    else
                                    {
                                        M = N;
                                    }
                                    // *
                                    if (MM < M)
                                    {
                                        INFO =  - 11;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DTREVC",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible.
            // *
            if (N == 0) return;
            // *
            // *     Set the constants to control overflow.
            // *
            UNFL = this._dlamch.Run("Safe minimum");
            OVFL = ONE / UNFL;
            this._dlabad.Run(ref UNFL, ref OVFL);
            ULP = this._dlamch.Run("Precision");
            SMLNUM = UNFL * (N / ULP);
            BIGNUM = (ONE - ULP) / SMLNUM;
            // *
            // *     Compute 1-norm of each column of strictly upper triangular
            // *     part of T to control overflow in triangular solver.
            // *
            WORK[1 + o_work] = ZERO;
            for (J = 2; J <= N; J++)
            {
                WORK[J + o_work] = ZERO;
                for (I = 1; I <= J - 1; I++)
                {
                    WORK[J + o_work] += Math.Abs(T[I+J * LDT + o_t]);
                }
            }
            // *
            // *     Index IP is used to specify the real or complex eigenvalue:
            // *       IP = 0, real eigenvalue,
            // *            1, first of conjugate complex pair: (wr,wi)
            // *           -1, second of conjugate complex pair: (wr,wi)
            // *
            N2 = 2 * N;
            // *
            if (RIGHTV)
            {
                // *
                // *        Compute right eigenvectors.
                // *
                IP = 0;
                IS = M;
                for (KI = N; KI >= 1; KI +=  - 1)
                {
                    // *
                    if (IP == 1) goto LABEL130;
                    if (KI == 1) goto LABEL40;
                    if (T[KI+(KI - 1) * LDT + o_t] == ZERO) goto LABEL40;
                    IP =  - 1;
                    // *
                LABEL40:;
                    if (SOMEV)
                    {
                        if (IP == 0)
                        {
                            if (!SELECT[KI + o_select]) goto LABEL130;
                        }
                        else
                        {
                            if (!SELECT[KI - 1 + o_select]) goto LABEL130;
                        }
                    }
                    // *
                    // *           Compute the KI-th eigenvalue (WR,WI).
                    // *
                    WR = T[KI+KI * LDT + o_t];
                    WI = ZERO;
                    if (IP != 0) WI = Math.Sqrt(Math.Abs(T[KI+(KI - 1) * LDT + o_t])) * Math.Sqrt(Math.Abs(T[KI - 1+KI * LDT + o_t]));
                    SMIN = Math.Max(ULP * (Math.Abs(WR) + Math.Abs(WI)), SMLNUM);
                    // *
                    if (IP == 0)
                    {
                        // *
                        // *              Real right eigenvector
                        // *
                        WORK[KI + N + o_work] = ONE;
                        // *
                        // *              Form right-hand side
                        // *
                        for (K = 1; K <= KI - 1; K++)
                        {
                            WORK[K + N + o_work] =  - T[K+KI * LDT + o_t];
                        }
                        // *
                        // *              Solve the upper quasi-triangular system:
                        // *                 (T(1:KI-1,1:KI-1) - WR)*X = SCALE*WORK.
                        // *
                        JNXT = KI - 1;
                        for (J = KI - 1; J >= 1; J +=  - 1)
                        {
                            if (J > JNXT) goto LABEL60;
                            J1 = J;
                            J2 = J;
                            JNXT = J - 1;
                            if (J > 1)
                            {
                                if (T[J+(J - 1) * LDT + o_t] != ZERO)
                                {
                                    J1 = J - 1;
                                    JNXT = J - 2;
                                }
                            }
                            // *
                            if (J1 == J2)
                            {
                                // *
                                // *                    1-by-1 diagonal block
                                // *
                                this._dlaln2.Run(false, 1, 1, SMIN, ONE, T, J+J * LDT + o_t
                                                 , LDT, ONE, ONE, WORK, J + N + o_work, N, WR
                                                 , ZERO, ref X, offset_x, 2, ref SCALE, ref XNORM, ref IERR);
                                // *
                                // *                    Scale X(1,1) to avoid overflow when updating
                                // *                    the right-hand side.
                                // *
                                if (XNORM > ONE)
                                {
                                    if (WORK[J + o_work] > BIGNUM / XNORM)
                                    {
                                        X[1+1 * 2 + o_x] /= XNORM;
                                        SCALE /= XNORM;
                                    }
                                }
                                // *
                                // *                    Scale if necessary
                                // *
                                if (SCALE != ONE) this._dscal.Run(KI, SCALE, ref WORK, 1 + N + o_work, 1);
                                WORK[J + N + o_work] = X[1+1 * 2 + o_x];
                                // *
                                // *                    Update right-hand side
                                // *
                                this._daxpy.Run(J - 1,  - X[1+1 * 2 + o_x], T, 1+J * LDT + o_t, 1, ref WORK, 1 + N + o_work, 1);
                                // *
                            }
                            else
                            {
                                // *
                                // *                    2-by-2 diagonal block
                                // *
                                this._dlaln2.Run(false, 2, 1, SMIN, ONE, T, J - 1+(J - 1) * LDT + o_t
                                                 , LDT, ONE, ONE, WORK, J - 1 + N + o_work, N, WR
                                                 , ZERO, ref X, offset_x, 2, ref SCALE, ref XNORM, ref IERR);
                                // *
                                // *                    Scale X(1,1) and X(2,1) to avoid overflow when
                                // *                    updating the right-hand side.
                                // *
                                if (XNORM > ONE)
                                {
                                    BETA = Math.Max(WORK[J - 1 + o_work], WORK[J + o_work]);
                                    if (BETA > BIGNUM / XNORM)
                                    {
                                        X[1+1 * 2 + o_x] /= XNORM;
                                        X[2+1 * 2 + o_x] /= XNORM;
                                        SCALE /= XNORM;
                                    }
                                }
                                // *
                                // *                    Scale if necessary
                                // *
                                if (SCALE != ONE) this._dscal.Run(KI, SCALE, ref WORK, 1 + N + o_work, 1);
                                WORK[J - 1 + N + o_work] = X[1+1 * 2 + o_x];
                                WORK[J + N + o_work] = X[2+1 * 2 + o_x];
                                // *
                                // *                    Update right-hand side
                                // *
                                this._daxpy.Run(J - 2,  - X[1+1 * 2 + o_x], T, 1+(J - 1) * LDT + o_t, 1, ref WORK, 1 + N + o_work, 1);
                                this._daxpy.Run(J - 2,  - X[2+1 * 2 + o_x], T, 1+J * LDT + o_t, 1, ref WORK, 1 + N + o_work, 1);
                            }
                        LABEL60:;
                        }
                        // *
                        // *              Copy the vector x or Q*x to VR and normalize.
                        // *
                        if (!OVER)
                        {
                            this._dcopy.Run(KI, WORK, 1 + N + o_work, 1, ref VR, 1+IS * LDVR + o_vr, 1);
                            // *
                            II = this._idamax.Run(KI, VR, 1+IS * LDVR + o_vr, 1);
                            REMAX = ONE / Math.Abs(VR[II+IS * LDVR + o_vr]);
                            this._dscal.Run(KI, REMAX, ref VR, 1+IS * LDVR + o_vr, 1);
                            // *
                            VR_IS = IS * LDVR + o_vr;
                            for (K = KI + 1; K <= N; K++)
                            {
                                VR[K + VR_IS] = ZERO;
                            }
                        }
                        else
                        {
                            if (KI > 1)
                            {
                                this._dgemv.Run("N", N, KI - 1, ONE, VR, offset_vr, LDVR
                                                , WORK, 1 + N + o_work, 1, WORK[KI + N + o_work], ref VR, 1+KI * LDVR + o_vr, 1);
                            }
                            // *
                            II = this._idamax.Run(N, VR, 1+KI * LDVR + o_vr, 1);
                            REMAX = ONE / Math.Abs(VR[II+KI * LDVR + o_vr]);
                            this._dscal.Run(N, REMAX, ref VR, 1+KI * LDVR + o_vr, 1);
                        }
                        // *
                    }
                    else
                    {
                        // *
                        // *              Complex right eigenvector.
                        // *
                        // *              Initial solve
                        // *                [ (T(KI-1,KI-1) T(KI-1,KI) ) - (WR + I* WI)]*X = 0.
                        // *                [ (T(KI,KI-1)   T(KI,KI)   )               ]
                        // *
                        if (Math.Abs(T[KI - 1+KI * LDT + o_t]) >= Math.Abs(T[KI+(KI - 1) * LDT + o_t]))
                        {
                            WORK[KI - 1 + N + o_work] = ONE;
                            WORK[KI + N2 + o_work] = WI / T[KI - 1+KI * LDT + o_t];
                        }
                        else
                        {
                            WORK[KI - 1 + N + o_work] =  - WI / T[KI+(KI - 1) * LDT + o_t];
                            WORK[KI + N2 + o_work] = ONE;
                        }
                        WORK[KI + N + o_work] = ZERO;
                        WORK[KI - 1 + N2 + o_work] = ZERO;
                        // *
                        // *              Form right-hand side
                        // *
                        for (K = 1; K <= KI - 2; K++)
                        {
                            WORK[K + N + o_work] =  - WORK[KI - 1 + N + o_work] * T[K+(KI - 1) * LDT + o_t];
                            WORK[K + N2 + o_work] =  - WORK[KI + N2 + o_work] * T[K+KI * LDT + o_t];
                        }
                        // *
                        // *              Solve upper quasi-triangular system:
                        // *              (T(1:KI-2,1:KI-2) - (WR+i*WI))*X = SCALE*(WORK+i*WORK2)
                        // *
                        JNXT = KI - 2;
                        for (J = KI - 2; J >= 1; J +=  - 1)
                        {
                            if (J > JNXT) goto LABEL90;
                            J1 = J;
                            J2 = J;
                            JNXT = J - 1;
                            if (J > 1)
                            {
                                if (T[J+(J - 1) * LDT + o_t] != ZERO)
                                {
                                    J1 = J - 1;
                                    JNXT = J - 2;
                                }
                            }
                            // *
                            if (J1 == J2)
                            {
                                // *
                                // *                    1-by-1 diagonal block
                                // *
                                this._dlaln2.Run(false, 1, 2, SMIN, ONE, T, J+J * LDT + o_t
                                                 , LDT, ONE, ONE, WORK, J + N + o_work, N, WR
                                                 , WI, ref X, offset_x, 2, ref SCALE, ref XNORM, ref IERR);
                                // *
                                // *                    Scale X(1,1) and X(1,2) to avoid overflow when
                                // *                    updating the right-hand side.
                                // *
                                if (XNORM > ONE)
                                {
                                    if (WORK[J + o_work] > BIGNUM / XNORM)
                                    {
                                        X[1+1 * 2 + o_x] /= XNORM;
                                        X[1+2 * 2 + o_x] /= XNORM;
                                        SCALE /= XNORM;
                                    }
                                }
                                // *
                                // *                    Scale if necessary
                                // *
                                if (SCALE != ONE)
                                {
                                    this._dscal.Run(KI, SCALE, ref WORK, 1 + N + o_work, 1);
                                    this._dscal.Run(KI, SCALE, ref WORK, 1 + N2 + o_work, 1);
                                }
                                WORK[J + N + o_work] = X[1+1 * 2 + o_x];
                                WORK[J + N2 + o_work] = X[1+2 * 2 + o_x];
                                // *
                                // *                    Update the right-hand side
                                // *
                                this._daxpy.Run(J - 1,  - X[1+1 * 2 + o_x], T, 1+J * LDT + o_t, 1, ref WORK, 1 + N + o_work, 1);
                                this._daxpy.Run(J - 1,  - X[1+2 * 2 + o_x], T, 1+J * LDT + o_t, 1, ref WORK, 1 + N2 + o_work, 1);
                                // *
                            }
                            else
                            {
                                // *
                                // *                    2-by-2 diagonal block
                                // *
                                this._dlaln2.Run(false, 2, 2, SMIN, ONE, T, J - 1+(J - 1) * LDT + o_t
                                                 , LDT, ONE, ONE, WORK, J - 1 + N + o_work, N, WR
                                                 , WI, ref X, offset_x, 2, ref SCALE, ref XNORM, ref IERR);
                                // *
                                // *                    Scale X to avoid overflow when updating
                                // *                    the right-hand side.
                                // *
                                if (XNORM > ONE)
                                {
                                    BETA = Math.Max(WORK[J - 1 + o_work], WORK[J + o_work]);
                                    if (BETA > BIGNUM / XNORM)
                                    {
                                        REC = ONE / XNORM;
                                        X[1+1 * 2 + o_x] *= REC;
                                        X[1+2 * 2 + o_x] *= REC;
                                        X[2+1 * 2 + o_x] *= REC;
                                        X[2+2 * 2 + o_x] *= REC;
                                        SCALE *= REC;
                                    }
                                }
                                // *
                                // *                    Scale if necessary
                                // *
                                if (SCALE != ONE)
                                {
                                    this._dscal.Run(KI, SCALE, ref WORK, 1 + N + o_work, 1);
                                    this._dscal.Run(KI, SCALE, ref WORK, 1 + N2 + o_work, 1);
                                }
                                WORK[J - 1 + N + o_work] = X[1+1 * 2 + o_x];
                                WORK[J + N + o_work] = X[2+1 * 2 + o_x];
                                WORK[J - 1 + N2 + o_work] = X[1+2 * 2 + o_x];
                                WORK[J + N2 + o_work] = X[2+2 * 2 + o_x];
                                // *
                                // *                    Update the right-hand side
                                // *
                                this._daxpy.Run(J - 2,  - X[1+1 * 2 + o_x], T, 1+(J - 1) * LDT + o_t, 1, ref WORK, 1 + N + o_work, 1);
                                this._daxpy.Run(J - 2,  - X[2+1 * 2 + o_x], T, 1+J * LDT + o_t, 1, ref WORK, 1 + N + o_work, 1);
                                this._daxpy.Run(J - 2,  - X[1+2 * 2 + o_x], T, 1+(J - 1) * LDT + o_t, 1, ref WORK, 1 + N2 + o_work, 1);
                                this._daxpy.Run(J - 2,  - X[2+2 * 2 + o_x], T, 1+J * LDT + o_t, 1, ref WORK, 1 + N2 + o_work, 1);
                            }
                        LABEL90:;
                        }
                        // *
                        // *              Copy the vector x or Q*x to VR and normalize.
                        // *
                        if (!OVER)
                        {
                            this._dcopy.Run(KI, WORK, 1 + N + o_work, 1, ref VR, 1+(IS - 1) * LDVR + o_vr, 1);
                            this._dcopy.Run(KI, WORK, 1 + N2 + o_work, 1, ref VR, 1+IS * LDVR + o_vr, 1);
                            // *
                            EMAX = ZERO;
                            for (K = 1; K <= KI; K++)
                            {
                                EMAX = Math.Max(EMAX, Math.Abs(VR[K+(IS - 1) * LDVR + o_vr]) + Math.Abs(VR[K+IS * LDVR + o_vr]));
                            }
                            // *
                            REMAX = ONE / EMAX;
                            this._dscal.Run(KI, REMAX, ref VR, 1+(IS - 1) * LDVR + o_vr, 1);
                            this._dscal.Run(KI, REMAX, ref VR, 1+IS * LDVR + o_vr, 1);
                            // *
                            VR_0 = (IS - 1) * LDVR + o_vr;
                            VR_IS = IS * LDVR + o_vr;
                            for (K = KI + 1; K <= N; K++)
                            {
                                VR[K + VR_0] = ZERO;
                                VR[K + VR_IS] = ZERO;
                            }
                            // *
                        }
                        else
                        {
                            // *
                            if (KI > 2)
                            {
                                this._dgemv.Run("N", N, KI - 2, ONE, VR, offset_vr, LDVR
                                                , WORK, 1 + N + o_work, 1, WORK[KI - 1 + N + o_work], ref VR, 1+(KI - 1) * LDVR + o_vr, 1);
                                this._dgemv.Run("N", N, KI - 2, ONE, VR, offset_vr, LDVR
                                                , WORK, 1 + N2 + o_work, 1, WORK[KI + N2 + o_work], ref VR, 1+KI * LDVR + o_vr, 1);
                            }
                            else
                            {
                                this._dscal.Run(N, WORK[KI - 1 + N + o_work], ref VR, 1+(KI - 1) * LDVR + o_vr, 1);
                                this._dscal.Run(N, WORK[KI + N2 + o_work], ref VR, 1+KI * LDVR + o_vr, 1);
                            }
                            // *
                            EMAX = ZERO;
                            for (K = 1; K <= N; K++)
                            {
                                EMAX = Math.Max(EMAX, Math.Abs(VR[K+(KI - 1) * LDVR + o_vr]) + Math.Abs(VR[K+KI * LDVR + o_vr]));
                            }
                            REMAX = ONE / EMAX;
                            this._dscal.Run(N, REMAX, ref VR, 1+(KI - 1) * LDVR + o_vr, 1);
                            this._dscal.Run(N, REMAX, ref VR, 1+KI * LDVR + o_vr, 1);
                        }
                    }
                    // *
                    IS -= 1;
                    if (IP != 0) IS -= 1;
                LABEL130:;
                    if (IP == 1) IP = 0;
                    if (IP ==  - 1) IP = 1;
                }
            }
            // *
            if (LEFTV)
            {
                // *
                // *        Compute left eigenvectors.
                // *
                IP = 0;
                IS = 1;
                for (KI = 1; KI <= N; KI++)
                {
                    // *
                    if (IP ==  - 1) goto LABEL250;
                    if (KI == N) goto LABEL150;
                    if (T[KI + 1+KI * LDT + o_t] == ZERO) goto LABEL150;
                    IP = 1;
                    // *
                LABEL150:;
                    if (SOMEV)
                    {
                        if (!SELECT[KI + o_select]) goto LABEL250;
                    }
                    // *
                    // *           Compute the KI-th eigenvalue (WR,WI).
                    // *
                    WR = T[KI+KI * LDT + o_t];
                    WI = ZERO;
                    if (IP != 0) WI = Math.Sqrt(Math.Abs(T[KI+(KI + 1) * LDT + o_t])) * Math.Sqrt(Math.Abs(T[KI + 1+KI * LDT + o_t]));
                    SMIN = Math.Max(ULP * (Math.Abs(WR) + Math.Abs(WI)), SMLNUM);
                    // *
                    if (IP == 0)
                    {
                        // *
                        // *              Real left eigenvector.
                        // *
                        WORK[KI + N + o_work] = ONE;
                        // *
                        // *              Form right-hand side
                        // *
                        for (K = KI + 1; K <= N; K++)
                        {
                            WORK[K + N + o_work] =  - T[KI+K * LDT + o_t];
                        }
                        // *
                        // *              Solve the quasi-triangular system:
                        // *                 (T(KI+1:N,KI+1:N) - WR)'*X = SCALE*WORK
                        // *
                        VMAX = ONE;
                        VCRIT = BIGNUM;
                        // *
                        JNXT = KI + 1;
                        for (J = KI + 1; J <= N; J++)
                        {
                            if (J < JNXT) goto LABEL170;
                            J1 = J;
                            J2 = J;
                            JNXT = J + 1;
                            if (J < N)
                            {
                                if (T[J + 1+J * LDT + o_t] != ZERO)
                                {
                                    J2 = J + 1;
                                    JNXT = J + 2;
                                }
                            }
                            // *
                            if (J1 == J2)
                            {
                                // *
                                // *                    1-by-1 diagonal block
                                // *
                                // *                    Scale if necessary to avoid overflow when forming
                                // *                    the right-hand side.
                                // *
                                if (WORK[J + o_work] > VCRIT)
                                {
                                    REC = ONE / VMAX;
                                    this._dscal.Run(N - KI + 1, REC, ref WORK, KI + N + o_work, 1);
                                    VMAX = ONE;
                                    VCRIT = BIGNUM;
                                }
                                // *
                                WORK[J + N + o_work] -= this._ddot.Run(J - KI - 1, T, KI + 1+J * LDT + o_t, 1, WORK, KI + 1 + N + o_work, 1);
                                // *
                                // *                    Solve (T(J,J)-WR)'*X = WORK
                                // *
                                this._dlaln2.Run(false, 1, 1, SMIN, ONE, T, J+J * LDT + o_t
                                                 , LDT, ONE, ONE, WORK, J + N + o_work, N, WR
                                                 , ZERO, ref X, offset_x, 2, ref SCALE, ref XNORM, ref IERR);
                                // *
                                // *                    Scale if necessary
                                // *
                                if (SCALE != ONE) this._dscal.Run(N - KI + 1, SCALE, ref WORK, KI + N + o_work, 1);
                                WORK[J + N + o_work] = X[1+1 * 2 + o_x];
                                VMAX = Math.Max(Math.Abs(WORK[J + N + o_work]), VMAX);
                                VCRIT = BIGNUM / VMAX;
                                // *
                            }
                            else
                            {
                                // *
                                // *                    2-by-2 diagonal block
                                // *
                                // *                    Scale if necessary to avoid overflow when forming
                                // *                    the right-hand side.
                                // *
                                BETA = Math.Max(WORK[J + o_work], WORK[J + 1 + o_work]);
                                if (BETA > VCRIT)
                                {
                                    REC = ONE / VMAX;
                                    this._dscal.Run(N - KI + 1, REC, ref WORK, KI + N + o_work, 1);
                                    VMAX = ONE;
                                    VCRIT = BIGNUM;
                                }
                                // *
                                WORK[J + N + o_work] -= this._ddot.Run(J - KI - 1, T, KI + 1+J * LDT + o_t, 1, WORK, KI + 1 + N + o_work, 1);
                                // *
                                WORK[J + 1 + N + o_work] -= this._ddot.Run(J - KI - 1, T, KI + 1+(J + 1) * LDT + o_t, 1, WORK, KI + 1 + N + o_work, 1);
                                // *
                                // *                    Solve
                                // *                      [T(J,J)-WR   T(J,J+1)     ]'* X = SCALE*( WORK1 )
                                // *                      [T(J+1,J)    T(J+1,J+1)-WR]             ( WORK2 )
                                // *
                                this._dlaln2.Run(true, 2, 1, SMIN, ONE, T, J+J * LDT + o_t
                                                 , LDT, ONE, ONE, WORK, J + N + o_work, N, WR
                                                 , ZERO, ref X, offset_x, 2, ref SCALE, ref XNORM, ref IERR);
                                // *
                                // *                    Scale if necessary
                                // *
                                if (SCALE != ONE) this._dscal.Run(N - KI + 1, SCALE, ref WORK, KI + N + o_work, 1);
                                WORK[J + N + o_work] = X[1+1 * 2 + o_x];
                                WORK[J + 1 + N + o_work] = X[2+1 * 2 + o_x];
                                // *
                                VMAX = Math.Max(Math.Abs(WORK[J + N + o_work]), Math.Max(Math.Abs(WORK[J + 1 + N + o_work]), VMAX));
                                VCRIT = BIGNUM / VMAX;
                                // *
                            }
                        LABEL170:;
                        }
                        // *
                        // *              Copy the vector x or Q*x to VL and normalize.
                        // *
                        if (!OVER)
                        {
                            this._dcopy.Run(N - KI + 1, WORK, KI + N + o_work, 1, ref VL, KI+IS * LDVL + o_vl, 1);
                            // *
                            II = this._idamax.Run(N - KI + 1, VL, KI+IS * LDVL + o_vl, 1) + KI - 1;
                            REMAX = ONE / Math.Abs(VL[II+IS * LDVL + o_vl]);
                            this._dscal.Run(N - KI + 1, REMAX, ref VL, KI+IS * LDVL + o_vl, 1);
                            // *
                            VL_IS = IS * LDVL + o_vl;
                            for (K = 1; K <= KI - 1; K++)
                            {
                                VL[K + VL_IS] = ZERO;
                            }
                            // *
                        }
                        else
                        {
                            // *
                            if (KI < N)
                            {
                                this._dgemv.Run("N", N, N - KI, ONE, VL, 1+(KI + 1) * LDVL + o_vl, LDVL
                                                , WORK, KI + 1 + N + o_work, 1, WORK[KI + N + o_work], ref VL, 1+KI * LDVL + o_vl, 1);
                            }
                            // *
                            II = this._idamax.Run(N, VL, 1+KI * LDVL + o_vl, 1);
                            REMAX = ONE / Math.Abs(VL[II+KI * LDVL + o_vl]);
                            this._dscal.Run(N, REMAX, ref VL, 1+KI * LDVL + o_vl, 1);
                            // *
                        }
                        // *
                    }
                    else
                    {
                        // *
                        // *              Complex left eigenvector.
                        // *
                        // *               Initial solve:
                        // *                 ((T(KI,KI)    T(KI,KI+1) )' - (WR - I* WI))*X = 0.
                        // *                 ((T(KI+1,KI) T(KI+1,KI+1))                )
                        // *
                        if (Math.Abs(T[KI+(KI + 1) * LDT + o_t]) >= Math.Abs(T[KI + 1+KI * LDT + o_t]))
                        {
                            WORK[KI + N + o_work] = WI / T[KI+(KI + 1) * LDT + o_t];
                            WORK[KI + 1 + N2 + o_work] = ONE;
                        }
                        else
                        {
                            WORK[KI + N + o_work] = ONE;
                            WORK[KI + 1 + N2 + o_work] =  - WI / T[KI + 1+KI * LDT + o_t];
                        }
                        WORK[KI + 1 + N + o_work] = ZERO;
                        WORK[KI + N2 + o_work] = ZERO;
                        // *
                        // *              Form right-hand side
                        // *
                        for (K = KI + 2; K <= N; K++)
                        {
                            WORK[K + N + o_work] =  - WORK[KI + N + o_work] * T[KI+K * LDT + o_t];
                            WORK[K + N2 + o_work] =  - WORK[KI + 1 + N2 + o_work] * T[KI + 1+K * LDT + o_t];
                        }
                        // *
                        // *              Solve complex quasi-triangular system:
                        // *              ( T(KI+2,N:KI+2,N) - (WR-i*WI) )*X = WORK1+i*WORK2
                        // *
                        VMAX = ONE;
                        VCRIT = BIGNUM;
                        // *
                        JNXT = KI + 2;
                        for (J = KI + 2; J <= N; J++)
                        {
                            if (J < JNXT) goto LABEL200;
                            J1 = J;
                            J2 = J;
                            JNXT = J + 1;
                            if (J < N)
                            {
                                if (T[J + 1+J * LDT + o_t] != ZERO)
                                {
                                    J2 = J + 1;
                                    JNXT = J + 2;
                                }
                            }
                            // *
                            if (J1 == J2)
                            {
                                // *
                                // *                    1-by-1 diagonal block
                                // *
                                // *                    Scale if necessary to avoid overflow when
                                // *                    forming the right-hand side elements.
                                // *
                                if (WORK[J + o_work] > VCRIT)
                                {
                                    REC = ONE / VMAX;
                                    this._dscal.Run(N - KI + 1, REC, ref WORK, KI + N + o_work, 1);
                                    this._dscal.Run(N - KI + 1, REC, ref WORK, KI + N2 + o_work, 1);
                                    VMAX = ONE;
                                    VCRIT = BIGNUM;
                                }
                                // *
                                WORK[J + N + o_work] -= this._ddot.Run(J - KI - 2, T, KI + 2+J * LDT + o_t, 1, WORK, KI + 2 + N + o_work, 1);
                                WORK[J + N2 + o_work] -= this._ddot.Run(J - KI - 2, T, KI + 2+J * LDT + o_t, 1, WORK, KI + 2 + N2 + o_work, 1);
                                // *
                                // *                    Solve (T(J,J)-(WR-i*WI))*(X11+i*X12)= WK+I*WK2
                                // *
                                this._dlaln2.Run(false, 1, 2, SMIN, ONE, T, J+J * LDT + o_t
                                                 , LDT, ONE, ONE, WORK, J + N + o_work, N, WR
                                                 ,  - WI, ref X, offset_x, 2, ref SCALE, ref XNORM, ref IERR);
                                // *
                                // *                    Scale if necessary
                                // *
                                if (SCALE != ONE)
                                {
                                    this._dscal.Run(N - KI + 1, SCALE, ref WORK, KI + N + o_work, 1);
                                    this._dscal.Run(N - KI + 1, SCALE, ref WORK, KI + N2 + o_work, 1);
                                }
                                WORK[J + N + o_work] = X[1+1 * 2 + o_x];
                                WORK[J + N2 + o_work] = X[1+2 * 2 + o_x];
                                VMAX = Math.Max(Math.Abs(WORK[J + N + o_work]), Math.Max(Math.Abs(WORK[J + N2 + o_work]), VMAX));
                                VCRIT = BIGNUM / VMAX;
                                // *
                            }
                            else
                            {
                                // *
                                // *                    2-by-2 diagonal block
                                // *
                                // *                    Scale if necessary to avoid overflow when forming
                                // *                    the right-hand side elements.
                                // *
                                BETA = Math.Max(WORK[J + o_work], WORK[J + 1 + o_work]);
                                if (BETA > VCRIT)
                                {
                                    REC = ONE / VMAX;
                                    this._dscal.Run(N - KI + 1, REC, ref WORK, KI + N + o_work, 1);
                                    this._dscal.Run(N - KI + 1, REC, ref WORK, KI + N2 + o_work, 1);
                                    VMAX = ONE;
                                    VCRIT = BIGNUM;
                                }
                                // *
                                WORK[J + N + o_work] -= this._ddot.Run(J - KI - 2, T, KI + 2+J * LDT + o_t, 1, WORK, KI + 2 + N + o_work, 1);
                                // *
                                WORK[J + N2 + o_work] -= this._ddot.Run(J - KI - 2, T, KI + 2+J * LDT + o_t, 1, WORK, KI + 2 + N2 + o_work, 1);
                                // *
                                WORK[J + 1 + N + o_work] -= this._ddot.Run(J - KI - 2, T, KI + 2+(J + 1) * LDT + o_t, 1, WORK, KI + 2 + N + o_work, 1);
                                // *
                                WORK[J + 1 + N2 + o_work] -= this._ddot.Run(J - KI - 2, T, KI + 2+(J + 1) * LDT + o_t, 1, WORK, KI + 2 + N2 + o_work, 1);
                                // *
                                // *                    Solve 2-by-2 complex linear equation
                                // *                      ([T(j,j)   T(j,j+1)  ]'-(wr-i*wi)*I)*X = SCALE*B
                                // *                      ([T(j+1,j) T(j+1,j+1)]             )
                                // *
                                this._dlaln2.Run(true, 2, 2, SMIN, ONE, T, J+J * LDT + o_t
                                                 , LDT, ONE, ONE, WORK, J + N + o_work, N, WR
                                                 ,  - WI, ref X, offset_x, 2, ref SCALE, ref XNORM, ref IERR);
                                // *
                                // *                    Scale if necessary
                                // *
                                if (SCALE != ONE)
                                {
                                    this._dscal.Run(N - KI + 1, SCALE, ref WORK, KI + N + o_work, 1);
                                    this._dscal.Run(N - KI + 1, SCALE, ref WORK, KI + N2 + o_work, 1);
                                }
                                WORK[J + N + o_work] = X[1+1 * 2 + o_x];
                                WORK[J + N2 + o_work] = X[1+2 * 2 + o_x];
                                WORK[J + 1 + N + o_work] = X[2+1 * 2 + o_x];
                                WORK[J + 1 + N2 + o_work] = X[2+2 * 2 + o_x];
                                VMAX = Math.Max(Math.Abs(X[1+1 * 2 + o_x]), Math.Max(Math.Abs(X[1+2 * 2 + o_x]), Math.Max(Math.Abs(X[2+1 * 2 + o_x]), Math.Max(Math.Abs(X[2+2 * 2 + o_x]), VMAX))));
                                VCRIT = BIGNUM / VMAX;
                                // *
                            }
                        LABEL200:;
                        }
                        // *
                        // *              Copy the vector x or Q*x to VL and normalize.
                        // *
                        if (!OVER)
                        {
                            this._dcopy.Run(N - KI + 1, WORK, KI + N + o_work, 1, ref VL, KI+IS * LDVL + o_vl, 1);
                            this._dcopy.Run(N - KI + 1, WORK, KI + N2 + o_work, 1, ref VL, KI+(IS + 1) * LDVL + o_vl, 1);
                            // *
                            EMAX = ZERO;
                            for (K = KI; K <= N; K++)
                            {
                                EMAX = Math.Max(EMAX, Math.Abs(VL[K+IS * LDVL + o_vl]) + Math.Abs(VL[K+(IS + 1) * LDVL + o_vl]));
                            }
                            REMAX = ONE / EMAX;
                            this._dscal.Run(N - KI + 1, REMAX, ref VL, KI+IS * LDVL + o_vl, 1);
                            this._dscal.Run(N - KI + 1, REMAX, ref VL, KI+(IS + 1) * LDVL + o_vl, 1);
                            // *
                            VL_IS = IS * LDVL + o_vl;
                            VL_0 = (IS + 1) * LDVL + o_vl;
                            for (K = 1; K <= KI - 1; K++)
                            {
                                VL[K + VL_IS] = ZERO;
                                VL[K + VL_0] = ZERO;
                            }
                        }
                        else
                        {
                            if (KI < N - 1)
                            {
                                this._dgemv.Run("N", N, N - KI - 1, ONE, VL, 1+(KI + 2) * LDVL + o_vl, LDVL
                                                , WORK, KI + 2 + N + o_work, 1, WORK[KI + N + o_work], ref VL, 1+KI * LDVL + o_vl, 1);
                                this._dgemv.Run("N", N, N - KI - 1, ONE, VL, 1+(KI + 2) * LDVL + o_vl, LDVL
                                                , WORK, KI + 2 + N2 + o_work, 1, WORK[KI + 1 + N2 + o_work], ref VL, 1+(KI + 1) * LDVL + o_vl, 1);
                            }
                            else
                            {
                                this._dscal.Run(N, WORK[KI + N + o_work], ref VL, 1+KI * LDVL + o_vl, 1);
                                this._dscal.Run(N, WORK[KI + 1 + N2 + o_work], ref VL, 1+(KI + 1) * LDVL + o_vl, 1);
                            }
                            // *
                            EMAX = ZERO;
                            for (K = 1; K <= N; K++)
                            {
                                EMAX = Math.Max(EMAX, Math.Abs(VL[K+KI * LDVL + o_vl]) + Math.Abs(VL[K+(KI + 1) * LDVL + o_vl]));
                            }
                            REMAX = ONE / EMAX;
                            this._dscal.Run(N, REMAX, ref VL, 1+KI * LDVL + o_vl, 1);
                            this._dscal.Run(N, REMAX, ref VL, 1+(KI + 1) * LDVL + o_vl, 1);
                            // *
                        }
                        // *
                    }
                    // *
                    IS += 1;
                    if (IP != 0) IS += 1;
                LABEL250:;
                    if (IP ==  - 1) IP = 0;
                    if (IP == 1) IP =  - 1;
                    // *
                }
                // *
            }
            // *
            return;
            // *
            // *     End of DTREVC
            // *

            #endregion

        }
    }
}
