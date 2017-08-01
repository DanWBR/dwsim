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
    /// This subroutine computes the I-th updated eigenvalue of a symmetric
    /// rank-one modification to a diagonal matrix whose elements are
    /// given in the array d, and that
    /// 
    /// D(i) .LT. D(j)  for  i .LT. j
    /// 
    /// and that RHO .GT. 0.  This is arranged by the calling routine, and is
    /// no loss in generality.  The rank-one modified system is thus
    /// 
    /// diag( D )  +  RHO *  Z * Z_transpose.
    /// 
    /// where we assume the Euclidean norm of Z is 1.
    /// 
    /// The method consists of approximating the rational functions in the
    /// secular equation by simpler interpolating rational functions.
    /// 
    ///</summary>
    public class DLAED4
    {
    

        #region Dependencies
        
        DLAMCH _dlamch; DLAED5 _dlaed5; DLAED6 _dlaed6; 

        #endregion


        #region Variables
        
        const int MAXIT = 30; const double ZERO = 0.0E0; const double ONE = 1.0E0; const double TWO = 2.0E0; 
        const double THREE = 3.0E0;const double FOUR = 4.0E0; const double EIGHT = 8.0E0; const double TEN = 10.0E0; 
        double[] ZZ = new double[3];

        #endregion

        public DLAED4(DLAMCH dlamch, DLAED5 dlaed5, DLAED6 dlaed6)
        {
    

            #region Set Dependencies
            
            this._dlamch = dlamch; this._dlaed5 = dlaed5; this._dlaed6 = dlaed6; 

            #endregion

        }
    
        public DLAED4()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAED5 dlaed5 = new DLAED5();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLAED6 dlaed6 = new DLAED6(dlamch);

            #endregion


            #region Set Dependencies
            
            this._dlamch = dlamch; this._dlaed5 = dlaed5; this._dlaed6 = dlaed6; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// This subroutine computes the I-th updated eigenvalue of a symmetric
        /// rank-one modification to a diagonal matrix whose elements are
        /// given in the array d, and that
        /// 
        /// D(i) .LT. D(j)  for  i .LT. j
        /// 
        /// and that RHO .GT. 0.  This is arranged by the calling routine, and is
        /// no loss in generality.  The rank-one modified system is thus
        /// 
        /// diag( D )  +  RHO *  Z * Z_transpose.
        /// 
        /// where we assume the Euclidean norm of Z is 1.
        /// 
        /// The method consists of approximating the rational functions in the
        /// secular equation by simpler interpolating rational functions.
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The length of all arrays.
        ///</param>
        /// <param name="I">
        /// (input) INTEGER
        /// The index of the eigenvalue to be computed.  1 .LE. I .LE. N.
        ///</param>
        /// <param name="D">
        /// (input) DOUBLE PRECISION array, dimension (N)
        /// The original eigenvalues.  It is assumed that they are in
        /// order, D(I) .LT. D(J)  for I .LT. J.
        ///</param>
        /// <param name="Z">
        /// (input) DOUBLE PRECISION array, dimension (N)
        /// The components of the updating vector.
        ///</param>
        /// <param name="DELTA">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// If N .GT. 2, DELTA contains (D(j) - lambda_I) in its  j-th
        /// component.  If N = 1, then DELTA(1) = 1. If N = 2, see DLAED5
        /// for detail. The vector DELTA contains the information necessary
        /// to construct the eigenvectors by DLAED3 and DLAED9.
        ///</param>
        /// <param name="RHO">
        /// (input) DOUBLE PRECISION
        /// The scalar in the symmetric updating formula.
        ///</param>
        /// <param name="DLAM">
        /// (output) DOUBLE PRECISION
        /// The computed lambda_I, the I-th updated eigenvalue.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .GT. 0:  if INFO = 1, the updating process failed.
        ///</param>
        public void Run(int N, int I, double[] D, int offset_d, double[] Z, int offset_z, ref double[] DELTA, int offset_delta, double RHO
                         , ref double DLAM, ref int INFO)
        {

            #region Variables
            
            bool ORGATI = false; bool SWTCH = false; bool SWTCH3 = false; int II = 0; int IIM1 = 0; int IIP1 = 0; int IP1 = 0; 
            int ITER = 0;int J = 0; int NITER = 0; double A = 0; double B = 0; double C = 0; double DEL = 0; double DLTLB = 0; 
            double DLTUB = 0;double DPHI = 0; double DPSI = 0; double DW = 0; double EPS = 0; double ERRETM = 0; double ETA = 0; 
            double MIDPT = 0;double PHI = 0; double PREW = 0; double PSI = 0; double RHOINV = 0; double TAU = 0; double TEMP = 0; 
            double TEMP1 = 0;double W = 0; int offset_zz = 0; int o_zz = -1; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_z = -1 + offset_z;  int o_delta = -1 + offset_delta; 

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
            // *  This subroutine computes the I-th updated eigenvalue of a symmetric
            // *  rank-one modification to a diagonal matrix whose elements are
            // *  given in the array d, and that
            // *
            // *             D(i) < D(j)  for  i < j
            // *
            // *  and that RHO > 0.  This is arranged by the calling routine, and is
            // *  no loss in generality.  The rank-one modified system is thus
            // *
            // *             diag( D )  +  RHO *  Z * Z_transpose.
            // *
            // *  where we assume the Euclidean norm of Z is 1.
            // *
            // *  The method consists of approximating the rational functions in the
            // *  secular equation by simpler interpolating rational functions.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N      (input) INTEGER
            // *         The length of all arrays.
            // *
            // *  I      (input) INTEGER
            // *         The index of the eigenvalue to be computed.  1 <= I <= N.
            // *
            // *  D      (input) DOUBLE PRECISION array, dimension (N)
            // *         The original eigenvalues.  It is assumed that they are in
            // *         order, D(I) < D(J)  for I < J.
            // *
            // *  Z      (input) DOUBLE PRECISION array, dimension (N)
            // *         The components of the updating vector.
            // *
            // *  DELTA  (output) DOUBLE PRECISION array, dimension (N)
            // *         If N .GT. 2, DELTA contains (D(j) - lambda_I) in its  j-th
            // *         component.  If N = 1, then DELTA(1) = 1. If N = 2, see DLAED5
            // *         for detail. The vector DELTA contains the information necessary
            // *         to construct the eigenvectors by DLAED3 and DLAED9.
            // *
            // *  RHO    (input) DOUBLE PRECISION
            // *         The scalar in the symmetric updating formula.
            // *
            // *  DLAM   (output) DOUBLE PRECISION
            // *         The computed lambda_I, the I-th updated eigenvalue.
            // *
            // *  INFO   (output) INTEGER
            // *         = 0:  successful exit
            // *         > 0:  if INFO = 1, the updating process failed.
            // *
            // *  Internal Parameters
            // *  ===================
            // *
            // *  Logical variable ORGATI (origin-at-i?) is used for distinguishing
            // *  whether D(i) or D(i+1) is treated as the origin.
            // *
            // *            ORGATI = .true.    origin at i
            // *            ORGATI = .false.   origin at i+1
            // *
            // *   Logical variable SWTCH3 (switch-for-3-poles?) is for noting
            // *   if we are working with THREE poles!
            // *
            // *   MAXIT is the maximum number of iterations allowed for each
            // *   eigenvalue.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *     Ren-Cang Li, Computer Science Division, University of California
            // *     at Berkeley, USA
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Local Arrays ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, MAX, MIN, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Since this routine is called in an inner loop, we do no argument
            // *     checking.
            // *
            // *     Quick return for N=1 and 2.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            if (N == 1)
            {
                // *
                // *         Presumably, I=1 upon entry
                // *
                DLAM = D[1 + o_d] + RHO * Z[1 + o_z] * Z[1 + o_z];
                DELTA[1 + o_delta] = ONE;
                return;
            }
            if (N == 2)
            {
                this._dlaed5.Run(I, D, offset_d, Z, offset_z, ref DELTA, offset_delta, RHO, ref DLAM);
                return;
            }
            // *
            // *     Compute machine epsilon
            // *
            EPS = this._dlamch.Run("Epsilon");
            RHOINV = ONE / RHO;
            // *
            // *     The case I = N
            // *
            if (I == N)
            {
                // *
                // *        Initialize some basic variables
                // *
                II = N - 1;
                NITER = 1;
                // *
                // *        Calculate initial guess
                // *
                MIDPT = RHO / TWO;
                // *
                // *        If ||Z||_2 is not one, then TEMP should be set to
                // *        RHO * ||Z||_2^2 / TWO
                // *
                for (J = 1; J <= N; J++)
                {
                    DELTA[J + o_delta] = (D[J + o_d] - D[I + o_d]) - MIDPT;
                }
                // *
                PSI = ZERO;
                for (J = 1; J <= N - 2; J++)
                {
                    PSI += Z[J + o_z] * Z[J + o_z] / DELTA[J + o_delta];
                }
                // *
                C = RHOINV + PSI;
                W = C + Z[II + o_z] * Z[II + o_z] / DELTA[II + o_delta] + Z[N + o_z] * Z[N + o_z] / DELTA[N + o_delta];
                // *
                if (W <= ZERO)
                {
                    TEMP = Z[N - 1 + o_z] * Z[N - 1 + o_z] / (D[N + o_d] - D[N - 1 + o_d] + RHO) + Z[N + o_z] * Z[N + o_z] / RHO;
                    if (C <= TEMP)
                    {
                        TAU = RHO;
                    }
                    else
                    {
                        DEL = D[N + o_d] - D[N - 1 + o_d];
                        A =  - C * DEL + Z[N - 1 + o_z] * Z[N - 1 + o_z] + Z[N + o_z] * Z[N + o_z];
                        B = Z[N + o_z] * Z[N + o_z] * DEL;
                        if (A < ZERO)
                        {
                            TAU = TWO * B / (Math.Sqrt(A * A + FOUR * B * C) - A);
                        }
                        else
                        {
                            TAU = (A + Math.Sqrt(A * A + FOUR * B * C)) / (TWO * C);
                        }
                    }
                    // *
                    // *           It can be proved that
                    // *               D(N)+RHO/2 <= LAMBDA(N) < D(N)+TAU <= D(N)+RHO
                    // *
                    DLTLB = MIDPT;
                    DLTUB = RHO;
                }
                else
                {
                    DEL = D[N + o_d] - D[N - 1 + o_d];
                    A =  - C * DEL + Z[N - 1 + o_z] * Z[N - 1 + o_z] + Z[N + o_z] * Z[N + o_z];
                    B = Z[N + o_z] * Z[N + o_z] * DEL;
                    if (A < ZERO)
                    {
                        TAU = TWO * B / (Math.Sqrt(A * A + FOUR * B * C) - A);
                    }
                    else
                    {
                        TAU = (A + Math.Sqrt(A * A + FOUR * B * C)) / (TWO * C);
                    }
                    // *
                    // *           It can be proved that
                    // *               D(N) < D(N)+TAU < LAMBDA(N) < D(N)+RHO/2
                    // *
                    DLTLB = ZERO;
                    DLTUB = MIDPT;
                }
                // *
                for (J = 1; J <= N; J++)
                {
                    DELTA[J + o_delta] = (D[J + o_d] - D[I + o_d]) - TAU;
                }
                // *
                // *        Evaluate PSI and the derivative DPSI
                // *
                DPSI = ZERO;
                PSI = ZERO;
                ERRETM = ZERO;
                for (J = 1; J <= II; J++)
                {
                    TEMP = Z[J + o_z] / DELTA[J + o_delta];
                    PSI += Z[J + o_z] * TEMP;
                    DPSI += TEMP * TEMP;
                    ERRETM += PSI;
                }
                ERRETM = Math.Abs(ERRETM);
                // *
                // *        Evaluate PHI and the derivative DPHI
                // *
                TEMP = Z[N + o_z] / DELTA[N + o_delta];
                PHI = Z[N + o_z] * TEMP;
                DPHI = TEMP * TEMP;
                ERRETM = EIGHT * ( - PHI - PSI) + ERRETM - PHI + RHOINV + Math.Abs(TAU) * (DPSI + DPHI);
                // *
                W = RHOINV + PHI + PSI;
                // *
                // *        Test for convergence
                // *
                if (Math.Abs(W) <= EPS * ERRETM)
                {
                    DLAM = D[I + o_d] + TAU;
                    goto LABEL250;
                }
                // *
                if (W <= ZERO)
                {
                    DLTLB = Math.Max(DLTLB, TAU);
                }
                else
                {
                    DLTUB = Math.Min(DLTUB, TAU);
                }
                // *
                // *        Calculate the new step
                // *
                NITER += 1;
                C = W - DELTA[N - 1 + o_delta] * DPSI - DELTA[N + o_delta] * DPHI;
                A = (DELTA[N - 1 + o_delta] + DELTA[N + o_delta]) * W - DELTA[N - 1 + o_delta] * DELTA[N + o_delta] * (DPSI + DPHI);
                B = DELTA[N - 1 + o_delta] * DELTA[N + o_delta] * W;
                if (C < ZERO) C = Math.Abs(C);
                if (C == ZERO)
                {
                    // *          ETA = B/A
                    // *           ETA = RHO - TAU
                    ETA = DLTUB - TAU;
                }
                else
                {
                    if (A >= ZERO)
                    {
                        ETA = (A + Math.Sqrt(Math.Abs(A * A - FOUR * B * C))) / (TWO * C);
                    }
                    else
                    {
                        ETA = TWO * B / (A - Math.Sqrt(Math.Abs(A * A - FOUR * B * C)));
                    }
                }
                // *
                // *        Note, eta should be positive if w is negative, and
                // *        eta should be negative otherwise. However,
                // *        if for some reason caused by roundoff, eta*w > 0,
                // *        we simply use one Newton step instead. This way
                // *        will guarantee eta*w < 0.
                // *
                if (W * ETA > ZERO) ETA =  - W / (DPSI + DPHI);
                TEMP = TAU + ETA;
                if (TEMP > DLTUB || TEMP < DLTLB)
                {
                    if (W < ZERO)
                    {
                        ETA = (DLTUB - TAU) / TWO;
                    }
                    else
                    {
                        ETA = (DLTLB - TAU) / TWO;
                    }
                }
                for (J = 1; J <= N; J++)
                {
                    DELTA[J + o_delta] -= ETA;
                }
                // *
                TAU += ETA;
                // *
                // *        Evaluate PSI and the derivative DPSI
                // *
                DPSI = ZERO;
                PSI = ZERO;
                ERRETM = ZERO;
                for (J = 1; J <= II; J++)
                {
                    TEMP = Z[J + o_z] / DELTA[J + o_delta];
                    PSI += Z[J + o_z] * TEMP;
                    DPSI += TEMP * TEMP;
                    ERRETM += PSI;
                }
                ERRETM = Math.Abs(ERRETM);
                // *
                // *        Evaluate PHI and the derivative DPHI
                // *
                TEMP = Z[N + o_z] / DELTA[N + o_delta];
                PHI = Z[N + o_z] * TEMP;
                DPHI = TEMP * TEMP;
                ERRETM = EIGHT * ( - PHI - PSI) + ERRETM - PHI + RHOINV + Math.Abs(TAU) * (DPSI + DPHI);
                // *
                W = RHOINV + PHI + PSI;
                // *
                // *        Main loop to update the values of the array   DELTA
                // *
                ITER = NITER + 1;
                // *
                for (NITER = ITER; NITER <= MAXIT; NITER++)
                {
                    // *
                    // *           Test for convergence
                    // *
                    if (Math.Abs(W) <= EPS * ERRETM)
                    {
                        DLAM = D[I + o_d] + TAU;
                        goto LABEL250;
                    }
                    // *
                    if (W <= ZERO)
                    {
                        DLTLB = Math.Max(DLTLB, TAU);
                    }
                    else
                    {
                        DLTUB = Math.Min(DLTUB, TAU);
                    }
                    // *
                    // *           Calculate the new step
                    // *
                    C = W - DELTA[N - 1 + o_delta] * DPSI - DELTA[N + o_delta] * DPHI;
                    A = (DELTA[N - 1 + o_delta] + DELTA[N + o_delta]) * W - DELTA[N - 1 + o_delta] * DELTA[N + o_delta] * (DPSI + DPHI);
                    B = DELTA[N - 1 + o_delta] * DELTA[N + o_delta] * W;
                    if (A >= ZERO)
                    {
                        ETA = (A + Math.Sqrt(Math.Abs(A * A - FOUR * B * C))) / (TWO * C);
                    }
                    else
                    {
                        ETA = TWO * B / (A - Math.Sqrt(Math.Abs(A * A - FOUR * B * C)));
                    }
                    // *
                    // *           Note, eta should be positive if w is negative, and
                    // *           eta should be negative otherwise. However,
                    // *           if for some reason caused by roundoff, eta*w > 0,
                    // *           we simply use one Newton step instead. This way
                    // *           will guarantee eta*w < 0.
                    // *
                    if (W * ETA > ZERO) ETA =  - W / (DPSI + DPHI);
                    TEMP = TAU + ETA;
                    if (TEMP > DLTUB || TEMP < DLTLB)
                    {
                        if (W < ZERO)
                        {
                            ETA = (DLTUB - TAU) / TWO;
                        }
                        else
                        {
                            ETA = (DLTLB - TAU) / TWO;
                        }
                    }
                    for (J = 1; J <= N; J++)
                    {
                        DELTA[J + o_delta] -= ETA;
                    }
                    // *
                    TAU += ETA;
                    // *
                    // *           Evaluate PSI and the derivative DPSI
                    // *
                    DPSI = ZERO;
                    PSI = ZERO;
                    ERRETM = ZERO;
                    for (J = 1; J <= II; J++)
                    {
                        TEMP = Z[J + o_z] / DELTA[J + o_delta];
                        PSI += Z[J + o_z] * TEMP;
                        DPSI += TEMP * TEMP;
                        ERRETM += PSI;
                    }
                    ERRETM = Math.Abs(ERRETM);
                    // *
                    // *           Evaluate PHI and the derivative DPHI
                    // *
                    TEMP = Z[N + o_z] / DELTA[N + o_delta];
                    PHI = Z[N + o_z] * TEMP;
                    DPHI = TEMP * TEMP;
                    ERRETM = EIGHT * ( - PHI - PSI) + ERRETM - PHI + RHOINV + Math.Abs(TAU) * (DPSI + DPHI);
                    // *
                    W = RHOINV + PHI + PSI;
                }
                // *
                // *        Return with INFO = 1, NITER = MAXIT and not converged
                // *
                INFO = 1;
                DLAM = D[I + o_d] + TAU;
                goto LABEL250;
                // *
                // *        End for the case I = N
                // *
            }
            else
            {
                // *
                // *        The case for I < N
                // *
                NITER = 1;
                IP1 = I + 1;
                // *
                // *        Calculate initial guess
                // *
                DEL = D[IP1 + o_d] - D[I + o_d];
                MIDPT = DEL / TWO;
                for (J = 1; J <= N; J++)
                {
                    DELTA[J + o_delta] = (D[J + o_d] - D[I + o_d]) - MIDPT;
                }
                // *
                PSI = ZERO;
                for (J = 1; J <= I - 1; J++)
                {
                    PSI += Z[J + o_z] * Z[J + o_z] / DELTA[J + o_delta];
                }
                // *
                PHI = ZERO;
                for (J = N; J >= I + 2; J +=  - 1)
                {
                    PHI += Z[J + o_z] * Z[J + o_z] / DELTA[J + o_delta];
                }
                C = RHOINV + PSI + PHI;
                W = C + Z[I + o_z] * Z[I + o_z] / DELTA[I + o_delta] + Z[IP1 + o_z] * Z[IP1 + o_z] / DELTA[IP1 + o_delta];
                // *
                if (W > ZERO)
                {
                    // *
                    // *           d(i)< the ith eigenvalue < (d(i)+d(i+1))/2
                    // *
                    // *           We choose d(i) as origin.
                    // *
                    ORGATI = true;
                    A = C * DEL + Z[I + o_z] * Z[I + o_z] + Z[IP1 + o_z] * Z[IP1 + o_z];
                    B = Z[I + o_z] * Z[I + o_z] * DEL;
                    if (A > ZERO)
                    {
                        TAU = TWO * B / (A + Math.Sqrt(Math.Abs(A * A - FOUR * B * C)));
                    }
                    else
                    {
                        TAU = (A - Math.Sqrt(Math.Abs(A * A - FOUR * B * C))) / (TWO * C);
                    }
                    DLTLB = ZERO;
                    DLTUB = MIDPT;
                }
                else
                {
                    // *
                    // *           (d(i)+d(i+1))/2 <= the ith eigenvalue < d(i+1)
                    // *
                    // *           We choose d(i+1) as origin.
                    // *
                    ORGATI = false;
                    A = C * DEL - Z[I + o_z] * Z[I + o_z] - Z[IP1 + o_z] * Z[IP1 + o_z];
                    B = Z[IP1 + o_z] * Z[IP1 + o_z] * DEL;
                    if (A < ZERO)
                    {
                        TAU = TWO * B / (A - Math.Sqrt(Math.Abs(A * A + FOUR * B * C)));
                    }
                    else
                    {
                        TAU =  - (A + Math.Sqrt(Math.Abs(A * A + FOUR * B * C))) / (TWO * C);
                    }
                    DLTLB =  - MIDPT;
                    DLTUB = ZERO;
                }
                // *
                if (ORGATI)
                {
                    for (J = 1; J <= N; J++)
                    {
                        DELTA[J + o_delta] = (D[J + o_d] - D[I + o_d]) - TAU;
                    }
                }
                else
                {
                    for (J = 1; J <= N; J++)
                    {
                        DELTA[J + o_delta] = (D[J + o_d] - D[IP1 + o_d]) - TAU;
                    }
                }
                if (ORGATI)
                {
                    II = I;
                }
                else
                {
                    II = I + 1;
                }
                IIM1 = II - 1;
                IIP1 = II + 1;
                // *
                // *        Evaluate PSI and the derivative DPSI
                // *
                DPSI = ZERO;
                PSI = ZERO;
                ERRETM = ZERO;
                for (J = 1; J <= IIM1; J++)
                {
                    TEMP = Z[J + o_z] / DELTA[J + o_delta];
                    PSI += Z[J + o_z] * TEMP;
                    DPSI += TEMP * TEMP;
                    ERRETM += PSI;
                }
                ERRETM = Math.Abs(ERRETM);
                // *
                // *        Evaluate PHI and the derivative DPHI
                // *
                DPHI = ZERO;
                PHI = ZERO;
                for (J = N; J >= IIP1; J +=  - 1)
                {
                    TEMP = Z[J + o_z] / DELTA[J + o_delta];
                    PHI += Z[J + o_z] * TEMP;
                    DPHI += TEMP * TEMP;
                    ERRETM += PHI;
                }
                // *
                W = RHOINV + PHI + PSI;
                // *
                // *        W is the value of the secular function with
                // *        its ii-th element removed.
                // *
                SWTCH3 = false;
                if (ORGATI)
                {
                    if (W < ZERO) SWTCH3 = true;
                }
                else
                {
                    if (W > ZERO) SWTCH3 = true;
                }
                if (II == 1 || II == N) SWTCH3 = false;
                // *
                TEMP = Z[II + o_z] / DELTA[II + o_delta];
                DW = DPSI + DPHI + TEMP * TEMP;
                TEMP *= Z[II + o_z];
                W += TEMP;
                ERRETM = EIGHT * (PHI - PSI) + ERRETM + TWO * RHOINV + THREE * Math.Abs(TEMP) + Math.Abs(TAU) * DW;
                // *
                // *        Test for convergence
                // *
                if (Math.Abs(W) <= EPS * ERRETM)
                {
                    if (ORGATI)
                    {
                        DLAM = D[I + o_d] + TAU;
                    }
                    else
                    {
                        DLAM = D[IP1 + o_d] + TAU;
                    }
                    goto LABEL250;
                }
                // *
                if (W <= ZERO)
                {
                    DLTLB = Math.Max(DLTLB, TAU);
                }
                else
                {
                    DLTUB = Math.Min(DLTUB, TAU);
                }
                // *
                // *        Calculate the new step
                // *
                NITER += 1;
                if (!SWTCH3)
                {
                    if (ORGATI)
                    {
                        C = W - DELTA[IP1 + o_delta] * DW - (D[I + o_d] - D[IP1 + o_d]) * Math.Pow(Z[I + o_z] / DELTA[I + o_delta],2);
                    }
                    else
                    {
                        C = W - DELTA[I + o_delta] * DW - (D[IP1 + o_d] - D[I + o_d]) * Math.Pow(Z[IP1 + o_z] / DELTA[IP1 + o_delta],2);
                    }
                    A = (DELTA[I + o_delta] + DELTA[IP1 + o_delta]) * W - DELTA[I + o_delta] * DELTA[IP1 + o_delta] * DW;
                    B = DELTA[I + o_delta] * DELTA[IP1 + o_delta] * W;
                    if (C == ZERO)
                    {
                        if (A == ZERO)
                        {
                            if (ORGATI)
                            {
                                A = Z[I + o_z] * Z[I + o_z] + DELTA[IP1 + o_delta] * DELTA[IP1 + o_delta] * (DPSI + DPHI);
                            }
                            else
                            {
                                A = Z[IP1 + o_z] * Z[IP1 + o_z] + DELTA[I + o_delta] * DELTA[I + o_delta] * (DPSI + DPHI);
                            }
                        }
                        ETA = B / A;
                    }
                    else
                    {
                        if (A <= ZERO)
                        {
                            ETA = (A - Math.Sqrt(Math.Abs(A * A - FOUR * B * C))) / (TWO * C);
                        }
                        else
                        {
                            ETA = TWO * B / (A + Math.Sqrt(Math.Abs(A * A - FOUR * B * C)));
                        }
                    }
                }
                else
                {
                    // *
                    // *           Interpolation using THREE most relevant poles
                    // *
                    TEMP = RHOINV + PSI + PHI;
                    if (ORGATI)
                    {
                        TEMP1 = Z[IIM1 + o_z] / DELTA[IIM1 + o_delta];
                        TEMP1 *= TEMP1;
                        C = TEMP - DELTA[IIP1 + o_delta] * (DPSI + DPHI) - (D[IIM1 + o_d] - D[IIP1 + o_d]) * TEMP1;
                        ZZ[1 + o_zz] = Z[IIM1 + o_z] * Z[IIM1 + o_z];
                        ZZ[3 + o_zz] = DELTA[IIP1 + o_delta] * DELTA[IIP1 + o_delta] * ((DPSI - TEMP1) + DPHI);
                    }
                    else
                    {
                        TEMP1 = Z[IIP1 + o_z] / DELTA[IIP1 + o_delta];
                        TEMP1 *= TEMP1;
                        C = TEMP - DELTA[IIM1 + o_delta] * (DPSI + DPHI) - (D[IIP1 + o_d] - D[IIM1 + o_d]) * TEMP1;
                        ZZ[1 + o_zz] = DELTA[IIM1 + o_delta] * DELTA[IIM1 + o_delta] * (DPSI + (DPHI - TEMP1));
                        ZZ[3 + o_zz] = Z[IIP1 + o_z] * Z[IIP1 + o_z];
                    }
                    ZZ[2 + o_zz] = Z[II + o_z] * Z[II + o_z];
                    this._dlaed6.Run(NITER, ORGATI, C, DELTA, IIM1 + o_delta, ZZ, offset_zz, W
                                     , ref ETA, ref INFO);
                    if (INFO != 0) goto LABEL250;
                }
                // *
                // *        Note, eta should be positive if w is negative, and
                // *        eta should be negative otherwise. However,
                // *        if for some reason caused by roundoff, eta*w > 0,
                // *        we simply use one Newton step instead. This way
                // *        will guarantee eta*w < 0.
                // *
                if (W * ETA >= ZERO) ETA =  - W / DW;
                TEMP = TAU + ETA;
                if (TEMP > DLTUB || TEMP < DLTLB)
                {
                    if (W < ZERO)
                    {
                        ETA = (DLTUB - TAU) / TWO;
                    }
                    else
                    {
                        ETA = (DLTLB - TAU) / TWO;
                    }
                }
                // *
                PREW = W;
                // *
                for (J = 1; J <= N; J++)
                {
                    DELTA[J + o_delta] -= ETA;
                }
                // *
                // *        Evaluate PSI and the derivative DPSI
                // *
                DPSI = ZERO;
                PSI = ZERO;
                ERRETM = ZERO;
                for (J = 1; J <= IIM1; J++)
                {
                    TEMP = Z[J + o_z] / DELTA[J + o_delta];
                    PSI += Z[J + o_z] * TEMP;
                    DPSI += TEMP * TEMP;
                    ERRETM += PSI;
                }
                ERRETM = Math.Abs(ERRETM);
                // *
                // *        Evaluate PHI and the derivative DPHI
                // *
                DPHI = ZERO;
                PHI = ZERO;
                for (J = N; J >= IIP1; J +=  - 1)
                {
                    TEMP = Z[J + o_z] / DELTA[J + o_delta];
                    PHI += Z[J + o_z] * TEMP;
                    DPHI += TEMP * TEMP;
                    ERRETM += PHI;
                }
                // *
                TEMP = Z[II + o_z] / DELTA[II + o_delta];
                DW = DPSI + DPHI + TEMP * TEMP;
                TEMP *= Z[II + o_z];
                W = RHOINV + PHI + PSI + TEMP;
                ERRETM = EIGHT * (PHI - PSI) + ERRETM + TWO * RHOINV + THREE * Math.Abs(TEMP) + Math.Abs(TAU + ETA) * DW;
                // *
                SWTCH = false;
                if (ORGATI)
                {
                    if ( - W > Math.Abs(PREW) / TEN) SWTCH = true;
                }
                else
                {
                    if (W > Math.Abs(PREW) / TEN) SWTCH = true;
                }
                // *
                TAU += ETA;
                // *
                // *        Main loop to update the values of the array   DELTA
                // *
                ITER = NITER + 1;
                // *
                for (NITER = ITER; NITER <= MAXIT; NITER++)
                {
                    // *
                    // *           Test for convergence
                    // *
                    if (Math.Abs(W) <= EPS * ERRETM)
                    {
                        if (ORGATI)
                        {
                            DLAM = D[I + o_d] + TAU;
                        }
                        else
                        {
                            DLAM = D[IP1 + o_d] + TAU;
                        }
                        goto LABEL250;
                    }
                    // *
                    if (W <= ZERO)
                    {
                        DLTLB = Math.Max(DLTLB, TAU);
                    }
                    else
                    {
                        DLTUB = Math.Min(DLTUB, TAU);
                    }
                    // *
                    // *           Calculate the new step
                    // *
                    if (!SWTCH3)
                    {
                        if (!SWTCH)
                        {
                            if (ORGATI)
                            {
                                C = W - DELTA[IP1 + o_delta] * DW - (D[I + o_d] - D[IP1 + o_d]) * Math.Pow(Z[I + o_z] / DELTA[I + o_delta],2);
                            }
                            else
                            {
                                C = W - DELTA[I + o_delta] * DW - (D[IP1 + o_d] - D[I + o_d]) * Math.Pow(Z[IP1 + o_z] / DELTA[IP1 + o_delta],2);
                            }
                        }
                        else
                        {
                            TEMP = Z[II + o_z] / DELTA[II + o_delta];
                            if (ORGATI)
                            {
                                DPSI += TEMP * TEMP;
                            }
                            else
                            {
                                DPHI += TEMP * TEMP;
                            }
                            C = W - DELTA[I + o_delta] * DPSI - DELTA[IP1 + o_delta] * DPHI;
                        }
                        A = (DELTA[I + o_delta] + DELTA[IP1 + o_delta]) * W - DELTA[I + o_delta] * DELTA[IP1 + o_delta] * DW;
                        B = DELTA[I + o_delta] * DELTA[IP1 + o_delta] * W;
                        if (C == ZERO)
                        {
                            if (A == ZERO)
                            {
                                if (!SWTCH)
                                {
                                    if (ORGATI)
                                    {
                                        A = Z[I + o_z] * Z[I + o_z] + DELTA[IP1 + o_delta] * DELTA[IP1 + o_delta] * (DPSI + DPHI);
                                    }
                                    else
                                    {
                                        A = Z[IP1 + o_z] * Z[IP1 + o_z] + DELTA[I + o_delta] * DELTA[I + o_delta] * (DPSI + DPHI);
                                    }
                                }
                                else
                                {
                                    A = DELTA[I + o_delta] * DELTA[I + o_delta] * DPSI + DELTA[IP1 + o_delta] * DELTA[IP1 + o_delta] * DPHI;
                                }
                            }
                            ETA = B / A;
                        }
                        else
                        {
                            if (A <= ZERO)
                            {
                                ETA = (A - Math.Sqrt(Math.Abs(A * A - FOUR * B * C))) / (TWO * C);
                            }
                            else
                            {
                                ETA = TWO * B / (A + Math.Sqrt(Math.Abs(A * A - FOUR * B * C)));
                            }
                        }
                    }
                    else
                    {
                        // *
                        // *              Interpolation using THREE most relevant poles
                        // *
                        TEMP = RHOINV + PSI + PHI;
                        if (SWTCH)
                        {
                            C = TEMP - DELTA[IIM1 + o_delta] * DPSI - DELTA[IIP1 + o_delta] * DPHI;
                            ZZ[1 + o_zz] = DELTA[IIM1 + o_delta] * DELTA[IIM1 + o_delta] * DPSI;
                            ZZ[3 + o_zz] = DELTA[IIP1 + o_delta] * DELTA[IIP1 + o_delta] * DPHI;
                        }
                        else
                        {
                            if (ORGATI)
                            {
                                TEMP1 = Z[IIM1 + o_z] / DELTA[IIM1 + o_delta];
                                TEMP1 *= TEMP1;
                                C = TEMP - DELTA[IIP1 + o_delta] * (DPSI + DPHI) - (D[IIM1 + o_d] - D[IIP1 + o_d]) * TEMP1;
                                ZZ[1 + o_zz] = Z[IIM1 + o_z] * Z[IIM1 + o_z];
                                ZZ[3 + o_zz] = DELTA[IIP1 + o_delta] * DELTA[IIP1 + o_delta] * ((DPSI - TEMP1) + DPHI);
                            }
                            else
                            {
                                TEMP1 = Z[IIP1 + o_z] / DELTA[IIP1 + o_delta];
                                TEMP1 *= TEMP1;
                                C = TEMP - DELTA[IIM1 + o_delta] * (DPSI + DPHI) - (D[IIP1 + o_d] - D[IIM1 + o_d]) * TEMP1;
                                ZZ[1 + o_zz] = DELTA[IIM1 + o_delta] * DELTA[IIM1 + o_delta] * (DPSI + (DPHI - TEMP1));
                                ZZ[3 + o_zz] = Z[IIP1 + o_z] * Z[IIP1 + o_z];
                            }
                        }
                        this._dlaed6.Run(NITER, ORGATI, C, DELTA, IIM1 + o_delta, ZZ, offset_zz, W
                                         , ref ETA, ref INFO);
                        if (INFO != 0) goto LABEL250;
                    }
                    // *
                    // *           Note, eta should be positive if w is negative, and
                    // *           eta should be negative otherwise. However,
                    // *           if for some reason caused by roundoff, eta*w > 0,
                    // *           we simply use one Newton step instead. This way
                    // *           will guarantee eta*w < 0.
                    // *
                    if (W * ETA >= ZERO) ETA =  - W / DW;
                    TEMP = TAU + ETA;
                    if (TEMP > DLTUB || TEMP < DLTLB)
                    {
                        if (W < ZERO)
                        {
                            ETA = (DLTUB - TAU) / TWO;
                        }
                        else
                        {
                            ETA = (DLTLB - TAU) / TWO;
                        }
                    }
                    // *
                    for (J = 1; J <= N; J++)
                    {
                        DELTA[J + o_delta] -= ETA;
                    }
                    // *
                    TAU += ETA;
                    PREW = W;
                    // *
                    // *           Evaluate PSI and the derivative DPSI
                    // *
                    DPSI = ZERO;
                    PSI = ZERO;
                    ERRETM = ZERO;
                    for (J = 1; J <= IIM1; J++)
                    {
                        TEMP = Z[J + o_z] / DELTA[J + o_delta];
                        PSI += Z[J + o_z] * TEMP;
                        DPSI += TEMP * TEMP;
                        ERRETM += PSI;
                    }
                    ERRETM = Math.Abs(ERRETM);
                    // *
                    // *           Evaluate PHI and the derivative DPHI
                    // *
                    DPHI = ZERO;
                    PHI = ZERO;
                    for (J = N; J >= IIP1; J +=  - 1)
                    {
                        TEMP = Z[J + o_z] / DELTA[J + o_delta];
                        PHI += Z[J + o_z] * TEMP;
                        DPHI += TEMP * TEMP;
                        ERRETM += PHI;
                    }
                    // *
                    TEMP = Z[II + o_z] / DELTA[II + o_delta];
                    DW = DPSI + DPHI + TEMP * TEMP;
                    TEMP *= Z[II + o_z];
                    W = RHOINV + PHI + PSI + TEMP;
                    ERRETM = EIGHT * (PHI - PSI) + ERRETM + TWO * RHOINV + THREE * Math.Abs(TEMP) + Math.Abs(TAU) * DW;
                    if (W * PREW > ZERO && Math.Abs(W) > Math.Abs(PREW) / TEN) SWTCH = !SWTCH;
                    // *
                }
                // *
                // *        Return with INFO = 1, NITER = MAXIT and not converged
                // *
                INFO = 1;
                if (ORGATI)
                {
                    DLAM = D[I + o_d] + TAU;
                }
                else
                {
                    DLAM = D[IP1 + o_d] + TAU;
                }
                // *
            }
            // *
        LABEL250:;
            // *
            return;
            // *
            // *     End of DLAED4
            // *

            #endregion

        }
    }
}
