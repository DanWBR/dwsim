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
    /// -- LAPACK auxiliary routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// This subroutine computes the square root of the I-th updated
    /// eigenvalue of a positive symmetric rank-one modification to
    /// a positive diagonal matrix whose entries are given as the squares
    /// of the corresponding entries in the array d, and that
    /// 
    /// 0 .LE. D(i) .LT. D(j)  for  i .LT. j
    /// 
    /// and that RHO .GT. 0. This is arranged by the calling routine, and is
    /// no loss in generality.  The rank-one modified system is thus
    /// 
    /// diag( D ) * diag( D ) +  RHO *  Z * Z_transpose.
    /// 
    /// where we assume the Euclidean norm of Z is 1.
    /// 
    /// The method consists of approximating the rational functions in the
    /// secular equation by simpler interpolating rational functions.
    /// 
    ///</summary>
    public class DLASD4
    {
    

        #region Dependencies
        
        DLAED6 _dlaed6; DLASD5 _dlasd5; DLAMCH _dlamch; 

        #endregion


        #region Variables
        
        const int MAXIT = 20; const double ZERO = 0.0E+0; const double ONE = 1.0E+0; const double TWO = 2.0E+0; 
        const double THREE = 3.0E+0;const double FOUR = 4.0E+0; const double EIGHT = 8.0E+0; const double TEN = 10.0E+0; 
        double[] DD = new double[3];double[] ZZ = new double[3]; 

        #endregion

        public DLASD4(DLAED6 dlaed6, DLASD5 dlasd5, DLAMCH dlamch)
        {
    

            #region Set Dependencies
            
            this._dlaed6 = dlaed6; this._dlasd5 = dlasd5; this._dlamch = dlamch; 

            #endregion

        }
    
        public DLASD4()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLASD5 dlasd5 = new DLASD5();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLAED6 dlaed6 = new DLAED6(dlamch);

            #endregion


            #region Set Dependencies
            
            this._dlaed6 = dlaed6; this._dlasd5 = dlasd5; this._dlamch = dlamch; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// This subroutine computes the square root of the I-th updated
        /// eigenvalue of a positive symmetric rank-one modification to
        /// a positive diagonal matrix whose entries are given as the squares
        /// of the corresponding entries in the array d, and that
        /// 
        /// 0 .LE. D(i) .LT. D(j)  for  i .LT. j
        /// 
        /// and that RHO .GT. 0. This is arranged by the calling routine, and is
        /// no loss in generality.  The rank-one modified system is thus
        /// 
        /// diag( D ) * diag( D ) +  RHO *  Z * Z_transpose.
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
        /// (input) DOUBLE PRECISION array, dimension ( N )
        /// The original eigenvalues.  It is assumed that they are in
        /// order, 0 .LE. D(I) .LT. D(J)  for I .LT. J.
        ///</param>
        /// <param name="Z">
        /// (input) DOUBLE PRECISION array, dimension ( N )
        /// The components of the updating vector.
        ///</param>
        /// <param name="DELTA">
        /// (output) DOUBLE PRECISION array, dimension ( N )
        /// If N .ne. 1, DELTA contains (D(j) - sigma_I) in its  j-th
        /// component.  If N = 1, then DELTA(1) = 1.  The vector DELTA
        /// contains the information necessary to construct the
        /// (singular) eigenvectors.
        ///</param>
        /// <param name="RHO">
        /// (input) DOUBLE PRECISION
        /// The scalar in the symmetric updating formula.
        ///</param>
        /// <param name="SIGMA">
        /// (output) DOUBLE PRECISION
        /// The computed sigma_I, the I-th updated eigenvalue.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension ( N )
        /// If N .ne. 1, WORK contains (D(j) + sigma_I) in its  j-th
        /// component.  If N = 1, then WORK( 1 ) = 1.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .GT. 0:  if INFO = 1, the updating process failed.
        ///</param>
        public void Run(int N, int I, double[] D, int offset_d, double[] Z, int offset_z, ref double[] DELTA, int offset_delta, double RHO
                         , ref double SIGMA, ref double[] WORK, int offset_work, ref int INFO)
        {

            #region Variables
            
            bool ORGATI = false; bool SWTCH = false; bool SWTCH3 = false; int II = 0; int IIM1 = 0; int IIP1 = 0; int IP1 = 0; 
            int ITER = 0;int J = 0; int NITER = 0; double A = 0; double B = 0; double C = 0; double DELSQ = 0; double DELSQ2 = 0; 
            double DPHI = 0;double DPSI = 0; double DTIIM = 0; double DTIIP = 0; double DTIPSQ = 0; double DTISQ = 0; 
            double DTNSQ = 0;double DTNSQ1 = 0; double DW = 0; double EPS = 0; double ERRETM = 0; double ETA = 0; double PHI = 0; 
            double PREW = 0;double PSI = 0; double RHOINV = 0; double SG2LB = 0; double SG2UB = 0; double TAU = 0; 
            double TEMP = 0;double TEMP1 = 0; double TEMP2 = 0; double W = 0; int offset_dd = 0; int o_dd = -1; 
            int offset_zz = 0; int o_zz = -1;

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_z = -1 + offset_z;  int o_delta = -1 + offset_delta;  int o_work = -1 + offset_work; 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.1) --
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
            // *  This subroutine computes the square root of the I-th updated
            // *  eigenvalue of a positive symmetric rank-one modification to
            // *  a positive diagonal matrix whose entries are given as the squares
            // *  of the corresponding entries in the array d, and that
            // *
            // *         0 <= D(i) < D(j)  for  i < j
            // *
            // *  and that RHO > 0. This is arranged by the calling routine, and is
            // *  no loss in generality.  The rank-one modified system is thus
            // *
            // *         diag( D ) * diag( D ) +  RHO *  Z * Z_transpose.
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
            // *  D      (input) DOUBLE PRECISION array, dimension ( N )
            // *         The original eigenvalues.  It is assumed that they are in
            // *         order, 0 <= D(I) < D(J)  for I < J.
            // *
            // *  Z      (input) DOUBLE PRECISION array, dimension ( N )
            // *         The components of the updating vector.
            // *
            // *  DELTA  (output) DOUBLE PRECISION array, dimension ( N )
            // *         If N .ne. 1, DELTA contains (D(j) - sigma_I) in its  j-th
            // *         component.  If N = 1, then DELTA(1) = 1.  The vector DELTA
            // *         contains the information necessary to construct the
            // *         (singular) eigenvectors.
            // *
            // *  RHO    (input) DOUBLE PRECISION
            // *         The scalar in the symmetric updating formula.
            // *
            // *  SIGMA  (output) DOUBLE PRECISION
            // *         The computed sigma_I, the I-th updated eigenvalue.
            // *
            // *  WORK   (workspace) DOUBLE PRECISION array, dimension ( N )
            // *         If N .ne. 1, WORK contains (D(j) + sigma_I) in its  j-th
            // *         component.  If N = 1, then WORK( 1 ) = 1.
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
            // *  Logical variable SWTCH3 (switch-for-3-poles?) is for noting
            // *  if we are working with THREE poles!
            // *
            // *  MAXIT is the maximum number of iterations allowed for each
            // *  eigenvalue.
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
            // *     .. External Subroutines ..
            // *     ..
            // *     .. External Functions ..
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
                // *        Presumably, I=1 upon entry
                // *
                SIGMA = Math.Sqrt(D[1 + o_d] * D[1 + o_d] + RHO * Z[1 + o_z] * Z[1 + o_z]);
                DELTA[1 + o_delta] = ONE;
                WORK[1 + o_work] = ONE;
                return;
            }
            if (N == 2)
            {
                this._dlasd5.Run(I, D, offset_d, Z, offset_z, ref DELTA, offset_delta, RHO, ref SIGMA
                                 , ref WORK, offset_work);
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
                TEMP = RHO / TWO;
                // *
                // *        If ||Z||_2 is not one, then TEMP should be set to
                // *        RHO * ||Z||_2^2 / TWO
                // *
                TEMP1 = TEMP / (D[N + o_d] + Math.Sqrt(D[N + o_d] * D[N + o_d] + TEMP));
                for (J = 1; J <= N; J++)
                {
                    WORK[J + o_work] = D[J + o_d] + D[N + o_d] + TEMP1;
                    DELTA[J + o_delta] = (D[J + o_d] - D[N + o_d]) - TEMP1;
                }
                // *
                PSI = ZERO;
                for (J = 1; J <= N - 2; J++)
                {
                    PSI += Z[J + o_z] * Z[J + o_z] / (DELTA[J + o_delta] * WORK[J + o_work]);
                }
                // *
                C = RHOINV + PSI;
                W = C + Z[II + o_z] * Z[II + o_z] / (DELTA[II + o_delta] * WORK[II + o_work]) + Z[N + o_z] * Z[N + o_z] / (DELTA[N + o_delta] * WORK[N + o_work]);
                // *
                if (W <= ZERO)
                {
                    TEMP1 = Math.Sqrt(D[N + o_d] * D[N + o_d] + RHO);
                    TEMP = Z[N - 1 + o_z] * Z[N - 1 + o_z] / ((D[N - 1 + o_d] + TEMP1) * (D[N + o_d] - D[N - 1 + o_d] + RHO / (D[N + o_d] + TEMP1))) + Z[N + o_z] * Z[N + o_z] / RHO;
                    // *
                    // *           The following TAU is to approximate
                    // *           SIGMA_n^2 - D( N )*D( N )
                    // *
                    if (C <= TEMP)
                    {
                        TAU = RHO;
                    }
                    else
                    {
                        DELSQ = (D[N + o_d] - D[N - 1 + o_d]) * (D[N + o_d] + D[N - 1 + o_d]);
                        A =  - C * DELSQ + Z[N - 1 + o_z] * Z[N - 1 + o_z] + Z[N + o_z] * Z[N + o_z];
                        B = Z[N + o_z] * Z[N + o_z] * DELSQ;
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
                    // *               D(N)^2+RHO/2 <= SIGMA_n^2 < D(N)^2+TAU <= D(N)^2+RHO
                    // *
                }
                else
                {
                    DELSQ = (D[N + o_d] - D[N - 1 + o_d]) * (D[N + o_d] + D[N - 1 + o_d]);
                    A =  - C * DELSQ + Z[N - 1 + o_z] * Z[N - 1 + o_z] + Z[N + o_z] * Z[N + o_z];
                    B = Z[N + o_z] * Z[N + o_z] * DELSQ;
                    // *
                    // *           The following TAU is to approximate
                    // *           SIGMA_n^2 - D( N )*D( N )
                    // *
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
                    // *           D(N)^2 < D(N)^2+TAU < SIGMA(N)^2 < D(N)^2+RHO/2
                    // *
                }
                // *
                // *        The following ETA is to approximate SIGMA_n - D( N )
                // *
                ETA = TAU / (D[N + o_d] + Math.Sqrt(D[N + o_d] * D[N + o_d] + TAU));
                // *
                SIGMA = D[N + o_d] + ETA;
                for (J = 1; J <= N; J++)
                {
                    DELTA[J + o_delta] = (D[J + o_d] - D[I + o_d]) - ETA;
                    WORK[J + o_work] = D[J + o_d] + D[I + o_d] + ETA;
                }
                // *
                // *        Evaluate PSI and the derivative DPSI
                // *
                DPSI = ZERO;
                PSI = ZERO;
                ERRETM = ZERO;
                for (J = 1; J <= II; J++)
                {
                    TEMP = Z[J + o_z] / (DELTA[J + o_delta] * WORK[J + o_work]);
                    PSI += Z[J + o_z] * TEMP;
                    DPSI += TEMP * TEMP;
                    ERRETM += PSI;
                }
                ERRETM = Math.Abs(ERRETM);
                // *
                // *        Evaluate PHI and the derivative DPHI
                // *
                TEMP = Z[N + o_z] / (DELTA[N + o_delta] * WORK[N + o_work]);
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
                    goto LABEL240;
                }
                // *
                // *        Calculate the new step
                // *
                NITER += 1;
                DTNSQ1 = WORK[N - 1 + o_work] * DELTA[N - 1 + o_delta];
                DTNSQ = WORK[N + o_work] * DELTA[N + o_delta];
                C = W - DTNSQ1 * DPSI - DTNSQ * DPHI;
                A = (DTNSQ + DTNSQ1) * W - DTNSQ * DTNSQ1 * (DPSI + DPHI);
                B = DTNSQ * DTNSQ1 * W;
                if (C < ZERO) C = Math.Abs(C);
                if (C == ZERO)
                {
                    ETA = RHO - SIGMA * SIGMA;
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
                TEMP = ETA - DTNSQ;
                if (TEMP > RHO) ETA = RHO + DTNSQ;
                // *
                TAU += ETA;
                ETA = ETA / (SIGMA + Math.Sqrt(ETA + SIGMA * SIGMA));
                for (J = 1; J <= N; J++)
                {
                    DELTA[J + o_delta] -= ETA;
                    WORK[J + o_work] += ETA;
                }
                // *
                SIGMA += ETA;
                // *
                // *        Evaluate PSI and the derivative DPSI
                // *
                DPSI = ZERO;
                PSI = ZERO;
                ERRETM = ZERO;
                for (J = 1; J <= II; J++)
                {
                    TEMP = Z[J + o_z] / (WORK[J + o_work] * DELTA[J + o_delta]);
                    PSI += Z[J + o_z] * TEMP;
                    DPSI += TEMP * TEMP;
                    ERRETM += PSI;
                }
                ERRETM = Math.Abs(ERRETM);
                // *
                // *        Evaluate PHI and the derivative DPHI
                // *
                TEMP = Z[N + o_z] / (WORK[N + o_work] * DELTA[N + o_delta]);
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
                        goto LABEL240;
                    }
                    // *
                    // *           Calculate the new step
                    // *
                    DTNSQ1 = WORK[N - 1 + o_work] * DELTA[N - 1 + o_delta];
                    DTNSQ = WORK[N + o_work] * DELTA[N + o_delta];
                    C = W - DTNSQ1 * DPSI - DTNSQ * DPHI;
                    A = (DTNSQ + DTNSQ1) * W - DTNSQ1 * DTNSQ * (DPSI + DPHI);
                    B = DTNSQ1 * DTNSQ * W;
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
                    TEMP = ETA - DTNSQ;
                    if (TEMP <= ZERO) ETA /= TWO;
                    // *
                    TAU += ETA;
                    ETA = ETA / (SIGMA + Math.Sqrt(ETA + SIGMA * SIGMA));
                    for (J = 1; J <= N; J++)
                    {
                        DELTA[J + o_delta] -= ETA;
                        WORK[J + o_work] += ETA;
                    }
                    // *
                    SIGMA += ETA;
                    // *
                    // *           Evaluate PSI and the derivative DPSI
                    // *
                    DPSI = ZERO;
                    PSI = ZERO;
                    ERRETM = ZERO;
                    for (J = 1; J <= II; J++)
                    {
                        TEMP = Z[J + o_z] / (WORK[J + o_work] * DELTA[J + o_delta]);
                        PSI += Z[J + o_z] * TEMP;
                        DPSI += TEMP * TEMP;
                        ERRETM += PSI;
                    }
                    ERRETM = Math.Abs(ERRETM);
                    // *
                    // *           Evaluate PHI and the derivative DPHI
                    // *
                    TEMP = Z[N + o_z] / (WORK[N + o_work] * DELTA[N + o_delta]);
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
                goto LABEL240;
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
                DELSQ = (D[IP1 + o_d] - D[I + o_d]) * (D[IP1 + o_d] + D[I + o_d]);
                DELSQ2 = DELSQ / TWO;
                TEMP = DELSQ2 / (D[I + o_d] + Math.Sqrt(D[I + o_d] * D[I + o_d] + DELSQ2));
                for (J = 1; J <= N; J++)
                {
                    WORK[J + o_work] = D[J + o_d] + D[I + o_d] + TEMP;
                    DELTA[J + o_delta] = (D[J + o_d] - D[I + o_d]) - TEMP;
                }
                // *
                PSI = ZERO;
                for (J = 1; J <= I - 1; J++)
                {
                    PSI += Z[J + o_z] * Z[J + o_z] / (WORK[J + o_work] * DELTA[J + o_delta]);
                }
                // *
                PHI = ZERO;
                for (J = N; J >= I + 2; J +=  - 1)
                {
                    PHI += Z[J + o_z] * Z[J + o_z] / (WORK[J + o_work] * DELTA[J + o_delta]);
                }
                C = RHOINV + PSI + PHI;
                W = C + Z[I + o_z] * Z[I + o_z] / (WORK[I + o_work] * DELTA[I + o_delta]) + Z[IP1 + o_z] * Z[IP1 + o_z] / (WORK[IP1 + o_work] * DELTA[IP1 + o_delta]);
                // *
                if (W > ZERO)
                {
                    // *
                    // *           d(i)^2 < the ith sigma^2 < (d(i)^2+d(i+1)^2)/2
                    // *
                    // *           We choose d(i) as origin.
                    // *
                    ORGATI = true;
                    SG2LB = ZERO;
                    SG2UB = DELSQ2;
                    A = C * DELSQ + Z[I + o_z] * Z[I + o_z] + Z[IP1 + o_z] * Z[IP1 + o_z];
                    B = Z[I + o_z] * Z[I + o_z] * DELSQ;
                    if (A > ZERO)
                    {
                        TAU = TWO * B / (A + Math.Sqrt(Math.Abs(A * A - FOUR * B * C)));
                    }
                    else
                    {
                        TAU = (A - Math.Sqrt(Math.Abs(A * A - FOUR * B * C))) / (TWO * C);
                    }
                    // *
                    // *           TAU now is an estimation of SIGMA^2 - D( I )^2. The
                    // *           following, however, is the corresponding estimation of
                    // *           SIGMA - D( I ).
                    // *
                    ETA = TAU / (D[I + o_d] + Math.Sqrt(D[I + o_d] * D[I + o_d] + TAU));
                }
                else
                {
                    // *
                    // *           (d(i)^2+d(i+1)^2)/2 <= the ith sigma^2 < d(i+1)^2/2
                    // *
                    // *           We choose d(i+1) as origin.
                    // *
                    ORGATI = false;
                    SG2LB =  - DELSQ2;
                    SG2UB = ZERO;
                    A = C * DELSQ - Z[I + o_z] * Z[I + o_z] - Z[IP1 + o_z] * Z[IP1 + o_z];
                    B = Z[IP1 + o_z] * Z[IP1 + o_z] * DELSQ;
                    if (A < ZERO)
                    {
                        TAU = TWO * B / (A - Math.Sqrt(Math.Abs(A * A + FOUR * B * C)));
                    }
                    else
                    {
                        TAU =  - (A + Math.Sqrt(Math.Abs(A * A + FOUR * B * C))) / (TWO * C);
                    }
                    // *
                    // *           TAU now is an estimation of SIGMA^2 - D( IP1 )^2. The
                    // *           following, however, is the corresponding estimation of
                    // *           SIGMA - D( IP1 ).
                    // *
                    ETA = TAU / (D[IP1 + o_d] + Math.Sqrt(Math.Abs(D[IP1 + o_d] * D[IP1 + o_d] + TAU)));
                }
                // *
                if (ORGATI)
                {
                    II = I;
                    SIGMA = D[I + o_d] + ETA;
                    for (J = 1; J <= N; J++)
                    {
                        WORK[J + o_work] = D[J + o_d] + D[I + o_d] + ETA;
                        DELTA[J + o_delta] = (D[J + o_d] - D[I + o_d]) - ETA;
                    }
                }
                else
                {
                    II = I + 1;
                    SIGMA = D[IP1 + o_d] + ETA;
                    for (J = 1; J <= N; J++)
                    {
                        WORK[J + o_work] = D[J + o_d] + D[IP1 + o_d] + ETA;
                        DELTA[J + o_delta] = (D[J + o_d] - D[IP1 + o_d]) - ETA;
                    }
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
                    TEMP = Z[J + o_z] / (WORK[J + o_work] * DELTA[J + o_delta]);
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
                    TEMP = Z[J + o_z] / (WORK[J + o_work] * DELTA[J + o_delta]);
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
                TEMP = Z[II + o_z] / (WORK[II + o_work] * DELTA[II + o_delta]);
                DW = DPSI + DPHI + TEMP * TEMP;
                TEMP *= Z[II + o_z];
                W += TEMP;
                ERRETM = EIGHT * (PHI - PSI) + ERRETM + TWO * RHOINV + THREE * Math.Abs(TEMP) + Math.Abs(TAU) * DW;
                // *
                // *        Test for convergence
                // *
                if (Math.Abs(W) <= EPS * ERRETM)
                {
                    goto LABEL240;
                }
                // *
                if (W <= ZERO)
                {
                    SG2LB = Math.Max(SG2LB, TAU);
                }
                else
                {
                    SG2UB = Math.Min(SG2UB, TAU);
                }
                // *
                // *        Calculate the new step
                // *
                NITER += 1;
                if (!SWTCH3)
                {
                    DTIPSQ = WORK[IP1 + o_work] * DELTA[IP1 + o_delta];
                    DTISQ = WORK[I + o_work] * DELTA[I + o_delta];
                    if (ORGATI)
                    {
                        C = W - DTIPSQ * DW + DELSQ * Math.Pow(Z[I + o_z] / DTISQ,2);
                    }
                    else
                    {
                        C = W - DTISQ * DW - DELSQ * Math.Pow(Z[IP1 + o_z] / DTIPSQ,2);
                    }
                    A = (DTIPSQ + DTISQ) * W - DTIPSQ * DTISQ * DW;
                    B = DTIPSQ * DTISQ * W;
                    if (C == ZERO)
                    {
                        if (A == ZERO)
                        {
                            if (ORGATI)
                            {
                                A = Z[I + o_z] * Z[I + o_z] + DTIPSQ * DTIPSQ * (DPSI + DPHI);
                            }
                            else
                            {
                                A = Z[IP1 + o_z] * Z[IP1 + o_z] + DTISQ * DTISQ * (DPSI + DPHI);
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
                    DTIIM = WORK[IIM1 + o_work] * DELTA[IIM1 + o_delta];
                    DTIIP = WORK[IIP1 + o_work] * DELTA[IIP1 + o_delta];
                    TEMP = RHOINV + PSI + PHI;
                    if (ORGATI)
                    {
                        TEMP1 = Z[IIM1 + o_z] / DTIIM;
                        TEMP1 *= TEMP1;
                        C = (TEMP - DTIIP * (DPSI + DPHI)) - (D[IIM1 + o_d] - D[IIP1 + o_d]) * (D[IIM1 + o_d] + D[IIP1 + o_d]) * TEMP1;
                        ZZ[1 + o_zz] = Z[IIM1 + o_z] * Z[IIM1 + o_z];
                        if (DPSI < TEMP1)
                        {
                            ZZ[3 + o_zz] = DTIIP * DTIIP * DPHI;
                        }
                        else
                        {
                            ZZ[3 + o_zz] = DTIIP * DTIIP * ((DPSI - TEMP1) + DPHI);
                        }
                    }
                    else
                    {
                        TEMP1 = Z[IIP1 + o_z] / DTIIP;
                        TEMP1 *= TEMP1;
                        C = (TEMP - DTIIM * (DPSI + DPHI)) - (D[IIP1 + o_d] - D[IIM1 + o_d]) * (D[IIM1 + o_d] + D[IIP1 + o_d]) * TEMP1;
                        if (DPHI < TEMP1)
                        {
                            ZZ[1 + o_zz] = DTIIM * DTIIM * DPSI;
                        }
                        else
                        {
                            ZZ[1 + o_zz] = DTIIM * DTIIM * (DPSI + (DPHI - TEMP1));
                        }
                        ZZ[3 + o_zz] = Z[IIP1 + o_z] * Z[IIP1 + o_z];
                    }
                    ZZ[2 + o_zz] = Z[II + o_z] * Z[II + o_z];
                    DD[1 + o_dd] = DTIIM;
                    DD[2 + o_dd] = DELTA[II + o_delta] * WORK[II + o_work];
                    DD[3 + o_dd] = DTIIP;
                    this._dlaed6.Run(NITER, ORGATI, C, DD, offset_dd, ZZ, offset_zz, W
                                     , ref ETA, ref INFO);
                    if (INFO != 0) goto LABEL240;
                }
                // *
                // *        Note, eta should be positive if w is negative, and
                // *        eta should be negative otherwise. However,
                // *        if for some reason caused by roundoff, eta*w > 0,
                // *        we simply use one Newton step instead. This way
                // *        will guarantee eta*w < 0.
                // *
                if (W * ETA >= ZERO) ETA =  - W / DW;
                if (ORGATI)
                {
                    TEMP1 = WORK[I + o_work] * DELTA[I + o_delta];
                    TEMP = ETA - TEMP1;
                }
                else
                {
                    TEMP1 = WORK[IP1 + o_work] * DELTA[IP1 + o_delta];
                    TEMP = ETA - TEMP1;
                }
                if (TEMP > SG2UB || TEMP < SG2LB)
                {
                    if (W < ZERO)
                    {
                        ETA = (SG2UB - TAU) / TWO;
                    }
                    else
                    {
                        ETA = (SG2LB - TAU) / TWO;
                    }
                }
                // *
                TAU += ETA;
                ETA = ETA / (SIGMA + Math.Sqrt(SIGMA * SIGMA + ETA));
                // *
                PREW = W;
                // *
                SIGMA += ETA;
                for (J = 1; J <= N; J++)
                {
                    WORK[J + o_work] += ETA;
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
                    TEMP = Z[J + o_z] / (WORK[J + o_work] * DELTA[J + o_delta]);
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
                    TEMP = Z[J + o_z] / (WORK[J + o_work] * DELTA[J + o_delta]);
                    PHI += Z[J + o_z] * TEMP;
                    DPHI += TEMP * TEMP;
                    ERRETM += PHI;
                }
                // *
                TEMP = Z[II + o_z] / (WORK[II + o_work] * DELTA[II + o_delta]);
                DW = DPSI + DPHI + TEMP * TEMP;
                TEMP *= Z[II + o_z];
                W = RHOINV + PHI + PSI + TEMP;
                ERRETM = EIGHT * (PHI - PSI) + ERRETM + TWO * RHOINV + THREE * Math.Abs(TEMP) + Math.Abs(TAU) * DW;
                // *
                if (W <= ZERO)
                {
                    SG2LB = Math.Max(SG2LB, TAU);
                }
                else
                {
                    SG2UB = Math.Min(SG2UB, TAU);
                }
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
                // *        Main loop to update the values of the array   DELTA and WORK
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
                        goto LABEL240;
                    }
                    // *
                    // *           Calculate the new step
                    // *
                    if (!SWTCH3)
                    {
                        DTIPSQ = WORK[IP1 + o_work] * DELTA[IP1 + o_delta];
                        DTISQ = WORK[I + o_work] * DELTA[I + o_delta];
                        if (!SWTCH)
                        {
                            if (ORGATI)
                            {
                                C = W - DTIPSQ * DW + DELSQ * Math.Pow(Z[I + o_z] / DTISQ,2);
                            }
                            else
                            {
                                C = W - DTISQ * DW - DELSQ * Math.Pow(Z[IP1 + o_z] / DTIPSQ,2);
                            }
                        }
                        else
                        {
                            TEMP = Z[II + o_z] / (WORK[II + o_work] * DELTA[II + o_delta]);
                            if (ORGATI)
                            {
                                DPSI += TEMP * TEMP;
                            }
                            else
                            {
                                DPHI += TEMP * TEMP;
                            }
                            C = W - DTISQ * DPSI - DTIPSQ * DPHI;
                        }
                        A = (DTIPSQ + DTISQ) * W - DTIPSQ * DTISQ * DW;
                        B = DTIPSQ * DTISQ * W;
                        if (C == ZERO)
                        {
                            if (A == ZERO)
                            {
                                if (!SWTCH)
                                {
                                    if (ORGATI)
                                    {
                                        A = Z[I + o_z] * Z[I + o_z] + DTIPSQ * DTIPSQ * (DPSI + DPHI);
                                    }
                                    else
                                    {
                                        A = Z[IP1 + o_z] * Z[IP1 + o_z] + DTISQ * DTISQ * (DPSI + DPHI);
                                    }
                                }
                                else
                                {
                                    A = DTISQ * DTISQ * DPSI + DTIPSQ * DTIPSQ * DPHI;
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
                        DTIIM = WORK[IIM1 + o_work] * DELTA[IIM1 + o_delta];
                        DTIIP = WORK[IIP1 + o_work] * DELTA[IIP1 + o_delta];
                        TEMP = RHOINV + PSI + PHI;
                        if (SWTCH)
                        {
                            C = TEMP - DTIIM * DPSI - DTIIP * DPHI;
                            ZZ[1 + o_zz] = DTIIM * DTIIM * DPSI;
                            ZZ[3 + o_zz] = DTIIP * DTIIP * DPHI;
                        }
                        else
                        {
                            if (ORGATI)
                            {
                                TEMP1 = Z[IIM1 + o_z] / DTIIM;
                                TEMP1 *= TEMP1;
                                TEMP2 = (D[IIM1 + o_d] - D[IIP1 + o_d]) * (D[IIM1 + o_d] + D[IIP1 + o_d]) * TEMP1;
                                C = TEMP - DTIIP * (DPSI + DPHI) - TEMP2;
                                ZZ[1 + o_zz] = Z[IIM1 + o_z] * Z[IIM1 + o_z];
                                if (DPSI < TEMP1)
                                {
                                    ZZ[3 + o_zz] = DTIIP * DTIIP * DPHI;
                                }
                                else
                                {
                                    ZZ[3 + o_zz] = DTIIP * DTIIP * ((DPSI - TEMP1) + DPHI);
                                }
                            }
                            else
                            {
                                TEMP1 = Z[IIP1 + o_z] / DTIIP;
                                TEMP1 *= TEMP1;
                                TEMP2 = (D[IIP1 + o_d] - D[IIM1 + o_d]) * (D[IIM1 + o_d] + D[IIP1 + o_d]) * TEMP1;
                                C = TEMP - DTIIM * (DPSI + DPHI) - TEMP2;
                                if (DPHI < TEMP1)
                                {
                                    ZZ[1 + o_zz] = DTIIM * DTIIM * DPSI;
                                }
                                else
                                {
                                    ZZ[1 + o_zz] = DTIIM * DTIIM * (DPSI + (DPHI - TEMP1));
                                }
                                ZZ[3 + o_zz] = Z[IIP1 + o_z] * Z[IIP1 + o_z];
                            }
                        }
                        DD[1 + o_dd] = DTIIM;
                        DD[2 + o_dd] = DELTA[II + o_delta] * WORK[II + o_work];
                        DD[3 + o_dd] = DTIIP;
                        this._dlaed6.Run(NITER, ORGATI, C, DD, offset_dd, ZZ, offset_zz, W
                                         , ref ETA, ref INFO);
                        if (INFO != 0) goto LABEL240;
                    }
                    // *
                    // *           Note, eta should be positive if w is negative, and
                    // *           eta should be negative otherwise. However,
                    // *           if for some reason caused by roundoff, eta*w > 0,
                    // *           we simply use one Newton step instead. This way
                    // *           will guarantee eta*w < 0.
                    // *
                    if (W * ETA >= ZERO) ETA =  - W / DW;
                    if (ORGATI)
                    {
                        TEMP1 = WORK[I + o_work] * DELTA[I + o_delta];
                        TEMP = ETA - TEMP1;
                    }
                    else
                    {
                        TEMP1 = WORK[IP1 + o_work] * DELTA[IP1 + o_delta];
                        TEMP = ETA - TEMP1;
                    }
                    if (TEMP > SG2UB || TEMP < SG2LB)
                    {
                        if (W < ZERO)
                        {
                            ETA = (SG2UB - TAU) / TWO;
                        }
                        else
                        {
                            ETA = (SG2LB - TAU) / TWO;
                        }
                    }
                    // *
                    TAU += ETA;
                    ETA = ETA / (SIGMA + Math.Sqrt(SIGMA * SIGMA + ETA));
                    // *
                    SIGMA += ETA;
                    for (J = 1; J <= N; J++)
                    {
                        WORK[J + o_work] += ETA;
                        DELTA[J + o_delta] -= ETA;
                    }
                    // *
                    PREW = W;
                    // *
                    // *           Evaluate PSI and the derivative DPSI
                    // *
                    DPSI = ZERO;
                    PSI = ZERO;
                    ERRETM = ZERO;
                    for (J = 1; J <= IIM1; J++)
                    {
                        TEMP = Z[J + o_z] / (WORK[J + o_work] * DELTA[J + o_delta]);
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
                        TEMP = Z[J + o_z] / (WORK[J + o_work] * DELTA[J + o_delta]);
                        PHI += Z[J + o_z] * TEMP;
                        DPHI += TEMP * TEMP;
                        ERRETM += PHI;
                    }
                    // *
                    TEMP = Z[II + o_z] / (WORK[II + o_work] * DELTA[II + o_delta]);
                    DW = DPSI + DPHI + TEMP * TEMP;
                    TEMP *= Z[II + o_z];
                    W = RHOINV + PHI + PSI + TEMP;
                    ERRETM = EIGHT * (PHI - PSI) + ERRETM + TWO * RHOINV + THREE * Math.Abs(TEMP) + Math.Abs(TAU) * DW;
                    if (W * PREW > ZERO && Math.Abs(W) > Math.Abs(PREW) / TEN) SWTCH = !SWTCH;
                    // *
                    if (W <= ZERO)
                    {
                        SG2LB = Math.Max(SG2LB, TAU);
                    }
                    else
                    {
                        SG2UB = Math.Min(SG2UB, TAU);
                    }
                    // *
                }
                // *
                // *        Return with INFO = 1, NITER = MAXIT and not converged
                // *
                INFO = 1;
                // *
            }
            // *
        LABEL240:;
            return;
            // *
            // *     End of DLASD4
            // *

            #endregion

        }
    }
}
