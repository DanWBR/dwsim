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

    #region The Class: DLAMCH
    
    /// <summary>
    /// -- LAPACK auxiliary routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DLAMCH determines double precision machine parameters.
    /// 
    ///</summary>
    public class DLAMCH
    {
    
        #region Dependencies
        
        LSAME _lsame; DLAMC2 _dlamc2; 
        #endregion
        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; bool FIRST = false; double BASE = 0; double EMAX = 0; 
        double EMIN = 0;double EPS = 0; double PREC = 0; double RMAX = 0; double RMIN = 0; double RND = 0; double SFMIN = 0; 
        double T = 0;
        #endregion
        public DLAMCH(LSAME lsame, DLAMC2 dlamc2)
        {
    
            #region Set Dependencies
            
            this._lsame = lsame; this._dlamc2 = dlamc2; 
            #endregion
            #region Data Initialization
            
            //FIRST/.TRUE.
            FIRST = true;
            #endregion
        }
    
        public DLAMCH()
        {
    
            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            #endregion
            #region Set Dependencies
            
            this._lsame = lsame; this._dlamc2 = dlamc2; 
            #endregion
            #region Data Initialization
            
            //FIRST/.TRUE.
            FIRST = true;
            #endregion
        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAMCH determines double precision machine parameters.
        /// 
        ///</summary>
        /// <param name="CMACH">
        /// (input) CHARACTER*1
        /// Specifies the value to be returned by DLAMCH:
        /// = 'E' or 'e',   DLAMCH := eps
        /// = 'S' or 's ,   DLAMCH := sfmin
        /// = 'B' or 'b',   DLAMCH := base
        /// = 'P' or 'p',   DLAMCH := eps*base
        /// = 'N' or 'n',   DLAMCH := t
        /// = 'R' or 'r',   DLAMCH := rnd
        /// = 'M' or 'm',   DLAMCH := emin
        /// = 'U' or 'u',   DLAMCH := rmin
        /// = 'L' or 'l',   DLAMCH := emax
        /// = 'O' or 'o',   DLAMCH := rmax
        /// 
        /// where
        /// 
        /// eps   = relative machine precision
        /// sfmin = safe minimum, such that 1/sfmin does not overflow
        /// base  = base of the machine
        /// prec  = eps*base
        /// t     = number of (base) digits in the mantissa
        /// rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise
        /// emin  = minimum exponent before (gradual) underflow
        /// rmin  = underflow threshold - base**(emin-1)
        /// emax  = largest exponent before overflow
        /// rmax  = overflow threshold  - (base**emax)*(1-eps)
        ///</param>
        public double Run(string CMACH)
        {
        double dlamch = 0;
            #region Variables
            
            bool LRND = false; int BETA = 0; int IMAX = 0; int IMIN = 0; int IT = 0; double RMACH = 0; double SMALL = 0; 
            #endregion
            #region Strings
            
            CMACH = CMACH.Substring(0, 1);  
            #endregion
            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     November 2006
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLAMCH determines double precision machine parameters.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  CMACH   (input) CHARACTER*1
            // *          Specifies the value to be returned by DLAMCH:
            // *          = 'E' or 'e',   DLAMCH := eps
            // *          = 'S' or 's ,   DLAMCH := sfmin
            // *          = 'B' or 'b',   DLAMCH := base
            // *          = 'P' or 'p',   DLAMCH := eps*base
            // *          = 'N' or 'n',   DLAMCH := t
            // *          = 'R' or 'r',   DLAMCH := rnd
            // *          = 'M' or 'm',   DLAMCH := emin
            // *          = 'U' or 'u',   DLAMCH := rmin
            // *          = 'L' or 'l',   DLAMCH := emax
            // *          = 'O' or 'o',   DLAMCH := rmax
            // *
            // *          where
            // *
            // *          eps   = relative machine precision
            // *          sfmin = safe minimum, such that 1/sfmin does not overflow
            // *          base  = base of the machine
            // *          prec  = eps*base
            // *          t     = number of (base) digits in the mantissa
            // *          rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise
            // *          emin  = minimum exponent before (gradual) underflow
            // *          rmin  = underflow threshold - base**(emin-1)
            // *          emax  = largest exponent before overflow
            // *          rmax  = overflow threshold  - (base**emax)*(1-eps)
            // *
            // * =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Save statement ..
            // *     ..
            // *     .. Data statements ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            #endregion
            #region Body
            if (FIRST)
            {
                this._dlamc2.Run(ref BETA, ref IT, ref LRND, ref EPS, ref IMIN, ref RMIN
                                 , ref IMAX, ref RMAX);
                BASE = BETA;
                T = IT;
                if (LRND)
                {
                    RND = ONE;
                    EPS = (Math.Pow(BASE,1 - IT)) / 2;
                }
                else
                {
                    RND = ZERO;
                    EPS = Math.Pow(BASE,1 - IT);
                }
                PREC = EPS * BASE;
                EMIN = IMIN;
                EMAX = IMAX;
                SFMIN = RMIN;
                SMALL = ONE / RMAX;
                if (SMALL >= SFMIN)
                {
                    // *
                    // *           Use SMALL plus a bit, to avoid the possibility of rounding
                    // *           causing overflow when computing  1/sfmin.
                    // *
                    SFMIN = SMALL * (ONE + EPS);
                }
            }
            // *
            if (this._lsame.Run(CMACH, "E"))
            {
                RMACH = EPS;
            }
            else
            {
                if (this._lsame.Run(CMACH, "S"))
                {
                    RMACH = SFMIN;
                }
                else
                {
                    if (this._lsame.Run(CMACH, "B"))
                    {
                        RMACH = BASE;
                    }
                    else
                    {
                        if (this._lsame.Run(CMACH, "P"))
                        {
                            RMACH = PREC;
                        }
                        else
                        {
                            if (this._lsame.Run(CMACH, "N"))
                            {
                                RMACH = T;
                            }
                            else
                            {
                                if (this._lsame.Run(CMACH, "R"))
                                {
                                    RMACH = RND;
                                }
                                else
                                {
                                    if (this._lsame.Run(CMACH, "M"))
                                    {
                                        RMACH = EMIN;
                                    }
                                    else
                                    {
                                        if (this._lsame.Run(CMACH, "U"))
                                        {
                                            RMACH = RMIN;
                                        }
                                        else
                                        {
                                            if (this._lsame.Run(CMACH, "L"))
                                            {
                                                RMACH = EMAX;
                                            }
                                            else
                                            {
                                                if (this._lsame.Run(CMACH, "O"))
                                                {
                                                    RMACH = RMAX;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            // *
            dlamch = RMACH;
            FIRST = false;
            return dlamch;
            // *
            // *     End of DLAMCH
            // *
            #endregion
        }
    }

    #endregion


    #region The Class: DLAMC1
    
    // *
    // ************************************************************************
    // *
    /// <summary>
    /// -- LAPACK auxiliary routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DLAMC1 determines the machine parameters given by BETA, T, RND, and
    /// IEEE1.
    /// 
    ///</summary>
    public class DLAMC1
    {
    
        #region Dependencies
        
        DLAMC3 _dlamc3; 
        #endregion
        #region Variables
        
        bool FIRST = false; bool LIEEE1 = false; bool LRND = false; int LBETA = 0; int LT = 0; 
        #endregion
        public DLAMC1(DLAMC3 dlamc3)
        {
    
            #region Set Dependencies
            
            this._dlamc3 = dlamc3; 
            #endregion
            #region Data Initialization
            
            //FIRST/.TRUE.
            FIRST = true;
            #endregion
        }
    
        public DLAMC1()
        {
    
            #region Dependencies (Initialization)
            
            DLAMC3 dlamc3 = new DLAMC3();
            #endregion
            #region Set Dependencies
            
            this._dlamc3 = dlamc3; 
            #endregion
            #region Data Initialization
            
            //FIRST/.TRUE.
            FIRST = true;
            #endregion
        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAMC1 determines the machine parameters given by BETA, T, RND, and
        /// IEEE1.
        /// 
        ///</summary>
        /// <param name="BETA">
        /// (output) INTEGER
        /// The base of the machine.
        ///</param>
        /// <param name="T">
        /// (output) INTEGER
        /// The number of ( BETA ) digits in the mantissa.
        ///</param>
        /// <param name="RND">
        /// (output) LOGICAL
        /// Specifies whether proper rounding  ( RND = .TRUE. )  or
        /// chopping  ( RND = .FALSE. )  occurs in addition. This may not
        /// be a reliable guide to the way in which the machine performs
        /// its arithmetic.
        ///</param>
        /// <param name="IEEE1">
        /// (output) LOGICAL
        /// Specifies whether rounding appears to be done in the IEEE
        /// 'round to nearest' style.
        ///</param>
        public void Run(ref int BETA, ref int T, ref bool RND, ref bool IEEE1)
        {
            #region Variables
            
            double A = 0; double B = 0; double C = 0; double F = 0; double ONE = 0; double QTR = 0; double SAVEC = 0; 
            double T1 = 0;double T2 = 0; 
            #endregion
            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     November 2006
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLAMC1 determines the machine parameters given by BETA, T, RND, and
            // *  IEEE1.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  BETA    (output) INTEGER
            // *          The base of the machine.
            // *
            // *  T       (output) INTEGER
            // *          The number of ( BETA ) digits in the mantissa.
            // *
            // *  RND     (output) LOGICAL
            // *          Specifies whether proper rounding  ( RND = .TRUE. )  or
            // *          chopping  ( RND = .FALSE. )  occurs in addition. This may not
            // *          be a reliable guide to the way in which the machine performs
            // *          its arithmetic.
            // *
            // *  IEEE1   (output) LOGICAL
            // *          Specifies whether rounding appears to be done in the IEEE
            // *          'round to nearest' style.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  The routine is based on the routine  ENVRON  by Malcolm and
            // *  incorporates suggestions by Gentleman and Marovich. See
            // *
            // *     Malcolm M. A. (1972) Algorithms to reveal properties of
            // *        floating-point arithmetic. Comms. of the ACM, 15, 949-951.
            // *
            // *     Gentleman W. M. and Marovich S. B. (1974) More on algorithms
            // *        that reveal properties of floating point arithmetic units.
            // *        Comms. of the ACM, 17, 276-277.
            // *
            // * =====================================================================
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Save statement ..
            // *     ..
            // *     .. Data statements ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            #endregion
            #region Body
            
            if (FIRST)
            {
                ONE = 1;
                // *
                // *        LBETA,  LIEEE1,  LT and  LRND  are the  local values  of  BETA,
                // *        IEEE1, T and RND.
                // *
                // *        Throughout this routine  we use the function  DLAMC3  to ensure
                // *        that relevant values are  stored and not held in registers,  or
                // *        are not affected by optimizers.
                // *
                // *        Compute  a = 2.0**m  with the  smallest positive integer m such
                // *        that
                // *
                // *           fl( a + 1.0 ) = a.
                // *
                A = 1;
                C = 1;
                // *
                // *+       WHILE( C.EQ.ONE )LOOP
            LABEL10:;
                if (C == ONE)
                {
                    A *= 2;
                    C = (double)this._dlamc3.Run(A, ONE);
                    C = (double)this._dlamc3.Run(C, -A);
                    goto LABEL10;
                }
                // *+       END WHILE
                // *
                // *        Now compute  b = 2.0**m  with the smallest positive integer m
                // *        such that
                // *
                // *           fl( a + b ) .gt. a.
                // *
                B = 1;
                C = this._dlamc3.Run(A, B);
                // *
                // *+       WHILE( C.EQ.A )LOOP
            LABEL20:;
                if (C == A)
                {
                    B *= 2;
                    C = (double)this._dlamc3.Run(A, B);
                    goto LABEL20;
                }
                // *+       END WHILE
                // *
                // *        Now compute the base.  a and c  are neighbouring floating point
                // *        numbers  in the  interval  ( beta**t, beta**( t + 1 ) )  and so
                // *        their difference is beta. Adding 0.25 to c is to ensure that it
                // *        is truncated to beta and not ( beta - 1 ).
                // *
                QTR = ONE / 4;
                SAVEC = C;
                C = (double)this._dlamc3.Run(C, -A);
                LBETA = (int)(C + QTR);
                // *
                // *        Now determine whether rounding or chopping occurs,  by adding a
                // *        bit  less  than  beta/2  and a  bit  more  than  beta/2  to  a.
                // *
                B = LBETA;
                F = (double)this._dlamc3.Run(B / 2, -B / 100);
                C = (double)this._dlamc3.Run(F, A);
                if (C == A)
                {
                    LRND = true;
                }
                else
                {
                    LRND = false;
                }
                F = (double)this._dlamc3.Run(B / 2, B / 100);
                C = (double)this._dlamc3.Run(F, A);
                if ((LRND) && (C == A)) LRND = false;
                // *
                // *        Try and decide whether rounding is done in the  IEEE  'round to
                // *        nearest' style. B/2 is half a unit in the last place of the two
                // *        numbers A and SAVEC. Furthermore, A is even, i.e. has last  bit
                // *        zero, and SAVEC is odd. Thus adding B/2 to A should not  change
                // *        A, but adding B/2 to SAVEC should change SAVEC.
                // *
                T1 = (double)this._dlamc3.Run(B / 2, A);
                T2 = (double)this._dlamc3.Run(B / 2, SAVEC);
                LIEEE1 = (T1 == A) && (T2 > SAVEC) && LRND;
                // *
                // *        Now find  the  mantissa, t.  It should  be the  integer part of
                // *        log to the base beta of a,  however it is safer to determine  t
                // *        by powering.  So we find t as the smallest positive integer for
                // *        which
                // *
                // *           fl( beta**t + 1.0 ) = 1.0.
                // *
                LT = 0;
                A = 1;
                C = 1;
                // *
                // *+       WHILE( C.EQ.ONE )LOOP
            LABEL30:;
                if (C == ONE)
                {
                    LT += 1;
                    A *= LBETA;
                    C = (double)this._dlamc3.Run(A, ONE);
                    C = (double)this._dlamc3.Run(C, -A);
                    goto LABEL30;
                }
                // *+       END WHILE
                // *
            }
            // *
            BETA = LBETA;
            T = LT;
            RND = LRND;
            IEEE1 = LIEEE1;
            FIRST = false;
            return;
            // *
            // *     End of DLAMC1
            // *
            #endregion
        }
    }

    #endregion


    #region The Class: DLAMC2
    
    // *
    // ************************************************************************
    // *
    /// <summary>
    /// -- LAPACK auxiliary routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DLAMC2 determines the machine parameters specified in its argument
    /// list.
    /// 
    ///</summary>
    public class DLAMC2
    {
    
        #region Dependencies
        
        DLAMC3 _dlamc3; DLAMC1 _dlamc1; DLAMC4 _dlamc4; DLAMC5 _dlamc5; 
        #endregion
        #region Variables
        
        bool FIRST = false; bool IWARN = false; int LBETA = 0; int LEMAX = 0; int LEMIN = 0; int LT = 0; double LEPS = 0; 
        double LRMAX = 0;double LRMIN = 0; 
        #endregion
        public DLAMC2(DLAMC3 dlamc3, DLAMC1 dlamc1, DLAMC4 dlamc4, DLAMC5 dlamc5)
        {
    
            #region Set Dependencies
            
            this._dlamc3 = dlamc3; this._dlamc1 = dlamc1; this._dlamc4 = dlamc4; this._dlamc5 = dlamc5; 
            #endregion
            #region Data Initialization
            
            //FIRST/.TRUE.
            FIRST = true;
            //IWARN/.FALSE.
            IWARN = false;
            #endregion
        }
    
        public DLAMC2()
        {
    
            #region Dependencies (Initialization)
            
            DLAMC3 dlamc3 = new DLAMC3();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            #endregion
            #region Set Dependencies
            
            this._dlamc3 = dlamc3; this._dlamc1 = dlamc1; this._dlamc4 = dlamc4; this._dlamc5 = dlamc5; 
            #endregion
            #region Data Initialization
            
            //FIRST/.TRUE.
            FIRST = true;
            //IWARN/.FALSE.
            IWARN = false;
            #endregion
        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAMC2 determines the machine parameters specified in its argument
        /// list.
        /// 
        ///</summary>
        /// <param name="BETA">
        /// (output) INTEGER
        /// The base of the machine.
        ///</param>
        /// <param name="T">
        /// (output) INTEGER
        /// The number of ( BETA ) digits in the mantissa.
        ///</param>
        /// <param name="RND">
        /// (output) LOGICAL
        /// Specifies whether proper rounding  ( RND = .TRUE. )  or
        /// chopping  ( RND = .FALSE. )  occurs in addition. This may not
        /// be a reliable guide to the way in which the machine performs
        /// its arithmetic.
        ///</param>
        /// <param name="EPS">
        /// (output) DOUBLE PRECISION
        /// The smallest positive number such that
        /// 
        /// fl( 1.0 - EPS ) .LT. 1.0,
        /// 
        /// where fl denotes the computed value.
        ///</param>
        /// <param name="EMIN">
        /// (output) INTEGER
        /// The minimum exponent before (gradual) underflow occurs.
        ///</param>
        /// <param name="RMIN">
        /// (output) DOUBLE PRECISION
        /// The smallest normalized number for the machine, given by
        /// BASE**( EMIN - 1 ), where  BASE  is the floating point value
        /// of BETA.
        ///</param>
        /// <param name="EMAX">
        /// (output) INTEGER
        /// The maximum exponent before overflow occurs.
        ///</param>
        /// <param name="RMAX">
        /// (output) DOUBLE PRECISION
        /// The largest positive number for the machine, given by
        /// BASE**EMAX * ( 1 - EPS ), where  BASE  is the floating point
        /// value of BETA.
        ///</param>
        public void Run(ref int BETA, ref int T, ref bool RND, ref double EPS, ref int EMIN, ref double RMIN
                         , ref int EMAX, ref double RMAX)
        {
            #region Variables
            
            bool IEEE = false; bool LIEEE1 = false; bool LRND = false; int GNMIN = 0; int GPMIN = 0; int I = 0; int NGNMIN = 0; 
            int NGPMIN = 0;double A = 0; double B = 0; double C = 0; double HALF = 0; double ONE = 0; double RBASE = 0; 
            double SIXTH = 0;double SMALL = 0; double THIRD = 0; double TWO = 0; double ZERO = 0; 
            #endregion
            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     November 2006
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLAMC2 determines the machine parameters specified in its argument
            // *  list.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  BETA    (output) INTEGER
            // *          The base of the machine.
            // *
            // *  T       (output) INTEGER
            // *          The number of ( BETA ) digits in the mantissa.
            // *
            // *  RND     (output) LOGICAL
            // *          Specifies whether proper rounding  ( RND = .TRUE. )  or
            // *          chopping  ( RND = .FALSE. )  occurs in addition. This may not
            // *          be a reliable guide to the way in which the machine performs
            // *          its arithmetic.
            // *
            // *  EPS     (output) DOUBLE PRECISION
            // *          The smallest positive number such that
            // *
            // *             fl( 1.0 - EPS ) .LT. 1.0,
            // *
            // *          where fl denotes the computed value.
            // *
            // *  EMIN    (output) INTEGER
            // *          The minimum exponent before (gradual) underflow occurs.
            // *
            // *  RMIN    (output) DOUBLE PRECISION
            // *          The smallest normalized number for the machine, given by
            // *          BASE**( EMIN - 1 ), where  BASE  is the floating point value
            // *          of BETA.
            // *
            // *  EMAX    (output) INTEGER
            // *          The maximum exponent before overflow occurs.
            // *
            // *  RMAX    (output) DOUBLE PRECISION
            // *          The largest positive number for the machine, given by
            // *          BASE**EMAX * ( 1 - EPS ), where  BASE  is the floating point
            // *          value of BETA.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  The computation of  EPS  is based on a routine PARANOIA by
            // *  W. Kahan of the University of California at Berkeley.
            // *
            // * =====================================================================
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, MAX, MIN;
            // *     ..
            // *     .. Save statement ..
            // *     ..
            // *     .. Data statements ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            #endregion
            #region Body
            
            if (FIRST)
            {
                ZERO = 0;
                ONE = 1;
                TWO = 2;
                // *
                // *        LBETA, LT, LRND, LEPS, LEMIN and LRMIN  are the local values of
                // *        BETA, T, RND, EPS, EMIN and RMIN.
                // *
                // *        Throughout this routine  we use the function  DLAMC3  to ensure
                // *        that relevant values are stored  and not held in registers,  or
                // *        are not affected by optimizers.
                // *
                // *        DLAMC1 returns the parameters  LBETA, LT, LRND and LIEEE1.
                // *
                this._dlamc1.Run(ref LBETA, ref LT, ref LRND, ref LIEEE1);
                // *
                // *        Start to find EPS.
                // *
                B = LBETA;
                A = Math.Pow(B, - LT);
                LEPS = A;
                // *
                // *        Try some tricks to see whether or not this is the correct  EPS.
                // *
                B = TWO / 3;
                HALF = ONE / 2;
                SIXTH = this._dlamc3.Run(B,  - HALF);
                THIRD = this._dlamc3.Run(SIXTH, SIXTH);
                B = (double)this._dlamc3.Run(THIRD,  - HALF);
                B = (double)this._dlamc3.Run(B, SIXTH);
                B = Math.Abs(B);
                if (B < LEPS) B = LEPS;
                // *
                LEPS = 1;
                // *
                // *+       WHILE( ( LEPS.GT.B ).AND.( B.GT.ZERO ) )LOOP
            LABEL10:;
                if ((LEPS > B) && (B > ZERO))
                {
                    LEPS = B;
                    C = (double)this._dlamc3.Run(HALF * LEPS, (Math.Pow(TWO,5)) * (Math.Pow(LEPS,2)));
                    C = (double)this._dlamc3.Run(HALF, -C);
                    B = (double)this._dlamc3.Run(HALF, C);
                    C = (double)this._dlamc3.Run(HALF, -B);
                    B = (double)this._dlamc3.Run(HALF, C);
                    goto LABEL10;
                }
                // *+       END WHILE
                // *
                if (A < LEPS) LEPS = A;
                // *
                // *        Computation of EPS complete.
                // *
                // *        Now find  EMIN.  Let A = + or - 1, and + or - (1 + BASE**(-3)).
                // *        Keep dividing  A by BETA until (gradual) underflow occurs. This
                // *        is detected when we cannot recover the previous A.
                // *
                RBASE = ONE / LBETA;
                SMALL = ONE;
                for (I = 1; I <= 3; I++)
                {
                    SMALL = (double)this._dlamc3.Run(SMALL * RBASE, ZERO);
                }
                A = (double)this._dlamc3.Run(ONE, SMALL);
                this._dlamc4.Run(ref NGPMIN, ONE, LBETA);
                this._dlamc4.Run(ref NGNMIN,  - ONE, LBETA);
                this._dlamc4.Run(ref GPMIN, A, LBETA);
                this._dlamc4.Run(ref GNMIN,  - A, LBETA);
                IEEE = false;
                // *
                if ((NGPMIN == NGNMIN) && (GPMIN == GNMIN))
                {
                    if (NGPMIN == GPMIN)
                    {
                        LEMIN = NGPMIN;
                        // *            ( Non twos-complement machines, no gradual underflow;
                        // *              e.g.,  VAX )
                    }
                    else
                    {
                        if ((GPMIN - NGPMIN) == 3)
                        {
                            LEMIN = NGPMIN - 1 + LT;
                            IEEE = true;
                            // *            ( Non twos-complement machines, with gradual underflow;
                            // *              e.g., IEEE standard followers )
                        }
                        else
                        {
                            LEMIN = Math.Min(NGPMIN, GPMIN);
                            // *            ( A guess; no known machine )
                            IWARN = true;
                        }
                    }
                    // *
                }
                else
                {
                    if ((NGPMIN == GPMIN) && (NGNMIN == GNMIN))
                    {
                        if (Math.Abs(NGPMIN - NGNMIN) == 1)
                        {
                            LEMIN = Math.Max(NGPMIN, NGNMIN);
                            // *            ( Twos-complement machines, no gradual underflow;
                            // *              e.g., CYBER 205 )
                        }
                        else
                        {
                            LEMIN = Math.Min(NGPMIN, NGNMIN);
                            // *            ( A guess; no known machine )
                            IWARN = true;
                        }
                        // *
                    }
                    else
                    {
                        if ((Math.Abs(NGPMIN - NGNMIN) == 1) && (GPMIN == GNMIN))
                        {
                            if ((GPMIN - Math.Min(NGPMIN, NGNMIN)) == 3)
                            {
                                LEMIN = Math.Max(NGPMIN, NGNMIN) - 1 + LT;
                                // *            ( Twos-complement machines with gradual underflow;
                                // *              no known machine )
                            }
                            else
                            {
                                LEMIN = Math.Min(NGPMIN, NGNMIN);
                                // *            ( A guess; no known machine )
                                IWARN = true;
                            }
                            // *
                        }
                        else
                        {
                            LEMIN = Math.Min(NGPMIN, Math.Min(NGNMIN, Math.Min(GPMIN, GNMIN)));
                            // *         ( A guess; no known machine )
                            IWARN = true;
                        }
                    }
                }
                FIRST = false;
                // ***
                // * Comment out this if block if EMIN is ok
                if (IWARN)
                {
                    FIRST = true;
                    //ERROR-ERROR            WRITE( 6, FMT = 9999 )LEMIN;
                }
                // ***
                // *
                // *        Assume IEEE arithmetic if we found denormalised  numbers above,
                // *        or if arithmetic seems to round in the  IEEE style,  determined
                // *        in routine DLAMC1. A true IEEE machine should have both  things
                // *        true; however, faulty machines may have one or the other.
                // *
                IEEE = IEEE || LIEEE1;
                // *
                // *        Compute  RMIN by successive division by  BETA. We could compute
                // *        RMIN as BASE**( EMIN - 1 ),  but some machines underflow during
                // *        this computation.
                // *
                LRMIN = 1;
                for (I = 1; I <= 1 - LEMIN; I++)
                {
                    LRMIN = this._dlamc3.Run(LRMIN * RBASE, ZERO);
                }
                // *
                // *        Finally, call DLAMC5 to compute EMAX and RMAX.
                // *
                this._dlamc5.Run(LBETA, LT, LEMIN, IEEE, ref LEMAX, ref LRMAX);
            }
            // *
            BETA = LBETA;
            T = LT;
            RND = LRND;
            EPS = LEPS;
            EMIN = LEMIN;
            RMIN = LRMIN;
            EMAX = LEMAX;
            RMAX = LRMAX;
            // *
            return;
            // *
            // *
            // *     End of DLAMC2
            // *
            #endregion
        }
    }

    #endregion


    #region The Class: DLAMC3
    
    // *
    // ************************************************************************
    // *
    /// <summary>
    /// -- LAPACK auxiliary routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DLAMC3  is intended to force  A  and  B  to be stored prior to doing
    /// the addition of  A  and  B ,  for use in situations where optimizers
    /// might hold one of these in a register.
    /// 
    ///</summary>
    public class DLAMC3
    {
    
        public DLAMC3()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAMC3  is intended to force  A  and  B  to be stored prior to doing
        /// the addition of  A  and  B ,  for use in situations where optimizers
        /// might hold one of these in a register.
        /// 
        ///</summary>
        /// <param name="A">
        /// (input) DOUBLE PRECISION
        ///</param>
        /// <param name="B">
        /// (input) DOUBLE PRECISION
        /// The values A and B.
        ///</param>
        public double Run(double A, double B)
        {
        double dlamc3 = 0;
            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     November 2006
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLAMC3  is intended to force  A  and  B  to be stored prior to doing
            // *  the addition of  A  and  B ,  for use in situations where optimizers
            // *  might hold one of these in a register.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  A       (input) DOUBLE PRECISION
            // *  B       (input) DOUBLE PRECISION
            // *          The values A and B.
            // *
            // * =====================================================================
            // *
            // *     .. Executable Statements ..
            // *
            #endregion
            dlamc3 = A + B;
            // *
            return dlamc3;
            // *
            // *     End of DLAMC3
            // *
        }
    }

    #endregion


    #region The Class: DLAMC4
    
    // *
    // ************************************************************************
    // *
    /// <summary>
    /// -- LAPACK auxiliary routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DLAMC4 is a service routine for DLAMC2.
    /// 
    ///</summary>
    public class DLAMC4
    {
    
        #region Dependencies
        
        DLAMC3 _dlamc3; 
        #endregion
        public DLAMC4(DLAMC3 dlamc3)
        {
    
            #region Set Dependencies
            
            this._dlamc3 = dlamc3; 
            #endregion
        }
    
        public DLAMC4()
        {
    
            #region Dependencies (Initialization)
            
            DLAMC3 dlamc3 = new DLAMC3();
            #endregion
            #region Set Dependencies
            
            this._dlamc3 = dlamc3; 
            #endregion
        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAMC4 is a service routine for DLAMC2.
        /// 
        ///</summary>
        /// <param name="EMIN">
        /// (output) INTEGER 
        /// The minimum exponent before (gradual) underflow, computed by
        /// setting A = START and dividing by BASE until the previous A
        /// can not be recovered.
        ///</param>
        /// <param name="START">
        /// (input) DOUBLE PRECISION
        /// The starting point for determining EMIN.
        ///</param>
        /// <param name="BASE">
        /// (input) INTEGER
        /// The base of the machine.
        ///</param>
        public void Run(ref int EMIN, double START, int BASE)
        {
            #region Variables
            
            int I = 0; double A = 0; double B1 = 0; double B2 = 0; double C1 = 0; double C2 = 0; double D1 = 0; double D2 = 0; 
            double ONE = 0;double RBASE = 0; double ZERO = 0; 
            #endregion
            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     November 2006
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLAMC4 is a service routine for DLAMC2.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  EMIN    (output) INTEGER 
            // *          The minimum exponent before (gradual) underflow, computed by
            // *          setting A = START and dividing by BASE until the previous A
            // *          can not be recovered.
            // *
            // *  START   (input) DOUBLE PRECISION
            // *          The starting point for determining EMIN.
            // *
            // *  BASE    (input) INTEGER
            // *          The base of the machine.
            // *
            // * =====================================================================
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            #endregion
            #region Body
            
            A = START;
            ONE = 1;
            RBASE = ONE / BASE;
            ZERO = 0;
            EMIN = 1;
            B1 = (double)this._dlamc3.Run(A * RBASE, ZERO);
            C1 = A;
            C2 = A;
            D1 = A;
            D2 = A;
            // *+    WHILE( ( C1.EQ.A ).AND.( C2.EQ.A ).AND.
            // *    $       ( D1.EQ.A ).AND.( D2.EQ.A )      )LOOP
        LABEL10:;
            if ((C1 == A) && (C2 == A) && (D1 == A) && (D2 == A))
            {
                EMIN -= 1;
                A = B1;
                B1 = (double)this._dlamc3.Run(A / BASE, ZERO);
                C1 = (double)this._dlamc3.Run(B1 * BASE, ZERO);
                D1 = ZERO;
                for (I = 1; I <= BASE; I++)
                {
                    D1 += B1;
                }
                B2 = (double)this._dlamc3.Run(A * RBASE, ZERO);
                C2 = (double)this._dlamc3.Run(B2 / RBASE, ZERO);
                D2 = ZERO;
                for (I = 1; I <= BASE; I++)
                {
                    D2 += B2;
                }
                goto LABEL10;
            }
            // *+    END WHILE
            // *
            return;
            // *
            // *     End of DLAMC4
            // *
            #endregion
        }
    }

    #endregion


    #region The Class: DLAMC5
    
    // *
    // ************************************************************************
    // *
    /// <summary>
    /// -- LAPACK auxiliary routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DLAMC5 attempts to compute RMAX, the largest machine floating-point
    /// number, without overflow.  It assumes that EMAX + abs(EMIN) sum
    /// approximately to a power of 2.  It will fail on machines where this
    /// assumption does not hold, for example, the Cyber 205 (EMIN = -28625,
    /// EMAX = 28718).  It will also fail if the value supplied for EMIN is
    /// too large (i.e. too close to zero), probably with overflow.
    /// 
    ///</summary>
    public class DLAMC5
    {
    
        #region Dependencies
        
        DLAMC3 _dlamc3; 
        #endregion
        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; 
        #endregion
        public DLAMC5(DLAMC3 dlamc3)
        {
    
            #region Set Dependencies
            
            this._dlamc3 = dlamc3; 
            #endregion
        }
    
        public DLAMC5()
        {
    
            #region Dependencies (Initialization)
            
            DLAMC3 dlamc3 = new DLAMC3();
            #endregion
            #region Set Dependencies
            
            this._dlamc3 = dlamc3; 
            #endregion
        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAMC5 attempts to compute RMAX, the largest machine floating-point
        /// number, without overflow.  It assumes that EMAX + abs(EMIN) sum
        /// approximately to a power of 2.  It will fail on machines where this
        /// assumption does not hold, for example, the Cyber 205 (EMIN = -28625,
        /// EMAX = 28718).  It will also fail if the value supplied for EMIN is
        /// too large (i.e. too close to zero), probably with overflow.
        /// 
        ///</summary>
        /// <param name="BETA">
        /// (input) INTEGER
        /// The base of floating-point arithmetic.
        ///</param>
        /// <param name="P">
        /// (input) INTEGER
        /// The number of base BETA digits in the mantissa of a
        /// floating-point value.
        ///</param>
        /// <param name="EMIN">
        /// (input) INTEGER
        /// The minimum exponent before (gradual) underflow.
        ///</param>
        /// <param name="IEEE">
        /// (input) LOGICAL
        /// A logical flag specifying whether or not the arithmetic
        /// system is thought to comply with the IEEE standard.
        ///</param>
        /// <param name="EMAX">
        /// (output) INTEGER
        /// The largest exponent before overflow
        ///</param>
        /// <param name="RMAX">
        /// (output) DOUBLE PRECISION
        /// The largest machine floating-point number.
        ///</param>
        public void Run(int BETA, int P, int EMIN, bool IEEE, ref int EMAX, ref double RMAX)
        {
            #region Variables
            
            int EXBITS = 0; int EXPSUM = 0; int I = 0; int LEXP = 0; int NBITS = 0; int TRY = 0; int UEXP = 0; double OLDY = 0; 
            double RECBAS = 0;double Y = 0; double Z = 0; 
            #endregion
            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     November 2006
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLAMC5 attempts to compute RMAX, the largest machine floating-point
            // *  number, without overflow.  It assumes that EMAX + abs(EMIN) sum
            // *  approximately to a power of 2.  It will fail on machines where this
            // *  assumption does not hold, for example, the Cyber 205 (EMIN = -28625,
            // *  EMAX = 28718).  It will also fail if the value supplied for EMIN is
            // *  too large (i.e. too close to zero), probably with overflow.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  BETA    (input) INTEGER
            // *          The base of floating-point arithmetic.
            // *
            // *  P       (input) INTEGER
            // *          The number of base BETA digits in the mantissa of a
            // *          floating-point value.
            // *
            // *  EMIN    (input) INTEGER
            // *          The minimum exponent before (gradual) underflow.
            // *
            // *  IEEE    (input) LOGICAL
            // *          A logical flag specifying whether or not the arithmetic
            // *          system is thought to comply with the IEEE standard.
            // *
            // *  EMAX    (output) INTEGER
            // *          The largest exponent before overflow
            // *
            // *  RMAX    (output) DOUBLE PRECISION
            // *          The largest machine floating-point number.
            // *
            // * =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MOD;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     First compute LEXP and UEXP, two powers of 2 that bound
            // *     abs(EMIN). We then assume that EMAX + abs(EMIN) will sum
            // *     approximately to the bound that is closest to abs(EMIN).
            // *     (EMAX is the exponent of the required number RMAX).
            // *
            #endregion
            #region Body
            
            LEXP = 1;
            EXBITS = 1;
        LABEL10:;
            TRY = LEXP * 2;
            if (TRY <= ( - EMIN))
            {
                LEXP = TRY;
                EXBITS += 1;
                goto LABEL10;
            }
            if (LEXP ==  - EMIN)
            {
                UEXP = LEXP;
            }
            else
            {
                UEXP = TRY;
                EXBITS += 1;
            }
            // *
            // *     Now -LEXP is less than or equal to EMIN, and -UEXP is greater
            // *     than or equal to EMIN. EXBITS is the number of bits needed to
            // *     store the exponent.
            // *
            if ((UEXP + EMIN) > ( - LEXP - EMIN))
            {
                EXPSUM = 2 * LEXP;
            }
            else
            {
                EXPSUM = 2 * UEXP;
            }
            // *
            // *     EXPSUM is the exponent range, approximately equal to
            // *     EMAX - EMIN + 1 .
            // *
            EMAX = EXPSUM + EMIN - 1;
            NBITS = 1 + EXBITS + P;
            // *
            // *     NBITS is the total number of bits needed to store a
            // *     floating-point number.
            // *
            if ((FortranLib.Mod(NBITS,2) == 1) && (BETA == 2))
            {
                // *
                // *        Either there are an odd number of bits used to store a
                // *        floating-point number, which is unlikely, or some bits are
                // *        not used in the representation of numbers, which is possible,
                // *        (e.g. Cray machines) or the mantissa has an implicit bit,
                // *        (e.g. IEEE machines, Dec Vax machines), which is perhaps the
                // *        most likely. We have to assume the last alternative.
                // *        If this is true, then we need to reduce EMAX by one because
                // *        there must be some way of representing zero in an implicit-bit
                // *        system. On machines like Cray, we are reducing EMAX by one
                // *        unnecessarily.
                // *
                EMAX -= 1;
            }
            // *
            if (IEEE)
            {
                // *
                // *        Assume we are on an IEEE machine which reserves one exponent
                // *        for infinity and NaN.
                // *
                EMAX -= 1;
            }
            // *
            // *     Now create RMAX, the largest machine number, which should
            // *     be equal to (1.0 - BETA**(-P)) * BETA**EMAX .
            // *
            // *     First compute 1.0 - BETA**(-P), being careful that the
            // *     result is less than 1.0 .
            // *
            RECBAS = ONE / BETA;
            Z = BETA - ONE;
            Y = ZERO;
            for (I = 1; I <= P; I++)
            {
                Z *= RECBAS;
                if (Y < ONE) OLDY = Y;
                Y = this._dlamc3.Run(Y, Z);
            }
            if (Y >= ONE) Y = OLDY;
            // *
            // *     Now multiply by BETA**EMAX to get RMAX.
            // *
            for (I = 1; I <= EMAX; I++)
            {
                Y = this._dlamc3.Run(Y * BETA, ZERO);
            }
            // *
            RMAX = Y;
            return;
            // *
            // *     End of DLAMC5
            // *
            #endregion
        }
    }

    #endregion

}
