//An Implementation of IAPWS-IF97 Steam Tables

//The International Association for the Properties of Water and Steam Properties published IAPWS-IF97.
//The formulation can be obtained from http://www.iapws.org or from ASME publication, ASME
//International Steam Tables for Industrial Use, 2nd Edition.

//There are several software based on IAPWS formulation (follow the links from, http://www.iapws.org ).
//This is yet another implementation not affiliated with IAPWS.

//Contact: steamtables @nkurul.com

//Copyright(C) 2011, Necdet Kurul
//Permission is hereby granted, free of charge, to any person obtaining a copy of this software
//and associated documentation files (the "Software"), to deal in the Software without restriction,
//including without limitation the rights to use, copy, modify, merge, publish, distribute, 
//sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is 
//furnished to do so, subject to the following conditions:

//The above copyright notice and this permission notice shall be included in all copies or 
//substantial portions of the Software.

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING 
//BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
//NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, 
//DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
//OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SteamProperties
{
    public class REGION3
    {
        public struct STATE
        {
            public double pkPa; // pressure in kPa
            public double tK;   // temperature in K
            public double rho;  // density in kg/m3

            public double p;   // reduced pressure (p/pRef)
            public double t;   // temperature (tStar / t)
            public double r;   // reduced density (rho / rStar)

            public int phase;
            public string subRegion;  // one of 3a, 3b, 3c, ...
                                      // this should be blank except while calculating Rho(p,t)

            public double Phi;
            public double Phi_r;
            public double Phi_t;
            public double Phi_rr;
            public double Phi_tt;
            public double Phi_rt;
        }

        public STATE state;
        public StmProp daddy;

        // region 3 constants

        double tStar = 647.096;      // K
        double rStar = 322.0;        // kg/m3
        double Rgas = 0.461526;      // kJ/kg-K

        double[] I = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                       1, 1, 1, 2, 2, 2, 2, 2, 2, 3,
                       3, 3, 3, 3, 4, 4, 4, 4, 5, 5,
                       5, 6, 6, 6, 7, 8, 9, 9,10,10,
                      11};

        double[] J = { 0, 0, 0, 1, 2, 7,10,12,23, 2,
                       6,15,17, 0, 2, 6, 7,22,26, 0,
                       2, 4,16,26, 0, 2, 4,26, 1, 3,
                      26, 0, 2,26, 2,26, 2,26, 0, 1,
                      26};

        double[] n = { 0.0                , 0.10658070028513e1 ,-0.15732845290239e2 , 0.20944396974307e2 ,
                      -0.76867707878716e1 , 0.26185947787954e1 ,-0.28080781148620e1 , 0.12053369696517e1 ,
                      -0.84566812812502e-2,-0.12654315477714e1 ,-0.11524407806681e1 , 0.88521043984318   ,
                      -0.64207765181607   , 0.38493460186671   ,-0.85214708824206   , 0.48972281541877e1 ,
                      -0.30502617256965e1 , 0.39420536879154e-1, 0.12558408424308   ,-0.27999329698710   ,
                       0.13899799569460e1 ,-0.20189915023570e1 ,-0.82147637173963e-2,-0.47596035734923   ,
                       0.43984074473500e-1,-0.44476435428739   , 0.90572070719733   , 0.70522450087967   ,
                       0.10770512626332   ,-0.32913623258954   ,-0.50871062041158   ,-0.22175400873096e-1,
                       0.94260751665092e-1, 0.16436278447961   ,-0.13503372241348e-1,-0.14834345352472e-1,
                       0.57922953628084e-3, 0.32308904703711e-2, 0.80964802996215e-4,-0.16557679795037e-3,
                      -0.44923899061815e-4 };


        // properties in Region 3

        public double Prt(double r, double t)
        {
            // r is density (kg/m3)
            // t is temperature (K)
            // returns pressure in kPa

            double p, tau, delta;

            delta = r / rStar;
            tau = tStar / t;

            CheckState(delta, tau);

            p = delta * Phi_r(delta, tau) * r * Rgas * t;
            state.pkPa = p;
            state.tK = t;
            state.rho = r;

            return p;
        }

        public double Rho(double p, double t, int phase)
        {
            // p is pressure in kPa
            // t is temperature in K
            // phase = 1 is liquid, =2 is steam =0 is unknown
            // returns density in kg/m3

            double v;

            //if (state.pkPa == p && state.tK == t && state.phase == phase)
            //{
            //    if (state.r != double.MinValue)
            //        return state.r;
            //}
            //else
            ClearState();


            // set the region

            SubRegion(p, t);

            // force the region if density of saturated water is asked for
            if (phase == 1)
            {
                if (state.subRegion == "3t")
                {
                    if (p < 19008.8)
                        state.subRegion = "3c";
                    else
                        state.subRegion = "3s";
                }
                else if (state.subRegion == "3r")
                    state.subRegion = "3s";
                else if (state.subRegion == "3x")
                    state.subRegion = "3u";
                else if (state.subRegion == "3z")
                    state.subRegion = "3y";
            }
            else if (phase == 2)
            {
                if (state.subRegion == "3c")
                    state.subRegion = "3t";
                else if (state.subRegion == "3s")
                {
                    if (p < 20500.0)
                        state.subRegion = "3t";
                    else
                        state.subRegion = "3r";
                }
                else if (state.subRegion == "3u")
                    state.subRegion = "3x";
                else if (state.subRegion == "3y")
                    state.subRegion = "3z";
            }

            switch (state.subRegion)
            {
                case "3a":
                    v = Vpt_3a(p, t);
                    break;
                case "3b":
                    v = Vpt_3b(p, t);
                    break;
                case "3c":
                    v = Vpt_3c(p, t);
                    break;
                case "3d":
                    v = Vpt_3d(p, t);
                    break;
                case "3e":
                    v = Vpt_3e(p, t);
                    break;
                case "3f":
                    v = Vpt_3f(p, t);
                    break;
                case "3g":
                    v = Vpt_3g(p, t);
                    break;
                case "3h":
                    v = Vpt_3h(p, t);
                    break;
                case "3i":
                    v = Vpt_3i(p, t);
                    break;
                case "3j":
                    v = Vpt_3j(p, t);
                    break;
                case "3k":
                    v = Vpt_3k(p, t);
                    break;
                case "3l":
                    v = Vpt_3l(p, t);
                    break;
                case "3m":
                    v = Vpt_3m(p, t);
                    break;
                case "3n":
                    v = Vpt_3n(p, t);
                    break;
                case "3o":
                    v = Vpt_3o(p, t);
                    break;
                case "3p":
                    v = Vpt_3p(p, t);
                    break;
                case "3q":
                    v = Vpt_3q(p, t);
                    break;
                case "3r":
                    v = Vpt_3r(p, t);
                    break;
                case "3s":
                    v = Vpt_3s(p, t);
                    break;
                case "3t":
                    v = Vpt_3t(p, t);
                    break;
                case "3u":
                    v = Vpt_3u(p, t);
                    break;
                case "3v":
                    v = Vpt_3v(p, t);
                    break;
                case "3w":
                    v = Vpt_3w(p, t);
                    break;
                case "3x":
                    v = Vpt_3x(p, t);
                    break;
                case "3y":
                    v = Vpt_3y(p, t);
                    break;
                case "3z":
                    v = Vpt_3z(p, t);
                    break;
                default:
                    v = double.MinValue;
                    break;
            }

            state.r = 1.0 / (v * rStar);
            state.pkPa = p;
            state.tK = t;
            state.phase = phase;
            return 1.0 / v;

        }

        public double V(double p, double t, int phase)
        {
            // p is pressure in kPa
            // t is temperature in K
            // phase = 1 is liquid, =2 is steam =0 is unknown
            // returns reduced specific volume, v*p/RT

            double r = 0.0;

            /*
            if (state.pkPa == p && state.tK == t && state.phase == phase)
            {
                if (state.rho != double.MinValue)
                    return 1.0 / state.rho * p / Rgas / t;
            }
             */

            r = Rho(p, t, phase);
            return 1.0 / r * p / Rgas / t;
        }

        public double H(double p, double t, int phase)
        {
            // p is pressure in kPa
            // t is temperature in K
            // phase = 1 is liquid, =2 is steam =0 is unknown
            // returns reduced specific enthalpy, h/RT

            double r = 0.0;
            /*
            if (state.pkPa == p && state.tK == t && state.phase == phase)
            {
                if (state.rho != double.MinValue)
                    r = state.rho;
            }
            */

            if (r == 0.0)
                r = Rho(p, t, phase);

            return Hrt(r, t);
        }

        public double S(double p, double t, int phase)
        {
            // p is pressure in kPa
            // t is temperature in K
            // phase = 1 is liquid, =2 is steam =0 is unknown
            // returns reduced specific entropy, s/R

            double r = 0.0;

            /*
            if (state.pkPa == p && state.tK == t && state.phase == phase)
            {
                if (state.rho != double.MinValue)
                    r = state.rho;
            }
             */

            if (r == 0.0)
                r = Rho(p, t, phase);

            return Srt(r, t);
        }

        public double CP(double p, double t, int phase)
        {
            // p is pressure in kPa
            // t is temperature in K
            // phase = 1 is liquid, =2 is steam =0 is unknown
            // returns reduced isobaric specific heat, cp/R

            double r = 0.0;

            if (state.pkPa == p && state.tK == t && state.phase == phase)
            {
                if (state.rho != double.MinValue)
                    r = state.rho;
            }

            if (r == 0.0)
                r = Rho(p, t, phase);

            return CPrt(r, t);
        }

        double Hrt(double r, double t)
        {
            // r is density (kg/m3)
            // t is temperature (K)
            // returns reduced specific enthalpy, h/RT

            double h, tau, delta;

            delta = r / rStar;
            tau = tStar / t;

            CheckState(delta, tau);

            h = tau * Phi_t(delta, tau) + delta * Phi_r(delta, tau);

            return h;
        }

        double Srt(double r, double t)
        {
            // r is density (kg/m3)
            // t is temperature (K)
            // returns reduced specific entropy, s/R

            double s, tau, delta;

            delta = r / rStar;
            tau = tStar / t;

            CheckState(delta, tau);

            s = tau * Phi_t(delta, tau) - Phi(delta, tau);

            return s;
        }

        double CPrt(double r, double t)
        {
            // r is density (kg/m3)
            // t is temperature (K)
            // returns reduced isobaric specific heat, cp/R

            double cp, tau, delta;

            delta = r / rStar;
            tau = tStar / t;

            CheckState(delta, tau);

            cp = -tau * tau * Phi_tt(delta, tau) +
                Math.Pow(delta * Phi_r(delta, tau) - delta * tau * Phi_rt(delta, tau), 2) /
                (2.0 * delta * Phi_r(delta, tau) + delta * delta * Phi_rr(delta, tau));

            return cp;
        }

        double CVrt(double r, double t)
        {
            // r is density (kg/m3)
            // t is temperature (K)
            // returns reduced ischoric specific heat, cv/R

            double cv, tau, delta;

            delta = r / rStar;
            tau = tStar / t;

            CheckState(delta, tau);

            cv = -tau * tau * Phi_tt(delta, tau);
            return cv;
        }


        // Helmholtz function and its derivatives

        double Phi(double r, double t)
        {
            if (state.Phi != double.MinValue)
                return state.Phi;

            double h = n[1] * Math.Log(r);
            for (int i = 2; i <= 40; i++)
                h += n[i] * Math.Pow(r, I[i]) * Math.Pow(t, J[i]);

            state.Phi = h;
            return h;
        }

        double Phi_r(double r, double t)
        {
            if (state.Phi_r != double.MinValue)
                return state.Phi_r;

            double h = n[1] / r;
            for (int i = 2; i <= 40; i++)
                h += n[i] * I[i] * Math.Pow(r, I[i] - 1) * Math.Pow(t, J[i]);

            state.Phi_r = h;
            return h;
        }

        double Phi_t(double r, double t)
        {
            if (state.Phi_t != double.MinValue)
                return state.Phi_t;

            double h = 0.0;
            for (int i = 2; i <= 40; i++)
                h += n[i] * J[i] * Math.Pow(r, I[i]) * Math.Pow(t, J[i] - 1);

            state.Phi_t = h;
            return h;
        }

        double Phi_rr(double r, double t)
        {
            if (state.Phi_rr != double.MinValue)
                return state.Phi_rr;

            double h = -n[1] / r / r;
            for (int i = 2; i <= 40; i++)
                h += n[i] * I[i] * (I[i] - 1) * Math.Pow(r, I[i] - 2) * Math.Pow(t, J[i]);

            state.Phi_rr = h;
            return h;
        }

        double Phi_tt(double r, double t)
        {
            if (state.Phi_tt != double.MinValue)
                return state.Phi_tt;

            double h = 0.0;
            for (int i = 2; i <= 40; i++)
                h += n[i] * J[i] * (J[i] - 1) * Math.Pow(r, I[i]) * Math.Pow(t, J[i] - 2);

            state.Phi_tt = h;
            return h;
        }

        double Phi_rt(double r, double t)
        {
            if (state.Phi_rt != double.MinValue)
                return state.Phi_rt;

            double h = 0.0;
            for (int i = 2; i <= 40; i++)
                h += n[i] * I[i] * J[i] * Math.Pow(r, I[i] - 1) * Math.Pow(t, J[i] - 1);

            state.Phi_rt = h;
            return h;
        }

        // Backward calculations for V(P,T)

        double Vpt_3a(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3a in m3/kg

            double v = 0.0;
            const double vStar = 0.0024;
            const int N = 30;
            const double a = 0.085;
            const double b = 0.817;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 1.0;

            p = Math.Pow(p / 100000.0 - a, c);
            t = Math.Pow(t / 760.0 - b, d);


            int[] I = {  0,-12,-12,-12,-10,-10,-10, -8, -8, -8,
                        -6, -5, -5, -5, -4, -3, -3, -3, -3, -2,
                        -2, -2, -1, -1, -1,  0,  0,  1,  1,  2,
                         2};

            int[] J = {  0, 5,10,12, 5,10,12, 5, 8,10,
                         1, 1, 5,10, 8, 0, 1, 3, 6, 0,
                         2, 3, 0, 1, 2, 0, 1, 0, 2, 0,
                         2 };
            double[] n = {  0.0                 , 0.110879558823853e-2, 0.572616740810616e3 ,-0.767051948380852e5 ,
                        -0.253321069529674e-1, 0.628008049345689e4 , 0.234105654131876e6 , 0.216867826045856   ,
                        -0.156237904341963e3 ,-0.269893956176613e5 ,-0.180407100085505e-3, 0.116732227668261e-2,
                         0.266987040856040e2 , 0.282776617243286e5 ,-0.242431520029523e4 , 0.435217323022733e-3,
                        -0.122494831387441e-1, 0.179357604019989e1 , 0.442729521058314e2 ,-0.593223489018342e-2,
                         0.453186261685774   , 0.135825703129140e1 , 0.408748415856745e-1, 0.474686397863312   ,
                         0.118646814997915e1 , 0.546987265727549   , 0.195266770452643   ,-0.502268790869663e-1,
                        -0.369645308193377   , 0.633828037528420e-2, 0.797441793901017e-1};

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3b(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3b in m3/kg

            double v = 0.0;
            const double vStar = 0.0041;
            const int N = 32;
            const double a = 0.280;
            const double b = 0.779;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 1.0;

            p = Math.Pow(p / 100000.0 - a, c);
            t = Math.Pow(t / 860.0 - b, d);


            int[] I = { 0,-12,-12,-10,-10, -8, -6, -6, -6, -5,
                       -5, -5, -4, -4, -4, -3, -3, -3, -3, -3,
                       -2, -2, -2, -1, -1,  0,  0,  1,  1,  2,
                        3,  4,  4 };

            int[] J = { 0,10,12, 8,14, 8, 5, 6, 8, 5,
                        8,10, 2, 4, 5, 0, 1, 2, 3, 5,
                        0, 2, 5, 0, 2, 0, 1, 0, 2, 0,
                        2, 0, 1  };
            double[] n = {  0.0                 ,-0.827670470003621e-1, 0.416887126010565e2 , 0.483651982197059e-1,
                       -0.291032084950276e5 ,-0.111422582236948e3 ,-0.202300083904014e-1, 0.294002509338515e3 ,
                        0.140244997609658e3 ,-0.344384158811459e3 , 0.361182452612149e3 ,-0.140699677420738e4 ,
                       -0.202023902676481e-2, 0.171346792457471e3 ,-0.425597804058632e1 , 0.691346085000334e-5,
                        0.151140509678925e-2,-0.416375290166236e-1,-0.413754957011042e2 ,-0.506673295721637e2 ,
                       -0.572212965569023e-3, 0.608817368401785e1 , 0.239600660256161e2 , 0.122261479925384e-1,
                        0.216356057692938e1 , 0.398198903368642   ,-0.116892827834085   ,-0.102845919373532   ,
                       -0.492676637589284   , 0.655540456406790e-1,-0.240462535078530   ,-0.269798180310075e-1,
                        0.128369435967012 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3c(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3c in m3/kg

            double v = 0.0;
            const double vStar = 0.0022;
            const int N = 35;
            const double a = 0.259;
            const double b = 0.903;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 1.0;

            p = Math.Pow(p / 40000.0 - a, c);
            t = Math.Pow(t / 690.0 - b, d);


            int[] I = {   0,-12,-12,-12,-10,-10,-10, -8, -8, -8,
                         -6, -5, -5, -5, -4, -4, -3, -3, -2, -2,
                         -2, -1, -1, -1,  0,  0,  0,  1,  1,  2,
                          2,  2,  2,  3,  3,  8   };

            int[] J = {  0, 6, 8,10, 6, 8,10, 5, 6, 7,
                         8, 1, 4, 7, 2, 8, 0, 3, 0, 4,
                         5, 0, 1, 2, 0, 1, 2, 0, 2, 0,
                         1, 3, 7, 0, 7, 1};

            double[] n = {  0.0                 , 0.311967788763030e1 , 0.276713458847564e5 , 0.322583103403269e8 ,
                         -0.342416065095363e3 ,-0.899732529907377e6 ,-0.793892049821251e8 , 0.953193003217388e2 ,
                          0.229784742345072e4 , 0.175336675322499e6 , 0.791214365222792e7 , 0.319933345844209e-4,
                         -0.659508863555767e2 ,-0.833426563212851e6 , 0.645734680583292e-1,-0.382031020570813e7 ,
                          0.406398848470079e-4, 0.310327498492008e2 ,-0.892996718483724e-3, 0.234604891591616e3 ,
                          0.377515668966951e4 , 0.158646812591361e-1, 0.707906336241843   , 0.126016225146570e2 ,
                          0.736143655772152   , 0.676544268999101   ,-0.178100588189137e2 ,-0.156531975531713   ,
                          0.117707430048158e2 , 0.840143653860447e-1,-0.186442467471949   ,-0.440170203949645e2 ,
                          0.123290423502494e7 ,-0.240650039730845e-1,-0.107077716660869e7 , 0.438319858566475e-1};

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3d(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3d in m3/kg

            double v = 0.0;
            const double vStar = 0.0029;
            const int N = 38;
            const double a = 0.559;
            const double b = 0.939;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 4.0;

            p = Math.Pow(p / 40000.0 - a, c);
            t = Math.Pow(t / 690.0 - b, d);


            int[] I = {   0,-12,-12,-12,-12,-12,-12,-10,-10,-10,
                        -10,-10,-10,-10, -8, -8, -8, -8, -6, -6,
                         -5, -5, -5, -5, -4, -4, -4, -3, -3, -2,
                         -2, -1, -1, -1,  0,  0,  1,  1,  3};

            int[] J = {  0, 4, 6, 7,10,12,16, 0, 2, 4,
                         6, 8,10,14, 3, 7, 8,10, 6, 8,
                         1, 2, 5, 7, 0, 1, 7, 2, 4, 0,
                         1, 0, 1, 5, 0, 2, 0, 6, 0};
            double[] n = {   0.0                  ,-0.452484847171645e-9 , 0.315210389538801e-4 ,-0.214991352047545e-2 ,
                          0.508058874808345e3  ,-0.127123036845932e8  , 0.115371133120497e13 ,-0.197805728776273e-15,
                          0.241554806033972e-10,-0.156481703640525e-5 , 0.277211346836625e-2 ,-0.203578994462286e2  ,
                          0.144369489909053e7  ,-0.411254217946539e11 , 0.623449786243773e-5 ,-0.221774281146038e2  ,
                         -0.689315087933158e5  ,-0.195419525060713e8  , 0.316373510564015e4  , 0.224040754426988e7  ,
                         -0.436701347922356e-5 ,-0.404213852833996e-3 ,-0.348153203414663e3  ,-0.385294213555289e6  ,
                          0.135203700099403e-6 , 0.134648383271089e-3 , 0.125031835351736e6  , 0.968123678455841e-1 ,
                          0.225660517512438e3  ,-0.190102435341872e-3 ,-0.299628410819229e-1 , 0.500833915372121e-2 ,
                          0.387842482998411    ,-0.138535367777182e4  , 0.870745245971773    , 0.171946252068742e1  ,
                         -0.326650121426383e-1 , 0.498044171727877e4  , 0.551478022765087e-2};

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3e(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3e in m3/kg

            double v = 0.0;
            const double vStar = 0.0032;
            const int N = 29;
            const double a = 0.587;
            const double b = 0.918;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 1.0;

            p = Math.Pow(p / 40000.0 - a, c);
            t = Math.Pow(t / 710.0 - b, d);


            int[] I = {  0,-12,-12,-10,-10,-10,-10,-10, -8, -8,
                        -8, -6, -5, -4, -4, -3, -3, -3, -2, -2,
                        -2, -2, -1,  0,  0,  1,  1,  1,  2,  2};

            int[] J = { 0,14,16, 3, 6,10,14,16, 7, 8,
                       10, 6, 6, 2, 4, 2, 6, 7, 0, 1,
                        3, 4, 0, 0, 1, 0, 4, 6, 0, 2};

            double[] n = {  0.0                  , 0.715815808404721e9  ,-0.114328360753449e12 , 0.376531002015720e-11,
                           -0.903983668691157e-4 , 0.665695908836252e6  , 0.535364174960127e10 , 0.794977402335603e11 ,
                            0.922230563421437e2  ,-0.142586073991215e6  ,-0.111796381424162e7  , 0.896121629640760e4  ,
                           -0.669989239070491e4  , 0.451242538486834e-2 ,-0.339731325977713e2  ,-0.120523111552278e1  ,
                            0.475992667717124e5  ,-0.266627750390341e6  ,-0.153314954386524e-3 , 0.305638404828265    ,
                            0.123654999499486e3  ,-0.104390794213011e4  ,-0.157496516174308e-1 , 0.685331118940253    ,
                            0.178373462873903e1  ,-0.544674124878910    , 0.204529931318843e4  ,-0.228342359328752e5  ,
                            0.413197481515899    ,-0.341931835910405e2 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3f(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3f in m3/kg

            double v = 0.0;
            const double vStar = 0.0064;
            const int N = 42;
            const double a = 0.587;
            const double b = 0.891;
            const double c = 0.5;
            const double d = 1.0;
            const double e = 4.0;

            p = Math.Pow(p / 40000.0 - a, c);
            t = Math.Pow(t / 730.0 - b, d);


            int[] I = {  0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
                         1, 2, 2, 3, 3, 3, 4, 5, 5, 6,
                         7, 7,10,12,12,12,14,14,14,14,
                        14,16,16,18,18,20,20,20,22,24,
                        24,28,32};

            int[] J = {  0, -3, -2, -1,  0,  1,  2, -1,  1,  2,
                         3,  0,  1, -5, -2,  0, -3, -8,  1, -6,
                        -4,  1, -6,-10, -8, -4,-12,-10, -8, -6,
                        -4,-10, -8,-12,-10,-12,-10, -6,-12,-12,
                        -4,-12,-12 };

            double[] n = {  0.0                  ,-0.251756547792325e-7 , 0.601307193668763e-5 ,-0.100615977450049e-2 ,
                            0.999969140252192    , 0.214107759236486e1  ,-0.165175571959086e2  ,-0.141987303638727e-2 ,
                            0.269251915156554e1  , 0.349741815858722e2  ,-0.300208695771783e2  ,-0.131546288252539e1  ,
                           -0.839091277286169e1  , 0.181545608337015e-9 ,-0.591099206478909e-3 , 0.152115067087106e1  ,
                            0.252956470663225e-4 , 0.100726265203786e-14,-0.149774533860650e1  ,-0.793940970562969e-9 ,
                           -0.150290891264717e-3 , 0.151205531275133e1  , 0.470942606221652e-5 , 0.195049710391712e-12,
                           -0.911627886266077e-8 , 0.604374640201265e-3 ,-0.225132933900136e-15, 0.610916973582981e-11,
                           -0.303063908043404e-6 ,-0.137796070798409e-4 ,-0.919296736666106e-3 , 0.639288223132545e-9 ,
                            0.753259479898699e-6 ,-0.400321478682929e-12, 0.756140294351614e-8 ,-0.912082054034891e-11,
                           -0.237612381140539e-7 , 0.269586010591874e-4 ,-0.732828135157839e-10, 0.241995578306660e-9 ,
                           -0.405735532730322e-3 , 0.189424143498011e-9 ,-0.486632965074563e-9 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3g(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3g in m3/kg

            double v = 0.0;
            const double vStar = 0.0027;
            const int N = 38;
            const double a = 0.872;
            const double b = 0.971;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 4.0;

            p = Math.Pow(p / 25000.0 - a, c);
            t = Math.Pow(t / 660.0 - b, d);


            int[] I = {  0,-12,-12,-12,-12,-12,-12,-10,-10,-10,
                        -8, -8, -8, -8, -6, -6, -5, -5, -4, -3,
                        -2, -2, -2, -2, -1, -1, -1,  0,  0,  0,
                         1,  1,  1,  3,  5,  6,  8, 10, 10 };

            int[] J = {  0, 7,12,14,18,22,24,14,20,24,
                         7, 8,10,12, 8,22, 7,20,22, 7,
                          3, 5,14,24, 2, 8,18, 0, 1, 2,
                          0, 1, 3,24,22,12, 3, 0, 6 };

            double[] n = {  0.0                 , 0.412209020652996e-4,-0.114987238280587e7 , 0.948180885032080e10,
                           -0.195788865718971e18, 0.496250704871300e25,-0.105549884548496e29,-0.758642165988278e12,
                           -0.922172769596101e23, 0.725379072059348e30,-0.617718249205859e2 , 0.107555033344858e5 ,
                           -0.379545802336487e8 , 0.228646846221831e12,-0.499741093010619e7 ,-0.280214310054101e31,
                            0.104915406769586e7 , 0.613754229168619e28, 0.802056715528378e32,-0.298617819828065e8 ,
                           -0.910782540134681e2 , 0.135033227281565e6 ,-0.712949383408211e19,-0.104578785289542e37,
                            0.304331584444093e2 , 0.593250797959445e10,-0.364174062110798e28, 0.921791403532461   ,
                           -0.337693609657471   ,-0.724644143758508e2 ,-0.110480239272601   , 0.536516031875059e1 ,
                           -0.291441872156205e4 , 0.616338176535305e40,-0.120889175861180e39, 0.818396024524612e23,
                            0.940781944835829e9 ,-0.367279669545448e5 ,-0.837513931798655e16 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3h(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3h in m3/kg

            double v = 0.0;
            const double vStar = 0.0032;
            const int N = 29;
            const double a = 0.898;
            const double b = 0.983;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 4.0;

            p = Math.Pow(p / 25000.0 - a, c);
            t = Math.Pow(t / 660.0 - b, d);


            int[] I = {  0,-12,-12,-10,-10,-10,-10,-10,-10, -8,
                        -8, -8, -8, -8, -6, -6, -6, -5, -5, -5,
                        -4, -4, -3, -3, -2, -1, -1,  0,  1,  1 };

            int[] J = { 0, 8,12, 4, 6, 8,10,14,16, 0,
                        1, 6, 7, 8, 4, 6, 8, 2, 3, 4,
                         2, 4, 1, 2, 0, 0, 2, 0, 0, 2 };

            double[] n = { 0.0                  , 0.561379678887577e-1 , 0.774135421587083e10 , 0.111482975877938e-8 ,
                       -0.143987128208183e-2 , 0.193696558764920e4  ,-0.605971823585005e9  , 0.171951568124337e14 ,
                       -0.185461154985145e17 , 0.387851168078010e-16,-0.395464327846105e-13,-0.170875935679023e3  ,
                       -0.212010620701220e4  , 0.177683337348191e8  , 0.110177443629575e2  ,-0.234396091693313e6  ,
                       -0.656174421999594e7  , 0.156362212977396e-4 ,-0.212946257021400e1  , 0.135249306374858e2  ,
                        0.177189164145813    , 0.139499167345464e4  ,-0.703670932036388e-2 ,-0.152011044389648    ,
                        0.981916922991113e-4 , 0.147199658618076e-2 , 0.202618487025578e2  , 0.899345518944240    ,
                       -0.211346402240858    , 0.249971752957491e2 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3i(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3i in m3/kg

            double v = 0.0;
            const double vStar = 0.0041;
            const int N = 42;
            const double a = 0.910;
            const double b = 0.984;
            const double c = 0.5;
            const double d = 1.0;
            const double e = 4.0;

            p = Math.Pow(p / 25000.0 - a, c);
            t = Math.Pow(t / 660.0 - b, d);


            int[] I = { 0, 0, 0, 0, 1, 1, 1, 1, 2, 3,
                        3, 4, 4, 4, 5, 5, 5, 7, 7, 8,
                        8,10,12,12,12,14,14,14,14,18,
                       18,18,18,18,20,20,22,24,24,32,
                       32,36,36 };

            int[] J = {  0,  0,  1, 10, -4, -2, -1,  0,  0, -5,
                         0, -3, -2, -1, -6, -1, 12, -4, -3, -6,
                        10, -8,-12, -6, -4,-10, -8, -4,  5,-12,
                       -10, -8, -6,  2,-12,-10,-12,-12, -8,-10,
                        -5,-10, -8 };

            double[] n = { 0.0                  , 0.106905684359136e1  ,-0.148620857922333e1  , 0.259862256980408e15 ,
                          -0.446352055678749e-11,-0.566620757170032e-6 ,-0.235302885736849e-2 ,-0.269226321968839    ,
                           0.922024992944392e1  , 0.357633505503772e-11,-0.173942565562222e2  , 0.700681785556229e-5 ,
                          -0.267050351075768e-3 ,-0.231779669675624e1  ,-0.753533046979752e-12, 0.481337131452891e1  ,
                          -0.223286270422356e22 ,-0.118746004987383e-4 , 0.646412934136496e-2 ,-0.410588536330937e-9 ,
                           0.422739537057241e20 , 0.313698180473812e-12, 0.164395334345040e-23,-0.339823323754373e-5 ,
                          -0.135268639905021e-1 ,-0.723252514211625e-14, 0.184386437538366e-8 ,-0.463959533752385e-1 ,
                          -0.992263100376750e14 , 0.688169154439335e-16,-0.222620998452197e-10,-0.540843018624083e-7 ,
                           0.345570606200257e-2 , 0.422275800304086e11 ,-0.126974478770487e-14, 0.927237985153679e-9 ,
                           0.612670812016489e-13,-0.722693924063497e-11,-0.383669502636822e-3 , 0.374684572410204e-3 ,
                          -0.931976897511086e5  ,-0.247690616026922e-1 , 0.658110546759474e2 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3j(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3j in m3/kg

            double v = 0.0;
            const double vStar = 0.0054;
            const int N = 29;
            const double a = 0.875;
            const double b = 0.964;
            const double c = 0.5;
            const double d = 1.0;
            const double e = 4.0;

            p = Math.Pow(p / 25000.0 - a, c);
            t = Math.Pow(t / 670.0 - b, d);


            int[] I = { 0, 0, 0, 0, 1, 1, 1, 2, 2, 3,
                        4, 4, 5, 5, 5, 6,10,12,12,14,
                       14,14,16,18,20,20,24,24,28,28 };

            int[] J = {  0, -1,  0,  1, -2, -1,  1, -1,  1, -2,
                        -2,  2, -3, -2,  0,  3, -6, -8, -3,-10,
                        -8, -5,-10,-12,-12,-10,-12, -6,-12, -5 };

            double[] n = { 0.0                  ,-0.111371317395540e-3 , 0.100342892423685e1  , 0.530615581928979e1  ,
                           0.179058760078792e-5 ,-0.728541958464774e-3 ,-0.187576133371704e2  , 0.199060874071849e-2 ,
                           0.243574755377290e2  ,-0.177040785499444e-3 ,-0.259680385227130e-2 ,-0.198704578406823e3  ,
                           0.738627790224287e-4 ,-0.236264692844138e-2 ,-0.161023121314333e1  , 0.622322971786473e4  ,
                          -0.960754116701669e-8 ,-0.510572269720488e-10, 0.767373781404211e-2 , 0.663855469485254e-14,
                          -0.717590735526745e-9 , 0.146564542926508e-4 , 0.309029474277013e-11,-0.464216300971708e-15,
                          -0.390499637961161e-13,-0.236716126781431e-9 , 0.454652854268717e-11,-0.422271787482497e-2 ,
                           0.283911742354706e-10, 0.270929002720228e1 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3k(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3k in m3/kg

            double v = 0.0;
            double vStar = 0.0077;
            const int N = 34;
            const double a = 0.802;
            const double b = 0.935;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 1.0;

            p = Math.Pow(p / 25000.0 - a, c);
            t = Math.Pow(t / 680.0 - b, d);


            int[] I = { 0,-2,-2,-1,-1, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 1, 1, 1, 1, 1, 2,
                        2, 2, 2, 2, 2, 5, 5, 5, 6, 6,
                        6, 6, 8,10,12 };

            int[] J = {  0, 10, 12, -5,  6,-12, -6, -2, -1,  0,
                         1,  2,  3, 14, -3, -2,  0,  1,  2, -8,
                        -6, -3, -2,  0,  4,-12, -6, -3,-12,-10,
                        -8, -5,-12,-12,-10 };

            double[] n = { 0.0                  ,-0.401215699576099e9  , 0.484501478318406e11 , 0.394721471363678e-14,
                           0.372629967374147e5  ,-0.369794374168666e-29,-0.380436407012452e-14, 0.475361629970233e-6 ,
                          -0.879148916140706e-3 , 0.844317863844331    , 0.122433162656600e2  ,-0.104529634830279e3  ,
                           0.589702771277429e3  ,-0.291026851164444e14 , 0.170343072841850e-5 ,-0.277617606975748e-3 ,
                          -0.344709605486686e1  , 0.221333862447095e2  ,-0.194646110037079e3  , 0.808354639772825e-15,
                          -0.180845209145470e-10,-0.696664158132412e-5 ,-0.181057560300994e-2 , 0.255830298579027e1  ,
                           0.328913873658481e4  ,-0.173270241249904e-18,-0.661876792558034e-6 ,-0.395688923421250e-2 ,
                           0.604203299819132e-17,-0.400879935920517e-13, 0.160751107464958e-8 , 0.383719409025556e-4 ,
                          -0.649565446702457e-14,-0.149095328506000e-11, 0.541449377329581e-8 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3l(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3l in m3/kg

            double v = 0.0;
            const double vStar = 0.0026;
            const int N = 43;
            const double a = 0.908;
            const double b = 0.989;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 4.0;

            p = Math.Pow(p / 24000.0 - a, c);
            t = Math.Pow(t / 650.0 - b, d);


            int[] I = {  0,-12,-12,-12,-12,-12,-10,-10, -8, -8,
                        -8, -8, -8, -8, -8, -6, -5, -5, -4, -4,
                        -3, -3, -3, -3, -2, -2, -2, -1, -1, -1,
                         0,  0,  0,  0,  1,  1,  2,  4,  5,  5,
                         6, 10, 10, 14 };

            int[] J = { 0,14,16,18,20,22,14,24, 6,10,
                       12,14,18,24,36, 8, 4, 5, 7,16,
                        1, 3,18,20, 2, 3,10, 0, 1, 3,
                        0, 1, 2,12, 0,16, 1, 0, 0, 1,
                       14, 4,12,10 };

            double[] n = { 0.0                 , 0.260702058647537e10,-0.188277213604704e15, 0.554923870289667e19,
                          -0.758966946387758e23, 0.413865186848908e27,-0.815038000738060e12,-0.381458260489955e33,
                          -0.123239564600519e-1, 0.226095631437174e8 ,-0.495017809506720e12, 0.529482996422863e16,
                          -0.444359478746295e23, 0.521635864527315e35,-0.487095672740742e55,-0.714430209937547e6 ,
                           0.127868634615495   ,-0.100752127917598e2 , 0.777451437960990e7 ,-0.108105480796471e25,
                          -0.357578581169659e-5,-0.212857169423484e1 , 0.270706111085238e30,-0.695953622348829e33,
                           0.110609027472280   , 0.721559163361354e2 ,-0.306367307532219e15, 0.265839618885530e-4,
                           0.253392392889754e-1,-0.214443041836579e3 , 0.937846601489667   , 0.223184043101700e1 ,
                           0.338401222509191e2 , 0.494237237179718e21,-0.198068404154428   ,-0.141415349881140e31,
                          -0.993862421613651e2 , 0.125070534142731e3 ,-0.996473529004439e3 , 0.473137909872765e5 ,
                           0.116662121219322e33,-0.315874976271533e16,-0.445703369196945e33, 0.642794932373694e33 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3m(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3m in m3/kg

            double v = 0.0;
            const double vStar = 0.0028;
            const int N = 40;
            const double a = 1.0;
            const double b = 0.997;
            const double c = 1.0;
            const double d = 0.25;
            const double e = 1.0;

            p = Math.Pow(p / 23000.0 - a, c);
            t = Math.Pow(t / 650.0 - b, d);


            int[] I = { 0, 0, 3, 8,20, 1, 3, 4, 5, 1,
                        6, 2, 4,14, 2, 5, 3, 0, 1, 1,
                        1,28, 2,16, 0, 5, 0, 3, 4,12,
                       16, 1, 8,14, 0, 2, 3, 4, 8,14,
                       24 };

            int[] J = { 0, 0, 0, 0, 2, 5, 5, 5, 5, 6,
                        6, 7, 8, 8,10,10,12,14,14,18,
                       20,20,22,22,24,24,28,28,28,28,
                       28,32,32,32,36,36,36,36,36,36,
                       36 };

            double[] n = { 0.0                 , 0.811384363481847   ,-0.568199310990094e4 ,-0.178657198172556e11,
                           0.795537657613427e32,-0.814568209346872e5 ,-0.659774567602874e8 ,-0.152861148659302e11,
                          -0.560165667510446e12, 0.458384828593949e6 ,-0.385754000383848e14, 0.453735800004273e8 ,
                           0.939454935735563e12, 0.266572856432938e28,-0.547578313899097e10, 0.200725701112386e15,
                           0.185007245563239e13, 0.185135446828337e9 ,-0.170451090076385e12, 0.157890366037614e15,
                          -0.202530509748774e16, 0.368193926183570e60, 0.170215539458936e18, 0.639234909918741e42,
                          -0.821698160721956e15,-0.795260241872306e24, 0.233415869478510e18,-0.600079934586803e23,
                           0.594584382273384e25, 0.189461279349492e40,-0.810093428842645e46, 0.188813911076809e22,
                           0.111052244098768e36, 0.291133958602503e46,-0.329421923951460e22,-0.137570282536696e26,
                           0.181508996303902e28,-0.346865122768353e30,-0.211961148774260e38,-0.128617899887675e49,
                           0.479817895699239e65 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3n(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3n in m3/kg

            double v = 0.0;
            const double vStar = 0.0031;
            const int N = 39;
            const double a = 0.976;
            const double b = 0.997;

            p = p / 23000.0 - a;
            t = t / 650.0 - b;


            int[] I = { 0, 0, 3, 4, 6, 7,10,12,14,18,
                        0, 3, 5, 6, 8,12, 0, 3, 7,12,
                        2, 3, 4, 2, 4, 7, 4, 3, 5, 6,
                        0, 0, 3, 1, 0, 1, 0, 1, 0, 1 };

            int[] J = {  0,-12,-12,-12,-12,-12,-12,-12,-12,-12,
                       -10,-10,-10,-10,-10,-10, -8, -8, -8, -8,
                        -6, -6, -6, -5, -5, -5, -4, -3, -3, -3,
                        -2, -1, -1,  0,  1,  1,  2,  4,  5,  6 };

            double[] n = { 0.0                  , 0.280967799943151e-38, 0.614869006573609e-30, 0.582238667048942e-27,
                           0.390628369238462e-22, 0.821445758255119e-20, 0.402137961842776e-14, 0.651718171878301e-12,
                          -0.211773355803058e-7 , 0.264953354380072e-2 ,-0.135031446451331e-31,-0.607246643970893e-23,
                          -0.402352115234494e-18,-0.744938506925544e-16, 0.189917206526237e-12, 0.364975183508473e-5 ,
                           0.177274872361946e-25,-0.334952758812999e-18,-0.421537726098389e-8 ,-0.391048167929649e-1 ,
                           0.541276911564176e-13, 0.705412100773699e-11, 0.258585887897486e-8 ,-0.493111362030162e-10,
                          -0.158649699894543e-5 ,-0.525037427886100    , 0.220019901729615e-2 ,-0.643064132636925e-2 ,
                           0.629154149015048e2  , 0.135147318617061e3  , 0.240560808321713e-6 ,-0.890763306701305e-3 ,
                          -0.440209599407714e4  ,-0.302807107747776e3  , 0.159158748314599e4  , 0.232534272709876e6  ,
                          -0.792681207132600e6  ,-0.869871364662769e11 , 0.354542769185671e12 , 0.400849240129329e15 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Exp(v) * vStar;
        }

        double Vpt_3o(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3o in m3/kg

            double v = 0.0;
            const double vStar = 0.0034;
            const int N = 24;
            const double a = 0.974;
            const double b = 0.996;
            const double c = 0.5;
            const double d = 1.0;
            const double e = 1.0;

            p = Math.Pow(p / 23000.0 - a, c);
            t = Math.Pow(t / 650.0 - b, d);


            int[] I = { 0, 0, 0, 0, 2, 3, 4, 4, 4, 4,
                        4, 5, 5, 6, 7, 8, 8, 8,10,10,
                       14,14,20,20,24 };

            int[] J = {  0,-12, -4, -1, -1,-10,-12, -8, -5, -4,
                        -1, -4, -3, -8,-12,-10, -8, -4,-12, -8,
                       -12, -8,-12,-10,-12 };

            double[] n = { 0.0                  , 0.128746023979718e-34,-0.735234770382342e-11, 0.289078692149150e-2 ,
                        0.244482731907223    , 0.141733492030985e-23,-0.354533853059476e-28,-0.594539202901431e-17,
                       -0.585188401782779e-8 , 0.201377325411803e-5 , 0.138647388209306e1  ,-0.173959365084772e-4 ,
                        0.137680878349369e-2 , 0.814897605805513e-14, 0.425596631351839e-25,-0.387449113787755e-17,
                        0.139814747930240e-12,-0.171849638951521e-2 , 0.641890529513296e-21, 0.118960578072018e-10,
                       -0.155282762571611e-17, 0.233907907347507e-7 ,-0.174093247766213e-12, 0.377682649089149e-8 ,
                       -0.516720236575302e-10 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3p(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3p in m3/kg

            double v = 0.0;
            const double vStar = 0.0041;
            const int N = 27;
            const double a = 0.972;
            const double b = 0.997;
            const double c = 0.5;
            const double d = 1.0;
            const double e = 1.0;

            p = Math.Pow(p / 23000.0 - a, c);
            t = Math.Pow(t / 650.0 - b, d);


            int[] I = { 0, 0, 0, 0, 0, 1, 2, 3, 3, 4,
                        6, 7, 7, 8,10,12,12,12,14,14,
                       14,16,18,20,22,24,24,36 };

            int[] J = {  0, -1,  0,  1,  2,  1, -1, -3,  0, -2,
                        -2, -5, -4, -2, -3,-12, -6, -5,-10, -8,
                        -3, -8, -8,-10,-10,-12, -8,-12 };

            double[] n = { 0.0                  ,-0.982825342010366e-4 , 0.105145700850612e1  , 0.116033094095084e3  ,
                        0.324664750281543e4  ,-0.123592348610137e4  ,-0.561403450013495e-1 , 0.856677401640869e-7 ,
                        0.236313425393924e3  , 0.972503292350109e-2 ,-0.103001994531927e1  ,-0.149653706199162e-8 ,
                       -0.215743778861592e-4 ,-0.834452198291445e1  , 0.586602660564988    , 0.343480022104968e-25,
                        0.816256095947021e-5 , 0.294985697916798e-2 , 0.711730466276584e-16, 0.400954763806941e-9 ,
                        0.107766027032853e2  ,-0.409449599138182e-6 ,-0.729121307758902e-5 , 0.677107970938909e-8 ,
                        0.602745973022975e-7 ,-0.382323011855257e-10, 0.179946628317437e-2 ,-0.345042834640005e-3 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3q(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3q in m3/kg

            double v = 0.0;
            const double vStar = 0.0022;
            const int N = 24;
            const double a = 0.848;
            const double b = 0.983;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 4.0;

            p = Math.Pow(p / 23000.0 - a, c);
            t = Math.Pow(t / 650.0 - b, d);


            int[] I = {  0,-12,-12,-10,-10,-10,-10, -8, -6, -5,
                        -5, -4, -4, -3, -2, -2, -2, -2, -1, -1,
                        -1,  0,  1,  1,  1 };

            int[] J = {  0, 10, 12,  6,  7,  8, 10,  8,  6,  2,
                         5,  3,  4,  3,  0,  1,  2,  4,  0,  1,
                         2,  0,  0,  1,  3 };

            double[] n = { 0.0                 ,-0.820433843259950e5 , 0.473271518461586e11,-0.805950021005413e-1,
                        0.328600025435980e2 ,-0.356617029982490e4 ,-0.172985781433335e10, 0.351769232729192e8 ,
                       -0.775489259985144e6 , 0.710346691966018e-4, 0.993499883820274e5 ,-0.642094171904570   ,
                       -0.612842816820083e4 , 0.232808472983776e3 ,-0.142808220416837e-4,-0.643596060678456e-2,
                       -0.428577227475614e1 , 0.225689939161918e4 , 0.100355651721510e-2, 0.333491455143516   ,
                        0.109697576888873e1 , 0.961917379376452   ,-0.838165632204598e-1, 0.247795908411492e1 ,
                       -0.319114969006533e4 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3r(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3r in m3/kg

            double v = 0.0;
            const double vStar = 0.0054;
            const int N = 27;
            const double a = 0.874;
            const double b = 0.982;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 1.0;

            p = Math.Pow(p / 23000.0 - a, c);
            t = Math.Pow(t / 650.0 - b, d);


            int[] I = { 0,-8,-8,-3,-3,-3,-3,-3, 0, 0,
                        0, 0, 3, 3, 8, 8, 8, 8,10,10,
                       10,10,10,10,10,10,12,14 };

            int[] J = {  0,  6, 14, -3,  3,  4,  5,  8, -1,  0,
                         1,  5, -6, -2,-12,-10, -8, -5,-12,-10,
                        -8, -6, -5, -4, -3, -2,-12,-12 };

            double[] n = { 0.0                  , 0.144165955660863e-2 ,-0.701438599628258e13 ,-0.830946716459219e-16,
                        0.261975135368109    , 0.393097214706245e3  ,-0.104334030654021e5  , 0.490112654154211e9  ,
                       -0.147104222772069e-3 , 0.103602748043408e1  , 0.305308890065089e1  ,-0.399745276971264e7  ,
                        0.569233719593750e-11,-0.464923504407778e-1 ,-0.535400396512906e-17, 0.399988795693162e-12,
                       -0.536479560201811e-6 , 0.159536722411202e-1 , 0.270303248860217e-14, 0.244247453858506e-7 ,
                       -0.983430636716454e-5 , 0.663513144224454e-1 ,-0.993456957845006e1  , 0.546491323528491e3  ,
                       -0.143365406393758e5  , 0.150764974125511e6  ,-0.337209709340105e-9 , 0.377501980025469e-8 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3s(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3s in m3/kg

            double v = 0.0;
            const double vStar = 0.0022;
            const int N = 29;
            const double a = 0.886;
            const double b = 0.990;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 4.0;

            p = Math.Pow(p / 21000.0 - a, c);
            t = Math.Pow(t / 640.0 - b, d);


            int[] I = {  0,-12,-12,-10, -8, -6, -5, -5, -4, -4,
                        -3, -3, -2, -1, -1, -1,  0,  0,  0,  0,
                         1,  1,  3,  3,  3,  4,  4,  4,  5, 14 };

            int[] J = { 0,20,24,22,14,36, 8,16, 6,32,
                        3, 8, 4, 1, 2, 3, 0, 1, 4,28,
                        0,32, 0, 1, 2, 3,18,24, 4,24 };

            double[] n = { 0.0                 ,-0.532466612140254e23, 0.100415480000824e32,-0.191540001821367e30,
                        0.105618377808847e17, 0.202281884477061e59, 0.884585472596134e8 , 0.166540181638363e23,
                       -0.313563197669111e6 ,-0.185662327545324e54,-0.624942093918942e-1,-0.504160724132590e10,
                        0.187514491833092e5 , 0.121399979993217e-2, 0.188317043049455e1 ,-0.167073503962060e4 ,
                        0.965961650599775   , 0.294885696802488e1 ,-0.653915627346115e5 , 0.604012200163444e50,
                       -0.198339358557937   ,-0.175984090163501e58, 0.356314881403987e1 ,-0.575991255144384e3 ,
                        0.456213415338071e5 ,-0.109174044987829e8 , 0.437796099975134e34,-0.616552611135792e46,
                        0.193568768917797e10, 0.950898170425042e54 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3t(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3t in m3/kg

            double v = 0.0;
            const double vStar = 0.0088;
            const int N = 33;
            const double a = 0.803;
            const double b = 1.02;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 1.0;

            p = Math.Pow(p / 20000.0 - a, c);
            t = Math.Pow(t / 650.0 - b, d);


            int[] I = { 0, 0, 0, 0, 0, 1, 1, 2, 2, 2,
                        3, 3, 4, 4, 7, 7, 7, 7, 7,10,
                       10,10,10,10,18,20,22,22,24,28,
                       32,32,32,36 };

            int[] J = { 0, 0, 1, 4,12, 0,10, 0, 6,14,
                        3, 8, 0,10, 3, 4, 7,20,36,10,
                       12,14,16,22,18,32,22,36,24,28,
                       22,32,36,36 };

            double[] n = { 0.0                 , 0.155287249586268e1 , 0.664235115009031e1 ,-0.289366236727210e4 ,
                       -0.385923202309848e13,-0.291002915783761e1 ,-0.829088246858083e12, 0.176814899675218e1 ,
                       -0.534686695713469e9 , 0.160464608687834e18, 0.196435366560186e6 , 0.156637427541729e13,
                       -0.178154560260006e1 ,-0.229746237623692e16, 0.385659001648006e8 , 0.110554446790543e10,
                       -0.677073830687349e14,-0.327910592086523e31,-0.341552040860644e51,-0.527251339709047e21,
                        0.245375640937055e24,-0.168776617209269e27, 0.358958955867578e29,-0.656475280339411e36,
                        0.355286045512301e39, 0.569021454413270e58,-0.700584546433113e48,-0.705772623326374e65,
                        0.166861176200148e53,-0.300475129680486e61,-0.668481295196808e51, 0.428432338620678e69,
                       -0.444227367758304e72,-0.281396013562745e77 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3u(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3u in m3/kg

            double v = 0.0;
            const double vStar = 0.0026;
            const int N = 38;
            const double a = 0.902;
            const double b = 0.988;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 1.0;

            p = Math.Pow(p / 23000.0 - a, c);
            t = Math.Pow(t / 650.0 - b, d);


            int[] I = {  0,-12,-10,-10,-10, -8, -8, -8, -6, -6,
                        -5, -5, -5, -3, -1, -1, -1, -1,  0,  0,
                         1,  2,  2,  3,  5,  5,  5,  6,  6,  8,
                         8, 10, 12, 12, 12, 14, 14, 14, 14 };

            int[] J = {  0, 14, 10, 12, 14, 10, 12, 14,  8, 12,
                         4,  8, 12,  2, -1,  1, 12, 14, -3,  1,
                        -2,  5, 10, -5, -4,  2,  3, -5,  2, -8,
                         8, -4,-12, -4,  4,-12,-10, -6,  6 };

            double[] n = { 0.0                  , 0.122088349258355e18 , 0.104216468608488e10 ,-0.882666931564652e16 ,
                        0.259929510849499e20 , 0.222612779142211e15 ,-0.878473585050085e18 ,-0.314432577551552e22 ,
                       -0.216934916996285e13 , 0.159079648196849e21 ,-0.339567617303423e3  , 0.884387651337836e13 ,
                       -0.843405926846418e21 , 0.114178193518022e2  ,-0.122708229235641e-3 ,-0.106201671767107e3  ,
                        0.903443213959313e25 ,-0.693996270370852e28 , 0.648916718965575e-8 , 0.718957567127851e4  ,
                        0.105581745346187e-2 ,-0.651903203602581e15 ,-0.160116813274676e25 ,-0.510254294237837e-8 ,
                       -0.152355388953402    , 0.677143292290144e12 , 0.276378438378930e15 , 0.116862983141686e-1 ,
                       -0.301426947980171e14 , 0.169719813884840e-7 , 0.104674840020929e27 ,-0.108016904560140e5  ,
                       -0.990623601934295e-12, 0.536116483602738e7  , 0.226145963747881e22 ,-0.488731565776210e-9 ,
                        0.151001548880670e-4 ,-0.227700464643920e5  ,-0.781754507698846e28 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3v(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3v in m3/kg

            double v = 0.0;
            const double vStar = 0.0031;
            const int N = 39;
            const double a = 0.960;
            const double b = 0.995;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 1.0;

            p = Math.Pow(p / 23000.0 - a, c);
            t = Math.Pow(t / 650.0 - b, d);


            int[] I = {  0,-10, -8, -6, -6, -6, -6, -6, -6, -5,
                        -5, -5, -5, -5, -5, -4, -4, -4, -4, -3,
                        -3, -3, -2, -2, -1, -1,  0,  0,  0,  1,
                         1,  3,  4,  4,  4,  5,  8, 10, 12, 14 };

            int[] J = {  0, -8,-12,-12, -3,  5,  6,  8, 10,  1,
                         2,  6,  8, 10, 14,-12,-10, -6, 10, -3,
                        10, 12,  2,  4, -2,  0, -2,  6, 10,-12,
                       -10,  3, -6,  3, 10,  2,-12, -2, -3,  1 };

            double[] n = { 0.0                  ,-0.415652812061591e-54, 0.177441742924043e-60,-0.357078668203377e-54,
                        0.359252213604114e-25,-0.259123736380269e2  , 0.594619766193460e5  ,-0.624184007103158e11 ,
                        0.313080299915944e17 , 0.105006446192036e-8 ,-0.192824336984852e-5 , 0.654144373749937e6  ,
                        0.513117462865044e13 ,-0.697595750347391e19 ,-0.103977184454767e29 , 0.119563135540666e-47,
                       -0.436677034051655e-41, 0.926990036530639e-29, 0.587793105620748e21 , 0.280375725094731e-17,
                       -0.192359972440634e23 , 0.742705723302738e27 ,-0.517429682450605e2  , 0.820612048645469e7  ,
                       -0.188214882341448e-8 , 0.184587261114837e-1 ,-0.135830407782663e-5 ,-0.723681885626348e17 ,
                       -0.223449194054124e27 ,-0.111526741826431e-34, 0.276032601145151e-28, 0.134856491567853e15 ,
                        0.652440293345860e-9 , 0.510655119774360e17 ,-0.468138358908732e32 ,-0.760667491183279e16 ,
                       -0.417247986986821e-18, 0.312545677756104e14 ,-0.100375333864186e15 , 0.247761392329058e27 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3w(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3w in m3/kg

            double v = 0.0;
            const double vStar = 0.0039;
            const int N = 35;
            const double a = 0.959;
            const double b = 0.995;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 4.0;

            p = Math.Pow(p / 23000.0 - a, c);
            t = Math.Pow(t / 650.0 - b, d);


            int[] I = {  0,-12,-12,-10,-10, -8, -8, -8, -6, -6,
                        -6, -6, -5, -4, -4, -3, -3, -2, -2, -1,
                        -1, -1,  0,  0,  1,  2,  2,  3,  3,  5,
                         5,  5,  8,  8, 10, 10 };

            int[] J = {  0,  8, 14, -1,  8,  6,  8, 14, -4, -3,
                         2,  8,-10, -1,  3,-10,  3,  1,  2, -8,
                        -4,  1,-12,  1, -1, -1,  2,-12, -5,-10,
                        -8, -6,-12,-10,-12, -8 };

            double[] n = { 0.0                  ,-0.586219133817016e-7 ,-0.894460355005526e11 , 0.531168037519774e-30,
                        0.109892402329239    ,-0.575368389425212e-1 , 0.228276853990249e5  ,-0.158548609655002e19 ,
                        0.329865748576503e-27,-0.634987981190669e-24, 0.615762068640611e-8 ,-0.961109240985747e8  ,
                       -0.406274286652625e-44,-0.471103725498077e-12, 0.725937724828145    , 0.187768525763682e-38,
                       -0.103308436323771e4  ,-0.662552816342168e-1 , 0.579514041765710e3  , 0.237416732616644e-26,
                        0.271700235739893e-14,-0.907886213483600e2  ,-0.171242509570207e-36, 0.156792067854621e3  ,
                        0.923261357901470    ,-0.597865988422577e1  , 0.321988767636389e7  ,-0.399441390042203e-29,
                        0.493429086046981e-7 , 0.812036983370565e-19,-0.207610284654137e-11,-0.340821291419719e-6 ,
                        0.542000573372233e-17,-0.856711586510214e-12, 0.266170454405981e-13, 0.858133791857099e-5 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3x(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3x in m3/kg

            double v = 0.0;
            const double vStar = 0.0049;
            const int N = 36;
            const double a = 0.910;
            const double b = 0.988;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 1.0;

            p = Math.Pow(p / 23000.0 - a, c);
            t = Math.Pow(t / 650.0 - b, d);


            int[] I = { 0,-8,-6,-5,-4,-4,-4,-3,-3,-1,
                        0, 0, 0, 1, 1, 2, 3, 3, 3, 4,
                        5, 5, 5, 6, 8, 8, 8, 8,10,12,
                       12,12,12,14,14,14,14 };

            int[] J = {  0, 14, 10, 10,  1,  2, 14, -2, 12,  5,
                         0,  4, 10,-10, -1,  6,-12,  0,  8,  3,
                        -6, -2,  1,  1, -6, -3,  1,  8, -8,-10,
                        -8, -5, -4,-12,-10, -8, -6 };

            double[] n = { 0.0                  , 0.377373741298151e19 ,-0.507100883722913e13 ,-0.103363225598860e16 ,
                        0.184790814320773e-5 ,-0.924729378390945e-3 ,-0.425999562292738e24 ,-0.462307771873973e-12,
                        0.107319065855767e22 , 0.648662492280682e11 , 0.244200600688281e1  ,-0.851535733484258e10 ,
                        0.169894481433592e22 , 0.215780222509020e-26,-0.320850551367334    ,-0.382642448458610e17 ,
                       -0.275386077674421e-28,-0.563199253391666e6  ,-0.326068646279314e21 , 0.397949001553184e14 ,
                        0.100824008584757e-6 , 0.162234569738433e5  ,-0.432355225319745e11 ,-0.592874245598610e12 ,
                        0.133061647281106e1  , 0.157338197797544e7  , 0.258189614270853e14 , 0.262413209706358e25 ,
                       -0.920011937431142e-1 , 0.220213765905426e-2 ,-0.110433759109547e2  , 0.847004870612087e7  ,
                       -0.592910695762536e9  ,-0.183027173269660e-4 , 0.181339603516302    ,-0.119228759669889e4  ,
                        0.430867658061468e7 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3y(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3y in m3/kg

            double v = 0.0;
            const double vStar = 0.0031;
            const int N = 20;
            const double a = 0.996;
            const double b = 0.994;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 4.0;

            p = Math.Pow(p / 22000.0 - a, c);
            t = Math.Pow(t / 650.0 - b, d);


            int[] I = { 0, 0, 0, 0, 0, 1, 2, 2, 2, 2,
                        3, 3, 3, 4, 4, 5, 5, 8, 8,10,
                       12 };

            int[] J = { 0,-3, 1, 5, 8, 8,-4,-1, 4, 5,
                       -8, 4, 8,-6, 6,-2, 1,-8,-2,-5,
                       -8 };

            double[] n = { 0.0                  ,-0.525597995024633e-9 , 0.583441305228407e4  ,-0.134778968457925e17 ,
                        0.118973500934212e26 ,-0.159096490904708e27 ,-0.315839902302021e-6 , 0.496212197158239e3  ,
                        0.327777227273171e19 ,-0.527114657850696e22 , 0.210017506281863e-16, 0.705106224399834e21 ,
                       -0.266713136106469e31 ,-0.145370512554562e-7 , 0.149333917053130e28 ,-0.149795620287641e8  ,
                       -0.381881906271100e16 , 0.724660165585797e-4 ,-0.937808169550193e14 , 0.514411468376383e10 ,
                       -0.828198594040141e5 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        double Vpt_3z(double p, double t)
        {
            // p is pressure in kPa
            // t is temperature in K
            // returns specific volume in Region 3z in m3/kg

            double v = 0.0;
            const double vStar = 0.0038;
            const int N = 23;
            const double a = 0.993;
            const double b = 0.994;
            const double c = 1.0;
            const double d = 1.0;
            const double e = 4.0;

            p = Math.Pow(p / 22000.0 - a, c);
            t = Math.Pow(t / 650.0 - b, d);


            int[] I = { 0,-8,-6,-5,-5,-4,-4,-4,-3,-3,
                       -3,-2,-1, 0, 1, 2, 3, 3, 6, 6,
                        6, 6, 8, 8 };

            int[] J = { 0, 3, 6, 6, 8, 5, 6, 8,-2, 5,
                        6, 2,-6, 3, 1, 6,-6,-2,-6,-5,
                       -4,-1,-8,-4 };

            double[] n = { 0.0                  , 0.244007892290650e-10,-0.463057430331242e7  , 0.728803274777712e10 ,
                        0.327776302858856e16 ,-0.110598170118409e10 ,-0.323899915729957e13 , 0.923814007023245e16 ,
                        0.842250080413712e-12, 0.663221436245506e12 ,-0.167170186672139e15 , 0.253749358701391e4  ,
                       -0.819731559610523e-20, 0.328380587890663e12 ,-0.625004791171543e8  , 0.803197957462023e21 ,
                       -0.204397011338353e-10,-0.378391047055938e4  , 0.972876545938620e-2 , 0.154355721681459e2  ,
                       -0.373962862928643e4  ,-0.682859011374572e11 ,-0.248488015614543e-3 , 0.394536049497068e7 };

            for (int i = 1; i <= N; i++)
                v += n[i] * Math.Pow(p, I[i]) * Math.Pow(t, J[i]);

            return Math.Pow(v, e) * vStar;
        }

        void SubRegion(double p, double t)
        {
            // p is pressure in kPa
            // T is temperature in K
            // sets the subregion

            state.subRegion = "";

            if (p > 40000.0 && p <= 100000.0)
            {
                double tB = T_atRegionBoundary(p, "3ab");
                if (t > tB)
                    state.subRegion = "3b";
                else
                    state.subRegion = "3a";
            }
            else if (p > 25000.0)
            {
                double tBab = T_atRegionBoundary(p, "3ab");
                double tBcd = T_atRegionBoundary(p, "3cd");
                double tBef = T_atRegionBoundary(p, "3ef");

                if (t <= tBcd)
                    state.subRegion = "3c";
                else if (t <= tBab)
                    state.subRegion = "3d";
                else if (t <= tBef)
                    state.subRegion = "3e";
                else
                    state.subRegion = "3f";
            }
            else if (p > 23500.0)
            {
                double tBcd = T_atRegionBoundary(p, "3cd");
                double tBef = T_atRegionBoundary(p, "3ef");
                double tBgh = T_atRegionBoundary(p, "3gh");
                double tBij = T_atRegionBoundary(p, "3ij");
                double tBjk = T_atRegionBoundary(p, "3jk");

                if (t <= tBcd)
                    state.subRegion = "3c";
                else if (t <= tBgh)
                    state.subRegion = "3g";
                else if (t <= tBef)
                    state.subRegion = "3h";
                else if (t <= tBij)
                    state.subRegion = "3i";
                else if (t <= tBjk)
                    state.subRegion = "3j";
                else
                    state.subRegion = "3k";
            }
            else if (p > 23000.0)
            {
                double tBcd = T_atRegionBoundary(p, "3cd");
                double tBef = T_atRegionBoundary(p, "3ef");
                double tBgh = T_atRegionBoundary(p, "3gh");
                double tBij = T_atRegionBoundary(p, "3ij");
                double tBjk = T_atRegionBoundary(p, "3jk");

                if (t <= tBcd)
                    state.subRegion = "3c";
                else if (t <= tBgh)
                    state.subRegion = "3l";
                else if (t <= tBef)
                    state.subRegion = "3h";
                else if (t <= tBij)
                    state.subRegion = "3i";
                else if (t <= tBjk)
                    state.subRegion = "3j";
                else
                    state.subRegion = "3k";
            }
            else if (p > 22500.0)
            {
                double tBcd = T_atRegionBoundary(p, "3cd");
                double tBef = T_atRegionBoundary(p, "3ef");
                double tBgh = T_atRegionBoundary(p, "3gh");
                double tBij = T_atRegionBoundary(p, "3ij");
                double tBjk = T_atRegionBoundary(p, "3jk");
                double tBmn = T_atRegionBoundary(p, "3mn");
                double tBop = T_atRegionBoundary(p, "3op");

                if (t <= tBcd)
                    state.subRegion = "3c";
                else if (t <= tBgh)
                    state.subRegion = "3l";
                else if (t <= tBmn)
                    state.subRegion = "3m";
                else if (t <= tBef)
                    state.subRegion = "3n";
                else if (t <= tBop)
                    state.subRegion = "3o";
                else if (t <= tBij)
                    state.subRegion = "3p";
                else if (t <= tBjk)
                    state.subRegion = "3j";
                else
                    state.subRegion = "3k";
            }
            else
            {
                double pSat97 = daddy.Pi_4(643.15);
                //double pSat97 = daddy.Psat(643.15, ref stat, 0);

                if (p > pSat97)
                {
                    double tBcd = T_atRegionBoundary(p, "3cd");
                    double tBqu = T_atRegionBoundary(p, "3qu");
                    double tBrx = T_atRegionBoundary(p, "3rx");
                    double tBjk = T_atRegionBoundary(p, "3jk");

                    if (t <= tBcd)
                        state.subRegion = "3c";
                    else if (t <= tBqu)
                        state.subRegion = "3q";

                    if (t > tBrx && t <= tBjk)
                        state.subRegion = "3r";
                    if (t > tBjk)
                        state.subRegion = "3k";


                    if (state.subRegion != "")
                        return;

                    // tBqu < T <= tBrx
                    // small regions right around critical point
                    // 3u, 3x, 3y, 3z, 3v and 3w

                    double tBuv = T_atRegionBoundary(p, "3uv");
                    double tBwx = T_atRegionBoundary(p, "3wx");
                    double tBef = T_atRegionBoundary(p, "3ef");

                    if (p > 22110.0)
                    {
                        if (t <= tBuv)
                            state.subRegion = "3u";
                        else if (t < tBef)
                            state.subRegion = "3v";
                        else if (t < tBwx)
                            state.subRegion = "3w";
                        else
                            state.subRegion = "3x";
                    }
                    else if (p > 22064.0)
                    {
                        if (t <= tBuv)
                            state.subRegion = "3u";
                        else if (t <= tBef)
                            state.subRegion = "3y";
                        else if (t <= tBwx)
                            state.subRegion = "3z";
                        else
                            state.subRegion = "3x";
                    }
                    else
                    {
                        double tSat = daddy.Theta_4(p);

                        if (t <= tSat)
                        {
                            if (p > 21931.61551)
                            {
                                if (t < tBuv)
                                    state.subRegion = "3u";
                                else
                                    state.subRegion = "3y";
                            }
                            else
                                state.subRegion = "3u";
                        }
                        else
                        {
                            if (p > 21900.96265)
                            {
                                if (t < tBwx)
                                    state.subRegion = "3z";
                                else
                                    state.subRegion = "3x";
                            }
                            else
                                state.subRegion = "3x";
                        }

                    }


                }
                else if (p > 20500.0)
                {
                    double tBcd = T_atRegionBoundary(p, "3cd");
                    double tBjk = T_atRegionBoundary(p, "3jk");
                    double tSat = daddy.Theta_4(p);

                    if (t <= tBcd)
                        state.subRegion = "3c";
                    else if (t <= tSat)
                        state.subRegion = "3s";
                    else if (t <= tBjk)
                        state.subRegion = "3r";
                    else
                        state.subRegion = "3k";
                }
                else if (p > 19008.81189173929)
                {
                    double tBcd = T_atRegionBoundary(p, "3cd");
                    double tSat = daddy.Theta_4(p);

                    if (t <= tBcd)
                        state.subRegion = "3c";
                    else if (t <= tSat)
                        state.subRegion = "3s";
                    else
                        state.subRegion = "3t";
                }
                else
                {
                    double tSat = daddy.Theta_4(p);
                    if (t <= tSat)
                        state.subRegion = "3c";
                    else
                        state.subRegion = "3t";
                }
            }


        }

        double T_atRegionBoundary(double p, string boundary)
        {
            // p is pressure in kPa
            // boundary is one of 3ab, 3cd, ...
            // returns the temperature at the boundary in K 

            p /= 1000.0;
            double t = 0.0;       // temperature at the boundary in K

            int[] I3ab = { 0, 0, 1, 2, -1, -2 };
            int[] I3cd = { 0, 0, 1, 2, 3 };
            int[] I3gh = { 0, 0, 1, 2, 3, 4 };
            int[] I3ij = { 0, 0, 1, 2, 3, 4 };
            int[] I3jk = { 0, 0, 1, 2, 3, 4 };
            int[] I3mn = { 0, 0, 1, 2, 3 };
            int[] I3op = { 0, 0, 1, 2, -1, -2 };
            int[] I3qu = { 0, 0, 1, 2, 3 };
            int[] I3rx = { 0, 0, 1, 2, 3 };
            int[] I3uv = { 0, 0, 1, 2, 3 };
            int[] I3wx = { 0, 0, 1, 2, -1, -2 };

            double[] n3ab = {0.0                 ,0.154793642129415e4 ,-0.187661219490113e3,0.213144632222113e2 ,
                            -0.191887498864292e4,0.918419702359447e3};
            double[] n3cd = {0.0                  ,0.585276966696349e3  ,0.278233532206915e1  ,-0.127283549295878e-1,
                            0.159090746562729e-3 };
            double[] n3gh = {0.0                  ,-0.249284240900418e5 ,0.428143584791546e4  ,-0.269029173140130e3 ,
                            0.751608051114157e1  ,-0.787105249910383e-1};
            double[] n3ij = {0.0                  ,0.584814781649163e3  ,-0.616179320924617   ,0.260763050899562    ,
                            -0.587071076864459e-2,0.515308185433082e-4};
            double[] n3jk = {0.0                  ,0.617229772068439e3  ,-0.770600270141675e1 ,0.697072596851896    ,
                            -0.157391839848015e-1,0.137897492684194e-3};
            double[] n3mn = {0.0                 ,0.535339483742384e3 ,0.761978122720128e1 ,-0.158365725441648  ,
                            0.192871054508108e-2 };
            double[] n3op = {0.0                 ,0.969461372400213e3 ,-0.332500170441278e3,0.642859598466067e2 ,
                            0.773845935768222e3 ,-0.152313732937084e4};
            double[] n3qu = {0.0                 ,0.565603648239126e3 ,0.529062258221222e1 ,-0.102020639611016  ,
                            0.122240301070145e-2};
            double[] n3rx = {0.0                  ,0.584561202520006e3  ,-0.102961025163669e1 ,0.243293362700452    ,
                            -0.294905044740799e-2 };
            double[] n3uv = {0.0                 ,0.528199646263062e3 ,0.890579602135307e1 ,-0.222814134903755  ,
                             0.286791682263697e-2};
            double[] n3wx = {0.0                ,0.728052609145380e1,0.973505869861952e2,0.147370491183191e2,
                             0.329196213998375e3,0.873371668682417e3};

            if (boundary == "3ab")
            {
                for (int i = 1; i <= 5; i++)
                    t += n3ab[i] * Math.Pow(Math.Log(p), I3ab[i]);
            }
            else if (boundary == "3op")
            {
                for (int i = 1; i <= 5; i++)
                    t += n3op[i] * Math.Pow(Math.Log(p), I3op[i]);
            }
            else if (boundary == "3ef")
            {
                t = 3.727888004 * (p - 22.064) + 647.096;
            }
            else if (boundary == "3cd")
            {
                for (int i = 1; i <= 4; i++)
                    t += n3cd[i] * Math.Pow(p, I3cd[i]);
            }
            else if (boundary == "3gh")
            {
                for (int i = 1; i <= 5; i++)
                    t += n3gh[i] * Math.Pow(p, I3gh[i]);
            }
            else if (boundary == "3ij")
            {
                for (int i = 1; i <= 5; i++)
                    t += n3ij[i] * Math.Pow(p, I3ij[i]);
            }
            else if (boundary == "3jk")
            {
                for (int i = 1; i <= 5; i++)
                    t += n3jk[i] * Math.Pow(p, I3jk[i]);
            }
            else if (boundary == "3mn")
            {
                for (int i = 1; i <= 4; i++)
                    t += n3mn[i] * Math.Pow(p, I3mn[i]);
            }
            else if (boundary == "3qu")
            {
                for (int i = 1; i <= 4; i++)
                    t += n3qu[i] * Math.Pow(p, I3qu[i]);
            }
            else if (boundary == "3rx")
            {
                for (int i = 1; i <= 4; i++)
                    t += n3rx[i] * Math.Pow(p, I3rx[i]);
            }
            else if (boundary == "3uv")
            {
                for (int i = 1; i <= 4; i++)
                    t += n3uv[i] * Math.Pow(p, I3uv[i]);
            }
            else if (boundary == "3wx")
            {
                for (int i = 1; i <= 5; i++)
                    t += n3wx[i] * Math.Pow(Math.Log(p), I3wx[i]);
            }

            return t;
        }

        void CheckState(double r, double t)
        {
            if (r != state.r || t != state.t)
            {
                ClearState();
                state.r = r;
                state.t = t;
            }
        }

        public void ClearState()
        {
            state.r = double.MinValue;
            state.t = double.MinValue;
            state.phase = 0;
            state.p = double.MinValue;
            state.Phi = double.MinValue;
            state.Phi_r = double.MinValue;
            state.Phi_rr = double.MinValue;
            state.Phi_tt = double.MinValue;
            state.Phi_t = double.MinValue;
            state.Phi_rt = double.MinValue;
        }

        // backward equations for (p, h)

        public double Tph(double p, double h)
        {
            // p is pressure in kPa
            // h is enthalpy in kJ/kg
            // returns temperature in K 


            double[] n3a3b = {  0.0                  , 0.201464004206875e4  , 0.374696550136983e1  ,
                               -0.219921901054187e-1 , 0.875131686009950e-4};


            if (p > 22.0640)
            {
                double pMpa = p / 1000.0;
                double h3ab = n3a3b[1] + ((n3a3b[2] + (n3a3b[3] + n3a3b[4] * pMpa) * pMpa)) * pMpa;
                if (h > h3ab)
                    return Tph_3b(p, h);
                else
                    return Tph_3a(p, h);
            }
            else
            {
                double tsat = daddy.Theta_4(p);
                double hf = H(p, tsat, 1);
                double hg = H(p, tsat, 2);

                if (h <= hf)
                    return Tph_3a(p, h);
                else if (h >= hg)
                    return Tph_3b(p, h);
                else
                    return tsat;
            }
        }

        public double Tps(double p, double s)
        {
            // p is pressure in kPa
            // s is entropy in kJ/kg K
            // returns temperature in K 


            if (p > 22.0640)
            {
                if (s < 4.41202148223476)
                    return Tps_3a(p, s);
                else
                    return Tps_3b(p, s);
            }
            else
            {
                double tsat = daddy.Theta_4(p);
                double sf = S(p, tsat, 1);
                double sg = S(p, tsat, 2);

                if (s <= sf)
                    return Tps_3a(p, s);
                else if (s >= sg)
                    return Tph_3b(p, s);
                else
                    return tsat;
            }
        }

        double Tph_3a(double p, double h)
        {
            // p is pressure in kPa
            // h is enthalpy in kJ/kg
            // returns temperature in K region 3a

            p /= 100000.0;
            h /= 2300.0;
            double t = 0.0;

            double[] Ih = {  0,-12,-12,-12,-12,-12,-12,-12,-12,-10,
                           -10,-10, -8, -8, -8, -8, -5, -3, -2, -2,
                            -2, -1, -1,  0,  0,  1,  3,  3,  4,  4,
                            10, 12 };

            double[] Jh = { 0, 0, 1, 2, 6,14,16,20,22, 1,
                            5,12, 0, 2, 4,10, 2, 0, 1, 3,
                            4, 0, 2, 0, 1, 1, 0, 1, 0, 3,
                            4, 5};

            double[] nh = { 0.0                 ,-0.133645667811215e-6, 0.455912656802978e-5,-0.146294640700979e-4,
                            0.639341312970080e-2, 0.372783927268847e3 ,-0.718654377460447e4 , 0.573494752103400e6 ,
                           -0.267569329111439e7 ,-0.334066283302614e-4,-0.245479214069597e-1, 0.478087847764996e2 ,
                            0.764664131818904e-5, 0.128350627676972e-2, 0.171219081377331e-1,-0.851007304583213e1 ,
                           -0.136513461629781e-1,-0.384460997596657e-5, 0.337423807911655e-2,-0.551624873066791   ,
                            0.729202277107470   ,-0.992522757376041e-2,-0.119308831407288   , 0.793929190615421   ,
                            0.454270731799386   , 0.209998591259910   ,-0.642109823904738e-2,-0.235155868604540e-1,
                            0.252233108341612e-2,-0.764885133368119e-2, 0.136176427574291e-1,-0.133027883575669e-1};

            for (int i = 1; i <= 31; i++)
                t += nh[i] * Math.Pow(p + 0.240, Ih[i]) * Math.Pow(h - 0.615, Jh[i]);

            return t * 760.0;
        }

        double Tph_3b(double p, double h)
        {
            // p is pressure in kPa
            // h is enthalpy in kJ/kg
            // returns temperature in K region 3b

            p /= 100000.0;
            h /= 2800.0;
            double t = 0.0;

            double[] Ih = {  0,-12,-12,-10,-10,-10,-10,-10, -8, -8,
                            -8, -8, -8, -6, -6, -6, -4, -4, -3, -2,
                            -2, -1, -1, -1, -1, -1, -1,  0,  0,  1,
                             3,  5,  6,  8};

            double[] Jh = { 0, 0, 1, 0, 1, 5,10,12, 0, 1,
                            2, 4,10, 0, 1, 2, 0, 1, 5, 0,
                            4, 2, 4, 6,10,14,16, 0, 2, 1,
                            1, 1, 1, 1 };

            double[] nh = { 0.0                 , 0.323254573644920e-4,-0.127575556587181e-3,-0.475851877356068e-3,
                            0.156183014181602e-2, 0.105724860113781   ,-0.858514221132534e2 , 0.724140095480911e3 ,
                            0.296475810273257e-2,-0.592721983365988e-2,-0.126305422818666e-1,-0.115716196364853   ,
                            0.849000969739595e2 ,-0.108602260086615e-1, 0.154304475328851e-1, 0.750455441524466e-1,
                            0.252520973612982e-1,-0.602507901232996e-1,-0.307622221350501e1 ,-0.574011959864879e-1,
                            0.503471360939849e1 ,-0.925081888584834   , 0.391733882917546e1 ,-0.773146007130190e2 ,
                            0.949308762098587e4 ,-0.141043719679409e7 , 0.849166230819026e7 , 0.861095729446704   ,
                            0.323346442811720   , 0.873281936020439   ,-0.436653048526683   , 0.286596714529479   ,
                           -0.131778331276228   , 0.676682064330275e-2};

            for (int i = 1; i <= 33; i++)
                t += nh[i] * Math.Pow(p + 0.298, Ih[i]) * Math.Pow(h - 0.720, Jh[i]);

            return t * 860.0;
        }

        double Tps_3a(double p, double s)
        {
            // p is pressure in kPa
            // s is entropy in kJ/kg-K
            // returns temperature in K region 3a

            p /= 100000.0;
            s /= 4.4;
            double t = 0.0;

            double[] Is = {  0,-12,-12,-10,-10,-10,-10, -8, -8, -8,
                            -8, -6, -6, -6, -5, -5, -5, -4, -4, -4,
                            -2, -2, -1, -1,  0,  0,  0,  1,  2,  2,
                             3,  8,  8, 10 };

            double[] Js = { 0,28,32, 4,10,12,14, 5, 7, 8,
                           28, 2, 6,32, 0,14,32, 6,10,36,
                            1, 4, 1, 6, 0, 1, 4, 0, 0, 3,
                            2, 0, 1, 2 };

            double[] ns = { 0.0                 , 0.150042008263875e10,-0.159397258480424e12, 0.502181140217975e-3,
                           -0.672057767855466e2 , 0.145058545404456e4 ,-0.823889534888890e4 ,-0.154852214233853   ,
                            0.112305046746695e2 ,-0.297000213482822e2 , 0.438565132635495e11, 0.137837838635464e-2,
                           -0.297478527157462e1 , 0.971777947349413e13,-0.571527767052398e-4, 0.288307949778420e5 ,
                           -0.744428289262703e14, 0.128017324848921e2 ,-0.368275545889071e3 , 0.664768904779177e16,
                            0.449359251958880e-1,-0.422897836099655e1 ,-0.240614376434179   ,-0.474341365254924e1 ,
                            0.724093999126110   , 0.923874349695897   , 0.399043655281015e1 , 0.384066651868009e-1,
                           -0.359344365571848e-2,-0.735196448821653   , 0.188367048396131   , 0.141064266818704e-3,
                           -0.257418501496337e-2, 0.123220024851555e-2 };

            for (int i = 1; i <= 33; i++)
                t += ns[i] * Math.Pow(p + 0.240, Is[i]) * Math.Pow(s - 0.703, Js[i]);

            return t * 760.0;
        }

        double Tps_3b(double p, double s)
        {
            // p is pressure in kPa
            // s is entropy in kJ/kg-K
            // returns temperature in K region 3b

            p /= 100000.0;
            s /= 5.3;
            double t = 0.0;

            double[] Is = {  0,-12,-12,-12,-12, -8, -8, -8, -6, -6,
                            -6, -5, -5, -5, -5, -5, -4, -3, -3, -2,
                             0,  2,  3,  4,  5,  6,  8, 12, 14  };

            double[] Js = { 0, 1, 3, 4, 7, 0, 1, 3, 0, 2,
                            4, 0, 1, 2, 4, 6,12, 1, 6, 2,
                            0, 1, 1, 0,24, 0, 3, 1, 2  };

            double[] ns = { 0.0                 , 0.527111701601660   ,-0.401317830052742e2 , 0.153020073134484e3 ,
                           -0.224799398218827e4 ,-0.193993484669048   ,-0.140467557893768e1 , 0.426799878114024e2 ,
                            0.752810643416743   , 0.226657238616417e2 ,-0.622873556909932e3 ,-0.660823667935396   ,
                            0.841267087271658   ,-0.253717501764397e2 , 0.485708963532948e3 , 0.880531517490555e3 ,
                            0.265015592794626e7 ,-0.359287150025783   ,-0.656991567673753e3 , 0.241768149185367e1 ,
                            0.856873461222588   , 0.655143675313458   ,-0.213535213206406   , 0.562974957606348e-2,
                           -0.316955725450471e15,-0.699997000152457e-3, 0.119845803210767e-1, 0.193848122022095e-4,
                           -0.215095749182309e-4 };


            for (int i = 1; i <= 28; i++)
                t += ns[i] * Math.Pow(p + 0.760, Is[i]) * Math.Pow(s - 0.818, Js[i]);

            return t * 860.0;
        }

    }
}
