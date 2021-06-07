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
    public class REGION1
    {

        struct STATE
        {
            public double p;
            public double t;
            public double G1;
            public double G1_p;
            public double G1_t;
            public double G1_pp;
            public double G1_tt;
            public double G1_pt;
        }

        STATE state;

        // region 1 constants

        double pStar = 16530.0;     // kPa
        double tStar = 1386.0;      // K

        double[] I  = {0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,0  ,1 ,
                       1  ,1  ,1  ,1  ,1  ,2  ,2  ,2  ,2  ,2 ,
                       3  ,3  ,3  ,4  ,4  ,4  ,5  ,8  ,8  ,21,
                       23 ,29 ,30 ,31 ,32 };

        double[] J  = { 0 ,-2 ,-1 , 0 , 1 , 2 , 3 , 4 , 5 ,-9 ,
                       -7 ,-1 , 0 , 1 , 3 ,-3 , 0 , 1 , 3 ,17 ,
                       -4 , 0 , 6 ,-5 ,-2 , 10,-8 ,-11,-6 ,-29,
                       -31,-38,-39,-40,-41};

        double[] n  = {  0.0                  ,  0.14632971213167     , -0.84548187169114     , -0.37563603672040e1   , 
                         0.33855169168385e1   , -0.95791963387872     ,  0.15772038513228     , -0.16616417199501e-1  , 
                         0.81214629983568e-3  ,  0.28319080123804e-3  , -0.60706301565874e-3  , -0.18990068218419e-1  , 
                        -0.32529748770505e-1  , -0.21841717175414e-1  , -0.52838357969930e-4  , -0.47184321073267e-3  , 
                        -0.30001780793026e-3  ,  0.47661393906987e-4  , -0.44141845330846e-5  , -0.72694996297594e-15 , 
                        -0.31679644845054e-4  , -0.28270797985312e-5  , -0.85205128120103e-9  , -0.22425281908000e-5  , 
                        -0.65171222895601e-6  , -0.14341729937924e-12 , -0.40516996860117e-6  , -0.12734301741641e-8  , 
                        -0.17424871230634e-9  , -0.68762131295531e-18 ,  0.14478307828521e-19 ,  0.26335781662795e-22 , 
                        -0.11947622640071e-22 ,  0.18228094581404e-23 , -0.93537087292458e-25  };



        // properties in Region 1

        public double V(double p, double t)
        {
            // p is pressure (kPa)
            // t is temperature (K)
            // returns reduced specific volume, v*p/RT

            p /= pStar;
            t = tStar / t;
            CheckState(p, t);

            double v = p * G1_p(p, t);
            return v;
        }

        public double H(double p, double t)
        {
            // p is pressure (kPa)
            // t is temperature (K)
            // returns reduced specific enthalpy, h/RT

            p /= pStar;
            t = tStar / t;
            CheckState(p, t);

            double h = t * G1_t(p, t);
            return h;
        }

        public double S(double p, double t)
        {
            // p is pressure (kPa)
            // t is temperature (K)
            // returns reduced specific entropy, s/R

            p /= pStar;
            t = tStar / t;
            CheckState(p, t);

            double s = t * G1_t(p, t) - G1(p, t);
            return s;
        }

        public double CP(double p, double t)
        {
            // p is pressure (kPa)
            // t is temperature (K)
            // returns reduced isobaric heat capacity, cP/R

            p /= pStar;
            t = tStar / t;
            CheckState(p, t);

            double cp = -t * t * G1_tt(p, t);
            return cp;
        }

        public double CV(double p, double t)
        {
            // p is pressure (kPa)
            // t is temperature (K)
            // returns reduced ischoric heat capacity, cV/R

            p /= pStar;
            t = tStar / t;
            CheckState(p, t);

            double cv = -t * t * G1_tt(p, t) + Math.Pow(G1_p(p, t) - t * G1_pt(p, t), 2) / G1_pp(p, t);
            return cv;
        }

        // Gibbs Function and its derivatives

        double G1(double p, double t)
        {
            if (state.G1 != double.MinValue)
                return state.G1;

            double g = 0.0;

            for (int i = 1; i <= 34; i++)
            {
                g += n[i] * Math.Pow(7.1 - p, I[i]) * Math.Pow(t - 1.222, J[i]);
            }
            state.G1 = g;
            return g;
        }

        double G1_p(double p, double t)
        {
            if (state.G1_p != double.MinValue)
                return state.G1_p;

            double g = 0.0;

            for (int i = 1; i <= 34; i++)
            {
                g += -n[i] * I[i] * Math.Pow(7.1 - p, I[i] - 1) * Math.Pow(t - 1.222, J[i]);
            }
            state.G1_p = g;
            return g;
        }

        double G1_t(double p, double t)
        {
            if (state.G1_t != double.MinValue)
                return state.G1_t;

            double g = 0.0;

            for (int i = 1; i <= 34; i++)
            {
                g += n[i] * J[i] * Math.Pow(7.1 - p, I[i]) * Math.Pow(t - 1.222, J[i] - 1);
            }
            state.G1_t = g;
            return g;
        }

        double G1_pp(double p, double t)
        {
            if (state.G1_pp != double.MinValue)
                return state.G1_pp;

            double g = 0.0;

            for (int i = 1; i <= 34; i++)
            {
                g += n[i] * I[i] * (I[i] - 1.0) * Math.Pow(7.1 - p, I[i] - 2) * Math.Pow(t - 1.222, J[i]);
            }
            state.G1_pp = g;
            return g;
        }

        double G1_tt(double p, double t)
        {
            if (state.G1_tt != double.MinValue)
                return state.G1_tt;

            double g = 0.0;

            for (int i = 1; i <= 34; i++)
            {
                g += n[i] * J[i] * (J[i] - 1.0) * Math.Pow(7.1 - p, I[i]) * Math.Pow(t - 1.222, J[i] - 2);
            }
            state.G1_tt = g;
            return g;
        }

        double G1_pt(double p, double t)
        {
            if (state.G1_pt != double.MinValue)
                return state.G1_pt;

            double g = 0.0;

            for (int i = 1; i <= 34; i++)
            {
                g += -n[i] * I[i] * J[i] * Math.Pow(7.1 - p, I[i] - 1) * Math.Pow(t - 1.222, J[i] - 1);
            }
            state.G1_pt = g;
            return g;
        }

        void CheckState(double p, double t)
        {
            if (p != state.p || t != state.t)
            {
                state.p = p;
                state.t = t;
                state.G1 = double.MinValue;
                state.G1_p = double.MinValue;
                state.G1_pp = double.MinValue;
                state.G1_pt = double.MinValue;
                state.G1_t = double.MinValue;
                state.G1_tt = double.MinValue;
            }
        }


        // backward equations

        public double Tph(double p, double h)
        {
            // p is pressure in kPa
            // j is enthalpy in kJ/kg
            // returns temperature in K


            p /= 1000.0;
            h /= 2500.0;

            double t = 0.0;

            double[] Ih = { 0,0,0,0,0,0,0,1,1,1,
                            1,1,1,1,2,2,3,3,4,5,
                            6 };

            double[] Jh = { 0  ,0  ,1  ,2  ,6  ,22 ,32 ,0  ,1  ,2  ,
                            3  ,4  ,10 ,32 ,10 ,32 ,10 ,32 ,32 ,32 ,
                            32 };

            double[] nh = { 0                    ,-0.23872489924521e3   , 0.40421188637945e3   , 0.11349746881718e3   ,
                           -0.58457616048039e1   ,-0.15285482413140e-3  ,-0.10866707695377e-5  ,-0.13391744872602e2   ,
                            0.43211039183559e2   ,-0.54010067170506e2   , 0.30535892203916e2   ,-0.65964749423638e1   ,
                            0.93965400878363e-2  , 0.11573647505340e-6  ,-0.25858641282073e-4  ,-0.40644363084799e-8  ,
                            0.66456186191635e-7  , 0.80670734103027e-10 ,-0.93477771213947e-12 , 0.58265442020601e-14 ,
                           -0.15020185953503e-16 };


            for (int i = 1; i <= 20; i++)
                t += nh[i] * Math.Pow(p, Ih[i]) * Math.Pow(h + 1.0, Jh[i]);

            return t;

        }

        public double Tps(double p, double s)
        {
            // p is pressure in kPa
            // s is entropy in kJ/kg-K
            // return temperature in K

            p /= 1000.0;
            double t =0.0;

            double[] Is = {0,0,0,0,0,0,0,1,1,1,
                           1,1,1,2,2,2,2,2,3,3,
                           4};

            double[] Js = {0 ,0 ,1 ,2 ,3 ,11,31,0 ,1 ,2 ,
                           3 ,12,31,0 ,1 ,2 ,9 ,31,10,32,
                           32 };

            double[] ns = { 0.0                 , 0.17478268058307e3  , 0.34806930892873e2  , 0.65292584978455e1  ,
                             0.33039981775489    ,-0.19281382923196e-6 ,-0.24909197244573e-22,-0.26107636489332    ,
                             0.22592965981586    ,-0.64256463395226e-1 , 0.78876289270526e-2 , 0.35672110607366e-9 ,
                             0.17332496994895e-23, 0.56608900654837e-3 ,-0.32635483139717e-3 , 0.44778286690632e-4 ,
                            -0.51322156908507e-9 ,-0.42522657042207e-25, 0.26400441360689e-12, 0.78124600459723e-28,
                            -0.30732199903668e-30};


            for (int i = 1; i <= 20; i++)
                t += ns[i] * Math.Pow(p, Is[i]) * Math.Pow(s + 2.0, Js[i]);

            return t;

        }

    }
}
