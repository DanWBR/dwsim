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
    public class REGION2
    {
        struct STATE
        {
            public double p;
            public double t;

            public double G0;
            public double G0_p;
            public double G0_t;
            public double G0_pp;
            public double G0_tt;
            public double G0_pt;

            public double Gr;
            public double Gr_p;
            public double Gr_t;
            public double Gr_pp;
            public double Gr_tt;
            public double Gr_pt;
        }

        STATE state;

        // region 2 constants

        double pStar = 1000.0;     // kPa
        double tStar = 540.0;      // K

        double[] J0 = { 0, 0, 1, -5, -4, -3, -2, -1, 2, 3 };


        double[] n0 = {   0.0                ,-0.96927686500217e1 , 0.10086655968018e2 ,-0.56087911283020e-2,
                         0.71452738081455e-1,-0.40710498223928   , 0.14240819171444e1 ,-0.43839511319450e1 ,
                        -0.28408632460772   , 0.21268463753307e-1 };

        double[] IR = {  0,  1,  1,  1,  1,  1,  2,  2,  2,  2,
                         2,  3,  3,  3,  3,  3,  4,  4,  4,  5,
                         6,  6,  6,  7,  7,  7,  8,  8,  9, 10,
                         10, 10, 16, 16, 18, 20, 20, 20, 21, 22,
                         23, 24, 24, 24};

        double[] JR = { 0, 0, 1, 2, 3, 6, 1, 2, 4, 7,36,
                        0, 1, 3, 6,35, 1, 2, 3, 7, 3,
                       16,35, 0,11,25, 8,36,13, 4,10,
                       14,29,50,57,20,35,48,21,53,39,
                       26,40,58 };

        double[] nR = { 0.0                 ,-0.17731742473213e-2 ,-0.17834862292358e-1 ,-0.45996013696365e-1 ,
                       -0.57581259083432e-1 ,-0.50325278727930e-1 ,-0.33032641670203e-4 ,-0.18948987516315e-3 ,
                       -0.39392777243355e-2 ,-0.43797295650573e-1 ,-0.26674547914087e-4 , 0.20481737692309e-7 ,
                        0.43870667284435e-6 ,-0.32277677238570e-4 ,-0.15033924542148e-2 ,-0.40668253562649e-1 ,
                       -0.78847309559367e-9 , 0.12790717852285e-7 , 0.48225372718507e-6 , 0.22922076337661e-5 ,
                       -0.16714766451061e-10,-0.21171472321355e-2 ,-0.23895741934104e2  ,-0.59059564324270e-17,
                       -0.12621808899101e-5 ,-0.38946842435739e-1 , 0.11256211360459e-10,-0.82311340897998e1  ,
                        0.19809712802088e-7 , 0.10406965210174e-18,-0.10234747095929e-12,-0.10018179379511e-8 ,
                       -0.80882908646985e-10, 0.10693031879409    ,-0.33662250574171    , 0.89185845355421e-24,
                        0.30629316876232e-12,-0.42002467698208e-5 ,-0.59056029685639e-25, 0.37826947613457e-5 ,
                       -0.12768608934681e-14, 0.73087610595061e-28, 0.55414715350778e-16,-0.94369707241210e-6 };


        // properties in Region 2

        public double V(double p, double t)
        {
            // p is pressure (kPa)
            // t is temperature (K)
            // returns reduced specific volume, v*p/RT

            p /= pStar;
            t = tStar / t;

            CheckState(p, t);
            double v = p * (G0_p(p, t) + Gr_p(p,t));
            
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
            double h = t * (G0_t(p, t) + Gr_t(p, t));
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

            double s = t * (G0_t(p, t) + Gr_t(p, t)) - (G0(p, t) + Gr(p, t));
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

            double cp = -t * t * (G0_tt(p, t) + Gr_tt(p, t));
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

            double cv = -t * t * (G0_tt(p, t) + Gr_tt(p, t)) +
                        Math.Pow((1.0 + p * Gr_p(p, t) - t * p * Gr_pt(p, t)), 2) /
                        (1.0 - p * p * Gr_pp(p, t));
            return cv;
        }

        // Ideal gas part of the Gibbs Function and its derivatives

        double G0(double p, double t)
        {
            if (state.G0 != double.MinValue)
                return state.G0;

            double g = Math.Log(p);
            for (int i = 1; i <= 9; i++)
                g += n0[i] * Math.Pow(t, J0[i]);

            state.G0 = g;
            return g;
        }

        double G0_p(double p, double t)
        {
            if (state.G0_p != double.MinValue)
                return state.G0_p;

            double g = 1.0 / p;
            state.G0_p = g;
            return g;
        }

        double G0_t(double p, double t)
        {
            if (state.G0_t != double.MinValue)
                return state.G0_t;

            double g = 0.0;
            for (int i = 1; i <= 9; i++)
                g += n0[i] * J0[i] * Math.Pow(t, J0[i] - 1);

            state.G0_t = g;
            return g;
        }

        double G0_pp(double p, double t)
        {
            if (state.G0_pp != double.MinValue)
                return state.G0_pp;

            double g = -1.0 / (p * p);
            state.G0_pp = g;
            return g;
        }

        double G0_tt(double p, double t)
        {
            if (state.G0_tt != double.MinValue)
                return state.G0_tt;

            double g = 0.0;
            for (int i = 1; i <= 9; i++)
                g += n0[i] * J0[i] * (J0[i] - 1.0) * Math.Pow(t, J0[i] - 2);

            state.G0_tt = g;
            return g;
        }

        double G0_pt(double p, double t)
        {
            state.G0_pt = 0.0;
            return 0.0;
        }

        // Residual part of the Gibbs function and its derivatives

        double Gr(double p, double t)
        {
            if (state.Gr != double.MinValue)
                return state.Gr;

            double g = 0.0;
            for (int i = 1; i <= 43; i++)
                g += nR[i] * Math.Pow(p, IR[i]) * Math.Pow(t - 0.5, JR[i]);

            state.Gr = g;
            return g;
        }

        double Gr_p(double p, double t)
        {
            if (state.Gr_p != double.MinValue)
                return state.Gr_p;

            double g = 0.0;
            for (int i = 1; i <= 43; i++)
                g += nR[i] * IR[i] * Math.Pow(p, IR[i] - 1) * Math.Pow(t - 0.5, JR[i]);

            state.Gr_p = g;
            return g;
        }

        double Gr_t(double p, double t)
        {
            if (state.Gr_t != double.MinValue)
                return state.Gr_t;

            double g = 0.0;
            for (int i = 1; i <= 43; i++)
                g += nR[i] * JR[i]* Math.Pow(p, IR[i]) * Math.Pow(t - 0.5, JR[i]-1);

            state.Gr_t = g;
            return g;
        }

        double Gr_pp(double p, double t)
        {
            if (state.Gr_pp != double.MinValue)
                return state.Gr_pp;

            double g = 0.0;
            for (int i = 1; i <= 43; i++)
                g += nR[i] * IR[i] * (IR[i] - 1.0) * Math.Pow(p, IR[i] - 2) * Math.Pow(t - 0.5, JR[i]);

            state.Gr_pp = g;
            return g;
        }

        double Gr_tt(double p, double t)
        {
            if (state.Gr_tt != double.MinValue)
                return state.Gr_tt;

            double g = 0.0;
            for (int i = 1; i <= 43; i++)
                g += nR[i] * JR[i] * (JR[i] - 1.0) * Math.Pow(p, IR[i]) * Math.Pow(t - 0.5, JR[i]-2);

            state.Gr_tt = g;
            return g;
        }

        double Gr_pt(double p, double t)
        {
            if (state.Gr_pt != double.MinValue)
                return state.Gr_pt;

            double g = 0.0;
            for (int i = 1; i <= 43; i++)
                g += nR[i] * IR[i] * JR[i] * Math.Pow(p, IR[i]-1) * Math.Pow(t - 0.5, JR[i] - 1);

            state.Gr_pt = g;
            return g;
        }



        void CheckState(double p, double t)
        {
            if (p != state.p || t != state.t)
            {
                state.p = p;
                state.t = t;
                state.G0 = double.MinValue;
                state.G0_p = double.MinValue;
                state.G0_pp = double.MinValue;
                state.G0_pt = double.MinValue;
                state.G0_t = double.MinValue;
                state.G0_tt = double.MinValue;
                state.Gr = double.MinValue;
                state.Gr_p = double.MinValue;
                state.Gr_pp = double.MinValue;
                state.Gr_pt = double.MinValue;
                state.Gr_t = double.MinValue;
                state.Gr_tt = double.MinValue;
            }
        }

        // backward equations for (p, h)

        public double Tph(double p, double h)
        {
            // p is pressure in kPa
            // h is enthalpy in kJ/kg
            // returns temperature in K 

            double[] n2a2b = { 0.0                , 0.90584278514723e3 ,-0.67955786399241   ,
                               0.12809002730136e-3, 0.26526571908428e4 , 0.45257578905948e1};

            if (p < 4000.0)
                return Tph_2a(p, h);
            else
            {
                double ps = p / 1000.0;
                double eta2b = n2a2b[4] + Math.Pow((ps - n2a2b[5]) / n2a2b[3],0.5);
                if (h < eta2b)
                    return Tph_2c(p, h);
                else
                    return Tph_2b(p, h);
            }

        }

        public double Tps(double p, double s)
        {
            // p is pressure in kPa
            // s is entropy in kJ/kg K
            // returns temperature in K 

            double ps = p / 1000.0;

            if (p < 4000.0)
                return Tps_2a(p, s);
            else if (s > 5.85)
                return Tps_2b(p, s);
            else
                return Tps_2c(p, s);
        }


        double Tph_2a(double p, double h)
        {
            // p is pressure in kPa
            // h is enthalpy in kJ/kg
            // returns temperature in K region 2a

            p /= 1000.0;
            h /= 2000.0;
            double t = 0.0;

            double[] Ih = {0,0,0,0,0,0,0,1,1,1,
                           1,1,1,1,1,1,2,2,2,2,
                           2,2,2,2,3,3,4,4,4,5,
                           5,5,6,6,7};

            double[] Jh = {0 ,0 ,1 ,2 ,3 ,7 ,20,0 ,1 ,2 ,
                           3 ,7 ,9 ,11,18,44,0 ,2 ,7 ,36,
                          38,40,42,44,24,44,12,32,44,32,
                          36,42,34,44,28};

            double[] nh = { 0.0                , 0.10898952318288e4 , 0.84951654495535e3 ,-0.10781748091826e3 ,
                            0.33153654801263e2 ,-0.74232016790248e1 , 0.11765048724356e2 , 0.18445749355790e1 ,
                           -0.41792700549624e1 , 0.62478196935812e1 ,-0.17344563108114e2 ,-0.20058176862096e3 ,
                            0.27196065473796e3 ,-0.45511318285818e3 , 0.30919688604755e4 , 0.25226640357872e6 ,
                           -0.61707422868339e-2,-0.31078046629583   , 0.11670873077107e2 , 0.12812798404046e9 ,
                           -0.98554909623276e9 , 0.28224546973002e10,-0.35948971410703e10, 0.17227349913197e10,
                           -0.13551334240775e5 , 0.12848734664650e8 , 0.13865724283226e1 , 0.23598832556514e6 ,
                           -0.13105236545054e8 , 0.73999835474766e4 ,-0.55196697030060e6 , 0.37154085996233e7 ,
                            0.19127729239660e5 ,-0.41535164835634e6 ,-0.62459855192507e2};

            for (int i = 1; i <= 34; i++)
                t += nh[i] * Math.Pow(p, Ih[i]) * Math.Pow(h - 2.1, Jh[i]);

            return t;
        }

        double Tph_2b(double p, double h)
        {
            // p is pressure in kPa
            // h is enthalpy in kJ/kg
            // returns temperature in K region 2b

            p /= 1000.0;
            h /= 2000.0;

            double t = 0.0;

            double[] Ih = {0,0,0,0,0,0,0,0,0,1,
                           1,1,1,1,1,1,1,2,2,2,
                           2,3,3,3,3,4,4,4,4,4,
                           4,5,5,5,6,7,7,9,9};

            double[] Jh = {0 ,0 ,1 ,2 ,12,18,24,28,40,0 ,
                           2 ,6 ,12,18,24,28,40,2 ,8 ,18,
                          40,1 ,2 ,12,24,2 ,12,18,24,28,
                          40,18,24,40,28,2 ,28,1 ,40};

            double[] nh = { 0.0                 , 0.14895041079516e4  , 0.74307798314034e3  ,-0.97708318797837e2  ,
                            0.24742464705674e1  ,-0.63281320016026    , 0.11385952129658e1  ,-0.47811863648625    ,
                            0.85208123431544e-2 , 0.93747147377932    , 0.33593118604916e1  , 0.33809355601454e1  ,
                            0.16844539671904    , 0.73875745236695    ,-0.47128737436186    , 0.15020273139707    ,
                           -0.21764114219750e-2 ,-0.21810755324761e-1 ,-0.10829784403677    ,-0.46333324635812e-1 ,
                            0.71280351959551e-4 , 0.11032831789999e-3 , 0.18955248387902e-3 , 0.30891541160537e-2 ,
                            0.13555504554949e-2 , 0.28640237477456e-6 ,-0.10779857357512e-4 ,-0.76462712454814e-4 ,
                            0.14052392818316e-4 ,-0.31083814331434e-4 ,-0.10302738212103e-5 , 0.28217281635040e-6 ,
                            0.12704902271945e-5 , 0.73803353468292e-7 ,-0.11030139238909e-7 ,-0.81456365207833e-13,
                           -0.25180545682962e-10,-0.17565233969407e-17, 0.86934156344163e-14};

            for (int i = 1; i <= 38; i++)
                t += nh[i] * Math.Pow(p - 2.0, Ih[i]) * Math.Pow(h - 2.6, Jh[i]);

            return t;
        }
        double Tph_2c(double p, double h)
        {
            // p is pressure in kPa
            // h is enthalpy in kJ/kg
            // returns temperature in K region 2c

            p /= 1000.0;
            h /= 2000.0;
            double t = 0.0;

            double[] Ih = { 0,-7,-7,-6,-6,-5,-5,-2,-2,-1,
                           -1, 0, 0, 1, 1, 2, 6, 6, 6, 6,
                            6, 6, 6, 6};

            double[] Jh = {0 ,0 ,4 ,0 ,2 ,0 ,2 ,0 ,1 ,0 ,
                           2 ,0 ,1 ,4 ,8 ,4 ,0 ,1 ,4 ,10,
                          12,16,20,22};

            double[] nh = { 0.0                 ,-0.32368398555242e13 , 0.73263350902181e13 , 0.35825089945447e12 ,
                           -0.58340131851590e12 ,-0.10783068217470e11 , 0.20825544563171e11 , 0.61074783564516e6  ,
                            0.85977722535580e6  ,-0.25745723604170e5  , 0.31081088422714e5  , 0.12082315865936e4  ,
                            0.48219755109255e3  , 0.37966001272486e1  ,-0.10842984880077e2  ,-0.45364172676660e-1 ,
                            0.14559115658698e-12, 0.11261597407230e-11,-0.17804982240686e-10, 0.12324579690832e-6 ,
                           -0.11606921130984e-5 , 0.27846367088554e-4 ,-0.59270038474176e-3 , 0.12918582991878e-2};

            for (int i = 1; i <= 23; i++)
                t += nh[i] * Math.Pow(p + 25.0, Ih[i]) * Math.Pow(h - 1.8, Jh[i]);

            return t;
        }

        double Tps_2a(double p, double s)
        {
            // p is pressure in kPa
            // s is entropy in kJ/kg-K
            // returns temperature in K region 2a

            p /= 1000.0;
            s /= 2.0;
            double t = 0.0;

            double[] Is = {  0.0 ,-1.5 ,-1.5 ,-1.5 ,-1.5 ,-1.5 ,-1.5 ,-1.25,-1.25,-1.25,
                            -1.0 ,-1.0 ,-1.0 ,-1.0 ,-1.0 ,-1.0 ,-0.75,-0.75,-0.5 ,-0.5 ,
                            -0.5 ,-0.5 ,-0.25,-0.25,-0.25,-0.25, 0.25, 0.25, 0.25, 0.25,
                             0.5 , 0.5 , 0.5 , 0.5 , 0.5 , 0.5 , 0.5 , 0.75, 0.75, 0.75,
                             0.75, 1.0 , 1.0 , 1.25, 1.25, 1.5 , 1.5 };

            double[] Js = {   0,-24,-23,-19,-13,-11,-10,-19,-15, -6,
                            -26,-21,-17,-16, -9, -8,-15,-14,-26,-13,
                             -9, -7,-27,-25,-11, -6,  1,  4,  8, 11,
                              0,  1,  5,  6, 10, 14, 16,  0,  4,  9,
                             17,  7, 18,  3, 15,  5, 18};

            double[] ns = {  0.0                ,-0.39235983861984e6 , 0.51526573827270e6 , 0.40482443161048e5 ,
                            -0.32193790923902e3 , 0.96961424218694e2 ,-0.22867846371773e2 ,-0.44942914124357e6 ,
                            -0.50118336020166e4 , 0.35684463560015   , 0.44235335848190e5 ,-0.13673388811708e5 ,
                             0.42163260207864e6 , 0.22516925837475e5 , 0.47442144865646e3 ,-0.14931130797647e3 ,
                            -0.19781126320452e6 ,-0.23554399470760e5 ,-0.19070616302076e5 , 0.55375669883164e5 ,
                             0.38293691437363e4 ,-0.60391860580567e3 , 0.19363102620331e4 , 0.42660643698610e4 ,
                            -0.59780638872718e4 ,-0.70401463926862e3 , 0.33836784107553e3 , 0.20862786635187e2 ,
                             0.33834172656196e-1,-0.43124428414893e-4, 0.16653791356412e3 ,-0.13986292055898e3 ,
                            -0.78849547999872   , 0.72132411753872e-1,-0.59754839398283e-2,-0.12141358953904e-4,
                             0.23227096733871e-6,-0.10538463566194e2 , 0.20718925496502e1 ,-0.72193155260427e-1,
                             0.20749887081120e-6,-0.18340657911379e-1, 0.29036272348696e-6, 0.21037527893619   ,
                             0.25681239729999e-3,-0.12799002933781e-1,-0.82198102652018e-5  };


            for (int i = 1; i <= 46; i++)
                t += ns[i] * Math.Pow(p, Is[i]) * Math.Pow(s - 2.0, Js[i]);

            return t;
        }
        double Tps_2b(double p, double s)
        {
            // p is pressure in kPa
            // s is entropy in kJ/kg-K
            // returns temperature in K region 2b

            p /= 1000.0;
            s /= 0.7853;
            double t = 0.0;

            double[] Is = {  0,-6,-6,-5,-5,-4,-4,-4,-3,-3,
                            -3,-3,-2,-2,-2,-2,-1,-1,-1,-1,
                            -1, 0, 0, 0, 0, 0, 0, 0, 1, 1,
                             1, 1, 1, 1, 2, 2, 2, 3, 3, 3,
                             4, 4, 5, 5, 5};

            double[] Js = {  0, 0,11, 0,11, 0, 1,11, 0, 1,
                            11,12, 0, 1, 6,10, 0, 1, 5, 8,
                             9, 0, 1, 2, 4, 5, 6, 9, 0, 1,
                             2, 3, 7, 8, 0, 1, 5, 0, 1, 3,
                             0, 1, 0, 1, 2};

            double[] ns = {  0.0                , 0.31687665083497e6 , 0.20864175881858e2 ,-0.39859399803599e6 ,
                            -0.21816058518877e2 , 0.22369785194242e6 ,-0.27841703445817e4 , 0.99207436071480e1 ,
                            -0.75197512299157e5 , 0.29708605951158e4 ,-0.34406878548526e1 , 0.38815564249115   ,
                             0.17511295085750e5 ,-0.14237112854449e4 , 0.10943803364167e1 , 0.89971619308495   ,
                            -0.33759740098958e4 , 0.47162885818355e3 ,-0.19188241993679e1 , 0.41078580492196   ,
                            -0.33465378172097   , 0.13870034777505e4 ,-0.40663326195838e3 , 0.41727347159610e2 ,
                             0.21932549434532e1 ,-0.10320050009077e1 , 0.35882943516703   , 0.52511453726066e-2,
                             0.12838916450705e2 ,-0.28642437219381e1 , 0.56912683664855   ,-0.99962954584931e-1,
                            -0.32632037778459e-2, 0.23320922576723e-3,-0.15334809857450   , 0.29072288239902e-1,
                             0.37534702741167e-3, 0.17296691702411e-2,-0.38556050844504e-3,-0.35017712292608e-4,
                            -0.14566393631492e-4, 0.56420857267269e-5, 0.41286150074605e-7,-0.20684671118824e-7,
                             0.16409393674725e-8 };

            for (int i = 1; i <= 44; i++)
                t += ns[i] * Math.Pow(p, Is[i]) * Math.Pow(10.0 - s, Js[i]);

            return t;
        }

        double Tps_2c(double p, double s)
        {
            // p is pressure in kPa
            // s is entropy in kJ/kg-K
            // returns temperature in K region 2c

            p /= 1000.0;
            s /= 2.9251;
            double t = 0.0;

            double[] Is = {  0,-2,-2,-1, 0, 0, 0, 0, 1, 1,
                             1, 1, 2, 2, 2, 3, 3, 3, 4, 4,
                             4, 5, 5, 5, 6, 6, 7, 7, 7, 7,
                             7};

            double[] Js = { 0,0,1,0,0,1,2,3,0,1,
                            3,4,0,1,2,0,1,5,0,1,
                            4,0,1,2,0,1,0,1,3,4,
                            5};

            double[] ns = {  0.0                 , 0.90968501005365e3  , 0.24045667088420e4  ,-0.59162326387130e3  ,
                             0.54145404128074e3  ,-0.27098308411192e3  , 0.97976525097926e3  ,-0.46966772959435e3  ,
                             0.14399274604723e2  ,-0.19104204230429e2  , 0.53299167111971e1  ,-0.21252975375934e2  ,
                            -0.31147334413760    , 0.60334840894623    ,-0.42764839702509e-1 , 0.58185597255259e-2 ,
                            -0.14597008284753e-1 , 0.56631175631027e-2 ,-0.76155864584577e-4 , 0.22440342919332e-3 ,
                            -0.12561095013413e-4 , 0.63323132660934e-6 ,-0.20541989675375e-5 , 0.36405370390082e-7 ,
                            -0.29759897789215e-8 , 0.10136618529763e-7 , 0.59925719692351e-11,-0.20677870105164e-10,
                            -0.20874278181886e-10, 0.10162166825089e-9 ,-0.16429828281347e-9 };


            for (int i = 1; i <= 30; i++)
                t += ns[i] * Math.Pow(p, Is[i]) * Math.Pow(2.0 - s, Js[i]);

            return t;
        }

    }
}
