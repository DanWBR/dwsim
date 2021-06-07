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
    public class REGION5
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

        // region 5 constants

        double pStar = 1000.0;     // kPa
        double tStar = 1000.0;      // K

        double[] J0 = { 0, 0, 1, -3, -2, -1, 2 };
        double[] n0 = {0.0                ,-0.13179983674201e2 , 0.68540841634434e1 ,-0.24805148933466e-1,
                       0.36901534980333   ,-0.31161318213925e1 ,-0.32961626538917};

        double[] IR = { 0, 1, 1, 1, 2, 2, 3 };
        double[] JR = { 0, 1, 2, 3, 3, 9, 7 };

        double[] nR = { 0.0                , 0.15736404855259e-2, 0.90153761673944e-3,-0.50270077677648e-2,
                        0.22440037409485e-5,-0.41163275453471e-5, 0.37919454822955e-7};


        // properties in Region 5

        public double V(double p, double t)
        {
            // p is pressure (kPa)
            // t is temperature (K)
            // returns reduced specific volume, v*p/RT

            p /= pStar;
            t = tStar / t;

            CheckState(p, t);
            double v = p * (G0_p(p, t) + Gr_p(p, t));

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
            for (int i = 1; i <= 6; i++)
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
            for (int i = 1; i <= 6; i++)
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
            for (int i = 1; i <= 6; i++)
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
            for (int i = 1; i <= 6; i++)
                g += nR[i] * Math.Pow(p, IR[i]) * Math.Pow(t, JR[i]);

            state.Gr = g;
            return g;
        }

        double Gr_p(double p, double t)
        {
            if (state.Gr_p != double.MinValue)
                return state.Gr_p;

            double g = 0.0;
            for (int i = 1; i <= 6; i++)
                g += nR[i] * IR[i] * Math.Pow(p, IR[i] - 1) * Math.Pow(t, JR[i]);

            state.Gr_p = g;
            return g;
        }

        double Gr_t(double p, double t)
        {
            if (state.Gr_t != double.MinValue)
                return state.Gr_t;

            double g = 0.0;
            for (int i = 1; i <= 6; i++)
                g += nR[i] * JR[i] * Math.Pow(p, IR[i]) * Math.Pow(t, JR[i] - 1);

            state.Gr_t = g;
            return g;
        }

        double Gr_pp(double p, double t)
        {
            if (state.Gr_pp != double.MinValue)
                return state.Gr_pp;

            double g = 0.0;
            for (int i = 1; i <= 6; i++)
                g += nR[i] * IR[i] * (IR[i] - 1.0) * Math.Pow(p, IR[i] - 2) * Math.Pow(t, JR[i]);

            state.Gr_pp = g;
            return g;
        }

        double Gr_tt(double p, double t)
        {
            if (state.Gr_tt != double.MinValue)
                return state.Gr_tt;

            double g = 0.0;
            for (int i = 1; i <= 6; i++)
                g += nR[i] * JR[i] * (JR[i] - 1.0) * Math.Pow(p, IR[i]) * Math.Pow(t, JR[i] - 2);

            state.Gr_tt = g;
            return g;
        }

        double Gr_pt(double p, double t)
        {
            if (state.Gr_pt != double.MinValue)
                return state.Gr_pt;

            double g = 0.0;
            for (int i = 1; i <= 6; i++)
                g += nR[i] * IR[i] * JR[i] * Math.Pow(p, IR[i] - 1) * Math.Pow(t, JR[i] - 1);

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


    }
}
