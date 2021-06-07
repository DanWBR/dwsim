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

    public partial class StmProp
    {

        // constants for the boundary between regions 2 and 3
        public double[] n23 = {0.0,
                               0.34805185628969e3, -0.11671859879975e1, 0.10192970039326e-2,
                               0.57254459862746e3, 0.13918839778870e2 };

        // constants for region 4 (saturation line)
        public double[] n4 = { 0.0,
                               0.11670521452767e4, -0.72421316703206e6, -0.17073846940092e2,
                               0.12020824702470e5, -0.32325550322333e7,  0.14915108613530e2,
                              -0.48232657361591e4,  0.40511340542057e6, -0.23855557567849,
                               0.65017534844798e3 };



        // Boundary between Regions 2 and 3

        public double P23(double t)
        {
            double result;

            // t is temperature in K
            // returns p between regions 2 and 3 in kPa

            result = n23[1] + n23[2] * t + n23[3] * t * t;

            return result*1000.0;
        }

        public double T23(double p)
        {
            // p is pressure in kPa
            // returns temperature between regions 2 and 3 in K

            p *= 0.001;

            double result = n23[4] + Math.Pow((p-n23[5])/n23[3],0.5);
            return result;
        }


        // Saturation Line

        public double Theta_4(double p)
        {
            // returns saturation temperature in K
            // p is pressure in kPa

            double pi = p / 1000.0;
            double b = Math.Pow(pi, 0.25);

            double E = b * b + n4[3]*b+n4[6];
            double F = n4[1] * b * b + n4[4] * b + n4[7];
            double G = n4[2] * b * b + n4[5] * b + n4[8];
            double D = 2.0 * G / (-F - Math.Pow(F * F - 4.0 * E * G, 0.5));
            double t1 = (n4[10] + D);
            double t2 = t1 * t1 - 4.0 * (n4[9] + n4[10] * D);
            return (t1 - Math.Pow(t2, 0.5)) / 2.0;
        }

        public double Pi_4 (double t)
        {
            // returns saturation Pressure in kPa
            // t is temperature in K

            double th = t + n4[9]/(t-n4[10]);

            double A = th * th + n4[1] * th + n4[2];
            double B = n4[3] * th * th + n4[4] * th + n4[5];
            double C = n4[6] * th * th + n4[7] * th + n4[8];

            double p = 2.0 * C / (-B + Math.Pow(B * B - 4.0 * A * C, 0.5));
            p = Math.Pow(p, 4);
            return p * 1000.0;
        }

    }
}