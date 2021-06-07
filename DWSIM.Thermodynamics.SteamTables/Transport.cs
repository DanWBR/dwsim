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
    public class TRANSPORT
    {


        public StmProp daddy;

        public double mu(double t, double rho)
        {
            // t is temperature in K
            // rho is density in kg/m3
            // returns viscosity in Pa-s.

            const double rhoStar = 322.0;   // kg/m3
            const double tStar = 647.096;   // K

            double[] Hi = {1.67752, 2.20462, 0.6366564, -0.241605};
            double[,] Hji = { { 5.20094e-1, 8.50895e-2,-1.08374   ,-2.89555e-1, 0.0       , 0.0       },
                             { 2.22531e-1, 9.99115e-1, 1.88797   , 1.26613   , 0.0       , 1.20573e-1},
                             {-2.81378e-1,-9.06851e-1,-7.72479e-1,-4.89837e-1,-2.57040e-1, 0.0       },
                             { 1.61913e-1, 2.57399e-1, 0.0       , 0.0       , 0.0       , 0.0       },
                             {-3.25372e-2, 0.0       , 0.0       , 6.98452e-2, 0.0       , 0.0       },
                             { 0.0       , 0.0       , 0.0       , 0.0       , 8.72102e-3, 0.0       },
                             { 0.0       , 0.0       , 0.0       ,-4.35673e-3, 0.0       ,-5.93264e-4} };


            double mu0 = 0.0, mu1 = 0.0;

            t /= tStar;
            rho /= rhoStar;


            double t1 = 0.0;
            for (int i = 0; i <= 3; i++)
                t1 += Hi[i] / Math.Pow(t, i);
            mu0 = 100.0 * Math.Pow(t, 0.5) / t1;

            t1 = 0.0;
            for (int i = 0; i <= 5; i++)
            {
                double t2 = 0.0;
                for (int j = 0; j <= 6; j++)
                    t2 += Hji[j, i] * Math.Pow(rho - 1.0, j);

                t1 += Math.Pow(1.0 / t - 1.0, i) * t2;
            }
            mu1 = Math.Exp(rho * t1);

            return mu0 * mu1 * 1.0e-6;

        }

        public double tc(double p, double t, double rho)
        {
            // p is pressure in kPa
            // t is temperature in K
            // rho is density in kg/m3

            const double rhoStar = 317.7;   // kg/m3
            const double tStar = 647.26;   // K

            double k0 = 0.0, k1 = 0.0, k2 = 0.0 ;

            double[] a = { 0.0102811, 0.0299621, 0.0156146, -0.00422464 };
            double[] b = { -0.397070, 0.400302, 1.060000 };
            double[] d = { 0.0, 0.0701309, 0.0118520, 0.00169937, -1.0200 };
            double[] B = { 0.0, -0.171587, 2.392190 };
            double[] C = { 0.0, 0.642857, -4.11717, -6.17937, 0.00308976, 0.0822994, 10.0932 };

            if (t < 273.16)
            {
                daddy.errorCondition = 1;
                daddy.AddErrorMessage("Temperature is too low for thermal conductivity. Results are for 273.16 °K");
                t = 273.16;
            }
            else if (t < 773.15)
            {
                if (p > 100000.0)
                {
                    daddy.errorCondition = 1;
                    daddy.AddErrorMessage("Pressure is too high for thermal conductivity. Results are for 100 MPa");
                    p = 100000.0;
                }
            }
            else if (t < 923.15)
            {
                if (p > 70000.0)
                {
                    daddy.errorCondition = 1;
                    daddy.AddErrorMessage("Pressure is too high for thermal conductivity. Results are for 70 MPa");
                    p = 70000.0;
                }
            }
            else if (t < 1073.15)
            {
                if (p > 40000.0)
                {
                    daddy.errorCondition = 1;
                    daddy.AddErrorMessage("Pressure is too high for thermal conductivity. Results are for 40 MPa");
                    p = 40000.0;
                }
            }

            t /= tStar;
            rho /= rhoStar;

            double t1 = 0.0;
            for (int i = 0; i <= 3; i++)
                t1 += a[i] * Math.Pow(t, i);
            k0 = Math.Pow(t, 0.5) * t1;

            k1 = b[0] + b[1] * rho + b[2] * Math.Exp(B[1] * Math.Pow(rho + B[2], 2));

            double DelT = Math.Abs(t - 1.0) + C[4];
            double Q = 2 + C[5] / Math.Pow(DelT, 0.6);
            double S;
            if (t > 1.0)
                S = 1.0 / DelT;
            else
                S = C[6] / Math.Pow(DelT, 0.6);

            k2 = (d[1] / Math.Pow(t, 10) + d[2]) * Math.Pow(rho, 1.8) *
                 Math.Exp(C[1] * (1.0 - Math.Pow(rho, 2.8))) +
                 d[3] * S * Math.Pow(rho, Q) * Math.Exp(Q / (1.0 + Q) * (1.0 - Math.Pow(rho, 1.0 + Q))) +
                 d[4] * Math.Exp(C[2] * Math.Pow(t, 1.5) + C[3] / Math.Pow(rho, 5));


            return k0 + k1 + k2;
        }
    }
}
