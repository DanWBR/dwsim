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
        const double tRef = 1.0;
        const double pRef = 1.0;
        const double tRef_btu = 1.8;
        const double pRef_btu = 0.1450377;  // psia
        const double delT_F = 459.67;       // deg. F


        // normalizing constants
        const double Rgas = 0.461526;       // kJ/kg-K
        const double Rgas_btu = 0.1102336;  // Btu/lbm-F


        public double Psat(double t, ref int stat, int btuFlag)
        {
            // t is the temperature
            // returns saturation pressure
            // stat = 0 on successful completion

            // if btuFlag = 0, temperature is in deg. K, pressure is in kPa
            // if btuFlag = 1, temperature is in deg. F, pressure is in psia

            double p;

            ClearErrors();
            
            if (btuFlag == 1)
                t = (t + delT_F)/tRef_btu;


            if (state.region != 4 || state.t != t)
                ClearState();

            if (t < T3p)
            {
                errorCondition = 1;
                AddErrorMessage("Temperature is out of range. Results are for " + T3p.ToString() + " °K");
                p = P3p;
            }
            else if (t > Tc1)
            {
                errorCondition = 1;
                AddErrorMessage("Temperature is out of range. Results are for " + Tc1.ToString() + " °K");
                p = Pc1;
            }
            else
            {
                p = Pi_4(t);

                state.region = 4;
                state.p = p;

                if (btuFlag == 1)
                    p *= pRef_btu;
                else
                    p *= pRef;
            }

            stat = errorCondition;
            return p;
        }

        public double Tsat(double p, ref int stat, int btuFlag)
        {
            // p is the pressure
            // returns saturation temperature
            // stat = 0 on successful completion

            // if btuFlag = 0, temperature is in deg. K, pressure is in kPa
            // if btuFlag = 1, temperature is in deg. F, pressure is in psia

            double t;

            ClearErrors();

            if (btuFlag == 1)
                p /= pRef_btu;
            else
                p /= pRef;

            if (state.region != 4 || state.p != p)
                ClearState();

            if (p < P3p)
            {
                errorCondition = 1;
                AddErrorMessage("Pressure is out of range. Results are for " + P3p.ToString() + " kPa");
                t = T3p;
            }
            else if (p > Pc1)
            {
                errorCondition = 1;
                AddErrorMessage("Pressure is out of range. Results are for " + Pc1.ToString() + " kPa");
                t = Tc1;
            }
            else
            {
                t = Theta_4(p);

                state.region = 4;
                state.t = t;
            }

            if (btuFlag == 1)
                t = t*tRef_btu - delT_F;
            else
                t *= tRef;

            stat = errorCondition;
            return t;
        }

        public double vfp(double p, ref int stat, int btuFlag)
        {
            // p is pressure in kPa or psia
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific volume of saturated liquid in m3/kg or ft3/lbm.

            double t = 0.0, v = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                p /= pRef_btu;
            else
                p /= pRef;

            if (p < P3p)
            {
                p = P3p;
                AddErrorMessage("Pressure is out of bounds. Results are for " + P3p.ToString() + " kPa");
                errorCondition = 1;
            }
            else if (p > Pc1)
            {
                p = Pc1;
                AddErrorMessage("Pressure is out of bounds. Results are for " + Pc1.ToString() + " kPa");
                errorCondition = 1;
            }

            t = Theta_4(p);  // reduced saturation temperature

            int reg = SubRegion(ref p, ref t);
            if (reg == 1 || reg == 2)
                v = Region1.V(p, t);
            else
                v = Region3.V(p, t,1);

            v /= (p / Rgas / t);

            if (btuFlag == 1)
                v *= 16.01846;

            stat = errorCondition;
            return v;

        }

        public double hfp(double p, ref int stat, int btuFlag)
        {
            // p is pressure in kPa or psia
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific enthalpy of saturated liquid in J/kg or Btu/lbm.

            double t = 0.0, h = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                p /= pRef_btu;
            else
                p /= pRef;

            if (p < P3p)
            {
                p = P3p;
                AddErrorMessage("Pressure is out of bounds. Results are for " + P3p.ToString() + " kPa");
                errorCondition = 1;
            }
            else if (p > Pc1)
            {
                p = Pc1;
                AddErrorMessage("Pressure is out of bounds. Results are for " + Pc1.ToString() + " kPa");
                errorCondition = 1;
            }

            t = Theta_4(p);  // reduced saturation temperature

            int reg = SubRegion(ref p, ref t);
            if (reg == 1 || reg == 2)
                h = Region1.H(p, t);
            else
                h = Region3.H(p, t, 1);

            h *= Rgas * t;

            if (btuFlag == 1)
                h /= 2.326;

            stat = errorCondition;
            return h;

        }

        public double sfp(double p, ref int stat, int btuFlag)
        {
            // p is pressure in kPa or psia
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific entropy of saturated liquid in J/kg-K or Btu/lbm-R.

            double t = 0.0, s = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                p /= pRef_btu;
            else
                p /= pRef;

            if (p < P3p)
            {
                p = P3p;
                AddErrorMessage("Pressure is out of bounds. Results are for " + P3p.ToString() + " kPa");
                errorCondition = 1;
            }
            else if (p > Pc1)
            {
                p = Pc1;
                AddErrorMessage("Pressure is out of bounds. Results are for " + Pc1.ToString() + " kPa");
                errorCondition = 1;
            }

            t = Theta_4(p);  // reduced saturation temperature

            int reg = SubRegion(ref p, ref t);
            if (reg == 1 || reg == 2)
                s = Region1.S(p, t);
            else
                s = Region3.S(p, t, 1);

            s *= Rgas;

            if (btuFlag == 1)
                s /= 4.1868;

            stat = errorCondition;
            return s;

        }

        public double mufp(double p, ref int stat, int btuFlag)
        {
            // p is pressure in kPa or psia
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific dynamic viscosity of saturated liquid in Pa-s or lbf-s/ft^2.

            double t = 0.0, v = 0.0, mu = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                p /= pRef_btu;
            else
                p /= pRef;

            if (p < P3p)
            {
                p = P3p;
                AddErrorMessage("Pressure is out of bounds. Results are for " + P3p.ToString() + " kPa");
                errorCondition = 1;
            }
            else if (p > Pc1)
            {
                p = Pc1;
                AddErrorMessage("Pressure is out of bounds. Results are for " + Pc1.ToString() + " kPa");
                errorCondition = 1;
            }

            t = Theta_4(p);  // reduced saturation temperature
            v = vfp(p, ref stat, 0);  // call in SI to get v in m3/kg

            mu = Transport.mu(t, 1.0/v);

            if (btuFlag == 1)
                mu *= 0.02088543;

            stat = errorCondition;
            return mu;

        }

        public double kfp(double p, ref int stat, int btuFlag)
        {
            // p is pressure in kPa or psia
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns thermal conductivity of saturated liquid in W/m-K or Btu/hr-ft-F.

            double t = 0.0, v = 0.0, k = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                p /= pRef_btu;
            else
                p /= pRef;

            if (p < P3p)
            {
                p = P3p;
                AddErrorMessage("Pressure is out of bounds. Results are for " + P3p.ToString() + " kPa");
                errorCondition = 1;
            }
            else if (p > Pc1)
            {
                p = Pc1;
                AddErrorMessage("Pressure is out of bounds. Results are for " + Pc1.ToString() + " kPa");
                errorCondition = 1;
            }

            t = Theta_4(p);  // reduced saturation temperature
            v = vfp(p, ref stat, 0);  // call in SI to get v in m3/kg

            k = Transport.tc(p,t,1.0/v);

            if (btuFlag == 1)
                k *= 0.5777893;

            stat = errorCondition;
            return k;

        }

        public double vgp(double p, ref int stat, int btuFlag)
        {
            // p is pressure in kPa or psia
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific volume of saturated vapor in m3/kg or ft3/lbm.

            double t = 0.0, v = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                p /= pRef_btu;
            else
                p /= pRef;

            if (p < P3p)
            {
                p = P3p;
                AddErrorMessage("Pressure is out of bounds. Results are for " + P3p.ToString() + " kPa");
                errorCondition = 1;
            }
            else if (p > Pc1)
            {
                p = Pc1;
                AddErrorMessage("Pressure is out of bounds. Results are for " + Pc1.ToString() + " kPa");
                errorCondition = 1;
            }

            t = Theta_4(p);  // reduced saturation temperature

            int reg = SubRegion(ref p, ref t);
            if (reg == 1 || reg == 2)
                v = Region2.V(p, t);
            else
                v = Region3.V(p, t, 2);

            v /= (p / Rgas / t);

            if (btuFlag == 1)
                v *= 16.01846;

            stat = errorCondition;
            return v;

        }

        public double hgp(double p, ref int stat, int btuFlag)
        {
            // p is pressure in kPa or psia
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific enthalpy of saturated vapor in J/kg or Btu/lbm.

            double t = 0.0, h = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                p /= pRef_btu;
            else
                p /= pRef;

            if (p < P3p)
            {
                p = P3p;
                AddErrorMessage("Pressure is out of bounds. Results are for " + P3p.ToString() + " kPa");
                errorCondition = 1;
            }
            else if (p > Pc1)
            {
                p = Pc1;
                AddErrorMessage("Pressure is out of bounds. Results are for " + Pc1.ToString() + " kPa");
                errorCondition = 1;
            }

            t = Theta_4(p);  // reduced saturation temperature

            int reg = SubRegion(ref p, ref t);
            if (reg == 1 || reg == 2)
                h = Region2.H(p, t);
            else
                h = Region3.H(p, t, 2);

            h *= Rgas * t;

            if (btuFlag == 1)
                h /= 2.326;

            stat = errorCondition;
            return h;

        }

        public double sgp(double p, ref int stat, int btuFlag)
        {
            // p is pressure in kPa or psia
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific entropy of saturated vapor in J/kg-K or Btu/lbm-R.

            double t = 0.0, s = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                p /= pRef_btu;
            else
                p /= pRef;

            if (p < P3p)
            {
                p = P3p;
                AddErrorMessage("Pressure is out of bounds. Results are for " + P3p.ToString() + " kPa");
                errorCondition = 1;
            }
            else if (p > Pc1)
            {
                p = Pc1;
                AddErrorMessage("Pressure is out of bounds. Results are for " + Pc1.ToString() + " kPa");
                errorCondition = 1;
            }

            t = Theta_4(p);  // reduced saturation temperature

            int reg = SubRegion(ref p, ref t);
            if (reg == 1 || reg == 2)
                s = Region2.S(p, t);
            else
                s = Region3.S(p, t, 2);

            s *= Rgas;

            if (btuFlag == 1)
                s /= 4.1868;

            stat = errorCondition;
            return s;

        }

        public double mugp(double p, ref int stat, int btuFlag)
        {
            // p is pressure in kPa or psia
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific dynamic viscosity of saturated liquid in Pa-s or lbf-s/ft^2.

            double t = 0.0, v = 0.0, mu = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                p /= pRef_btu;
            else
                p /= pRef;

            if (p < P3p)
            {
                p = P3p;
                AddErrorMessage("Pressure is out of bounds. Results are for " + P3p.ToString() + " kPa");
                errorCondition = 1;
            }
            else if (p > Pc1)
            {
                p = Pc1;
                AddErrorMessage("Pressure is out of bounds. Results are for " + Pc1.ToString() + " kPa");
                errorCondition = 1;
            }

            t = Theta_4(p);  // reduced saturation temperature
            v = vgp(p, ref stat, 0);  // call in SI to get v in m3/kg

            mu = Transport.mu(t, 1.0 / v);

            if (btuFlag == 1)
                mu *= 0.02088543;

            stat = errorCondition;
            return mu;

        }

        public double kgp(double p, ref int stat, int btuFlag)
        {
            // p is pressure in kPa or psia
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns thermal conductivity of saturated vapor in W/m-K or Btu/hr-ft-F.

            double t = 0.0, v = 0.0, k = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                p /= pRef_btu;
            else
                p /= pRef;

            if (p < P3p)
            {
                p = P3p;
                AddErrorMessage("Pressure is out of bounds. Results are for " + P3p.ToString() + " kPa");
                errorCondition = 1;
            }
            else if (p > Pc1)
            {
                p = Pc1;
                AddErrorMessage("Pressure is out of bounds. Results are for " + Pc1.ToString() + " kPa");
                errorCondition = 1;
            }

            t = Theta_4(p);  // reduced saturation temperature
            v = vgp(p, ref stat, 0);  // call in SI to get v in m3/kg

            k = Transport.tc(p, t, 1.0 / v);

            if (btuFlag == 1)
                k *= 0.5777893;

            stat = errorCondition;
            return k;

        }

        public double vft(double t, ref int stat, int btuFlag)
        {
            // t is temperature in K or F
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific volume of saturated liquid in m3/kg or ft3/lbm.

            double p = 0.0, v = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                t = (t+delT_F) / tRef_btu;
            else
                t /= tRef;

            if (t < T3p)
            {
                t = T3p;
                AddErrorMessage("Temperature is out of range. Results are for " + T3p.ToString() + " °K");
                errorCondition = 1;
            }
            else if (t > Tc1)
            {
                t = Tc1;
                AddErrorMessage("Temperature is out of range. Results are for " + Tc1.ToString() + " °K");
                errorCondition = 1;
            }

            p = Pi_4(t);     // reduced saturation pressure

            int reg = SubRegion(ref p, ref t);
            if (reg == 1 || reg == 2)
                v = Region1.V(p, t);
            else
                v = Region3.V(p, t, 1);

            v /= (p / Rgas / t);

            if (btuFlag == 1)
                v *= 16.01846;

            stat = errorCondition;
            return v;

        }

        public double hft(double t, ref int stat, int btuFlag)
        {
            // t is temperature in K or F
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific enthalpy of saturated liquid in J/kg or Btu/lbm.

            double p = 0.0, h = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                t = (t + delT_F) / tRef_btu;
            else
                t /= tRef;

            if (t < T3p)
            {
                t = T3p;
                AddErrorMessage("Temperature is out of range. Results are for " + T3p.ToString() + " °K");
                errorCondition = 1;
            }
            else if (t > Tc1)
            {
                t = Tc1;
                AddErrorMessage("Temperature is out of range. Results are for " + Tc1.ToString() + " °K");
                errorCondition = 1;
            }

            p = Pi_4(t);     // reduced saturation pressure


            int reg = SubRegion(ref p, ref t);
            if (reg == 1 || reg == 2)
                h = Region1.H(p, t);
            else
                h = Region3.H(p, t, 1);

            h *= Rgas * t;

            if (btuFlag == 1)
                h /= 2.326;

            stat = errorCondition;
            return h;

        }

        public double sft(double t, ref int stat, int btuFlag)
        {
            // t is temperature in K or F
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific entropy of saturated liquid in J/kg-K or Btu/lbm-R.

            double p = 0.0, s = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                t = (t + delT_F) / tRef_btu;
            else
                t /= tRef;

            if (t < T3p)
            {
                t = T3p;
                AddErrorMessage("Temperature is out of range. Results are for " + T3p.ToString() + " °K");
                errorCondition = 1;
            }
            else if (t > Tc1)
            {
                t = Tc1;
                AddErrorMessage("Temperature is out of range. Results are for " + Tc1.ToString() + " °K");
                errorCondition = 1;
            }

            p = Pi_4(t);     // reduced saturation pressure

            int reg = SubRegion(ref p, ref t);
            if (reg == 1 || reg == 2)
                s = Region1.S(p, t);
            else
                s = Region3.S(p, t, 1);

            s *= Rgas;

            if (btuFlag == 1)
                s /= 4.1868;

            stat = errorCondition;
            return s;

        }

        public double muft(double t, ref int stat, int btuFlag)
        {
            // t is temperature in K or F
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific dynamic viscosity of saturated liquid in Pa-s or lbf-s/ft^2.

            double p = 0.0, v = 0.0, mu = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                t = (t + delT_F) / tRef_btu;
            else
                t /= tRef;

            if (t < T3p)
            {
                t = T3p;
                AddErrorMessage("Temperature is out of range. Results are for " + T3p.ToString() + " °K");
                errorCondition = 1;
            }
            else if (t > Tc1)
            {
                t = Tc1;
                AddErrorMessage("Temperature is out of range. Results are for " + Tc1.ToString() + " °K");
                errorCondition = 1;
            }

            p = Pi_4(t);     // reduced saturation pressure

            v = vft(t, ref stat, 0);  // call in SI to get v in m3/kg

            mu = Transport.mu(t, 1.0 / v);

            if (btuFlag == 1)
                mu *= 0.02088543;

            stat = errorCondition;
            return mu;

        }

        public double kft(double t, ref int stat, int btuFlag)
        {
            // t is temperature in K or F
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns thermal conductivity of saturated liquid in W/m-K or Btu/hr-ft-F.

            double p = 0.0, v = 0.0, k = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                t = (t + delT_F) / tRef_btu;
            else
                t /= tRef;

            if (t < T3p)
            {
                t = T3p;
                AddErrorMessage("Temperature is out of range. Results are for " + T3p.ToString() + " °K");
                errorCondition = 1;
            }
            else if (t > Tc1)
            {
                t = Tc1;
                AddErrorMessage("Temperature is out of range. Results are for " + Tc1.ToString() + " °K");
                errorCondition = 1;
            }

            p = Pi_4(t);     // reduced saturation pressure

            v = vft(t, ref stat, 0);  // call in SI to get v in m3/kg

            k = Transport.tc(p, t, 1.0 / v);

            if (btuFlag == 1)
                k *= 0.5777893;

            stat = errorCondition;
            return k;

        }

        public double vgt(double t, ref int stat, int btuFlag)
        {
            // t is temperature in K or F
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific volume of saturated vapor in m3/kg or ft3/lbm.

            double p = 0.0, v = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                t = (t + delT_F) / tRef_btu;
            else
                t /= tRef;

            if (t < T3p)
            {
                t = T3p;
                AddErrorMessage("Temperature is out of range. Results are for " + T3p.ToString() + " °K");
                errorCondition = 1;
            }
            else if (t > Tc1)
            {
                t = Tc1;
                AddErrorMessage("Temperature is out of range. Results are for " + Tc1.ToString() + " °K");
                errorCondition = 1;
            }

            p = Pi_4(t);     // reduced saturation pressure

            int reg = SubRegion(ref p, ref t);
            if (reg == 1 || reg == 2)
                v = Region2.V(p, t);
            else
                v = Region3.V(p, t, 2);

            v /= (p / Rgas / t);

            if (btuFlag == 1)
                v *= 16.01846;

            stat = errorCondition;
            return v;

        }

        public double hgt(double t, ref int stat, int btuFlag)
        {
            // t is temperature in K or F
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific enthalpy of saturated vapor in J/kg or Btu/lbm.

            double p = 0.0, h = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                t = (t + delT_F) / tRef_btu;
            else
                t /= tRef;

            if (t < T3p)
            {
                t = T3p;
                AddErrorMessage("Temperature is out of range. Results are for " + T3p.ToString() + " °K");
                errorCondition = 1;
            }
            else if (t > Tc1)
            {
                t = Tc1;
                AddErrorMessage("Temperature is out of range. Results are for " + Tc1.ToString() + " °K");
                errorCondition = 1;
            }

            p = Pi_4(t);     // reduced saturation pressure


            int reg = SubRegion(ref p, ref t);
            if (reg == 1 || reg == 2)
                h = Region2.H(p, t);
            else
                h = Region3.H(p, t, 2);

            h *= Rgas * t;

            if (btuFlag == 1)
                h /= 2.326;

            stat = errorCondition;
            return h;

        }

        public double sgt(double t, ref int stat, int btuFlag)
        {
            // t is temperature in K or F
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific entropy of saturated vapor in J/kg-K or Btu/lbm-R.

            double p = 0.0, s = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                t = (t + delT_F) / tRef_btu;
            else
                t /= tRef;

            if (t < T3p)
            {
                t = T3p;
                AddErrorMessage("Temperature is out of range. Results are for " + T3p.ToString() + " °K");
                errorCondition = 1;
            }
            else if (t > Tc1)
            {
                t = Tc1;
                AddErrorMessage("Temperature is out of range. Results are for " + Tc1.ToString() + " °K");
                errorCondition = 1;
            }

            p = Pi_4(t);     // reduced saturation pressure

            int reg = SubRegion(ref p, ref t);
            if (reg == 1 || reg == 2)
                s = Region2.S(p, t);
            else
                s = Region3.S(p, t, 2);

            s *= Rgas;

            if (btuFlag == 1)
                s /= 4.1868;

            stat = errorCondition;
            return s;

        }

        public double mugt(double t, ref int stat, int btuFlag)
        {
            // t is temperature in K or F
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific dynamic viscosity of saturated vapor in Pa-s or lbf-s/ft^2.

            double p = 0.0, v = 0.0, mu = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                t = (t + delT_F) / tRef_btu;
            else
                t /= tRef;

            if (t < T3p)
            {
                t = T3p;
                AddErrorMessage("Temperature is out of range. Results are for " + T3p.ToString() + " °K");
                errorCondition = 1;
            }
            else if (t > Tc1)
            {
                t = Tc1;
                AddErrorMessage("Temperature is out of range. Results are for " + Tc1.ToString() + " °K");
                errorCondition = 1;
            }

            p = Pi_4(t);     // reduced saturation pressure

            v = vgt(t, ref stat, 0);  // call in SI to get v in m3/kg

            mu = Transport.mu(t, 1.0 / v);

            if (btuFlag == 1)
                mu *= 0.02088543;

            stat = errorCondition;
            return mu;

        }

        public double kgt(double t, ref int stat, int btuFlag)
        {
            // t is temperature in K or F
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns thermal conductivity of saturated vapor in W/m-K or Btu/hr-ft-F.

            double p = 0.0, v = 0.0, k = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                t = (t + delT_F) / tRef_btu;
            else
                t /= tRef;

            if (t < T3p)
            {
                t = T3p;
                AddErrorMessage("Temperature is out of range. Results are for " + T3p.ToString() + " °K");
                errorCondition = 1;
            }
            else if (t > Tc1)
            {
                t = Tc1;
                AddErrorMessage("Temperature is out of range. Results are for " + Tc1.ToString() + " °K");
                errorCondition = 1;
            }

            p = Pi_4(t);     // reduced saturation pressure

            v = vgt(t, ref stat, 0);  // call in SI to get v in m3/kg

            k = Transport.tc(p, t, 1.0 / v);

            if (btuFlag == 1)
                k *= 0.5777893;

            stat = errorCondition;
            return k;

        }

        public double vpt(double p, double t, ref int stat, int btuFlag)
        {
            // p is pressure in kPa or psia
            // t is temperature in K or deg F
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific volume in m3/kg or ft3/lbm.

            double v = 1.0;

            ClearErrors();

            if (btuFlag == 1)
                t = (t + delT_F) / tRef_btu;

            if (btuFlag == 1)
                p /= pRef_btu;
            else
                p /= pRef;

            // get the subregion

            int reg = SubRegion(ref p, ref t);

            if (reg == 1)
                v = Region1.V(p, t) / (p/Rgas/t);
            else if (reg == 2)
                v = Region2.V(p, t) / (p / Rgas / t);
            else if (reg == 3)
                v = 1.0 / Region3.Rho(p, t,0);
            else if (reg == 5)
                v = Region5.V(p, t)/ (p / Rgas / t);

            if (btuFlag==1)
                v *= 16.01846;

            stat = errorCondition;
            return v;
        }

        public double hpt(double p, double t, ref int stat, int btuFlag)
        {
            // p is pressure in kPa or psia
            // t is temperature in K or deg F
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific enthalpy in kJ/kg or Btu/lbm.

            double h=0.0;

            ClearErrors();

            if (btuFlag == 1)
                t = (t + delT_F) / tRef_btu;

            if (btuFlag == 1)
                p /= pRef_btu;
            else
                p /= pRef;

            // get the subregion

            int reg = SubRegion(ref p, ref t);

            if (reg == 1)
                h = Region1.H(p, t);
            else if (reg == 2)
                h = Region2.H(p, t);
            else if (reg == 3)
                h = Region3.H(p, t,0);
            else if (reg == 5)
                h = Region5.H(p, t);

            stat = errorCondition;

            if (btuFlag == 1)
                return h * Rgas_btu * (t * 1.8);
            else
                return h * Rgas * t;
        }

        public double spt(double p, double t, ref int stat, int btuFlag)
        {
            // p is pressure in kPa or psia
            // t is temperature in K or deg F
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific entropy in kJ/kg-K or Btu/lbm-F.

            double s=0.0;

            ClearErrors();

            if (btuFlag == 1)
                t = (t + delT_F) / tRef_btu;

            if (btuFlag == 1)
                p /= pRef_btu;
            else
                p /= pRef;

            // get the subregion

            int reg = SubRegion(ref p, ref t);

            if (reg == 1)
                s = Region1.S(p, t);
            else if (reg == 2)
                s = Region2.S(p, t);
            else if (reg == 3)
                s = Region3.S(p, t,0);
            else if (reg == 5)
                s = Region5.S(p, t);

            stat = errorCondition;

            if (btuFlag == 1)
                return s * Rgas_btu;
            else
                return s * Rgas;
        }

        public double cppt(double p, double t, ref int stat, int btuFlag)
        {
            // p is pressure in kPa or psia
            // t is temperature in K or deg F
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific heat at constant pressure in kJ/kg-K or Btu/lbm-F.

            double cp = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                t = (t + delT_F) / tRef_btu;

            if (btuFlag == 1)
                p /= pRef_btu;
            else
                p /= pRef;

            // get the subregion

            int reg = SubRegion(ref p, ref t);

            if (reg == 1)
                cp = Region1.CP(p, t);
            else if (reg == 2)
                cp = Region2.CP(p, t);
            else if (reg == 3)
                cp = Region3.CP(p, t, 0);
            else if (reg == 5)
                cp = Region5.CP(p, t);

            stat = errorCondition;

            if (btuFlag == 1)
                return cp * Rgas_btu;
            else
                return cp * Rgas;
        }

        public double mupt(double p, double t, ref int stat, int btuFlag)
        {
            // p is pressure in kPa or psia
            // t is temperature in K or deg F
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns specific dynamic viscosity of saturated liquid in Pa-s or lbf-s/ft^2.

            double v = 0.0, mu = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                t = (t + delT_F) / tRef_btu;

            if (btuFlag == 1)
                p /= pRef_btu;
            else
                p /= pRef;

            // get the subregion

            int reg = SubRegion(ref p, ref t);

            if (reg == 1)
                v = Region1.V(p, t);
            else if (reg == 2)
                v = Region2.V(p, t);
            else if (reg == 3)
                v = Region3.V(p, t, 0);
            else if (reg == 5)
                v = Region5.V(p, t);

            v /= (p / Rgas / t);

            stat = errorCondition;

            mu = Transport.mu(t, 1.0 / v);

            if (btuFlag == 1)
                mu *= 0.02088543;

            if (stat != 0) stat = errorCondition;
            return mu;

        }

        public double kpt(double p, double t, ref int stat, int btuFlag)
        {
            // t is temperature in K or F
            // stat = 0 on successful completion, stat != 0 on error
            // btuFlag = 1 for BTU units, btuFlag is 0 for SI
            // returns thermal conductivity of saturated vapor in W/m-K or Btu/hr-ft-F.

            double v = 0.0, k = 0.0;

            ClearErrors();

            if (btuFlag == 1)
                t = (t + delT_F) / tRef_btu;

            if (btuFlag == 1)
                p /= pRef_btu;
            else
                p /= pRef;


            // get the subregion

            int reg = SubRegion(ref p, ref t);

            if (reg == 1)
                v = Region1.V(p, t);
            else if (reg == 2)
                v = Region2.V(p, t);
            else if (reg == 3)
                v = Region3.V(p, t, 0);
            else if (reg == 5)
                v = Region5.V(p, t);

            v /= (p / Rgas / t);

            stat = errorCondition;

            k = Transport.tc(p, t, 1.0 / v);

            if (btuFlag == 1)
                k *= 0.5777893;

            if (stat != 0) stat = errorCondition;

            return k;
        }

        public double Tph(double p, double h, ref int stat, int btuFlag)
        {
            // p is pressure in kPa or Btu/lbm
            // h is enthalpy in kJ/kg or Btu/lbm
            // stat is 0 on return if successful completion
            // Units are in SI if btuFlag = 0, in BTU if btuFlag = 1

            double t = 0.0;
            double x = double.MinValue;

            ClearErrors();

            if (btuFlag == 1)
            {
                p /= pRef_btu;
                h *= 2.326;
            }

            if (p < P3p)
            {
                p = P3p;
                AddErrorMessage("Pressure is out of bounds. Results are for " + P3p.ToString() + " kPa");
                errorCondition = 1;
            }
            else if (p > 100000.0)
            {
                p = 100000.0;
                AddErrorMessage("Pressure is out of bounds. Results are for " + p.ToString() + " kPa");
                errorCondition = 1;
            }

            double hmax = ((7.38214E-14*p - 6.22294E-09)*p - 4.57127E-03)*p + 4.16092E+03;
            double hmin = Math.Max(((1.14504E-15 * p - 7.53710E-10) * p + 1.01820E-03) * p - 3.89901E-02, 0.0);

            if (h > hmax)
            {
                h = hmax;
                AddErrorMessage("Enthalpy is too high. Results are for " + h.ToString() + " kJ/kg");
                errorCondition = 1;
            }
            else if (h < hmin)
            {
                h = hmin;
                AddErrorMessage("Enthalpy is too low. Results are for " + h.ToString() + " kJ/kg");
                errorCondition = 1;
            }


            if (p < Pc1)
            {
                double tsat, hf, hg;

                tsat = Theta_4(p);

                if (tsat < 623.15)
                {
                    hf = Region1.H(p, tsat) * Rgas * tsat;
                    hg = Region2.H(p, tsat) * Rgas * tsat;
                    x = (h - hf) / (hg - hf);
                    if (x < 0.0)
                        t = Region1.Tph(p, h);
                    else if (x <= 1.0 && x >= 0.0)
                        t = tsat;
                    else
                        t = Region2.Tph(p, h);
                }
                else
                {
                    hf = Region3.H(p, tsat, 1) * Rgas * tsat;
                    hg = Region3.H(p, tsat, 2) * Rgas * tsat;
                    x = (h - hf) / (hg - hf);
                    if (x < 0.0)
                    {
                        // Approximate Region 1-3 boundary 
                        double h13 = (((7.27071E-18 * p - 2.06464E-12) * p + 2.23637E-07) * p - 1.14480E-02) * p + 1.80136E+03;
                        if (h < h13)
                            t = Region1.Tph(p, h);
                        else
                            t = Region3.Tph(p, h);
                    }
                    else if (x <= 1.0 && x >= 0.0)
                        t = tsat;
                    else
                    {  // this could be in region 2 or 3.
                        double t23 = T23(p);
                        double h23 = Region2.H(p, t23) * Rgas * t23;
                        if (h < h23)
                            t = Region3.Tph(p, h);
                        else
                            t = Region2.Tph(p, h);
                    }
                }
            }
            else
            {
                int reg;
                bool regionFound = false;
                // we have to do some guessing
                
                // Approximate Region 1-3 boundary above critical pressure
                double h13 = (((7.27071E-18 * p - 2.06464E-12) * p + 2.23637E-07) * p - 1.14480E-02) * p + 1.80136E+03;

                if (h < (h13 + 10.0) )  // add some to be sure
                {
                    t = Region1.Tph(p, h);
                    reg = SubRegion(ref p, ref t);
                    if (reg == 1)
                        regionFound = true;  // got it.
                }

                if ( ! regionFound ) 
                {
                    // this is region 2 or 3.
                    // approximate region 2-3 boundary above critical pressure
                    double h23;
                    if (p < 40000.0)
                        h23 = (((-1.53693E-15 * p + 1.99564E-10) * p - 9.49050E-06) * p + 1.94414E-01) * p + 1.17743E+03;
                    else
                        h23 = (((8.04207E-18 * p - 2.67059E-12) * p + 3.33749E-07) * p - 1.46824E-02) * p + 2.81053E+03;

                    if (h > h23 - 10.0)  // add some tolerance
                    {
                        t = Region2.Tph(p, h);
                        reg = SubRegion(ref p, ref t);
                        if (reg == 3)
                            regionFound = false;
                        else
                            regionFound = true;
                    }
                }


                if (!regionFound)
                {
                    // Not in region 1 or 2. Has to be region 3.
                    t = Region3.Tph(p, h);
                }
            }

            if (btuFlag == 1)
                t = t * tRef_btu - delT_F;

            stat = errorCondition;
            state.x = x;
            return t;
        }

        public double Tps(double p, double s, ref int stat, int btuFlag)
        {
            // p is pressure in kPa or Btu/lbm
            // s is entropy in kJ/kg-K or Btu/lbm-F
            // stat is 0 on return if successful completion
            // Units are in SI if btuFlag = 0, in BTU if btuFlag = 1

            double t = 0.0;
            double x=double.MinValue;

            ClearErrors();

            if (btuFlag == 1)
            {
                p /= pRef_btu;
                s *=  4.1868;
            }

            if (p < P3p)
            {
                p = P3p;
                AddErrorMessage("Pressure is out of bounds. Results are for " + P3p.ToString() + " kPa");
                errorCondition = 1;
            }
            else if (p > 100000.0)
            {
                p = 100000.0;
                AddErrorMessage("Pressure is out of bounds. Results are for " + p.ToString() + " kPa");
                errorCondition = 1;
            }

            double smax = -5.58993E-01 * Math.Log(p) + 1.25450E+01;
            double smin = 0.0;

            if (s > smax)
            {
                s = smax;
                AddErrorMessage("Entropy is too high. Results are for " + s.ToString() + " kJ/kg °K");
                errorCondition = 1;
            }
            else if (s < smin)
            {
                s = smin;
                AddErrorMessage("Enthalpy is too low. Results are for " + s.ToString() + " kJ/kg °K");
                errorCondition = 1;
            }


            if (p < Pc1)
            {
                double tsat, sf, sg;

                tsat = Theta_4(p);

                if (tsat < 623.15)
                {
                    sf = Region1.S(p, tsat) * Rgas ;
                    sg = Region2.S(p, tsat) * Rgas ;
                    x = (s - sf) / (sg - sf);
                    if (x < 0.0)
                        t = Region1.Tps(p, s);
                    else if (x <= 1.0 && x >= 0.0)
                        t = tsat;
                    else
                        t = Region2.Tps(p, s);
                }
                else
                {
                    sf = Region3.S(p, tsat, 1) * Rgas ;
                    sg = Region3.S(p, tsat, 2) * Rgas ;
                    x = (s - sf) / (sg - sf);
                    if (x < 0.0)
                    {
                        // Approximate Region 1-3 boundary
                        double s13 = (3.76683E-11 * p - 8.36729E-06) * p + 3.86940E+00;
                        if (s < s13)
                            t = Region1.Tps(p, s);
                        else
                            t = Region3.Tps(p, s);
                    }
                    else if (x <= 1.0 && x >= 0.0)
                        t = tsat;
                    else
                    {  // this could be in region 2 or 3.
                        double t23 = T23(p);
                        double s23 = Region2.S(p, t23) * Rgas;
                        if (s < s23)
                            t = Region3.Tps(p, s);
                        else
                            t = Region2.Tps(p, s);
                    }
                }
            }
            else
            {
                int reg;
                bool regionFound = false;
                // we have to do some guessing

                // Approximate Region 1-3 boundary above critical pressure
                double s13 = (3.76683E-11 * p - 8.36729E-06) * p + 3.86940E+00;

                if (s < (s13 + 0.05))  // add some to be sure
                {
                    t = Region1.Tps(p, s);
                    reg = SubRegion(ref p, ref t);
                    if (reg == 1)
                        regionFound = true;  // got it.
                }

                if (!regionFound)
                {
                    // this is region 2 or 3.
                    // approximate region 2-3 boundary above critical pressure
                    double s23;
                    if (p < 40000.0)
                        s23 = (((((-2.11798E-26 * p + 3.88682E-21) * p - 2.95203E-16) * p + 1.18836E-11) * p - 2.67319E-07) * p + 3.17149E-03) * p - 1.01618E+01;

                    else
                        s23 = ((((-2.96307E-25 * p + 1.17911E-19) * p - 1.89365E-14) * p + 1.54787E-09) * p - 6.35368E-05) * p + 6.08131E+00;

                    if (s > s23 - 0.05)  // add some tolerance
                    {
                        t = Region2.Tps(p, s);
                        reg = SubRegion(ref p, ref t);
                        if (reg == 3)
                            regionFound = false;
                        else
                            regionFound = true;
                    }
                }


                if (!regionFound)
                {
                    // Not in region 1 or 2. Has to be region 3.
                    t = Region3.Tps(p, s);
                }

                return t;

            }

            if (btuFlag == 1)
                t = t * tRef_btu - delT_F;

            stat = errorCondition;
            state.x = x;
            return t;
        }

        public double P3rt(double r, double t, ref int stat, int btuFlag)
        {
            // This function is for test purposes only.
            // r is density in kg/m3 or lbm/ft3
            // t is temperature in K or F
            // stat is 0 on return if successful completion
            // Units are in SI if btuFlag = 0, in BTU if btuFlag = 1

            double p;

            if (btuFlag == 1)
            {
                r *= 16.01846;
                t = (t + delT_F) / 1.8;
            }

            p = Region3.Prt(r, t);

            if (btuFlag == 1)
                p *= pRef_btu;

            return p;
        }
    }
}