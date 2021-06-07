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

        public int SubRegion(ref double p, ref double t)
        {
            // returns the region
            // t is temperature in K
            // p is pressure in kPa

            if (t == state.t && p == state.p)
                return state.region;
            
            int region=-1;


            if (p > 100000.0)
            {
                errorCondition = 1;
                AddErrorMessage("Pressure is out of bounds, Results are for 100 MPa");
                p = 100000.0;
            }
            else if (p < 0.0)
            {
                errorCondition = 1;
                AddErrorMessage("Pressure is out of bounds, Results are for 0");
                p = 0.0;
            }

            if (t < 273.15)
            {
                errorCondition = 1;
                AddErrorMessage("Temperature is out of bounds, Results are for 273.15 K");
                t = 273.15;
                region = 1;
            }
            else if (t < 623.15)
            {
                // calculate saturated pressure
                double psat = Pi_4(t);
                if (p < psat) 
                    region = 2;
                else
                    region = 1;
            }
            else if (t < 863.15)
            {
                // calculate boundary between regions 2 and 3

                double p23 = P23(t);

                if (p < p23)
                    region = 2;
                else
                    region = 3;
            }
            else if (t < 1073.15)
            {
                region = 2;
            }
            else if (t < 2273.15)
            {
                if (p > 50000.0)
                {
                    errorCondition = 1;
                    AddErrorMessage("Pressure is out of bounds, Results are for 50 MPa");
                    p = 50000.0;
                    region = 5;
                }
                region = 5;
            }
            else
            {
                errorCondition = 1;
                AddErrorMessage("Temperature is out of bounds, Results are for 2273.15 K");
                if (p > 50000.0)
                {
                    AddErrorMessage("Pressure is out of bounds, Results are for 50 MPa");
                    p = 50000.0;
                }
                t = 2273.15;
                region = 5;
            }

            state.region = region;
            return region;
        }
    }
}