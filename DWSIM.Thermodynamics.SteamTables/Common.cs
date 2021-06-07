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

    public struct STATE
    {
        public double t;  // t/tStar
        public double p;  // p/pStar
        public int region;
        public double x;  // quality
    }

    public partial class StmProp
    {
        public STATE state;
        public REGION1 Region1;
        public REGION2 Region2;
        public REGION3 Region3;
        public REGION5 Region5;
        public TRANSPORT Transport;

        public int errorCondition;
        public string errorMessage;

        // critical properties in reduced units
        const double Tc1 = 647.096;       // Tc/tStar
        const double Pc1 = 22064.0;       // Pc/pStar

        // minimum values on the saturation line (triple point)
        const double P3p = 0.611657;
        const double T3p = 273.16;

        const int LIQ_FLAG = 1;
        const int STM_FLAG = 2;


        public StmProp()
        {
            state = new STATE();
            Region1 = new REGION1();
            Region2 = new REGION2();
            Region3 = new REGION3();
            Region5 = new REGION5();
            Transport = new TRANSPORT();


            Region3.daddy = this;
            Transport.daddy = this;

            errorCondition = 0;
            errorMessage = "Normal Termination";
        }


        public void AddErrorMessage(string errMsg)
        {
            if (errorMessage == "")
                errorMessage = errMsg;
            else
                errorMessage += "\r\n" + errMsg;
        }

        void ClearErrors()
        {
            errorMessage = "";
            errorCondition = 0;
        }

        void ClearState()
        {
            state.t = 0.0;
            state.p = 0.0;
            state.region = -1;
            state.x = double.MinValue;
        }
    }
}
