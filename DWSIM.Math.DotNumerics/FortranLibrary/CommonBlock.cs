#region Copyright © 2009 Jose Antonio De Santiago-Castillo.

//Copyright © 2009 Jose Antonio De Santiago-Castillo 
//E-mail:JAntonioDeSantiago@gmail.com
//Web: www.DotNumerics.com
//
#endregion

using System;
using System.Collections.Generic;
using System.Text;

namespace DotNumerics.FortranLibrary
{
    public class CommonBlock
    {
        #region Fields

        //private string MeName = "";

        private Odouble[] MeDoubleData;
        private Oint[] MeIntData;

        #endregion

        #region Constructor

        public CommonBlock(int DimensionDouble, int DimensionInt, int DimensionSingle, int DimensionComplex)
        {
            //this.MeName = TheName;
            if (DimensionDouble > 0)
            {
                this.MeDoubleData = new Odouble[DimensionDouble];

                for (int i = 0; i < DimensionDouble; i++)
                {
                    MeDoubleData[i] = new Odouble();
                }
            }
            if (DimensionInt > 0)
            {
                this.MeIntData = new Oint[DimensionInt];

                for (int i = 0; i < DimensionInt; i++)
                {
                    MeIntData[i] = new Oint();
                }
            }
        }

        #endregion

        #region Properties

        public Odouble[] doubleData
        {
            get { return this.MeDoubleData; }
            set { this.MeDoubleData = value; }
        }

        public Oint[] intData
        {
            get { return this.MeIntData; }
            set { this.MeIntData = value; }
        }

        #endregion

        #region Methods


        #endregion

    }
}
