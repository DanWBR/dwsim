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
    public class Obool
    {
        private bool _v = false;

        public bool v
        {
            get { return _v; }
            set { _v = value; }
        }
    }
}
