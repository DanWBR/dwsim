/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System;
using System.Globalization;

namespace SwarmOps
{
    public static partial class Tools
    {
        private static CultureInfo _cultureInfo = new CultureInfo("en-US");

        /// <summary>
        /// Convert numeric value d to a string with convenient formatting.
        /// </summary>
        public static string FormatNumber(double? d)
        {
            string s;

            if (d.HasValue)
            {
                double dAbs = Math.Abs(d.Value);

                if (dAbs < 1e-2)
                {
                    s = String.Format(_cultureInfo, "{0:0.##e0}", d.Value);
                }
                else if (dAbs > 1e+6)
                {
                    s = String.Format(_cultureInfo, "{0:0.##e+0}", d.Value);
                }
                else if (dAbs > 1e+3)
                {
                    s = String.Format(_cultureInfo, "{0:0.}", d.Value);
                }
                else
                {
                    s = String.Format(_cultureInfo, "{0:0.##}", d.Value);
                }
            }
            else
            {
                s = "--";
            }

            return s;
        }

        /// <summary>
        /// Convert numeric value d to a percentage string, e.g. d==0.253212
        /// returns string "25.32%"
        /// </summary>
        public static string FormatPercent(double? d)
        {
            return String.Format(_cultureInfo, "{0:0.##}%", d*100);
        }
    }
}
