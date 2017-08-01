/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

namespace SwarmOps
{
    public static partial class Tools
    {
        /// <summary>
        /// Convert an array of System.double values to a string.
        /// </summary>
        /// <param name="x">Array of values to be converted to string.</param>
        public static string ArrayToString(double[] x)
        {
            return ArrayToString(x, 4);
        }

        /// <summary>
        /// Convert an array of System.double values to a string.
        /// </summary>
        /// <param name="x">Array of values to be converted to string.</param>
        /// <param name="digits">Number of digits for each value.</param>
        /// <returns></returns>
        public static string ArrayToString(double[] x, int digits)
        {
            string s = "{ ";

            if (x.Length>0)
            {
                s += NumberToString(x[0], digits);

                for (int i = 1; i < x.Length; i++)
                {
                    s += ", " + NumberToString(x[i], digits);
                }
            }

            s += " }";

            return s;
        }

        /// <summary>
        /// Convert an array of System.double values to a string. Not formatted like a C# array.
        /// </summary>
        /// <param name="x">Array of values to be converted to string.</param>
        public static string ArrayToStringRaw(double[] x)
        {
            return ArrayToStringRaw(x, 4);
        }

        /// <summary>
        /// Convert an array of System.double values to a string. Not formattet like a C# array.
        /// </summary>
        /// <param name="x">Array of values to be converted to string.</param>
        /// <param name="digits">Number of digits for each value.</param>
        /// <returns></returns>
        public static string ArrayToStringRaw(double[] x, int digits)
        {
            string s = "";

            for (int i = 0; i < x.Length; i++)
            {
                s += NumberToString(x[i], digits) + " ";
            }

            return s;
        }

        /// <summary>
        /// Convert a double-value to a string, formatted for array output.
        /// </summary>
        /// <param name="x">Value to convert.</param>
        /// <param name="digits">Number of digits.</param>
        public static string NumberToString(double x, int digits)
        {
            return System.Math.Round(x, digits).ToString(_cultureInfo);
        }
    }
}
