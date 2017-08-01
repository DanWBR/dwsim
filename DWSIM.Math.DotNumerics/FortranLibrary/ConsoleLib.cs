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
    public class ConsoleLib
    {

        public static void ConsoleReadLine(Characters destination, int length)
        {
            string s = Console.ReadLine();
            if (s.Length > length) s = s.Substring(0, length);
            destination.ToBlanks(length);
            FortranLib.Copy(destination, s);
        }

        public static void WriteInt(Characters destination, int number, int width, int m)
        {
            //w is a nonzero, unsigned integer constant denoting the size of
            //the external field, including blanks and a sign, if necessary.
            //A minus sign (-) is always printed on output if the number
            //is negative. If the number is positive, a plus sign (+) is
            //printed only if SP is in effect.
            //m is an unsigned integer constant denoting the minimum
            //number of digits required on output. m is ignored on input.
            //The value of m must not exceed w; if m is omitted, a value of
            //1 is assumed.


            //Examples of I Editing on Output
            //Value      Format      Output
            //-12        I7.6         -000012
            //12345      I5         12345

            destination.ToBlanks(width);

            char[] formatCharArray = new char[width];
            for (int i = 0; i < formatCharArray.Length; i++) formatCharArray[i] = '#';
            for (int i = 0; i < m; i++) formatCharArray[i] = '0';
            Array.Reverse(formatCharArray);
            string format = new string(formatCharArray);
            string s = number.ToString(format);

            if (s.Length > width)
            {
                s = new string('*', width);
            }
            else
            {
                s = new string(' ', width - s.Length) + s;
            }
            FortranLib.Copy(destination, s);
        }

        public static void WriteFloat(Characters destination, float number, int width, int d)
        {
            //The term w must be large enough to include:
            //• a minus sign for a negative value or a plus sign (when SP is in effect) for a positive value
            //• the decimal point
            //• d digits to the right of the decimal
            //If w is insufficiently large, the entire field width is filled with asterisks.
            //Therefore, w must be > d + 2.

            destination.ToBlanks(width);

            string s = "";
            if (width <= d + 2)
            {
                s = new string('*', width);
            }
            else
            {
                char[] formatCharArray = new char[width];
                for (int i = 0; i < formatCharArray.Length; i++) formatCharArray[i] = '#';
                for (int i = 0; i < d; i++) formatCharArray[i] = '0';
                formatCharArray[d] = '.';
                Array.Reverse(formatCharArray);
                string format = new string(formatCharArray);
                s = number.ToString(format);
            }
            if (s.Length > width)
            {
                s = new string('*', width);
            }
            else
            {
                s = new string(' ', width - s.Length) + s;
            }
            FortranLib.Copy(destination, s);
        }


        public static bool ReadFloat(Characters source, int width, int d, out float destination)
        {
            string numberStg = source.Substring(1, width).ToString();
            float internalValue;
            bool isOK = float.TryParse(numberStg, out internalValue);

            destination = internalValue;

            if (numberStg.Contains(".") == false)
            {
                destination *= (float)Math.Pow(10, -d);
            }
            return isOK;
        }


        public static void Date_And_Time(Characters date, Characters time)
        {
            DateTime _dateTime = DateTime.Now;
            date.ToBlanks(8);
            time.ToBlanks(10);
            FortranLib.Copy(date, _dateTime.ToString("yyyyMMdd"));
            FortranLib.Copy(time, _dateTime.ToString("hhmmss.fff"));
        }


    }
}
