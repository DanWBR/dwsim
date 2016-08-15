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

    public class FortranLib
    {


        /// <summary>
        ///LEN_TRIM
        ///Description: Returns the length of the character argument without counting trailing blank
        ///characters.
        ///Syntax: result = LEN_TRIM (string [, kind])
        ///Class: Elemental function; Generic
        ///Arguments:
        ///string Must be of type character.
        ///kind (opt) Must be a scalar integer initialization expression.
        ///Examples
        ///LEN_TRIM ('   C  D   ') has the value 7.
        ///LEN_TRIM ('     ') has the value 0.
        /// </summary>
        /// <remarks>
        ///  From Intel Fortran Language Reference
        /// </remarks>
        /// <param name="s"></param>
        /// <returns></returns>
        public static int LEN_TRIM(string s)
        {
            if (s == null) return 0;

            int lentrim = s.Length;
            char blank = ' ';

            for (int i = s.Length - 1; i > -1; i--)
            {
                if (s[i] == blank)
                {
                    lentrim--;
                }
                else
                {
                    break;
                }
            }
            return lentrim;
        }

        public static int LEN_TRIM(Characters chars)
        {
            return chars.LenTrim();
        }

        /// <summary>
        ///REPEAT
        ///Description: Concatenates several copies of a string.
        ///Syntax: result = REPEAT (string, ncopies)
        ///Class: Transformational function; Generic
        ///Arguments:
        ///string Must be scalar and of type character.
        ///ncopies Must be scalar and of type integer. It must not be negative.
        ///Results: The result is a scalar of type character and length ncopies x LEN(string). The
        ///kind parameter is the same as string. The value of the result is the
        ///concatenation of ncopies copies of string.
        ///        Examples
        ///REPEAT ('S', 3) has the value SSS.
        ///REPEAT ('ABC', 0) has the value of a zero-length string.
        /// </summary>
        /// <remarks>
        ///   From Intel Fortran Language Reference
        /// </remarks>
        /// <param name="s"></param>
        /// <param name="count"></param>
        /// <returns></returns>
        public static string REPEAT(string s, int ncopies)
        {
            string value="";

            if (string.IsNullOrEmpty(s) == true || ncopies < 1) return value;

            StringBuilder sBuilder = new StringBuilder(s.Length * ncopies);
            for (int i = 0; i < ncopies; i++)
            {
                sBuilder.Append(s);
            }
            value = sBuilder.ToString();
            return value;
        }

        #region SIGN
        //Intrinsic Function    Definition       No.of Args.     Generic Name    Specific Names   Argument    Type
        //
        //Transfer of sign    |a1| if a2>= 0        2               SIGN            ISIGN            INTEGER   INTEGER
        //                    -|a1| if a2< 0

        /// <summary>
        /// Transfer of sign
        /// Returns |a1| if a2>= 0 
        /// Returns -|a1| if a2< 0
        /// </summary>
        /// <returns> Returns |a1| if a2>= 0, Returns -|a1| if a2< 0 </returns>
        public static int Sign(int a1, int a2)
        {
            if (a2 >= 0) return Math.Abs(a1);
            return -Math.Abs(a1);
        }

        /// <summary>
        /// Transfer of sign
        /// Returns |a1| if a2>= 0 
        /// Returns -|a1| if a2< 0
        /// </summary>
        /// <returns> Returns |a1| if a2>= 0, Returns -|a1| if a2< 0 </returns>
        public static float Sign(float a1, float a2)
        {
            if (a2 >= 0) return Math.Abs(a1);
            return -Math.Abs(a1);
        }

        /// <summary>
        /// Transfer of sign
        /// Returns |a1| if a2>= 0 
        /// Returns -|a1| if a2< 0
        /// </summary>
        /// <returns> Returns |a1| if a2>= 0, Returns -|a1| if a2< 0 </returns>
        public static double Sign(double a1, double a2)
        {
            if (a2 >= 0) return Math.Abs(a1);
            return -Math.Abs(a1);
        }


        #endregion


        #region MOD
        //Intrinsic Function     Definition     No.of Args.     Generic Name    Specific Names       Argument     Type
        //
        //Remainder           a1-int(a1/a2)*a2      2                MOD            MOD                 INTEGER        INTEGER
        //                                                                          AMOD                REAL           REAL
        //                                                                          DMOD                DOUBLE         DOUBLE


        /// <summary>
        /// Summary:
        ///     Returns the remainder resulting from the division of a specified number by
        ///     another specified number.
        ///
        /// Parameters:
        ///   y:
        ///     A divisor.
        ///
        ///   x:
        ///     A dividend.
        ///
        /// Returns:
        ///     A number equal to x - (y Q), where Q = Math.Truncate(x/y) 
        /// </summary>
        /// <param name="a1">A divisor.</param>
        /// <param name="a2">A dividend.</param>
        /// <returns>A number equal to x - (y Q), where Q = Math.Truncate(x/y)</returns>
        public static int Mod(int a1, int a2)
        {
            int val = a1 - a2 * (a1 / a2); // En este caso a1/a2 es automaticamente truncado debido a que los dos valores son enteros
            return val;
        }

        /// <summary>
        /// Summary:
        ///     Returns the remainder resulting from the division of a specified number by
        ///     another specified number.
        ///
        /// Parameters:
        ///   y:
        ///     A divisor.
        ///
        ///   x:
        ///     A dividend.
        ///
        /// Returns:
        ///     A number equal to x - (y Q), where Q = Math.Truncate(x/y) 
        /// </summary>
        /// <param name="a1">A divisor.</param>
        /// <param name="a2">A dividend.</param>
        /// <returns>A number equal to x - (y Q), where Q = Math.Truncate(x/y)</returns>
        public static float AMod(float a1, float a2)
        {
            float val = a1 - a2 * Convert.ToSingle(Math.Truncate(a1 / a2));
            return val;
        }

        /// <summary>
        /// Summary:
        ///     Returns the remainder resulting from the division of a specified number by
        ///     another specified number.
        ///
        /// Parameters:
        ///   y:
        ///     A divisor.
        ///
        ///   x:
        ///     A dividend.
        ///
        /// Returns:
        ///     A number equal to x - (y Q), where Q = Math.Truncate(x/y) 
        /// </summary>
        /// <param name="a1">A divisor.</param>
        /// <param name="a2">A dividend.</param>
        /// <returns>A number equal to x - (y Q), where Q = Math.Truncate(x/y)</returns>
        public static double DMod(double a1, double a2)
        {
            double val = a1 - a2 * Math.Truncate(a1 / a2);
            return val;
        }


        #endregion

        #region SCAN

        /// <summary>
        ///SCAN(STRING, SET, BACK)
        ///Scan a string for any one of the characters in a set of characters.
        ///Argument Type and Attributes
        ///STRING must be of type character.
        ///SET must be of type character with the same kind type parameter
        ///as STRING.
        ///    BACK (optional)
        ///must be of type logical.
        ///Class
        ///Elemental function
        ///Result Type and Attributes
        ///Default integer.
        ///Result Value
        ///v Case (i): If BACK is absent or is present with the value .FALSE. and if
        ///STRING contains at least one character that is in SET, the value of the result
        ///is the position of the leftmost character of STRING that is in SET.
        ///v Case (ii): If BACK is present with the value .TRUE. and if STRING contains
        ///at least one character that is in SET, the value of the result is the position of
        ///the rightmost character of STRING that is in SET.
        ///v Case (iii): The value of the result is zero if no character of STRING is in SET
        ///or if the length of STRING or SET is zero.
        ///Examples
        ///v Case (i): SCAN (’FORTRAN’, ’TR’) has the value 3.
        ///v Case (ii): SCAN (’FORTRAN’, ’TR’, BACK = .TRUE.) has the value 5.
        ///v Case (iii): SCAN (’FORTRAN’, ’BCD’) has the value 0.
        /// </summary>
        /// <remarks>
        ///   IBM XL Fortran for AIX
        /// </remarks>
        /// <param name="s"></param>
        /// <param name="value"></param>
        /// <param name="back"></param>
        /// <returns></returns>
        public static int SCAN(string s, string set, bool back)
        {
            int index = -1;
            if (back == false)
            {
                index = s.IndexOfAny(set.ToCharArray());
            }
            else
            {
                char[] chArry = s.ToCharArray();
                Array.Reverse(chArry);
                string invS = new string(chArry);
                int indexB = invS.IndexOfAny(set.ToCharArray());
                if (indexB != -1)
                {
                    index = s.Length - indexB - 1;
                }
            }
            index++; //Fortran index
            return index;
        }

        public static int SCAN(string s, string set)
        {
            return SCAN(s, set, false);
        }

        public static int SCAN(Characters charArray, string set, bool back)
        {
            return FortranLib.SCAN(charArray.ToString(), set, back);
        }

        public static int SCAN(Characters charArray, string set)
        {
            return FortranLib.SCAN(charArray.ToString(), set);
        }

        #endregion


        #region INDEX

        /// <summary>
        /// INDEX
        ///INDEX(STRING, SUBSTRING, BACK)
        ///Returns the starting position of a substring within a string.
        ///Argument Type and Attributes
        ///STRING must be of type character.
        ///SUBSTRING must be of type character with the same kind type parameter
        ///as STRING.
        ///BACK (optional)
        ///must be of type logical.
        ///Class
        ///Elemental function
        ///Result Type and Attributes
        ///Default integer.
        ///Result Value
        ///v Case (i): If BACK is absent or present with the value .FALSE., the result is
        ///the minimum positive value of I such that STRING (I : I + LEN
        ///(SUBSTRING) - 1) = SUBSTRING or zero if there is no such value. Zero is
        ///returned if LEN (STRING) .LT. LEN (SUBSTRING). One is returned if LEN
        ///(SUBSTRING) = 0.
        ///v Case (ii): If BACK is present with the value .TRUE., the result is the
        ///maximum value of I less than or equal to LEN (STRING) - LEN
        ///(SUBSTRING) + 1, such that STRING (I : I + LEN (SUBSTRING) - 1) =
        ///SUBSTRING or zero if there is no such value. Zero is returned if LEN
        ///(STRING) .LT. LEN (SUBSTRING) and LEN (STRING) + 1 is returned if LEN
        ///(SUBSTRING) = 0.
        ///Examples
        ///INDEX (’FORTRAN’, ’R’) has the value 3.
        ///INDEX (’FORTRAN’, ’R’, BACK = .TRUE.) has the value 5.
        /// </summary>
        /// <remarks>
        ///   IBM XL Fortran for AIX
        /// </remarks>
        /// <param name="s"></param>
        /// <param name="value"></param>
        /// <param name="back"></param>
        /// <returns></returns>    
        public static int INDEX(string s, string value, bool back)
        {
            int index=-1;
            if (back == false)
            {
                index = s.IndexOf(value);
            }
            else
            {
                index = s.LastIndexOf(value);
            }
            index++;
            return index;
        }

        public static int INDEX(string s, string value)
        {
            return INDEX(s, value, false);
        }

        public static int INDEX(Characters chars, string value, bool back)
        {
            return FortranLib.INDEX(chars.ToString(), value, back);
        }

        public static int INDEX(Characters chars, string value)
        {
            return FortranLib.INDEX(chars.ToString(), value, false);
        }

        public static int INDEX(Characters chars, Characters value, bool back)
        {
            return FortranLib.INDEX(chars.ToString(), value.ToString(), back);
        }

        public static int INDEX(Characters chars, Characters value)
        {
            return FortranLib.INDEX(chars.ToString(), value.ToString());
        }

        #endregion



        #region ADJUSTL


        /// <summary>
        ///ADJUSTL(STRING)
        ///Adjust to the left, removing leading blanks and inserting trailing blanks.
        ///Argument Type and Attributes
        ///STRING must be of type character.
        ///Class
        ///Elemental function
        ///Result Type and Attributes
        ///Character of the same length and kind type parameter as STRING.
        ///Result Value
        ///The value of the result is the same as STRING except that any leading blanks
        ///have been deleted and the same number of trailing blanks have been inserted.
        ///Examples
        ///ADJUSTL (’bWORD’) has the value ’WORDb’.
        /// </summary>
        /// <remarks>
        ///   IBM XL Fortran for AIX
        /// </remarks>
        /// <param name="s"></param>
        /// <returns></returns>
        public static string ADJUSTL(string s)
        {
            StringBuilder sBuilder = new StringBuilder(s.Length);
            sBuilder.Append(s.TrimStart());
            string spaces = new string(' ', s.Length - sBuilder.Length);
            sBuilder.Append(spaces);
            return sBuilder.ToString();
        }


        #endregion


        #region ADJUSTL


        /// <summary>
        ///ADJUSTR(STRING)
        ///Adjust to the right, removing trailing blanks and inserting leading blanks.
        ///Argument Type and Attributes
        ///STRING must be of type character.
        ///Class
        ///Elemental function
        ///Result Type and Attributes
        ///Character of the same length and kind type parameter as STRING.
        ///Result Value
        ///The value of the result is the same as STRING except that any trailing blanks
        ///have been deleted and the same number of leading blanks have been inserted.
        ///Examples
        ///ADJUSTR (’WORDb’) has the value ’bWORD’.
        /// </summary>
        /// <remarks>
        ///   IBM XL Fortran for AIX
        /// </remarks>
        /// <param name="s"></param>
        /// <returns></returns>
        public string ADJUSTR(string s)
        {
            StringBuilder sBuilder = new StringBuilder(s.Length);
            sBuilder.Append(s.TrimEnd());
            string spaces = new string(' ', s.Length - sBuilder.Length);
            sBuilder.Insert(0, spaces);
            return sBuilder.ToString();
        }

        #endregion

        #region Strings

        public static string Substring(string s, int startIndex, int lastIndex)
        {
            startIndex--;
            lastIndex--;
            int length = lastIndex - startIndex + 1;

            return s.Substring(startIndex, length);
        }

        public static string Substring(string s, int startIndex)
        {
            return FortranLib.Substring(s, startIndex, s.Length);
        }

        public static void Copy(ref string destinationString, int startIndex, int lastIndex, string sourceString)
        {
            char[] destinationArray = destinationString.ToCharArray();
            FortranLib.Copy(destinationArray, startIndex, lastIndex, sourceString.ToCharArray());
            destinationString = new string(destinationArray);
        }

        public static void Copy(ref string destinationString, int startIndex, int lastIndex, char source)
        {
            if (startIndex != lastIndex)
            {
                ArgumentException argEx = new ArgumentException("startIndex != lastIndex");
            }

            FortranLib.Copy(ref destinationString, startIndex, lastIndex, source.ToString());
        }

        public static void Copy(ref string destinationString, int startIndex, string sourceString)
        {
            int lastIndex = Math.Min(destinationString.Length, sourceString.Length);
            FortranLib.Copy(ref destinationString, startIndex, lastIndex, sourceString);
        }


        public static void Copy(ref string destinationString, string sourceString)
        {
            FortranLib.Copy(ref destinationString, 1, sourceString);
        }




        /// <summary>
        ///Retrieves a substring . The substring starts at a specified
        ///character position and finisch a specified position.
        /// </summary>
        /// <param name="s"></param>
        /// <param name="startIndex"></param>
        /// <param name="lastIndex"></param>
        /// <returns></returns>
        public static char[] Substring(char[] s, int startIndex, int lastIndex)
        {
            char[] substring = new char[lastIndex - startIndex + 1];

            startIndex--;
            lastIndex--;
            for (int i = startIndex; i < lastIndex; i++)
            {
                substring[i - startIndex] = s[i];
            }

            return substring;
        }

        /// <summary>
        ///Retrieves a substring . The substring starts at a specified
        ///character position.
        /// </summary>
        /// <param name="s"></param>
        /// <param name="startIndex"></param>
        /// <returns></returns>
        public static char[] Substring(char[] s, int startIndex)
        {
            return FortranLib.Substring(s, startIndex, s.Length);
        }


        public static void Copy(char[] destinationArray, int startIndex, int lastIndex ,char[] sourceArray)
        {
            startIndex--; // C# index
            int length = lastIndex - startIndex;
            length = Math.Min(length, sourceArray.Length);
            length = Math.Min(length, destinationArray.Length - startIndex);
            for (int i = 0; i < length; i++)
            {
                destinationArray[i + startIndex] = sourceArray[i];
            }
        }

        public static void Copy(char[] destinationArray, int startIndex, char[] sourceArray)
        {
            FortranLib.Copy(destinationArray, startIndex, destinationArray.Length, sourceArray);
        }

        public static void Copy(Characters destination, int startIndex, int lastIndex, Characters source)
        {
            FortranLib.Copy(destination.CharArray, startIndex, lastIndex, source.CharArray);
        }

        public static void Copy(Characters destination, int startIndex, Characters source)
        {
            FortranLib.Copy(destination.CharArray, startIndex, source.CharArray);
        }

        public static void Copy(Characters destination, Characters source)
        {
            FortranLib.Copy(destination.CharArray, 1, source.CharArray);
        }

        public static void Copy(Characters destination, int startIndex, int lastIndex, string source)
        {
            FortranLib.Copy(destination.CharArray, startIndex, lastIndex, source.ToCharArray());
        }

        public static void Copy(Characters destination, int startIndex, string source)
        {
            FortranLib.Copy(destination.CharArray, startIndex, source.ToCharArray());
        }

        public static void Copy(Characters destination, string source)
        {
            FortranLib.Copy(destination.CharArray, 1, source.ToCharArray());
        }

        #endregion




    }

}
