using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Drawing.SkiaSharp.GraphicObjects;
using DWSIM.Interfaces;

namespace DWSIM.Drawing.SkiaSharp.Extended
{
    public class Shared
    {
        public static object ReturnInstance(string typename)
        {
            Type t = Type.GetType(typename, false);
            if ((t != null))
                return Activator.CreateInstance(t);
            else
                return null;
        }
    }
}
