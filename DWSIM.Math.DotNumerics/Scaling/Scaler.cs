using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNumerics.Scaling
{
    public class Scaler
    {
        
        public static double Scale(double value, double min, double max, double minScale, double maxScale)
        {
            double scaled = minScale + (value - min) / (max - min) * (maxScale - minScale);
            if (double.IsNaN(scaled) || double.IsInfinity(scaled)) scaled = minScale;
            return scaled;
        }

        public static double UnScale(float scaledvalue, double min, double max, double minScale, double maxScale)
        {
            double unscaled = min + (scaledvalue - minScale) * (max - min) / (maxScale - minScale);
            if (double.IsNaN(unscaled) || double.IsInfinity(unscaled)) unscaled = min;
            return unscaled;
        }

    }
}
