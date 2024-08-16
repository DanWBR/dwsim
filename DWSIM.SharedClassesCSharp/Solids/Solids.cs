using DWSIM.Interfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
using System.Xml.Linq;
using static System.Net.Mime.MediaTypeNames;

namespace DWSIM.SharedClassesCSharp.Solids
{
    public class AdditionalSolidPhaseProperties : IAdditionalSolidPhaseProperties, ICustomXMLSerialization
    {
        public IAdditionalSolidPhaseProperties Clone()
        {
            var obj = new AdditionalSolidPhaseProperties();
            obj.LoadData(this.SaveData());
            return obj;
        }

        public bool LoadData(List<XElement> data)
        {
            XMLSerializer.XMLSerializer.Deserialize(this, data);
            return true;
        }

        public List<XElement> SaveData()
        {
            return XMLSerializer.XMLSerializer.Serialize(this);
        }
    }

    public class SolidParticleSize : ISolidParticleSize, ICustomXMLSerialization
    {
        public double Size { get; set; } = 0.0;

        public double MassFraction { get; set; } = 0.0;

        public double RelativeSize { get; set; } = 0.0;

        public double RelativeMassFraction { get; set; } = 0.0;

        public bool LoadData(List<XElement> data)
        {
            XMLSerializer.XMLSerializer.Deserialize(this, data);
            return true;
        }

        public List<XElement> SaveData()
        {
            return XMLSerializer.XMLSerializer.Serialize(this);
        }
    }

    public class SolidShapeCurve : ISolidShapeCurve, ICustomXMLSerialization
    {
        public string UniqueID { get; set; } = System.Guid.NewGuid().ToString();

        public string Name { get; set; } = "";

        public string Description { get; set; } = "";

        public string Shape { get; set; } = "";

        public List<ISolidParticleSize> Data { get; set; } = new List<ISolidParticleSize>();

        public double GetAverageDiameter()
        {
            return Data.Select(dp => dp.Size * dp.MassFraction).Sum() / Data.Select(dp => dp.MassFraction).Sum();
        }

        public double GetMeanDiameter()
        {
            var diameters = Data.Select(dp => dp.Size * dp.MassFraction).ToList();
            return diameters.Sum();
        }

        public double GetDiameterStdDev()
        {
            var mean = GetMeanDiameter();
            var stddev = Math.Sqrt(Data.Select(dp => Math.Pow(dp.Size - mean, 2) * dp.MassFraction).Sum());
            return stddev;
        }

        public bool LoadData(List<XElement> data)
        {
            XMLSerializer.XMLSerializer.Deserialize(this, data);
            if (data.Last().Name == "Data")
            {
                var cdata = data.Last().Elements().ToList();
                foreach (XElement xel in cdata)
                {
                    try
                    {
                        var obj = new SolidParticleSize();
                        obj.LoadData(xel.Elements().ToList());
                        Data.Add(obj);
                    }
                    catch { }
                }
            }
            return true;
        }

        public List<XElement> SaveData()
        {
            var data = XMLSerializer.XMLSerializer.Serialize(this);
            data.Add(new XElement("Data"));
            var cx = data.Last();
            foreach (var d in Data)
            {
                cx.Add(new XElement("Data", ((ICustomXMLSerialization)d).SaveData().ToArray()));
            }
            return data;
        }

        public ISolidShapeCurve Clone()
        {
            var obj = new SolidShapeCurve();
            obj.LoadData(this.SaveData());
            return obj;
        }

        public double GetValue(double x)
        {
            var ordered = Data.OrderBy(k => k.Size).ToList();
            var xd = ordered.Select(k => k.Size).ToList();
            var yd = ordered.Select(k => k.MassFraction).ToList();
            var min = Data.Select(d => d.Size).Min();
            var max = Data.Select(d => d.Size).Max();
            double value = 0.0;
            if (x < min)
                return 0.0;
            else if (x > max)
                return 0.0;
            else
            {
                double x1, x2, y1, y2;

                if (x < xd[0] || x > xd[xd.Count - 1])
                    return 0.0;
                else
                {
                    for (int i = 0; i < xd.Count - 1; i++)
                    {
                        if (x >= xd[i])
                        {
                            x1 = xd[i]; x2 = xd[i+1]; y1 = yd[i]; y2 = yd[i+1];
                            value = MathNet.Numerics.Interpolate.Linear(new[] { x1, x2 }, new[] { y1, y2 }).Interpolate(x);
                        }
                    }
                }

            }
            //value = MathNet.Numerics.Interpolate.Common(xd, yd).Interpolate(x);
            return value;
        }

        public void Update()
        {
            if (Data.Count < 2) return;
            for (int i = 1; i < Data.Count; i++)
            {
                Data[i].RelativeSize = Data[i].Size - Data[i-1].Size;
                Data[i].RelativeMassFraction = Math.Abs(Data[i].MassFraction - Data[i-1].MassFraction);
            }
        }

        public double GetRelativeValue(double x, double x0)
        {
            var y = GetValue(x);
            var y0 = GetValue(x0);
            return y - y0;
        }
    }

    public class SolidParticleSizeDistribution : ISolidParticleSizeDistribution, ICustomXMLSerialization
    {
        public string UniqueID { get; set; } = System.Guid.NewGuid().ToString();

        public string Name { get; set; } = "";

        public string Description { get; set; } = "";

        public List<ISolidShapeCurve> Curves { get; set; } = new List<ISolidShapeCurve>();

        public bool LoadData(List<XElement> data)
        {
            XMLSerializer.XMLSerializer.Deserialize(this, data);
            if (data.Last().Name == "Curves")
            {
                var cdata = data.Last().Elements().ToList();
                foreach (XElement xel in cdata)
                {
                    try
                    {
                        var obj = new SolidShapeCurve();
                        obj.LoadData(xel.Elements().ToList());
                        Curves.Add(obj);
                    }
                    catch { }
                }
            }
            return true;
        }

        public List<XElement> SaveData()
        {
            var data = XMLSerializer.XMLSerializer.Serialize(this);
            data.Add(new XElement("Curves"));
            var cx = data.Last();
            foreach (var curve in Curves)
            {
                cx.Add(new XElement("Curve", ((ICustomXMLSerialization)curve).SaveData().ToArray()));
            }
            return data;
        }

        public ISolidParticleSizeDistribution Clone()
        {
            var obj = new SolidParticleSizeDistribution();
            obj.LoadData(this.SaveData());
            return obj;
        }

    }

    public class SolidParticleData : ISolidParticleData, ICustomXMLSerialization
    {
        public string UniqueID { get; set; } = System.Guid.NewGuid().ToString();

        public string Name { get; set; } = "";

        public string Description { get; set; } = "";

        public Dictionary<string, string> Distributions { get; set; } = new Dictionary<string, string>();

        public Dictionary<string, ISolidParticleSizeDistribution> InternalDistributions { get; set; } = new Dictionary<string, ISolidParticleSizeDistribution>();

        public bool Calculated { get; set; } = false;

        public ISolidParticleData Clone()
        {
            var clone = new SolidParticleData();
            clone.LoadData(SaveData());
            return clone;
        }

        public bool LoadData(List<XElement> data)
        {
            XMLSerializer.XMLSerializer.Deserialize(this, data);
            if (data.Last().Name == "InternalDistributions")
            {
                var cdata = data.Last().Elements().ToList();
                foreach (XElement xel in cdata)
                {
                    try
                    {
                        var obj = new SolidParticleSizeDistribution();
                        obj.LoadData(xel.Elements().ToList());
                        if (xel.Attribute("Key") != null)
                        {
                            InternalDistributions.Add(xel.Attribute("Key").Value, obj);
                        }
                        else
                        {
                            InternalDistributions.Add(obj.Name, obj);
                        }
                    }
                    catch { }
                }
            }
            return true;
        }

        public List<XElement> SaveData()
        {
            var data = XMLSerializer.XMLSerializer.Serialize(this);
            data.Add(new XElement("InternalDistributions"));
            var cx = data.Last();
            foreach (var dist in InternalDistributions)
            {
                cx.Add(new XElement("InternalDistribution", ((ICustomXMLSerialization)dist.Value).SaveData().ToArray()));
                cx.Elements().Last().SetAttributeValue("Key", dist.Key);
            }
            return data;
        }
    }

}
