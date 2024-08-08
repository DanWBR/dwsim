using DWSIM.Interfaces;
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
           return Data.Select(dp => dp.Size).Average();
        }

        public double GetMeanDiameter()
        {
            var diameters = Data.Select(dp => dp.Size).ToList();
            return MathNet.Numerics.Statistics.Statistics.Mean(diameters);
        }

        public double GetDiameterStdDev()
        {
            var diameters = Data.Select(dp => dp.Size).ToList();
            return MathNet.Numerics.Statistics.Statistics.StandardDeviation(diameters);
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
            var ordered = Data.OrderByDescending(k => k.Size).ToList();
            var xd = ordered.Select(k => k.Size).ToList();
            var yd = ordered.Select(k => k.MassFraction).ToList();
            var min = Data.Select(d => d.Size).Min();
            var max = Data.Select(d => d.Size).Max();
            if (x < min)
                return 0.0;
            else if (x > max)
                return 0.0;
            else
            {
                double value = MathNet.Numerics.Interpolate.Common(xd, yd).Interpolate(x);
                return value;
            }
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
                    catch{ }
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

        public Dictionary<string, ISolidParticleSizeDistribution> InternalDistributions { get; set; } = new  Dictionary<string, ISolidParticleSizeDistribution>();

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
                        InternalDistributions.Add(obj.Name, obj);
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
            foreach (var dist in InternalDistributions.Values)
            {
                cx.Add(new XElement("InternalDistribution", ((ICustomXMLSerialization)dist).SaveData().ToArray()));
            }
            return data;
        }
    }

}
