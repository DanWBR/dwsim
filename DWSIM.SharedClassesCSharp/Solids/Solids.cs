using DWSIM.Interfaces;
using System.Collections.Generic;
using System.Xml.Linq;

namespace DWSIM.SharedClassesCSharp.Solids
{
    public class AdditionalSolidPhaseProperties : IAdditionalSolidPhaseProperties, ICustomXMLSerialization
    {
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

    public class SolidParticleSizeDistribution : ISolidParticleSizeDistribution, ICustomXMLSerialization
    {
        public string UniqueID { get; set; } = System.Guid.NewGuid().ToString();

        public string Name { get; set; } = "";

        public string Description { get; set; } = "";

        public List<ISolidShapeCurve> Curves { get; set; } = new List<ISolidShapeCurve>();

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

    public class SolidParticleData : ISolidParticleData, ICustomXMLSerialization
    {
        public string UniqueID { get; set; } = System.Guid.NewGuid().ToString();

        public string Name { get; set; } = "";

        public string Description { get; set; } = "";

        public Dictionary<string, ISolidParticleSizeDistribution> Distributions { get; set; } = new Dictionary<string, ISolidParticleSizeDistribution>();

        public ISolidParticleData Clone()
        {
            var clone = new SolidParticleData();
            clone.LoadData(SaveData());
            return clone;
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

}
