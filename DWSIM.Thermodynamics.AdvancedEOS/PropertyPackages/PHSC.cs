using System.Collections.Generic;
using DWSIM.Thermodynamics.PropertyPackages.Auxiliary;
using System.IO;
using System.Reflection;
using FileHelpers;
using DWSIM.Thermodynamics.AdvancedEOS.EditingForms;
using DWSIM.Interfaces;
using System.Xml.Linq;
using System.Linq;
using DWSIM.Thermodynamics.AdvancedEOS.Auxiliary;

namespace DWSIM.Thermodynamics.AdvancedEOS
{
    public class PHSCPropertyPackage : AdvEOSPropertyPackageBase
    {

        public Dictionary<string, PHSC_Param> CompoundParameters = new Dictionary<string, PHSC_Param>();
        public Dictionary<string, Dictionary<string, PHSC_IP>> InteractionParameters = new Dictionary<string, Dictionary<string, PHSC_IP>>();

        public PHSCPropertyPackage()
        {
            PropertyPackageModel = Model.PHSC;
            ComponentName = "Perturbed Hard-Sphere-Chain (PHSC)";
            ComponentDescription = "The perturbed hard-sphere-chain (PHSC) equation of state was developed for normal fluids and polymers, including their mixtures.";
            IsConfigurable = true;
        }

        public override void DisplayEditingForm()
        {
            PHSC_Editor editor = new PHSC_Editor() { PP = this };
            editor.ShowDialog();
        }

        protected override string GetModelSpecificParameters()
        {

            StringWriter contents = new StringWriter();

            System.Globalization.CultureInfo ci = System.Globalization.CultureInfo.InvariantCulture;

            int i = 0;
            foreach (ICompound c in CurrentMaterialStream.Phases[0].Compounds.Values)
            {
                string cname = "comp" + i.ToString();
                if (CompoundParameters.ContainsKey(c.ConstantProperties.Name))
                {
                    contents.WriteLine(cname + ".EoSParam(1) = " + CompoundParameters[c.ConstantProperties.Name].V.ToString(ci) + ";");
                    contents.WriteLine(cname + ".EoSParam(2) = " + CompoundParameters[c.ConstantProperties.Name].A.ToString(ci) + ";");
                    contents.WriteLine(cname + ".EoSParam(3) = " + CompoundParameters[c.ConstantProperties.Name].E.ToString(ci) + ";");
                    contents.WriteLine("");
                }
                i += 1;
            }

            return contents.ToString();

        }

        protected override string GetModelInteractionParameters()
        {

            StringWriter contents = new StringWriter();

            System.Globalization.CultureInfo ci = System.Globalization.CultureInfo.InvariantCulture;

            var compounds = CurrentMaterialStream.Phases[0].Compounds.Values.Select(x => x.ConstantProperties.Name).ToList();

            foreach (string c1 in compounds)
            {
                foreach (string c2 in compounds)
                {
                    if (InteractionParameters.ContainsKey(c1))
                    {
                        if (InteractionParameters[c1].ContainsKey(c2))
                        {
                            contents.WriteLine("mix.k(" + (compounds.IndexOf(c1) + 1) + "," + (compounds.IndexOf(c2) + 1) + ") = '" + InteractionParameters[c1][c2].kij.ToString(ci) + "';");
                            contents.WriteLine("mix.k(" + (compounds.IndexOf(c2) + 1) + "," + (compounds.IndexOf(c1) + 1) + ") = '" + InteractionParameters[c1][c2].kij.ToString(ci) + "';");
                        }
                    }

                }
            }
            contents.WriteLine("");

            return contents.ToString();

        }

        public override List<System.Xml.Linq.XElement> SaveData()
        {
            var data = base.SaveData();

            System.Globalization.CultureInfo ci = System.Globalization.CultureInfo.InvariantCulture;

            data.Add(new XElement("InteractionParameters"));
            foreach (KeyValuePair<string, Dictionary<string, PHSC_IP>> kvp in InteractionParameters)
            {
                foreach (KeyValuePair<string, PHSC_IP> kvp2 in kvp.Value)
                {
                    data[data.Count - 1].Add(new XElement("InteractionParameter",
                        new XAttribute("Compound1", kvp2.Value.Compound1),
                        new XAttribute("Compound2", kvp2.Value.Compound2),
                        new XAttribute("Value", kvp2.Value.kij.ToString(ci))));
                }
            }
            data.Add(new XElement("CompoundParameters"));
            foreach (KeyValuePair<string, PHSC_Param> kvp in CompoundParameters)
            {
                data[data.Count - 1].Add(new XElement("CompoundParameterSet",
                        new XAttribute("Compound", kvp.Value.Compound),
                        new XAttribute("CAS_ID", kvp.Value.CAS_ID),
                        new XAttribute("V", kvp.Value.V.ToString(ci)),
                        new XAttribute("A", kvp.Value.A.ToString(ci)),
                        new XAttribute("E", kvp.Value.E.ToString(ci))));
            }
            return data;
        }

        public override bool LoadData(List<System.Xml.Linq.XElement> data)
        {

            base.LoadData(data);

            System.Globalization.CultureInfo ci = System.Globalization.CultureInfo.InvariantCulture;

            foreach (XElement xel in (from xel2 in data where xel2.Name == "InteractionParameters" select xel2).SingleOrDefault().Elements().ToList())
            {
                PHSC_IP ip = new PHSC_IP
                {
                    Compound1 = xel.Attribute("Compound1").Value,
                    Compound2 = xel.Attribute("Compound2").Value,
                    kij = double.Parse(xel.Attribute("Value").Value, ci)
                };
                Dictionary<string, PHSC_IP> dic = new Dictionary<string, PHSC_IP>();
                dic.Add(xel.Attribute("Compound1").Value, ip);
                if (!this.InteractionParameters.ContainsKey(xel.Attribute("Compound1").Value))
                {
                    this.InteractionParameters.Add(xel.Attribute("Compound1").Value, dic);
                }
                else
                {
                    if (!this.InteractionParameters[xel.Attribute("Compound1").Value].ContainsKey(xel.Attribute("Compound2").Value))
                    {
                        this.InteractionParameters[xel.Attribute("Compound1").Value].Add(xel.Attribute("Compound2").Value, ip);
                    }
                    else
                    {
                        this.InteractionParameters[xel.Attribute("Compound1").Value][xel.Attribute("Compound2").Value] = ip;
                    }
                }
            }

            foreach (XElement xel in (from xel2 in data where xel2.Name == "CompoundParameters" select xel2).SingleOrDefault().Elements().ToList())
            {
                PHSC_Param param = new PHSC_Param
                {
                    Compound = xel.Attribute("Compound").Value,
                    CAS_ID = xel.Attribute("CAS_ID").Value,
                    V = double.Parse(xel.Attribute("V").Value, ci),
                    A = double.Parse(xel.Attribute("A").Value, ci),
                    E = double.Parse(xel.Attribute("E").Value, ci)
                };
                if (!this.CompoundParameters.ContainsKey(xel.Attribute("Compound").Value))
                {
                    this.CompoundParameters.Add(xel.Attribute("Compound").Value, param);
                }
                else
                {
                    this.CompoundParameters[xel.Attribute("Compound").Value] = param;
                }
            }

            return true;
        }

    }
}

