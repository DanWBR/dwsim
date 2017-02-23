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
    public class VPTPropertyPackage : AdvEOSPropertyPackageBase
    {

        public Dictionary<string, Dictionary<string, VPT_IP>> InteractionParameters = new Dictionary<string, Dictionary<string, VPT_IP>>();

        public VPTPropertyPackage()
        {
            PropertyPackageModel = Model.VPT;
            ComponentName = "Valderrama-Patel-Teja EOS (VPT)";
            ComponentDescription = "";
            IsConfigurable = true;
        }

        public override void DisplayEditingForm()
        {
            VPT_Editor editor = new VPT_Editor() { PP = this };
            editor.Show();
        }

        protected override string GetModelSpecificParameters()
        {
            StringWriter contents = new StringWriter();

            System.Globalization.CultureInfo ci = System.Globalization.CultureInfo.InvariantCulture;

            int i = 0;
            foreach (ICompound c in CurrentMaterialStream.Phases[0].Compounds.Values)
            {
                string cname = "comp" + i.ToString();
                contents.WriteLine(cname + ".EoSParam(1) = " + c.ConstantProperties.Critical_Compressibility.ToString(ci) + ";");
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
                            contents.WriteLine("mix.k1(" + (compounds.IndexOf(c1) + 1) + "," + (compounds.IndexOf(c2) + 1) + ") = '" + InteractionParameters[c1][c2].k1.ToString(ci) + "';");
                            contents.WriteLine("mix.k1(" + (compounds.IndexOf(c2) + 1) + "," + (compounds.IndexOf(c1) + 1) + ") = '" + InteractionParameters[c1][c2].k1.ToString(ci) + "';");
                            contents.WriteLine("mix.k2(" + (compounds.IndexOf(c1) + 1) + "," + (compounds.IndexOf(c2) + 1) + ") = '" + InteractionParameters[c1][c2].k1.ToString(ci) + "';");
                            contents.WriteLine("mix.k2(" + (compounds.IndexOf(c2) + 1) + "," + (compounds.IndexOf(c1) + 1) + ") = '" + InteractionParameters[c1][c2].k1.ToString(ci) + "';");
                            contents.WriteLine("mix.k3(" + (compounds.IndexOf(c1) + 1) + "," + (compounds.IndexOf(c2) + 1) + ") = '" + InteractionParameters[c1][c2].k1.ToString(ci) + "';");
                            contents.WriteLine("mix.k3(" + (compounds.IndexOf(c2) + 1) + "," + (compounds.IndexOf(c1) + 1) + ") = '" + InteractionParameters[c1][c2].k1.ToString(ci) + "';");
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
            foreach (KeyValuePair<string, Dictionary<string, VPT_IP>> kvp in InteractionParameters)
            {
                foreach (KeyValuePair<string, VPT_IP> kvp2 in kvp.Value)
                {
                    data[data.Count - 1].Add(new XElement("InteractionParameter",
                        new XAttribute("Compound1", kvp2.Value.Compound1),
                        new XAttribute("Compound2", kvp2.Value.Compound1),
                        new XAttribute("k1", kvp2.Value.k1.ToString(ci)),
                        new XAttribute("k2", kvp2.Value.k2.ToString(ci)),
                        new XAttribute("k3", kvp2.Value.k3.ToString(ci))));
                }
            }
            return data;
        }

        public override bool LoadData(List<System.Xml.Linq.XElement> data)
        {

            base.LoadData(data);

            System.Globalization.CultureInfo ci = System.Globalization.CultureInfo.InvariantCulture;

            foreach (XElement xel in (from xel2 in data where xel2.Name == "InteractionParameters" select xel2).SingleOrDefault().Elements().ToList())
            {
                VPT_IP ip = new VPT_IP
                {
                    Compound1 = xel.Attribute("Compound1").Value,
                    Compound2 = xel.Attribute("Compound2").Value,
                    k1 = double.Parse(xel.Attribute("k1").Value, ci),
                    k2 = double.Parse(xel.Attribute("k2").Value, ci),
                    k3 = double.Parse(xel.Attribute("k3").Value, ci)
                };
                Dictionary<string, VPT_IP> dic = new Dictionary<string, VPT_IP>();
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

            return true;
        }


    }
}
