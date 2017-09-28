using System.Collections.Generic;
using DWSIM.Thermodynamics.PropertyPackages.Auxiliary;
using System.IO;
using System.Reflection;
using FileHelpers;
using DWSIM.Thermodynamics.AdvancedEOS.EditingForms;
using DWSIM.Interfaces;
using System.Xml.Linq;
using System.Linq;

namespace DWSIM.Thermodynamics.AdvancedEOS
{
    public abstract class BaseSAFTPropertyPackage : AdvEOSPropertyPackageBase
    {

        public Dictionary<string, PCSParam> CompoundParameters = new Dictionary<string, PCSParam>();
        public Dictionary<string, Dictionary<string, PCSIP>> InteractionParameters = new Dictionary<string, Dictionary<string, PCSIP>>();

        public override void DisplayEditingForm()
        {
            PCSAFT_Editor editor = new PCSAFT_Editor() { PP = this };
            editor.ShowDialog();
        }

        protected void ReadParameters()
        {

            char pathsep = System.IO.Path.DirectorySeparatorChar;

            PCSParam[] pcsaftdatac = null;
            FileHelperEngine<PCSParam> fh1 = new FileHelperEngine<PCSParam>();

            var res = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceNames();

            using (Stream filestr = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("DWSIM.Thermodynamics.AdvancedEOS.Data.pcsaft.dat"))
            {
                using (StreamReader t = new StreamReader(filestr))
                {
                    pcsaftdatac = fh1.ReadStream(t);
                }
            }

            foreach (PCSParam pcsaftdata in pcsaftdatac)
            {
                if (pcsaftdata.kAiBi != 0d)
                {
                    pcsaftdata.associationparams = "2\n[0 " + pcsaftdata.kAiBi + "; " + pcsaftdata.kAiBi + " 0]\n[0 " + pcsaftdata.epsilon2 + "; " + pcsaftdata.epsilon2 + " 0]";
                }
                if (!CompoundParameters.ContainsKey(pcsaftdata.casno))
                    CompoundParameters.Add(pcsaftdata.casno, pcsaftdata);
            }

            fh1 = null;

            PCSIP[] pripc = null;
            FileHelperEngine<PCSIP> fh2 = new FileHelperEngine<PCSIP>();

            using (Stream filestr = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("DWSIM.Thermodynamics.AdvancedEOS.Data.pcsaft_ip.dat"))
            {
                using (StreamReader t = new StreamReader(filestr))
                {
                    pripc = fh2.ReadStream(t);
                }
            }

            foreach (PCSIP ip in pripc)
            {
                if (InteractionParameters.ContainsKey(ip.casno1))
                {
                    if (InteractionParameters[ip.casno1].ContainsKey(ip.casno2))
                    {
                    }
                    else
                    {
                        InteractionParameters[ip.casno1].Add(ip.casno2, (PCSIP)ip.Clone());
                    }
                }
                else
                {
                    InteractionParameters.Add(ip.casno1, new Dictionary<string, PCSIP>());
                    InteractionParameters[ip.casno1].Add(ip.casno2, (PCSIP)ip.Clone());
                }
            }
            foreach (PCSIP ip in pripc)
            {
                if (InteractionParameters.ContainsKey(ip.casno1))
                {
                    if (InteractionParameters[ip.casno1].ContainsKey(ip.casno2))
                    {
                    }
                    else
                    {
                        InteractionParameters[ip.casno1].Add(ip.casno2, (PCSIP)ip.Clone());
                    }
                }
                else
                {
                    InteractionParameters.Add(ip.casno1, new Dictionary<string, PCSIP>());
                    InteractionParameters[ip.casno1].Add(ip.casno2, (PCSIP)ip.Clone());
                }
            }

            pripc = null;
            fh2 = null;

        }

        protected override string GetModelSpecificParameters()
        {

            StringWriter contents = new StringWriter();

            System.Globalization.CultureInfo ci = System.Globalization.CultureInfo.InvariantCulture;

            int i = 0;
            foreach (ICompound c in CurrentMaterialStream.Phases[0].Compounds.Values)
            {
                string cname = "comp" + i.ToString();
                if (CompoundParameters.ContainsKey(c.ConstantProperties.CAS_Number))
                {
                    contents.WriteLine(cname + ".EoSParam(1) = " + CompoundParameters[c.ConstantProperties.CAS_Number].m.ToString(ci) + ";");
                    contents.WriteLine(cname + ".EoSParam(2) = " + CompoundParameters[c.ConstantProperties.CAS_Number].sigma.ToString(ci) + ";");
                    contents.WriteLine(cname + ".EoSParam(3) = " + CompoundParameters[c.ConstantProperties.CAS_Number].epsilon.ToString(ci) + ";");
                    if (CompoundParameters[c.ConstantProperties.CAS_Number].associationparams != "")
                    {
                        string[] split = new string[] { "\n" };
                        contents.WriteLine(cname + ".EoSParam(4) = " + CompoundParameters[c.ConstantProperties.CAS_Number].associationparams.Split(split, System.StringSplitOptions.RemoveEmptyEntries)[0] + ";");
                        contents.WriteLine(cname + ".EoSParam(5) = " + CompoundParameters[c.ConstantProperties.CAS_Number].associationparams.Split(split, System.StringSplitOptions.RemoveEmptyEntries)[1] + ";");
                        contents.WriteLine(cname + ".EoSParam(6) = " + CompoundParameters[c.ConstantProperties.CAS_Number].associationparams.Split(split, System.StringSplitOptions.RemoveEmptyEntries)[2] + ";");
                    }
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

            var compounds = CurrentMaterialStream.Phases[0].Compounds.Values.Select(x => x.ConstantProperties.CAS_Number).ToList();

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
            foreach (KeyValuePair<string, Dictionary<string, PCSIP>> kvp in InteractionParameters)
            {
                foreach (KeyValuePair<string, PCSIP> kvp2 in kvp.Value)
                {
                    if ((this.CurrentMaterialStream != null))
                    {
                        if (this.CurrentMaterialStream.Phases[0].Compounds.ContainsKey(kvp.Key) & this.CurrentMaterialStream.Phases[0].Compounds.ContainsKey(kvp2.Key))
                        {
                            data[data.Count - 1].Add(new XElement("InteractionParameter",
                                new XAttribute("Compound1", kvp2.Value.compound1),
                                new XAttribute("Compound2", kvp2.Value.compound2),
                                new XAttribute("CAS1", kvp.Key),
                                new XAttribute("CAS2", kvp2.Key),
                                new XAttribute("Value", kvp2.Value.kij.ToString(ci))));
                        }
                    }
                }
            }
            data.Add(new XElement("CompoundParameters"));
            foreach (KeyValuePair<string, PCSParam> kvp in CompoundParameters)
            {
                if ((this.CurrentMaterialStream != null))
                {
                    if (this.CurrentMaterialStream.Phases[0].Compounds.ContainsKey(kvp.Key))
                    {
                        data[data.Count - 1].Add(new XElement("CompoundParameterSet",
                                new XAttribute("Compound", kvp.Value.compound),
                                new XAttribute("CAS_ID", kvp.Value.casno),
                                new XAttribute("MW", kvp.Value.mw.ToString(ci)),
                                new XAttribute("m", kvp.Value.m.ToString(ci)),
                                new XAttribute("sigma", kvp.Value.sigma.ToString(ci)),
                                new XAttribute("epsilon_k", kvp.Value.epsilon.ToString(ci)),
                                new XAttribute("assocparam", kvp.Value.associationparams.Replace(System.Environment.NewLine, "|"))));
                    }
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
                PCSIP ip = new PCSIP
                {
                    compound1 = xel.Attribute("Compound1").Value,
                    compound2 = xel.Attribute("Compound2").Value,
                    casno1 = xel.Attribute("CAS1").Value,
                    casno2 = xel.Attribute("CAS2").Value,
                    kij = double.Parse(xel.Attribute("Value").Value, ci)
                };
                Dictionary<string, PCSIP> dic = new Dictionary<string, PCSIP>();
                dic.Add(xel.Attribute("CAS1").Value, ip);
                if (!this.InteractionParameters.ContainsKey(xel.Attribute("CAS1").Value))
                {
                    this.InteractionParameters.Add(xel.Attribute("CAS1").Value, dic);
                }
                else
                {
                    if (!this.InteractionParameters[xel.Attribute("CAS1").Value].ContainsKey(xel.Attribute("CAS2").Value))
                    {
                        this.InteractionParameters[xel.Attribute("CAS1").Value].Add(xel.Attribute("CAS2").Value, ip);
                    }
                    else
                    {
                        this.InteractionParameters[xel.Attribute("CAS1").Value][xel.Attribute("CAS2").Value] = ip;
                    }
                }
            }

            foreach (XElement xel in (from xel2 in data where xel2.Name == "CompoundParameters" select xel2).SingleOrDefault().Elements().ToList())
            {
                PCSParam param = new PCSParam
                {
                    compound = xel.Attribute("Compound").Value,
                    casno = xel.Attribute("CAS_ID").Value,
                    mw = double.Parse(xel.Attribute("MW").Value, ci),
                    m = double.Parse(xel.Attribute("m").Value, ci),
                    sigma = double.Parse(xel.Attribute("sigma").Value, ci),
                    epsilon = double.Parse(xel.Attribute("epsilon_k").Value, ci),
                    associationparams = xel.Attribute("assocparam").Value.Replace("|", System.Environment.NewLine)
                };
                if (!this.CompoundParameters.ContainsKey(xel.Attribute("CAS_ID").Value))
                {
                    this.CompoundParameters.Add(xel.Attribute("CAS_ID").Value, param);
                }
                else
                {
                    this.CompoundParameters[xel.Attribute("CAS_ID").Value] = param;
                }
            }

            return true;
        }

    }
}