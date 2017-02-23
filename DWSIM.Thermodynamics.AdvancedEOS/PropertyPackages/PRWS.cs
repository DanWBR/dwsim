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
using System;

namespace DWSIM.Thermodynamics.AdvancedEOS
{
    public class PRWSPropertyPackage : AdvEOSPropertyPackageBase
    {

        public Dictionary<string, Dictionary<string, PHSC_IP>> InteractionParameters = new Dictionary<string, Dictionary<string, PHSC_IP>>();
        public Dictionary<string, Dictionary<string, NRTL_IPData>> InteractionParametersNRTL = new Dictionary<string, Dictionary<string, NRTL_IPData>>();

        public PRWSPropertyPackage()
        {
            PropertyPackageModel = Model.PRWS;
            ComponentName = "Peng-Robinson w/ Wong-Sandler Mixing Rules (PRWS)";
            ComponentDescription = "";
            IsConfigurable = true;
            ReadParameters();
        }

        public override void DisplayEditingForm()
        {
            PRWS_Editor editor = new PRWS_Editor() { PP = this };
            editor.Show();
        }

        private void ReadParameters()
        {

            char pathsep = System.IO.Path.DirectorySeparatorChar;

            NRTL_IPData[] nrtlipc = null;
            FileHelperEngine<NRTL_IPData> fh1 = new FileHelperEngine<NRTL_IPData>();

            using (System.IO.Stream filestr = System.Reflection.Assembly.GetAssembly(this.GetType()).GetManifestResourceStream("DWSIM.Thermodynamics.AdvancedEOS.Data.nrtl.dat"))
            {
                using (System.IO.StreamReader t = new System.IO.StreamReader(filestr))
                {
                    nrtlipc = fh1.ReadStream(t);
                }
            }

            ChemSepHelper.ChemSepIDConverter csdb = new ChemSepHelper.ChemSepIDConverter();

            //load ChemSep database interactions
            foreach (NRTL_IPData nrtlip in nrtlipc)
            {
                if (this.InteractionParametersNRTL.ContainsKey(csdb.GetDWSIMName(nrtlip.ID1.ToString()).ToString()))
                {
                    if (!this.InteractionParametersNRTL[csdb.GetDWSIMName(nrtlip.ID1.ToString()).ToString()].ContainsKey(csdb.GetDWSIMName(nrtlip.ID2.ToString()).ToString()))
                    {
                        this.InteractionParametersNRTL[csdb.GetDWSIMName(nrtlip.ID1.ToString()).ToString()].Add(csdb.GetDWSIMName(nrtlip.ID2.ToString()).ToString(), (NRTL_IPData)nrtlip.Clone());
                    }
                    if (!this.InteractionParametersNRTL[csdb.GetDWSIMName(nrtlip.ID1.ToString()).ToString()].ContainsKey(csdb.GetCSName(nrtlip.ID2.ToString()).ToString()))
                    {
                        this.InteractionParametersNRTL[csdb.GetDWSIMName(nrtlip.ID1.ToString()).ToString()].Add(csdb.GetCSName(nrtlip.ID2.ToString()).ToString(), (NRTL_IPData)nrtlip.Clone());
                    }
                }
                else
                {
                    this.InteractionParametersNRTL.Add(csdb.GetDWSIMName(nrtlip.ID1.ToString()).ToString(), new Dictionary<string, NRTL_IPData>());
                    this.InteractionParametersNRTL[csdb.GetDWSIMName(nrtlip.ID1.ToString()).ToString()].Add(csdb.GetDWSIMName(nrtlip.ID2.ToString()).ToString(), (NRTL_IPData)nrtlip.Clone());
                    if (!this.InteractionParametersNRTL[csdb.GetDWSIMName(nrtlip.ID1.ToString()).ToString()].ContainsKey(csdb.GetCSName(nrtlip.ID2.ToString()).ToString()))
                    {
                        this.InteractionParametersNRTL[csdb.GetDWSIMName(nrtlip.ID1.ToString()).ToString()].Add(csdb.GetCSName(nrtlip.ID2.ToString()).ToString(), (NRTL_IPData)nrtlip.Clone());
                    }
                }
            }

            foreach (NRTL_IPData nrtlip in nrtlipc)
            {
                if (this.InteractionParametersNRTL.ContainsKey(csdb.GetCSName(nrtlip.ID1.ToString()).ToString()))
                {
                    if (!this.InteractionParametersNRTL[csdb.GetCSName(nrtlip.ID1.ToString()).ToString()].ContainsKey(csdb.GetCSName(nrtlip.ID2.ToString()).ToString()))
                    {
                        this.InteractionParametersNRTL[csdb.GetCSName(nrtlip.ID1.ToString()).ToString()].Add(csdb.GetCSName(nrtlip.ID2.ToString()).ToString(), (NRTL_IPData)nrtlip.Clone());
                    }
                    if (!this.InteractionParametersNRTL[csdb.GetCSName(nrtlip.ID1.ToString()).ToString()].ContainsKey(csdb.GetDWSIMName(nrtlip.ID2.ToString()).ToString()))
                    {
                        this.InteractionParametersNRTL[csdb.GetCSName(nrtlip.ID1.ToString()).ToString()].Add(csdb.GetDWSIMName(nrtlip.ID2.ToString()).ToString(), (NRTL_IPData)nrtlip.Clone());
                    }
                }
                else
                {
                    this.InteractionParametersNRTL.Add(csdb.GetCSName(nrtlip.ID1.ToString()).ToString(), new Dictionary<string, NRTL_IPData>());
                    this.InteractionParametersNRTL[csdb.GetCSName(nrtlip.ID1.ToString()).ToString()].Add(csdb.GetCSName(nrtlip.ID2.ToString()).ToString(), (NRTL_IPData)nrtlip.Clone());
                    if (!this.InteractionParametersNRTL[csdb.GetCSName(nrtlip.ID1.ToString()).ToString()].ContainsKey(csdb.GetDWSIMName(nrtlip.ID2.ToString()).ToString()))
                    {
                        this.InteractionParametersNRTL[csdb.GetCSName(nrtlip.ID1.ToString()).ToString()].Add(csdb.GetDWSIMName(nrtlip.ID2.ToString()).ToString(), (NRTL_IPData)nrtlip.Clone());
                    }
                }
            }

            System.Globalization.CultureInfo ci = System.Globalization.CultureInfo.InvariantCulture;

            //load user database interactions
            if ((GlobalSettings.Settings.UserInteractionsDatabases != null))
            {
                foreach (string IPDBPath in GlobalSettings.Settings.UserInteractionsDatabases)
                {
                    BaseClasses.InteractionParameter[] Interactions = null;
                    try
                    {
                        Interactions = Databases.UserIPDB.ReadInteractions(IPDBPath, "NRTL");
                        foreach (BaseClasses.InteractionParameter IP in Interactions)
                        {
                            NRTL_IPData IPD = new NRTL_IPData();
                            IPD.A12 = Convert.ToDouble(IP.Parameters["A12"], ci);
                            IPD.A21 = Convert.ToDouble(IP.Parameters["A21"], ci);
                            IPD.alpha12 = Convert.ToDouble(IP.Parameters["alpha12"], ci);
                            IPD.comment = IP.Description;

                            if (this.InteractionParameters.ContainsKey(IP.Comp1))
                            {
                                if (this.InteractionParametersNRTL[IP.Comp1].ContainsKey(IP.Comp2))
                                {
                                }
                                else
                                {
                                    this.InteractionParametersNRTL[IP.Comp1].Add(IP.Comp2, (NRTL_IPData)IPD.Clone());
                                }
                            }
                            else
                            {
                                this.InteractionParametersNRTL.Add(IP.Comp1, new Dictionary<string, NRTL_IPData>());
                                this.InteractionParametersNRTL[IP.Comp1].Add(IP.Comp2, (NRTL_IPData)IPD.Clone());
                            }
                        }
                    }
                    catch
                    {

                    }
                }
            }


            nrtlipc = null;
            fh1 = null;

            PR_IPData[] pripc = null;
            FileHelperEngine<PR_IPData> fh2 = new FileHelperEngine<PR_IPData>();

            using (System.IO.Stream filestr = System.Reflection.Assembly.GetAssembly(this.GetType()).GetManifestResourceStream("DWSIM.Thermodynamics.AdvancedEOS.Data.pr_ip.dat"))
            {
                using (System.IO.StreamReader t = new System.IO.StreamReader(filestr))
                {
                    pripc = fh2.ReadStream(t);
                }
            }

            foreach (PR_IPData prip in pripc)
            {
                var c1 = csdb.GetCSName(prip.ID1.ToString()).ToString();
                var c2 = csdb.GetCSName(prip.ID2.ToString()).ToString();
                if (this.InteractionParameters.ContainsKey(c1))
                {
                    if (this.InteractionParameters[c1].ContainsKey(c2))
                    {
                    }
                    else
                    {
                        this.InteractionParameters[c1].Add(c2, new PHSC_IP() { Compound1 = c1, Compound2 = c2, kij = prip.kij });
                    }
                }
                else
                {
                    this.InteractionParameters.Add(c1, new Dictionary<string, PHSC_IP>());
                    this.InteractionParameters[c1].Add(c2, new PHSC_IP() { Compound1 = c1, Compound2 = c2, kij = prip.kij });
                }
            }
            foreach (PR_IPData prip in pripc)
            {
                var c1 = csdb.GetCSName(prip.ID1.ToString()).ToString();
                var c2 = csdb.GetCSName(prip.ID2.ToString()).ToString();
                if (this.InteractionParameters.ContainsKey(c1))
                {
                    if (this.InteractionParameters[c1].ContainsKey(c2))
                    {
                    }
                    else
                    {
                        this.InteractionParameters[c1].Add(c2, new PHSC_IP() { Compound1 = c1, Compound2 = c2, kij = prip.kij });
                    }
                }
                else
                {
                    this.InteractionParameters.Add(c1, new Dictionary<string, PHSC_IP>());
                    this.InteractionParameters[c1].Add(c2, new PHSC_IP() { Compound1 = c1, Compound2 = c2, kij = prip.kij });
                }
            }
            pripc = null;
            fh2 = null;

        }

        protected override string GetModelSpecificParameters()
        {
            return "";
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
                            contents.WriteLine("mix.k1(" + (compounds.IndexOf(c1) + 1) + "," + (compounds.IndexOf(c2) + 1) + ") = '" + InteractionParameters[c1][c2].kij.ToString(ci) + "';");
                            contents.WriteLine("mix.k1(" + (compounds.IndexOf(c2) + 1) + "," + (compounds.IndexOf(c1) + 1) + ") = '" + InteractionParameters[c1][c2].kij.ToString(ci) + "';");
                        }
                    }

                }
            }
            contents.WriteLine("");

            foreach (string c1 in compounds)
            {
                foreach (string c2 in compounds)
                {
                    if (InteractionParametersNRTL.ContainsKey(c1))
                    {
                        if (InteractionParametersNRTL[c1].ContainsKey(c2))
                        {
                            contents.WriteLine("mix.k2(" + (compounds.IndexOf(c1) + 1) + "," + (compounds.IndexOf(c2) + 1) + ") = '" + InteractionParametersNRTL[c1][c2].alpha12.ToString(ci) + "';");
                            contents.WriteLine("mix.k2(" + (compounds.IndexOf(c2) + 1) + "," + (compounds.IndexOf(c1) + 1) + ") = '" + InteractionParametersNRTL[c1][c2].alpha12.ToString(ci) + "';");
                        }
                    }

                }
            }
            contents.WriteLine("");

            foreach (string c1 in compounds)
            {
                foreach (string c2 in compounds)
                {
                    if (InteractionParametersNRTL.ContainsKey(c1))
                    {
                        if (InteractionParametersNRTL[c1].ContainsKey(c2))
                        {
                            contents.WriteLine("mix.k3(" + (compounds.IndexOf(c1) + 1) + "," + (compounds.IndexOf(c2) + 1) + ") = '" + InteractionParametersNRTL[c1][c2].A12.ToString(ci) + "';");
                            contents.WriteLine("mix.k3(" + (compounds.IndexOf(c2) + 1) + "," + (compounds.IndexOf(c1) + 1) + ") = '" + InteractionParametersNRTL[c1][c2].A21.ToString(ci) + "';");
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

            data.Add(new XElement("InteractionParametersPR"));
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
            data.Add(new XElement("InteractionParametersNRTL"));
            foreach (KeyValuePair<string, Dictionary<string, NRTL_IPData>> kvp in InteractionParametersNRTL)
            {
                foreach (KeyValuePair<string, NRTL_IPData> kvp2 in kvp.Value)
                {
                    data[data.Count - 1].Add(new XElement("InteractionParameter",
                        new XAttribute("Compound1", kvp2.Value.ID1),
                        new XAttribute("Compound2", kvp2.Value.ID2),
                        new XAttribute("A12", kvp2.Value.A12.ToString(ci)),
                        new XAttribute("A21", kvp2.Value.A21.ToString(ci)),
                        new XAttribute("alpha12", kvp2.Value.alpha12.ToString(ci))));
                }
            }
            return data;
        }

        public override bool LoadData(List<System.Xml.Linq.XElement> data)
        {

            base.LoadData(data);

            System.Globalization.CultureInfo ci = System.Globalization.CultureInfo.InvariantCulture;

            foreach (XElement xel in (from xel2 in data where xel2.Name == "InteractionParametersPR" select xel2).SingleOrDefault().Elements().ToList())
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

            foreach (XElement xel in (from xel2 in data where xel2.Name == "InteractionParametersNRTL" select xel2).SingleOrDefault().Elements().ToList())
            {
                NRTL_IPData ip = new NRTL_IPData
                {
                    ID1 = xel.Attribute("Compound1").Value,
                    ID2 = xel.Attribute("Compound2").Value,
                    A12 = double.Parse(xel.Attribute("A12").Value, ci),
                    A21 = double.Parse(xel.Attribute("A21").Value, ci),
                    alpha12 = double.Parse(xel.Attribute("alpha12").Value, ci)
                };
                Dictionary<string, NRTL_IPData> dic = new Dictionary<string, NRTL_IPData>();
                dic.Add(xel.Attribute("Compound1").Value, ip);
                if (!this.InteractionParametersNRTL.ContainsKey(xel.Attribute("Compound1").Value))
                {
                    this.InteractionParametersNRTL.Add(xel.Attribute("Compound1").Value, dic);
                }
                else
                {
                    if (!this.InteractionParametersNRTL[xel.Attribute("Compound1").Value].ContainsKey(xel.Attribute("Compound2").Value))
                    {
                        this.InteractionParametersNRTL[xel.Attribute("Compound1").Value].Add(xel.Attribute("Compound2").Value, ip);
                    }
                    else
                    {
                        this.InteractionParametersNRTL[xel.Attribute("Compound1").Value][xel.Attribute("Compound2").Value] = ip;
                    }
                }
            }

            return true;
        }

    }
}


