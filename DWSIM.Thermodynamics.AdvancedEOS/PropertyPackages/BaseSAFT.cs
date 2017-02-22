using System.Collections.Generic;
using DWSIM.Thermodynamics.PropertyPackages.Auxiliary;
using System.IO;
using System.Reflection;
using FileHelpers;
using DWSIM.Thermodynamics.AdvancedEOS.EditingForms;
using DWSIM.Interfaces;

namespace DWSIM.Thermodynamics.AdvancedEOS
{
    public abstract class BaseSAFTPropertyPackage : AdvEOSPropertyPackageBase
    {

        public Dictionary<string, PCSParam> CompoundParameters = new Dictionary<string, PCSParam>();
        public Dictionary<string, Dictionary<string, PCSIP>> InteractionParameters = new Dictionary<string, Dictionary<string, PCSIP>>();

        public override void DisplayEditingForm()
        {
            PCSAFT_Editor editor = new PCSAFT_Editor() { PP = this };
            editor.Show();
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
                        string[] split = new string[] { System.Environment.NewLine };
                        contents.WriteLine(cname + ".EoSParam(4) = " + CompoundParameters[c.ConstantProperties.CAS_Number].associationparams.Split(split, System.StringSplitOptions.RemoveEmptyEntries)[0] + ";");
                        contents.WriteLine(cname + ".EoSParam(5) = " + CompoundParameters[c.ConstantProperties.CAS_Number].associationparams.Split(split, System.StringSplitOptions.RemoveEmptyEntries)[1] + ";");
                        contents.WriteLine(cname + ".EoSParam(6) = " + CompoundParameters[c.ConstantProperties.CAS_Number].associationparams.Split(split, System.StringSplitOptions.RemoveEmptyEntries)[2] + ";");
                    }
                    contents.WriteLine("");
                }
                i += 1;
            }
            contents.WriteLine("");

            return contents.ToString();

        }

        protected override string GetModelInteractionParameters()
        {

            StringWriter contents = new StringWriter();

            System.Globalization.CultureInfo ci = System.Globalization.CultureInfo.InvariantCulture;

            int i = 1;
            int j = 1;
            foreach (ICompound c in CurrentMaterialStream.Phases[0].Compounds.Values)
            {
                foreach (ICompound c2 in CurrentMaterialStream.Phases[0].Compounds.Values)
                {
                    if (InteractionParameters.ContainsKey(c.ConstantProperties.CAS_Number))
                    {
                        if (InteractionParameters[c.ConstantProperties.CAS_Number].ContainsKey(c2.ConstantProperties.CAS_Number))
                        {
                            if (i != j)
                            {
                                contents.WriteLine("mix.k(" + i + "," + j + ") = '" + InteractionParameters[c.ConstantProperties.CAS_Number][c2.ConstantProperties.CAS_Number].kij.ToString(ci) + "';");
                                contents.WriteLine("mix.k(" + j + "," + i + ") = '" + InteractionParameters[c.ConstantProperties.CAS_Number][c2.ConstantProperties.CAS_Number].kij.ToString(ci) + "';");
                            }
                        }
                    }
                    j += 1;
                }
                i += 1;
            }
            contents.WriteLine("");

            return contents.ToString();

        }

    }
}