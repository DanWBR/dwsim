using System.Collections.Generic;
using DWSIM.Thermodynamics.PropertyPackages.Auxiliary;
using System.IO;
using System.Reflection;
using FileHelpers;
using DWSIM.Thermodynamics.AdvancedEOS.EditingForms;

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
            return "";
        }

    }
}