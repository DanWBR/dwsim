using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.Thermodynamics.BaseClasses;
using DWSIM.Thermodynamics.PropertyPackages;
using DWSIM.Thermodynamics.PropertyPackages.Auxiliary;
using Eto.Drawing;
using Eto.Forms;
using s = DWSIM.UI.Shared.Common;


namespace DWSIM.UI.Desktop.Editors
{
    public class PropertyPackageSettingsView : DynamicLayout
    {

        public IFlowsheet flowsheet;
        public PropertyPackage pp;

        public PropertyPackageSettingsView(IFlowsheet fs, PropertyPackage ppack): base()
        {
            flowsheet = fs;
            pp = ppack;
            Init();
        }

        void Init()
        {

            Padding = new Padding(10);

            var comps = flowsheet.SelectedCompounds.Values.Select((x) => x.Name).ToList();
            var nf = "N4";
            Double val;

            s.CreateAndAddLabelRow(this, "Interaction Parameters");

            switch (pp.ComponentName)
            {
                case "NRTL":
                    var ppn = (NRTLPropertyPackage)pp;
                    var ipn = ppn.m_uni.InteractionParameters;
                    foreach (var c1 in comps)
                    {
                        if (!ipn.ContainsKey(c1)) { ipn.Add(c1, new Dictionary<string, NRTL_IPData>()); }
                        foreach (var c2 in comps)
                        {
                            if (c1 != c2)
                            {
                                if (!ipn[c1].ContainsKey(c2))
                                {
                                    if (ipn.ContainsKey(c2) && !ipn[c2].ContainsKey(c1))
                                    { ipn[c1].Add(c2, new NRTL_IPData()); }
                                }
                            }
                        }
                    }

                    foreach (var c1 in comps)
                    {
                        foreach (var c2 in comps)
                        {
                            if (c1 != c2 && ipn[c1].ContainsKey(c2))
                            {
                                var ip = ipn[c1][c2];
                                s.CreateAndAddTextBoxRow(this, nf, c1 + "/" + c2 + " A12", ip.A12,
                                 (arg3, arg2) =>
                                 {
                                     if (Double.TryParse(arg3.Text.ToString(), out val))
                                     {
                                         arg3.TextColor = (SystemColors.ControlText);
                                         ip.A12 = Double.Parse(arg3.Text);
                                     }
                                     else
                                     {
                                         arg3.TextColor = (Colors.Red);
                                     }
                                 });
                                s.CreateAndAddTextBoxRow(this, nf, c1 + "/" + c2 + " A21", ip.A21,
                                 (arg3, arg2) =>
                                 {
                                     if (Double.TryParse(arg3.Text.ToString(), out val))
                                     {
                                         arg3.TextColor = (SystemColors.ControlText);
                                         ip.A21 = Double.Parse(arg3.Text);
                                     }
                                     else
                                     {
                                         arg3.TextColor = (Colors.Red);
                                     }
                                 });
                                s.CreateAndAddTextBoxRow(this, nf, c1 + "/" + c2 + " B12", ip.B12,
                                 (arg3, arg2) =>
                                 {
                                     if (Double.TryParse(arg3.Text.ToString(), out val))
                                     {
                                         arg3.TextColor = (SystemColors.ControlText);
                                         ip.B12 = Double.Parse(arg3.Text);
                                     }
                                     else
                                     {
                                         arg3.TextColor = (Colors.Red);
                                     }
                                 });
                                s.CreateAndAddTextBoxRow(this, nf, c1 + "/" + c2 + " B21", ip.B21,
                                 (arg3, arg2) =>
                                 {
                                     if (Double.TryParse(arg3.Text.ToString(), out val))
                                     {
                                         arg3.TextColor = (SystemColors.ControlText);
                                         ip.B21 = Double.Parse(arg3.Text);
                                     }
                                     else
                                     {
                                         arg3.TextColor = (Colors.Red);
                                     }
                                 });
                                s.CreateAndAddTextBoxRow(this, nf, c1 + "/" + c2 + " C12", ip.C12,
                                 (arg3, arg2) =>
                                 {
                                     if (Double.TryParse(arg3.Text.ToString(), out val))
                                     {
                                         arg3.TextColor = (SystemColors.ControlText);
                                         ip.C12 = Double.Parse(arg3.Text);
                                     }
                                     else
                                     {
                                         arg3.TextColor = (Colors.Red);
                                     }
                                 });
                                s.CreateAndAddTextBoxRow(this, nf, c1 + "/" + c2 + " C21", ip.C21,
                                 (arg3, arg2) =>
                                 {
                                     if (Double.TryParse(arg3.Text.ToString(), out val))
                                     {
                                         arg3.TextColor = (SystemColors.ControlText);
                                         ip.C21 = Double.Parse(arg3.Text);
                                     }
                                     else
                                     {
                                         arg3.TextColor = (Colors.Red);
                                     }
                                 });
                                s.CreateAndAddTextBoxRow(this, nf, c1 + "/" + c2 + " alpha12", ip.alpha12,
                                 (arg3, arg2) =>
                                 {
                                     if (Double.TryParse(arg3.Text.ToString(), out val))
                                     {
                                         arg3.TextColor = (SystemColors.ControlText);
                                         ip.alpha12 = Double.Parse(arg3.Text);
                                     }
                                     else
                                     {
                                         arg3.TextColor = (Colors.Red);
                                     }
                                 });
                            }
                        }
                    }
                    break;
                case "UNIQUAC":
                    var ppu = (UNIQUACPropertyPackage)pp;
                    var ipu = ppu.m_uni.InteractionParameters;
                    foreach (var c1 in comps)
                    {
                        if (!ipu.ContainsKey(c1)) { ipu.Add(c1, new Dictionary<string, UNIQUAC_IPData>()); }
                        foreach (var c2 in comps)
                        {
                            if (c1 != c2)
                            {
                                if (!ipu[c1].ContainsKey(c2))
                                {
                                    if (ipu.ContainsKey(c2) && !ipu[c2].ContainsKey(c1))
                                    { ipu[c1].Add(c2, new UNIQUAC_IPData()); }
                                }
                            }
                        }
                    }

                    foreach (var c1 in comps)
                    {
                        foreach (var c2 in comps)
                        {
                            if (c1 != c2 && ipu[c1].ContainsKey(c2))
                            {
                                var ip = ipu[c1][c2];
                                s.CreateAndAddTextBoxRow(this, nf, c1 + "/" + c2 + " A12", ip.A12,
                                 (arg3, arg2) =>
                                 {
                                     if (Double.TryParse(arg3.Text.ToString(), out val))
                                     {
                                         arg3.TextColor = (SystemColors.ControlText);
                                         ip.A12 = Double.Parse(arg3.Text);
                                     }
                                     else
                                     {
                                         arg3.TextColor = (Colors.Red);
                                     }
                                 });
                                s.CreateAndAddTextBoxRow(this, nf, c1 + "/" + c2 + " A21", ip.A21,
                                 (arg3, arg2) =>
                                 {
                                     if (Double.TryParse(arg3.Text.ToString(), out val))
                                     {
                                         arg3.TextColor = (SystemColors.ControlText);
                                         ip.A21 = Double.Parse(arg3.Text);
                                     }
                                     else
                                     {
                                         arg3.TextColor = (Colors.Red);
                                     }
                                 });
                                s.CreateAndAddTextBoxRow(this, nf, c1 + "/" + c2 + " B12", ip.B12,
                               (arg3, arg2) =>
                               {
                                   if (Double.TryParse(arg3.Text.ToString(), out val))
                                   {
                                       arg3.TextColor = (SystemColors.ControlText);
                                       ip.B12 = Double.Parse(arg3.Text);
                                   }
                                   else
                                   {
                                       arg3.TextColor = (Colors.Red);
                                   }
                               });
                                s.CreateAndAddTextBoxRow(this, nf, c1 + "/" + c2 + " B21", ip.B21,
                                 (arg3, arg2) =>
                                 {
                                     if (Double.TryParse(arg3.Text.ToString(), out val))
                                     {
                                         arg3.TextColor = (SystemColors.ControlText);
                                         ip.B21 = Double.Parse(arg3.Text);
                                     }
                                     else
                                     {
                                         arg3.TextColor = (Colors.Red);
                                     }
                                 });
                                s.CreateAndAddTextBoxRow(this, nf, c1 + "/" + c2 + " C12", ip.C12,
                                 (arg3, arg2) =>
                                 {
                                     if (Double.TryParse(arg3.Text.ToString(), out val))
                                     {
                                         arg3.TextColor = (SystemColors.ControlText);
                                         ip.C12 = Double.Parse(arg3.Text);
                                     }
                                     else
                                     {
                                         arg3.TextColor = (Colors.Red);
                                     }
                                 });
                                s.CreateAndAddTextBoxRow(this, nf, c1 + "/" + c2 + " C21", ip.C21,
                                 (arg3, arg2) =>
                                 {
                                     if (Double.TryParse(arg3.Text.ToString(), out val))
                                     {
                                         arg3.TextColor = (SystemColors.ControlText);
                                         ip.C21 = Double.Parse(arg3.Text);
                                     }
                                     else
                                     {
                                         arg3.TextColor = (Colors.Red);
                                     }
                                 });
                            }
                        }
                    }
                    break;
                case "Peng-Robinson (PR)":
                    var ppr = (PengRobinsonPropertyPackage)pp;
                    var ipc = ppr.m_pr.InteractionParameters;
                    foreach (var c1 in comps)
                    {
                        if (!ipc.ContainsKey(c1)) { ipc.Add(c1, new Dictionary<string, PR_IPData>()); }
                        foreach (var c2 in comps)
                        {
                            if (c1 != c2)
                            {
                                if (!ipc[c1].ContainsKey(c2))
                                {
                                    if (ipc.ContainsKey(c2) && !ipc[c2].ContainsKey(c1))
                                    { ipc[c1].Add(c2, new PR_IPData()); }
                                }
                            }
                        }
                    }

                    foreach (var c1 in comps)
                    {
                        foreach (var c2 in comps)
                        {
                            if (c1 != c2 && ipc[c1].ContainsKey(c2))
                            {
                                var ip = ipc[c1][c2];
                                s.CreateAndAddTextBoxRow(this, nf, c1 + "/" + c2, ip.kij,
                                 (arg3, arg2) =>
                                 {
                                     if (Double.TryParse(arg3.Text.ToString(), out val))
                                     {
                                         arg3.TextColor = (SystemColors.ControlText);
                                         ip.kij = Double.Parse(arg3.Text);
                                     }
                                     else
                                     {
                                         arg3.TextColor = (Colors.Red);
                                     }
                                 });
                            }
                        }
                    }
                    break;
                case "Soave-Redlich-Kwong (SRK)":
                    var pps = (SRKPropertyPackage)pp;
                    var ipc2 = pps.m_pr.InteractionParameters;
                    foreach (var c1 in comps)
                    {
                        if (!ipc2.ContainsKey(c1)) { ipc2.Add(c1, new Dictionary<string, PR_IPData>()); }
                        foreach (var c2 in comps)
                        {
                            if (c1 != c2)
                            {
                                if (!ipc2[c1].ContainsKey(c2))
                                {
                                    if (ipc2.ContainsKey(c2) && !ipc2[c2].ContainsKey(c1))
                                    { ipc2[c1].Add(c2, new PR_IPData()); }
                                }
                            }
                        }
                    }

                    foreach (var c1 in comps)
                    {
                        foreach (var c2 in comps)
                        {
                            if (c1 != c2 && ipc2[c1].ContainsKey(c2))
                            {
                                var ip = ipc2[c1][c2];
                                s.CreateAndAddTextBoxRow(this, nf, c1 + "/" + c2, ip.kij,
                                 (arg3, arg2) =>
                                 {
                                     if (Double.TryParse(arg3.Text.ToString(), out val))
                                     {
                                         arg3.TextColor = (SystemColors.ControlText);
                                         ip.kij = Double.Parse(arg3.Text);
                                     }
                                     else
                                     {
                                         arg3.TextColor = (Colors.Red);
                                     }
                                 });
                            }
                        }
                    }
                    break;
                case "Lee-Kesler-Plöcker":
                    var ppl = (LKPPropertyPackage)pp;
                    var ipl = ppl.m_lk.InteractionParameters;
                    foreach (var c1 in comps)
                    {
                        if (!ipl.ContainsKey(c1)) { ipl.Add(c1, new Dictionary<string, LKP_IPData>()); }
                        foreach (var c2 in comps)
                        {
                            if (c1 != c2)
                            {
                                if (!ipl[c1].ContainsKey(c2))
                                {
                                    if (ipl.ContainsKey(c2) && !ipl[c2].ContainsKey(c1))
                                    { ipl[c1].Add(c2, new LKP_IPData()); }
                                }
                            }
                        }
                    }

                    foreach (var c1 in comps)
                    {
                        foreach (var c2 in comps)
                        {
                            if (c1 != c2 && ipl[c1].ContainsKey(c2))
                            {
                                var ip = ipl[c1][c2];
                                s.CreateAndAddTextBoxRow(this, nf, c1 + "/" + c2, ip.kij,
                                 (arg3, arg2) =>
                                 {
                                     if (Double.TryParse(arg3.Text.ToString(), out val))
                                     {
                                         arg3.TextColor = (SystemColors.ControlText);
                                         ip.kij = Double.Parse(arg3.Text);
                                     }
                                     else
                                     {
                                         arg3.TextColor = (Colors.Red);
                                     }
                                 });
                            }
                        }
                    }
                    break;

            }

            s.CreateAndAddLabelRow(this, "Model Configuration");

            var parkeys = pp.Parameters.Keys.ToList();

            foreach (var par in parkeys)
            {
                s.CreateAndAddTextBoxRow(this, "R",
                                         flowsheet.GetTranslatedString(par), Double.Parse(pp.Parameters[par].ToString()),
                         (arg3, arg2) =>
                         {
                             if (Double.TryParse(arg3.Text.ToString(), out val))
                             {
                                 arg3.TextColor = (SystemColors.ControlText);
                                 pp.Parameters[par] = Double.Parse(arg3.Text);
                             }
                             else
                             {
                                 arg3.TextColor = (Colors.Red);
                             }
                         });
            }
        }
    }
}