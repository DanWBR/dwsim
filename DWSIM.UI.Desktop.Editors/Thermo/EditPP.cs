using System;
using System.Collections.Generic;
using System.Linq;
using DWSIM.Interfaces;
using DWSIM.Thermodynamics.PropertyPackages;
using DWSIM.Thermodynamics.PropertyPackages.Auxiliary;
using Eto.Drawing;
using Eto.Forms;
using s = DWSIM.UI.Shared.Common;
using DWSIM.UI.Shared;
using DWSIM.ExtensionMethods;

namespace DWSIM.UI.Desktop.Editors
{
    public class PropertyPackageIPView : DynamicLayout
    {

        public IFlowsheet flowsheet;
        public PropertyPackage pp;

        public PropertyPackageIPView(IFlowsheet fs, PropertyPackage ppack): base()
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
            
        }
    }

    public class PropertyPackageSettingsView : DynamicLayout
    {

        public IFlowsheet flowsheet;
        public PropertyPackage pp;

        public PropertyPackageSettingsView(IFlowsheet fs, PropertyPackage ppack) : base()
        {
            flowsheet = fs;
            pp = ppack;
            Init();
        }

        void Init()
        {

            Padding = new Padding(10);

            this.CreateAndAddLabelRow("Liquid Phase Density");

            this.CreateAndAddDropDownRow("Calculation Method", pp.LiquidDensityCalculationMode_Subcritical.GetEnumNames(),
                (int)pp.LiquidDensityCalculationMode_Subcritical, (dd, e) => {
                    pp.LiquidDensityCalculationMode_Subcritical = dd.SelectedIndex.ToEnum<PropertyPackage.LiquidDensityCalcMode>();
                    pp.LiquidDensityCalculationMode_Supercritical = dd.SelectedIndex.ToEnum<PropertyPackage.LiquidDensityCalcMode>();
                }, null);

            this.CreateAndAddCheckBoxRow("Correct Experimental Data for Pressure", pp.LiquidDensity_CorrectExpDataForPressure, (chk, e) => {
                pp.LiquidDensity_CorrectExpDataForPressure = chk.Checked.GetValueOrDefault();
            }, null);

            var c1 = this.CreateAndAddCheckBoxRow("Use Peneloux Volume Translation Coefficient (PR/SRK EOS only)", pp.LiquidDensity_UsePenelouxVolumeTranslation, (chk, e) => {
                pp.LiquidDensity_UsePenelouxVolumeTranslation = chk.Checked.GetValueOrDefault();
            }, null);

            c1.Enabled = pp is PengRobinsonPropertyPackage || pp is SRKPropertyPackage;

            this.CreateAndAddLabelRow("Liquid Phase Viscosity");

            this.CreateAndAddDropDownRow("Calculation Method", pp.LiquidViscosityCalculationMode_Subcritical.GetEnumNames(),
                (int)pp.LiquidViscosityCalculationMode_Subcritical, (dd, e) => {
                    pp.LiquidViscosityCalculationMode_Subcritical = dd.SelectedIndex.ToEnum<PropertyPackage.LiquidViscosityCalcMode>();
                    pp.LiquidViscosityCalculationMode_Supercritical = dd.SelectedIndex.ToEnum<PropertyPackage.LiquidViscosityCalcMode>();
                }, null);

            this.CreateAndAddDropDownRow("Mixing Rule", pp.LiquidViscosity_MixingRule.GetEnumNames(),
                (int)pp.LiquidViscosity_MixingRule, (dd, e) => {
                    pp.LiquidViscosity_MixingRule = dd.SelectedIndex.ToEnum<PropertyPackage.LiquidViscosityMixRule>();
                }, null);

            this.CreateAndAddCheckBoxRow("Correct Experimental Data for Pressure", pp.LiquidViscosity_CorrectExpDataForPressure, (chk, e) => {
                pp.LiquidViscosity_CorrectExpDataForPressure = chk.Checked.GetValueOrDefault();
            }, null);

            this.CreateAndAddLabelRow("Fugacity Calculation");

            var c3 = this.CreateAndAddDropDownRow("Vapor Phase Fugacity", pp.VaporPhaseFugacityCalculationMode.GetEnumNames(),
                (int)pp.VaporPhaseFugacityCalculationMode, (dd, e) => {
                    pp.VaporPhaseFugacityCalculationMode = dd.SelectedIndex.ToEnum<PropertyPackage.VaporPhaseFugacityCalcMode>();
                }, null);

            c3.Enabled = pp is ActivityCoefficientPropertyPackage;

            var c2 = this.CreateAndAddCheckBoxRow("Liquid Phase: Use Poynting Correction Factor", pp.LiquidFugacity_UsePoyntingCorrectionFactor, (chk, e) => {
                pp.LiquidFugacity_UsePoyntingCorrectionFactor = chk.Checked.GetValueOrDefault();
            }, null);

            c2.Enabled = pp is ActivityCoefficientPropertyPackage;

            this.CreateAndAddLabelRow("Enthalpy, Entropy, Cp and Cv");

            var c4 = this.CreateAndAddDropDownRow("Calculation Method", pp.EnthalpyEntropyCpCvCalculationMode.GetEnumNames(),
                (int)pp.EnthalpyEntropyCpCvCalculationMode, (dd, e) => {
                    pp.EnthalpyEntropyCpCvCalculationMode = dd.SelectedIndex.ToEnum<PropertyPackage.EnthalpyEntropyCpCvCalcMode>();
                }, null);

            c4.Enabled = pp is ActivityCoefficientPropertyPackage;

            var c4a = this.CreateAndAddDropDownRow("Calculation Method (EOS-based models)", pp.LiquidEnthalpyEntropyCpCvCalculationMode_EOS.GetEnumNames(),
                (int)pp.LiquidEnthalpyEntropyCpCvCalculationMode_EOS, (dd, e) => {
                    pp.LiquidEnthalpyEntropyCpCvCalculationMode_EOS = dd.SelectedIndex.ToEnum<PropertyPackage.LiquidEnthalpyEntropyCpCvCalcMode_EOS>();
                }, null);

            c4a.Enabled = pp.PackageType == PackageType.EOS;

            this.CreateAndAddLabelRow("Other");

            var c5 = this.CreateAndAddCheckBoxRow("Ignore Missing UNIQUAC/NRTL Interaction Parameters", pp.ActivityCoefficientModels_IgnoreMissingInteractionParameters, (chk, e) => {
                pp.ActivityCoefficientModels_IgnoreMissingInteractionParameters = chk.Checked.GetValueOrDefault();
            }, null);

            c5.Enabled = pp is UNIQUACPropertyPackage || pp is NRTLPropertyPackage;

            var c5a = this.CreateAndAddCheckBoxRow("Automatically Estimate Missing UNIQUAC/NRTL Interaction Parameters", pp.AutoEstimateMissingNRTLUNIQUACParameters, (chk, e) => {
                pp.AutoEstimateMissingNRTLUNIQUACParameters = chk.Checked.GetValueOrDefault();
            }, null);

            c5a.Enabled = pp is UNIQUACPropertyPackage || pp is NRTLPropertyPackage;

            var c6 = this.CreateAndAddCheckBoxRow("Ignore Maximum Salinity Limit (Seawater Model only)", pp.IgnoreSalinityLimit, (chk, e) => {
                pp.IgnoreSalinityLimit = chk.Checked.GetValueOrDefault();
            }, null);

            c6.Enabled = pp is SeawaterPropertyPackage;

            var c7 = this.CreateAndAddCheckBoxRow("Ignore Vapor Fraction Bounds (Sour Water Model only)", pp.IgnoreVaporFractionLimit, (chk, e) => {
                pp.IgnoreVaporFractionLimit = chk.Checked.GetValueOrDefault();
            }, null);

            c7.Enabled = pp is SourWaterPropertyPackage;

        }
    }
}