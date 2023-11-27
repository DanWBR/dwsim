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
using Eto.Forms;
using s = DWSIM.UI.Shared.Common;
using DWSIM.UI.Shared;
using Eto.Drawing;
using DWSIM.ExtensionMethods;
using DWSIM.Thermodynamics.Databases.ChemeoLink;

namespace DWSIM.UI.Desktop.Editors
{
    public class ConversionReaction
    {

        private static double sf = GlobalSettings.Settings.UIScalingFactor;

        public IFlowsheet flowsheet;
        public DynamicLayout container;
        public IReaction rx, rx0;

        private Label txtEquation;

        public ConversionReaction(IFlowsheet fs, IReaction reaction, DynamicLayout layout)
        {
            container = layout;
            flowsheet = fs;
            rx0 = reaction;
            rx = (IReaction)((ICloneable)reaction).Clone();
            rx.ID = reaction.ID;
            Initialize();
        }

        void Initialize()
        {

            container.CreateAndAddLabelRow("Reaction ID");

            container.CreateAndAddStringEditorRow2("Name", "", rx.Name, (sender, e) => { rx.Name = sender.Text; });

            container.CreateAndAddStringEditorRow2("Description", "", rx.Description, (sender, e) => { rx.Description = sender.Text; });

            container.CreateAndAddLabelRow("Compounds and Stoichiometry (Include / Name / Heat of Formation (kJ/kg) / Stoich. Coeff.)");

            var compcontainer = new DynamicLayout();

            List<string> toremove = new List<string>();
            foreach (var comp in rx.Components)
                if (!flowsheet.SelectedCompounds.ContainsKey(comp.Key)) toremove.Add(comp.Key);
            foreach (var comp in toremove)
                rx.Components.Remove(comp);

            Double val;

            foreach (ICompoundConstantProperties comp in flowsheet.SelectedCompounds.Values)
            {
                var chk = new CheckBox() { Text = comp.Name, Checked = (rx.Components.ContainsKey(comp.Name) ? true : false) };
                chk.CheckedChanged += (sender, e) =>
                {
                    if (!rx.Components.ContainsKey(comp.Name))
                    {
                        rx.Components.Add(comp.Name, new DWSIM.Thermodynamics.BaseClasses.ReactionStoichBase(comp.Name, 0, false, 0, 0));
                    }
                    else
                    {
                        rx.Components.Remove(comp.Name);
                    }
                    UpdateEquation();
                };

                var sc = new TextBox() { Width = (int)(sf*50), Text = (rx.Components.ContainsKey(comp.Name) ? rx.Components[comp.Name].StoichCoeff.ToString() : 0.0f.ToString()) };

                sc.TextChanged += (sender, e) =>
                {
                    if (Double.TryParse(sc.Text.ToString(), out val))
                    {
                        sc.TextColor = SystemColors.ControlText;
                        if (!rx.Components.ContainsKey(comp.Name))
                        {
                            rx.Components.Add(comp.Name, new DWSIM.Thermodynamics.BaseClasses.ReactionStoichBase(comp.Name, Double.Parse(sc.Text), false, 0, 0));
                        }
                        else
                        {
                            rx.Components[comp.Name].StoichCoeff = double.Parse(sc.Text);
                        }
                        UpdateEquation();
                    }
                    else
                    {
                        sc.TextColor = Colors.Red;
                    }
                };

                var hf = new TextBox() { Enabled = false, Width = (int)(sf * 100), Text = comp.IG_Enthalpy_of_Formation_25C.ToString("N2") };

                compcontainer.Add(new TableRow(chk, null, hf, sc));
            }

            container.CreateAndAddControlRow(compcontainer);
            container.CreateAndAddEmptySpace();

            var comps = flowsheet.SelectedCompounds.Values.Select((x) => x.Name).ToList();
            comps.Insert(0, "");

            container.CreateAndAddLabelRow("Base Compound");

            var basecompselector = container.CreateAndAddDropDownRow("Base Compound", comps, 0, null);

            var basecomp = rx.Components.Values.Where((x) => x.IsBaseReactant).FirstOrDefault();

            if (basecomp != null)
            {
                basecompselector.SelectedIndex = comps.IndexOf(basecomp.CompName);
            }
            else
            {
                basecompselector.SelectedIndex = 0;
            }

            basecompselector.SelectedIndexChanged += (sender, e) =>
            {
                if (rx.Components.ContainsKey(comps[basecompselector.SelectedIndex]))
                {
                    foreach (var rxc in rx.Components.Values)
                    {
                        rxc.IsBaseReactant = false;
                    }
                    rx.Components[comps[basecompselector.SelectedIndex]].IsBaseReactant = true;
                    rx.BaseReactant = comps[basecompselector.SelectedIndex];
                    UpdateEquation();
                }
            };

            container.CreateAndAddLabelRow("Reaction Balance");

            txtEquation = container.CreateAndAddLabelRow2("");

            container.CreateAndAddLabelRow("Temperature Limits");

            var nf = flowsheet.FlowsheetOptions.NumberFormat;
            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;

            container.CreateAndAddTextBoxRow(nf, "Minimum Temperature (" + su.temperature + ")", rx.Tmin.ConvertFromSI(su.temperature), (sender, e) => { if (sender.Text.IsValidDouble()) rx.Tmin = sender.Text.ToDoubleFromCurrent().ConvertToSI(su.temperature); });
            container.CreateAndAddTextBoxRow(nf, "Maximum Temperature (" + su.temperature + ")", rx.Tmax.ConvertFromSI(su.temperature), (sender, e) => { if (sender.Text.IsValidDouble()) rx.Tmax = sender.Text.ToDoubleFromCurrent().ConvertToSI(su.temperature); });

            container.CreateAndAddLabelRow("Reaction Phase");

            var rxphaseselector = container.CreateAndAddDropDownRow("Reaction Phase", Shared.StringArrays.reactionphase().ToList(), 0, null);

            rxphaseselector.SelectedIndex = (int)rx.ReactionPhase;

            rxphaseselector.SelectedIndexChanged += (sender, e) =>
            {
                rx.ReactionPhase = rxphaseselector.SelectedIndex.ToEnum<Interfaces.Enums.ReactionPhase>();
            };

            container.CreateAndAddLabelRow("Conversion Expression");

            container.CreateAndAddLabelRow2("Conversion Expression, % conv = f(T), T in K");

            container.CreateAndAddFullTextBoxRow(rx.Expression, (sender, e) => rx.Expression = sender.Text);

            UpdateEquation();

        }

        void UpdateEquation()
        {

            double bp = 0;
            double br = 0;
            double hp = 0;
            double hr = 0;

            txtEquation.Text = "Stoichiometry: ";

            string eq = "";
            //build reaction equation
            //scan for reactants
            foreach (var rxc in rx.Components.Values)
            {
                if (rxc.StoichCoeff < 0)
                {
                    if ((int)rxc.StoichCoeff == -1)
                    {
                        eq += flowsheet.SelectedCompounds[rxc.CompName].Formula + " + ";
                    }
                    else
                    {
                        eq += Math.Abs(rxc.StoichCoeff) + flowsheet.SelectedCompounds[rxc.CompName].Formula + " + ";
                    }
                    br += Math.Abs(rxc.StoichCoeff) * flowsheet.SelectedCompounds[rxc.CompName].Molar_Weight;
                    hr += Math.Abs(rxc.StoichCoeff) * flowsheet.SelectedCompounds[rxc.CompName].IG_Enthalpy_of_Formation_25C * flowsheet.SelectedCompounds[rxc.CompName].Molar_Weight;
                }
            }
            if (eq.Length >= 2)
                eq = eq.Remove(eq.Length - 2, 2);
            eq += "--> ";
            //scan for products
            foreach (var rxc in rx.Components.Values)
            {
                if (rxc.StoichCoeff > 0)
                {
                    if ((int)rxc.StoichCoeff == 1)
                    {
                        eq += flowsheet.SelectedCompounds[rxc.CompName].Formula + " + ";
                    }
                    else
                    {
                        eq += Math.Abs(rxc.StoichCoeff) + flowsheet.SelectedCompounds[rxc.CompName].Formula + " + ";
                    }
                    bp += Math.Abs(rxc.StoichCoeff) * flowsheet.SelectedCompounds[rxc.CompName].Molar_Weight;
                    hp += Math.Abs(rxc.StoichCoeff) * flowsheet.SelectedCompounds[rxc.CompName].IG_Enthalpy_of_Formation_25C * flowsheet.SelectedCompounds[rxc.CompName].Molar_Weight;
                }
            }
            eq = eq.Remove(eq.Length - 2, 2);
            if (rx.Components.ContainsKey(rx.BaseReactant)) { rx.ReactionHeat = (hp - hr) / Math.Abs(rx.Components[rx.BaseReactant].StoichCoeff); }

            txtEquation.Text += eq;

            if (Math.Abs(bp - br) < 0.01)
            {
                txtEquation.Text += " [OK, Heat of Reaction = " + rx.ReactionHeat.ToString("N2") + " kJ/kmol Base Compound]";
                txtEquation.TextColor = SystemColors.ControlText;
            }
            else
            {
                txtEquation.Text += " [NOT OK, check coefficients: " + (bp - br).ToString("N") + "]";
                txtEquation.TextColor = Colors.Red;
            }

        }

    }
}