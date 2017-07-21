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

namespace DWSIM.UI.Desktop.Editors
{
    public class KineticReaction
    {

        public IFlowsheet flowsheet;
        public DynamicLayout container;
        public IReaction rx, rx0;

        private Label txtEquation;

        public KineticReaction(IFlowsheet fs, IReaction reaction, DynamicLayout layout)
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

            container.CreateAndAddLabelRow("Compounds and Stoichiometry (Include / Name / Stoich. Coeff. / Direct Order Exponent / Reverse Order Exponent)");

            var compcontainer = new DynamicLayout();
            compcontainer.BackgroundColor = new Eto.Drawing.Color(compcontainer.BackgroundColor, 0.7f);

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

                var sc = new TextBox() { Width = 30, Text = (rx.Components.ContainsKey(comp.Name) ? rx.Components[comp.Name].StoichCoeff.ToString() : 0.0f.ToString()) };

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

                var txtdo = new TextBox() { Width = 30, Text = (rx.Components.ContainsKey(comp.Name) ? rx.Components[comp.Name].DirectOrder.ToString() : 0.0f.ToString()) };

                txtdo.TextChanged += (sender, e) =>
                {
                    if (Double.TryParse(txtdo.Text.ToString(), out val))
                    {
                        txtdo.TextColor = SystemColors.ControlText;
                        if (!rx.Components.ContainsKey(comp.Name))
                        {
                            rx.Components.Add(comp.Name, new DWSIM.Thermodynamics.BaseClasses.ReactionStoichBase(comp.Name, 0, false, Double.Parse(txtdo.Text), 0));
                        }
                        else
                        {
                            rx.Components[comp.Name].DirectOrder = double.Parse(txtdo.Text);
                        }
                        UpdateEquation();
                    }
                    else
                    {
                        txtdo.TextColor = Colors.Red;
                    }
                };

                var txtro = new TextBox() { Width = 30, Text = (rx.Components.ContainsKey(comp.Name) ? rx.Components[comp.Name].ReverseOrder.ToString() : 0.0f.ToString()) };

                txtro.TextChanged += (sender, e) =>
                {
                    if (Double.TryParse(txtro.Text.ToString(), out val))
                    {
                        txtro.TextColor = SystemColors.ControlText;
                        if (!rx.Components.ContainsKey(comp.Name))
                        {
                            rx.Components.Add(comp.Name, new DWSIM.Thermodynamics.BaseClasses.ReactionStoichBase(comp.Name, 0, false, 0, Double.Parse(txtro.Text)));
                        }
                        else
                        {
                            rx.Components[comp.Name].ReverseOrder = double.Parse(txtro.Text);
                        }
                        UpdateEquation();
                    }
                    else
                    {
                        txtro.TextColor = Colors.Red;
                    }
                };

                compcontainer.Add(new TableRow(chk, null, sc, txtdo, txtro));
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
                }
            };

            container.CreateAndAddLabelRow("Reaction Balance");

            txtEquation = container.CreateAndAddLabelRow2("");

            container.CreateAndAddLabelRow("Reaction Phase");

            var rxphaseselector = container.CreateAndAddDropDownRow("Reaction Phase", Shared.StringArrays.reactionphase().ToList(), 0, null);

            switch (rx.ReactionPhase)
            {
                case Interfaces.Enums.PhaseName.Vapor:
                    rxphaseselector.SelectedIndex = (0);
                    break;
                case Interfaces.Enums.PhaseName.Liquid:
                    rxphaseselector.SelectedIndex = (1);
                    break;
                case Interfaces.Enums.PhaseName.Mixture:
                    rxphaseselector.SelectedIndex = (2);
                    break;
            }

            rxphaseselector.SelectedIndexChanged += (sender, e) =>
            {
                switch (rxphaseselector.SelectedIndex)
                {
                    case 0:
                        rx.ReactionPhase = Interfaces.Enums.PhaseName.Vapor;
                        break;
                    case 1:
                        rx.ReactionPhase = Interfaces.Enums.PhaseName.Liquid;
                        break;
                    case 2:
                        rx.ReactionPhase = Interfaces.Enums.PhaseName.Mixture;
                        break;
                }
            };

            container.CreateAndAddLabelRow("Reaction Basis");

            var rxbasisselector = container.CreateAndAddDropDownRow("Reaction Basis", Shared.StringArrays.reactionbasis().ToList(), 0, null);

            switch (rx.ReactionBasis)
            {
                case Interfaces.Enums.ReactionBasis.Activity:
                    rxphaseselector.SelectedIndex = (0);
                    break;
                case Interfaces.Enums.ReactionBasis.Fugacity:
                    rxphaseselector.SelectedIndex = (1);
                    break;
                case Interfaces.Enums.ReactionBasis.MassConc:
                    rxphaseselector.SelectedIndex = (2);
                    break;
                case Interfaces.Enums.ReactionBasis.MassFrac:
                    rxphaseselector.SelectedIndex = (3);
                    break;
                case Interfaces.Enums.ReactionBasis.MolarConc:
                    rxphaseselector.SelectedIndex = (4);
                    break;
                case Interfaces.Enums.ReactionBasis.MolarFrac:
                    rxphaseselector.SelectedIndex = (5);
                    break;
                case Interfaces.Enums.ReactionBasis.PartialPress:
                    rxphaseselector.SelectedIndex = (6);
                    break;
            }

            rxbasisselector.SelectedIndexChanged += (sender, e) =>
            {
                switch (rxbasisselector.SelectedIndex)
                {
                    case 0:
                        rx.ReactionBasis = Interfaces.Enums.ReactionBasis.Activity;
                        break;
                    case 1:
                        rx.ReactionBasis = Interfaces.Enums.ReactionBasis.Fugacity;
                        break;
                    case 2:
                        rx.ReactionBasis = Interfaces.Enums.ReactionBasis.MassConc;
                        break;
                    case 3:
                        rx.ReactionBasis = Interfaces.Enums.ReactionBasis.MassFrac;
                        break;
                    case 4:
                        rx.ReactionBasis = Interfaces.Enums.ReactionBasis.MolarConc;
                        break;
                    case 5:
                        rx.ReactionBasis = Interfaces.Enums.ReactionBasis.MolarFrac;
                        break;
                    case 6:
                        rx.ReactionBasis = Interfaces.Enums.ReactionBasis.PartialPress;
                        break;
                }
            };

            container.CreateAndAddLabelRow("Kinetic Parameters");

            container.CreateAndAddLabelRow2("Direct and Reverse Reactions Velocity Constants (k = A*exp(-E/RT), E in J/mol and T in K)");

            container.CreateAndAddStringEditorRow2("Direct Reaction A", "", rx.A_Forward.ToString(), (sender, e) => {
                if (Double.TryParse(sender.Text.ToString(), out val))
                {
                    sender.TextColor = SystemColors.ControlText;
                    rx.A_Forward = double.Parse(sender.Text);
                }
                else sender.TextColor = Colors.Red;
            });

            container.CreateAndAddStringEditorRow2("Direct Reaction E", "", rx.E_Forward.ToString(), (sender, e) =>
            {
                if (Double.TryParse(sender.Text.ToString(), out val))
                {
                    sender.TextColor = SystemColors.ControlText;
                    rx.E_Forward = double.Parse(sender.Text);
                }
                else sender.TextColor = Colors.Red;
            });

            container.CreateAndAddStringEditorRow2("Reverse Reaction A", "", rx.A_Reverse.ToString(), (sender, e) =>
            {
                if (Double.TryParse(sender.Text.ToString(), out val))
                {
                    sender.TextColor = SystemColors.ControlText;
                    rx.A_Reverse = double.Parse(sender.Text);
                }
                else sender.TextColor = Colors.Red;
            });

            container.CreateAndAddStringEditorRow2("Reverse Reaction E", "", rx.E_Reverse.ToString(), (sender, e) =>
            {
                if (Double.TryParse(sender.Text.ToString(), out val))
                {
                    sender.TextColor = SystemColors.ControlText;
                    rx.E_Reverse = double.Parse(sender.Text);
                }
                else sender.TextColor = Colors.Red;
            });

            container.CreateAndAddLabelRow("Units");

            var us = new DWSIM.SharedClasses.SystemsOfUnits.Units();
			var units = us.GetUnitSet(Interfaces.Enums.UnitOfMeasure.molar_conc);
			units.AddRange(us.GetUnitSet(Interfaces.Enums.UnitOfMeasure.mass_conc));
			units.AddRange(us.GetUnitSet(Interfaces.Enums.UnitOfMeasure.pressure));
			units.Insert(0, "");

            container.CreateAndAddDropDownRow("Basis Units (Base Compound)", units, units.IndexOf(rx.ConcUnit), (sender, e) => rx.ConcUnit = sender.SelectedValue.ToString());

            container.CreateAndAddDropDownRow("Velocity Units", units, units.IndexOf(rx.VelUnit), (sender, e) => rx.VelUnit = sender.SelectedValue.ToString());

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