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

namespace DWSIM.UI.Desktop.Editors
{
    public class KineticReaction
    {

        private static double sf = GlobalSettings.Settings.UIScalingFactor;

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

            container.CreateAndAddStringEditorRow2("Description", "", rx.Description, (sender, e) => { rx.Description = sender.Text; });

            DynamicLayout p1, p2;

            TableLayout t1;

            p1 = UI.Shared.Common.GetDefaultContainer();
            p2 = UI.Shared.Common.GetDefaultContainer();

            //p1.Width = 420;

            t1 = new TableLayout();
            t1.Rows.Add(new TableRow(p1, p2));

            t1.Rows[0].Cells[0].ScaleWidth = true;
            t1.Rows[0].Cells[1].ScaleWidth = true;

            p1.CreateAndAddLabelRow("Compounds and Stoichiometry (Include / Name / Heat of Formation (kJ/kg) / Stoich. Coeff. / Direct Order Exponent / Reverse Order Exponent)");

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

                var sc = new TextBox() { Width = (int)(sf * 45), Text = (rx.Components.ContainsKey(comp.Name) ? rx.Components[comp.Name].StoichCoeff.ToString() : 0.0f.ToString()) };

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

                var txtdo = new TextBox() { Width = (int)(sf * 45), Text = (rx.Components.ContainsKey(comp.Name) ? rx.Components[comp.Name].DirectOrder.ToString() : 0.0f.ToString()) };

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

                var txtro = new TextBox() { Width = (int)(sf * 45), Text = (rx.Components.ContainsKey(comp.Name) ? rx.Components[comp.Name].ReverseOrder.ToString() : 0.0f.ToString()) };

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

                var hf = new TextBox() { Enabled = false, Width = (int)(sf * 100), Text = comp.IG_Enthalpy_of_Formation_25C.ToString("N2") };

                compcontainer.Add(new TableRow(chk, null, hf, sc, txtdo, txtro));
            }

            p1.CreateAndAddControlRow(compcontainer);
            p1.CreateAndAddEmptySpace();

            var comps = flowsheet.SelectedCompounds.Values.Select((x) => x.Name).ToList();
            comps.Insert(0, "");

            p1.CreateAndAddLabelRow("Base Compound");

            var basecompselector = p1.CreateAndAddDropDownRow("Base Compound", comps, 0, null);

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

            p1.CreateAndAddLabelRow("Reaction Balance");

            txtEquation = p1.CreateAndAddLabelRow2("");

            p1.CreateAndAddLabelRow("Reaction Phase");

            var rxphaseselector = p1.CreateAndAddDropDownRow("Reaction Phase", Shared.StringArrays.reactionphase().ToList(), 0, null);

            rxphaseselector.SelectedIndex = (int)rx.ReactionPhase;

            rxphaseselector.SelectedIndexChanged += (sender, e) =>
            {
                rx.ReactionPhase = rxphaseselector.SelectedIndex.ToEnum<Interfaces.Enums.ReactionPhase>();
            };

            p1.CreateAndAddLabelRow("Reaction Basis");

            var rxbasisselector = p1.CreateAndAddDropDownRow("Reaction Basis", Shared.StringArrays.reactionbasis().ToList(), 0, null);

            switch (rx.ReactionBasis)
            {
                case Interfaces.Enums.ReactionBasis.Activity:
                    rxbasisselector.SelectedIndex = (0);
                    break;
                case Interfaces.Enums.ReactionBasis.Fugacity:
                    rxbasisselector.SelectedIndex = (1);
                    break;
                case Interfaces.Enums.ReactionBasis.MassConc:
                    rxbasisselector.SelectedIndex = (2);
                    break;
                case Interfaces.Enums.ReactionBasis.MassFrac:
                    rxbasisselector.SelectedIndex = (3);
                    break;
                case Interfaces.Enums.ReactionBasis.MolarConc:
                    rxbasisselector.SelectedIndex = (4);
                    break;
                case Interfaces.Enums.ReactionBasis.MolarFrac:
                    rxbasisselector.SelectedIndex = (5);
                    break;
                case Interfaces.Enums.ReactionBasis.PartialPress:
                    rxbasisselector.SelectedIndex = (6);
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

            p1.CreateAndAddLabelRow("Temperature Limits");

            var nf = flowsheet.FlowsheetOptions.NumberFormat;
            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;

            p1.CreateAndAddTextBoxRow(nf, "Minimum Temperature (" + su.temperature + ")", rx.Tmin.ConvertFromSI(su.temperature), (sender, e) => { if (sender.Text.IsValidDouble()) rx.Tmin = sender.Text.ToDoubleFromCurrent().ConvertToSI(su.temperature); });
            p1.CreateAndAddTextBoxRow(nf, "Maximum Temperature (" + su.temperature + ")", rx.Tmax.ConvertFromSI(su.temperature), (sender, e) => { if (sender.Text.IsValidDouble()) rx.Tmax = sender.Text.ToDoubleFromCurrent().ConvertToSI(su.temperature); });

            p2.CreateAndAddLabelRow("Reaction Kinetics");

            p2.CreateAndAddCheckBoxRow("Advanced Kinetics", rx.ReactionKinetics == Interfaces.Enums.ReactionKinetics.PythonScript,
                (chk, e) => { if (chk.Checked.GetValueOrDefault()) rx.ReactionKinetics = Interfaces.Enums.ReactionKinetics.PythonScript; else rx.ReactionKinetics = Interfaces.Enums.ReactionKinetics.Expression; });

            var scripts = new List<string>();
            scripts.Add("");
            scripts.AddRange(flowsheet.Scripts.Values.Select(x => x.Title).ToArray());

            p2.CreateAndAddDropDownRow("Python Script (for Advanced Kinetics)", scripts, scripts.Contains(rx.ScriptTitle) ? scripts.IndexOf(rx.ScriptTitle) : 0,
                (cb, e) => rx.ScriptTitle = cb.SelectedValue.ToString());

            p2.CreateAndAddDescriptionRow("Create a Python Script using the Script Manager which returns the reaction rate 'r' in the currently selected units. " +
                                            "You can get the compound amounts through the R1, R2, ..., Rn, P1, P2, ..., Pn and N1, N2, ... Nn variables or use the 'Amounts' dictionary to get the amount by the compound's name. " +
                                            "You can also use the current temperature 'T' in Kelvin and pressure 'P' in Pa.");

            p2.CreateAndAddLabelRow("Velocity Constant for Forward Reactions");

            p2.CreateAndAddDropDownRow("Equation Type for Forward Reactions", new List<string>() { "Arrhenius (k = A*exp(-E/RT))", "User-Defined Expression" }, (int)rx.ReactionKinFwdType, (sender, e) =>
            {
                switch (sender.SelectedIndex)
                {
                    case 0:
                        rx.ReactionKinFwdType = Interfaces.Enums.ReactionKineticType.Arrhenius;
                        break;
                    case 1:
                        rx.ReactionKinFwdType = Interfaces.Enums.ReactionKineticType.UserDefined;
                        break;
                }
            });

            p2.CreateAndAddStringEditorRow2("A", "", rx.A_Forward.ToString(), (sender, e) =>
            {
                if (Double.TryParse(sender.Text.ToString(), out val))
                {
                    sender.TextColor = SystemColors.ControlText;
                    rx.A_Forward = double.Parse(sender.Text);
                }
                else sender.TextColor = Colors.Red;
            });

            p2.CreateAndAddStringEditorRow2("E", "", rx.E_Forward.ToString(), (sender, e) =>
            {
                if (Double.TryParse(sender.Text.ToString(), out val))
                {
                    sender.TextColor = SystemColors.ControlText;
                    rx.E_Forward = double.Parse(sender.Text);
                }
                else sender.TextColor = Colors.Red;
            });

            p2.CreateAndAddStringEditorRow2("User Expression - f(T), T in K", "", rx.ReactionKinFwdExpression, (sender, e) =>
            {
                rx.ReactionKinFwdExpression = sender.Text;
            });

            p2.CreateAndAddLabelRow("Velocity Constant for Reverse Reactions");

            p2.CreateAndAddDropDownRow("Equation Type for Reverse Reactions", new List<string>() { "Arrhenius (k = A*exp(-E/RT))", "User-Defined Expression" }, (int)rx.ReactionKinRevType, (sender, e) =>
            {
                switch (sender.SelectedIndex)
                {
                    case 0:
                        rx.ReactionKinRevType = Interfaces.Enums.ReactionKineticType.Arrhenius;
                        break;
                    case 1:
                        rx.ReactionKinRevType = Interfaces.Enums.ReactionKineticType.UserDefined;
                        break;
                }
            });

            p2.CreateAndAddStringEditorRow2("A", "", rx.A_Reverse.ToString(), (sender, e) =>
            {
                if (Double.TryParse(sender.Text.ToString(), out val))
                {
                    sender.TextColor = SystemColors.ControlText;
                    rx.A_Reverse = double.Parse(sender.Text);
                }
                else sender.TextColor = Colors.Red;
            });

            p2.CreateAndAddStringEditorRow2("E (J/mol)", "", rx.E_Reverse.ToString(), (sender, e) =>
            {
                if (Double.TryParse(sender.Text.ToString(), out val))
                {
                    sender.TextColor = SystemColors.ControlText;
                    rx.E_Reverse = double.Parse(sender.Text);
                }
                else sender.TextColor = Colors.Red;
            });

            p2.CreateAndAddStringEditorRow2("User Expression - f(T), T in K", "", rx.ReactionKinRevExpression, (sender, e) =>
            {
                rx.ReactionKinRevExpression = sender.Text;
            });

            p2.CreateAndAddLabelRow("Units");

            var us = new DWSIM.SharedClasses.SystemsOfUnits.Units();
            var units = us.GetUnitSet(Interfaces.Enums.UnitOfMeasure.molar_conc);
            units.AddRange(us.GetUnitSet(Interfaces.Enums.UnitOfMeasure.mass_conc));
            units.AddRange(us.GetUnitSet(Interfaces.Enums.UnitOfMeasure.pressure));
            units.Insert(0, "");

            p2.CreateAndAddDropDownRow("Basis Units (Base Compound)", units, units.IndexOf(rx.ConcUnit), (sender, e) => rx.ConcUnit = sender.SelectedValue.ToString());

            var units2 = us.GetUnitSet(Interfaces.Enums.UnitOfMeasure.reac_rate);
            units2.Insert(0, "");

            p2.CreateAndAddDropDownRow("Velocity Units", units2, units2.IndexOf(rx.VelUnit), (sender, e) => rx.VelUnit = sender.SelectedValue.ToString());

            var units3 = us.GetUnitSet(Interfaces.Enums.UnitOfMeasure.molar_enthalpy);
            units3.Insert(0, "");

            p2.CreateAndAddDropDownRow("E (Forward)", units3, units3.IndexOf(rx.E_Forward_Unit), (sender, e) => rx.E_Forward_Unit = sender.SelectedValue.ToString());

            p2.CreateAndAddDropDownRow("E (Reverse)", units3, units3.IndexOf(rx.E_Reverse_Unit), (sender, e) => rx.E_Reverse_Unit = sender.SelectedValue.ToString());

            UpdateEquation();

            container.Add(t1);

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