using DWSIM.Interfaces;
using DWSIM.Thermodynamics.PropertyPackages;
using Eto.Drawing;
using Eto.Forms;
using System;
using System.Linq;
using DWSIM.UI.Shared;
using f = DWSIM.Interfaces.Enums.FlashSetting;
using DWSIM.ExtensionMethods;

namespace DWSIM.UI.Desktop.Editors
{
    public class FlashSettingsEditor : DynamicLayout
    {
        public IFlowsheet flowsheet;
        public PropertyPackage pp;

        public FlashSettingsEditor(IFlowsheet fs, PropertyPackage ppack) : base()
        {
            flowsheet = fs;
            pp = ppack;
            Init();
        }

        void Init()
        {

            Padding = new Padding(10);

            var comps = flowsheet.SelectedCompounds.Values.Select((x) => x.Name).ToList();
            var nf = flowsheet.FlowsheetOptions.NumberFormat;

            var s = pp.FlashSettings;

            var ci = System.Globalization.CultureInfo.InvariantCulture;

            var c = this;

            c.CreateAndAddLabelRow("General");

            var options = new[] { "Default", "VLE", "VLLE", "SVLE", "SVLLE", "No Flash" };

            var o1 = s[f.ForceEquilibriumCalculationType];

            c.CreateAndAddDropDownRow("Phase Equilibria Calculation Type", options.ToList(), o1,
                (dd, e) =>
                {
                    s[f.ForceEquilibriumCalculationType] = options[dd.SelectedIndex];
                });

            var fsoptions = new[] { "Rigorous VLE", "Ideal VLE", "No Flash", "Nothing (Throw Error)" };

            c.CreateAndAddDropDownRow("Fail-Safe Procedure", fsoptions.ToList(), int.Parse(s[f.FailSafeCalculationMode]),
                (dd, e) =>
                {
                    s[f.FailSafeCalculationMode] = dd.SelectedIndex.ToString();
                });

            var methods = new[] { "Nested Loops", "Inside-Out", "Gibbs Minimization" };

            var o2 = s[f.ForceEquilibriumCalculationType];

            c.CreateAndAddDropDownRow("Numerical Method", methods.ToList(), (int)pp.FlashCalculationApproach,
                (dd, e) =>
                {
                    pp.FlashCalculationApproach = dd.SelectedIndex.ToEnum<PropertyPackage.FlashCalculationApproachType>();
                });

            c.CreateAndAddCheckBoxRow("Identify Phases After Equilibrium Calculation", bool.Parse(s[f.UsePhaseIdentificationAlgorithm]),
                (cb, e) =>
                {
                    s[f.UsePhaseIdentificationAlgorithm] = cb.Checked.GetValueOrDefault().ToString();
                });

            c.CreateAndAddCheckBoxRow("Calculate Saturation Conditions (Bubble/Dew Points)", bool.Parse(s[f.CalculateBubbleAndDewPoints]),
                (cb, e) =>
                {
                    s[f.CalculateBubbleAndDewPoints] = cb.Checked.GetValueOrDefault().ToString();
                });

            c.CreateAndAddCheckBoxRow("Validate Equilibrium Calculation Results", bool.Parse(s[f.ValidateEquilibriumCalc]),
                (cb, e) =>
                {
                    s[f.ValidateEquilibriumCalc] = cb.Checked.GetValueOrDefault().ToString();
                });

            c.CreateAndAddCheckBoxRow("Display Warning for Missing Compound Parameters", pp.DisplayMissingCompoundPropertiesWarning,
                (cb, e) =>
                {
                    pp.DisplayMissingCompoundPropertiesWarning = cb.Checked.GetValueOrDefault();
                });

            c.CreateAndAddLabelRow("Nested Loops Options");

            c.CreateAndAddCheckBoxRow("Handle Solids", bool.Parse(s[f.HandleSolidsInDefaultEqCalcMode]),
                (cb, e) =>
                {
                    s[f.HandleSolidsInDefaultEqCalcMode] = cb.Checked.GetValueOrDefault().ToString();
                });

            c.CreateAndAddCheckBoxRow("Immiscible Water", bool.Parse(s[f.ImmiscibleWaterOption]),
                (cb, e) =>
                {
                    s[f.ImmiscibleWaterOption] = cb.Checked.GetValueOrDefault().ToString();
                });

            c.CreateAndAddCheckBoxRow("PH/PS Flash Fast Mode", bool.Parse(s[f.NL_FastMode]),
                (cb, e) =>
                {
                    s[f.NL_FastMode] = cb.Checked.GetValueOrDefault().ToString();
                });

            c.CreateAndAddCheckBoxRow("PH Flash - Interpolate Temperature on Oscillating Cases", bool.Parse(s[f.PHFlash_Use_Interpolated_Result_In_Oscillating_Temperature_Cases]),
                (cb, e) =>
                {
                    s[f.PHFlash_Use_Interpolated_Result_In_Oscillating_Temperature_Cases] = cb.Checked.GetValueOrDefault().ToString();
                });

            c.CreateAndAddCheckBoxRow("PV Flash - Try Ideal Calculation on Failure", bool.Parse(s[f.PVFlash_TryIdealCalcOnFailure]),
                (cb, e) =>
                {
                    s[f.PVFlash_TryIdealCalcOnFailure] = cb.Checked.GetValueOrDefault().ToString();
                });

            c.CreateAndAddLabelRow("Convergence Parameters");

            c.CreateAndAddTextBoxRow(nf, "PV Flash - Temperature Delta for K-value Numerical Derivative Calculation",
                double.Parse(s[f.PVFlash_TemperatureDerivativeEpsilon], ci),
                (tb, e) =>
                {
                    if (tb.Text.IsValidDouble())
                        s[f.PVFlash_TemperatureDerivativeEpsilon] = tb.Text.ToDoubleFromCurrent().ToString(ci);
                });

            c.CreateAndAddTextBoxRow(nf, "PV Flash - Maximum Temperature Update Delta (K) (1.0 - 50.0)",
                double.Parse(s[f.PVFlash_MaximumTemperatureChange], ci),
                (tb, e) =>
                {
                    if (tb.Text.IsValidDouble())
                        s[f.PVFlash_MaximumTemperatureChange] = tb.Text.ToDoubleFromCurrent().ToString(ci);
                });

            c.CreateAndAddTextBoxRow(nf, "PV Flash - Fixed Damping Factor Value (0.0 - 2.0)",
                double.Parse(s[f.PVFlash_FixedDampingFactor], ci),
                (tb, e) =>
                {
                    if (tb.Text.IsValidDouble())
                        s[f.PVFlash_FixedDampingFactor] = tb.Text.ToDoubleFromCurrent().ToString(ci);
                });

            c.CreateAndAddTextBoxRow(nf, "PH/PS Flash - Maximum Temperature Update Delta (K) (1.0 - 50.0)",
                double.Parse(s[f.PHFlash_MaximumTemperatureChange], ci),
                (tb, e) =>
                {
                    if (tb.Text.IsValidDouble())
                        s[f.PHFlash_MaximumTemperatureChange] = tb.Text.ToDoubleFromCurrent().ToString(ci);
                });

            c.CreateAndAddTextBoxRow(nf, "PT Flash - Fixed Damping Factor Value (0.0 - 2.0)",
                double.Parse(s[f.PTFlash_DampingFactor], ci),
                (tb, e) =>
                {
                    if (tb.Text.IsValidDouble())
                        s[f.PTFlash_DampingFactor] = tb.Text.ToDoubleFromCurrent().ToString(ci);
                });

            c.CreateAndAddLabelRow("Convergence Error Tolerances");

            c.CreateAndAddTextBoxRow(nf, "Flash PT/PV - Internal loop convergence tolerance (absolute)",
                double.Parse(s[f.PTFlash_Internal_Loop_Tolerance], ci),
                (tb, e) =>
                {
                    if (tb.Text.IsValidDouble())
                        s[f.PTFlash_Internal_Loop_Tolerance] = tb.Text.ToDoubleFromCurrent().ToString(ci);
                });

            c.CreateAndAddTextBoxRow(nf, "Flash PT/PV - External loop convergence tolerance (absolute)",
                double.Parse(s[f.PTFlash_External_Loop_Tolerance], ci),
                (tb, e) =>
                {
                    if (tb.Text.IsValidDouble())
                        s[f.PTFlash_External_Loop_Tolerance] = tb.Text.ToDoubleFromCurrent().ToString(ci);
                });

            c.CreateAndAddTextBoxRow(nf, "Flash PH/PS - Internal loop convergence tolerance (absolute)",
                double.Parse(s[f.PHFlash_Internal_Loop_Tolerance], ci),
                (tb, e) =>
                {
                    if (tb.Text.IsValidDouble())
                        s[f.PHFlash_Internal_Loop_Tolerance] = tb.Text.ToDoubleFromCurrent().ToString(ci);
                });

            c.CreateAndAddTextBoxRow(nf, "Flash PH/PS - External loop convergence tolerance (absolute)",
                double.Parse(s[f.PHFlash_External_Loop_Tolerance], ci),
                (tb, e) =>
                {
                    if (tb.Text.IsValidDouble())
                        s[f.PHFlash_External_Loop_Tolerance] = tb.Text.ToDoubleFromCurrent().ToString(ci);
                });

            c.CreateAndAddTextBoxRow("N0", "Flash PT/PV - maximum number of iterations in the internal loop",
                int.Parse(s[f.PTFlash_Maximum_Number_Of_Internal_Iterations], ci),
                (tb, e) =>
                {
                    if (tb.Text.IsValidDouble())
                        s[f.PTFlash_Maximum_Number_Of_Internal_Iterations] = ((int)tb.Text.ToDoubleFromCurrent()).ToString(ci);
                });

            c.CreateAndAddTextBoxRow("N0", "Flash PT/PV - maximum number of iterations in the external loop",
                int.Parse(s[f.PTFlash_Maximum_Number_Of_External_Iterations], ci),
                (tb, e) =>
                {
                    if (tb.Text.IsValidDouble())
                        s[f.PTFlash_Maximum_Number_Of_External_Iterations] = ((int)tb.Text.ToDoubleFromCurrent()).ToString(ci);
                });

            c.CreateAndAddTextBoxRow("N0", "Flash PH/PS - maximum number of iterations in the internal loop",
                int.Parse(s[f.PHFlash_Maximum_Number_Of_Internal_Iterations], ci),
                (tb, e) =>
                {
                    if (tb.Text.IsValidDouble())
                        s[f.PTFlash_Maximum_Number_Of_Internal_Iterations] = ((int)tb.Text.ToDoubleFromCurrent()).ToString(ci);
                });

            c.CreateAndAddTextBoxRow("N0", "Flash PH/PS - maximum number of iterations in the external loop",
                int.Parse(s[f.PHFlash_Maximum_Number_Of_External_Iterations], ci),
                (tb, e) =>
                {
                    if (tb.Text.IsValidDouble())
                        s[f.PTFlash_Maximum_Number_Of_External_Iterations] = ((int)tb.Text.ToDoubleFromCurrent()).ToString(ci);
                });

        }

    }
}
