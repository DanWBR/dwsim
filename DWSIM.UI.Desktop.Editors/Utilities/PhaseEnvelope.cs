using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using DWSIM.Thermodynamics.PropertyPackages;
using DWSIM.Thermodynamics.Streams;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.SharedClasses.SystemsOfUnits;

using System.Threading.Tasks;

using Eto.Forms;
using Eto.Drawing;

using DWSIM.ExtensionMethods;
using DWSIM.UI.Shared;
using s = DWSIM.UI.Shared.Common;

using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;
using DWSIM.UI.Desktop.Shared.Controls;

namespace DWSIM.UI.Desktop.Editors.Utilities
{
    public class PhaseEnvelopeView : TableLayout
    {

        public IFlowsheet flowsheet;

        public PhaseEnvelopeView(IFlowsheet fs)
            : base()
        {
            flowsheet = fs;
            Init();
        }

        void Init()
        {

            Spacing = new Size(5, 5);

            DynamicLayout p1;
            TableLayout p2;

            p1 = UI.Shared.Common.GetDefaultContainer();
            p2 = new TableLayout() { Spacing = new Size(5, 5), Padding = new Padding(15) };

            Rows.Add(new TableRow(p1, p2));

            p1.Width = 420;

            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;
            var nf = flowsheet.FlowsheetOptions.NumberFormat;

            var mslist = flowsheet.SimulationObjects.Values.Where((x) => x.GraphicObject.ObjectType == ObjectType.MaterialStream).Select((x2) => x2.GraphicObject.Tag).ToList();

            mslist.Insert(0, "");

            p1.CreateAndAddLabelRow("Setup");

            p1.CreateAndAddDescriptionRow("The Phase Envelope utility calculates various VLE envelopes for mixtures.");

            var spinner = p1.CreateAndAddDropDownRow("Material Stream", mslist, 0, (arg3, arg2) => { });

            var spinnerPE = p1.CreateAndAddDropDownRow("Envelope Type", Shared.StringArrays.envelopetype().ToList(), 0, null);

            var EnvelopeOptions = new PhaseEnvelopeOptions();

            var ft = new List<string>() { "PVF", "TVF" };

            var tabcontainer0 = new TabControl() { };

            var c1 = UI.Shared.Common.GetDefaultContainer();
            var c3 = UI.Shared.Common.GetDefaultContainer();
            var c2 = UI.Shared.Common.GetDefaultContainer();

            c1.CreateAndAddCheckBoxRow("Quality Line", EnvelopeOptions.QualityLine, (arg3, arg1) => { EnvelopeOptions.QualityLine = arg3.Checked.GetValueOrDefault(); });
            c1.CreateAndAddDescriptionRow("Includes a Quality Line in the chart (TP only).");
            c1.CreateAndAddTextBoxRow("G6", "Quality Value", EnvelopeOptions.QualityValue, (arg3, arg1) => { if (arg3.Text.IsValidDouble()) EnvelopeOptions.QualityValue = arg3.Text.ToDoubleFromCurrent(); });
            c1.CreateAndAddDescriptionRow("Vapor Phase Mole Fraction for Quality Line, between 0.0 and 1.0.");
            c1.CreateAndAddCheckBoxRow("Stability Curve", EnvelopeOptions.StabilityCurve, (arg3, arg1) => { EnvelopeOptions.StabilityCurve = arg3.Checked.GetValueOrDefault(); });
            c1.CreateAndAddDescriptionRow("Includes the Stability Curve in the chart (TP only). Works with PR and SRK Property Packages.");
            c1.CreateAndAddCheckBoxRow("Phase Identification Curve", EnvelopeOptions.PhaseIdentificationCurve, (arg3, arg1) => { EnvelopeOptions.PhaseIdentificationCurve = arg3.Checked.GetValueOrDefault(); });
            c1.CreateAndAddDescriptionRow("Calculates the PI curve (TP only), where the region above the curve is for a liquid-like phase and the region beyond the curve is for a vapor-like phase. Calculated using the PR EOS.");
            c1.CreateAndAddCheckBoxRow("Operation Point", EnvelopeOptions.OperatingPoint, (arg3, arg1) => { EnvelopeOptions.OperatingPoint = arg3.Checked.GetValueOrDefault(); });
            c1.CreateAndAddDescriptionRow("Includes the operating point in the chart.");
            c2.CreateAndAddCheckBoxRow("Custom Initialization", EnvelopeOptions.BubbleUseCustomParameters, (arg3, arg1) => { EnvelopeOptions.BubbleUseCustomParameters = arg3.Checked.GetValueOrDefault(); });
            c2.CreateAndAddDescriptionRow("Use the custom initialization options if the generated curve for bubble points has invalid points.");
            c2.CreateAndAddDropDownRow("Initial Flash", ft, 0, (arg3, arg1) => { EnvelopeOptions.BubbleCurveInitialFlash = ft[arg3.SelectedIndex]; });
            c2.CreateAndAddTextBoxRow("G6", "Initial Pressure (" + su.pressure + ")", EnvelopeOptions.BubbleCurveInitialPressure, (arg3, arg1) => { if (arg3.Text.IsValidDouble()) EnvelopeOptions.BubbleCurveInitialPressure = arg3.Text.ToDoubleFromCurrent().ConvertToSI(su.pressure); });
            c2.CreateAndAddTextBoxRow("G6", "Initial Temperature (" + su.temperature + ")", EnvelopeOptions.BubbleCurveInitialTemperature, (arg3, arg1) => { if (arg3.Text.IsValidDouble()) EnvelopeOptions.BubbleCurveInitialTemperature = arg3.Text.ToDoubleFromCurrent().ConvertToSI(su.pressure); });
            c2.CreateAndAddTextBoxRow("G6", "Maximum Temperature (" + su.temperature + ")", EnvelopeOptions.BubbleCurveMaximumTemperature, (arg3, arg1) => { if (arg3.Text.IsValidDouble()) EnvelopeOptions.BubbleCurveMaximumTemperature = arg3.Text.ToDoubleFromCurrent().ConvertToSI(su.pressure); });
            c2.CreateAndAddTextBoxRow("G6", "Pressure Step (" + su.deltaP + ")", EnvelopeOptions.BubbleCurveDeltaP, (arg3, arg1) => { if (arg3.Text.IsValidDouble()) EnvelopeOptions.BubbleCurveDeltaP = arg3.Text.ToDoubleFromCurrent().ConvertToSI(su.deltaP); });
            c2.CreateAndAddTextBoxRow("G6", "Temperature Step (" + su.deltaT + ")", EnvelopeOptions.BubbleCurveDeltaT, (arg3, arg1) => { if (arg3.Text.IsValidDouble()) EnvelopeOptions.BubbleCurveDeltaT = arg3.Text.ToDoubleFromCurrent().ConvertToSI(su.deltaT); });
            c2.CreateAndAddTextBoxRow("N0", "Maximum Points", EnvelopeOptions.BubbleCurveMaximumPoints, (arg3, arg1) => { if (arg3.Text.IsValidDouble()) EnvelopeOptions.BubbleCurveMaximumPoints = int.Parse(arg3.Text); });
            c3.CreateAndAddCheckBoxRow("Custom Initialization", EnvelopeOptions.DewUseCustomParameters, (arg3, arg1) => { EnvelopeOptions.DewUseCustomParameters = arg3.Checked.GetValueOrDefault(); });
            c3.CreateAndAddDescriptionRow("Use the custom initialization options if the generated curve for dew points has invalid points.");
            c3.CreateAndAddDropDownRow("Initial Flash", ft, 0, (arg3, arg1) => { EnvelopeOptions.DewCurveInitialFlash = ft[arg3.SelectedIndex]; });
            c3.CreateAndAddTextBoxRow("G6", "Initial Pressure (" + su.pressure + ")", EnvelopeOptions.DewCurveInitialPressure, (arg3, arg1) => { if (arg3.Text.IsValidDouble()) EnvelopeOptions.DewCurveInitialPressure = arg3.Text.ToDoubleFromCurrent().ConvertToSI(su.pressure); });
            c3.CreateAndAddTextBoxRow("G6", "Initial Temperature (" + su.temperature + ")", EnvelopeOptions.DewCurveInitialTemperature, (arg3, arg1) => { if (arg3.Text.IsValidDouble()) EnvelopeOptions.DewCurveInitialTemperature = arg3.Text.ToDoubleFromCurrent().ConvertToSI(su.pressure); });
            c3.CreateAndAddTextBoxRow("G6", "Maximum Temperature (" + su.temperature + ")", EnvelopeOptions.DewCurveMaximumTemperature, (arg3, arg1) => { if (arg3.Text.IsValidDouble()) EnvelopeOptions.DewCurveMaximumTemperature = arg3.Text.ToDoubleFromCurrent().ConvertToSI(su.pressure); });
            c3.CreateAndAddTextBoxRow("G6", "Pressure Step (" + su.deltaP + ")", EnvelopeOptions.DewCurveDeltaP, (arg3, arg1) => { if (arg3.Text.IsValidDouble()) EnvelopeOptions.DewCurveDeltaP = arg3.Text.ToDoubleFromCurrent().ConvertToSI(su.deltaP); });
            c3.CreateAndAddTextBoxRow("G6", "Temperature Step (" + su.deltaT + ")", EnvelopeOptions.DewCurveDeltaT, (arg3, arg1) => { if (arg3.Text.IsValidDouble()) EnvelopeOptions.DewCurveDeltaT = arg3.Text.ToDoubleFromCurrent().ConvertToSI(su.deltaT); });
            c3.CreateAndAddTextBoxRow("N0", "Maximum Points", EnvelopeOptions.DewCurveMaximumPoints, (arg3, arg1) => { if (arg3.Text.IsValidDouble()) EnvelopeOptions.DewCurveMaximumPoints = int.Parse(arg3.Text); });

            tabcontainer0.Pages.Add(new TabPage(new TableRow(c1)) { Text = "Envelope Options" });
            tabcontainer0.Pages.Add(new TabPage(new TableRow(c2)) { Text = "BP Initialization" });
            tabcontainer0.Pages.Add(new TabPage(new TableRow(c3)) { Text = "DP Initialization" });

            p1.CreateAndAddControlRow(tabcontainer0);

            var button = p1.CreateAndAddButtonRow("Build Envelope", null, null);

            p1.CreateAndAddEmptySpace();
            p1.CreateAndAddEmptySpace();

            var tabcontainer = new TabControl() { Height = 400 };

            var chart = new Eto.OxyPlot.Plot { BackgroundColor = Colors.White };

            var txtResults = new TextArea() { ReadOnly = true, Font = Fonts.Monospace(GlobalSettings.Settings.ResultsReportFontSize) };

            tabcontainer.Pages.Add(new TabPage(new TableRow(chart)) { Text = "Chart" });
            tabcontainer.Pages.Add(new TabPage(new TableRow(txtResults)) { Text = "Data" });

            p2.CreateAndAddLabelRow("Results");

            p2.CreateAndAddControlRow(tabcontainer);

            button.Click += (sender, e) =>
            {

                if (spinnerPE.SelectedIndex >= 0 && spinner.SelectedIndex > 0)
                {

                    var ms = (MaterialStream)flowsheet.GetFlowsheetSimulationObject(mslist[spinner.SelectedIndex]);
                    var calc = new DWSIM.Thermodynamics.ShortcutUtilities.Calculation(ms);

                    switch (spinnerPE.SelectedIndex + 1)
                    {
                        case 1:
                            calc.CalcType = Thermodynamics.ShortcutUtilities.CalculationType.PhaseEnvelopePT;
                            break;
                        case 2:
                            calc.CalcType = Thermodynamics.ShortcutUtilities.CalculationType.PhaseEnvelopePH;
                            break;
                        case 3:
                            calc.CalcType = Thermodynamics.ShortcutUtilities.CalculationType.PhaseEnvelopePS;
                            break;
                        case 4:
                            calc.CalcType = Thermodynamics.ShortcutUtilities.CalculationType.PhaseEnvelopeTH;
                            break;
                        case 5:
                            calc.CalcType = Thermodynamics.ShortcutUtilities.CalculationType.PhaseEnvelopeTS;
                            break;
                        case 6:
                            calc.CalcType = Thermodynamics.ShortcutUtilities.CalculationType.PhaseEnvelopeVP;
                            break;
                        case 7:
                            calc.CalcType = Thermodynamics.ShortcutUtilities.CalculationType.PhaseEnvelopeVT;
                            break;
                    }

                    calc.PhaseEnvelopeOptions = EnvelopeOptions;

                    DWSIM.Thermodynamics.ShortcutUtilities.CalculationResults results = null;

                    var token = new System.Threading.CancellationTokenSource();

                    var pg = ProgressDialog.ShowWithAbort(this, "Please Wait", "Calculating Envelope Lines...", false, "Abort/Cancel", (s, e1) => {

                        token.Cancel();

                    });

                    Task.Factory.StartNew(() =>
                    {
                        Application.Instance.Invoke(() => txtResults.Text = "Please wait...");
                        results = calc.Calculate();
                    }, token.Token).ContinueWith((t) =>
                    {
                        Application.Instance.Invoke(() =>
                        {
                            pg.Close();
                            pg = null;
                            if (results.ExceptionResult == null)
                            {

                                if (results.PlotModels.Count > 0)
                                {

                                    chart.Model = (OxyPlot.PlotModel)results.PlotModels[0];
                                    chart.Model.Padding = new OxyPlot.OxyThickness(5, 5, 20, 5);
                                    chart.Invalidate();

                                    txtResults.Text = results.TextOutput;

                                }
                                else
                                {

                                    chart.Model = null;
                                    txtResults.Text = "Invalid result";

                                }

                            }
                            else
                            {
                                txtResults.Text = results.ExceptionResult.Message;
                            }
                        });
                    });

                }

            };

        }

    }
}