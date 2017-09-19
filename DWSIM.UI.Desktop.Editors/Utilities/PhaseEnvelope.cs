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

namespace DWSIM.UI.Desktop.Editors.Utilities
{
    public class PhaseEnvelopeView : DynamicLayout
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

            Padding = new Padding(10);

            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;
            var nf = flowsheet.FlowsheetOptions.NumberFormat;

            var mslist = flowsheet.SimulationObjects.Values.Where((x) => x.GraphicObject.ObjectType == ObjectType.MaterialStream).Select((x2) => x2.GraphicObject.Tag).ToList();

            mslist.Insert(0, "");

            this.CreateAndAddLabelRow("Setup");

            this.CreateAndAddDescriptionRow("The Phase Envelope utility calculates various VLE envelopes for mixtures.");

            var spinner = this.CreateAndAddDropDownRow("Material Stream", mslist, 0, (arg3, arg2) => { });

            var spinnerPE = this.CreateAndAddDropDownRow("Envelope Type", Shared.StringArrays.envelopetype().ToList(), 0, null);

            var button = this.CreateAndAddButtonRow("Build Envelope", null, null);

            var tabcontainer = new TabControl() { Height = 400 };

            var chart = new Eto.OxyPlot.Plot { BackgroundColor = Colors.White };

            var txtResults = new TextArea() {ReadOnly = true, Font = Fonts.Monospace(GlobalSettings.Settings.ResultsReportFontSize)};

            tabcontainer.Pages.Add(new TabPage(new TableRow(txtResults)) { Text = "Data" });
            tabcontainer.Pages.Add(new TabPage(new TableRow(chart)) { Text = "Chart" });

            this.CreateAndAddLabelRow("Results");

            this.CreateAndAddControlRow(tabcontainer);

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

                    DWSIM.Thermodynamics.ShortcutUtilities.CalculationResults results = null;

                    Task.Factory.StartNew(() =>
                    {
                        Application.Instance.Invoke(() => txtResults.Text = "Please wait...");
                        results = calc.Calculate();
                    }).ContinueWith((t) =>
                    {
                        Application.Instance.Invoke(() =>
                        {
                            if (results.ExceptionResult == null)
                            {

                                if (results.PlotModels.Count > 0)
                                {

                                    chart.Model = (OxyPlot.PlotModel)results.PlotModels[0];
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