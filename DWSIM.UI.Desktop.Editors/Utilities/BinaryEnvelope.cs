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

using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;

namespace DWSIM.UI.Desktop.Editors.Utilities
{
    public class BinaryEnvelopeView : DynamicLayout
    {

        public IFlowsheet flowsheet;

        public BinaryEnvelopeView(IFlowsheet fs)
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

            var complist = flowsheet.SelectedCompounds.Values.Select((x2) => x2.Name).ToList();

            complist.Insert(0, "");

            this.CreateAndAddLabelRow("Setup");

            this.CreateAndAddDescriptionRow("The Binary Envelope utility calculates Temperature and Pressure VLE/VLLE envelopes for binary mixtures.");

            var spinnerComp1 = this.CreateAndAddDropDownRow("Compound 1", complist, 0, null);

            var spinnerComp2 = this.CreateAndAddDropDownRow("Compound 2", complist, 0, null);

            var spinnerPE = this.CreateAndAddDropDownRow("Envelope Type", Shared.StringArrays.binaryenvelopetype().ToList(), 0, null);

            var tval = this.CreateAndAddTextBoxRow(nf, "Temperature (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, 298.15f), null);
            var pval = this.CreateAndAddTextBoxRow(nf, "Pressure (" + su.pressure + ")", cv.ConvertFromSI(su.pressure, 101325.0f), null);

            var pplist = flowsheet.PropertyPackages.Values.Select((x2) => x2.Tag).ToList();

            pplist.Insert(0, "");

            var spinnerpp = this.CreateAndAddDropDownRow("Property Package", pplist, 0, null);

            var button = this.CreateAndAddButtonRow("Build Envelope", null, null);

            var tabcontainer = new TabControl() { Height = 400 };

            var chart = new Eto.OxyPlot.Plot { BackgroundColor = Colors.White };

            var txtResults = new TextArea() { ReadOnly = true, Font = Fonts.Monospace(GlobalSettings.Settings.ResultsReportFontSize) };

            tabcontainer.Pages.Add(new TabPage(new TableRow(txtResults)) { Text = "Data" });
            tabcontainer.Pages.Add(new TabPage(new TableRow(chart)) { Text = "Chart" });

            this.CreateAndAddLabelRow("Results");

            this.CreateAndAddControlRow(tabcontainer);

            button.Click += (sender, e) =>
            {

                if (spinnerPE.SelectedIndex >= 0 && spinnerComp1.SelectedIndex > 0 && spinnerComp2.SelectedIndex > 0 && spinnerpp.SelectedIndex > 0)
                {

                    var comp1 = flowsheet.SelectedCompounds[complist[spinnerComp1.SelectedIndex]];
                    var comp2 = flowsheet.SelectedCompounds[complist[spinnerComp2.SelectedIndex]];

                    var ms = new MaterialStream("", "");
                    ms.SetFlowsheet(flowsheet);
                    ms.PropertyPackage = (PropertyPackage)flowsheet.PropertyPackages.Values.Where(x => x.Tag == spinnerpp.SelectedValue.ToString()).First();
                    ms.SetFlowsheet(flowsheet);

                    foreach (var phase in ms.Phases.Values)
                    {
                        phase.Compounds.Add(comp1.Name, new DWSIM.Thermodynamics.BaseClasses.Compound(comp1.Name, ""));
                        phase.Compounds[comp1.Name].ConstantProperties = comp1;
                        phase.Compounds.Add(comp2.Name, new DWSIM.Thermodynamics.BaseClasses.Compound(comp2.Name, ""));
                        phase.Compounds[comp2.Name].ConstantProperties = comp2;
                    }
                    double val;
                    if (Double.TryParse(tval.Text, out val))
                    {
                        ms.Phases[0].Properties.temperature = cv.ConvertToSI(su.temperature, Double.Parse(tval.Text));
                    }
                    if (Double.TryParse(pval.Text, out val))
                    {
                        ms.Phases[0].Properties.pressure = cv.ConvertToSI(su.pressure, Double.Parse(pval.Text));
                    }

                    var calc = new DWSIM.Thermodynamics.ShortcutUtilities.Calculation(ms);

                    switch (spinnerPE.SelectedIndex)
                    {
                        case 0:
                            calc.CalcType = Thermodynamics.ShortcutUtilities.CalculationType.BinaryEnvelopePxy;
                            break;
                        case 1:
                            calc.CalcType = Thermodynamics.ShortcutUtilities.CalculationType.BinaryEnvelopeTxy;
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