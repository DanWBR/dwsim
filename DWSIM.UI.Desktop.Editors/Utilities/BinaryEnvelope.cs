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
using DWSIM.UI.Desktop.Shared.Controls;

namespace DWSIM.UI.Desktop.Editors.Utilities
{
    public class BinaryEnvelopeView : TableLayout
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

            Spacing = new Size(5, 5);

            DynamicLayout p1;
            TableLayout p2;
            
            p1 = UI.Shared.Common.GetDefaultContainer();
            p2 = new TableLayout() { Spacing = new Size(5, 5), Padding = new Padding(15) };

            Rows.Add(new TableRow (p1, p2));

            p1.Width = 420;
            
            var l1 = p1.CreateAndAddLabelRow("Envelope Setup");

            var tabcontainer = new TabControl();

            Eto.OxyPlot.Plot chart = null;
            
            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;
            var nf = flowsheet.FlowsheetOptions.NumberFormat;

            var complist = flowsheet.SelectedCompounds.Values.Select((x2) => x2.Name).ToList();

            complist.Insert(0, "");

            p1.CreateAndAddDescriptionRow("The Binary Envelope utility calculates Temperature and Pressure VLE/VLLE envelopes for binary mixtures.");

            var spinnerComp1 = p1.CreateAndAddDropDownRow("Compound 1", complist, 0, null);

            var spinnerComp2 = p1.CreateAndAddDropDownRow("Compound 2", complist, 0, null);

            var spinnerPE = p1.CreateAndAddDropDownRow("Envelope Type", Shared.StringArrays.binaryenvelopetype().ToList(), 1, null);

            var tval = p1.CreateAndAddTextBoxRow(nf, "Temperature (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, 298.15f), null);
            var pval = p1.CreateAndAddTextBoxRow(nf, "Pressure (" + su.pressure + ")", cv.ConvertFromSI(su.pressure, 101325.0f), null);

            bool vle = true, lle = false, sle = false, critical = false;

            p1.CreateAndAddLabelRow("Display Options");

            p1.CreateAndAddCheckBoxRow("VLE", vle, (arg1a, arg2a) => { vle = arg1a.Checked.GetValueOrDefault(); });
            p1.CreateAndAddDescriptionRow("VLE calculation works on all diagram types.");
            p1.CreateAndAddCheckBoxRow("LLE", lle, (arg1a, arg2a) => { lle = arg1a.Checked.GetValueOrDefault(); });
            p1.CreateAndAddDescriptionRow("LLE calculation works on T-x/y and P-x/y diagrams if the selected Property Package is associated with a Flash Algorithm which supports Liquid-Liquid equilibria.");
            p1.CreateAndAddCheckBoxRow("SLE", sle, (arg1a, arg2a) => { sle = arg1a.Checked.GetValueOrDefault(); });
            p1.CreateAndAddDescriptionRow("SLE calculation works on T-x/y diagrams only.");
            p1.CreateAndAddCheckBoxRow("Critical Line", critical, (arg1a, arg2a) => { critical = arg1a.Checked.GetValueOrDefault(); });
            p1.CreateAndAddDescriptionRow("Critical Line calculation works on T-x/y diagrams only.");

            var pplist = flowsheet.PropertyPackages.Values.Select((x2) => x2.Tag).ToList();

            pplist.Insert(0, "");

            var spinnerpp = p1.CreateAndAddDropDownRow("Property Package", pplist, 0, null);

            var button = p1.CreateAndAddButtonRow("Build Envelope", null, null);
            
            chart = new Eto.OxyPlot.Plot { BackgroundColor = Colors.White };

            var txtResults = new TextArea() { ReadOnly = true, Font = Fonts.Monospace(GlobalSettings.Settings.ResultsReportFontSize) };

            tabcontainer.Pages.Add(new TabPage(new TableRow(chart)) { Text = "Chart" });
            tabcontainer.Pages.Add(new TabPage(new TableRow(txtResults)) { Text = "Data" });

            p2.CreateAndAddLabelRow("Results");

            p2.CreateAndAddControlRow(tabcontainer);
            
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
                    calc.BinaryEnvelopeOptions = new object[] { "", 0, 0, vle, lle, sle, critical, false };

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

                    var pg = ProgressDialog.Show(this, "Please Wait", "Calculating Envelope Lines...", false);

                    Task.Factory.StartNew(() =>
                    {
                        Application.Instance.Invoke(() =>
                        {
                            txtResults.Text = "Please wait...";
                
                        });
                        results = calc.Calculate();
                    }).ContinueWith((t) =>
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