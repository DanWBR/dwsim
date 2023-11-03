using System;
using System.Collections.Generic;
using System.Linq;
using DWSIM.Interfaces;
using DWSIM.UnitOperations.UnitOperations;
using DWSIM.UnitOperations.Reactors;
using Eto.Forms;
using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;
using s = DWSIM.UI.Shared.Common;
using Eto.Drawing;
using OxyPlot;
using OxyPlot.Axes;
using DWSIM.ExtensionMethods;
using DWSIM.UI.Shared;
using DWSIM.CrossPlatform.UI.Controls.ReoGrid.DataFormat;
using DWSIM.CrossPlatform.UI.Controls;
using DWSIM.CrossPlatform.UI.Controls.ReoGrid;
using DWSIM.Thermodynamics.Databases.ChemeoLink;

namespace DWSIM.UI.Desktop.Editors
{
    public class Results
    {

        static string imgprefix = "DWSIM.UI.Desktop.Editors.Resources.Icons.";

        public ISimulationObject SimObject;

        public TableLayout container;

        public Results(ISimulationObject selectedobject, TableLayout layout)
        {
            SimObject = selectedobject;
            container = layout;
            Initialize();
        }

        void Initialize()
        {

            var su = SimObject.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            var nf = SimObject.GetFlowsheet().FlowsheetOptions.NumberFormat;

            if (SimObject is Pipe)
            {
                var pipe = (Pipe)SimObject;
                string[] datatype = {"Length", "Inclination", "Pressure", "Temperature",
                    "Liquid Velocity", "Vapor Velocity", "Heat Flow", "Liquid Holdup",
                    "Overall HTC","Internal HTC","Wall k/L","Insulation k/L", "External HTC"};

                string[] units = { su.distance, "degrees", su.pressure, su.temperature, su.velocity, su.velocity,
                                    su.heatflow, "", su.heat_transf_coeff, su.heat_transf_coeff, su.heat_transf_coeff,
                                    su.heat_transf_coeff, su.heat_transf_coeff};

                var btn = new Button { Text = "View Pipe Properties Profile" };
                container.Rows.Add(new TableRow(btn));
                btn.Click += (sender, e) =>
                {

                    var plotcontainer = s.GetDefaultContainer();

                    var chart = new Eto.OxyPlot.Plot() { Height = 400, BackgroundColor = Colors.White };

                    chart.Visible = true;

                    List<double> px, py;

                    var txtres = s.CreateAndAddMultilineMonoSpaceTextBoxRow(plotcontainer, "", 400, true, null);

                    if (SimObject.GraphicObject != null)
                    {
                        s.CreateAndAddLabelRow(plotcontainer, "Pipe Segment Profiles: " + SimObject.GraphicObject.Tag);
                    }
                    else
                    {
                        s.CreateAndAddLabelRow(plotcontainer, "Pipe Segment Profiles");
                    }
                    var xsp = s.CreateAndAddDropDownRow(plotcontainer, "X Axis Data", datatype.ToList(), 0, null);
                    var ysp = s.CreateAndAddDropDownRow(plotcontainer, "Y Axis Data", datatype.ToList(), 2, null);
                    s.CreateAndAddButtonRow(plotcontainer, "Update Chart/Table", null, (sender2, e2) =>
                    {
                        px = PopulateData(pipe, xsp.SelectedIndex);
                        py = PopulateData(pipe, ysp.SelectedIndex);
                        var model = CreatePipeResultsModel(px.ToArray(), py.ToArray(),
                                                           datatype[xsp.SelectedIndex] + " (" + units[xsp.SelectedIndex] + ")",
                                                           datatype[ysp.SelectedIndex] + " (" + units[ysp.SelectedIndex] + ")");
                        chart.Model = model;
                        chart.Visible = true;
                        chart.Model.InvalidatePlot(true);
                        chart.Invalidate();
                        int i = 0;
                        var txt = new System.Text.StringBuilder();
                        txt.AppendLine(datatype[xsp.SelectedIndex] + " (" + units[xsp.SelectedIndex] + ")\t\t" + datatype[ysp.SelectedIndex] + " (" + units[ysp.SelectedIndex] + ")");
                        for (i = 0; i <= px.Count - 1; i++)
                        {
                            txt.AppendLine(px[i].ToString(nf) + "\t\t" + py[i].ToString(nf));
                        }
                        txtres.Text = txt.ToString();
                    });
                    s.CreateAndAddLabelRow(plotcontainer, "Results Chart");
                    s.CreateAndAddControlRow(plotcontainer, chart);
                    s.CreateAndAddEmptySpace(plotcontainer);
                    s.CreateAndAddLabelRow(plotcontainer, "Results Table");
                    s.CreateAndAddControlRow(plotcontainer, txtres);

                    if (SimObject.GraphicObject != null)
                    {
                        var form = s.GetDefaultEditorForm("Pipe Properties Profile: " + SimObject.GraphicObject.Tag, 400, 500, plotcontainer);
                        form.Topmost = true;
                        form.Show();
                    }
                    else
                    {
                        var form = s.GetDefaultEditorForm("Pipe Properties Profile", 400, 500, plotcontainer);
                        form.Topmost = true;
                        form.Show();
                    }
                };
            }
            else if (SimObject is Column)
            {
                var column = (Column)SimObject;
                string[] datatype = { "Stage", "Pressure", "Temperature", "Vapor Molar Flow", "Liquid Molar Flow" };

                string[] units = { "", su.pressure, su.temperature, su.molarflow, su.molarflow };

                var btn = new Button { Text = "View Column Properties Profile" };
                container.Rows.Add(new TableRow(btn));
                btn.Click += (sender, e) =>
                {

                    var plotcontainer = s.GetDefaultContainer();

                    var chart = new Eto.OxyPlot.Plot() { Height = 400, BackgroundColor = Colors.White };
                    chart.Visible = true;

                    List<double> px, py;

                    s.CreateAndAddLabelRow(plotcontainer, "Column Profile Results: " + SimObject.GraphicObject.Tag);
                    var xsp = s.CreateAndAddDropDownRow(plotcontainer, "X Axis Data", datatype.ToList(), 2, null);
                    var ysp = s.CreateAndAddDropDownRow(plotcontainer, "Y Axis Data", datatype.ToList(), 0, null);

                    s.CreateAndAddButtonRow(plotcontainer, "Update Chart", null, (sender2, e2) =>
                    {
                        px = PopulateColumnData(column, xsp.SelectedIndex);
                        py = PopulateColumnData(column, ysp.SelectedIndex);
                        string xunits, yunits;
                        xunits = " (" + units[xsp.SelectedIndex] + ")";
                        yunits = " (" + units[ysp.SelectedIndex] + ")";
                        if (xsp.SelectedIndex == 0) { xunits = ""; }
                        if (ysp.SelectedIndex == 0) { yunits = ""; }
                        var model = CreateColumnResultsModel(px.ToArray(), py.ToArray(),
                                                           datatype[xsp.SelectedIndex] + xunits,
                                                           datatype[ysp.SelectedIndex] + yunits);
                        chart.Model = model;
                        chart.Visible = true;
                        chart.Model.InvalidatePlot(true);
                        chart.Invalidate();
                    });

                    s.CreateAndAddLabelRow(plotcontainer, "Results Chart");
                    s.CreateAndAddControlRow(plotcontainer, chart);
                    s.CreateAndAddEmptySpace(plotcontainer);
                    var form = s.GetDefaultEditorForm("Column Profile: " + SimObject.GraphicObject.Tag, 400, 500, plotcontainer);
                    form.Topmost = true;
                    form.Show();

                };
            }
            else if (SimObject is Reactor_PFR)
            {
                var reactor = (Reactor_PFR)SimObject;

                if (reactor.points != null && reactor.points.Count > 0)
                {

                    var btn2 = new Button { Text = "Export Profile to new Spreadsheet" };
                    container.Rows.Add(new TableRow(btn2));
                    btn2.Click += (sender, e) =>
                    {
                        var grid = (ReoGridControl)SimObject.GetFlowsheet().GetSpreadsheetObject();
                        var sheet = grid.CreateWorksheet(SimObject.GraphicObject.Tag + "_" +new Random().Next(1000).ToString());
                        grid.Worksheets.Add(sheet);
                        grid.CurrentWorksheet = sheet;
                        var item = reactor.Profile[0];
                        sheet.Cells[0, 0].Data = string.Format("Length ({0})", su.distance);
                        sheet.Cells[0, 1].Data = string.Format("Temperature ({0})", su.temperature);
                        sheet.Cells[0, 2].Data = string.Format("Pressure ({0})", su.pressure);
                        int i;
                        int j = 3;
                        foreach (var pitem in item.Item4)
                        {
                            sheet.Cells[0, j].Data = string.Format("{0} MolFrac", pitem.Compound);
                            sheet.Cells[0, (j + 1)].Data = string.Format("{0} MassFrac", pitem.Compound);
                            sheet.Cells[0, (j + 2)].Data = string.Format("{0} MolFlow ({1})", pitem.Compound, su.molarflow);
                            sheet.Cells[0, (j + 3)].Data = string.Format("{0} MassFlow ({1})", pitem.Compound, su.massflow);
                            sheet.Cells[0, (j + 4)].Data = string.Format("{0} MolConc ({1})", pitem.Compound, su.molar_conc);
                            sheet.Cells[0, (j + 5)].Data = string.Format("{0} MassConc ({1})", pitem.Compound, su.mass_conc);
                            j+=6;
                        }

                        i = 1;
                        foreach (var item2 in reactor.Profile)
                        {
                            sheet.Cells[i, 0].Data = item2.Item1.ConvertFromSI(su.distance);
                            sheet.Cells[i, 1].Data = item2.Item2.ConvertFromSI(su.temperature);
                            sheet.Cells[i, 2].Data = item2.Item3.ConvertFromSI(su.pressure);
                            j = 3;
                            foreach (var pitem in item2.Item4)
                            {
                                sheet.Cells[i, j].Data = pitem.MolarFraction;
                                sheet.Cells[i, (j + 1)].Data = pitem.MassFraction;
                                sheet.Cells[i, (j + 2)].Data = pitem.MolarFlow.ConvertFromSI(su.molarflow);
                                sheet.Cells[i, (j + 3)].Data = pitem.MassFlow.ConvertFromSI(su.massflow);
                                sheet.Cells[i, (j + 4)].Data = pitem.MolarConcentration.ConvertFromSI(su.molar_conc);
                                sheet.Cells[i, (j + 5)].Data = pitem.MassConcentration.ConvertFromSI(su.mass_conc);
                                j+=6;
                            }
                            i++;
                        }

                        sheet.SetRangeDataFormat(new RangePosition(1, 0, i, (j + 5)), CellDataFormatFlag.Number, new NumberDataFormatter.NumberFormatArgs() { DecimalPlaces=6, NegativeStyle=NumberDataFormatter.NumberNegativeStyle.Minus, UseSeparator=false });
                        for (int k = 0; (k <= j); k++)
                        {
                            sheet.AutoFitColumnWidth(k);
                        }

                        MessageBox.Show(string.Format("Data export finished successfully to sheet \'{0}\'.", sheet.Name), "DWSIM", MessageBoxButtons.OK, MessageBoxType.Information);

                    };

                    var btn = new Button { Text = "View PFR Properties Profile" };
                    container.Rows.Add(new TableRow(btn));
                    btn.Click += (sender, e) =>
                    {

                        var chart = new Eto.OxyPlot.Plot() { Height = 400, BackgroundColor = Colors.White };
                        chart.Visible = true;

                        var model = CreatePFRResultsModel(reactor);
                        chart.Model = model;
                        chart.Model.InvalidatePlot(true);
                        chart.Invalidate();

                        var form = new Form()
                        {
                            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico"),
                            Content = new Scrollable { Content = chart, Border = BorderType.None, ExpandContentWidth = true, ExpandContentHeight = true },
                            Title = "PFR Profile: " + SimObject.GraphicObject.Tag,
                            ClientSize = new Size(800, 600),
                            ShowInTaskbar = false,
                            Maximizable = false,
                            Minimizable = false,
                            Topmost = true,
                            Resizable = true
                        };
                        form.Show();

                    };
                }
            }
            else if (SimObject is HeatExchanger)
            {
                var hx = (HeatExchanger)SimObject;

                if (hx.HeatProfile.Length > 0)
                {

                    var btn = new Button { Text = "View Heat Exchanged Profile" };
                    container.Rows.Add(new TableRow(btn));
                    btn.Click += (sender, e) =>
                    {

                        var chart = new Eto.OxyPlot.Plot() { Height = 400, BackgroundColor = Colors.White };
                        chart.Visible = true;

                        var model = s.CreatePlotModel(hx.HeatProfile.ToList().ConvertFromSI(su.heatflow).ToArray(),
                            hx.TemperatureProfileCold.ToList().ConvertFromSI(su.temperature).ToArray(),
                            hx.TemperatureProfileHot.ToList().ConvertFromSI(su.temperature).ToArray(),
                            "Heat Profile", hx.GraphicObject.Tag, "Heat Exchanged (" + su.heatflow + ")",
                            "Temperature (" + su.temperature + ")",
                            "Cold Fluid", "Hot Fluid");
                        chart.Model = model;
                        chart.Model.InvalidatePlot(true);
                        chart.Invalidate();

                        var maxT = Math.Max(hx.TemperatureProfileHot.Max(), hx.TemperatureProfileCold.Max());
                        var minT = Math.Min(hx.TemperatureProfileHot.Min(), hx.TemperatureProfileCold.Min());

                        model.AddLineSeries(new double[] { hx.Q.GetValueOrDefault(), hx.Q.GetValueOrDefault() }.ConvertUnits("kW", su.heatflow),
                            new double[] { minT, maxT }.ConvertUnits("K", su.temperature), OxyColors.Red, "Operating Point");

                        model.LegendItemAlignment = OxyPlot.HorizontalAlignment.Center;
                        model.LegendOrientation = LegendOrientation.Horizontal;

                        var doccontainer = new DocumentControl();
                        var docp1 = new DocumentPage { Text = "Chart", Closable = false };
                        var docp2 = new DocumentPage { Text = "Table", Closable = false };
                        doccontainer.Pages.Add(docp1);
                        doccontainer.Pages.Add(docp2);

                        var gridcontrol = GridControl.GetGridControl();

                        var grid = gridcontrol.GridControl;
                        grid.Width = 700;

                        var sheet = grid.Worksheets[0];

                        sheet.SetRows(100);
                        sheet.SetCols(3);
                        sheet.SetColumnsWidth(0, 3, 200);
                        sheet.ColumnHeaders[0].Text = String.Format("Heat Exchanged ({0})", su.heatflow);
                        sheet.ColumnHeaders[1].Text = String.Format("Hot Fluid Temperature ({0})", su.temperature);
                        sheet.ColumnHeaders[2].Text = String.Format("Cold Fluid Temperature ({0})", su.temperature);

                        for (int i = 0; i < hx.HeatProfile.Count(); i++)
                        {
                            sheet.Cells[i, 0].Data = hx.HeatProfile[i].ConvertFromSI(su.heatflow);
                            sheet.Cells[i, 1].Data = hx.TemperatureProfileHot[i].ConvertFromSI(su.temperature);
                            sheet.Cells[i, 2].Data = hx.TemperatureProfileCold[i].ConvertFromSI(su.temperature);
                        }

                        docp2.Content = gridcontrol;

                        docp1.Content = chart;

                        var form = new Form()
                        {
                            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico"),
                            Content = new Scrollable { Content = doccontainer, Border = BorderType.None, ExpandContentWidth = true, ExpandContentHeight = true },
                            Title = "Heat Profile: " + SimObject.GraphicObject.Tag,
                            ClientSize = new Size(800, 600),
                            ShowInTaskbar = false,
                            Maximizable = false,
                            Minimizable = false,
                            Topmost = true,
                            Resizable = true
                        };
                        form.Show();
                        form.Center();

                    };
                }
            }

            var obj = (ISimulationObject)SimObject;

            try
            {
                var structreport = obj.GetStructuredReport();
                if (structreport.Count > 0)
                {

                    var containerd = UI.Shared.Common.GetDefaultContainer();
                    container.Rows.Add(new TableRow(containerd));
                    foreach (var item in structreport)
                    {
                        switch (item.Item1)
                        {
                            case Interfaces.Enums.ReportItemType.Label:
                                containerd.CreateAndAddLabelRow(item.Item2[0]);
                                break;
                            case Interfaces.Enums.ReportItemType.Description:
                                containerd.CreateAndAddDescriptionRow(item.Item2[0]);
                                break;
                            case Interfaces.Enums.ReportItemType.SingleColumn:
                                containerd.CreateAndAddLabelRow2(item.Item2[0]);
                                break;
                            case Interfaces.Enums.ReportItemType.DoubleColumn:
                                containerd.CreateAndAddThreeLabelsRow(item.Item2[0], item.Item2[1], "");
                                break;
                            case Interfaces.Enums.ReportItemType.TripleColumn:
                                containerd.CreateAndAddThreeLabelsRow(item.Item2[0], item.Item2[1], item.Item2[2]);
                                break;
                        }
                    }
                    containerd.EndVertical();
                }
                else
                {
                    var txtcontrol = new TextArea { ReadOnly = true };
                    txtcontrol.Font = GlobalSettings.Settings.RunningPlatform() == GlobalSettings.Settings.Platform.Mac ? new Font("Menlo", GlobalSettings.Settings.ResultsReportFontSize) : Fonts.Monospace(GlobalSettings.Settings.ResultsReportFontSize);

                    container.Rows.Add(new TableRow(txtcontrol));

                    try
                    {
                        if (obj.Calculated)
                        {
                            txtcontrol.Text = "Object successfully calculated on " + obj.LastUpdated.ToString() + "\n\n";
                            txtcontrol.Text += obj.GetReport(SimObject.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem,
                                                       System.Globalization.CultureInfo.InvariantCulture,
                                                        SimObject.GetFlowsheet().FlowsheetOptions.NumberFormat);
                        }
                        else
                        {
                            if (obj.ErrorMessage != "")
                            {
                                txtcontrol.Text = "An error occured during the calculation of this object. Details:\n\n" + obj.ErrorMessage;
                            }
                            else
                            {
                                txtcontrol.Text = "This object hasn't been calculated yet.";
                            }
                        }
                    }
                    catch (Exception ex)
                    {
                        txtcontrol.Text = "Report generation failed. Please recalculate the flowsheet and try again.";
                        txtcontrol.Text += "\n\nError details: " + ex.ToString();
                    }

                }
            }
            catch (Exception ex)
            {
                var txtcontrol = new TextArea { ReadOnly = true };
                txtcontrol.Font = GlobalSettings.Settings.RunningPlatform() == GlobalSettings.Settings.Platform.Mac ? new Font("Menlo", GlobalSettings.Settings.ResultsReportFontSize) : Fonts.Monospace(GlobalSettings.Settings.ResultsReportFontSize);
                container.Rows.Add(new TableRow(txtcontrol));
                txtcontrol.Text = "Report generation failed.";
                txtcontrol.Text += "\n\nError details: " + ex.ToString();
            }
        }

        List<double> PopulateData(Pipe pipe, int position)
        {
            int qi = 1;
            var su = SimObject.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            List<double> vec = new List<double>();
            switch (position)
            {
                case 0: //distance

                    double comp_ant = 0.0f;
                    foreach (var sec in pipe.Profile.Sections.Values)
                    {
                        for (qi = 1; qi == 1; qi++)
                        {
                            foreach (var res in sec.Results)
                            {
                                vec.Add(cv.ConvertFromSI(su.distance, comp_ant));
                                comp_ant += sec.Comprimento / sec.Incrementos;
                            }
                        }
                    }
                    break;
                case 1: //elevation

                    foreach (var sec in pipe.Profile.Sections.Values)
                    {
                        for (qi = 1; qi == 1; qi++)
                        {
                            foreach (var res in sec.Results)
                            {
                                vec.Add(Math.Atan(sec.Elevacao / Math.Pow(Math.Pow(sec.Comprimento, 2) - Math.Pow(sec.Elevacao, 2), 0.5) * 180 / Math.PI));
                            }
                        }
                    }
                    break;
                case 2: //pressure

                    foreach (var sec in pipe.Profile.Sections.Values)
                    {
                        for (qi = 1; qi == 1; qi++)
                        {
                            foreach (var res in sec.Results)
                            {
                                vec.Add(cv.ConvertFromSI(su.pressure, res.Pressure_Initial.GetValueOrDefault()));
                            }
                        }
                    }
                    break;
                case 3: //temperaturee

                    foreach (var sec in pipe.Profile.Sections.Values)
                    {
                        for (qi = 1; qi == 1; qi++)
                        {
                            foreach (var res in sec.Results)
                            {
                                vec.Add(cv.ConvertFromSI(su.temperature, res.Temperature_Initial.GetValueOrDefault()));
                            }
                        }
                    }
                    break;
                case 4: //vel liqe

                    foreach (var sec in pipe.Profile.Sections.Values)
                    {
                        for (qi = 1; qi == 1; qi++)
                        {
                            foreach (var res in sec.Results)
                            {
                                vec.Add(cv.ConvertFromSI(su.velocity, res.LiqVel.GetValueOrDefault()));
                            }
                        }
                    }
                    break;
                case 5: //vel vape

                    foreach (var sec in pipe.Profile.Sections.Values)
                    {
                        for (qi = 1; qi == 1; qi++)
                        {
                            foreach (var res in sec.Results)
                            {
                                vec.Add(cv.ConvertFromSI(su.velocity, res.VapVel.GetValueOrDefault()));
                            }
                        }
                    }
                    break;
                case 6: //heatflowe

                    foreach (var sec in pipe.Profile.Sections.Values)
                    {
                        for (qi = 1; qi == 1; qi++)
                        {
                            foreach (var res in sec.Results)
                            {
                                vec.Add(cv.ConvertFromSI(su.heatflow, res.HeatTransferred.GetValueOrDefault()));
                            }
                        }
                    }
                    break;
                case 7: //liqholde

                    foreach (var sec in pipe.Profile.Sections.Values)
                    {
                        for (qi = 1; qi == 1; qi++)
                        {
                            foreach (var res in sec.Results)
                            {
                                vec.Add(res.LiquidHoldup.GetValueOrDefault());
                            }
                        }
                    }
                    break;
                case 8: //OHTCe

                    foreach (var sec in pipe.Profile.Sections.Values)
                    {
                        for (qi = 1; qi == 1; qi++)
                        {
                            foreach (var res in sec.Results)
                            {
                                vec.Add(cv.ConvertFromSI(su.heat_transf_coeff, res.HTC.GetValueOrDefault()));
                            }
                        }
                    }
                    break;
                case 9: //IHTCC

                    foreach (var sec in pipe.Profile.Sections.Values)
                    {
                        for (qi = 1; qi == 1; qi++)
                        {
                            foreach (var res in sec.Results)
                            {
                                vec.Add(cv.ConvertFromSI(su.heat_transf_coeff, res.HTC_internal));
                            }
                        }
                    }
                    break;
                case 10: //IHTC

                    foreach (var sec in pipe.Profile.Sections.Values)
                    {
                        for (qi = 1; qi == 1; qi++)
                        {
                            foreach (var res in sec.Results)
                            {
                                vec.Add(cv.ConvertFromSI(su.heat_transf_coeff, res.HTC_pipewall));
                            }
                        }
                    }
                    break;
                case 11: //IHTC

                    foreach (var sec in pipe.Profile.Sections.Values)
                    {
                        for (qi = 1; qi == 1; qi++)
                        {
                            foreach (var res in sec.Results)
                            {
                                vec.Add(cv.ConvertFromSI(su.heat_transf_coeff, res.HTC_insulation));
                            }
                        }
                    }
                    break;
                case 12: //EHTC

                    foreach (var sec in pipe.Profile.Sections.Values)
                    {
                        for (qi = 1; qi == 1; qi++)
                        {
                            foreach (var res in sec.Results)
                            {
                                vec.Add(cv.ConvertFromSI(su.heat_transf_coeff, res.HTC_external));
                            }
                        }
                    }
                    break;
            }
            return vec;
        }

        OxyPlot.PlotModel CreatePipeResultsModel(double[] x, double[] y, string xtitle, string ytitle)
        {
            var title = "";
            if (SimObject.GraphicObject != null)
            {
                title = SimObject.GraphicObject.Tag;
            }
            else
            {
                title = "Pipe Segment";
            }
            var model = new global::OxyPlot.PlotModel() { Subtitle = "Properties Profile", Title = title };
            model.TitleFontSize = 14;
            model.SubtitleFontSize = 11;
            model.Axes.Add(new LinearAxis()
            {
                MajorGridlineStyle = LineStyle.Dash,
                MinorGridlineStyle = LineStyle.Dot,
                Position = AxisPosition.Bottom,
                FontSize = 12,
                Title = xtitle
            });
            model.Axes.Add(new LinearAxis()
            {
                MajorGridlineStyle = LineStyle.Dash,
                MinorGridlineStyle = LineStyle.Dot,
                Position = AxisPosition.Left,
                FontSize = 12,
                Title = ytitle
            });
            model.LegendFontSize = 11;
            model.LegendPlacement = LegendPlacement.Outside;
            model.LegendOrientation = LegendOrientation.Vertical;
            model.LegendPosition = LegendPosition.BottomCenter;
            model.TitleHorizontalAlignment = TitleHorizontalAlignment.CenteredWithinView;
            model.AddLineSeries(x, y);

            return model;

        }

        OxyPlot.PlotModel CreatePFRResultsModel(Reactor_PFR reactor)
        {

            var su = SimObject.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;

            var model = new PlotModel() { Subtitle = "Properties Profile", Title = SimObject.GraphicObject.Tag };
            model.TitleFontSize = 14;
            model.SubtitleFontSize = 11;
            model.Axes.Add(new LinearAxis()
            {
                MajorGridlineStyle = LineStyle.Dash,
                MinorGridlineStyle = LineStyle.Dot,
                Position = AxisPosition.Bottom,
                FontSize = 12,
                Title = "Reactor Length (" + su.distance + ")"
            });
            model.Axes.Add(new LinearAxis()
            {
                MajorGridlineStyle = LineStyle.Dash,
                MinorGridlineStyle = LineStyle.Dot,
                Position = AxisPosition.Left,
                FontSize = 12,
                Title = "Concentration (" + su.molar_conc + ")",
                Key = "conc"
            });
            model.Axes.Add(new LinearAxis()
            {
                MajorGridlineStyle = LineStyle.Dash,
                MinorGridlineStyle = LineStyle.Dot,
                Position = AxisPosition.Right,
                FontSize = 12,
                Title = "Temperature (" + su.temperature + ")",
                Key = "temp",
                PositionTier = 0
            });
            model.Axes.Add(new LinearAxis()
            {
                MajorGridlineStyle = LineStyle.Dash,
                MinorGridlineStyle = LineStyle.Dot,
                Position = AxisPosition.Right,
                FontSize = 12,
                Title = "Pressure (" + su.pressure + ")",
                Key = "press",
                PositionTier = 1
            });

            model.LegendFontSize = 11;
            model.LegendPlacement = LegendPlacement.Outside;
            model.LegendOrientation = LegendOrientation.Horizontal;
            model.LegendPosition = LegendPosition.BottomCenter;
            model.TitleHorizontalAlignment = TitleHorizontalAlignment.CenteredWithinView;

            List<double> vx = new List<double>(), vy = new List<double>();
            List<List<double>> vya = new List<List<double>>();
            List<string> vn = new List<string>();

            foreach (var obj in reactor.points)
            {
                vx.Add(((double[])obj)[0]);
            }
            int j;
            for (j = 1; j <= reactor.ComponentConversions.Count + 2; j++)
            {
                vy = new List<double>();
                foreach (var obj in reactor.points)
                {
                    vy.Add(((double[])obj)[j]);
                }
                vya.Add(vy);
            }
            foreach (var st in reactor.ComponentConversions.Keys)
            {
                vn.Add(st);
            }
            OxyColor color;
            for (j = 0; j <= vn.Count - 1; j++)
            {
                color = OxyColor.FromRgb(Convert.ToByte(new Random().Next(0, 255)), Convert.ToByte(new Random().Next(0, 255)), Convert.ToByte(new Random().Next(0, 255)));
                model.AddLineSeries(cv.ConvertArrayFromSI(su.volume, vx.ToArray()), cv.ConvertArrayFromSI(su.molar_conc, vya[j].ToArray()), color);
                model.Series[model.Series.Count - 1].Title = vn[j];
                ((OxyPlot.Series.LineSeries)(model.Series[model.Series.Count - 1])).YAxisKey = "conc";
            }
            color = OxyColor.FromRgb(Convert.ToByte(new Random().Next(0, 255)), Convert.ToByte(new Random().Next(0, 255)), Convert.ToByte(new Random().Next(0, 255)));
            model.AddLineSeries(cv.ConvertArrayFromSI(su.volume, vx.ToArray()), cv.ConvertArrayFromSI(su.temperature, vya[j].ToArray()), color);
            model.Series[model.Series.Count - 1].Title = "Temperature";
            ((OxyPlot.Series.LineSeries)(model.Series[model.Series.Count - 1])).YAxisKey = "temp";
            color = OxyColor.FromRgb(Convert.ToByte(new Random().Next(0, 255)), Convert.ToByte(new Random().Next(0, 255)), Convert.ToByte(new Random().Next(0, 255)));
            model.AddLineSeries(cv.ConvertArrayFromSI(su.volume, vx.ToArray()), cv.ConvertArrayFromSI(su.pressure, vya[j + 1].ToArray()), color);
            model.Series[model.Series.Count - 1].Title = "Pressure";
            ((OxyPlot.Series.LineSeries)(model.Series[model.Series.Count - 1])).YAxisKey = "press";

            return model;


        }

        List<double> PopulateColumnData(Column col, int position)
        {
            var su = SimObject.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem;
            List<double> vec = new List<double>();
            switch (position)
            {
                case 0: //distance

                    double comp_ant = 1.0f;
                    foreach (var st in col.Stages)
                    {
                        vec.Add(comp_ant);
                        comp_ant += 1.0f;
                    }
                    break;
                case 1: //pressure
                    vec = cv.ConvertArrayFromSI(su.pressure, col.P0).ToList();
                    break;
                case 2: //temperature
                    vec = cv.ConvertArrayFromSI(su.temperature, col.Tf).ToList();
                    break;
                case 3: //vapor flow
                    vec = cv.ConvertArrayFromSI(su.molarflow, col.Vf).ToList();
                    break;
                case 4: //liquid flow
                    vec = cv.ConvertArrayFromSI(su.molarflow, col.Lf).ToList();
                    break;
            }
            return vec;
        }

        OxyPlot.PlotModel CreateColumnResultsModel(double[] x, double[] y, string xtitle, string ytitle)
        {
            var model = new PlotModel() { Subtitle = "Column Profile", Title = SimObject.GraphicObject.Tag };
            model.TitleFontSize = 14;
            model.SubtitleFontSize = 11;
            model.Axes.Add(new LinearAxis()
            {
                MajorGridlineStyle = LineStyle.Dash,
                MinorGridlineStyle = LineStyle.Dot,
                Position = AxisPosition.Bottom,
                FontSize = 12,
                Title = xtitle
            });
            if (Math.Abs(y[0] - 1.0f) < 0.0001)
            {
                model.Axes.Add(new LinearAxis()
                {
                    MajorGridlineStyle = LineStyle.Dash,
                    MinorGridlineStyle = LineStyle.Dot,
                    Position = AxisPosition.Left,
                    FontSize = 12,
                    Title = ytitle,
                    StartPosition = 1,
                    EndPosition = 0,
                    MajorStep = 1.0f,
                    MinorStep = 0.5f
                });
            }
            else
            {
                model.Axes.Add(new LinearAxis()
                {
                    MajorGridlineStyle = LineStyle.Dash,
                    MinorGridlineStyle = LineStyle.Dot,
                    Position = AxisPosition.Left,
                    FontSize = 12,
                    Title = ytitle
                });
            }
            model.LegendFontSize = 11;
            model.LegendPlacement = LegendPlacement.Outside;
            model.LegendOrientation = LegendOrientation.Vertical;
            model.LegendPosition = LegendPosition.BottomCenter;
            model.TitleHorizontalAlignment = TitleHorizontalAlignment.CenteredWithinView;
            model.AddLineSeries(x, y);

            return model;

        }

    }

}