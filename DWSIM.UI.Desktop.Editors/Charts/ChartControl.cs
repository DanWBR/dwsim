using Eto.Forms;
using System;
using OxyPlot;
using Eto.OxyPlot;
using DWSIM.SharedClasses.Charts;
using System.Collections.Generic;
using DWSIM.Interfaces.Enums;
using DWSIM.CrossPlatform.UI.Controls.ReoGrid;
using DWSIM.ExtensionMethods;
using DWSIM.UI.Shared;
using OxyPlot.Series;
using System.Linq;

namespace DWSIM.UI.Desktop.Editors.Charts
{
    class ChartControl : DynamicLayout
    {

        public Plot ChartView;
        public Chart Chart;
        public DWSIM.CrossPlatform.UI.Controls.ReoGrid.ReoGridControl Spreadsheet;
        public DWSIM.UI.Desktop.Shared.Flowsheet Flowsheet;
        private List<string> ColorChoices = new List<string>();
        public Splitter Splitter;

        public ChartControl()
        {
            var colors = typeof(OxyColors).GetFields();

            foreach (var c in colors)
            {
                ColorChoices.Add(c.Name);
            }

            Chart = new Chart();

            Splitter = new Splitter { Orientation = Orientation.Horizontal, FixedPanel = SplitterFixedPanel.Panel2 };

            ChartView = new Plot();

            ChartView.Model = (PlotModel)Chart.PlotModel;

            Splitter.Panel1 = ChartView;

            var PropertiesLayout = new DynamicLayout();

            Splitter.Panel2 = new Scrollable { Content = PropertiesLayout };

            Splitter.Panel2.Width = 350;

            this.Add(Splitter, true, true);

            UpdatePlotModelData();
            UpdatePropertiesLayout();

        }

        public void UpdatePropertiesLayout()
        {

            Application.Instance.Invoke(() =>
            {

                try
                {

                    var pm = (PlotModel)Chart.PlotModel;

                    var pl = new DynamicLayout();

                    pl.Padding = new Eto.Drawing.Padding(10);

                    pl.CreateAndAddButtonRow("Update Chart Data", null, (btn, e) =>
                        {
                            Application.Instance.Invoke(() =>
                            {
                                UpdatePlotModelData();
                                UpdatePropertiesLayout();
                            });
                        });

                    pl.CreateAndAddLabelRow("General");

                    pl.CreateAndAddStringEditorRow("Chart Name", Chart.DisplayName, (t, e) =>
                        {
                            Chart.DisplayName = t.Text;
                        }, () => Application.Instance.Invoke(() => ((DocumentPage)Parent).Text = Chart.DisplayName));

                    pl.CreateAndAddDropDownRow("Data Source", Chart.ChartSource.GetEnumNames(), (int)Chart.ChartSource, (dd, e) =>
                        {
                            Chart.ChartSource = (ChartSource)dd.SelectedIndex;
                            UpdatePropertiesLayout();
                        });

                    if (Chart.ChartSource == ChartSource.FlowsheetObject)
                    {

                        pl.CreateAndAddLabelRow("Data Source");

                        var objlist = new List<String>();

                        objlist = Flowsheet.SimulationObjects.Values.Select((x) => x.GraphicObject.Tag).ToList();
                        objlist.Insert(0, "");

                        string selobj = "";

                        if (Flowsheet.SimulationObjects.ContainsKey(Chart.ChartSourceObjectID))
                        {
                            selobj = Flowsheet.SimulationObjects[Chart.ChartSourceObjectID].GraphicObject.Tag;
                        }

                        pl.CreateAndAddDropDownRow("Source Object", objlist, objlist.IndexOf(selobj), (dd, e) =>
                            {

                                var obj = Flowsheet.GetFlowsheetSimulationObject(dd.SelectedValue.ToString());
                                if (obj != null) Chart.ChartSourceObjectID = obj.Name;
                                UpdatePropertiesLayout();
                            });

                        if (Flowsheet.SimulationObjects.ContainsKey(Chart.ChartSourceObjectID))
                        {

                            var obj = Flowsheet.GetFlowsheetSimulationObject(selobj);
                            var chartnames = obj.GetChartModelNames();
                            var cindex = 0;

                            if (Chart.ChartSourceObjectChartID != "")
                            {
                                if (chartnames.Contains(Chart.ChartSourceObjectChartID))
                                {
                                    cindex = chartnames.IndexOf(Chart.ChartSourceObjectChartID);
                                }
                            }

                            pl.CreateAndAddDropDownRow("Source Object Chart Type", chartnames, cindex, (dd, e) =>
                                {

                                    Chart.ChartSourceObjectChartID = chartnames[dd.SelectedIndex];
                                    Application.Instance.Invoke(() =>
                                    {
                                        UpdatePlotModelData();
                                        UpdatePropertiesLayout();
                                    });


                                });

                            pl.CreateAndAddCheckBoxRow("Auto Update Chart from Object", Chart.ChartSourceObjectAutoUpdate,
                                    (chk, e) => { Chart.ChartSourceObjectAutoUpdate = chk.Checked.GetValueOrDefault(); });

                        }

                    }
                    else
                    {

                        pl.CreateAndAddLabelRow2("X Axis Data Sources");
                        var ml1 = pl.CreateAndAddMultilineTextBoxRow(String.Join(Environment.NewLine, Chart.SpreadsheetDataSourcesX), false, true, (txt, e) =>
                            {
                            });
                        pl.CreateAndAddButtonRow("Update Data", null, (btn, e) =>
                            {
                                try
                                {
                                    Chart.SpreadsheetDataSourcesX.Clear();
                                    foreach (var line in ml1.Text.Split('\n'))
                                    {
                                        Chart.SpreadsheetDataSourcesX.Add(line.Trim());
                                    }
                                    Application.Instance.Invoke(() =>
                                    {
                                        UpdatePlotModelData();
                                    });
                                }
                                catch (Exception ex)
                                {
                                    MessageBox.Show("Error updating chart data: " + ex.Message, "DWSIM", MessageBoxType.Error);
                                }
                            });


                        pl.CreateAndAddLabelRow2("Y Axis Data Sources");
                        var ml2 = pl.CreateAndAddMultilineTextBoxRow(String.Join(Environment.NewLine, Chart.SpreadsheetDataSourcesY), false, true, (txt, e) =>
                            {

                            });
                        pl.CreateAndAddButtonRow("Update Data", null, (btn, e) =>
                            {
                                try
                                {
                                    Chart.SpreadsheetDataSourcesY.Clear();
                                    foreach (var line in ml2.Text.Split('\n'))
                                    {
                                        Chart.SpreadsheetDataSourcesY.Add(line.Trim());
                                    }
                                    Application.Instance.Invoke(() =>
                                    {
                                        UpdatePlotModelData();
                                    });
                                }
                                catch (Exception ex)
                                {
                                    MessageBox.Show("Error updating chart data: " + ex.Message, "DWSIM", MessageBoxType.Error);
                                }
                            });

                    }

                    if (pm != null)
                    {

                        pl.CreateAndAddLabelRow("Plot");

                        pl.CreateAndAddStringEditorRow("Title", pm.Title, (txt, e) =>
                            {
                                Application.Instance.Invoke(() =>
                                {
                                    pm.Title = txt.Text;
                                    ChartView.Model.InvalidatePlot(true);
                                    ChartView.Invalidate();
                                });
                            });

                        pl.CreateAndAddDropDownRow("Title Position", pm.TitleHorizontalAlignment.GetEnumNames(),
                                (int)pm.TitleHorizontalAlignment, (dd, e) =>
                                {
                                    pm.TitleHorizontalAlignment = dd.SelectedIndex.ToEnum<TitleHorizontalAlignment>();
                                    Application.Instance.Invoke(() =>
                                    {
                                        ChartView.Model.InvalidatePlot(true);
                                        ChartView.Invalidate();
                                    });
                                });

                        pl.CreateAndAddNumericEditorRow2("Title Font Size", pm.TitleFontSize, 4.0, 30, 1, (ns, e) =>
                            {
                                pm.TitleFontSize = ns.Text.ToDoubleFromCurrent();
                                Application.Instance.Invoke(() =>
                                {
                                    ChartView.Model.InvalidatePlot(true);
                                    ChartView.Invalidate();
                                });
                            });

                        pl.CreateAndAddStringEditorRow("Subtitle", pm.Subtitle, (txt, e) =>
                            {
                                pm.Subtitle = txt.Text;
                                Application.Instance.Invoke(() =>
                                {
                                    ChartView.Model.InvalidatePlot(true);
                                    ChartView.Invalidate();
                                });
                            });

                        pl.CreateAndAddNumericEditorRow2("Subtitle Font Size", pm.SubtitleFontSize, 4.0, 30, 1, (ns, e) =>
                            {
                                pm.SubtitleFontSize = ns.Text.ToDoubleFromCurrent();
                                Application.Instance.Invoke(() =>
                                {
                                    ChartView.Model.InvalidatePlot(true);
                                    ChartView.Invalidate();
                                });
                            });

                        pl.CreateAndAddCheckBoxRow("Display Legend", pm.IsLegendVisible,
                                (chk, e) =>
                                {
                                    pm.IsLegendVisible = chk.Checked.GetValueOrDefault();
                                    Application.Instance.Invoke(() =>
                                    {
                                        ChartView.Model.InvalidatePlot(true);
                                        ChartView.Invalidate();
                                    });
                                });

                        if (pm.IsLegendVisible)
                        {

                            pl.CreateAndAddDropDownRow("Legend Position", pm.LegendPosition.GetEnumNames(),
                                    (int)pm.LegendPosition, (dd, e) =>
                                    {
                                        pm.LegendPosition = dd.SelectedIndex.ToEnum<LegendPosition>();
                                        Application.Instance.Invoke(() =>
                                        {
                                            ChartView.Model.InvalidatePlot(true);
                                            ChartView.Invalidate();
                                        });
                                    });

                            pl.CreateAndAddDropDownRow("Legend Placement", pm.LegendPlacement.GetEnumNames(),
                                    (int)pm.LegendPlacement, (dd, e) =>
                                    {
                                        pm.LegendPlacement = dd.SelectedIndex.ToEnum<LegendPlacement>();
                                        Application.Instance.Invoke(() =>
                                        {
                                            ChartView.Model.InvalidatePlot(true);
                                            ChartView.Invalidate();
                                        });
                                    });

                            pl.CreateAndAddDropDownRow("Legend Orientation", pm.LegendOrientation.GetEnumNames(),
                                    (int)pm.LegendOrientation, (dd, e) =>
                                    {
                                        pm.LegendOrientation = dd.SelectedIndex.ToEnum<LegendOrientation>();
                                        Application.Instance.Invoke(() =>
                                        {
                                            ChartView.Model.InvalidatePlot(true);
                                            ChartView.Invalidate();
                                        });
                                    });

                            pl.CreateAndAddStringEditorRow("Legend Title", pm.LegendTitle, (txt, e) =>
                                {
                                    pm.LegendTitle = txt.Text;
                                    Application.Instance.Invoke(() =>
                                    {
                                        ChartView.Model.InvalidatePlot(true);
                                        ChartView.Invalidate();
                                    });
                                });

                            pl.CreateAndAddNumericEditorRow2("Legend Title Font Size", pm.LegendTitleFontSize, 4.0, 30, 1, (ns, e) =>
                                {
                                    pm.LegendTitleFontSize = ns.Text.ToDoubleFromCurrent();
                                    Application.Instance.Invoke(() =>
                                    {
                                        ChartView.Model.InvalidatePlot(true);
                                        ChartView.Invalidate();
                                    });
                                });

                            pl.CreateAndAddDropDownRow("Legend Item Alignment", pm.LegendItemAlignment.GetEnumNames(),
                                    (int)pm.LegendItemAlignment, (dd, e) =>
                                    {
                                        pm.LegendItemAlignment = dd.SelectedIndex.ToEnum<OxyPlot.HorizontalAlignment>();
                                        Application.Instance.Invoke(() =>
                                        {
                                            ChartView.Model.InvalidatePlot(true);
                                            ChartView.Invalidate();
                                        });
                                    });

                            pl.CreateAndAddDropDownRow("Legend Item Order", pm.LegendItemOrder.GetEnumNames(),
                                    (int)pm.LegendItemOrder, (dd, e) =>
                                    {
                                        pm.LegendItemOrder = dd.SelectedIndex.ToEnum<OxyPlot.LegendItemOrder>();
                                        Application.Instance.Invoke(() =>
                                        {
                                            ChartView.Model.InvalidatePlot(true);
                                            ChartView.Invalidate();
                                        });
                                    });

                            pl.CreateAndAddNumericEditorRow2("Legend Item Spacing", pm.LegendItemSpacing, 0.1, 5, 1, (ns, e) =>
                                {
                                    pm.LegendItemSpacing = ns.Text.ToDoubleFromCurrent();
                                    Application.Instance.Invoke(() =>
                                    {
                                        ChartView.Model.InvalidatePlot(true);
                                        ChartView.Invalidate();
                                    });
                                });

                            pl.CreateAndAddNumericEditorRow2("Legend Line Spacing", pm.LegendLineSpacing, 0.1, 5, 1, (ns, e) =>
                                {
                                    pm.LegendLineSpacing = ns.Text.ToDoubleFromCurrent();
                                    Application.Instance.Invoke(() =>
                                    {
                                        ChartView.Model.InvalidatePlot(true);
                                        ChartView.Invalidate();
                                    });
                                });

                        }

                        if (pm.Axes.Count >= 1)
                        {

                            pl.CreateAndAddLabelRow("X Axis");

                            pl.CreateAndAddStringEditorRow("Title", pm.Axes[0].Title, (txt, e) =>
                                {
                                    pm.Axes[0].Title = txt.Text;
                                    Application.Instance.Invoke(() =>
                                    {
                                        ChartView.Model.InvalidatePlot(true);
                                        ChartView.Invalidate();
                                    });
                                });

                            pl.CreateAndAddNumericEditorRow2("Font Size", pm.Axes[0].FontSize, 4.0, 30, 1, (ns, e) =>
                                {
                                    pm.Axes[0].FontSize = ns.Text.ToDoubleFromCurrent();
                                    Application.Instance.Invoke(() =>
                                    {
                                        ChartView.Model.InvalidatePlot(true);
                                        ChartView.Invalidate();
                                    });
                                });

                            pl.CreateAndAddNumericEditorRow2("Title Font Size", pm.Axes[0].TitleFontSize, 4.0, 30, 1, (ns, e) =>
                                {
                                    pm.Axes[0].TitleFontSize = ns.Text.ToDoubleFromCurrent();
                                    Application.Instance.Invoke(() =>
                                    {
                                        ChartView.Model.InvalidatePlot(true);
                                        ChartView.Invalidate();
                                    });
                                });

                            pl.CreateAndAddNumericEditorRow2("Title Position", pm.Axes[0].TitlePosition, 0.0, 1.0, 2, (ns, e) =>
                                {
                                    pm.Axes[0].TitlePosition = ns.Text.ToDoubleFromCurrent();
                                    Application.Instance.Invoke(() =>
                                    {
                                        ChartView.Model.InvalidatePlot(true);
                                        ChartView.Invalidate();
                                    });
                                });

                        }

                        if (pm.Axes.Count >= 2)
                        {
                            pl.CreateAndAddLabelRow("Y Axis");

                            pl.CreateAndAddStringEditorRow("Title", pm.Axes[1].Title, (txt, e) =>
                                {
                                    pm.Axes[1].Title = txt.Text;
                                    Application.Instance.Invoke(() =>
                                    {
                                        ChartView.Model.InvalidatePlot(true);
                                        ChartView.Invalidate();
                                    });
                                });

                            pl.CreateAndAddNumericEditorRow2("Font Size", pm.Axes[1].FontSize, 4.0, 30, 1, (ns, e) =>
                                {
                                    pm.Axes[1].FontSize = ns.Text.ToDoubleFromCurrent();
                                    Application.Instance.Invoke(() =>
                                    {
                                        ChartView.Model.InvalidatePlot(true);
                                        ChartView.Invalidate();
                                    });
                                });

                            pl.CreateAndAddNumericEditorRow2("Title Font Size", pm.Axes[1].TitleFontSize, 4.0, 30, 1, (ns, e) =>
                                {
                                    pm.Axes[1].TitleFontSize = ns.Text.ToDoubleFromCurrent();
                                    Application.Instance.Invoke(() =>
                                    {
                                        ChartView.Model.InvalidatePlot(true);
                                        ChartView.Invalidate();
                                    });
                                });

                            pl.CreateAndAddNumericEditorRow2("Title Position", pm.Axes[1].TitlePosition, 0.0, 1.0, 2, (ns, e) =>
                                {
                                    pm.Axes[1].TitlePosition = ns.Text.ToDoubleFromCurrent();
                                    Application.Instance.Invoke(() =>
                                    {
                                        ChartView.Model.InvalidatePlot(true);
                                        ChartView.Invalidate();
                                    });
                                });
                        }
                    }

                    var i = 0;

                    foreach (LineSeries series in pm.Series)
                    {

                        pl.CreateAndAddLabelRow(String.Format("Line Series #{0}", i));

                        pl.CreateAndAddStringEditorRow("Title", series.Title, (txt, e) =>
                            {
                                series.Title = txt.Text;
                                Application.Instance.Invoke(() =>
                                {
                                    ChartView.Model.InvalidatePlot(true);
                                    ChartView.Invalidate();
                                });
                            });

                        pl.CreateAndAddDropDownRow("Line Type", series.LineStyle.GetEnumNames(),
                                 (int)series.LineStyle, (dd, e) =>
                                 {
                                     series.LineStyle = dd.SelectedIndex.ToEnum<LineStyle>();
                                     Application.Instance.Invoke(() =>
                                     {
                                         ChartView.Model.InvalidatePlot(true);
                                         ChartView.Invalidate();
                                     });
                                 });

                        var cchoices = new List<string>(ColorChoices);
                        cchoices.Insert(0, series.Color.ToString());

                        pl.CreateAndAddDropDownRow("Line Color", cchoices, 0, (dd, e) =>
                                {
                                    if (dd.SelectedIndex > 0) series.Color = (OxyColor)typeof(OxyColors).GetField(dd.SelectedValue.ToString()).GetValue(null);
                                    Application.Instance.Invoke(() =>
                                    {
                                        ChartView.Model.InvalidatePlot(true);
                                        ChartView.Invalidate();
                                    });
                                });

                        pl.CreateAndAddNumericEditorRow2("Line Width", series.StrokeThickness, 0.1, 10.0, 1, (ns, e) =>
                            {
                                series.StrokeThickness = ns.Text.ToDoubleFromCurrent();
                                Application.Instance.Invoke(() =>
                                {
                                    ChartView.Model.InvalidatePlot(true);
                                    ChartView.Invalidate();
                                });
                            });

                        pl.CreateAndAddDropDownRow("Marker Type", series.MarkerType.GetEnumNames(),
                                 (int)series.MarkerType, (dd, e) =>
                                 {
                                     series.MarkerType = dd.SelectedIndex.ToEnum<MarkerType>();
                                     Application.Instance.Invoke(() =>
                                     {
                                         ChartView.Model.InvalidatePlot(true);
                                         ChartView.Invalidate();
                                     });
                                 });

                        cchoices = new List<string>(ColorChoices);
                        cchoices.Insert(0, series.Color.ToString());

                        pl.CreateAndAddNumericEditorRow2("Marker Size", series.MarkerSize, 0.1, 10.0, 1, (ns, e) =>
                            {
                                series.MarkerSize = ns.Text.ToDoubleFromCurrent();
                                Application.Instance.Invoke(() =>
                                {
                                    ChartView.Model.InvalidatePlot(true);
                                    ChartView.Invalidate();
                                });
                            });

                        pl.CreateAndAddDropDownRow("Marker Fill Color", cchoices, 0, (dd, e) =>
                            {
                                if (dd.SelectedIndex > 0) series.MarkerFill = (OxyColor)typeof(OxyColors).GetField(dd.SelectedValue.ToString()).GetValue(null);
                                Application.Instance.Invoke(() =>
                                {
                                    ChartView.Model.InvalidatePlot(true);
                                    ChartView.Invalidate();
                                });
                            });

                        pl.CreateAndAddNumericEditorRow2("Marker Stroke Size", series.MarkerStrokeThickness, 0.1, 10.0, 1, (ns, e) =>
                            {
                                series.MarkerStrokeThickness = ns.Text.ToDoubleFromCurrent();
                                Application.Instance.Invoke(() =>
                                {
                                    ChartView.Model.InvalidatePlot(true);
                                    ChartView.Invalidate();
                                });
                            });

                        pl.CreateAndAddDropDownRow("Marker Stroke Color", cchoices, 0, (dd, e) =>
                            {
                                if (dd.SelectedIndex > 0) series.MarkerStroke = (OxyColor)typeof(OxyColors).GetField(dd.SelectedValue.ToString()).GetValue(null);
                                Application.Instance.Invoke(() =>
                                {
                                    ChartView.Model.InvalidatePlot(true);
                                    ChartView.Invalidate();
                                });
                            });
                    }

                    Splitter.Panel2 = new Scrollable { Content = pl };
                    Splitter.Panel2.Width = 350;

                }
                catch (Exception ex)
                {
                    Flowsheet.ShowMessage("Error updating chart property list: " + ex.Message, Interfaces.IFlowsheet.MessageType.GeneralError);
                }

            });

        }

        public void UpdatePlotModelData()
        {
            try
            {
                if (Chart.PlotModel != null)
                {
                    if ((Chart.ChartSource == ChartSource.FlowsheetObject))
                    {
                        if (Chart.ChartSourceObjectAutoUpdate)
                        {
                            Chart.PlotModel = Flowsheet.SimulationObjects[Chart.ChartSourceObjectID].GetChartModel(Chart.ChartSourceObjectChartID);
                        }

                    }
                    else
                    {
                        PlotModel pm = (PlotModel)Chart.PlotModel;
                        if (((pm.Series.Count != Chart.SpreadsheetDataSourcesX.Count)
                                    && (pm.Series.Count != Chart.SpreadsheetDataSourcesY.Count)))
                        {
                            pm.Series.Clear();
                            List<List<double>> xnumbers = new List<List<double>>();
                            List<List<double>> ynumbers = new List<List<double>>();
                            foreach (var item in Chart.SpreadsheetDataSourcesX)
                            {
                                List<double> xlist = new List<double>();
                                var sheet = Spreadsheet.GetWorksheetByName(item.Split('!')[0]);
                                object[,] data = sheet.GetRangeData(new RangePosition(item.Split('!')[1]));
                                if ((data.GetLength(0) > 1))
                                {
                                    int j = 0;
                                    for (j = 0; (j
                                                <= (data.GetLength(0) - 1)); j++)
                                    {
                                        double d = 0;
                                        double.TryParse(data[j, 0].ToString(), out d);
                                        xlist.Add(d);
                                    }

                                }
                                else if ((data.GetLength(1) > 1))
                                {
                                    int j = 0;
                                    for (j = 0; (j
                                                <= (data.GetLength(1) - 1)); j++)
                                    {
                                        double d = 0;
                                        double.TryParse(data[0, j].ToString(), out d);
                                        xlist.Add(d);
                                    }

                                }

                                xnumbers.Add(xlist);
                            }

                            foreach (var item in Chart.SpreadsheetDataSourcesY)
                            {
                                List<double> ylist = new List<double>();
                                var sheet = Spreadsheet.GetWorksheetByName(item.Split('!')[0]);
                                object[,] data = sheet.GetRangeData(new RangePosition(item.Split('!')[1]));
                                if ((data.GetLength(0) > 1))
                                {
                                    int j = 0;
                                    for (j = 0; (j
                                                <= (data.GetLength(0) - 1)); j++)
                                    {
                                        double d = 0;
                                        double.TryParse(data[j, 0].ToString(), out d);
                                        ylist.Add(d);
                                    }

                                }
                                else if ((data.GetLength(1) > 1))
                                {
                                    int j = 0;
                                    for (j = 0; (j
                                                <= (data.GetLength(1) - 1)); j++)
                                    {
                                        double d = 0;
                                        double.TryParse(data[0, j].ToString(), out d);
                                        ylist.Add(d);
                                    }

                                }

                                ynumbers.Add(ylist);
                            }

                            for (int i = 0; (i
                                        <= (xnumbers.Count - 1)); i++)
                            {
                                pm.AddLineSeries(xnumbers[i], ynumbers[i], ("Series" + (i + 1).ToString()));
                            }

                        }
                        else
                        {
                            List<List<double>> xnumbers = new List<List<double>>();
                            List<List<double>> ynumbers = new List<List<double>>();
                            foreach (var item in Chart.SpreadsheetDataSourcesX)
                            {
                                List<double> xlist = new List<double>();
                                var sheet = Spreadsheet.GetWorksheetByName(item.Split('!')[0]);
                                object[,] data = sheet.GetRangeData(new RangePosition(item.Split('!')[1]));
                                if ((data.GetLength(0) > 1))
                                {
                                    int j = 0;
                                    for (j = 0; (j
                                                <= (data.GetLength(0) - 1)); j++)
                                    {
                                        double d = 0;
                                        double.TryParse(data[j, 0].ToString(), out d);
                                        xlist.Add(d);
                                    }

                                }
                                else if ((data.GetLength(1) > 1))
                                {
                                    int j = 0;
                                    for (j = 0; (j
                                                <= (data.GetLength(1) - 1)); j++)
                                    {
                                        double d = 0;
                                        double.TryParse(data[0, j].ToString(), out d);
                                        xlist.Add(d);
                                    }

                                }

                                xnumbers.Add(xlist);
                            }

                            foreach (var item in Chart.SpreadsheetDataSourcesY)
                            {
                                List<double> ylist = new List<double>();
                                var sheet = Spreadsheet.GetWorksheetByName(item.Split('!')[0]);
                                object[,] data = sheet.GetRangeData(new RangePosition(item.Split('!')[1]));
                                if ((data.GetLength(0) > 1))
                                {
                                    int j = 0;
                                    for (j = 0; (j
                                                <= (data.GetLength(0) - 1)); j++)
                                    {
                                        double d = 0;
                                        double.TryParse(data[j, 0].ToString(), out d);
                                        ylist.Add(d);
                                    }

                                }
                                else if ((data.GetLength(1) > 1))
                                {
                                    int j = 0;
                                    for (j = 0; (j
                                                <= (data.GetLength(1) - 1)); j++)
                                    {
                                        double d = 0;
                                        double.TryParse(data[0, j].ToString(), out d);
                                        ylist.Add(d);
                                    }

                                }

                                ynumbers.Add(ylist);
                            }

                            for (int i = 0; (i
                                        <= (xnumbers.Count - 1)); i++)
                            {
                                LineSeries lineSeries = (LineSeries)pm.Series[i];
                                lineSeries.Points.Clear();
                                for (int j = 0; (j
                                            <= (xnumbers[i].Count - 1)); j++)
                                {
                                    lineSeries.Points.Add(new DataPoint(xnumbers[i][j], ynumbers[i][j]));
                                }

                            }

                        }

                    }

                    PlotModel pm2 = (PlotModel)Chart.PlotModel;

                    pm2.InvalidatePlot(true);
                    ChartView.Model = pm2;
                    ChartView.Invalidate();

                }
            }
            catch (Exception ex)
            {
                Flowsheet.ShowMessage("Error updating chart data: " + ex.Message, Interfaces.IFlowsheet.MessageType.GeneralError);
            }
        }

    }

}
