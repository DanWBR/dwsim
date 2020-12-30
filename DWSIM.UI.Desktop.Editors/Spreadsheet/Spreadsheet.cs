using System;
using System.Collections.Generic;
using System.Linq;

using DWSIM.Interfaces;

using Eto.Forms;

using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;
using Eto.Drawing;

using DWSIM.ExtensionMethods;
using DWSIM.UI.Shared;
using s = DWSIM.UI.Shared.Common;
using System.Collections.ObjectModel;

using global::DWSIM.SharedClasses.Spreadsheet;
using System.Globalization;
using System.Xml.Linq;
using DWSIM.CrossPlatform.UI.Controls.ReoGrid;
using Cell = DWSIM.CrossPlatform.UI.Controls.ReoGrid.Cell;
using DWSIM.CrossPlatform.UI.Controls.ReoGrid.EtoRenderer;
using DWSIM.CrossPlatform.UI.Controls.ReoGrid.Formula;
using DWSIM.UI.Desktop.Shared;
using DWSIM.SharedClasses.Charts;

namespace DWSIM.UI.Desktop.Editors
{
    public class Spreadsheet
    {

        public Dictionary<String, Interfaces.ISimulationObject> ObjList;

        protected SpreadsheetCellParameters ccparams;

        public List<List<object>> dt1 = new List<List<object>>();
        public List<List<object>> dt2 = new List<List<object>>();

        private Flowsheet flowsheet;

        private ReoGridFullControl scontrol;

        public ReoGridControl Sheet;

        public bool Loaded = true;

        private List<string> Columns = new List<string>();

        public Spreadsheet(Flowsheet fs)
        {

            Columns.AddRange(new[] { "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z" });

            flowsheet = fs;
            scontrol = new ReoGridFullControl(GlobalSettings.Settings.AutomationMode);
            Sheet = scontrol.GridControl;
            SetCustomFunctions();
            flowsheet.GetSpreadsheetObjectFunc = () => { return Sheet; };
            Sheet.CurrentWorksheet.Name = "MAIN";

            if (scontrol.ImportDataMenuItem != null) scontrol.ImportDataMenuItem.Click += (sender, e) =>
            {

                Application.Instance.Invoke(() =>
                {

                    var selector = new PropertySelector() { Flowsheet = flowsheet, ObjList = ObjList };

                    selector.btnOK.Click += (sender2, e2) =>
                    {
                        var units = "";
                        if (selector.list4.SelectedValue != null) units = selector.list4.SelectedValue.ToString();
                        Sheet.CurrentWorksheet.Cells[Sheet.CurrentWorksheet.SelectionRange.StartPos].Formula =
                        String.Format("GETPROPVAL({3}{1}{3}{0}{3}{2}{3}{0}{3}{4}{3})",
                                                  ';',
                                                  selector.list2.SelectedKey,
                                                  selector.list3.SelectedKey,
                                                  '"', units);

                        selector.Close();

                        Sheet.CurrentWorksheet.EndEdit(EndEditReason.Cancel);

                    };

                    selector.ShowModal();

                });

            };

            if (scontrol.ExportDataMenuItem != null) scontrol.ExportDataMenuItem.Click += (sender, e) =>
            {

                Application.Instance.Invoke(() =>
                {

                    var selector = new PropertySelector() { Flowsheet = flowsheet, ObjList = ObjList };

                    selector.btnOK.Click += (sender2, e2) =>
                    {
                        var units = "";
                        if (selector.list4.SelectedValue != null) units = selector.list4.SelectedValue.ToString();
                        var scell = Sheet.CurrentWorksheet.Cells[Sheet.CurrentWorksheet.SelectionRange.StartPos];
                        var currdata = scell.Formula == null ? scell.Data : scell.Formula;
                        scell.Formula = String.Format("SETPROPVAL({3}{1}{3}{0}{3}{2}{3}{0}{3}{4}{3}{0}{3}{5}{3})",
                                                   ';',
                                                   selector.list2.SelectedKey,
                                                   selector.list3.SelectedKey,
                                                   '"',
                                                   currdata, units);

                        selector.Close();

                        Sheet.CurrentWorksheet.EndEdit(EndEditReason.Cancel);

                    };

                    selector.ShowModal();

                });

            };

            if (scontrol.CreateChartMenuItem != null) scontrol.CreateChartMenuItem.Click += (sender, e) =>
            {
                Application.Instance.Invoke(() =>
                {
                    CreateChartFromRange(sender, e);
                });
            };


        }

        private void CreateChartFromRange(object sender, EventArgs e)
        {
            Application.Instance.Invoke(() =>
            {
                DocumentPage tabpage = new DocumentPage();
                var chart = new Chart();
                object data = Sheet.CurrentWorksheet.GetRangeData(Sheet.CurrentWorksheet.SelectionRange);
                int firstcol;
                int lastcol;
                int firstrow;
                int lastrow;
                firstcol = Sheet.CurrentWorksheet.SelectionRange.Col;
                lastcol = Sheet.CurrentWorksheet.SelectionRange.EndCol;
                firstrow = (Sheet.CurrentWorksheet.SelectionRange.Row + 1);
                lastrow = (Sheet.CurrentWorksheet.SelectionRange.EndRow + 1);
                double d;
                bool hasheaders = !double.TryParse(Sheet.CurrentWorksheet.Cells[(firstrow - 1), firstcol].Data.ToString(), out d);
                object name = Sheet.CurrentWorksheet.Name;
                string xcol = Sheet.CurrentWorksheet.SelectionRange.StartPos.ToAddress().Trim(new char[] {
                    '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'});
                List<string> ycols = new List<string>();
                for (int i = (firstcol + 1); (i <= lastcol); i++)
                {
                    ycols.Add(Sheet.CurrentWorksheet.Cells[0, i].Address.Trim(new char[] {
                    '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'}));
                }

                if (hasheaders)
                {
                    firstrow++;
                }

                foreach (var item in ycols)
                {
                    chart.SpreadsheetDataSourcesX.Add((name + ("!"
                                    + (xcol
                                    + (firstrow + (":"
                                    + (xcol + lastrow)))))));
                    chart.SpreadsheetDataSourcesY.Add((name + ("!"
                                    + (item
                                    + (firstrow + (":"
                                    + (item + lastrow)))))));
                }

                var chartcontrol = new Charts.ChartControl();

                chartcontrol.Flowsheet = this.flowsheet;
                chartcontrol.Chart = chart;
                chartcontrol.Spreadsheet = this.Sheet;

                tabpage.Content = chartcontrol;

                if (hasheaders)
                {
                    chartcontrol.UpdatePlotModelData();
                    var j = 0;
                    for (int i = (firstcol + 1); (i <= lastcol); i++)
                    {
                        ((OxyPlot.PlotModel)chart.PlotModel).Series[j].Title =
                                            Sheet.CurrentWorksheet.Cells[firstrow - 2, i].Data.ToString();
                        j += 1;
                    }
                    ((OxyPlot.PlotModel)chart.PlotModel).Axes[0].Title =
                        Sheet.CurrentWorksheet.Cells[firstrow - 2, firstcol].Data.ToString();
                    ((OxyPlot.PlotModel)chart.PlotModel).Axes[1].Title = "";
                    if (((OxyPlot.PlotModel)chart.PlotModel).Series.Count == 1)
                    {
                        ((OxyPlot.PlotModel)chart.PlotModel).Axes[1].Title =
                            Sheet.CurrentWorksheet.Cells[firstrow - 2, firstcol + 1].Data.ToString();
                    }
                }

                tabpage.Shown += (s1, e1) =>
                {
                    chartcontrol.UpdatePlotModelData();
                    chartcontrol.UpdatePropertiesLayout();
                };

                tabpage.Text = chart.DisplayName;
                flowsheet.Charts.Add(chart.ID, chart);
                flowsheet.AddChart.Invoke(tabpage);
            });
        }

        public PixelLayout GetSpreadsheet(IFlowsheet obj)
        {

            return scontrol;

        }

        public void WriteAll()
        {

            foreach (var ws in Sheet.Worksheets)
            {
                ws.Recalculate();
            }

        }

        public void EvaluateAll()
        {
            WriteAll();
        }

        public List<string[]> GetDataFromRange(string range)
        {

            var list = new List<string[]>();
            var slist = new List<string>();

            var rdata = Sheet.Worksheets[0].GetRangeData(new RangePosition(range));

            for (var i = 0; i < rdata.GetLength(0); i++)
            {
                slist = new List<string>();
                for (var j = 0; j < rdata.GetLength(1); j++)
                {
                    slist.Add(rdata[i, j] != null ? rdata[i, j].ToString() : "");
                }
                list.Add(slist.ToArray());
            }

            return list;
        }

        public void CopyDT1FromString(string text)
        {
            string[] rows = text.Split('|');
            int n = (rows.Length - 1);
            int m = 0;
            string value;
            var ci = System.Globalization.CultureInfo.InvariantCulture;
            NumberStyles format = (NumberStyles)(NumberStyles.Any - NumberStyles.AllowThousands);
            if ((n > 0))
            {
                m = (rows[0].Split(';').Length - 1);
            }
            if (((n > 0) && (m > 0)))
            {
                List<List<object>> elm = new List<List<object>>();
                try
                {
                    for (int i = 0; (i <= n); i++)
                    {
                        elm.Add(new List<object>());
                        if ((n > 0))
                        {
                            m = (rows[i].Split(';').Length - 1);
                        }

                        for (int j = 0; (j <= m); j++)
                        {
                            value = rows[i].Split(';')[j];
                            double d;
                            if (double.TryParse(value, format, ci, out d))
                            {
                                elm[i].Add(double.Parse(value, format, ci));
                            }
                            else
                            {
                                elm[i].Add(value);
                            }

                        }

                    }

                }
                catch
                {
                }

                dt1 = elm;
            }


        }

        public void CopyDT2FromString(string text)
        {
            string[] rows = text.Split('|');
            int n = 98;
            int m = 25;
            if (n > 0)
            {
                m = rows[0].Split(';').Length - 1;
            }
            if (n > 0 & m > 0)
            {
                List<List<object>> elm = new List<List<object>>();
                for (int i = 0; i <= n; i++)
                {
                    elm.Add(new List<object>());
                    for (int j = 0; j <= m; j++)
                    {
                        SpreadsheetCellParameters scp = new SpreadsheetCellParameters();
                        try
                        {
                            XElement element = new XElement("dummy");
                            string text0 = rows[i].Replace("&gt;", "greater_than").Replace("&lt;", "less_than");
                            string xmltext = text0.Split(';')[j];
                            if (xmltext != " ")
                            {
                                string text1 = xmltext.Replace("greater_than", "&gt;").Replace("less_than", "&lt;");
                                element = XElement.Parse(text1);
                                scp.LoadData(element.Elements().ToList());
                                elm[i].Add(scp);
                            }
                            else
                            {
                                elm[i].Add(scp);
                            }
                        }
                        catch (Exception)
                        {
                        }
                    }
                }
                dt2 = elm;
            }

        }

        public void CopyFromDT()
        {
            int i, j, n1, m1, n2, m2, maxrow, maxcol;

            n1 = dt1.Count - 1;

            if (n1 == -1) return;

            m1 = dt1[0].Count - 1;

            n2 = dt2.Count - 1;
            m2 = dt2[0].Count - 1;

            maxrow = Sheet.Worksheets[0].RowCount - 1;
            maxcol = Sheet.Worksheets[0].ColumnCount - 1;

            var sheet = Sheet.Worksheets[0];

            for (i = 0; i <= n1; i++)
            {
                for (j = 0; j <= m1; j++)
                {
                    if (i <= maxrow & j <= dt1[i].Count - 1)
                    {
                        if (dt1[i][j] != null) sheet.Cells[i, j].Data = dt1[i][j];
                    }
                    if (i <= n2 & j <= dt2[i].Count)
                    {
                        if (dt2[i][j] == null)
                            sheet.Cells[i, j].Tag = new SpreadsheetCellParameters();
                        else if (dt2[i][j] is SpreadsheetCellParameters)
                            sheet.Cells[i, j].Tag = dt2[i][j];
                        else if (dt2[i][j] is string)
                        {
                            SpreadsheetCellParameters cellparam = new SpreadsheetCellParameters();
                            try
                            {
                                cellparam.Expression = dt2[i][j].ToString();
                                if (cellparam.Expression.StartsWith(":"))
                                {
                                    cellparam.CellType = SharedClasses.Spreadsheet.VarType.Read;
                                    string[] str;
                                    str = cellparam.Expression.Split(new char[] { ',' });
                                    cellparam.ObjectID = str[0].Substring(1);
                                    cellparam.PropID = str[1];
                                }
                                else
                                    cellparam.CellType = SharedClasses.Spreadsheet.VarType.Expression;
                            }
                            catch
                            {
                            }
                            sheet.Cells[i, j].Tag = cellparam;
                        }
                    }
                }
            }

            ParseOldData();

        }


        void ParseOldData()
        {
            //var separator = System.Threading.Thread.CurrentThread.CurrentCulture.TextInfo.ListSeparator;
            var separator = ";";
            var esheet = Sheet.NewWorksheet("EXPORTS");
            List<Tuple<string, string, string, string>> elist = new List<Tuple<string, string, string, string>>();
            int i, j;
            Cell cell;
            for (i = 0; (i <= (Sheet.Worksheets[0].RowCount - 1)); i++)
            {
                for (j = 0; (j <= (Sheet.Worksheets[0].ColumnCount - 1)); j++)
                {
                    cell = Sheet.Worksheets[0].Cells[i, j];
                    ccparams = (SpreadsheetCellParameters)cell.Tag;
                    if (!(ccparams == null))
                    {
                        var expression = ccparams.Expression;
                        switch (ccparams.CellType)
                        {
                            case SharedClasses.Spreadsheet.VarType.Expression:
                            case SharedClasses.Spreadsheet.VarType.Read:
                                if ((expression != ""))
                                {
                                    if ((expression.Substring(0, 1) == "="))
                                    {
                                        cell.Formula = expression.TrimStart('=').ToUpper();
                                        cell.Style.BackColor = Colors.LightYellow.ToSolidColor();
                                    }
                                    else if ((expression.Substring(0, 1) == ":"))
                                    {
                                        string[] str;
                                        string obj;
                                        string prop;
                                        str = expression.Split(new char[] { ',' });
                                        obj = str[0].Substring(1);
                                        ccparams.ObjectID = obj;
                                        if ((str.Length < 3))
                                        {
                                            prop = str[1];
                                        }
                                        else
                                        {
                                            prop = (str[1] + ("," + str[2]));
                                        }
                                        ccparams.PropID = prop;
                                        cell.Formula = string.Format("GETPROPVAL({3}{0}{3}{1}{3}{2}{3})", obj, separator, prop, '\"');
                                        cell.Style.BackColor = Colors.LightGreen.ToSolidColor();
                                    }
                                    else
                                    {
                                        cell.Data = expression;
                                        if (((ccparams.CellType != SharedClasses.Spreadsheet.VarType.Write)
                                                    && (ccparams.CellType != SharedClasses.Spreadsheet.VarType.Unit)))
                                        {
                                            ccparams.ToolTipText = expression;
                                        }

                                    }
                                }
                                break;
                            case SharedClasses.Spreadsheet.VarType.Unit:
                                cell.Style.BackColor = Colors.Beige.ToSolidColor();
                                break;
                            case SharedClasses.Spreadsheet.VarType.Write:
                                if ((expression.Substring(0, 1) == "="))
                                {
                                    cell.Formula = expression.TrimStart('=').ToUpper();
                                    if (!(ccparams.CellType == SharedClasses.Spreadsheet.VarType.Write))
                                    {
                                        cell.Style.BackColor = Colors.LightYellow.ToSolidColor();
                                    }
                                    else
                                    {
                                        cell.Data = expression;
                                        if (((ccparams.CellType != SharedClasses.Spreadsheet.VarType.Write)
                                                    && (ccparams.CellType != SharedClasses.Spreadsheet.VarType.Unit)))
                                        {
                                            ccparams.ToolTipText = expression;
                                        }

                                    }

                                    elist.Add(new Tuple<string, string, string, string>(string.Format("SETPROPVAL({4}{1}{4}{0}{4}{2}{4}{0}{4}{3}{4})", separator, ccparams.ObjectID, ccparams.PropID, ("MAIN!" + cell.Address), '\"'), ccparams.ObjectID, ccparams.PropID, ccparams.PropUnit));
                                    cell.Style.BackColor = Colors.LightBlue.ToSolidColor();
                                }
                                break;
                        }
                    }
                }
            }

            esheet.Cells[0, 0].Data = "EXPRESSION";
            esheet.Cells[0, 1].Data = "OBJECT";
            esheet.Cells[0, 2].Data = "PROPERTY";
            esheet.Cells[0, 3].Data = "UNITS";

            esheet.Cells[0, 0].Style.Bold = true;
            esheet.Cells[0, 1].Style.Bold = true;
            esheet.Cells[0, 2].Style.Bold = true;
            esheet.Cells[0, 3].Style.Bold = true;

            i = 1;
            foreach (var item in elist)
            {
                esheet.Cells[i, 0].Formula = item.Item1;
                esheet.Cells[i, 1].Data = flowsheet.SimulationObjects[item.Item2].GraphicObject.Tag;
                esheet.Cells[i, 2].Data = item.Item3;
                esheet.Cells[i, 3].Data = item.Item4;
            }

        }

        void SetCustomFunctions()
        {

            FormulaExtension.CustomFunctions["GETNAME"] = (cell, args) =>
            { 
                    try
                    {
                        return flowsheet.SimulationObjects[args[0].ToString()].GraphicObject.Tag;
                    }
                    catch (Exception ex)
                    {
                        return "ERROR: " + ex.Message;
                    }
            };

            FormulaExtension.CustomFunctions["GETPROPVAL"] = (cell, args) =>
            {
                if (args.Length == 2)
                {
                    try
                    {
                        return flowsheet.SimulationObjects[args[0].ToString()].GetPropertyValue(args[1].ToString());
                    }
                    catch (Exception ex)
                    {
                        return "ERROR: " + ex.Message;
                    }
                }
                else if (args.Length == 3)
                {
                    try
                    {
                        var obj = flowsheet.SimulationObjects[args[0].ToString()];
                        var val = obj.GetPropertyValue(args[1].ToString());
                        return General.ConvertUnits(double.Parse(val.ToString()), obj.GetPropertyUnit(args[1].ToString()), args[2].ToString());
                    }
                    catch (Exception ex)
                    {
                        return "ERROR: " + ex.Message;
                    }
                }
                else
                    return "INVALID ARGS";
            };

            FormulaExtension.CustomFunctions["SETPROPVAL"] = (cell, args) =>
            {
                if (args.Length == 3)
                {
                    try
                    {
                        if (Loaded)
                        {
                            var ws = cell.Worksheet;
                            var wcell = ws.Cells[ws.RowCount - 1, ws.ColumnCount - 1];
                            wcell.Data = null;
                            wcell.Formula = args[2].ToString().Trim('"');
                            Evaluator.Evaluate(wcell);
                            var val = wcell.Data;
                            if (wcell.Data == null)
                            {
                                val = wcell.Formula;
                            }
                            flowsheet.SimulationObjects[args[0].ToString()].SetPropertyValue(args[1].ToString(), val);
                            wcell.Formula = null;
                            wcell.Data = null;
                            return string.Format("EXPORT OK [{0}, {1} = {2}]", flowsheet.SimulationObjects[args[0].ToString()].GraphicObject.Tag, args[1].ToString(), val);
                        }
                        else
                        {
                            return "NOT READY";
                        }
                    }
                    catch (Exception ex)
                    {
                        return "ERROR: " + ex.Message;
                    }
                }
                else if (args.Length == 4)
                {
                    try
                    {
                        if (Loaded)
                        {
                            var obj = flowsheet.SimulationObjects[args[0].ToString()];
                            var prop = args[1].ToString();
                            var ws = cell.Worksheet;
                            var wcell = ws.Cells[ws.RowCount - 1, ws.ColumnCount - 1];
                            wcell.Formula = args[2].ToString().Trim('"');
                            Evaluator.Evaluate(wcell);
                            var val = wcell.Data;
                            wcell.Formula = "";
                            wcell.Data = "";
                            var units = args[3].ToString();
                            var newval = General.ConvertUnits(double.Parse(val.ToString()), units, obj.GetPropertyUnit(prop));
                            obj.SetPropertyValue(prop, newval);
                            return string.Format("EXPORT OK [{0}, {1} = {2} {3}]", obj.GraphicObject.Tag, prop, val, units);
                        }
                        else
                        {
                            return "NOT READY";
                        }
                    }
                    catch (Exception ex)
                    {
                        return "ERROR: " + ex.Message;
                    }
                }
                else
                    return "INVALID ARGS";
            };

            FormulaExtension.CustomFunctions["GETPROPUNITS"] = (cell, args) =>
            {
                if (args.Length == 2)
                {
                    try
                    {
                        return flowsheet.SimulationObjects[args[0].ToString()].GetPropertyUnit(args[1].ToString());
                    }
                    catch (Exception ex)
                    {
                        return "ERROR: " + ex.Message;
                    }
                }
                else
                    return "INVALID ARGS";
            };

            FormulaExtension.CustomFunctions["GETOBJID"] = (cell, args) =>
            {
                if (args.Length == 1)
                {
                    try
                    {
                        return flowsheet.GetFlowsheetSimulationObject(args[0].ToString()).Name;
                    }
                    catch (Exception ex)
                    {
                        return "ERROR: " + ex.Message;
                    }
                }
                else
                    return "INVALID ARGS";
            };

            FormulaExtension.CustomFunctions["GETOBJNAME"] = (cell, args) =>
            {
                if (args.Length == 1)
                {
                    try
                    {
                        return flowsheet.SimulationObjects[args[0].ToString()].GraphicObject.Tag;
                    }
                    catch (Exception ex)
                    {
                        return "ERROR: " + ex.Message;
                    }
                }
                else
                    return "INVALID ARGS";
            };
        }
    }

}