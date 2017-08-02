using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.UnitOperations.UnitOperations;
using DWSIM.UnitOperations.Reactors;
using DWSIM.UnitOperations.SpecialOps;
using DWSIM.UnitOperations.Streams;
using DWSIM.Thermodynamics.Streams;

using Eto.Forms;

using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;
using Eto.Drawing;

using System.Diagnostics;
using System.IO;

using DWSIM.ExtensionMethods;
using DWSIM.UI.Shared;
using s = DWSIM.UI.Shared.Common;
using System.Collections.ObjectModel;

using global::DWSIM.SharedClasses.Spreadsheet;
using Ciloci.Flee;
using System.Globalization;
using System.Xml.Linq;

namespace DWSIM.UI.Desktop.Editors
{
    public class Spreadsheet
    {

        public Dictionary<String, Interfaces.ISimulationObject> ObjList;

        private IGenericExpression<Object> ExprObj;
        private ExpressionContext ExprContext = new ExpressionContext();

        public string OldValue = "";
        public object OldTag;
        public string NewValue = "";
        public object NewTag;

        protected SpreadsheetCellParameters ccparams;
        protected RowItem rowitem;
        string selectedcell = "";

        public object[,] dt1 = new object[100, 26];
        public object[,] dt2 = new object[100, 26];

        private IFlowsheet flowsheet;

        private GridView grid;

        private ObservableCollection<RowItem> rowlist;

        private string nf;

        public Spreadsheet(IFlowsheet fs)
        {
            flowsheet = fs;
            nf = flowsheet.FlowsheetOptions.NumberFormat;

            this.ExprContext = new Ciloci.Flee.ExpressionContext();
            this.ExprContext.Imports.AddType(typeof(System.Math));
            this.ExprContext.Imports.AddType(typeof(System.String));

            rowlist = new ObservableCollection<RowItem>();

            int i;
            for (i = 0; i < 100; i++)
            {
                rowlist.Add(new RowItem((i + 1).ToString()));
            }

            DefineVariables();

        }

        public TableLayout GetSpreadsheet(IFlowsheet obj)
        {

            grid = new GridView { DataStore = rowlist, RowHeight = 20 };
            if (Application.Instance.Platform.IsWinForms) grid.Height = 1100;

            if (GlobalSettings.Settings.RunningPlatform() != GlobalSettings.Settings.Platform.Windows)
            {
                grid.Columns.Add(new GridColumn { HeaderText = "", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.index) }, Editable = false, AutoSize = false, Width = 50, Resizable = false });
            }
            grid.Columns.Add(new GridColumn { HeaderText = "A", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.A) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "B", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.B) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "C", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.C) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "D", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.D) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "E", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.E) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "F", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.F) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "G", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.G) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "H", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.H) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "I", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.I) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "J", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.J) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "K", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.K) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "L", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.L) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "M", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.M) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "N", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.N) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "O", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.O) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "P", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.P) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "Q", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.Q) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "R", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.R) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "S", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.S) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "T", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.T) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "U", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.U) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "V", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.V) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "W", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.W) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "X", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.X) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "Y", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.Y) }, AutoSize = false, Editable = false, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "Z", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.Z) }, AutoSize = false, Editable = false, Width = 80 });

            grid.AllowColumnReordering = false;
            grid.AllowMultipleSelection = false;
            grid.GridLines = GridLines.Both;
            grid.Style = "spreadsheet";

            var table = new TableLayout();

            var cellcp = new Panel();

            var txtcell = new TextBox { Width = 80, ReadOnly = true };
            var txttype = new TextBox { Width = 200, ReadOnly = true };
            var txtformula = new TextBox() { PlaceholderText = "To enter a formula,  type '=' followed by the math expression and press ENTER to commit changes." };
            var btnImport = new Button { Text = "Import" };
            var btnExport = new Button { Text = "Export" };
            var btnClear = new Button { Text = "Clear" };
            var btnEvaluate = new Button { Text = "Evaluate" };
            var btnEvaluateAll = new Button { Text = "Evaluate All" };

            btnEvaluate.Click += (sender, e) =>
            {
                EvaluateAll(ccparams);
                grid.ReloadData(int.Parse(rowitem.index) - 1);
            };

            btnEvaluateAll.Click += (sender, e) =>
            {
                EvaluateAll();
            };


            btnClear.Click += (sender, e) =>
            {
                ccparams.Expression = "";
                ccparams.ObjectID = "";
                ccparams.PropID = "";
                ccparams.PropUnit = "";
                ccparams.CurrVal = "";
                ccparams.PrevVal = "";
                ccparams.CellType = VarType.None;
                UpdateValue(ccparams);
                grid.ReloadData(int.Parse(rowitem.index) - 1);
            };

            btnImport.Click += (sender, e) =>
            {

                var selector = new PropertySelector() { Flowsheet = flowsheet, ObjList = ObjList };

                selector.btnOK.Click += (sender2, e2) =>
                {

                    ccparams.Expression = ":" + selector.list2.SelectedKey + "," + selector.list3.SelectedKey;
                    ccparams.CellType = global::DWSIM.SharedClasses.Spreadsheet.VarType.Read;
                    UpdateValue(ccparams);
                    grid.ReloadData(int.Parse(rowitem.index) - 1);
                    selector.Close();

                };

                selector.ShowModal(grid);
            };

            btnExport.Click += (sender, e) =>
            {
                var selector = new PropertySelector() { Flowsheet = flowsheet, ObjList = ObjList, mode = 1 };

                selector.btnOK.Click += (sender2, e2) =>
                {

                    ccparams.ObjectID = selector.list2.SelectedKey;
                    ccparams.PropID = selector.list3.SelectedKey;
                    ccparams.CellType = global::DWSIM.SharedClasses.Spreadsheet.VarType.Write;
                    UpdateValue(ccparams);
                    grid.ReloadData(int.Parse(rowitem.index) - 1);
                    selector.Close();

                };

                selector.ShowModal(grid);
            };

            var tr = new TableRow(new Label { Text = "Selected Cell", VerticalAlignment = VerticalAlignment.Center }, txtcell,
                                    new Label { Text = "Type", VerticalAlignment = VerticalAlignment.Center }, txttype,
                                    new Label { Text = "Contents", VerticalAlignment = VerticalAlignment.Center }, txtformula,
                                    btnImport, btnExport, btnClear, btnEvaluate, btnEvaluateAll);

            tr.Cells[5].ScaleWidth = true;

            var tb = new TableLayout { Spacing = new Size(5, 5), Padding = new Padding(10) };
            tb.Rows.Add(tr);
            if (GlobalSettings.Settings.RunningPlatform() == GlobalSettings.Settings.Platform.Mac) tb.Height = 44;

            cellcp.Content = tb;

            table.Rows.Add(new TableRow(cellcp));

            var cnt = new DynamicLayout { Padding = new Padding(10, 0) };
            s.CreateAndAddDescriptionRow(cnt, "Select cells by clicking or touching on them (using arrow keys on the keyboard won't change the currently selected data cell). After selecting the cell, edit its contents on the 'Contents' input box, or configure it to import or export data from/to the flowsheet by clicking on the corresponding buttons.", true);

            table.Rows.Add(new TableRow(cnt));

            txtformula.TextChanged += (sender, e) =>
            {
                rowitem.CellParams[selectedcell].Expression = txtformula.Text;
            };

            txtformula.KeyDown += (sender, e) =>
            {
                if (e.Key == Keys.Enter)
                {
                    UpdateValue(rowitem.CellParams[selectedcell]);
                    grid.ReloadData(int.Parse(rowitem.index) - 1);
                }
            };

            grid.CellDoubleClick += (sender, e) =>
            {
                if (txtformula.Enabled) txtformula.Focus();
            };

            grid.CellClick += (sender, e) =>
            {
                if (e.Item == null) return;
                if (e.GridColumn.HeaderText == "") return;
                selectedcell = e.GridColumn.HeaderText + (e.Row + 1).ToString();
                txtcell.Text = selectedcell;
                rowitem = ((RowItem)e.Item);
                var cellp = ((RowItem)e.Item).CellParams[selectedcell];
                ccparams = cellp;
                switch (cellp.CellType)
                {
                    case VarType.Expression:
                        txttype.Text = "Expression";
                        txtformula.Enabled = true;
                        break;
                    case VarType.Read:
                        txttype.Text = "Imported from " + flowsheet.SimulationObjects[ccparams.ObjectID].GraphicObject.Tag + ", " + flowsheet.GetTranslatedString(ccparams.PropID);
                        txtformula.Enabled = false;
                        break;
                    case VarType.Write:
                        txttype.Text = "Exported to " + flowsheet.SimulationObjects[ccparams.ObjectID].GraphicObject.Tag + ", " + flowsheet.GetTranslatedString(ccparams.PropID);
                        txtformula.Enabled = false;
                        break;
                    case VarType.None:
                        txttype.Text = "Text";
                        txtformula.Enabled = true;
                        break;
                }
                txttype.ToolTip = txttype.Text;
                txtformula.Text = cellp.Expression;
                txtformula.ToolTip = txtformula.Text;
            };

            table.Rows.Add(new TableRow(new Scrollable { Content = grid, Border = BorderType.None }));

            return table;

        }

        public void WriteAll()
        {

            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;

            foreach (var row in rowlist)
            {
                foreach (var cellparam in row.CellParams.Values)
                {
                    if ((cellparam != null && cellparam.CellType == SharedClasses.Spreadsheet.VarType.Write))
                    {
                        var obj = flowsheet.SimulationObjects[ccparams.ObjectID];
                        obj.SetPropertyValue(ccparams.PropID, cellparam.CurrVal, su);
                    }
                }
            }

        }

        public void EvaluateAll(SpreadsheetCellParameters cell = null)
        {
            bool changed = false;
            if (cell == null)
            {
                try
                {
                    if ((flowsheet != null))
                    {
                        int i = 0;
                        foreach (var row in rowlist)
                        {
                            changed = false;
                            foreach (var cellparam in row.CellParams.Values)
                            {
                                ccparams = cellparam;
                                if ((ccparams != null && ccparams.CurrVal != "" && ccparams.Expression != ""))
                                {
                                    if (!string.IsNullOrEmpty(ccparams.Expression))
                                    {
                                        ccparams.PrevVal = ccparams.CurrVal;
                                        UpdateValue(ccparams);
                                        changed = true;
                                    }
                                }
                            }
                            if (changed) Application.Instance.Invoke(() => grid.ReloadData(i));
                            i += 1;
                        }
                    }
                }
                catch (Exception)
                {
                }
            }
            else
            {
                ccparams = cell;
                if ((ccparams != null))
                {
                    if (!string.IsNullOrEmpty(ccparams.Expression))
                    {
                        ccparams.PrevVal = cell.CurrVal;
                        UpdateValue(ccparams);
                    }
                }
            }


        }

        public void UpdateValue(SpreadsheetCellParameters cell)
        {

            nf = flowsheet.FlowsheetOptions.NumberFormat;
            var expression = cell.Expression;

            GetValues();

            try
            {
                ccparams = cell;
                if (!string.IsNullOrEmpty(expression))
                {
                    if (expression.Substring(0, 1) == "=")
                    {
                        this.ExprContext.Options.ParseCulture = System.Globalization.CultureInfo.InvariantCulture;
                        this.ExprContext.ParserOptions.FunctionArgumentSeparator = ';';
                        this.ExprObj = this.ExprContext.CompileGeneric<object>(expression.Substring(1));
                        var result = ExprObj.Evaluate();
                        double dresult = double.Parse(result.ToString());
                        cell.RawValue = dresult;
                        cell.CurrVal = dresult.ToString(nf);
                    }
                    else if (expression.Substring(0, 1) == ":")
                    {
                        string[] str = null;
                        string obj = null;
                        string prop = null;
                        str = expression.Split(new char[] { ',' });
                        obj = str[0].Substring(1);
                        ccparams.ObjectID = obj;
                        if (str.Length < 3)
                        {
                            prop = str[1];
                        }
                        else
                        {
                            prop = str[1] + "," + str[2];
                        }
                        ccparams.PropID = prop;
                        double result = ((double)flowsheet.SimulationObjects[obj].GetPropertyValue(prop, flowsheet.FlowsheetOptions.SelectedUnitSystem));
                        cell.RawValue = result;
                        cell.CurrVal = result.ToString(nf);
                        cell.ToolTipText = ccparams.ToolTipText;
                    }
                    else
                    {
                        double d;
                        if (double.TryParse(expression, out d))
                        {
                            cell.RawValue = double.Parse(expression);
                        }
                        cell.CurrVal = expression;
                    }
                }
            }
            catch (Exception ex)
            {
                cell.CurrVal = this.OldValue;
                ccparams.ToolTipText = "";
                flowsheet.ShowMessage(flowsheet.GetTranslatedString("Invalidexpressiononcell") + " " + GetCellString(cell) + " - " + ex.Message, IFlowsheet.MessageType.GeneralError);
            }
        }

        public string GetCellString(SpreadsheetCellParameters cell)
        {
            return cell.CellString;
        }

        public void DefineVariables()
        {
            foreach (var row in rowlist)
            {
                foreach (var ce in row.CellParams.Values)
                {
                    this.ExprContext.Variables.DefineVariable(this.GetCellString(ce), typeof(double));
                }
            }

        }

        public void GetValues()
        {
            foreach (var row in rowlist)
            {
                foreach (var ce in row.CellParams.Values)
                {
                    this.ExprContext.Variables[this.GetCellString(ce)] = ce.RawValue;
                }
            }
        }

        public List<string[]> GetDataFromRange(string range)
        {
            string l1, l2;
            int r1, r2, c1, c2;

            l1 = range.Split(':')[0].Substring(1);
            l2 = range.Split(':')[1].Substring(1);
            r1 = int.Parse(l1);
            r2 = int.Parse(l2);
            c1 = GetColumnNumber(range.Split(':')[0]);
            c2 = GetColumnNumber(range.Split(':')[1]);

            var list = new List<string[]>();

            int i, j;
            i = r1 - 1;
            for (i = r1 - 1; i < r2; i++)
            {
                var sublist = new List<string>();
                j = 1;
                foreach (var cellparam in rowlist[i].CellParams.Values)
                {
                    if (j >= c1)
                    {
                        sublist.Add(cellparam.CurrVal);
                    }
                    j += 1;
                    if (j > c2) break;
                }
                list.Add(sublist.ToArray());
            }

            return list;
        }

        public int GetColumnNumber(string cellname)
        {

            int column = 0;
            string colLetra = null;

            colLetra = cellname.Substring(0, 1);

            switch (colLetra)
            {
                case "A":
                    column = 0;
                    break;
                case "B":
                    column = 1;
                    break;
                case "C":
                    column = 2;
                    break;
                case "D":
                    column = 3;
                    break;
                case "E":
                    column = 4;
                    break;
                case "F":
                    column = 5;
                    break;
                case "G":
                    column = 6;
                    break;
                case "H":
                    column = 7;
                    break;
                case "I":
                    column = 8;
                    break;
                case "J":
                    column = 9;
                    break;
                case "K":
                    column = 10;
                    break;
                case "L":
                    column = 11;
                    break;
                case "M":
                    column = 12;
                    break;
                case "N":
                    column = 13;
                    break;
                case "O":
                    column = 14;
                    break;
                case "P":
                    column = 15;
                    break;
                case "Q":
                    column = 16;
                    break;
                case "R":
                    column = 17;
                    break;
                case "S":
                    column = 18;
                    break;
                case "T":
                    column = 19;
                    break;
                case "U":
                    column = 20;
                    break;
                case "V":
                    column = 21;
                    break;
                case "W":
                    column = 22;
                    break;
                case "X":
                    column = 23;
                    break;
                case "Y":
                    column = 24;
                    break;
                case "Z":
                    column = 25;
                    break;
                default:
                    return 0;
            }

            return column + 1;

        }

        public void CopyToDT()
        {
            int i = 0;
            int j = 0;

            i = 0;
            foreach (var row in rowlist)
            {
                j = 0;
                foreach (var cellparam in row.CellParams.Values)
                {
                    dt1[i, j] = cellparam.CurrVal;
                    dt2[i, j] = cellparam;
                    j = j + 1;
                }
                i = i + 1;
            }

        }

        public string CopyDT1ToString()
        {

            System.Globalization.CultureInfo ci = CultureInfo.InvariantCulture;

            double val = 0;

            string text = "";
            for (int i = 0; i < dt1.GetUpperBound(0); i++)
            {
                for (int j = 0; j < dt1.GetUpperBound(1); j++)
                {
                    if (double.TryParse(dt1[i, j].ToString(), NumberStyles.Any, ci, out val))
                    {
                        text += double.Parse(dt1[i, j].ToString(), ci).ToString() + ";";
                    }
                    else
                    {
                        if (dt1[i, j] != null)
                        {
                            text += dt1[i, j].ToString() + ";";
                        }
                        else
                        {
                            text += ";";
                        }
                    }
                }
                text = text.TrimEnd(';') + "|";
            }
            text = text.TrimEnd('|');

            return text;

        }

        public string CopyDT2ToString()
        {

            CultureInfo ci = CultureInfo.InvariantCulture;

            string text = "";
            for (int i = 0; i < dt2.GetUpperBound(0); i++)
            {
                for (int j = 0; j < dt1.GetUpperBound(1); j++)
                {
                    if ((dt2[i, j] != null))
                    {
                        try
                        {
                            XElement xel = new XElement("dummy", ((ICustomXMLSerialization)dt2[i, j]).SaveData().ToArray());
                            text += xel.ToString() + ";";
                        }
                        catch (Exception)
                        {
                            text += " ;";
                        }
                    }
                    else
                    {
                        text += " ;";
                    }
                }
                text = text.TrimEnd(';') + "|";
            }
            text = text.TrimEnd('|');

            return text;

        }

        public void CopyDT1FromString(string text)
        {
            string[] rows = text.Split('|');
            int n = 99;
            int m = 25;
            if (n > 0 & m > 0)
            {
                object[,] elm = new object[100, 26];
                try
                {
                    for (int i = 0; i <= n; i++)
                    {
                        if (n > 0)
                        {
                            m = rows[i].Split(';').Length - 1;
                        }
                        for (int j = 0; j <= m; j++)
                        {
                            double d;
                            if (double.TryParse(rows[i].Split(';')[j], out d)) elm[i, j] = double.Parse(rows[i].Split(';')[j]); else elm[i, j] = 0d;
                        }
                    }
                }
                catch (Exception) { }
                dt1 = elm;
            }

        }

        public void CopyDT2FromString(string text)
        {
            string[] rows = text.Split('|');
            int n = 99;
            int m = 25;
            if (n > 0)
            {
                m = rows[0].Split(';').Length - 1;
            }
            if (n > 0 & m > 0)
            {
                object[,] elm = new object[100, 26];
                for (int i = 0; i <= n; i++)
                {
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
                                elm[i, j] = scp;
                            }
                            else
                            {
                                elm[i, j] = scp;
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
            int i = 0;
            int j = 0;

            i = 0;
            foreach (var row in rowlist)
            {
                j = 0;
                foreach (var ce in row.CellParams.Values)
                {
                    try
                    {
                        if (dt2[i, j] is SpreadsheetCellParameters)
                        {
                            ce.LoadData(((SpreadsheetCellParameters)dt2[i, j]).SaveData());
                        }
                        else if (dt2[i, j] is object)
                        {
                            ce.Expression = dt2[i, j].ToString();
                            if (Convert.ToString(dt2[i, j]).StartsWith(":"))
                            {
                                ce.CellType = VarType.Read;
                                string[] str = null;
                                str = Convert.ToString(dt2[i, j]).Split(new char[] { ',' });
                                ce.ObjectID = str[0].Substring(1);
                                ce.PropID = str[1];
                            }
                            else
                            {
                                ce.CellType = VarType.Expression;
                            }
                        }
                    }
                    catch (Exception) { }
                    j = j + 1;
                }
                i = i + 1;
            }

        }

        public class RowItem
        {
            public string index { get; set; }
            public string A { get { return CellParams["A" + index.ToString()].CurrVal; } set { CellParams["A" + index.ToString()].Expression = value; } }
            public string B { get { return CellParams["B" + index.ToString()].CurrVal; } set { CellParams["B" + index.ToString()].Expression = value; } }
            public string C { get { return CellParams["C" + index.ToString()].CurrVal; } set { CellParams["C" + index.ToString()].Expression = value; } }
            public string D { get { return CellParams["D" + index.ToString()].CurrVal; } set { CellParams["D" + index.ToString()].Expression = value; } }
            public string E { get { return CellParams["E" + index.ToString()].CurrVal; } set { CellParams["E" + index.ToString()].Expression = value; } }
            public string F { get { return CellParams["F" + index.ToString()].CurrVal; } set { CellParams["F" + index.ToString()].Expression = value; } }
            public string G { get { return CellParams["G" + index.ToString()].CurrVal; } set { CellParams["G" + index.ToString()].Expression = value; } }
            public string H { get { return CellParams["H" + index.ToString()].CurrVal; } set { CellParams["H" + index.ToString()].Expression = value; } }
            public string I { get { return CellParams["I" + index.ToString()].CurrVal; } set { CellParams["I" + index.ToString()].Expression = value; } }
            public string J { get { return CellParams["J" + index.ToString()].CurrVal; } set { CellParams["J" + index.ToString()].Expression = value; } }
            public string K { get { return CellParams["K" + index.ToString()].CurrVal; } set { CellParams["K" + index.ToString()].Expression = value; } }
            public string L { get { return CellParams["L" + index.ToString()].CurrVal; } set { CellParams["L" + index.ToString()].Expression = value; } }
            public string M { get { return CellParams["M" + index.ToString()].CurrVal; } set { CellParams["M" + index.ToString()].Expression = value; } }
            public string N { get { return CellParams["N" + index.ToString()].CurrVal; } set { CellParams["N" + index.ToString()].Expression = value; } }
            public string O { get { return CellParams["O" + index.ToString()].CurrVal; } set { CellParams["O" + index.ToString()].Expression = value; } }
            public string P { get { return CellParams["P" + index.ToString()].CurrVal; } set { CellParams["P" + index.ToString()].Expression = value; } }
            public string Q { get { return CellParams["Q" + index.ToString()].CurrVal; } set { CellParams["Q" + index.ToString()].Expression = value; } }
            public string R { get { return CellParams["R" + index.ToString()].CurrVal; } set { CellParams["R" + index.ToString()].Expression = value; } }
            public string S { get { return CellParams["S" + index.ToString()].CurrVal; } set { CellParams["S" + index.ToString()].Expression = value; } }
            public string T { get { return CellParams["T" + index.ToString()].CurrVal; } set { CellParams["T" + index.ToString()].Expression = value; } }
            public string U { get { return CellParams["U" + index.ToString()].CurrVal; } set { CellParams["U" + index.ToString()].Expression = value; } }
            public string V { get { return CellParams["V" + index.ToString()].CurrVal; } set { CellParams["V" + index.ToString()].Expression = value; } }
            public string W { get { return CellParams["W" + index.ToString()].CurrVal; } set { CellParams["W" + index.ToString()].Expression = value; } }
            public string X { get { return CellParams["X" + index.ToString()].CurrVal; } set { CellParams["X" + index.ToString()].Expression = value; } }
            public string Y { get { return CellParams["Y" + index.ToString()].CurrVal; } set { CellParams["Y" + index.ToString()].Expression = value; } }
            public string Z { get { return CellParams["Z" + index.ToString()].CurrVal; } set { CellParams["Z" + index.ToString()].Expression = value; } }

            public Dictionary<string, SharedClasses.Spreadsheet.SpreadsheetCellParameters> CellParams = new Dictionary<string, SharedClasses.Spreadsheet.SpreadsheetCellParameters>();

            public SharedClasses.Spreadsheet.SpreadsheetCellParameters GetCell(string cell)
            {
                return CellParams[cell];
            }

            public RowItem(string idx)
            {
                index = idx;

                CellParams.Add("A" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "A" + index.ToString() });
                CellParams.Add("B" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "B" + index.ToString() });
                CellParams.Add("C" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "C" + index.ToString() });
                CellParams.Add("D" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "D" + index.ToString() });
                CellParams.Add("E" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "E" + index.ToString() });
                CellParams.Add("F" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "F" + index.ToString() });
                CellParams.Add("G" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "G" + index.ToString() });
                CellParams.Add("H" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "H" + index.ToString() });
                CellParams.Add("I" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "I" + index.ToString() });
                CellParams.Add("J" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "J" + index.ToString() });
                CellParams.Add("K" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "K" + index.ToString() });
                CellParams.Add("L" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "L" + index.ToString() });
                CellParams.Add("M" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "M" + index.ToString() });
                CellParams.Add("N" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "N" + index.ToString() });
                CellParams.Add("O" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "O" + index.ToString() });
                CellParams.Add("P" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "P" + index.ToString() });
                CellParams.Add("Q" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "Q" + index.ToString() });
                CellParams.Add("R" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "R" + index.ToString() });
                CellParams.Add("S" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "S" + index.ToString() });
                CellParams.Add("T" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "T" + index.ToString() });
                CellParams.Add("U" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "U" + index.ToString() });
                CellParams.Add("V" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "V" + index.ToString() });
                CellParams.Add("W" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "W" + index.ToString() });
                CellParams.Add("X" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "X" + index.ToString() });
                CellParams.Add("Y" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "Y" + index.ToString() });
                CellParams.Add("Z" + index.ToString(), new SpreadsheetCellParameters() { CellType = VarType.None, CellString = "Z" + index.ToString() });

            }
        }

    }
}
