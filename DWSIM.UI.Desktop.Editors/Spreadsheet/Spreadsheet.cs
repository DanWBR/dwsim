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

namespace DWSIM.UI.Desktop.Editors
{
    public class Spreadsheet
    {

        public Dictionary<String, Interfaces.ISimulationObject> ObjList;

        private IGenericExpression<Object> ExprObj ;
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
        private bool loaded;

        public Spreadsheet(IFlowsheet fs)
        {
            flowsheet = fs;
        }

        public void WriteAll()
        {

            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;

            foreach (var row in rowlist)
            {
                foreach (var cellparam in row.CellParams.Values)
                {
                    if ((cellparam != null))
                    {
                        if (cellparam.CellType == SharedClasses.Spreadsheet.VarType.Write)
                        {
                            var obj = flowsheet.SimulationObjects[ccparams.ObjectID];
                            obj.SetPropertyValue(ccparams.PropID, cellparam.CurrVal, su);
                        }
                    }
                }
            }

        }

        public void EvaluateAll(SpreadsheetCellParameters cell = null)
        {
            if (cell == null)
            {
                try
                {
                    if ((flowsheet != null))
                    {
                        if (GlobalSettings.Settings.CalculatorActivated)
                        {
                            foreach (var row in rowlist)
                            {
                                foreach (var cellparam in row.CellParams.Values)
                                {
                                    ccparams = cellparam;
                                    if ((ccparams != null))
                                    {
                                        if (!string.IsNullOrEmpty(ccparams.Expression))
                                        {
                                            ccparams.PrevVal = ccparams.CurrVal;
                                            UpdateValue(ccparams);
                                        }
                                    }
                                }
                            }
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

            var expression = cell.Expression;

            if (this.ExprContext == null)
            {
                this.ExprContext = new Ciloci.Flee.ExpressionContext();
                this.ExprContext.Imports.AddType(typeof(System.Math));
                this.ExprContext.Imports.AddType(typeof(System.String));
            }

            if (this.loaded == false) DefineVariables();

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
                        cell.CurrVal = ExprObj.Evaluate().ToString();
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
                        cell.CurrVal = flowsheet.SimulationObjects[obj].GetPropertyValue(prop, flowsheet.FlowsheetOptions.SelectedUnitSystem).ToString();
                        cell.ToolTipText = ccparams.ToolTipText;
                    }
                    else {
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
                    double val = 0;
                    double.TryParse(ce.CurrVal, out val);
                    this.ExprContext.Variables[this.GetCellString(ce)] = val;
                }
            }
        }

        public TableLayout GetSpreadsheet(IFlowsheet obj)
        {

            rowlist = new ObservableCollection<RowItem>();

            grid = new GridView { DataStore = rowlist, RowHeight = 20 };

            if (GlobalSettings.Settings.RunningPlatform() != GlobalSettings.Settings.Platform.Windows)
            {
                grid.Columns.Add(new GridColumn { HeaderText = "", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.index) }, Editable = false, AutoSize = false, Width = 50, Resizable = false });
            }
            grid.Columns.Add(new GridColumn { HeaderText = "A", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.A) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "B", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.B) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "C", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.C) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "D", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.D) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "E", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.E) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "F", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.F) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "G", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.G) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "H", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.H) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "I", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.I) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "J", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.J) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "K", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.K) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "L", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.L) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "M", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.M) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "N", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.N) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "O", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.O) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "P", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.P) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "Q", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.Q) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "R", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.R) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "S", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.S) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "T", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.T) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "U", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.U) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "V", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.V) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "W", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.W) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "X", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.X) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "Y", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.Y) }, AutoSize = false, Editable = true, Width = 80 });
            grid.Columns.Add(new GridColumn { HeaderText = "Z", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.Z) }, AutoSize = false, Editable = true, Width = 80 });

            grid.AllowColumnReordering = false;
            grid.AllowMultipleSelection = false;
            grid.GridLines = GridLines.Both;
            grid.Style = "spreadsheet";

            int i;
            for (i = 0; i <= 50; i++)
            {
                rowlist.Add(new RowItem((i + 1).ToString()));
            }

            var table = new TableLayout();

            var cellcp = new Panel();

            var txtcell = new TextBox { Width = 80, ReadOnly = true};
            var txttype = new TextBox { Width = 200, ReadOnly = true};
            var txtformula = new TextBox() { PlaceholderText = "To enter a formula,  type '=' followed by the math expression and press ENTER to commit changes." };
            var btnImport = new Button { Text = "Import" };
            var btnExport = new Button { Text = "Export" };
            var btnClear = new Button { Text = "Clear" };
            var btnEvaluate = new Button { Text = "Evaluate" };
            var btnEvaluateAll = new Button { Text = "Evaluate All" };

            btnEvaluate.Click += (sender, e) => {
                EvaluateAll(ccparams);
                grid.ReloadData(int.Parse(rowitem.index) - 1);
            };

            btnEvaluateAll.Click += (sender, e) =>
            {
                EvaluateAll();
                for (i = 0; i <= 50; i++)
                {
                    grid.ReloadData(i);
                }
            };


            btnClear.Click += (sender, e) => {

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

            btnImport.Click += (sender, e) => {
                
                var selector = new PropertySelector() { Flowsheet = flowsheet, ObjList = ObjList };

                selector.btnOK.Click += (sender2, e2) => {

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

            var tb = new TableLayout {Spacing = new Size(5, 5), Padding = new Padding(10), Height = 44 };
            tb.Rows.Add(tr);
            
            cellcp.Content = tb;

            table.Rows.Add(new TableRow(cellcp));

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

            grid.CellClick += (sender, e) =>
            {
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

            grid.CellEdited += (sender, e) => {
                UpdateValue(rowitem.CellParams[selectedcell]);
            };

            DefineVariables();

            loaded = true;

            table.Rows.Add(new TableRow(new Scrollable { Content = grid, Border = BorderType.None }));

            return table;

        }

        public class RowItem
        {
            public string index { get; set; }
            public string A { get { return CellParams["A" + index.ToString()].CurrVal; } set { CellParams["A" + index.ToString()].Expression = value; } }
            public string B { get { return CellParams["B" + index.ToString()].CurrVal; } set { CellParams["B" + index.ToString()].Expression = value; } }
            public string C { get { return CellParams["D" + index.ToString()].CurrVal; } set { CellParams["C" + index.ToString()].Expression = value; } }
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
