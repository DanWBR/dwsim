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
using System.Collections.ObjectModel;

namespace DWSIM.UI.Desktop.Editors
{
    public class Spreadsheet
    {

        public static GridView GetGrid(IFlowsheet obj)
        {

            var rowlist = new ObservableCollection<RowItem>();

            var grid = new GridView { DataStore = rowlist, RowHeight = 20 };

            //grid.Columns.Add(new GridColumn { HeaderText = "#", DataCell = new TextBoxCell { Binding = Binding.Property<RowItem, string>(r => r.index) }, Editable = false, AutoSize = true});
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
                rowlist.Add(new RowItem { index = i.ToString() });
            }

            var ctxmenu = new ContextMenu();

            string selectedcell = "";            

            grid.CellClick += (sender, e) =>
            {
                selectedcell = e.GridColumn.HeaderText + e.Row.ToString();
            };

            ctxmenu.Opening += (sender, e) => {
                ctxmenu.Items.Clear();
                ctxmenu.Items.Add(new ButtonMenuItem {Text = "Selected Cell: " + selectedcell});                    
            };

            grid.ContextMenu = ctxmenu;

            return grid;

        }

        public class RowItem
        {
            public string index { get; set; }
            public string A { get; set; }
            public string B { get; set; }
            public string C { get; set; }
            public string D { get; set; }
            public string E { get; set; }
            public string F { get; set; }
            public string G { get; set; }
            public string H { get; set; }
            public string I { get; set; }
            public string J { get; set; }
            public string K { get; set; }
            public string L { get; set; }
            public string M { get; set; }
            public string N { get; set; }
            public string O { get; set; }
            public string P { get; set; }
            public string Q { get; set; }
            public string R { get; set; }
            public string S { get; set; }
            public string T { get; set; }
            public string U { get; set; }
            public string V { get; set; }
            public string W { get; set; }
            public string X { get; set; }
            public string Y { get; set; }
            public string Z { get; set; }

            public RowItem()
            {
                index = "";
                A = "";
                B = "";
                C = "";
                D = "";
                E = "";
                F = "";
                G = "";
                H = "";
                I = "";
                J = "";
                K = "";
                L = "";
                M = "";
                N = "";
                O = "";
                P = "";
                Q = "";
                R = "";
                S = "";
                T = "";
                U = "";
                V = "";
                W = "";
                X = "";
                Y = "";
                Z = "";
            }
        }

    }
}
