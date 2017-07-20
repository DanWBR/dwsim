using System;
using System.Collections.Generic;
using Eto.Forms;
using Eto.Drawing;
using System.Diagnostics;
using DWSIM.Interfaces.Enums.GraphicObjects;

namespace DWSIM.UI.Desktop.Editors
{
    partial class InsertObject : Dialog
    {

        public Interfaces.IFlowsheet Flowsheet;

        public int FlowsheetHeight = 400;

        private bool initialized = false;

        private List<Type> typelist = new  List<Type>();
        public Dictionary<String, Interfaces.ISimulationObject> ObjList;   

        void InitializeComponent()
        {
            
            string imgprefix = "DWSIM.UI.Desktop.Editors.Resources.Icons.";

            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            Maximizable = false;
            Minimizable = false;
            WindowStyle = Eto.Forms.WindowStyle.Default;

            Title = "Add Object to the Flowsheet";

            var container = new TableLayout();

            var topcontainer = new TableLayout();

            var bottomcontainer = new TableLayout();

            var rightcontainer = new TableLayout();

            var lblName = new Label() { Width = 300 };
            var lblVersion = new Label();
            var lblLib = new Label();
            var lblLibVersion = new Label();
            var lblDesc = new TextArea { ReadOnly = true };
            var lblAbout = new TextArea { ReadOnly = true };

            rightcontainer.Rows.Add(new TableRow(new Label { Text = "Name" }, lblName));
            rightcontainer.Rows.Add(new TableRow(new Label { Text = "Version" }, lblVersion));
            rightcontainer.Rows.Add(new TableRow(new Label { Text = "Library" }, lblLib));
            rightcontainer.Rows.Add(new TableRow(new Label { Text = "Library Version" }, lblLibVersion));
            rightcontainer.Rows.Add(new TableRow(new Label { Text = "Description" }, lblDesc));
            rightcontainer.Rows.Add(new TableRow(new Label { Text = "About" }, lblAbout));

            rightcontainer.Padding = new Padding(5, 5, 5, 5);
            rightcontainer.Spacing = new Size(10, 10);

            var list = new ListBox { Height = 200, Width = 300 };

            var btnAdd = new Button { Text = "Add Object" };

            var btnCancel = new Button { Text = "Cancel" };

            btnCancel.Click += (sender, e) => {
                Close();
            };

            btnAdd.Click += (sender, e) => {

                ObjectType tobj = ObjectType.Nenhum;

                switch (ObjList[list.SelectedValue.ToString()].GetType().Name)
                {
                    case "Adjust":
                        tobj = ObjectType.OT_Adjust;
                        break;
                    case "Spec":
                        tobj = ObjectType.OT_Spec;
                        break;
                    case "Recycle":
                        tobj = ObjectType.OT_Recycle;
                        break;
                    case "EnergyRecycle":
                        tobj = ObjectType.OT_EnergyRecycle;
                        break;
                    case "Mixer":
                        tobj = ObjectType.NodeIn;
                        break;
                    case "Splitter":
                        tobj = ObjectType.NodeOut;
                        break;
                    case "Pump":
                        tobj = ObjectType.Pump;
                        break;
                    case "Tank":
                        tobj = ObjectType.Tank;
                        break;
                    case "Vessel":
                        tobj = ObjectType.Vessel;
                        break;
                    case "MaterialStream":
                        tobj = ObjectType.MaterialStream;
                        break;
                    case "EnergyStream":
                        tobj = ObjectType.EnergyStream;
                        break;
                    case "Compressor":
                        tobj = ObjectType.Compressor;
                        break;
                    case "Expander":
                        tobj = ObjectType.Expander;
                        break;
                    case "Cooler":
                        tobj = ObjectType.Cooler;
                        break;
                    case "Heater":
                        tobj = ObjectType.Heater;
                        break;
                    case "Pipe":
                        tobj = ObjectType.Pipe;
                        break;
                    case "Valve":
                        tobj = ObjectType.Valve;
                        break;
                    case "Reactor_Conversion":
                        tobj = ObjectType.RCT_Conversion;
                        break;
                    case "Reactor_Equilibrium":
                        tobj = ObjectType.RCT_Equilibrium;
                        break;
                    case "Reactor_Gibbs":
                        tobj = ObjectType.RCT_Gibbs;
                        break;
                    case "Reactor_CSTR":
                        tobj = ObjectType.RCT_CSTR;
                        break;
                    case "Reactor_PFR":
                        tobj = ObjectType.RCT_PFR;
                        break;
                    case "HeatExchanger":
                        tobj = ObjectType.HeatExchanger;
                        break;
                    case "ShortcutColumn":
                        tobj = ObjectType.ShortcutColumn;
                        break;
                    case "DistillationColumn":
                        tobj = ObjectType.DistillationColumn;
                        break;
                    case "AbsorptionColumn":
                        tobj = ObjectType.AbsorptionColumn;
                        break;
                    case "ReboiledAbsorber":
                        tobj = ObjectType.ReboiledAbsorber;
                        break;
                    case "RefluxedAbsorber":
                        tobj = ObjectType.RefluxedAbsorber;
                        break;
                    case "ComponentSeparator":
                        tobj = ObjectType.ComponentSeparator;
                        break;
                    case "OrificePlate":
                        tobj = ObjectType.OrificePlate;
                        break;
                    case "CustomUO":
                        tobj = ObjectType.CustomUO;
                        break;
                    case "ExcelUO":
                        tobj = ObjectType.ExcelUO;
                        break;
                    case "CapeOpenUO":
                        tobj = ObjectType.CapeOpenUO;
                        break;
                    case "SolidsSeparator":
                        tobj = ObjectType.SolidSeparator;
                        break;
                    case "Filter":
                        tobj = ObjectType.Filter;
                        break;
                    case "Flowsheet":
                        tobj = ObjectType.FlowsheetUO;
                        break;
                }

                Flowsheet.AddObject(tobj, 50, 50, "");
                Flowsheet.UpdateInterface();
                
                this.Close();

            };

            topcontainer.Rows.Add(new TableRow(new Label {Text = "Select an object from the list to add it to the flowsheet."}));
            topcontainer.Rows.Add(new TableRow(list, rightcontainer));
            topcontainer.Padding = new Padding(5, 5, 5, 5);
            topcontainer.Spacing = new Size(10, 10);

            bottomcontainer.Rows.Add(new TableRow(null, btnCancel, btnAdd));
            bottomcontainer.Padding = new Padding(5, 5, 5, 5);
            bottomcontainer.Spacing = new Size(10, 10);

            container.Rows.Add(new TableRow(topcontainer));
            container.Rows.Add(new TableRow(bottomcontainer));

            container.Padding = new Padding(5, 5, 5, 5);

            Content = container;

            Load += (sender, e) => {
                foreach (var item in ObjList)
                {
                    var li = new ListItem() { Text = item.Key };
                    list.Items.Add(li);
                }

                list.SelectedIndexChanged += (sender2, e2) =>
                {

                    if (!initialized) return;

                    var obj = ObjList[list.SelectedValue.ToString()];

                    var fi = FileVersionInfo.GetVersionInfo(obj.GetType().Assembly.Location);

                    lblName.Text = obj.GetDisplayName();
                    lblVersion.Text = obj.GetVersion().ToString();
                    lblLib.Text = fi.OriginalFilename;
                    lblLibVersion.Text = obj.GetType().Assembly.GetName().Version.ToString();

                    lblDesc.Text = obj.GetDisplayDescription();

                    lblAbout.Text = fi.FileDescription + "\n" + fi.Comments + "\n" + fi.LegalCopyright;

                };
                
                list.SelectedIndex = 0;
            };

            initialized = true;

        }

    }
}
