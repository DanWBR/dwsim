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
using System.Globalization;


namespace DWSIM.UI.Desktop.Editors
{
    public class MaterialStreamListViewer : TableLayout
    {

        public TextArea txtView;
        public List<List<string>> Data;
        private IFlowsheet Flowsheet;
        private Label lblLastUpdate;

        public int PadSize = 20;

        public MaterialStreamListViewer(IFlowsheet fs)
        {
            Flowsheet = fs;
            Init();
        }

        public void UpdateList()
        {

            Data = new List<List<string>>();

            var su = Flowsheet.FlowsheetOptions.SelectedUnitSystem;
            var nf = Flowsheet.FlowsheetOptions.NumberFormat;

            string[] props = null, units = null;
            string value = null;
            List<string> mslist;

            mslist = Flowsheet.SimulationObjects.Values.Where((x) => x is MaterialStream).Select((y) => y.GraphicObject.Tag).ToList();

            foreach (var ms in Flowsheet.SimulationObjects.Values.Where((x) => x is MaterialStream))
            {
                props = ms.GetProperties(Interfaces.Enums.PropertyType.ALL);
                units = (string[])props.Clone();
                break;
            }

            if (props == null) return;

            foreach (var item in props)
            {
                var list = new List<string>();
                foreach (var ms in Flowsheet.SimulationObjects.Values.Where((x) => x is MaterialStream))
                {
                    list.Add("");
                }
                Data.Add(list);
            }


            int i = 0, j = 0;
            double d = 0;
            foreach (string prop in props)
            {
                j = 0;
                foreach (var ms in Flowsheet.SimulationObjects.Values.Where((x) => x is MaterialStream))
                {
                    var data = ms.GetPropertyValue(prop, su);
                    if (data != null) value = data.ToString(); else value = "";
                    units[i] = ms.GetPropertyUnit(prop, su);
                    if (double.TryParse(value, out d))
                    {
                        if (double.IsNaN(d))
                        {
                            Data[i][j] = "N/A";
                        }
                        else
                        {
                            Data[i][j] = double.Parse(value).ToString(nf);
                        }
                    }
                    else
                    {
                        Data[i][j] = value;
                    }
                    j += 1;
                }
                i += 1;
            }

            var maxlength = props.Select((x) => Flowsheet.GetTranslatedString(x)).Select((x) => x.Length).Max();

            string textlist = "Property / Material Stream".PadRight(maxlength + 10);
            foreach (var name in mslist)
            {
                textlist += name.PadLeft(PadSize);
            }
            textlist += Environment.NewLine;

            i = 0;
            foreach (var list in Data)
            {
                if (units[i] != "")
                {
                    textlist += (Flowsheet.GetTranslatedString(props[i]) + " (" + units[i] + ")").PadRight(maxlength + 10);
                }
                else {
                    textlist += Flowsheet.GetTranslatedString(props[i]).PadRight(maxlength + 10);
                }
                foreach (var list2 in list)
                {
                    textlist += list2.PadLeft(PadSize);
                }
                textlist += Environment.NewLine;
                i += 1;
            }

            txtView.Text = textlist;

            lblLastUpdate.Text = "Updated on: " + DateTime.Now.ToString();

        }

        void Init()
        {

            var topcontainer = new TableLayout();
            var centercontainer = new TableLayout();
                       
            var btnUpdate = new Button { Text = "Update Data" };
            btnUpdate.Click += (sender, e) =>
            {
                UpdateList();
            };

            var txtpad = new TextBox{Text = PadSize.ToString()};
            txtpad.TextChanged += (sender, e) =>{
                int ps = 0;
                if (int.TryParse(txtpad.Text, out ps)) PadSize = int.Parse(txtpad.Text);
            };

            lblLastUpdate = new Label { Text = "Updated on: ", VerticalAlignment = VerticalAlignment.Center };

            topcontainer.Padding = new Padding(5, 5, 5, 5);
            topcontainer.Spacing = new Size(10, 10);
            if (GlobalSettings.Settings.RunningPlatform() == GlobalSettings.Settings.Platform.Mac) topcontainer.Height = 34;

            topcontainer.Rows.Add(new TableRow(lblLastUpdate, null, new Label { Text = "Column Size", VerticalAlignment = VerticalAlignment.Center }, txtpad, btnUpdate) { ScaleHeight = false });

            txtView = new TextArea { ReadOnly = true, Wrap = false, Font = Fonts.Monospace(SystemFonts.Default().Size) };

            centercontainer.Rows.Add(new TableRow(txtView));
            centercontainer.Padding = new Padding(5, 5, 5, 5);
            centercontainer.Spacing = new Size(10, 10);

            Rows.Add(new TableRow(topcontainer));
            Rows.Add(new TableRow(centercontainer));

        }

    }
}
