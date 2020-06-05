using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;

using Eto.Forms;
using Eto.Drawing;

using DWSIM.ExtensionMethods;
using DWSIM.UI.Shared;
using s = DWSIM.UI.Shared.Common;
using DWSIM.Interfaces.Enums;
using DWSIM.Drawing.SkiaSharp.GraphicObjects.Charts;

namespace DWSIM.UI.Desktop.Editors.Charts
{
    public class ChartObjectEditor : Form
    {

        public OxyPlotGraphic Chart;

        public Button btnOK;

        public ChartObjectEditor(OxyPlotGraphic obj)
        {
            Chart = obj;
            Init();
        }

        void Init()
        {

            string imgprefix = "DWSIM.UI.Desktop.Editors.Resources.Icons.";

            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            Maximizable = false;
            Minimizable = false;
            WindowStyle = Eto.Forms.WindowStyle.Default;

            Title = "Configure Chart Object";

            var container = s.GetDefaultContainer();

            var objselector = container.CreateAndAddDropDownRow("Flowsheet Object", new List<string>(), 0, null);

            var chartselector = container.CreateAndAddDropDownRow("Chart Name", new List<string>(), 0, null);

            foreach (var obj in Chart.Flowsheet.SimulationObjects.Values)
            {
                objselector.Items.Add(new ListItem { Text = obj.GraphicObject.Tag, Key = obj.Name });
            }
            objselector.Items.Add("Dynamic Mode Integrators");

            objselector.SelectedIndexChanged += (sender, e) =>
            {
                if (objselector.SelectedIndex < objselector.Items.Count - 1)
                {
                    Chart.OwnerID = objselector.SelectedKey;
                    chartselector.Items.Clear();
                    if (Chart.Flowsheet.SimulationObjects.ContainsKey(Chart.OwnerID))
                    {
                        var charts = Chart.Flowsheet.SimulationObjects[Chart.OwnerID].GetChartModelNames();
                        foreach (var str in charts)
                        {
                            chartselector.Items.Add(new ListItem { Key = str, Text = str });
                        }
                    }
                }
                else
                {
                    Chart.OwnerID = objselector.SelectedValue.ToString();
                    chartselector.Items.Clear();
                    foreach (var item in Chart.Flowsheet.DynamicsManager.IntegratorList)
                    {
                        chartselector.Items.Add(new ListItem { Text = item.Value.Description, Key = item.Value.ID });
                    }
                }
            };

            if (Chart.OwnerID != null)
            {
                if (Chart.Flowsheet.SimulationObjects.ContainsKey(Chart.OwnerID))
                {
                    objselector.SelectedKey = Chart.OwnerID;
                    chartselector.Items.Clear();
                    var charts = Chart.Flowsheet.SimulationObjects[Chart.OwnerID].GetChartModelNames();
                    foreach (var str in charts)
                    {
                        chartselector.Items.Add(new ListItem { Key = str, Text = str });
                    }
                    chartselector.SelectedKey = Chart.ModelName;
                }
                else if (Chart.OwnerID == "Dynamic Mode Integrators")
                {
                    objselector.SelectedKey = Chart.OwnerID;
                    chartselector.Items.Clear();
                    foreach (var item in Chart.Flowsheet.DynamicsManager.IntegratorList)
                    {
                        chartselector.Items.Add(new ListItem { Text = item.Value.Description, Key = item.Value.Description });
                    }
                    chartselector.SelectedKey = Chart.ModelName;
                }
            }

            chartselector.SelectedIndexChanged += (sender, e) =>
            {
                if (chartselector.SelectedIndex > -1)
                {
                    Chart.ModelName = chartselector.SelectedValue.ToString();
                }
            };

            container.CreateAndAddTextBoxRow("N0", "Chart Width", Chart.Width, (sender, e) =>
            {
                if (sender.Text.IsValidDouble()) Chart.Width = (int)sender.Text.ToDoubleFromCurrent();
            });

            container.CreateAndAddTextBoxRow("N0", "Chart Height", Chart.Height, (sender, e) =>
            {
                if (sender.Text.IsValidDouble()) Chart.Height = (int)sender.Text.ToDoubleFromCurrent();
            });

            Content = container;

            ClientSize = new Size(400, 200);

        }

    }
}
