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

            objselector.SelectedIndexChanged += (sender, e) => {
                Chart.OwnerID = objselector.SelectedKey;
                chartselector.Items.Clear();
                if (Chart.Flowsheet.SimulationObjects.ContainsKey(Chart.OwnerID))
                {
                    var charts = Chart.Flowsheet.SimulationObjects[Chart.OwnerID].GetChartModelNames();
                    foreach (var str in charts)
                    {
                        chartselector.Items.Add(str);
                    }
                }
            };
            
            if (Chart.Flowsheet.SimulationObjects.ContainsKey(Chart.OwnerID))
            {
                objselector.SelectedKey = Chart.OwnerID;
                var charts = Chart.Flowsheet.SimulationObjects[Chart.OwnerID].GetChartModelNames();
                foreach (var str in charts)
                {
                    objselector.Items.Add(str);
                }
            }

            chartselector.SelectedIndexChanged += (sender, e) =>
            {
                Chart.ModelName = chartselector.SelectedValue.ToString();
            };

            container.CreateAndAddTextBoxRow("N0", "Chart Width", Chart.Width, (sender, e) => {
                if (sender.Text.IsValidDouble()) Chart.Width = (int)sender.Text.ToDoubleFromCurrent();
            });

            container.CreateAndAddTextBoxRow("N0", "Chart Height", Chart.Height, (sender, e) =>
            {
                if (sender.Text.IsValidDouble()) Chart.Height = (int)sender.Text.ToDoubleFromCurrent();
            });

            container.CreateAndAddTextBoxRow("N2", "Chart Scale", Chart.Scale, (sender, e) =>
            {
                if (sender.Text.IsValidDouble()) Chart.Scale = sender.Text.ToDoubleFromCurrent();
            });

            Content = container;

            ClientSize = new Size(400, 300);

        }

    }
}
