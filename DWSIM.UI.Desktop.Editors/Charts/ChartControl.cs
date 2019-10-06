using Eto.Forms;
using System;
using OxyPlot;
using Eto.OxyPlot;
using DWSIM.SharedClasses.Charts;

namespace DWSIM.UI.Desktop.Editors.Charts
{
    class ChartControl : DynamicLayout
    {

        public Plot ChartView;
        public Chart Chart;
        public PropertyGrid PGrid;

        public ChartControl()
        {

            Chart = new Chart();

            Splitter sp = new Splitter { Orientation = Orientation.Horizontal, FixedPanel = SplitterFixedPanel.Panel2 };

            ChartView = new Plot();

            ChartView.Model = (PlotModel)Chart.PlotModel;

            sp.Panel1 = ChartView;

            PGrid = new PropertyGrid();

            sp.Panel2 = PGrid;

            sp.Panel2.Width = 300;

            PGrid.SelectedObject = Chart;

            this.Add(sp, true, true);

        }


    }

}
