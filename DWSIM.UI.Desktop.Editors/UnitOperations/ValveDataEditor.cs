using DWSIM.CrossPlatform.UI.Controls.ReoGrid;
using Eto.Forms;
using System;
using DWSIM.ExtensionMethods.Eto;
using DWSIM.ExtensionMethods;

namespace DWSIM.UI.Desktop.Editors.UnitOperations
{
    public class ValveDataEditor : DynamicLayout
    {

        public DWSIM.UnitOperations.UnitOperations.Valve valv;

        private ReoGridFullControl gridcontrol;

        public ValveDataEditor(DWSIM.UnitOperations.UnitOperations.Valve valve)
        {
            valv = valve;
            Init();
        }

        void Init()
        {

            Padding = new Eto.Drawing.Padding(10);

            gridcontrol = GridControl.GetGridControl();

            var grid = gridcontrol.GridControl;

            var sheet = grid.Worksheets[0];

            sheet.SetRows(100);
            sheet.SetCols(2);
            sheet.SetColumnsWidth(0, 2, 100);
            sheet.ColumnHeaders[0].Text = "Opening (%)";
            sheet.ColumnHeaders[1].Text = "Kv/Kvmax (%)";

            this.Add(gridcontrol);

            Populate();

        }

        void Populate()
        {

            var grid = gridcontrol.GridControl;

            var sheet = grid.Worksheets[0];

            for (int i = 0; i < valv.OpeningKvRelDataTableX.Count; i++)
            {
                sheet.SetCellData(i, 0, valv.OpeningKvRelDataTableX[i]);
            }
            for (int i = 0; i < valv.OpeningKvRelDataTableY.Count; i++)
            {
                sheet.SetCellData(i, 1, valv.OpeningKvRelDataTableY[i]);
            }

        }

        public void Save()
        {

            var grid = gridcontrol.GridControl;

            var sheet = grid.Worksheets[0];

            valv.OpeningKvRelDataTableX.Clear();
            valv.OpeningKvRelDataTableY.Clear();
            for (int i = 0; i < 100; i++)
            {
                var datax = sheet.GetCellData(i, 0);
                var datay = sheet.GetCellData(i, 1);
                if (datax != null && datay != null)
                {
                    try
                    {
                        valv.OpeningKvRelDataTableX.Add(datax.ToString().ToDoubleFromInvariant());
                        valv.OpeningKvRelDataTableY.Add(datay.ToString().ToDoubleFromInvariant());
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show(String.Format("Error on data table: {0}", ex.Message), "Error", MessageBoxButtons.OK, MessageBoxType.Error);
                    }
                }
            }

        }
    }
}
