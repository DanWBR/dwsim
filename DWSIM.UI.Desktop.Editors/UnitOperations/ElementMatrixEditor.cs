using DWSIM.CrossPlatform.UI.Controls.ReoGrid;
using Eto.Forms;
using System;
using DWSIM.ExtensionMethods.Eto;

namespace DWSIM.UI.Desktop.Editors.UnitOperations
{
    public class ElementMatrixEditor : DynamicLayout
    {

        public DWSIM.UnitOperations.Reactors.Reactor_Gibbs greactor;

        private ReoGridFullControl gridcontrol;

        public ElementMatrixEditor(DWSIM.UnitOperations.Reactors.Reactor_Gibbs reactor)
        {
            greactor = reactor;
            Init();
        }

        void Init()
        {

            Padding = new Eto.Drawing.Padding(10);

            gridcontrol = GridControl.GetGridControl();

            Extensions2.AddButtonWithLabel(this, "Reset/Rebuild Element Matrix", "Reset", (btn, e) => {
                greactor.CreateElementMatrix();
                Application.Instance.Invoke(() => Populate());
            });

            Extensions2.AddButtonWithLabel(this, "Update/Save Matrix Changes", "Save", (btn, e) => {
                Application.Instance.Invoke(() => Save());
            });

            this.Add(gridcontrol);

            Populate();

        }

        void Populate()
        {

            var grid = gridcontrol.GridControl;

            var sheet = grid.Worksheets[0];

            int i, j, c, e;

            j = 0;
            foreach (var comp in greactor.ComponentIDs)
            {
                sheet.ColumnHeaders[j].Text = comp;
                j += 1;
            }
            i = 0;
            foreach (var el in greactor.Elements)
            {
                sheet.RowHeaders[i].Text = el;
                i += 1;
            }

            c = greactor.ComponentIDs.Count - 1;
            e = greactor.Elements.Length - 1;

            for (i = 0; i <= e; i++)
            {
                for (j = 0; j <= c; j++)
                {
                    sheet.Cells[i, j].Data = greactor.ElementMatrix[i, j];
                }
            }

            sheet.SetRows(sheet.MaxContentRow + 1);
            sheet.SetCols(sheet.MaxContentCol + 1);

            sheet.SetColumnsWidth(0, c + 1, 100);

        }

        void Save()
        {

            var grid = gridcontrol.GridControl;

            var sheet = grid.Worksheets[0];

            int i, j, c, e;

            c = greactor.ComponentIDs.Count - 1;
            e = greactor.Elements.Length - 1;

            for (i = 0; i <= e; i++)
            {
                for (j = 0; j <= c; j++)
                {
                    try
                    {
                        greactor.ElementMatrix[i, j] = (double)sheet.Cells[i, j].Data;
                    }
                    catch { }
                }
            }

        }
    }
}
