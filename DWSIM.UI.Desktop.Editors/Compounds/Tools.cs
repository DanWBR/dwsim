using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.Thermodynamics.BaseClasses;
using DWSIM.UI.Desktop.Shared;
using Eto.Drawing;
using Eto.Forms;
using c = DWSIM.UI.Shared.Common;
using DWSIM.UI.Shared;

namespace DWSIM.UI.Desktop.Editors
{
    public class CompoundTools : DynamicLayout
    {
        public Flowsheet flowsheet;

        public CompoundTools(Flowsheet fs) :
            base()
        {
            flowsheet = fs;
            Init();
        }

        void Init()
        {

            Padding = new Padding(10);

            this.CreateAndAddLabelRow("Compound Creator");
            var btn0 = this.CreateAndAddLabelAndButtonRow("Create a new compound from UNIFAC information and experimental data", "Create Compound", null,
                (sender, e) => {
                    var form = c.GetDefaultEditorForm("Compound Creator", 500, 600, new CompoundCreatorView(flowsheet));
                    form.Show();
                    this.ParentWindow.Close();
                });

            this.CreateAndAddLabelRow("Petroleum Characterization");
            var btn1 = this.CreateAndAddLabelAndButtonRow("Create new compounds from Bulk C7+ Petroleum Assay information", "Bulk C7+ Petroleum Characterization", null,
                (sender, e) =>
                {
                    var form = c.GetDefaultEditorForm("Bulk C7+ Petroleum Characterization Tool", 500, 600, new BulkC7PCharacterization(flowsheet));
                    form.Show();
                    this.ParentWindow.Close();
                });

            var btn2 = this.CreateAndAddLabelAndButtonRow("Create new compounds from Distillation Curves Petroleum Assay information", "Distillation Curves Petroleum Characterization", null,
            (sender, e) =>
            {
                var form = c.GetDefaultEditorForm("Distillation Curves Petroleum Characterization Tool", 500, 600, new DistCurvePCharacterization(flowsheet));
                form.Show();
                this.ParentWindow.Close();
            });

            btn0.Width = 350;
            btn1.Width = 350;
            btn2.Width = 350;

        }
    }
}
