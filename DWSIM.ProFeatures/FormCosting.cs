using DWSIM.Interfaces;
using System;

namespace DWSIM.ProFeatures
{


    public partial class FormCosting : WeifenLuo.WinFormsUI.Docking.DockContent
    {

        public IFlowsheet CurrentFlowsheet;

        public FormCosting()
        {
            InitializeComponent();
            lblFeature = _lblFeature;
            _lblFeature.Name = "lblFeature";
        }

        private void FormCosting_Load(object sender, EventArgs e)
        {

            ExtensionMethods.FormExtensions.ChangeDefaultFont(this);

        }

        private void Button2_Click(object sender, EventArgs e)
        {

            Functions.CreateTransitionObject(CurrentFlowsheet, "", "Costing", "", "", default);

            Functions.DisplayTransitionForm(CurrentFlowsheet, "Costing");

        }

    }
}