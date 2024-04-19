using DWSIM.Interfaces;
using System;

namespace DWSIM.ProFeatures
{


    public partial class FormCosting : WeifenLuo.WinFormsUI.Docking.DockContent
    {

        public IFlowsheet CurrentFlowsheet;

        public IAnalyticsProvider AnalyticsProvider;

        public FormCosting()
        {
            InitializeComponent();
            lblFeature = _lblFeature;
            _lblFeature.Name = "lblFeature";
        }

        private void Button2_Click(object sender, EventArgs e)
        {

            Functions.CreateTransitionObject(CurrentFlowsheet, "", "Costing", "", "", default);

            Functions.DisplayTransitionForm(AnalyticsProvider, CurrentFlowsheet, "Costing");

        }

        private void FormCosting_Load_1(object sender, EventArgs e)
        {
            ExtensionMethods.FormExtensions.ChangeDefaultFont(this);
        }
    }
}