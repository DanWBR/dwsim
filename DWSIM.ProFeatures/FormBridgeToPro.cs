using DWSIM.Interfaces;
using System;
using System.Windows.Forms;

namespace DWSIM.ProFeatures
{

    public partial class FormBridgeToPro : Form
    {

        public IFlowsheet CurrentFlowsheet;
        private bool Transitioning = false;

        public FormBridgeToPro(bool? skipIntro = false)
        {
            InitializeComponent();
            lblFeature = _lblFeature;
            _lblFeature.Name = "lblFeature";

            if (skipIntro.HasValue && skipIntro.Value)
            {
                SwitchToFormPortal();
            }
        }

        private void FormBridgeToPro_Load(object sender, EventArgs e)
        {
            ExtensionMethods.FormExtensions.ChangeDefaultFont(this);
        }

        private void Button1_Click(object sender, EventArgs e)
        {
            CurrentFlowsheet.FlowsheetOptions.FlowsheetTransitionObject = null;

            Close();
        }

        private void SwitchToFormPortal()
        {
            BridgeToProPanel.Visible = false;
            formPortal.Visible = true;
            formPortal.SetFlowsheet(CurrentFlowsheet);
            formPortal.OnInitialize();
        }

        private void Button2_Click(object sender, EventArgs e)
        {

            Transitioning = true;
            SwitchToFormPortal();

            // Functions.ProcessTransition(CurrentFlowsheet);

            //Close();

        }

        private void FormBridgeToPro_FormClosing(object sender, System.Windows.Forms.FormClosingEventArgs e)
        {

            if (!Transitioning && CurrentFlowsheet != null)
                CurrentFlowsheet.FlowsheetOptions.FlowsheetTransitionObject = null;

        }

        private void formPortal_VisibleChanged(object sender, EventArgs e)
        {

        }
    }
}