using DWSIM.Interfaces;
using System;
using System.Windows.Forms;

namespace DWSIM.ProFeatures
{

    public partial class FormBridgeToPro : Form
    {

        public IFlowsheet CurrentFlowsheet;
        public IAnalyticsProvider AnalyticsProvider;
        private bool Transitioning = false;

        public FormBridgeToPro(IAnalyticsProvider analytics, bool? skipIntro = false)
        {
            InitializeComponent();
            AnalyticsProvider = analytics;
            formPortal.AnalyticsProvider = analytics;
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

            AnalyticsProvider?.RegisterEvent("Portal Window: User Clicked 'Switch to Pro'", lblFeature.Text, null);

            Transitioning = true;
            SwitchToFormPortal();

            // Functions.ProcessTransition(CurrentFlowsheet);

            //Close();

        }

        private void FormBridgeToPro_FormClosing(object sender, System.Windows.Forms.FormClosingEventArgs e)
        {

            if (!Transitioning && CurrentFlowsheet != null)
            {
                AnalyticsProvider?.RegisterEvent("Portal Window: Closed by User", lblFeature.Text, null);
                CurrentFlowsheet.FlowsheetOptions.FlowsheetTransitionObject = null;
            }

        }

        private void formPortal_VisibleChanged(object sender, EventArgs e)
        {

        }
    }
}