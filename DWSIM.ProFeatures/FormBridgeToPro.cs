using System;
using System.Diagnostics;
using System.Windows.Forms;
using DWSIM.Interfaces;

namespace DWSIM.ProFeatures
{

    public partial class FormBridgeToPro:Form
    {

        public static Action<IFlowsheet> TransitionAction;
        public IFlowsheet CurrentFlowsheet;
        private FormPortal _formPortal;

        public FormBridgeToPro()
        {
            InitializeComponent();
            lblFeature = _lblFeature;
            _lblFeature.Name = "lblFeature";
        }

        private void FormBridgeToPro_Load(object sender, EventArgs e)
        {

            ExtensionMethods.FormExtensions.ChangeDefaultFont(this);

        }

        private void Button1_Click(object sender, EventArgs e)
        {

            Close();

        }

        private void Button2_Click(object sender, EventArgs e)
        {
            _formPortal = new FormPortal(CurrentFlowsheet);
            _formPortal.Show();
            // TransitionAction?.Invoke(CurrentFlowsheet)

           // Process.Start("https://simulate365.com/registration/");

        }

    }
}