using System;
using System.Diagnostics;

namespace DWSIM.ProFeatures
{

    public partial class FormCosting : WeifenLuo.WinFormsUI.Docking.DockContent
    {
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
            Process.Start("https://simulate365.com/registration/");
        }

    }
}