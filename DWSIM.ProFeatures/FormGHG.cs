using System;

namespace DWSIM.ProFeatures
{

    public partial class FormGHG : WeifenLuo.WinFormsUI.Docking.DockContent
    {
        public FormGHG()
        {
            InitializeComponent();
            lblFeature = _lblFeature;
            _lblFeature.Name = "lblFeature";
        }

        private void FormGHG_Load(object sender, EventArgs e)
        {

            ExtensionMethods.FormExtensions.ChangeDefaultFont(this);

        }

    }
}