using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.UI.Shared;
using Eto.Forms;
using Settings = DWSIM.GlobalSettings.Settings;

namespace DWSIM.UI.Forms.Forms
{
    class GeneralSettings
    {

        public Form GetForm()
        {

            string prefix = this.GetLocalizationPrefix();

            var tab1 = Common.GetDefaultContainer();
            tab1.Tag = "Flowsheet".Localize(prefix);

            tab1.CreateAndAddLabelRow("Designer".Localize(prefix));
            tab1.CreateAndAddCheckBoxRow("EnableAntiAliasing".Localize(prefix), Settings.DrawingAntiAlias, (CheckBox sender, EventArgs obj) => { Settings.DrawingAntiAlias = sender.Checked.Value; });

            var tab2 = Common.GetDefaultContainer();
            tab2.Tag = "Solver".Localize(prefix);

            tab2.CreateAndAddLabelRow("ControlOptions".Localize(prefix));
            tab2.CreateAndAddCheckBoxRow("EnableCPUParallelProcessing".Localize(prefix), Settings.EnableParallelProcessing, (CheckBox sender, EventArgs obj) => { Settings.EnableParallelProcessing = sender.Checked.Value; });
            tab2.CreateAndAddEmptySpace();
            tab2.CreateAndAddCheckBoxRow("EnableCPUSIMDAccel".Localize(prefix), Settings.UseSIMDExtensions, (CheckBox sender, EventArgs obj) => { Settings.UseSIMDExtensions = sender.Checked.Value; });
            tab2.CreateAndAddEmptySpace();
            tab2.CreateAndAddCheckBoxRow("EnableGPUAccel".Localize(prefix), Settings.EnableGPUProcessing, (CheckBox sender, EventArgs obj) => { Settings.EnableGPUProcessing = sender.Checked.Value; });
            tab2.CreateAndAddEmptySpace();
            tab2.CreateAndAddCheckBoxRow("BreakOnException".Localize(prefix), Settings.SolverBreakOnException, (CheckBox sender, EventArgs obj) => { Settings.SolverBreakOnException = sender.Checked.Value; });
            tab2.CreateAndAddEmptySpace();

            var tab3 = Common.GetDefaultContainer();
            tab3.Tag = "UserComps".Localize(prefix);
            
            var tab4 = Common.GetDefaultContainer();
            tab4.Tag = "Backup".Localize(prefix);

            tab4.CreateAndAddLabelRow("BackupCopies".Localize(prefix));
            tab4.CreateAndAddCheckBoxRow("EnableBackupCopies".Localize(prefix), Settings.EnableBackupCopies, (CheckBox sender, EventArgs obj) => { Settings.EnableBackupCopies = sender.Checked.Value; });
            tab4.CreateAndAddEmptySpace();
            tab4.CreateAndAddTextBoxRow("N0", "BackupInterval".Localize(prefix), Settings.BackupInterval, (TextBox sender, EventArgs obj) => { Settings.BackupInterval = sender.Text.IsValidDouble() ? (int)sender.Text.ToDouble() : Settings.BackupInterval; });
            tab4.CreateAndAddEmptySpace();
            tab4.CreateAndAddButtonRow("PurgeBackupFolder".Localize(prefix), null, null);
            tab4.CreateAndAddEmptySpace();

            var tab5 = Common.GetDefaultContainer();
            tab5.Tag = "Misc".Localize(prefix);

            return Common.GetDefaultTabbedForm("Title".Localize(prefix), 500, 400, new[] { tab1, tab2, tab3, tab4, tab5 });
        
        }


    }
}
