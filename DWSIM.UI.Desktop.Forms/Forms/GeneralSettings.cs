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
            tab1.Tag = "User Interface";

            tab1.CreateAndAddLabelRow("Renderer");

            int currentrenderer = 0;
            var renderers = new List<String>();

            switch (GlobalSettings.Settings.RunningPlatform()){
                case Settings.Platform.Windows:
                    renderers.AddRange(Enum.GetNames(typeof(Settings.WindowsPlatformRenderer)));
                    currentrenderer = (int)Settings.WindowsRenderer;
                    break;
                case Settings.Platform.Linux:
                    renderers.AddRange(Enum.GetNames(typeof(Settings.LinuxPlatformRenderer)));
                    currentrenderer = (int)Settings.LinuxRenderer;
                    break;
                case Settings.Platform.Mac:
                    renderers.AddRange(Enum.GetNames(typeof(Settings.MacOSPlatformRenderer)));
                    currentrenderer = (int)Settings.MacOSRenderer;
                    break;
            }

            tab1.CreateAndAddDropDownRow("Platform Renderer", renderers, currentrenderer, (sender, e) => {
                switch (GlobalSettings.Settings.RunningPlatform())
                {
                    case Settings.Platform.Windows:
                        Settings.WindowsRenderer = (Settings.WindowsPlatformRenderer)sender.SelectedIndex;
                        break;
                    case Settings.Platform.Linux:
                        Settings.LinuxRenderer = (Settings.LinuxPlatformRenderer)sender.SelectedIndex;
                        break;
                    case Settings.Platform.Mac:
                        Settings.MacOSRenderer = (Settings.MacOSPlatformRenderer)sender.SelectedIndex;
                        break;
                }            
            });

            tab1.CreateAndAddDescriptionRow("This sets the GUI Renderer for the current platform. Recommended renderers for each platform are:\nWindows: WPF (Windows Presentation Foundation)\nLinux: GTK 2\nmacOS: MonoMac");
            tab1.CreateAndAddDescriptionRow("Changes to this setting will have effect upon application restart.");

            tab1.CreateAndAddLabelRow("Flowsheet Designer");
            tab1.CreateAndAddCheckBoxRow("EnableAntiAliasing".Localize(prefix), Settings.DrawingAntiAlias, (CheckBox sender, EventArgs obj) => { Settings.DrawingAntiAlias = sender.Checked.Value; });
            tab1.CreateAndAddDescriptionRow("Sets anti-aliasing (edge smoothing) for the Flowsheet Designer.");

            var tab2 = Common.GetDefaultContainer();
            tab2.Tag = "Solver".Localize(prefix);

            tab2.CreateAndAddLabelRow("ControlOptions".Localize(prefix));
            tab2.CreateAndAddCheckBoxRow("EnableCPUParallelProcessing".Localize(prefix), Settings.EnableParallelProcessing, (CheckBox sender, EventArgs obj) => { Settings.EnableParallelProcessing = sender.Checked.GetValueOrDefault(); });
            tab2.CreateAndAddEmptySpace();
            tab2.CreateAndAddCheckBoxRow("EnableCPUSIMDAccel".Localize(prefix), Settings.UseSIMDExtensions, (CheckBox sender, EventArgs obj) => { Settings.UseSIMDExtensions = sender.Checked.GetValueOrDefault(); });
            tab2.CreateAndAddEmptySpace();
            //tab2.CreateAndAddCheckBoxRow("EnableGPUAccel".Localize(prefix), Settings.EnableGPUProcessing, (CheckBox sender, EventArgs obj) => { Settings.EnableGPUProcessing = sender.Checked.Value; });
            //tab2.CreateAndAddEmptySpace();
            tab2.CreateAndAddCheckBoxRow("BreakOnException".Localize(prefix), Settings.SolverBreakOnException, (CheckBox sender, EventArgs obj) => { Settings.SolverBreakOnException = sender.Checked.GetValueOrDefault(); });
            tab2.CreateAndAddEmptySpace();

            var tab3 = Common.GetDefaultContainer();
            tab3.Tag = "UserComps".Localize(prefix);

            tab3.CreateAndAddLabelRow("User-Defined Compound Datasets");
            tab3.CreateAndAddListBoxRow(200, new string[] { }, null);
            tab3.CreateAndAddButtonRow("Add Dataset", null, null);
            tab3.CreateAndAddButtonRow("Remove selected", null, null);

            tab3.CreateAndAddLabelRow("User-Defined Interaction Parameter Datasets");
            tab3.CreateAndAddListBoxRow(200, new string[] { }, null);
            tab3.CreateAndAddButtonRow("Add Dataset", null, null);
            tab3.CreateAndAddButtonRow("Remove selected", null, null);

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

            tab5.CreateAndAddLabelRow("Reports");
            var sizes = new[] {"6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16" };
            tab5.CreateAndAddDropDownRow("Font Size", sizes.ToList(), sizes.ToList().IndexOf(Settings.ResultsReportFontSize.ToString()), (DropDown sender, EventArgs obj) => { Settings.ResultsReportFontSize = int.Parse(sender.SelectedValue.ToString()); });

            tab5.CreateAndAddLabelRow("Octave Settings");
            TextBox tbox = null;
            tbox = tab5.CreateAndAddLabelAndTextBoxAndButtonRow("Binaries Path", GlobalSettings.Settings.OctavePath, "Search", null,
                (sender, e) => GlobalSettings.Settings.OctavePath = sender.Text,
                (sender, e) => {
                    var searchdialog = new SelectFolderDialog() { Title = "Search", Directory = GlobalSettings.Settings.OctavePath };
                    if (searchdialog.ShowDialog(tab5) == DialogResult.Ok)
                    {
                        tbox.Text = searchdialog.Directory;
                    }
                });
            tab5.CreateAndAddTextBoxRow("N0", "Calling Timeout (minutes)", GlobalSettings.Settings.OctaveTimeoutInMinutes, (sender, e) =>
            {
                if (sender.Text.IsValidDouble()) GlobalSettings.Settings.OctaveTimeoutInMinutes = sender.Text.ToDouble();
            });

            tab5.CreateAndAddLabelRow("Python Settings");
            TextBox tbox2 = null;
            tbox2 = tab5.CreateAndAddLabelAndTextBoxAndButtonRow("Binaries Path", GlobalSettings.Settings.PythonPath, "Search", null,
                (sender, e) => GlobalSettings.Settings.PythonPath = sender.Text,
                (sender, e) =>
                {
                    var searchdialog = new SelectFolderDialog() { Title = "Search", Directory = GlobalSettings.Settings.PythonPath };
                    if (searchdialog.ShowDialog(tab5) == DialogResult.Ok)
                    {
                        tbox2.Text = searchdialog.Directory;
                    }
                });
            tab5.CreateAndAddTextBoxRow("N0", "Calling Timeout (minutes)", GlobalSettings.Settings.PythonTimeoutInMinutes, (sender, e) => {
                if (sender.Text.IsValidDouble()) GlobalSettings.Settings.PythonTimeoutInMinutes = sender.Text.ToDouble();
            });

            return Common.GetDefaultTabbedForm("Title".Localize(prefix), 500, 400, new[] { tab1, tab2, tab3, tab4, tab5 });
        
        }


    }
}
