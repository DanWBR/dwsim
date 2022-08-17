using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using Cudafy;
using Cudafy.Host;
using DWSIM.UI.Shared;
using Eto.Forms;
using Settings = DWSIM.GlobalSettings.Settings;

namespace DWSIM.UI.Forms.Forms
{
    class GeneralSettings
    {

        bool loaded = false;

        public Form GetForm()
        {

            string prefix = this.GetLocalizationPrefix();

            var tab1 = DWSIM.UI.Shared.Common.GetDefaultContainer();
            tab1.Tag = "Interface";

            if (Application.Instance.Platform.IsMac)
            {
                tab1.CreateAndAddLabelRow("TouchBar");

                tab1.CreateAndAddCheckBoxRow("Enable Custom TouchBar Buttons", Settings.EnableCustomTouchBar, (CheckBox sender, EventArgs obj) => { Settings.EnableCustomTouchBar = sender.Checked.Value; });
                tab1.CreateAndAddDescriptionRow("Enables/disables custom TouchBar buttons on supported MacBook Pro models + macOS versions.");
            }

            tab1.CreateAndAddLabelRow("Renderer");

            int currentrenderer = 0;
            var renderers = new List<String>();

            switch (GlobalSettings.Settings.RunningPlatform())
            {
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

            //tab1.CreateAndAddDropDownRow("Platform Renderer", renderers, currentrenderer, (sender, e) =>
            //{
            //    switch (GlobalSettings.Settings.RunningPlatform())
            //    {
            //        case Settings.Platform.Windows:
            //            Settings.WindowsRenderer = (Settings.WindowsPlatformRenderer)sender.SelectedIndex;
            //            break;
            //        case Settings.Platform.Linux:
            //            Settings.LinuxRenderer = (Settings.LinuxPlatformRenderer)sender.SelectedIndex;
            //            break;
            //        case Settings.Platform.Mac:
            //            Settings.MacOSRenderer = (Settings.MacOSPlatformRenderer)sender.SelectedIndex;
            //            break;
            //    }
            //});

            //tab1.CreateAndAddDescriptionRow("This sets the GUI Renderer for the current platform. Recommended renderers for each platform are:\nWindows: WPF (Windows Presentation Foundation)\nLinux: GTK 2\nmacOS: MonoMac");
            //tab1.CreateAndAddDescriptionRow("Changes to this setting will have effect upon application restart.");

            //if (Settings.RunningPlatform() == Settings.Platform.Mac)
            //{
            //    var check1 = tab1.CreateAndAddCheckBoxRow("Enable Dark Mode (macOS Mojave only)", Settings.DarkMode, (CheckBox sender, EventArgs obj) => { Settings.DarkMode = sender.Checked.Value; });
            //    check1.Enabled = false;
            //}
            
            tab1.CreateAndAddNumericEditorRow("Scaling Factor", Settings.UIScalingFactor, 0.2, 3.0, 2, (sender, e) => Settings.UIScalingFactor = sender.Value);
            tab1.CreateAndAddDescriptionRow("Sets the Scaling Factor for controls (windows, panels, buttons, lists, etc). Useful on Linux when used in conjunction with Font Scaling on High DPI displays.");

            tab1.CreateAndAddLabelRow("Flowsheet Designer");

            tab1.CreateAndAddDropDownRow("Flowsheet Renderer", new List<string>() { "Software (CPU)", "Hardware (OpenGL)" }, (int)Settings.FlowsheetRenderer, (sender, e) =>
             {
                 Settings.FlowsheetRenderer = (Settings.SkiaCanvasRenderer)sender.SelectedIndex;
             });

            tab1.CreateAndAddCheckBoxRow("EnableAntiAliasing".Localize(prefix), Settings.DrawingAntiAlias, (CheckBox sender, EventArgs obj) => { Settings.DrawingAntiAlias = sender.Checked.Value; });
            tab1.CreateAndAddDescriptionRow("Sets anti-aliasing (edge smoothing) for the Flowsheet Designer.");

            tab1.CreateAndAddLabelRow("Editors");

            var sizes = new[] { "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16" };

            if (Settings.EditorFontSize == -1) Settings.EditorFontSize = (int)(new Eto.Drawing.Font(Eto.Drawing.SystemFont.Label).Size);

            tab1.CreateAndAddDropDownRow("Font Size (Editor Labels/Descriptions)", sizes.ToList(), sizes.ToList().IndexOf(Settings.EditorFontSize.ToString()), (DropDown sender, EventArgs obj) => { Settings.EditorFontSize = int.Parse(sender.SelectedValue.ToString()); });
            tab1.CreateAndAddDescriptionRow("Sets the Font Size for Editor labels and descriptions.");

            tab1.CreateAndAddDropDownRow("Font Size (Text Reports)", sizes.ToList(), sizes.ToList().IndexOf(Settings.ResultsReportFontSize.ToString()), (DropDown sender, EventArgs obj) => { Settings.ResultsReportFontSize = int.Parse(sender.SelectedValue.ToString()); });
            tab1.CreateAndAddDescriptionRow("Sets the Font Size for text reports.");

            tab1.CreateAndAddCheckBoxRow("Fix Size of Input Controls", Settings.EditorTextBoxFixedSize, (CheckBox sender, EventArgs obj) => { Settings.EditorTextBoxFixedSize = sender.Checked.Value; });
            tab1.CreateAndAddDescriptionRow("Fix the size of input textboxes and checkboxes on editors.");

            tab1.CreateAndAddCheckBoxRow("View/Open Object Editor After Selection", Settings.EditOnSelect, (CheckBox sender, EventArgs obj) => { Settings.EditOnSelect = sender.Checked.Value; });
            tab1.CreateAndAddDescriptionRow("Opens the object editor after selection on the flowsheet.");

            tab1.CreateAndAddCheckBoxRow("Call Solver on Editor Property Update", Settings.CallSolverOnEditorPropertyChanged, (CheckBox sender, EventArgs obj) => { Settings.CallSolverOnEditorPropertyChanged = sender.Checked.Value; });
            tab1.CreateAndAddDescriptionRow("Requests a flowsheet calculation after an object property is changed/updated on the editor.");

            CheckBox chkInsp = null, chkCPUP = null;

            var tab2a = DWSIM.UI.Shared.Common.GetDefaultContainer();
            tab2a.Tag = "Inspector";

            chkInsp = tab2a.CreateAndAddCheckBoxRow("Enable Inspector Reports", Settings.InspectorEnabled, (CheckBox sender, EventArgs obj) => {
                Settings.InspectorEnabled = sender.Checked.GetValueOrDefault();
                Settings.EnableParallelProcessing = !Settings.InspectorEnabled;
                chkCPUP.Checked = !sender.Checked.GetValueOrDefault();
            });
            tab2a.CreateAndAddDescriptionRow("Enabling Inspector Reports will create model description and performance reports on-the-fly as the calculations are requested by the Flowsheet Solver. Use the Solution Inspector tool to view these reports.");
            tab2a.CreateAndAddDescriptionRow("When the Inspector Reports feature is enabled and the Flowsheet Solver is called, the Parallel CPU Processing is automatically disabled. You must re-enable it manually and disable the Inspector to increase the calculation speed again.");
            tab2a.CreateAndAddCheckBoxRow("Clear Previous Reports on new Flowsheet Calculation Request", Settings.ClearInspectorHistoryOnNewCalculationRequest, (CheckBox sender, EventArgs obj) => { Settings.ClearInspectorHistoryOnNewCalculationRequest = sender.Checked.GetValueOrDefault(); });
            tab2a.CreateAndAddDescriptionRow("This will erase all previously stored reports when a new flowsheet calculation request is made by the user.");

            var tab2 = DWSIM.UI.Shared.Common.GetDefaultContainer();
            tab2.Tag = "Solver".Localize(prefix);

            tab2.CreateAndAddTextBoxRow("N0", "Solver Timeout (s)", GlobalSettings.Settings.SolverTimeoutSeconds, (sender, e) =>
            {
                if (sender.Text.IsValidDouble()) GlobalSettings.Settings.SolverTimeoutSeconds = Convert.ToInt32(sender.Text);
            });
            tab2.CreateAndAddDescriptionRow("Set the solver's maximum calculation (waiting) time.");

            chkCPUP = tab2.CreateAndAddCheckBoxRow("EnableCPUParallelProcessing".Localize(prefix), Settings.EnableParallelProcessing, (CheckBox sender, EventArgs obj) => { 
                Settings.EnableParallelProcessing = sender.Checked.GetValueOrDefault();
                Settings.InspectorEnabled = !Settings.EnableParallelProcessing;
                chkInsp.Checked = !sender.Checked.GetValueOrDefault();
            });
            tab2.CreateAndAddDescriptionRow("Enables utilization of all CPU cores during flowsheet calculations.");
            tab2.CreateAndAddCheckBoxRow("EnableCPUSIMDAccel".Localize(prefix), Settings.UseSIMDExtensions, (CheckBox sender, EventArgs obj) => { Settings.UseSIMDExtensions = sender.Checked.GetValueOrDefault(); });
            tab2.CreateAndAddDescriptionRow("Enables utilization of special CPU instructions for accelerated math calculations.");

            tab2.CreateAndAddCheckBoxRow("BreakOnException".Localize(prefix), Settings.SolverBreakOnException, (CheckBox sender, EventArgs obj) => { Settings.SolverBreakOnException = sender.Checked.GetValueOrDefault(); });
            tab2.CreateAndAddDescriptionRow("If activated, the solver won't calculate the rest of the flowsheet if an error occurs during the calculation of an intermediate block/object.");
            tab2.CreateAndAddCheckBoxRow("EnableGPUAccel".Localize(prefix), Settings.EnableGPUProcessing, (CheckBox sender, EventArgs obj) => { Settings.EnableGPUProcessing = sender.Checked.Value; });
            TextArea tbgpucaps = null;
            var cbgpu = tab2.CreateAndAddDropDownRow("Computing Device", new List<string>(), 0, (sender, e) =>
            {
                if (!(sender.SelectedValue == null))
                {
                    if (sender.SelectedValue.ToString().Contains("Emulator"))
                    {
                        Settings.CudafyTarget = (int)eGPUType.Emulator;
                    }
                    else if (sender.SelectedValue.ToString().Contains("CUDA"))
                    {
                        Settings.CudafyTarget = (int)eGPUType.Cuda;
                    }
                    else
                    {
                        Settings.CudafyTarget = (int)eGPUType.OpenCL;
                    }

                    Settings.CudafyTarget = Settings.CudafyTarget;
                    try
                    {
                        foreach (GPGPUProperties prop in CudafyHost.GetDeviceProperties((eGPUType)Settings.CudafyTarget, false))
                        {
                            if (sender.SelectedValue.ToString().Split('|')[1].Contains(prop.Name))
                            {
                                Settings.SelectedGPU = sender.SelectedValue.ToString();
                                Settings.CudafyDeviceID = prop.DeviceId;
                                Application.Instance.Invoke(() => GetCUDACaps(prop, tbgpucaps));
                                break;
                            }

                        }
                    }
                    catch (Exception)
                    {
                    }
                    if (loaded)
                    {
                        if (Settings.gpu != null) Settings.gpu.Dispose();
                        Settings.gpu = null;
                        try
                        {
                            //set CUDA params
                            CudafyModes.Compiler = eGPUCompiler.All;
                            CudafyModes.Target = (eGPUType)Settings.CudafyTarget;
                            Cudafy.Translator.CudafyTranslator.GenerateDebug = false;
                            DWSIM.Thermodynamics.Calculator.InitComputeDevice();
                            Console.WriteLine("GPU initialized successfully: " + Settings.SelectedGPU + "(" + CudafyModes.Target.ToString() + ")");
                        }
                        catch (Exception ex)
                        {
                            Console.WriteLine("GPU initialization failed: " + ex.ToString());
                            var ex1 = ex;
                            while (ex1.InnerException != null)
                            {
                                Console.WriteLine("GPU initialization failed (IEX): " + ex1.InnerException.ToString());
                                if (ex1.InnerException is ReflectionTypeLoadException)
                                {
                                    foreach (var tlex in ((ReflectionTypeLoadException)(ex1.InnerException)).LoaderExceptions)
                                    { Console.WriteLine("GPU initialization failed (TLEX): " + tlex.Message); }
                                }
                                ex1 = ex1.InnerException;
                            }
                        }
                    }
                }
            });
            tbgpucaps = tab2.CreateAndAddMultilineMonoSpaceTextBoxRow("", 200, true, null);

            Task.Factory.StartNew(() =>
            {
                List<string> list = new List<string>();
                try
                {
                    CudafyModes.Target = eGPUType.Cuda;
                    foreach (GPGPUProperties prop in CudafyHost.GetDeviceProperties(CudafyModes.Target, false))
                        list.Add("CUDA | " + prop.Name + " (" + prop.DeviceId + ")");
                }
                catch (Exception)
                {
                }
                try
                {
                    CudafyModes.Target = eGPUType.OpenCL;
                    foreach (GPGPUProperties prop in CudafyHost.GetDeviceProperties(CudafyModes.Target, false))
                        list.Add("OpenCL | " + prop.Name + " (" + prop.DeviceId + ")");
                }
                catch (Exception)
                {
                }
                return list;
            }).ContinueWith(t =>
            {
                foreach (var item in t.Result)
                {
                    cbgpu.Items.Add(item);
                }
                CudafyModes.Target = (eGPUType)Settings.CudafyTarget;
                if (Settings.SelectedGPU != "")
                {
                    foreach (var s in cbgpu.Items)
                    {
                        if (s.Text == Settings.SelectedGPU)
                        {
                            cbgpu.SelectedValue = s;
                            break;
                        }
                    }
                }
                else
                {
                }
                loaded = true;
            }, TaskScheduler.FromCurrentSynchronizationContext());

            var tab3 = DWSIM.UI.Shared.Common.GetDefaultContainer();
            tab3.Tag = "UserComps".Localize(prefix);

            tab3.CreateAndAddLabelRow("User-Defined Compound Datasets");
            var list1 = tab3.CreateAndAddListBoxRow(200, GlobalSettings.Settings.UserDatabases.ToArray(), null);
            tab3.CreateAndAddButtonRow("Add Dataset", null, (sender, e) =>
            {
                var dialog = new OpenFileDialog();
                dialog.Title = "Select File".Localize();
                dialog.Filters.Add(new FileFilter("Compound dataset files", new[] { ".xml", ".json" }));
                dialog.MultiSelect = false;
                dialog.CurrentFilterIndex = 0;
                if (dialog.ShowDialog(null) == DialogResult.Ok)
                {
                    if (!GlobalSettings.Settings.UserDatabases.Contains(dialog.FileName))
                    {
                        GlobalSettings.Settings.UserDatabases.Add(dialog.FileName);
                        list1.Items.Add(dialog.FileName);
                    }
                }
            });
            tab3.CreateAndAddButtonRow("Remove selected", null, (sender, e) =>
            {
                GlobalSettings.Settings.UserDatabases.Remove(list1.SelectedValue.ToString());
                list1.Items.RemoveAt(list1.SelectedIndex);
            });

            tab3.CreateAndAddLabelRow("User-Defined Interaction Parameter Datasets");
            var list2 = tab3.CreateAndAddListBoxRow(200, GlobalSettings.Settings.UserInteractionsDatabases.ToArray(), null);
            tab3.CreateAndAddButtonRow("Add Dataset", null, (sender, e) =>
            {
                var dialog = new OpenFileDialog();
                dialog.Title = "Select File".Localize();
                dialog.Filters.Add(new FileFilter("Interaction Parameter dataset files", new[] { ".xml" }));
                dialog.MultiSelect = false;
                dialog.CurrentFilterIndex = 0;
                if (dialog.ShowDialog(null) == DialogResult.Ok)
                {
                    if (!GlobalSettings.Settings.UserInteractionsDatabases.Contains(dialog.FileName))
                    {
                        GlobalSettings.Settings.UserInteractionsDatabases.Add(dialog.FileName);
                        list2.Items.Add(dialog.FileName);
                    }
                }
            });
            tab3.CreateAndAddButtonRow("Remove selected", null, (sender, e) =>
            {
                GlobalSettings.Settings.UserInteractionsDatabases.Remove(list2.SelectedValue.ToString());
                list2.Items.RemoveAt(list2.SelectedIndex);
            });

            var tab4 = DWSIM.UI.Shared.Common.GetDefaultContainer();
            tab4.Tag = "Backup".Localize(prefix);

            tab4.CreateAndAddLabelRow("BackupCopies".Localize(prefix));
            tab4.CreateAndAddCheckBoxRow("EnableBackupCopies".Localize(prefix), Settings.EnableBackupCopies, (CheckBox sender, EventArgs obj) => { Settings.EnableBackupCopies = sender.Checked.Value; });
            tab4.CreateAndAddEmptySpace();
            tab4.CreateAndAddTextBoxRow("N0", "BackupInterval".Localize(prefix), Settings.BackupInterval, (TextBox sender, EventArgs obj) => { Settings.BackupInterval = sender.Text.IsValidDouble() ? (int)sender.Text.ToDouble() : Settings.BackupInterval; });
            tab4.CreateAndAddEmptySpace();
            tab4.CreateAndAddButtonRow("PurgeBackupFolder".Localize(prefix), null, (sender, e) =>
            {
                string backupdir = "";
                if (GlobalSettings.Settings.RunningPlatform() == GlobalSettings.Settings.Platform.Mac)
                {
                    backupdir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Personal), "Documents", "DWSIM Application Data", "Backup") + Path.DirectorySeparatorChar;
                }
                else
                {
                    backupdir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "DWSIM Application Data", "Backup") + Path.DirectorySeparatorChar;
                }
                var files = Directory.GetFiles(backupdir);
                foreach (var file in files)
                {
                    try
                    {
                        File.Delete(file);
                    }
                    catch { }
                }
            });
            tab4.CreateAndAddEmptySpace();

            var tab5 = DWSIM.UI.Shared.Common.GetDefaultContainer();

            tab5.Tag = "Misc".Localize(prefix);
            tab5.CreateAndAddLabelRow("Updates");
            tab5.CreateAndAddCheckBoxRow("Check for Updates", Settings.CheckForUpdates, (chk, e) => Settings.CheckForUpdates = chk.Checked.GetValueOrDefault());

            tab5.CreateAndAddLabelRow("Python Settings");
            tab5.CreateAndAddDescriptionRow("Setup the path for Python binaries to enable integration with DWSIM.");
            TextBox tbox2 = null;
            tbox2 = tab5.CreateAndAddLabelAndTextBoxAndButtonRow("Binaries Path", GlobalSettings.Settings.PythonPath, "Search", null,
                (sender, e) => GlobalSettings.Settings.PythonPath = sender.Text,
                (sender, e) =>
                {
                    var searchdialog = new SelectFolderDialog() { Title = "Search" };
                    if (searchdialog.ShowDialog(tab5) == DialogResult.Ok)
                    {
                        tbox2.Text = searchdialog.Directory;
                    }
                });
            tab5.CreateAndAddTextBoxRow("N0", "Calling Timeout (minutes)", GlobalSettings.Settings.PythonTimeoutInMinutes, (sender, e) =>
            {
                if (sender.Text.IsValidDouble()) GlobalSettings.Settings.PythonTimeoutInMinutes = sender.Text.ToDouble();
            });

            var form =  DWSIM.UI.Shared.Common.GetDefaultTabbedForm("Title".Localize(prefix), 700, 550, new[] { tab1, tab2, tab2a, tab3, tab4, tab5 });

            form.Closed += (s, e) => {
                try
                {
                    DWSIM.GlobalSettings.Settings.SaveSettings("dwsim_newui.ini");
                }
                catch
                {
                }
            };

            return form;

        }

        public void GetCUDACaps(GPGPUProperties prop, TextArea tbGPUCaps)
        {
            int i = 0;
            tbGPUCaps.Text = "";
            tbGPUCaps.Text += ((string.Format("   --- General Information for device {0} ---", i) + "\r\n"));
            tbGPUCaps.Text += ((string.Format("Name:  {0}", prop.Name) + "\r\n"));
            tbGPUCaps.Text += ((string.Format("Device Id:  {0}", prop.DeviceId) + "\r\n"));
            tbGPUCaps.Text += ((string.Format("Compute capability:  {0}.{1}", prop.Capability.Major, prop.Capability.Minor) + "\r\n"));
            tbGPUCaps.Text += ((string.Format("Clock rate: {0}", prop.ClockRate) + "\r\n"));
            tbGPUCaps.Text += ((string.Format("Simulated: {0}", prop.IsSimulated) + "\r\n"));
            tbGPUCaps.Text += ((string.Format("   --- Memory Information for device {0} ---", i) + "\r\n"));
            tbGPUCaps.Text += ((string.Format("Total global mem:  {0}", prop.TotalMemory) + "\r\n"));
            tbGPUCaps.Text += ((string.Format("Total constant Mem:  {0}", prop.TotalConstantMemory) + "\r\n"));
            tbGPUCaps.Text += ((string.Format("Max mem pitch:  {0}", prop.MemoryPitch) + "\r\n"));
            tbGPUCaps.Text += ((string.Format("Texture Alignment:  {0}", prop.TextureAlignment) + "\r\n"));
            tbGPUCaps.Text += ((string.Format("   --- MP Information for device {0} ---", i) + "\r\n"));
            tbGPUCaps.Text += ((string.Format("Shared mem per mp: {0}", prop.SharedMemoryPerBlock) + "\r\n"));
            tbGPUCaps.Text += ((string.Format("Registers per mp:  {0}", prop.RegistersPerBlock) + "\r\n"));
            tbGPUCaps.Text += ((string.Format("Threads in warp:  {0}", prop.WarpSize) + "\r\n"));
            tbGPUCaps.Text += ((string.Format("Max threads per block:  {0}", prop.MaxThreadsPerBlock) + "\r\n"));
            tbGPUCaps.Text += ((string.Format("Max thread dimensions:  ({0}, {1}, {2})", prop.MaxThreadsSize.x, prop.MaxThreadsSize.y, prop.MaxThreadsSize.z) + "\r\n"));
            tbGPUCaps.Text += ((string.Format("Max grid dimensions:  ({0}, {1}, {2})", prop.MaxGridSize.x, prop.MaxGridSize.y, prop.MaxGridSize.z) + "\r\n"));
            tbGPUCaps.CaretIndex = 0;
            tbGPUCaps.SelectedText = "";
        }


    }
}
