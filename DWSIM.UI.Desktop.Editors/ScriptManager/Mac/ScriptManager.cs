using System;
using Eto.Forms;
using Eto.Drawing;
using DWSIM.Interfaces;
using s = DWSIM.UI.Shared.Common;
using DWSIM.Interfaces.Enums;

namespace DWSIM.UI.Desktop.Editors
{
    public class ScriptManager_Mac : ScriptManagerBase
    {

        string imgprefix = "DWSIM.UI.Desktop.Editors.Resources.Icons.";

        private DWSIM.UI.Desktop.Shared.Flowsheet Flowsheet;

        private ListBox lbScripts;
        private ScriptItem ScriptEditor;

        private bool adding = false;

        private DWSIM.Interfaces.IScript selscript;

        public ScriptManager_Mac(DWSIM.UI.Desktop.Shared.Flowsheet fs)
            : base()
        {
            Flowsheet = fs;
            Init();
        }

        void Init()
        {

            var ti1 = new ButtonSegmentedItem() { ToolTip = "New Script", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-new.png")) };
            var ti2 = new ButtonSegmentedItem() { ToolTip = "Update Selected", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-approve_and_update.png")) };
            var ti3 = new ButtonSegmentedItem() { ToolTip = "Print", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-print.png")) };
            var ti3a = new ButtonSegmentedItem() { ToolTip = "Remove Selected", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-filled_trash.png")) };

            var ts1 = new SegmentedButton { Height = 20, Items = { ti1, ti2, ti3, ti3a } };

            var ti4 = new ButtonSegmentedItem() { ToolTip = "Cut", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-cut.png")) };
            var ti5 = new ButtonSegmentedItem() { ToolTip = "Copy", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-copy.png")) };
            var ti6 = new ButtonSegmentedItem() { ToolTip = "Paste", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-paste.png")) };

            var ts2 = new SegmentedButton { Height = 20, Items = { ti4, ti5, ti6 } };

            var ti7 = new ButtonSegmentedItem() { ToolTip = "Undo", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-undo.png")) };
            var ti8 = new ButtonSegmentedItem() { ToolTip = "Redo", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-redo.png")) };

            var ts3 = new SegmentedButton { Height = 20, Items = { ti7, ti8 } };

            var ti9 = new Button() { Height = 20, Width = 20, ImagePosition = ButtonImagePosition.Overlay, Text = "", ToolTip = "Toggle Comment/Uncomment Selected Lines", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-code.png")) };

            var ti10 = new ButtonSegmentedItem() { ToolTip = "Indent Right", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "indent-right.png")) };
            var ti11 = new ButtonSegmentedItem() { ToolTip = "Indent Left", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "left-indentation-option.png")) };

            var ts4 = new SegmentedButton { Height = 20, Items = { ti10, ti11 } };

            var ti12 = new ButtonSegmentedItem() { ToolTip = "Decrease Font Size", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-decrease_font.png")) };
            var ti13 = new ButtonSegmentedItem() { ToolTip = "Increase Font Size", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-increase_font.png")) };

            var ts5 = new SegmentedButton {Height = 20, Items = { ti12, ti13 } };

            var ti14 = new Button() { Height = 20, Width = 20, ImagePosition = ButtonImagePosition.Overlay, Text = "", ToolTip = "Insert Snippet", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "code.png")) };

            var ti15 = new ButtonSegmentedItem() { ToolTip = "Run Script", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-play.png")) };
            var ti16 = new ButtonSegmentedItem() { ToolTip = "Run Script (Async)", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-circled_play.png")) };

            var ts6 = new SegmentedButton { Height = 20, Items = { ti15, ti16 } };

            var ti17 = new Button() { Height = 20, Width = 20, ImagePosition = ButtonImagePosition.Overlay, Text = "", ToolTip = "Help", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-help.png")) };

            var menu1 = new StackLayout
            {
                Items = {ts1},
                Orientation = Orientation.Horizontal,
                Spacing = 4,
                HorizontalContentAlignment = HorizontalAlignment.Stretch,
                Padding = 5
            };

            var menu2 = new StackLayout
            {
                Items = { ts6, ts2, ts3, ti9, ts4, ts5, ti14, ti17 },
                Orientation = Orientation.Horizontal,
                Spacing = 4,
                HorizontalContentAlignment = HorizontalAlignment.Stretch,
                Padding = 5
            };

            var leftcontainer = new TableLayout();
            var rightcontainer = new TableLayout();

            ScriptEditor = new ScriptItem(Flowsheet);

            ti1.Click += (sender, e) =>
            {
                var script = new DWSIM.FlowsheetSolver.Script { ID = Guid.NewGuid().ToString(), Title = "Script" + (Flowsheet.Scripts.Count + 1).ToString() };
                Flowsheet.Scripts.Add(script.ID, script);
                lbScripts.Items.Add(new ListItem { Key = script.ID, Text = script.Title });
            };

            lbScripts = new ListBox();

            lbScripts.SelectedIndexChanged += (sender, e) =>
            {
                try
                {
                    if (lbScripts.SelectedIndex < 0) return;

                    if (!Application.Instance.Platform.IsWpf)
                    {
                        if (selscript != null) selscript.ScriptText = ScriptEditor.txtScript.ScriptText;
                    }
                    else
                    {
                        if (selscript != null) selscript.ScriptText = ScriptEditor.txtScript.Text;
                    }

                    selscript = Flowsheet.Scripts[lbScripts.SelectedKey];

                    adding = true;

                    ScriptEditor.cbLinkedObject.Items.Clear();

                    ScriptEditor.cbLinkedObject.Items.Add(new ListItem { Text = "Simulation", Key = "Simulation" });
                    ScriptEditor.cbLinkedObject.Items.Add(new ListItem { Text = "Solver", Key = "Solver" });

                    foreach (var obj in Flowsheet.SimulationObjects.Values)
                    {
                        ScriptEditor.cbLinkedObject.Items.Add(new ListItem { Text = obj.GraphicObject.Tag, Key = obj.Name });
                    }

                    adding = false;

                    //ScriptEditor.txtName.Text = selscript.Title;
                    if (!Application.Instance.Platform.IsWpf)
                    {
                        ScriptEditor.txtScript.ScriptText = selscript.ScriptText;
                    }
                    else
                    {
                        ScriptEditor.txtScript.Text = selscript.ScriptText;
                    }

                    ScriptEditor.chkLink.Checked = selscript.Linked;

                    if (!string.IsNullOrEmpty(selscript.LinkedObjectName))
                    {
                        ScriptEditor.cbLinkedObject.SelectedKey = Flowsheet.SimulationObjects[selscript.LinkedObjectName].Name;
                    }
                    else
                    {
                        switch (selscript.LinkedObjectType)
                        {
                            case Scripts.ObjectType.Simulation:
                                ScriptEditor.cbLinkedObject.SelectedIndex = 0;
                                break;
                            case Scripts.ObjectType.Solver:
                                ScriptEditor.cbLinkedObject.SelectedIndex = 1;
                                break;
                        }
                    }

                    switch (selscript.LinkedEventType)
                    {
                        case Scripts.EventType.ObjectCalculationStarted:
                            ScriptEditor.cbLinkedEvent.SelectedIndex = 0;
                            break;
                        case Scripts.EventType.ObjectCalculationFinished:
                            ScriptEditor.cbLinkedEvent.SelectedIndex = 1;
                            break;
                        case Scripts.EventType.ObjectCalculationError:
                            ScriptEditor.cbLinkedEvent.SelectedIndex = 2;
                            break;
                        case Scripts.EventType.SimulationOpened:
                            ScriptEditor.cbLinkedEvent.SelectedIndex = 0;
                            break;
                        case Scripts.EventType.SimulationSaved:
                            ScriptEditor.cbLinkedEvent.SelectedIndex = 1;
                            break;
                        case Scripts.EventType.SimulationClosed:
                            ScriptEditor.cbLinkedEvent.SelectedIndex = 2;
                            break;
                        case Scripts.EventType.SolverStarted:
                            ScriptEditor.cbLinkedEvent.SelectedIndex = 0;
                            break;
                        case Scripts.EventType.SolverFinished:
                            ScriptEditor.cbLinkedEvent.SelectedIndex = 1;
                            break;
                        case Scripts.EventType.SolverRecycleLoop:
                            ScriptEditor.cbLinkedEvent.SelectedIndex = 2;
                            break;
                        case Scripts.EventType.SimulationTimer1:
                            ScriptEditor.cbLinkedEvent.SelectedIndex = 3;
                            break;
                        case Scripts.EventType.SimulationTimer5:
                            ScriptEditor.cbLinkedEvent.SelectedIndex = 4;
                            break;
                        case Scripts.EventType.SimulationTimer15:
                            ScriptEditor.cbLinkedEvent.SelectedIndex = 5;
                            break;
                        case Scripts.EventType.SimulationTimer30:
                            ScriptEditor.cbLinkedEvent.SelectedIndex = 6;
                            break;
                        case Scripts.EventType.SimulationTimer60:
                            ScriptEditor.cbLinkedEvent.SelectedIndex = 7;
                            break;
                    }

                    ScriptEditor.cbPythonInt.SelectedIndex = (int)selscript.PythonInterpreter;
                }
                catch (Exception ex)
                {
                    MessageBox.Show(ex.ToString(), MessageBoxType.Error);
                }

            };

            ti3a.Click += (sender, e) =>
            {
                if (MessageBox.Show("Confirm removal of the selected script?", "Delete Script", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.Yes) == DialogResult.Yes)
                {
                    Flowsheet.Scripts.Remove(lbScripts.SelectedKey);
                    lbScripts.Items.RemoveAt(lbScripts.SelectedIndex);
                }
            };

            ti15.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                Flowsheet.ShowMessage("Running script '" + Flowsheet.Scripts[lbScripts.SelectedKey].Title + "'...", IFlowsheet.MessageType.Information);
                Flowsheet.Scripts[lbScripts.SelectedKey].ScriptText = ScriptEditor.txtScript.ScriptText;
                Flowsheet.RunScript(lbScripts.SelectedKey);
            };

            ti16.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                Flowsheet.ShowMessage("Running script '" + Flowsheet.Scripts[lbScripts.SelectedKey].Title + "' asynchronously...", IFlowsheet.MessageType.Information);
                Flowsheet.Scripts[lbScripts.SelectedKey].ScriptText = ScriptEditor.txtScript.ScriptText;
                Flowsheet.RunScriptAsync(lbScripts.SelectedKey);
            };

            ti2.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                Flowsheet.Scripts[lbScripts.SelectedKey].ScriptText = ScriptEditor.txtScript.ScriptText;
            };

            leftcontainer.Rows.Add(new Label { Text = "Script List", Font = SystemFonts.Bold(), Height = 30, VerticalAlignment = VerticalAlignment.Center });
            leftcontainer.Rows.Add(new TableRow(menu1));
            leftcontainer.Rows.Add(new TableRow(lbScripts));
            leftcontainer.Padding = new Padding(5, 5, 5, 5);
            leftcontainer.Spacing = new Size(0, 0);
            leftcontainer.Width = 250;

            ScriptEditor.chkLink.CheckedChanged += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                Flowsheet.Scripts[lbScripts.SelectedKey].Linked = ScriptEditor.chkLink.Checked.GetValueOrDefault();
            };

            ScriptEditor.cbLinkedObject.SelectedIndexChanged += cbLinkedObject_SelectedIndexChanged;

            ScriptEditor.cbLinkedEvent.SelectedIndexChanged += cbLinkedObject_SelectedIndexChanged;

            //ScriptEditor.txtName.TextChanged += (sender, e) =>
            //{
            //    if (lbScripts.SelectedIndex < 0) return;
            //    Flowsheet.Scripts[lbScripts.SelectedKey].Title = ScriptEditor.txtName.Text;
            //    lbScripts.Items[lbScripts.SelectedIndex].Text = ScriptEditor.txtName.Text;
            //};

            rightcontainer.Rows.Add(new Label { Text = "Selected Script", Font = SystemFonts.Bold(), Height = 30, VerticalAlignment = VerticalAlignment.Center });
            rightcontainer.Rows.Add(new TableRow(menu2));
            rightcontainer.Rows.Add(new TableRow(ScriptEditor));
            rightcontainer.Padding = new Padding(5, 5, 5, 5);

            var splitc = new Splitter() { };
            splitc.Panel1 = leftcontainer;
            splitc.Panel1.Width = 250;
            splitc.Panel2 = rightcontainer;
            splitc.SplitterWidth = 2;

            Rows.Add(new TableRow(splitc));

        }

        void cbLinkedObject_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (!Loaded) return;
            if (adding) return;
            if (lbScripts.SelectedIndex < 0) return;
            if (!Flowsheet.Scripts.ContainsKey(lbScripts.SelectedKey)) return;
            var scr = Flowsheet.Scripts[lbScripts.SelectedKey];
            switch (ScriptEditor.cbLinkedObject.SelectedIndex)
            {
                case 0:
                    scr.LinkedObjectType = Scripts.ObjectType.Simulation;
                    scr.LinkedObjectName = "";
                    if (ScriptEditor.cbLinkedEvent.SelectedIndex == 0)
                    {
                        scr.LinkedEventType = Scripts.EventType.SimulationOpened;
                    }
                    else if (ScriptEditor.cbLinkedEvent.SelectedIndex == 1)
                    {
                        scr.LinkedEventType = Scripts.EventType.SimulationSaved;
                    }
                    else if (ScriptEditor.cbLinkedEvent.SelectedIndex == 2)
                    {
                        scr.LinkedEventType = Scripts.EventType.SimulationClosed;
                    }
                    else if (ScriptEditor.cbLinkedEvent.SelectedIndex == 3)
                    {
                        scr.LinkedEventType = Scripts.EventType.SimulationTimer1;
                    }
                    else if (ScriptEditor.cbLinkedEvent.SelectedIndex == 4)
                    {
                        scr.LinkedEventType = Scripts.EventType.SimulationTimer5;
                    }
                    else if (ScriptEditor.cbLinkedEvent.SelectedIndex == 5)
                    {
                        scr.LinkedEventType = Scripts.EventType.SimulationTimer15;
                    }
                    else if (ScriptEditor.cbLinkedEvent.SelectedIndex == 6)
                    {
                        scr.LinkedEventType = Scripts.EventType.SimulationTimer30;
                    }
                    else if (ScriptEditor.cbLinkedEvent.SelectedIndex == 7)
                    {
                        scr.LinkedEventType = Scripts.EventType.SimulationTimer60;
                    }
                    break;
                case 1:
                    scr.LinkedObjectType = Scripts.ObjectType.Solver;
                    scr.LinkedObjectName = "";
                    if (ScriptEditor.cbLinkedEvent.SelectedIndex == 0)
                    {
                        scr.LinkedEventType = Scripts.EventType.SolverStarted;
                    }
                    else if (ScriptEditor.cbLinkedEvent.SelectedIndex == 1)
                    {
                        scr.LinkedEventType = Scripts.EventType.SolverFinished;
                    }
                    else
                    {
                        scr.LinkedEventType = Scripts.EventType.SolverRecycleLoop;
                    }
                    break;
                default:
                    if (ScriptEditor.chkLink.Checked.GetValueOrDefault())
                    {
                        scr.LinkedObjectType = Scripts.ObjectType.FlowsheetObject;
                        scr.LinkedObjectName = Flowsheet.SimulationObjects[ScriptEditor.cbLinkedObject.SelectedKey].Name;
                    }
                    if (ScriptEditor.cbLinkedEvent.SelectedIndex == 0)
                    {
                        scr.LinkedEventType = Scripts.EventType.ObjectCalculationStarted;
                    }
                    else if (ScriptEditor.cbLinkedEvent.SelectedIndex == 1)
                    {
                        scr.LinkedEventType = Scripts.EventType.ObjectCalculationFinished;
                    }
                    else
                    {
                        scr.LinkedEventType = Scripts.EventType.ObjectCalculationError;
                    }
                    break;
            }
        }

        public override void UpdateList()
        {

            lbScripts.Items.Clear();
            foreach (var s in Flowsheet.Scripts)
            {
                lbScripts.Items.Add(new ListItem { Key = s.Key, Text = s.Value.Title });
            }

        }

        public override void UpdateScripts()
        {
            if (lbScripts.SelectedIndex < 0) return;
            Flowsheet.ShowMessage("Storing updated scripts for saving...", IFlowsheet.MessageType.Information);
            Flowsheet.Scripts[lbScripts.SelectedKey].ScriptText = ScriptEditor.txtScript.ScriptText;
        }

    }
}