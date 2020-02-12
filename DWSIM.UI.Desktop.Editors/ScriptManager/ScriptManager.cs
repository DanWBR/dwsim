using System;
using Eto.Forms;
using Eto.Drawing;
using DWSIM.Interfaces;
using s = DWSIM.UI.Shared.Common;
using DWSIM.Interfaces.Enums;
using System.Linq;

namespace DWSIM.UI.Desktop.Editors
{
    public class ScriptManager_Mac : ScriptManagerBase
    {

        string imgprefix = "DWSIM.UI.Desktop.Editors.Resources.Icons.";

        private DWSIM.UI.Desktop.Shared.Flowsheet Flowsheet;

        private ListBox lbScripts;
        private ScriptItem ScriptEditor;

        private bool adding = false;

        private bool selecting = false;

        private DWSIM.Interfaces.IScript selscript;

        public ScriptManager_Mac(DWSIM.UI.Desktop.Shared.Flowsheet fs)
            : base()
        {
            Flowsheet = fs;
            Init();
        }

        void Init()
        {

            var leftcontainer = new TableLayout();
            var rightcontainer = new TableLayout();

            var ti1 = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "New Script", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-new.png")).WithSize(16, 16) };
            var ti2 = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Update Selected", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-approve_and_update.png")).WithSize(16, 16) };
            var ti3 = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Print", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-print.png")).WithSize(16, 16) };
            var ti3a = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Remove Selected", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-filled_trash.png")).WithSize(16, 16) };

            var ti4 = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Cut", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-cut.png")).WithSize(16, 16) };
            var ti5 = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Copy", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-copy.png")).WithSize(16, 16) };
            var ti6 = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Paste", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-paste.png")).WithSize(16, 16) };

            var ti7 = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Undo", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-undo.png")).WithSize(16, 16) };
            var ti8 = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Redo", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-redo.png")).WithSize(16, 16) };

            var ti9 = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, Text = "", ToolTip = "Toggle Comment/Uncomment Selected Lines", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-code.png")).WithSize(16, 16) };

            var ti10 = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Indent Right", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "indent-right.png")).WithSize(16, 16) };
            var ti11 = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Indent Left", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "left-indentation-option.png")).WithSize(16, 16) };

            var ti12 = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Decrease Font Size", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-decrease_font.png")).WithSize(16, 16) };
            var ti13 = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Increase Font Size", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-increase_font.png")).WithSize(16, 16) };

            var ti14 = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, Text = "", ToolTip = "Insert Snippet", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "code.png")).WithSize(16, 16) };

            var ti15 = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Run Script", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-play.png")).WithSize(16, 16) };
            var ti16 = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Run Script (Async)", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-circled_play.png")).WithSize(16, 16) };

            var ti17 = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, Text = "", ToolTip = "Help", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-help.png")).WithSize(16, 16) };

            var l1 = new Label() { VerticalAlignment = VerticalAlignment.Bottom, Text = "Rename", Font = new Font(SystemFont.Default, UI.Shared.Common.GetEditorFontSize()) };

            var t1 = new TextBox { Width = 250 };

            lbScripts = new ListBox();

            ScriptEditor = new ScriptItem(Flowsheet);

            leftcontainer.Rows.Add(new Label { Text = "Script List", Font = new Font(SystemFont.Bold, UI.Shared.Common.GetEditorFontSize()), Height = 30, VerticalAlignment = VerticalAlignment.Center });

            if (!Application.Instance.Platform.IsGtk)
            {
                var menu1 = new StackLayout
                {
                    Items = { ti1, ti2, ti3, ti3a },
                    Orientation = Orientation.Horizontal,
                    Spacing = 4,
                    HorizontalContentAlignment = HorizontalAlignment.Stretch,
                    VerticalContentAlignment = VerticalAlignment.Bottom,
                    Padding = 5,
                    Height = 34
                };
                leftcontainer.Rows.Add(new TableRow(menu1));
            }
            else
            {
                var menu1 = new StackLayout
                {
                    Items = { ti1, ti2 },
                    Orientation = Orientation.Horizontal,
                    Spacing = 4,
                    HorizontalContentAlignment = HorizontalAlignment.Stretch,
                    VerticalContentAlignment = VerticalAlignment.Bottom,
                    Padding = 5,
                    Height = 34
                };
                var menu1a = new StackLayout
                {
                    Items = { ti3, ti3a },
                    Orientation = Orientation.Horizontal,
                    Spacing = 4,
                    HorizontalContentAlignment = HorizontalAlignment.Stretch,
                    VerticalContentAlignment = VerticalAlignment.Bottom,
                    Padding = 5,
                    Height = 34
                };
                leftcontainer.Rows.Add(new TableRow(menu1));
                leftcontainer.Rows.Add(new TableRow(menu1a));
            }

            leftcontainer.Rows.Add(new TableRow(lbScripts));
            leftcontainer.Padding = new Padding(5, 5, 5, 5);
            leftcontainer.Spacing = new Size(0, 0);
            leftcontainer.Width = 250;

            rightcontainer.Rows.Add(new Label { Text = "Selected Script", Font = new Font(SystemFont.Bold, UI.Shared.Common.GetEditorFontSize()), Height = 30, VerticalAlignment = VerticalAlignment.Center });

            if (!Application.Instance.Platform.IsGtk)
            {
                var menu2 = new StackLayout
                {
                    Items = { ti15, ti16, new Label {Text =" " },
                    ti4, ti5, ti6, new Label {Text =" " },
                    ti7, ti8, new Label {Text =" " },
                    ti9, ti10, ti11, new Label {Text =" " },
                    ti12, ti13, new Label {Text =" " },
                    ti14, new Label {Text =" " },
                    ti17, l1, t1 },
                    Orientation = Orientation.Horizontal,
                    Spacing = 4,
                    HorizontalContentAlignment = HorizontalAlignment.Stretch,
                    VerticalContentAlignment = VerticalAlignment.Bottom,
                    Padding = 5,
                    Height = 34
                };
                rightcontainer.Rows.Add(new TableRow(menu2));
            }
            else
            {
                var menu2 = new StackLayout
                {
                    Items = { ti15, ti16, new Label {Text =" " },
                    ti4, ti5, ti6, new Label {Text =" " },
                    ti7, ti8, new Label {Text =" " }},
                    Orientation = Orientation.Horizontal,
                    Spacing = 4,
                    HorizontalContentAlignment = HorizontalAlignment.Stretch,
                    VerticalContentAlignment = VerticalAlignment.Bottom,
                    Padding = 5,
                    Height = 34
                };
                var menu2a = new StackLayout
                {
                    Items = {ti9, ti10, ti11, new Label {Text =" " },
                    ti12, ti13, new Label {Text =" " },
                    ti14, new Label {Text =" " },
                    ti17, l1, t1 },
                    Orientation = Orientation.Horizontal,
                    Spacing = 4,
                    HorizontalContentAlignment = HorizontalAlignment.Stretch,
                    VerticalContentAlignment = VerticalAlignment.Bottom,
                    Padding = 5,
                    Height = 34
                };
                var menu2b = new StackLayout
                {
                    Items = {ti14, new Label {Text =" " },
                    ti17, l1, t1 },
                    Orientation = Orientation.Horizontal,
                    Spacing = 4,
                    HorizontalContentAlignment = HorizontalAlignment.Stretch,
                    VerticalContentAlignment = VerticalAlignment.Bottom,
                    Padding = 5,
                    Height = 34
                };
                rightcontainer.Rows.Add(new TableRow(menu2));
                rightcontainer.Rows.Add(new TableRow(menu2a));
                rightcontainer.Rows.Add(new TableRow(menu2b));
            }

            rightcontainer.Rows.Add(new TableRow(ScriptEditor));
            rightcontainer.Padding = new Padding(5, 5, 5, 5);

            ti1.Click += (sender, e) =>
            {
                var script = new DWSIM.FlowsheetSolver.Script { ID = Guid.NewGuid().ToString(), Title = "Script" + (Flowsheet.Scripts.Count + 1).ToString() };
                Flowsheet.Scripts.Add(script.ID, script);
                lbScripts.Items.Add(new ListItem { Key = script.ID, Text = script.Title });
            };

            lbScripts.SelectedIndexChanged += (sender, e) =>
            {
                Application.Instance.Invoke(() =>
                {
                    try
                    {
                        if (lbScripts.SelectedIndex < 0) return;

                        selecting = true;

                        if (selscript != null) selscript.ScriptText = ScriptEditor.txtScript.ScriptText;

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

                        t1.Text = selscript.Title;

                        ScriptEditor.txtScript.ScriptText = selscript.ScriptText;

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
                    finally {
                        selecting = false;
                    }
                });

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

            ti3.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                ScriptEditor.txtScript.Print();
            };

            ti4.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                ScriptEditor.txtScript.Cut();
            };

            ti5.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                ScriptEditor.txtScript.Copy();
            };

            ti6.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                ScriptEditor.txtScript.Paste();
            };

            ti7.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                ScriptEditor.txtScript.Undo();
            };

            ti8.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                ScriptEditor.txtScript.Redo();
            };

            ti9.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                ScriptEditor.txtScript.ToggleCommenting();
            };

            ti10.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                ScriptEditor.txtScript.Indent();
            };

            ti11.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                ScriptEditor.txtScript.Unindent();
            };

            ti12.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                ScriptEditor.txtScript.DecreaseFontSize();
            };

            ti13.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                ScriptEditor.txtScript.IncreaseFontSize();
            };

            ti14.Click += (sender, e) =>
            {
                var c1 = new ContextMenu();
                Application.Instance.Invoke(() =>
                {
                    var snippets = SharedClasses.Scripts.IronPythonSnippets.GetSnippets();
                    foreach (var group1 in snippets.GroupBy((x) => x.Category1))
                    {
                        ButtonMenuItem tsmi = new ButtonMenuItem() { Text = group1.Key };
                        c1.Items.Add(tsmi);
                        foreach (var group2 in group1.GroupBy((x2) => x2.Category2))
                        {
                            ButtonMenuItem tsmi2 = new ButtonMenuItem() { Text = group2.Key };
                            tsmi.Items.Add(tsmi2);
                            foreach (var snippet in group2)
                            {
                                ButtonMenuItem tsmi3 = new ButtonMenuItem() { Text = snippet.Name + " (" + snippet.Scope + ")", Tag = snippet.Snippet };
                                tsmi3.Click += (sender2, e2) =>
                                {
                                    ScriptEditor.txtScript.InsertSnippet(tsmi3.Tag.ToString());
                                };
                                tsmi2.Items.Add(tsmi3);
                            }
                        }
                    }
                    DWSIM.SharedClasses.Scripts.IronPythonSnippets.PopulateWithDynamicSnippets(c1, Flowsheet, (text) =>
                    {
                        ScriptEditor.txtScript.InsertSnippet(text);
                    });
                    c1.Show(ti14);
                });
            };

            ScriptEditor.chkLink.CheckedChanged += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                Flowsheet.Scripts[lbScripts.SelectedKey].Linked = ScriptEditor.chkLink.Checked.GetValueOrDefault();
            };

            ScriptEditor.cbLinkedObject.SelectedIndexChanged += cbLinkedObject_SelectedIndexChanged;

            ScriptEditor.cbLinkedEvent.SelectedIndexChanged += cbLinkedObject_SelectedIndexChanged;

            t1.TextChanged += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                Flowsheet.Scripts[lbScripts.SelectedKey].Title = t1.Text;
                lbScripts.Items[lbScripts.SelectedIndex].Text = t1.Text;
            };

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
            if (selecting) return;
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