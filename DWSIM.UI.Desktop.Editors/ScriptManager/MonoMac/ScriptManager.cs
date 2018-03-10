using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Eto.Forms;
using Eto.Drawing;

using System.IO;

using DWSIM.Interfaces;
using DWSIM.ExtensionMethods;
using DWSIM.UI.Shared;
using s = DWSIM.UI.Shared.Common;
using System.Collections.ObjectModel;

using System.Globalization;
using DWSIM.Interfaces.Enums;

namespace DWSIM.UI.Desktop.Editors
{
    public class ScriptManager_Mac : ScriptManagerBase
    {

        private DWSIM.UI.Desktop.Shared.Flowsheet Flowsheet;

        private ListBox lbScripts;
        private ScriptItem_Mac ScriptEditor;

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

            var leftcontainer = new TableLayout();
            var rightcontainer = new TableLayout();

            ScriptEditor = new ScriptItem_Mac();

            var btnNew = new Button { Text = "New Script" };
            btnNew.Font = new Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize());
            btnNew.Click += (sender, e) =>
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

                    if (selscript != null) selscript.ScriptText = ScriptEditor.txtScript.Text;

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

                    ScriptEditor.txtName.Text = selscript.Title;
                    ScriptEditor.txtScript.Text = selscript.ScriptText;

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

            var btnDelete = new Button { Text = "Remove Selected" };
            btnDelete.Font = new Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize());
            btnDelete.Click += (sender, e) =>
            {
                Flowsheet.Scripts.Remove(lbScripts.SelectedKey);
                lbScripts.Items.RemoveAt(lbScripts.SelectedIndex);
            };

            ScriptEditor.btnRun.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                Flowsheet.ShowMessage("Running script '" + Flowsheet.Scripts[lbScripts.SelectedKey].Title + "'...", IFlowsheet.MessageType.Information);
                Flowsheet.Scripts[lbScripts.SelectedKey].ScriptText = ScriptEditor.txtScript.Text;
                Flowsheet.RunScript(lbScripts.SelectedKey);
            };

            ScriptEditor.btnRunAsync.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                Flowsheet.ShowMessage("Running script '" + Flowsheet.Scripts[lbScripts.SelectedKey].Title + "' asynchronously...", IFlowsheet.MessageType.Information);
                Flowsheet.Scripts[lbScripts.SelectedKey].ScriptText = ScriptEditor.txtScript.Text;
                Flowsheet.RunScriptAsync(lbScripts.SelectedKey);
            };

            ScriptEditor.btnUpdate.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                Flowsheet.Scripts[lbScripts.SelectedKey].ScriptText = ScriptEditor.txtScript.Text;
            };

            leftcontainer.Rows.Add(new Label { Text = "Script List", Font = SystemFonts.Bold() });
            leftcontainer.Rows.Add(new TableRow(btnNew));
            leftcontainer.Rows.Add(new TableRow(btnDelete));
            leftcontainer.Rows.Add(new TableRow(lbScripts));
            leftcontainer.Padding = new Padding(5, 5, 5, 5);
            leftcontainer.Spacing = new Size(0, 0);
            leftcontainer.Width = 200;

            ScriptEditor.chkLink.CheckedChanged += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                Flowsheet.Scripts[lbScripts.SelectedKey].Linked = ScriptEditor.chkLink.Checked.GetValueOrDefault();
            };

            //this.KeyDown += (sender, e) =>
            //{
            //    switch (e.Key)
            //    {
            //        case Keys.F5 | Keys.Shift:
            //            btnRun.PerformClick();
            //            break;
            //        case Keys.S | Keys.Shift:
            //            btnUpdate.PerformClick();
            //            break;
            //    }
            //};

            ScriptEditor.cbLinkedObject.SelectedIndexChanged += cbLinkedObject_SelectedIndexChanged;

            ScriptEditor.cbLinkedEvent.SelectedIndexChanged += cbLinkedObject_SelectedIndexChanged;

            ScriptEditor.txtName.TextChanged += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                Flowsheet.Scripts[lbScripts.SelectedKey].Title = ScriptEditor.txtName.Text;
                lbScripts.Items[lbScripts.SelectedIndex].Text = ScriptEditor.txtName.Text;
            };

            rightcontainer.Rows.Add(new Label { Text = "Selected Script", Font = SystemFonts.Bold() });
            rightcontainer.Rows.Add(new TableRow(ScriptEditor));
            rightcontainer.Padding = new Padding(5, 5, 5, 5);
            rightcontainer.Spacing = new Size(10, 10);

            Rows.Add(new TableRow(leftcontainer, rightcontainer));

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
            Flowsheet.Scripts[lbScripts.SelectedKey].ScriptText = ScriptEditor.txtScript.Text;
        }

    }
}