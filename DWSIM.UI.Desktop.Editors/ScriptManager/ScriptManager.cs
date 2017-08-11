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
    public class ScriptManager : TableLayout
    {

        private DWSIM.UI.Desktop.Shared.Flowsheet Flowsheet;

        private ListBox lbScripts;
        private ScriptItem ScriptEditor;

        public ScriptManager(DWSIM.UI.Desktop.Shared.Flowsheet fs)
        {
            Flowsheet = fs;
            Init();
        }

        void Init()
        {

            var leftcontainer = new TableLayout();
            var rightcontainer = new TableLayout();

            var btnNew = new Button { Text = "New Script" };
            btnNew.Click += (sender, e) =>
            {
                var script = new DWSIM.FlowsheetSolver.Script { ID = Guid.NewGuid().ToString(), Title = "Script" + (Flowsheet.Scripts.Count + 1).ToString() };
                Flowsheet.Scripts.Add(script.ID, script);
                lbScripts.Items.Add(new ListItem { Key = script.ID, Text = script.Title });
            };

            lbScripts = new ListBox();

            lbScripts.SelectedIndexChanged += (sender, e) =>
            {

                if (lbScripts.SelectedIndex < 0) return;

                var script = Flowsheet.Scripts[lbScripts.SelectedKey];

                ScriptEditor.cbLinkedObject.Items.Clear();
                ScriptEditor.cbLinkedObject.Items.Add(new ListItem { Text = "Simulation", Key = "Simulation" });
                ScriptEditor.cbLinkedObject.Items.Add(new ListItem { Text = "Solver", Key = "Solver" });
                foreach (var obj in Flowsheet.SimulationObjects.Values)
                {
                    ScriptEditor.cbLinkedObject.Items.Add(new ListItem { Text = obj.GraphicObject.Tag, Key = obj.Name });
                }

                ScriptEditor.txtName.Text = script.Title;
                ScriptEditor.txtScript.Text = script.ScriptText;
                ScriptEditor.chkLink.Checked = script.Linked;

                if (!string.IsNullOrEmpty(script.LinkedObjectName))
                {
                    if (Flowsheet.SimulationObjects.ContainsKey(script.LinkedObjectName)) ScriptEditor.cbLinkedObject.SelectedKey = script.LinkedObjectName;
                }
                else
                {
                    switch (script.LinkedObjectType)
                    {
                        case Scripts.ObjectType.Simulation:
                            ScriptEditor.cbLinkedObject.SelectedIndex = 0;
                            break;
                        case Scripts.ObjectType.Solver:
                            ScriptEditor.cbLinkedObject.SelectedIndex = 1;
                            break;
                    }
                }

                switch (script.LinkedEventType)
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

                ScriptEditor.cbPythonInt.SelectedIndex = (int)script.PythonInterpreter;

            };

            var btnDelete = new Button { Text = "Remove Selected" };
            btnDelete.Click += (sender, e) =>
            {
                Flowsheet.Scripts.Remove(lbScripts.SelectedKey);
                lbScripts.Items.RemoveAt(lbScripts.SelectedIndex);
            };

            var btnRun = new Button { Text = "Run Selected" };
            btnRun.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                Flowsheet.Scripts[lbScripts.SelectedKey].ScriptText = ScriptEditor.txtScript.Text;
                Flowsheet.RunScript(lbScripts.SelectedKey);
            };

            var btnUpdate = new Button { Text = "Update Selected" };
            btnUpdate.Click += (sender, e) =>
            {
                if (lbScripts.SelectedIndex < 0) return;
                Flowsheet.Scripts[lbScripts.SelectedKey].ScriptText = ScriptEditor.txtScript.Text;
            };

            leftcontainer.Rows.Add(new Label { Text = "Script List", Font = SystemFonts.Bold() });
            leftcontainer.Rows.Add(new TableRow(btnNew));
            leftcontainer.Rows.Add(new TableRow(btnUpdate));
            leftcontainer.Rows.Add(new TableRow(btnRun));
            leftcontainer.Rows.Add(new TableRow(btnDelete));
            leftcontainer.Rows.Add(new TableRow(lbScripts));
            leftcontainer.Padding = new Padding(5, 5, 5, 5);
            leftcontainer.Spacing = new Size(10, 10);
            leftcontainer.Width = 200;

            ScriptEditor = new ScriptItem();

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
            if (lbScripts.SelectedIndex < 0) return;
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
                        if (ScriptEditor.cbLinkedObject.SelectedValue != null)
                        {
                            scr.LinkedObjectType = Scripts.ObjectType.FlowsheetObject;
                            scr.LinkedObjectName = Flowsheet.GetFlowsheetSimulationObject(ScriptEditor.cbLinkedObject.SelectedValue.ToString()).Name;
                        }
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

        public void UpdateList()
        {

            lbScripts.Items.Clear();
            foreach (var s in Flowsheet.Scripts)
            {
                lbScripts.Items.Add(new ListItem { Key = s.Key, Text = s.Value.Title });
            }

        }

    }
}
