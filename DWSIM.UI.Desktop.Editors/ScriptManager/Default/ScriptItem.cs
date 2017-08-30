using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.UnitOperations.UnitOperations;
using DWSIM.UnitOperations.Reactors;
using DWSIM.UnitOperations.SpecialOps;
using DWSIM.UnitOperations.Streams;
using DWSIM.Thermodynamics.Streams;

using Eto.Forms;

using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;
using Eto.Drawing;

using System.Diagnostics;
using System.IO;

using DWSIM.ExtensionMethods;
using DWSIM.UI.Shared;
using s = DWSIM.UI.Shared.Common;
using System.Collections.ObjectModel;

using global::DWSIM.SharedClasses.Spreadsheet;
using Ciloci.Flee;
using System.Globalization;
using System.Xml.Linq;
using DWSIM.Interfaces.Enums;

namespace DWSIM.UI.Desktop.Editors
{

    public class ScriptItem : TableLayout
    {

        public CheckBox chkLink;
        public DropDown cbLinkedObject, cbLinkedEvent, cbPythonInt;
        public DWSIM.UI.Controls.CodeEditorControl txtScript;
        public TextBox txtName;

        public Action<string> ChangeNameCallback;

        private FlowsheetBase.FlowsheetBase flowsheet;
        private string scriptID;

        public ScriptItem(FlowsheetBase.FlowsheetBase fs, string sID)
        {
            flowsheet = fs;
            scriptID = sID;
            Init();
        }

        void Init()
        {

            var script = flowsheet.Scripts[scriptID];

            txtScript = new Controls.CodeEditorControl();

            chkLink = new CheckBox { Text = "Link Script" };

            var lbl1 = new Label { Text = "Object", VerticalAlignment = VerticalAlignment.Center };
            var lbl2 = new Label { Text = "Event", VerticalAlignment = VerticalAlignment.Center };
            var lbl3 = new Label { Text = "Python Interpreter", VerticalAlignment = VerticalAlignment.Center };

            var btn1 = new Button {Text = "Update Script"};
            var btn2 = new Button { Text = "Update and Run" };
            var btn3 = new Button { Text = "Update and Run (Async)" };

            btn1.Click += (sender, e) => {
                flowsheet.Scripts[scriptID].ScriptText = txtScript.Text;
            };

            btn2.Click += (sender, e) => {
                flowsheet.Scripts[scriptID].ScriptText = txtScript.Text;
                flowsheet.ShowMessage("Running script '" + script.Title + "'...", IFlowsheet.MessageType.Information);
                flowsheet.RunScript(scriptID);
            };

            btn3.Click += (sender, e) =>
            {
                flowsheet.Scripts[scriptID].ScriptText = txtScript.Text;
                flowsheet.ShowMessage("Running script '" + script.Title + "' asynchronously...", IFlowsheet.MessageType.Information);
                flowsheet.RunScriptAsync(scriptID);
            };

            var lbl4 = new Label { Text = "Script Name", VerticalAlignment = VerticalAlignment.Center };
            txtName = new TextBox { Text = "Script Name" };
            
            cbLinkedObject = new DropDown();
            cbLinkedEvent = new DropDown();
            cbPythonInt = new DropDown();

            cbLinkedEvent.Items.AddRange(new String[] { "Simulation Opened", "Simulation Saved", "Simulation Closed", "1 min. Timer", "5 min. Timer", "15 min. Timer", "30 min. Timer", "60 min. Timer" }.Select((x) => new ListItem { Key = x, Text = x }));
            cbPythonInt.Items.AddRange(new String[] { "IronPython", "Python.NET" }.Select((x) => new ListItem { Key = x, Text = x }));

            var tr1 = new TableRow(btn1, btn2, btn3, lbl4, txtName, chkLink, lbl1, cbLinkedObject, lbl2, cbLinkedEvent, lbl3, cbPythonInt);
            tr1.Cells[4].ScaleWidth = true;
            var tb1 = new TableLayout { Spacing = new Size(5, 5), Padding = new Padding(0, 0, 0, 10) };
            tb1.Rows.Add(tr1);

            cbLinkedObject.SelectedIndexChanged += (sender, e) =>
            {
                if (cbLinkedObject.SelectedIndex < 0) return;
                switch (cbLinkedObject.SelectedIndex)
                {
                    case 0:
                        cbLinkedEvent.Items.Clear();
                        cbLinkedEvent.Items.AddRange(new string[] {
			                "Simulation Opened",
			                "Simulation Saved",
			                "Simulation Closed",
			                "1 min. Timer",
			                "5 min. Timer",
			                "15 min. Timer",
			                "30 min. Timer",
			                "60 min. Timer"
		                }.Select((x) => new ListItem { Key = x, Text = x }));
                        break;
                    case 1:
                        cbLinkedEvent.Items.Clear();
                        cbLinkedEvent.Items.AddRange(new string[] {
			                "Solver Started",
			                "Solver Finished",
			                "Recycle Loop"
		                }.Select((x) => new ListItem { Key = x, Text = x }));
                        break;
                    default:
                        cbLinkedEvent.Items.Clear();
                        cbLinkedEvent.Items.AddRange(new string[] {
			                "Object Calculation Started",
			                "Object Calculation Finished",
			                "Object Calculation Error"
		                }.Select((x) => new ListItem { Key = x, Text = x }));
                        break;
                }
                cbLinkedEvent.SelectedIndex = 0;
            };
                           
            var tr3 = new TableRow(txtScript);
            var tb3 = new TableLayout { Spacing = new Size(5, 5) };
            tb3.Rows.Add(tr3);
            
            Rows.Add(new TableRow(tb1));
            Rows.Add(new TableRow(tb3));


            cbLinkedObject.Items.Clear();
            cbLinkedObject.Items.Add(new ListItem { Text = "Simulation", Key = "Simulation" });
            cbLinkedObject.Items.Add(new ListItem { Text = "Solver", Key = "Solver" });
            foreach (var obj in flowsheet.SimulationObjects.Values)
            {
                cbLinkedObject.Items.Add(new ListItem { Text = obj.GraphicObject.Tag, Key = obj.Name });
            }

            txtName.Text = script.Title;
            txtScript.Text = script.ScriptText;
            chkLink.Checked = script.Linked;

            if (!string.IsNullOrEmpty(script.LinkedObjectName))
            {
                if (flowsheet.SimulationObjects.ContainsKey(script.LinkedObjectName)) cbLinkedObject.SelectedKey = script.LinkedObjectName;
            }
            else
            {
                switch (script.LinkedObjectType)
                {
                    case Scripts.ObjectType.Simulation:
                        cbLinkedObject.SelectedIndex = 0;
                        break;
                    case Scripts.ObjectType.Solver:
                        cbLinkedObject.SelectedIndex = 1;
                        break;
                }
            }

            switch (script.LinkedEventType)
            {
                case Scripts.EventType.ObjectCalculationStarted:
                    cbLinkedEvent.SelectedIndex = 0;
                    break;
                case Scripts.EventType.ObjectCalculationFinished:
                    cbLinkedEvent.SelectedIndex = 1;
                    break;
                case Scripts.EventType.ObjectCalculationError:
                    cbLinkedEvent.SelectedIndex = 2;
                    break;
                case Scripts.EventType.SimulationOpened:
                    cbLinkedEvent.SelectedIndex = 0;
                    break;
                case Scripts.EventType.SimulationSaved:
                    cbLinkedEvent.SelectedIndex = 1;
                    break;
                case Scripts.EventType.SimulationClosed:
                    cbLinkedEvent.SelectedIndex = 2;
                    break;
                case Scripts.EventType.SolverStarted:
                    cbLinkedEvent.SelectedIndex = 0;
                    break;
                case Scripts.EventType.SolverFinished:
                    cbLinkedEvent.SelectedIndex = 1;
                    break;
                case Scripts.EventType.SolverRecycleLoop:
                    cbLinkedEvent.SelectedIndex = 2;
                    break;
                case Scripts.EventType.SimulationTimer1:
                    cbLinkedEvent.SelectedIndex = 3;
                    break;
                case Scripts.EventType.SimulationTimer5:
                    cbLinkedEvent.SelectedIndex = 4;
                    break;
                case Scripts.EventType.SimulationTimer15:
                    cbLinkedEvent.SelectedIndex = 5;
                    break;
                case Scripts.EventType.SimulationTimer30:
                    cbLinkedEvent.SelectedIndex = 6;
                    break;
                case Scripts.EventType.SimulationTimer60:
                    cbLinkedEvent.SelectedIndex = 7;
                    break;
            }

            cbPythonInt.SelectedIndex = (int)script.PythonInterpreter;

            cbPythonInt.SelectedIndexChanged += (sender, e) => {
                script.PythonInterpreter = (DWSIM.Interfaces.Enums.Scripts.Interpreter)cbPythonInt.SelectedIndex;
            };

            chkLink.CheckedChanged += (sender, e) =>
            {
                flowsheet.Scripts[scriptID].Linked = chkLink.Checked.GetValueOrDefault();
            };

            txtName.TextChanged += (sender, e) =>
            {
                script.Title = txtName.Text;
                if (ChangeNameCallback != null) ChangeNameCallback.Invoke(txtName.Text);
            };

            cbLinkedObject.SelectedIndexChanged += cbLinkedObject_SelectedIndexChanged;

            cbLinkedEvent.SelectedIndexChanged += cbLinkedObject_SelectedIndexChanged;

        }

        void cbLinkedObject_SelectedIndexChanged(object sender, EventArgs e)
        {
            var scr = flowsheet.Scripts[scriptID];
            switch (cbLinkedObject.SelectedIndex)
            {
                case 0:
                    scr.LinkedObjectType = Scripts.ObjectType.Simulation;
                    scr.LinkedObjectName = "";
                    if (cbLinkedEvent.SelectedIndex == 0)
                    {
                        scr.LinkedEventType = Scripts.EventType.SimulationOpened;
                    }
                    else if (cbLinkedEvent.SelectedIndex == 1)
                    {
                        scr.LinkedEventType = Scripts.EventType.SimulationSaved;
                    }
                    else if (cbLinkedEvent.SelectedIndex == 2)
                    {
                        scr.LinkedEventType = Scripts.EventType.SimulationClosed;
                    }
                    else if (cbLinkedEvent.SelectedIndex == 3)
                    {
                        scr.LinkedEventType = Scripts.EventType.SimulationTimer1;
                    }
                    else if (cbLinkedEvent.SelectedIndex == 4)
                    {
                        scr.LinkedEventType = Scripts.EventType.SimulationTimer5;
                    }
                    else if (cbLinkedEvent.SelectedIndex == 5)
                    {
                        scr.LinkedEventType = Scripts.EventType.SimulationTimer15;
                    }
                    else if (cbLinkedEvent.SelectedIndex == 6)
                    {
                        scr.LinkedEventType = Scripts.EventType.SimulationTimer30;
                    }
                    else if (cbLinkedEvent.SelectedIndex == 7)
                    {
                        scr.LinkedEventType = Scripts.EventType.SimulationTimer60;
                    }
                    break;
                case 1:
                    scr.LinkedObjectType = Scripts.ObjectType.Solver;
                    scr.LinkedObjectName = "";
                    if (cbLinkedEvent.SelectedIndex == 0)
                    {
                        scr.LinkedEventType = Scripts.EventType.SolverStarted;
                    }
                    else if (cbLinkedEvent.SelectedIndex == 1)
                    {
                        scr.LinkedEventType = Scripts.EventType.SolverFinished;
                    }
                    else
                    {
                        scr.LinkedEventType = Scripts.EventType.SolverRecycleLoop;
                    }
                    break;
                default:
                    if (chkLink.Checked.GetValueOrDefault())
                    {
                        if (cbLinkedObject.SelectedValue != null)
                        {
                            scr.LinkedObjectType = Scripts.ObjectType.FlowsheetObject;
                            scr.LinkedObjectName = flowsheet.GetFlowsheetSimulationObject(cbLinkedObject.SelectedValue.ToString()).Name;
                        }
                    }
                    if (cbLinkedEvent.SelectedIndex == 0)
                    {
                        scr.LinkedEventType = Scripts.EventType.ObjectCalculationStarted;
                    }
                    else if (cbLinkedEvent.SelectedIndex == 1)
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

    }
}
