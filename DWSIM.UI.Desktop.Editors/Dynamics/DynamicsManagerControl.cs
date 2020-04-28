using Eto.Drawing;
using Eto.Forms;
using DWSIM.DynamicsManager;
using DWSIM.ExtensionMethods;
using DWSIM.UI.Shared;
using ext = DWSIM.UI.Shared.Common;
using System.CodeDom;
using System.Linq;
using System.Collections.Generic;
using System;

namespace DWSIM.UI.Desktop.Editors.Dynamics
{

    public class DynamicsManagerControl : TableLayout
    {
        string imgprefix = "DWSIM.UI.Desktop.Editors.Resources.Icons.";

        public CheckBox chkDynamics;

        private Panel eventEditor;

        private ListBox lbEventSets, lbEvents;

        private Shared.Flowsheet Flowsheet;

        public DynamicsManagerControl(Shared.Flowsheet fs) : base()
        {
            Flowsheet = fs;
        }

        public void Init()
        {

            var tl1 = new TableLayout { Padding = new Padding(5), Spacing = new Size(10, 10) };

            var tr1 = new TableRow();

            chkDynamics = new CheckBox { Text = "Dynamic Mode Enabled" };

            tr1.Cells.Add(chkDynamics);

            tr1.Cells.Add(null);

            var btnShowIntegrator = new Button { Text = "Show Integrator Controls" };

            tr1.Cells.Add(btnShowIntegrator);

            tl1.Rows.Add(tr1);

            Rows.Add(new TableRow(tl1));

            var DocumentContainer = new DocumentControl() { AllowReordering = false, DisplayArrows = false };

            DocumentContainer.Pages.Add(new DocumentPage { Text = "Model Status", Closable = false });
            DocumentContainer.Pages.Add(new DocumentPage { Text = "Event Sets", Closable = false });
            DocumentContainer.Pages.Add(new DocumentPage { Text = "Cause-and-Effect Matrices", Closable = false });
            DocumentContainer.Pages.Add(new DocumentPage { Text = "Integrators", Closable = false });
            DocumentContainer.Pages.Add(new DocumentPage { Text = "Schedules", Closable = false });
            DocumentContainer.Pages.Add(new DocumentPage { Text = "Controllers", Closable = false });
            DocumentContainer.Pages.Add(new DocumentPage { Text = "Indicators", Closable = false });

            var tl2 = new TableLayout { Padding = new Padding(5), Spacing = new Size(10, 10) };

            var tr2 = new TableRow();

            tr2.Cells.Add(DocumentContainer);

            tl2.Rows.Add(tr2);

            Rows.Add(new TableRow(tl2));

            // model status

            var l1 = new PixelLayout();

            var pb1 = new ImageView { Height = 40, Width = 40, Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-ok.png"), 40, 40, ImageInterpolation.Default) };
            var pb2 = new ImageView { Height = 40, Width = 40, Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-ok.png"), 40, 40, ImageInterpolation.Default) };
            var pb3 = new ImageView { Height = 40, Width = 40, Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-ok.png"), 40, 40, ImageInterpolation.Default) };

            var label1 = new Label { Height = 40, Text = "All Endpoint Material Streams are connected to Control Valves", Font = new Font(SystemFont.Default, UI.Shared.Common.GetEditorFontSize()), VerticalAlignment = VerticalAlignment.Center };
            var label2 = new Label { Height = 40, Text = "All Control Valves are configured with Kv Calculation Modes", Font = new Font(SystemFont.Default, UI.Shared.Common.GetEditorFontSize()), VerticalAlignment = VerticalAlignment.Center };
            var label3 = new Label { Height = 40, Text = "All Unit Operations in Flowsheet support Dynamic Mode", Font = new Font(SystemFont.Default, UI.Shared.Common.GetEditorFontSize()), VerticalAlignment = VerticalAlignment.Center };

            l1.Add(pb1, 20, 20);
            l1.Add(pb2, 20, 70);
            l1.Add(pb3, 20, 120);

            l1.Add(label1, 70, 20);
            l1.Add(label2, 70, 70);
            l1.Add(label3, 70, 120);

            DocumentContainer.Pages[0].Content = l1;

            // event sets

            var lce = new TableLayout();
            var rce = new TableLayout();
            var rce2 = new TableLayout();

            var btnAddEventSet = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Add New Set", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-plus_math.png")).WithSize(16, 16) };
            var btnRemoveEventSet = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Remove Selected Set", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-delete.png")).WithSize(16, 16) };

            btnAddEventSet.Click += (s, e) =>
            {
                try
                {
                    var es = new EventSet { ID = Guid.NewGuid().ToString() };
                    Dialog form = null;
                    var tb = new TextBox { Text = es.Description };
                    form = ext.CreateDialogWithButtons(tb, "Enter a Name", () => es.Description = tb.Text);
                    form.Location = btnAddEventSet.Location;
                    form.ShowModal(this);
                    lbEventSets.Items.Add(new ListItem { Key = es.ID, Text = es.Description });
                    Flowsheet.DynamicsManager.EventSetList.Add(es.ID, es);
                }
                catch (Exception ex)
                {
                    MessageBox.Show("Error: " + ex.Message, MessageBoxType.Error);
                }
            };

            btnRemoveEventSet.Click += (s, e) =>
            {
                var result = MessageBox.Show("Confirm?", "Remove Event Set", MessageBoxType.Question);
                if (result == DialogResult.Yes)
                {
                    try
                    {
                        Flowsheet.DynamicsManager.EventSetList.Remove(lbEventSets.SelectedKey);
                        lbEventSets.Items.RemoveAt(lbEventSets.SelectedIndex);
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show("Error: " + ex.Message, MessageBoxType.Error);
                    }
                }
            };

            lbEventSets = new ListBox();

            lce.Rows.Add(new Label { Text = "Event Sets", Font = new Font(SystemFont.Bold, UI.Shared.Common.GetEditorFontSize()), Height = 30, VerticalAlignment = VerticalAlignment.Center });

            var menu1 = new StackLayout
            {
                Items = { btnAddEventSet, btnRemoveEventSet },
                Orientation = Orientation.Horizontal,
                Spacing = 4,
                HorizontalContentAlignment = HorizontalAlignment.Stretch,
                VerticalContentAlignment = VerticalAlignment.Bottom,
                Padding = 5,
                Height = 34
            };
            lce.Rows.Add(new TableRow(menu1));

            lce.Rows.Add(new TableRow(lbEventSets));
            lce.Padding = new Padding(5, 5, 5, 5);
            lce.Spacing = new Size(0, 0);
            lce.Width = 250;

            rce.Rows.Add(new Label { Text = "Selected Event Set", Font = new Font(SystemFont.Bold, UI.Shared.Common.GetEditorFontSize()), Height = 30, VerticalAlignment = VerticalAlignment.Center });

            var btnAddEvent = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Add New Event", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-plus_math.png")).WithSize(16, 16) };
            var btnRemoveEvent = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Remove Selected Event", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-delete.png")).WithSize(16, 16) };

            btnAddEvent.Click += (s, e) =>
            {
                try
                {
                    var ev = new DynamicEvent { ID = Guid.NewGuid().ToString() };
                    Dialog form = null;
                    var tb = new TextBox { Text = ev.Description };
                    form = ext.CreateDialogWithButtons(tb, "Enter a Name", () => ev.Description = tb.Text);
                    form.Location = btnAddEvent.Location;
                    form.ShowModal(this);
                    lbEvents.Items.Add(new ListItem { Key = ev.ID, Text = ev.Description });
                    Flowsheet.DynamicsManager.EventSetList[lbEventSets.SelectedKey].Events.Add(ev.ID, ev);
                }
                catch (Exception ex)
                {
                    MessageBox.Show("Error: " + ex.Message, MessageBoxType.Error);
                }
            };

            btnRemoveEvent.Click += (s, e) =>
            {
                var result = MessageBox.Show("Confirm?", "Remove Event", MessageBoxType.Question);
                if (result == DialogResult.Yes)
                {
                    try
                    {
                        Flowsheet.DynamicsManager.EventSetList[lbEventSets.SelectedKey].Events.Remove(lbEvents.SelectedKey);
                        lbEvents.Items.RemoveAt(lbEvents.SelectedIndex);
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show("Error: " + ex.Message, MessageBoxType.Error);
                    }
                }
            };

            var menu2 = new StackLayout
            {
                Items = { btnAddEvent, btnRemoveEvent },
                Orientation = Orientation.Horizontal,
                Spacing = 4,
                HorizontalContentAlignment = HorizontalAlignment.Stretch,
                VerticalContentAlignment = VerticalAlignment.Bottom,
                Padding = 5,
                Height = 34
            };
            rce.Rows.Add(new TableRow(menu2));

            lbEvents = new ListBox();

            rce.Rows.Add(new TableRow(lbEvents));
            rce.Padding = new Padding(5, 5, 5, 5);

            rce2.Rows.Add(new Label { Text = "Selected Event", Font = new Font(SystemFont.Bold, UI.Shared.Common.GetEditorFontSize()), Height = 30, VerticalAlignment = VerticalAlignment.Center });

            eventEditor = new DynamicLayout();

            rce2.Rows.Add(new TableRow(eventEditor));
            rce2.Padding = new Padding(5, 5, 5, 5);

            var splites2 = new Splitter() { };
            splites2.Panel1 = rce;
            splites2.Panel1.Width = 250;
            splites2.Panel2 = rce2;
            splites2.SplitterWidth = 2;

            var splites = new Splitter() { };
            splites.Panel1 = lce;
            splites.Panel1.Width = 250;
            splites.Panel2 = splites2;
            splites.SplitterWidth = 2;

            DocumentContainer.Pages[1].Content = splites;

            // populate lists

            foreach (var es in Flowsheet.DynamicsManager.EventSetList)
            {
                lbEventSets.Items.Add(new ListItem { Key = es.Key, Text = es.Value.Description });
            }

            lbEventSets.SelectedIndexChanged += (s, e) =>
            {
                lbEvents.Items.Clear();
                var es = Flowsheet.DynamicsManager.EventSetList[lbEventSets.SelectedKey];
                foreach (var ev in es.Events)
                {
                    lbEvents.Items.Add(new ListItem { Key = ev.Key, Text = ev.Value.Description });
                }
            };

            lbEvents.SelectedIndexChanged += (s, e) =>
            {
                var ev = Flowsheet.DynamicsManager.EventSetList[lbEventSets.SelectedKey].Events[lbEvents.SelectedKey];
                Flowsheet.RunCodeOnUIThread(() =>
                {
                    PopulateEventContainer(ev);
                });
            };

        }

        public void PopulateEventContainer(Interfaces.IDynamicsEvent ev)
        {

            var prevval = GlobalSettings.Settings.EditorTextBoxFixedSize;

            GlobalSettings.Settings.EditorTextBoxFixedSize = false;

            var layout = new DynamicLayout();

            layout.CreateAndAddCheckBoxRow("Active", ev.Enabled, (s, e) =>
            {
                ev.Enabled = s.Checked.GetValueOrDefault();
            });

            layout.CreateAndAddStringEditorRow("Name", ev.Description, (s, e) =>
            {
                ev.Description = s.Text;
                lbEvents.Items[lbEvents.SelectedIndex].Text = s.Text;
            });

            var dtp = new DateTimePicker { Mode = DateTimePickerMode.Time, Value = ev.TimeStamp };
            dtp.Font = new Font(SystemFont.Default, UI.Shared.Common.GetEditorFontSize());
            dtp.ValueChanged += (s, e) =>
            {
                ev.TimeStamp = dtp.Value.GetValueOrDefault();
            };

            layout.CreateAndAddLabelAndControlRow("Timestamp", dtp);

            layout.CreateAndAddDropDownRow("Type", ev.EventType.GetEnumNames(), (int)ev.EventType, (s, e) =>
            {
                ev.EventType = s.SelectedIndex.ToEnum<Interfaces.Enums.Dynamics.DynamicsEventType>();
            });

            var objects = Flowsheet.SimulationObjects.Values.Select((x) => x.GraphicObject.Tag).ToList();
            objects.Insert(0, "");

            DropDown propselector = null;
            List<string> props = new List<string>();
            List<string> propids = new List<string>();
            props.Add("");
            propids.Add("");

            int idx = 0;

            if (ev.SimulationObjectID != "")
            {
                if (Flowsheet.SimulationObjects.ContainsKey(ev.SimulationObjectID))
                {
                    idx = objects.IndexOf(Flowsheet.SimulationObjects[ev.SimulationObjectID].GraphicObject.Tag);
                    propids.AddRange(Flowsheet.SimulationObjects[ev.SimulationObjectID].GetProperties(Interfaces.Enums.PropertyType.WR));
                    props.AddRange(propids.Select((x) => Flowsheet.GetTranslatedString(x)).ToArray());
                }
            }

            layout.CreateAndAddDropDownRow("Object", objects, idx, (s, e) =>
            {
                if (s.SelectedIndex != 0)
                {
                    ev.SimulationObjectID = Flowsheet.GetFlowsheetSimulationObject(s.SelectedValue.ToString()).Name;
                    propids.Clear();
                    propids.Add("");
                    propids.AddRange(Flowsheet.SimulationObjects[ev.SimulationObjectID].GetProperties(Interfaces.Enums.PropertyType.WR));
                    props.Clear();
                    props.Add("");
                    props.AddRange(propids.Select((x) => Flowsheet.GetTranslatedString(x)).ToArray());
                    propselector.Items.Clear();
                    propselector.Items.AddRange(props.Select((x) => new ListItem { Text = x }));
                }
                else
                {
                    ev.SimulationObjectID = "";
                }
            });

            propselector = layout.CreateAndAddDropDownRow("Property", props,
                props.IndexOf(Flowsheet.GetTranslatedString(ev.SimulationObjectProperty)),
                (s, e) =>
                {
                    if (s.SelectedIndex >= 0) ev.SimulationObjectProperty = propids[s.SelectedIndex];
                });

            layout.CreateAndAddStringEditorRow("Value",
                ev.SimulationObjectPropertyValue, (s, e) =>
                {
                    ev.SimulationObjectPropertyValue = s.Text;
                });

            layout.CreateAndAddStringEditorRow("Units",
                ev.SimulationObjectPropertyUnits, (s, e) =>
                {
                    ev.SimulationObjectPropertyUnits = s.Text;
                });

            GlobalSettings.Settings.EditorTextBoxFixedSize = prevval;

            layout.Invalidate();

            eventEditor.Content = layout;

        }

    }
}
