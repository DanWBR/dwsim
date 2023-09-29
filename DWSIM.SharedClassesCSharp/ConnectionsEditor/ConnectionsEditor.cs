using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using System;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Windows.Forms;

namespace DWSIM.SharedClassesCSharp.ConnectionsEditor
{
    public partial class ConnectionsEditor : UserControl
    {

        public ISimulationObject SimObject;

        public ConnectionsEditor()
        {
            InitializeComponent();
        }

        private void ConnectionsEditor_Load(object sender, EventArgs e)
        {

            Initialize();

        }

        public void UpdateInfo()
        {
        
        }

        void Initialize()
        {

            var mslist = SimObject.GetFlowsheet().GraphicObjects.Values.Where((x) => x.ObjectType == ObjectType.MaterialStream).Select((m) => m.Tag).ToList();
            var eslist = SimObject.GetFlowsheet().GraphicObjects.Values.Where((x) => x.ObjectType == ObjectType.EnergyStream).Select((m) => m.Tag).ToList();

            var mslist_in = SimObject.GetFlowsheet().GraphicObjects.Values.Where((x) => x.ObjectType == ObjectType.MaterialStream && !x.OutputConnectors[0].IsAttached).Select((m) => m.Tag).ToList();
            var eslist_in = SimObject.GetFlowsheet().GraphicObjects.Values.Where((x) => x.ObjectType == ObjectType.EnergyStream && !x.OutputConnectors[0].IsAttached).Select((m) => m.Tag).ToList();

            var mslist_out = SimObject.GetFlowsheet().GraphicObjects.Values.Where((x) => x.ObjectType == ObjectType.MaterialStream && !x.InputConnectors[0].IsAttached).Select((m) => m.Tag).ToList();
            var eslist_out = SimObject.GetFlowsheet().GraphicObjects.Values.Where((x) => x.ObjectType == ObjectType.EnergyStream && !x.InputConnectors[0].IsAttached).Select((m) => m.Tag).ToList();

            mslist.Insert(0, "");
            eslist.Insert(0, "");

            mslist_in.Insert(0, "");
            eslist_in.Insert(0, "");

            mslist_out.Insert(0, "");
            eslist_out.Insert(0, "");

            foreach (var cp in SimObject.GraphicObject.InputConnectors)
            {
                if (cp.Type != ConType.ConEn)
                {
                    CreateAndAddRow("In", cp, mslist_in);
                }
            }

            foreach (var cp in SimObject.GraphicObject.OutputConnectors)
            {
                if (cp.Type != ConType.ConEn)
                {
                    CreateAndAddRow("Out", cp, mslist_out);
                }
            }

            foreach (var cp in SimObject.GraphicObject.InputConnectors)
            {
                if (cp.Type == ConType.ConEn)
                {
                    CreateAndAddRow("In", cp, eslist_in);
                }
            }

            foreach (var cp in SimObject.GraphicObject.OutputConnectors)
            {
                if (cp.Type == ConType.ConEn)
                {
                    CreateAndAddRow("Out", cp, eslist_out);
                }
            }

            if (SimObject.GraphicObject.EnergyConnector.Active) { CreateAndAddRow("Out", SimObject.GraphicObject.EnergyConnector, eslist_out); }

            PropertiesLayout.Controls.Add(new TableLayoutPanel());

        }

        void CreateAndAddRow(string direction, IConnectionPoint connector, List<string> options)
        {

            var cbConnection = new ComboBox {Dock = DockStyle.Fill };
            var label = new Label { Dock = DockStyle.Fill, TextAlign = ContentAlignment.MiddleLeft };
            var btnDisconnect = new Button();

            var dd = (int)(24 * GlobalSettings.Settings.DpiScale);
            var dd2 = (int)(20 * GlobalSettings.Settings.DpiScale);

            btnDisconnect.BackgroundImage = Properties.Resources.disconnect;
            btnDisconnect.BackgroundImageLayout = ImageLayout.Zoom;
            btnDisconnect.Size = new Size(dd2, dd2);

            cbConnection.Items.AddRange(options.ToArray());
            cbConnection.SelectedIndex = 0;

            label.Text = connector.ConnectorName;

            var tl = new TableLayoutPanel();
            tl.Height = dd;
            tl.RowStyles.Clear();
            tl.RowStyles.Add(new RowStyle(SizeType.Percent, 100.0f));
            tl.ColumnStyles.Clear();
            tl.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 40.0f));
            tl.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 60.0f));
            tl.ColumnStyles.Add(new ColumnStyle(SizeType.Absolute, dd));

            tl.Controls.Add(label, 0, 0);
            tl.Controls.Add(cbConnection, 1, 0);
            tl.Controls.Add(btnDisconnect, 2, 0);

            if (connector.IsAttached)
            {
                if (connector.AttachedConnector != null)
                {
                    if (direction == "In" && (connector.Type == ConType.ConIn | connector.Type == ConType.ConEn))
                    {
                        if (connector.IsEnergyConnector)
                        {
                            if (!options.Contains(connector.AttachedConnector.AttachedTo.Tag))
                            {
                                options.Add(connector.AttachedConnector.AttachedTo.Tag);
                                cbConnection.Items.Add(connector.AttachedConnector.AttachedTo.Tag);
                            }
                            if (options.Contains(connector.AttachedConnector.AttachedTo.Tag))
                            {
                                cbConnection.SelectedIndex = Array.IndexOf(options.ToArray(), connector.AttachedConnector.AttachedTo.Tag);
                            }
                        }
                        else
                        {
                            if (!options.Contains(connector.AttachedConnector.AttachedFrom.Tag))
                            {
                                options.Add(connector.AttachedConnector.AttachedFrom.Tag);
                                cbConnection.Items.Add(connector.AttachedConnector.AttachedFrom.Tag);
                            }
                            if (options.Contains(connector.AttachedConnector.AttachedFrom.Tag))
                            {
                                cbConnection.SelectedIndex = Array.IndexOf(options.ToArray(), connector.AttachedConnector.AttachedFrom.Tag);
                            }
                        }
                    }
                    else if (connector.Type == ConType.ConOut | connector.Type == ConType.ConEn)
                    {
                        if (!options.Contains(connector.AttachedConnector.AttachedTo.Tag))
                        {
                            options.Add(connector.AttachedConnector.AttachedTo.Tag);
                            cbConnection.Items.Add(connector.AttachedConnector.AttachedTo.Tag);
                        }
                        if (options.Contains(connector.AttachedConnector.AttachedTo.Tag))
                        {
                            cbConnection.SelectedIndex = Array.IndexOf(options.ToArray(), connector.AttachedConnector.AttachedTo.Tag);
                        }
                    }
                }
            }
            else
            {
                cbConnection.SelectedIndex = 0;
            }

            cbConnection.Tag = false;

            Action<object, EventArgs> schangedaction = (sender, e) =>
            {

                if (cbConnection.SelectedIndex == -1) return;

                var fgui = SimObject.GetFlowsheet();

                if ((bool)cbConnection.Tag)
                {

                    var sel = options.ElementAt(cbConnection.SelectedIndex);

                    var gobj = SimObject.GraphicObject;
                    var flowsheet = SimObject.GetFlowsheet();

                    if (connector.IsAttached && sel == "")
                    {
                        if (direction == "In" && (connector.Type == ConType.ConIn | connector.Type == ConType.ConEn))
                        {
                            try
                            {
                                var objfrom = connector.AttachedConnector.AttachedFrom;
                                flowsheet.DisconnectObjects(objfrom, gobj);
                                flowsheet.ShowMessage(String.Format("Disconnected {0} from {1}.", objfrom.Tag, gobj.Tag), IFlowsheet.MessageType.Information);
                            }
                            catch (Exception ex)
                            {
                                flowsheet.ShowMessage(ex.Message.ToString(), IFlowsheet.MessageType.GeneralError);
                            }
                            fgui.UpdateInterface();
                            fgui.UpdateOpenEditForms();
                            return;
                        }
                        else if (connector.Type == ConType.ConOut | connector.Type == ConType.ConEn)
                        {
                            try
                            {
                                var objto = connector.AttachedConnector.AttachedTo;
                                flowsheet.DisconnectObjects(gobj, objto);
                                flowsheet.ShowMessage(String.Format("Disconnected {0} from {1}.", gobj.Tag, objto.Tag), IFlowsheet.MessageType.Information);
                            }
                            catch (Exception ex)
                            {
                                flowsheet.ShowMessage(ex.Message.ToString(), IFlowsheet.MessageType.GeneralError); ;
                            }
                            fgui.UpdateInterface();
                            fgui.UpdateOpenEditForms();
                            return;
                        }
                    }

                    if (sel != "")
                    {

                        var gobj2 = flowsheet.GetFlowsheetSimulationObject(sel)?.GraphicObject;

                        if (gobj2 == null) return;

                        if (direction == "In" && (connector.Type == ConType.ConIn | connector.Type == ConType.ConEn))
                        {
                            if (connector.IsAttached)
                            {
                                try
                                {
                                    var objfrom = connector.AttachedConnector.AttachedFrom;
                                    flowsheet.DisconnectObjects(objfrom, gobj);
                                    flowsheet.ShowMessage(String.Format("Disconnected {0} from {1}.", objfrom.Tag, gobj.Tag), IFlowsheet.MessageType.Information);
                                }
                                catch (Exception ex)
                                {
                                    flowsheet.ShowMessage(ex.Message.ToString(), IFlowsheet.MessageType.GeneralError);
                                }
                            }
                            if (connector.IsEnergyConnector)
                            {
                                if (gobj2.InputConnectors[0].IsAttached)
                                {
                                    flowsheet.ShowMessage("Selected object already connected to another object.", IFlowsheet.MessageType.GeneralError);
                                    cbConnection.SelectedIndex = 0;
                                    return;
                                }
                                try
                                {
                                    flowsheet.ConnectObjects(gobj, gobj2, 0, 0);
                                    flowsheet.ShowMessage(String.Format("Connected {0} to {1}.", gobj.Tag, gobj2.Tag), IFlowsheet.MessageType.Information);
                                }
                                catch (Exception ex)
                                {
                                    flowsheet.ShowMessage(ex.Message.ToString(), IFlowsheet.MessageType.GeneralError);
                                }
                            }
                            else
                            {
                                if (gobj2.OutputConnectors[0].IsAttached)
                                {
                                    flowsheet.ShowMessage("Selected object already connected to another object.", IFlowsheet.MessageType.GeneralError);
                                    cbConnection.SelectedIndex = 0;
                                    return;
                                }
                                try
                                {
                                    flowsheet.ConnectObjects(gobj2, gobj, 0, gobj.InputConnectors.IndexOf(connector));
                                    flowsheet.ShowMessage(String.Format("Connected {0} to {1}.", gobj2.Tag, gobj.Tag), IFlowsheet.MessageType.Information);
                                }
                                catch (Exception ex)
                                {
                                    flowsheet.ShowMessage(ex.Message.ToString(), IFlowsheet.MessageType.GeneralError);
                                }
                            }
                        }
                        else if (connector.Type == ConType.ConOut | connector.Type == ConType.ConEn)
                        {
                            if (gobj2.InputConnectors[0].IsAttached)
                            {
                                flowsheet.ShowMessage("Selected object already connected to another object.", IFlowsheet.MessageType.GeneralError);
                                cbConnection.SelectedIndex = 0;
                                return;
                            }
                            try
                            {
                                if (connector.IsAttached)
                                {
                                    var objto = connector.AttachedConnector.AttachedTo;
                                    flowsheet.DisconnectObjects(gobj, objto);
                                    flowsheet.ShowMessage(String.Format("Disconnected {0} from {1}.", gobj.Tag, objto.Tag), IFlowsheet.MessageType.Information);
                                }
                                flowsheet.ConnectObjects(gobj, gobj2, gobj.OutputConnectors.IndexOf(connector), 0);
                                flowsheet.ShowMessage(String.Format("Connected {0} to {1}.", gobj.Tag, gobj2.Tag), IFlowsheet.MessageType.Information);
                            }
                            catch (Exception ex)
                            {
                                flowsheet.ShowMessage(ex.Message.ToString(), IFlowsheet.MessageType.GeneralError);
                            }
                        }

                        fgui.UpdateInterface();
                        fgui.UpdateOpenEditForms();

                    }
                }

            };

            cbConnection.KeyUp += (sender, e) =>
            {
                if (e.KeyCode == Keys.Enter)
                {

                    var items = new List<string>();
                    foreach (var item in cbConnection.Items)
                    {
                        items.Add(item.ToString());
                    }

                    if (!items.Contains(cbConnection.Text))
                    {
                        var gobj = SimObject.GraphicObject;
                        var flowsheet = SimObject.GetFlowsheet();
                        float posx, posy;
                        if (direction == "In")
                        {
                            posx = gobj.X - 100;
                            posy = gobj.Y + gobj.Height / 2;
                        }
                        else
                        {
                            posx = gobj.X + gobj.Width + 100;
                            posy = gobj.Y + gobj.Height / 2;
                        }
                        ISimulationObject stream = flowsheet.GetFlowsheetSimulationObject(cbConnection.Text);
                        if (stream == null)
                        {
                            if (connector.Type == ConType.ConEn)
                            {
                                stream = flowsheet.AddObject(ObjectType.EnergyStream, (int)posx, (int)posy, cbConnection.Text);
                            }
                            else
                            {
                                stream = flowsheet.AddObject(ObjectType.MaterialStream, (int)posx, (int)posy, cbConnection.Text);
                            }
                        }
                        dynamic go = stream.GraphicObject;
                        go.CreateConnectors(0, 0);
                        if (direction == "In")
                        {
                            try
                            {
                                if (connector.IsAttached)
                                {
                                    var objfrom = connector.AttachedConnector.AttachedFrom;
                                    flowsheet.DisconnectObjects(objfrom, gobj);
                                    flowsheet.ShowMessage(String.Format("Disconnected {0} from {1}.", objfrom.Tag, gobj.Tag), IFlowsheet.MessageType.Information);
                                }
                                flowsheet.ConnectObjects(stream.GraphicObject, gobj, 0, gobj.InputConnectors.IndexOf(connector));
                                flowsheet.ShowMessage(String.Format("Connected {0} to {1}.", stream.GraphicObject.Tag, gobj.Tag), IFlowsheet.MessageType.Information);
                                cbConnection.Items.Add(cbConnection.Text);
                                var list = new List<string>(options);
                                list.Add(cbConnection.Text);
                                options = list;
                            }
                            catch (Exception ex)
                            {
                                flowsheet.ShowMessage(ex.Message.ToString(), IFlowsheet.MessageType.GeneralError);
                            }
                        }
                        else
                        {
                            try
                            {
                                if (connector.IsAttached)
                                {
                                    var objto = connector.AttachedConnector.AttachedTo;
                                    flowsheet.DisconnectObjects(gobj, objto);
                                    flowsheet.ShowMessage(String.Format("Disconnected {0} from {1}", gobj.Tag, objto.Tag), IFlowsheet.MessageType.Information);
                                }
                                if (connector.IsEnergyConnector)
                                {
                                    flowsheet.ConnectObjects(gobj, stream.GraphicObject, 0, 0);
                                }
                                else
                                {
                                    flowsheet.ConnectObjects(gobj, stream.GraphicObject, gobj.OutputConnectors.IndexOf(connector), 0);
                                }
                                flowsheet.ShowMessage(String.Format("Connected {0} to {1}.", gobj.Tag, stream.GraphicObject.Tag), IFlowsheet.MessageType.Information);
                                cbConnection.Items.Add(cbConnection.Text);
                                var list = new List<string>(options);
                                list.Add(cbConnection.Text);
                                options = list;
                            }
                            catch (Exception ex)
                            {
                                flowsheet.ShowMessage(ex.Message.ToString(), IFlowsheet.MessageType.GeneralError);
                            }
                        }
                    }
                    else
                    {
                        schangedaction.Invoke(cbConnection, e);
                    }
                }
            };

            cbConnection.SelectedValueChanged += (sender, e) => schangedaction.Invoke(sender, e);

            cbConnection.Tag = true;

            tl.Dock = DockStyle.Fill;

            PropertiesLayout.Controls.Add(tl);

            return;

        }

    }
}
