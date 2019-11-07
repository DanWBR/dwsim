using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using Eto.Forms;

using s = DWSIM.UI.Shared.Common;

namespace DWSIM.UI.Desktop.Editors
{
    public class ConnectionsEditor
    {

        public ISimulationObject SimObject;

        public TableLayout container;

        public ConnectionsEditor(ISimulationObject selectedobject, DynamicLayout layout)
        {
            SimObject = selectedobject;
            container = new TableLayout();
            Initialize();
            layout.Add(container);
        }

        void Initialize()
        {

            var mslist = SimObject.GetFlowsheet().GraphicObjects.Values.Where((x) => x.ObjectType == ObjectType.MaterialStream).Select((m) => m.Tag).ToList();
            var eslist = SimObject.GetFlowsheet().GraphicObjects.Values.Where((x) => x.ObjectType == ObjectType.EnergyStream).Select((m) => m.Tag).ToList();

            mslist.Insert(0, "");
            eslist.Insert(0, "");

            foreach (var cp in SimObject.GraphicObject.InputConnectors)
            {
                if (cp.Type != ConType.ConEn)
                {
                    CreateAndAddRow("In", cp, mslist);
                }
            }

            foreach (var cp in SimObject.GraphicObject.OutputConnectors)
            {
                if (cp.Type != ConType.ConEn)
                {
                    CreateAndAddRow("Out", cp, mslist);
                }
            }

            foreach (var cp in SimObject.GraphicObject.InputConnectors)
            {
                if (cp.Type == ConType.ConEn)
                {
                    CreateAndAddRow("In", cp, eslist);
                }
            }

            foreach (var cp in SimObject.GraphicObject.OutputConnectors)
            {
                if (cp.Type == ConType.ConEn)
                {
                    CreateAndAddRow("Out", cp, eslist);
                }
            }

            if (SimObject.GraphicObject.EnergyConnector.Active) { CreateAndAddRow("Out", SimObject.GraphicObject.EnergyConnector, eslist); }

        }

        void CreateAndAddRow(string direction, IConnectionPoint connector, IEnumerable<String> options)
        {

            DynamicLayout cont1 = new DynamicLayout();

            var cbConnection = s.CreateAndAddEditableDropDownRow(cont1, connector.ConnectorName, options.ToList(), 0, null);

            if (connector.IsAttached)
            {
                if (connector.AttachedConnector != null)
                {
                    if (direction == "In" && (connector.Type == ConType.ConIn | connector.Type == ConType.ConEn))
                    {
                        if (connector.IsEnergyConnector)
                        {
                            if (options.Contains(connector.AttachedConnector.AttachedTo.Tag))
                            {
                                cbConnection.SelectedIndex = Array.IndexOf(options.ToArray(), connector.AttachedConnector.AttachedTo.Tag);
                            }
                        }
                        else
                        {
                            if (options.Contains(connector.AttachedConnector.AttachedFrom.Tag))
                            {
                                cbConnection.SelectedIndex = Array.IndexOf(options.ToArray(), connector.AttachedConnector.AttachedFrom.Tag);
                            }
                        }
                    }
                    else if (connector.Type == ConType.ConOut | connector.Type == ConType.ConEn)
                    {
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

                var fgui = (Interfaces.IFlowsheet)SimObject.GetFlowsheet();

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
                            return;
                        }
                        else if (connector.Type == ConType.ConOut | connector.Type == ConType.ConEn)
                        {
                            try
                            {
                                var objto = connector.AttachedConnector.AttachedTo;
                                flowsheet.DisconnectObjects(gobj, objto);
                                flowsheet.ShowMessage(String.Format("Disconnected {0} from {1}", gobj.Tag, objto.Tag), IFlowsheet.MessageType.Information);
                            }
                            catch (Exception ex)
                            {
                                flowsheet.ShowMessage(ex.Message.ToString(), IFlowsheet.MessageType.GeneralError); ;
                            }
                            fgui.UpdateInterface();
                            return;
                        }
                    }

                    if (sel != "")
                    {

                        var gobj2 = flowsheet.GetFlowsheetSimulationObject(sel).GraphicObject;

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
                                    flowsheet.ShowMessage(String.Format("Disconnected {0} from {1}", gobj.Tag, objto.Tag), IFlowsheet.MessageType.Information);
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

                    }
                }

            };

            cbConnection.KeyUp += (sender, e) =>
            {
                if (e.Key == Keys.Enter)
                {
                    var items = cbConnection.Items.Select((x) => x.Text).ToList();
                    if (!items.Contains(cbConnection.Text))
                    {
                        var gobj = SimObject.GraphicObject;
                        var flowsheet = SimObject.GetFlowsheet();
                        int posx, posy;
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
                                stream = flowsheet.AddObject(ObjectType.EnergyStream, posx, posy, cbConnection.Text);
                            }
                            else
                            {
                                stream = flowsheet.AddObject(ObjectType.MaterialStream, posx, posy, cbConnection.Text);
                            }
                        }
                        ((Drawing.SkiaSharp.GraphicObjects.GraphicObject)stream.GraphicObject).CreateConnectors(0, 0);
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

            container.Rows.Add(new TableRow(cont1));

            return;

        }


    }
}
