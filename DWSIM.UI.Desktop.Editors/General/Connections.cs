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

            if (SimObject.GraphicObject.EnergyConnector.Active) { CreateAndAddRow("In", SimObject.GraphicObject.EnergyConnector, eslist); }

        }

        void CreateAndAddRow(string direction, IConnectionPoint connector, IEnumerable<String> options)
        {

            DynamicLayout cont1 = new DynamicLayout();

            var cbConnection = s.CreateAndAddDropDownRow(cont1, connector.ConnectorName, options.ToList(), 0, null);

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

            cbConnection.SelectedValueChanged += (sender, e) =>
            {
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
                                flowsheet.DisconnectObjects(connector.AttachedConnector.AttachedFrom, gobj);
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
                                flowsheet.DisconnectObjects(gobj, connector.AttachedConnector.AttachedTo);
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
                                    flowsheet.DisconnectObjects(connector.AttachedConnector.AttachedFrom, gobj);
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
                                    flowsheet.DisconnectObjects(gobj, connector.AttachedConnector.AttachedTo);
                                }
                                flowsheet.ConnectObjects(gobj, gobj2, gobj.OutputConnectors.IndexOf(connector), 0);
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

            cbConnection.Tag = true;

            container.Rows.Add(new TableRow(cont1));

            return;

        }


    }
}
