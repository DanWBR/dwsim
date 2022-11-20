Imports CapeOpen
Imports Controls.PropertyGridEx
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes
Imports DWSIM.Interfaces
Imports DWSIM.SharedClasses.SystemsOfUnits
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.SepOps
Imports System.Linq

Public Class PropertyChanged

    Public Shared Sub PropertyValueChanged(ByVal s As Object, ByVal e As System.Windows.Forms.PropertyValueChangedEventArgs)

        Dim Flowsheet = My.Application.ActiveSimulation

        Dim sobj As GraphicObject = Flowsheet.FormSurface.FlowsheetSurface.SelectedObject

        If Not sobj Is Nothing Then

            'connections
            If sobj.ObjectType = ObjectType.Cooler Or sobj.ObjectType = ObjectType.Pipe Or sobj.ObjectType = ObjectType.Expander Then
                If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + sobj.Height / 2 - 10, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesada")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + sobj.Height / 2 - 10, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeenergia")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X + sobj.Width + 40, sobj.Y + sobj.Height + 30, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.EnergyConnector.IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        End If
                    End If
                End If
            ElseIf sobj.ObjectType = ObjectType.ExcelUO Then
                If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada1")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 45, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada2")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 15, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(1).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada3")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 15, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(2).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada4")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 45, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(3).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida1")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y - 45, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida2")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y - 15, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(1).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida3")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 15, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(2).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida4")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 45, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(3).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(3).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(3).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeenergia")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X - 60, sobj.Y + 75, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(4).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                End If
            ElseIf sobj.ObjectType = ObjectType.Compressor Or sobj.ObjectType = ObjectType.Heater Or sobj.ObjectType = ObjectType.Pump Or
                         sobj.ObjectType = ObjectType.RCT_PFR Or sobj.ObjectType = ObjectType.RCT_CSTR Then
                If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + sobj.Height / 2 - 10, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesada")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + sobj.Height / 2 - 10, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeenergia")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X - 60, sobj.Y + sobj.Height + 20, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(1).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                End If
            ElseIf sobj.ObjectType = ObjectType.Valve Or sobj.ObjectType = ObjectType.OrificePlate Or sobj.ObjectType = ObjectType.OT_Recycle Or sobj.ObjectType = ObjectType.Tank Then
                If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + sobj.Height / 2 - 10, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesada")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + sobj.Height / 2 - 10, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        End If
                    End If
                End If
            ElseIf sobj.ObjectType = ObjectType.Vessel Then
                If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada1")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 50, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada2")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 20, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(1).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 1)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 1)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada3")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 10, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(2).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 2)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 2)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada4")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 40, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(3).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 3)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 3)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada5")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 70, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(4).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 4)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 4)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada6")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 100, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(5).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 5)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(5).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 5)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(5).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label = (DWSIM.App.GetLocalString("Saidadevapor")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y - 20, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 0, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 0, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label = (DWSIM.App.GetLocalString("Saidadelquido")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 50, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(1).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 1, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 1, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label = (DWSIM.App.GetLocalString("Saidadelquido") & " (2)") Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 100, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(2).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 2, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 2, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeenergia")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X - 60, sobj.Y + 130, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(6).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(6).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(6).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                End If
            ElseIf sobj.ObjectType = ObjectType.FlowsheetUO Then
                If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada1")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 60, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada2")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 30, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(1).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 1)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 1)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada3")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 0, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(2).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 2)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 2)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada4")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 30, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(3).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 3)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 3)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada5")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 60, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(4).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 4)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 4)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada6")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 90, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(5).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 5)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(5).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 5)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(5).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada7")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 120, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(6).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 6)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(6).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 6)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(6).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada8")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 150, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(7).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 7)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(7).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 7)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(7).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada9")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 180, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(8).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 8)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(8).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 8)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(8).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada10")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 210, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(9).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 9)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(9).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 9)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(9).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida1")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y - 60, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida2")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y - 30, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(1).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida3")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 0, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(2).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida4")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 30, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(3).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(3).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(3).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida5")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 60, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(4).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(4).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(4).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida6")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 90, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(5).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(5).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(5).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida7")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 120, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(6).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(6).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(6).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida8")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 150, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(7).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(7).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(7).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida9")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 180, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(8).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(8).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(8).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida10")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 210, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(9).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(9).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(9).AttachedConnector.AttachedTo)
                        End If
                    End If
                End If
            ElseIf sobj.ObjectType = ObjectType.RCT_Conversion Or sobj.ObjectType = ObjectType.RCT_Equilibrium Or sobj.ObjectType = ObjectType.RCT_Gibbs Then
                If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + sobj.Height / 2 - 10, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label = (DWSIM.App.GetLocalString("Saidadevapor")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 0.17 * sobj.Height - 10, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 0, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 0, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label = (DWSIM.App.GetLocalString("Saidadelquido")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + sobj.Height * 0.843 - 10, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(1).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 1, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 1, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeenergia")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X - 60, sobj.Y + sobj.Height + 20, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(1).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                End If
            ElseIf sobj.ObjectType = ObjectType.NodeIn Then
                If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada1")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 75, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada2")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 45, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(1).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 1)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 1)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada3")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 15, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(2).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 2)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 2)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada4")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 15, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(3).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 3)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 3)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada5")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 45, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(4).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 4)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 4)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada6")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 75, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(5).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 5)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(5).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 5)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(5).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Conectadoasada")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        End If
                    End If
                End If
            ElseIf sobj.ObjectType = ObjectType.NodeOut Then
                If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida1")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y - 30, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 0, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 0, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida2")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(1).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 1, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 1, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida3")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 30, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(2).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 2, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 2, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                        End If
                    End If
                End If
            ElseIf sobj.ObjectType = ObjectType.ShortcutColumn Then
                If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("SCFeed")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 0.5 * sobj.Height - 10, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("SCReboilerDuty")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X - 60, sobj.Y + sobj.Height + 20, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(1).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 1)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 1)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("SCDistillate")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 0.3 * sobj.Height - 10, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 0, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 0, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("SCBottoms")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 0.98 * sobj.Height - 10, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(1).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 1, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 1, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("SCCondenserDuty")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X + sobj.Width + 40, sobj.Y + 0.175 * sobj.Height - 30, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.EnergyConnector.IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.EnergyConnector.AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.EnergyConnector.AttachedConnector.AttachedTo)
                        End If
                    End If
                End If
            ElseIf sobj.ObjectType = ObjectType.HeatExchanger Then
                If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada1")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + sobj.Height / 2 - 10, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada2")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + sobj.Height / 2 - 50, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(1).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida1")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + sobj.Height / 2 - 10, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 0, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 0, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida2")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + sobj.Height / 2 + 30, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(1).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 1, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 1, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                        End If
                    End If
                End If
            ElseIf sobj.ObjectType = ObjectType.OT_EnergyRecycle Then
                If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X - 60, sobj.Y, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesada")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X + sobj.Width + 40, sobj.Y, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        End If
                    End If
                End If
            ElseIf sobj.ObjectType = ObjectType.ComponentSeparator Or sobj.ObjectType = ObjectType.SolidSeparator Or sobj.ObjectType = ObjectType.Filter Then
                If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + sobj.Height * 0.5 - 10, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("OutletStream1")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 0.17 * sobj.Height - 10, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 0, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 0, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("OutletStream2")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + sobj.Height * 0.83 - 10, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(1).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 1, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 1, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeenergia")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X + sobj.Width + 40, sobj.Y + sobj.Height + 20, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.EnergyConnector.IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.EnergyConnector.AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.EnergyConnector.AttachedConnector.AttachedTo)
                        End If
                    End If
                End If
            ElseIf sobj.ObjectType = ObjectType.CustomUO Then
                If e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada1")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 65, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(0).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada2")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 35, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(1).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 1)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 1)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(1).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada3")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y - 5, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(2).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 2)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 2)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(2).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada4")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 25, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(4).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 4)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 4)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(4).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada5")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 55, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(5).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 5)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(5).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 5)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(5).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedeentrada6")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X - 60, sobj.Y + 85, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(6).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 6)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(6).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj, 0, 6)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(6).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("CorrentedeenergiaE")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X - 60, sobj.Y + 115, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.InputConnectors(3).IsAttached Then
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        Else
                            Flowsheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                            Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), sobj)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj.InputConnectors(3).AttachedConnector.AttachedFrom, sobj)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida1")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y - 65, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(0).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 0, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 0, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida2")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y - 35, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(1).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 1, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 1, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(1).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida3")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y - 5, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(2).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 2, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 2, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(2).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida4")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 25, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(4).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 4, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(4).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 4, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(4).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida5")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 55, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(5).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 5, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(5).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 5, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(5).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("Correntedesaida6")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, sobj.X + sobj.Width + 40, sobj.Y + 85, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(6).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 6, 0)
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(6).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), 6, 0)
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(6).AttachedConnector.AttachedTo)
                        End If
                    End If
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("CorrentedeenergiaS")) Then
                    If e.ChangedItem.Value <> "" Then
                        If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                            Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, sobj.X + sobj.Width + 40, sobj.Y + 115, e.ChangedItem.Value)
                        ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                            MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Exit Sub
                        End If
                        If Not sobj.OutputConnectors(3).IsAttached Then
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        Else
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(3).AttachedConnector.AttachedTo)
                            Flowsheet.ConnectObject(sobj, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface))
                        End If
                    Else
                        If e.OldValue.ToString <> "" Then
                            Flowsheet.DisconnectObject(sobj, sobj.OutputConnectors(3).AttachedConnector.AttachedTo)
                        End If
                    End If
                End If
            End If

        End If

    End Sub

    Public Shared Sub PropertyValueChanged(ByVal s As Object, ByVal e As System.Windows.Forms.PropertyValueChangedEventArgs, ByVal uo As CapeOpenUO)

        Dim Flowsheet = My.Application.ActiveSimulation

        BasePropertyValueChanged(s, e, uo)

        If e.ChangedItem.Parent.Label.Contains("Parameters") Then

            uo.RestoreParams()

        ElseIf e.ChangedItem.Parent.Label.Contains("Ports") Then
            Dim index, indexc, i As Integer
            i = 0
            For Each gi As GridItem In e.ChangedItem.Parent.GridItems
                If gi.Label = e.ChangedItem.Label Then
                    index = i
                    Exit For
                End If
                i += 1
            Next
            If e.ChangedItem.Label.Contains("[CAPE_INLET, CAPE_MATERIAL]") Then
                For Each p As UnitPort In uo._ports
                    i = 0
                    If e.ChangedItem.Label.Contains(p.ComponentName) Then
                        For Each c As ConnectionPoint In uo.GraphicObject.InputConnectors
                            If p.ComponentName = c.ConnectorName And
                                p.direction = CapePortDirection.CAPE_INLET And
                                p.portType = CapePortType.CAPE_MATERIAL Then
                                indexc = i
                                Exit For
                            End If
                            i += 1
                        Next
                    End If
                Next
                If e.ChangedItem.Value <> "" Then
                    If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                        Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, uo.GraphicObject.X - 40, uo.GraphicObject.Y, e.ChangedItem.Value)
                    ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                        MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                        Exit Sub
                    End If
                    If uo.GraphicObject.InputConnectors(indexc).IsAttached Then
                        Flowsheet.DisconnectObject(uo.GraphicObject.InputConnectors(indexc).AttachedConnector.AttachedFrom, uo.GraphicObject)
                        uo._ports(index).Disconnect()
                    End If
                    Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), uo.GraphicObject, 0, indexc)
                    uo._ports(index).Connect(Flowsheet.GetFlowsheetSimulationObject(e.ChangedItem.Value))
                Else
                    If e.OldValue.ToString <> "" Then
                        Flowsheet.DisconnectObject(uo.GraphicObject.InputConnectors(indexc).AttachedConnector.AttachedFrom, uo.GraphicObject)
                        uo._ports(index).Disconnect()
                    End If
                End If
            ElseIf e.ChangedItem.Label.Contains("[CAPE_OUTLET, CAPE_MATERIAL]") Then
                For Each p As UnitPort In uo._ports
                    i = 0
                    If e.ChangedItem.Label.Contains(p.ComponentName) Then
                        For Each c As ConnectionPoint In uo.GraphicObject.OutputConnectors
                            If p.ComponentName = c.ConnectorName And
                                p.direction = CapePortDirection.CAPE_OUTLET And
                                p.portType = CapePortType.CAPE_MATERIAL Then
                                indexc = i
                                Exit For
                            End If
                            i += 1
                        Next
                    End If
                Next
                If e.ChangedItem.Value <> "" Then
                    If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                        Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.MaterialStream, uo.GraphicObject.X + uo.GraphicObject.Width + 40, uo.GraphicObject.Y, e.ChangedItem.Value)
                    ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                        MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                        Exit Sub
                    End If
                    If uo.GraphicObject.OutputConnectors(indexc).IsAttached Then
                        Flowsheet.DisconnectObject(uo.GraphicObject, uo.GraphicObject.OutputConnectors(indexc).AttachedConnector.AttachedTo)
                        uo._ports(index).Disconnect()
                    End If
                    Flowsheet.ConnectObject(uo.GraphicObject, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), indexc, 0)
                    uo._ports(index).Connect(Flowsheet.GetFlowsheetSimulationObject(e.ChangedItem.Value))
                Else
                    If e.OldValue.ToString <> "" Then
                        Flowsheet.DisconnectObject(uo.GraphicObject, uo.GraphicObject.OutputConnectors(indexc).AttachedConnector.AttachedTo)
                        uo._ports(index).Disconnect()
                    End If
                End If
            ElseIf e.ChangedItem.Label.Contains("[CAPE_INLET, CAPE_ENERGY]") Then
                For Each p As UnitPort In uo._ports
                    i = 0
                    If e.ChangedItem.Label.Contains(p.ComponentName) Then
                        For Each c As ConnectionPoint In uo.GraphicObject.InputConnectors
                            If p.ComponentName = c.ConnectorName And
                                p.direction = CapePortDirection.CAPE_INLET And
                                p.portType = CapePortType.CAPE_ENERGY Then
                                indexc = i
                                Exit For
                            End If
                            i += 1
                        Next
                    End If
                Next
                If e.ChangedItem.Value <> "" Then
                    If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                        Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, uo.GraphicObject.X - 40, uo.GraphicObject.Y, e.ChangedItem.Value)
                    ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).OutputConnectors(0).IsAttached Then
                        MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                        Exit Sub
                    End If
                    If uo.GraphicObject.InputConnectors(indexc).IsAttached Then
                        Flowsheet.DisconnectObject(uo.GraphicObject.InputConnectors(indexc).AttachedConnector.AttachedFrom, uo.GraphicObject)
                        uo._ports(index).Disconnect()
                    End If
                    Flowsheet.ConnectObject(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), uo.GraphicObject, 0, indexc)
                    uo._ports(index).Connect(Flowsheet.GetFlowsheetSimulationObject(e.ChangedItem.Value))
                Else
                    If e.OldValue.ToString <> "" Then
                        Flowsheet.DisconnectObject(uo.GraphicObject.InputConnectors(indexc).AttachedConnector.AttachedFrom, uo.GraphicObject)
                        uo._ports(index).Disconnect()
                    End If
                End If
            ElseIf e.ChangedItem.Label.Contains("[CAPE_OUTLET, CAPE_ENERGY]") Then
                For Each p As UnitPort In uo._ports
                    i = 0
                    If e.ChangedItem.Label.Contains(p.ComponentName) Then
                        For Each c As ConnectionPoint In uo.GraphicObject.OutputConnectors
                            If p.ComponentName = c.ConnectorName And
                                p.direction = CapePortDirection.CAPE_OUTLET And
                                p.portType = CapePortType.CAPE_ENERGY Then
                                indexc = i
                                Exit For
                            End If
                            i += 1
                        Next
                    End If
                Next
                If e.ChangedItem.Value <> "" Then
                    If FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface) Is Nothing Then
                        Dim oguid As String = Flowsheet.FormSurface.AddObjectToSurface(ObjectType.EnergyStream, uo.GraphicObject.X + uo.GraphicObject.Width + 40, uo.GraphicObject.Y, e.ChangedItem.Value)
                    ElseIf CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), GraphicObject).InputConnectors(0).IsAttached Then
                        MessageBox.Show(DWSIM.App.GetLocalString("Todasasconexespossve"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                        Exit Sub
                    End If
                    If uo.GraphicObject.OutputConnectors(indexc).IsAttached Then
                        Flowsheet.DisconnectObject(uo.GraphicObject, uo.GraphicObject.OutputConnectors(indexc).AttachedConnector.AttachedTo)
                        uo._ports(index).Disconnect()
                    End If
                    Flowsheet.ConnectObject(uo.GraphicObject, FormFlowsheet.SearchSurfaceObjectsByTag(e.ChangedItem.Value, Flowsheet.FormSurface.FlowsheetSurface), indexc, 0)
                    uo._ports(index).Connect(Flowsheet.GetFlowsheetSimulationObject(e.ChangedItem.Value))
                Else
                    If e.OldValue.ToString <> "" Then
                        Flowsheet.DisconnectObject(uo.GraphicObject, uo.GraphicObject.OutputConnectors(indexc).AttachedConnector.AttachedTo)
                        uo._ports(index).Disconnect()
                    End If
                End If
            End If

            uo.UpdateConnectorPositions()

        End If

    End Sub

    Public Shared Sub BasePropertyValueChanged(ByVal s As Object, ByVal e As System.Windows.Forms.PropertyValueChangedEventArgs, uo As ISimulationObject)

        If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("UOPropertyPackage")) Then
            If e.ChangedItem.Value <> "" Then
                If uo.Flowsheet.Options.PropertyPackages.ContainsKey(e.ChangedItem.Value) Then
                    uo.PropertyPackage = uo.Options.PropertyPackages(e.ChangedItem.Value)
                End If
            End If
        End If

    End Sub

    Public Shared Sub HandlePropertyChange(ByVal s As Object, ByVal e As System.Windows.Forms.PropertyValueChangedEventArgs, uo As ISimulationObject)

        Dim Flowsheet = My.Application.ActiveSimulation

        'handle connection updates

        PropertyValueChanged(s, e)

        'handle other property changes

        Dim sobj As GraphicObject = uo.GraphicObject

        Flowsheet.FormSurface.FlowsheetSurface.SelectedObject = sobj

        If Not sobj Is Nothing Then

            Dim value As Double, units As String
            value = e.ChangedItem.GetValue
            units = e.ChangedItem.GetUnits

            If sobj.ObjectType = ObjectType.FlowsheetUO Then

                Dim fs As Flowsheet = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If e.ChangedItem.PropertyDescriptor.Category.Equals(DWSIM.App.GetLocalString("LinkedInputParms")) Then

                    Dim pkey As String = CType(e.ChangedItem.PropertyDescriptor, CustomProperty.CustomPropertyDescriptor).CustomProperty.Tag

                    fs.Fsheet.Collections.ObjectCollection(fs.InputParams(pkey).ObjectID).SetPropertyValue(fs.InputParams(pkey).ObjectProperty, e.ChangedItem.Value, Flowsheet.Options.SelectedUnitSystem)

                    If Settings.CalculatorActivated Then

                        sobj.Calculated = True
                        Flowsheet.FormProps.HandleObjectStatusChanged(sobj)

                        'Call function to calculate flowsheet
                        Dim objargs As New FlowsheetSolver.CalculationArgs
                        With objargs
                            .Calculated = True
                            .Name = sobj.Name
                            .Tag = sobj.Tag
                            .ObjectType = ObjectType.FlowsheetUO
                            .Sender = "PropertyGrid"
                        End With

                        If fs.IsSpecAttached = True And fs.SpecVarType = SpecVarType.Source Then Flowsheet.Collections.FlowsheetObjectCollection(fs.AttachedSpecId).Calculate()
                        Flowsheet.CalculationQueue.Enqueue(objargs)

                    End If

                End If

            ElseIf sobj.ObjectType = ObjectType.MaterialStream Then

                Dim ms As MaterialStream = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If Not e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Base")) Then

                    Dim T, P, W, Q, QV, HM, SM, VF As Double

                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Temperatura")) Then
                        If units <> "" Then
                            T = Converter.ConvertToSI(units, value)
                        Else
                            T = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                        End If
                        ms.Phases(0).Properties.temperature = T
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Presso")) Then
                        If units <> "" Then
                            P = Converter.ConvertToSI(units, value)
                        Else
                            P = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.pressure, e.ChangedItem.Value)
                        End If
                        ms.Phases(0).Properties.pressure = P
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Vazomssica")) Then
                        If units <> "" Then
                            W = Converter.ConvertToSI(units, value)
                        Else
                            W = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.massflow, e.ChangedItem.Value)
                        End If
                        ms.Phases(0).Properties.massflow = W
                        ms.Phases(0).Properties.molarflow = Nothing
                        ms.Phases(0).Properties.volumetric_flow = Nothing
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Vazomolar")) Then
                        If units <> "" Then
                            Q = Converter.ConvertToSI(units, value)
                        Else
                            Q = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.molarflow, e.ChangedItem.Value)
                        End If
                        ms.Phases(0).Properties.molarflow = Q
                        ms.Phases(0).Properties.massflow = Nothing
                        ms.Phases(0).Properties.volumetric_flow = Nothing
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Vazovolumtrica")) Then
                        If units <> "" Then
                            QV = Converter.ConvertToSI(units, value)
                        Else
                            QV = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.volumetricFlow, e.ChangedItem.Value)
                        End If
                        ms.Phases(0).Properties.volumetric_flow = QV
                        ms.Phases(0).Properties.massflow = Nothing
                        ms.Phases(0).Properties.molarflow = Nothing
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("EntalpiaEspecfica")) Then
                        If units <> "" Then
                            HM = Converter.ConvertToSI(units, value)
                        Else
                            HM = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.enthalpy, e.ChangedItem.Value)
                        End If
                        ms.Phases(0).Properties.enthalpy = HM
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("EntropiaEspecfica")) Then
                        If units <> "" Then
                            SM = Converter.ConvertToSI(units, value)
                        Else
                            SM = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.entropy, e.ChangedItem.Value)
                        End If
                        ms.Phases(0).Properties.entropy = SM
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Vapor")) Then
                        VF = e.ChangedItem.Value
                        ms.Phases(2).Properties.molarfraction = VF
                    End If

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = False
                        .Name = sobj.Name
                        .Tag = sobj.Tag
                        .ObjectType = ObjectType.MaterialStream
                        .Sender = "PropertyGrid"
                    End With

                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.EnergyStream Then

                Dim es As EnergyStream = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Energia")) Then

                    If units <> "" Then
                        es.EnergyFlow = Converter.ConvertToSI(units, value)
                    Else
                        es.EnergyFlow = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.heatflow, e.ChangedItem.Value)
                    End If

                End If

                If Settings.CalculatorActivated Then

                    sobj.Calculated = True
                    Flowsheet.FormProps.HandleObjectStatusChanged(sobj)

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = True
                        .Name = sobj.Name
                        .Tag = sobj.Tag
                        .ObjectType = ObjectType.EnergyStream
                        .Sender = "PropertyGrid"
                    End With

                    If es.IsSpecAttached = True And es.SpecVarType = SpecVarType.Source Then Flowsheet.Collections.FlowsheetObjectCollection(es.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If


            ElseIf sobj.ObjectType = ObjectType.NodeOut Then

                Dim sp As UnitOperations.UnitOperations.Splitter = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)
                sp.OutCount = 0
                For Each cp In sp.GraphicObject.OutputConnectors
                    If cp.IsAttached Then sp.OutCount += 1
                Next
                If e.ChangedItem.Label.Contains("[Split Ratio] ") Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Or Convert.ToDouble(e.ChangedItem.Value) > 1.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))

                    Dim i, j As Integer
                    Dim total As Double

                    Dim cp As ConnectionPoint
                    For Each cp In sp.GraphicObject.OutputConnectors
                        If cp.IsAttached Then
                            If e.ChangedItem.Label.Contains(cp.AttachedConnector.AttachedTo.Tag) Then j = i
                        End If
                        i += 1
                    Next
                    For i = 0 To sp.OutCount - 2
                        If i <> j Then
                            total += sp.Ratios(i)
                        Else
                            total += e.ChangedItem.Value
                        End If
                    Next
                    If total <= 1 Then
                        sp.Ratios(j) = e.ChangedItem.Value
                        sp.Ratios(sp.OutCount - 1) = 1 - total
                    Else
                        Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetPropertyName("PROP_SP_1")) Then

                    Select Case sp.OperationMode
                        Case UnitOperations.UnitOperations.Splitter.OpMode.StreamMassFlowSpec
                            If units <> "" Then
                                sp.StreamFlowSpec = Converter.ConvertToSI(units, value)
                            Else
                                sp.StreamFlowSpec = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.massflow, e.ChangedItem.Value)
                            End If
                        Case UnitOperations.UnitOperations.Splitter.OpMode.StreamMoleFlowSpec
                            If units <> "" Then
                                sp.StreamFlowSpec = Converter.ConvertToSI(units, value)
                            Else
                                sp.StreamFlowSpec = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.molarflow, e.ChangedItem.Value)
                            End If
                    End Select

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetPropertyName("PROP_SP_2")) Then

                    Select Case sp.OperationMode
                        Case UnitOperations.UnitOperations.Splitter.OpMode.StreamMassFlowSpec
                            If units <> "" Then
                                sp.Stream2FlowSpec = Converter.ConvertToSI(units, value)
                            Else
                                sp.Stream2FlowSpec = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.molarflow, e.ChangedItem.Value)
                            End If
                        Case UnitOperations.UnitOperations.Splitter.OpMode.StreamMoleFlowSpec
                            If units <> "" Then
                                sp.Stream2FlowSpec = Converter.ConvertToSI(units, value)
                            Else
                                sp.Stream2FlowSpec = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.molarflow, e.ChangedItem.Value)
                            End If
                    End Select

                End If

                If Settings.CalculatorActivated Then

                    sobj.Calculated = True

                    Flowsheet.FormProps.HandleObjectStatusChanged(sobj)

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = False
                        .Name = sobj.Name
                        .Tag = sobj.Tag
                        .ObjectType = ObjectType.NodeOut
                        .Sender = "PropertyGrid"
                    End With

                    If sp.IsSpecAttached = True And sp.SpecVarType = SpecVarType.Source Then Flowsheet.Collections.FlowsheetObjectCollection(sp.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.Pump Then

                Dim bb As Pump = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If e.ChangedItem.Label.Contains("Delta P") Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.DeltaP = Converter.ConvertToSI(units, value)
                    Else
                        bb.DeltaP = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Pressoajusante")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.Pout = Converter.ConvertToSI(units, value)
                    Else
                        bb.Pout = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.pressure, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Eficincia")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) <= 20.0# Or Convert.ToDouble(e.ChangedItem.Value) > 100.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))

                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = False
                        .Tag = sobj.Tag
                        .Name = sobj.Name
                        .ObjectType = ObjectType.Pump
                        .Sender = "PropertyGrid"
                    End With

                    If bb.IsSpecAttached = True And bb.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(bb.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.Valve Then

                Dim bb As Valve = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Quedadepresso")) Then

                    If units <> "" Then
                        bb.DeltaP = Converter.ConvertToSI(units, value)
                    Else
                        bb.DeltaP = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                    End If

                    'If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("ValveOutletPressure")) Then

                    If units <> "" Then
                        bb.OutletPressure = Converter.ConvertToSI(units, value)
                    Else
                        bb.OutletPressure = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.pressure, e.ChangedItem.Value)
                    End If

                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = False
                        .Tag = sobj.Tag
                        .Name = sobj.Name
                        .ObjectType = ObjectType.Valve
                        .Sender = "PropertyGrid"
                    End With

                    If bb.IsSpecAttached = True And bb.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(bb.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.Filter Then

                Dim ft As Filter = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("FilterMediumResistance")) Then
                    If units <> "" Then
                        ft.FilterMediumResistance = Converter.ConvertToSI(units, value)
                    Else
                        ft.FilterMediumResistance = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.mediumresistance, e.ChangedItem.Value)
                    End If
                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("FilterSpecificCakeResistance")) Then
                    If units <> "" Then
                        ft.SpecificCakeResistance = Converter.ConvertToSI(units, value)
                    Else
                        ft.SpecificCakeResistance = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.cakeresistance, e.ChangedItem.Value)
                    End If
                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("FilterCycleTime")) Then
                    If units <> "" Then
                        ft.FilterCycleTime = Converter.ConvertToSI(units, value)
                    Else
                        ft.FilterCycleTime = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.time, e.ChangedItem.Value)
                    End If
                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("FilterPressureDrop")) Then
                    If units <> "" Then
                        ft.PressureDrop = Converter.ConvertToSI(units, value)
                    Else
                        ft.PressureDrop = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                    End If
                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("FilterArea")) Then
                    If units <> "" Then
                        ft.TotalFilterArea = Converter.ConvertToSI(units, value)
                    Else
                        ft.TotalFilterArea = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.area, e.ChangedItem.Value)
                    End If
                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = False
                        .Tag = sobj.Tag
                        .Name = sobj.Name
                        .ObjectType = sobj.ObjectType
                        .Sender = "PropertyGrid"
                    End With

                    If ft.IsSpecAttached = True And ft.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(ft.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.Compressor Then

                Dim bb As Compressor = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If e.ChangedItem.Label.Contains("Delta P") Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.DeltaP = Converter.ConvertToSI(units, value)
                    Else
                        bb.DeltaP = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Presso")) Then
                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.POut = Converter.ConvertToSI(units, value)
                    Else
                        bb.POut = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.pressure, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Eficincia")) Then
                    If Convert.ToDouble(e.ChangedItem.Value) <= 20.0# Or Convert.ToDouble(e.ChangedItem.Value) > 100.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))

                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = False
                        .Tag = sobj.Tag
                        .Name = sobj.Name
                        .ObjectType = ObjectType.Compressor
                        .Sender = "PropertyGrid"
                    End With

                    If bb.IsSpecAttached = True And bb.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(bb.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.Expander Then

                Dim bb As Expander = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If e.ChangedItem.Label.Contains("Delta P") Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.DeltaP = Converter.ConvertToSI(units, value)
                    Else
                        bb.DeltaP = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Presso")) Then
                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.POut = Converter.ConvertToSI(units, value)
                    Else
                        bb.POut = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.pressure, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Eficincia")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) <= 20.0# Or Convert.ToDouble(e.ChangedItem.Value) > 100.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))

                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = False
                        .Tag = sobj.Tag
                        .Name = sobj.Name
                        .ObjectType = ObjectType.Expander
                        .Sender = "PropertyGrid"
                    End With

                    If bb.IsSpecAttached = True And bb.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(bb.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.Pipe Then

                Dim bb As Pipe = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature")) Then

                    If units <> "" Then
                        bb.OutletTemperature = Converter.ConvertToSI(units, value)
                    Else
                        bb.OutletTemperature = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("ValveOutletPressure")) Then

                    If units <> "" Then
                        bb.OutletPressure = Converter.ConvertToSI(units, value)
                    Else
                        bb.OutletPressure = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.pressure, e.ChangedItem.Value)
                    End If

                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = False
                        .Tag = sobj.Tag
                        .Name = sobj.Name
                        .ObjectType = ObjectType.Pipe
                        .Sender = "PropertyGrid"
                    End With

                    If bb.IsSpecAttached = True And bb.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(bb.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.Heater Then

                Dim bb As Heater = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Eficincia")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) <= 20.0# Or Convert.ToDouble(e.ChangedItem.Value) > 100.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Calor")) Then

                    If units <> "" Then
                        bb.DeltaQ = Converter.ConvertToSI(units, value)
                    Else
                        bb.DeltaQ = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.heatflow, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Quedadepresso")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.DeltaP = Converter.ConvertToSI(units, value)
                    Else
                        bb.DeltaP = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature")) Then

                    If units <> "" Then
                        bb.OutletTemperature = Converter.ConvertToSI(units, value)
                    Else
                        bb.OutletTemperature = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("FraomolardafaseFaseV")) Then

                    bb.OutletVaporFraction = Double.Parse(e.ChangedItem.Value)

                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = False
                        .Tag = sobj.Tag
                        .Name = sobj.Name
                        .ObjectType = ObjectType.Heater
                        .Sender = "PropertyGrid"
                    End With

                    If bb.IsSpecAttached = True And bb.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(bb.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.Cooler Then

                Dim bb As Cooler = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Eficincia")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) <= 20.0# Or Convert.ToDouble(e.ChangedItem.Value) > 100.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Calor")) Then

                    If units <> "" Then
                        bb.DeltaQ = Converter.ConvertToSI(units, value)
                    Else
                        bb.DeltaQ = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.heatflow, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Quedadepresso")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.DeltaP = Converter.ConvertToSI(units, value)
                    Else
                        bb.DeltaP = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature")) Then

                    If units <> "" Then
                        bb.OutletTemperature = Converter.ConvertToSI(units, value)
                    Else
                        bb.OutletTemperature = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("FraomolardafaseFaseV")) Then

                    bb.OutletVaporFraction = Double.Parse(e.ChangedItem.Value)

                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = False
                        .Tag = sobj.Tag
                        .Name = sobj.Name
                        .ObjectType = ObjectType.Cooler
                        .Sender = "PropertyGrid"
                    End With

                    If bb.IsSpecAttached = True And bb.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(bb.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.Tank Then

                Dim bb As Tank = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Quedadepresso")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.DeltaP = Converter.ConvertToSI(units, value)
                    Else
                        bb.DeltaP = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("AquecimentoResfriame")) Then

                    If units <> "" Then
                        bb.DeltaQ = Converter.ConvertToSI(units, value)
                    Else
                        bb.DeltaQ = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.heatflow, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("TKVol")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.Volume = Converter.ConvertToSI(units, value)
                    Else
                        bb.Volume = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.volume, e.ChangedItem.Value)
                    End If

                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Tag = sobj.Tag
                        .Calculated = False
                        .Name = sobj.Name
                        .ObjectType = ObjectType.Tank
                        .Sender = "PropertyGrid"
                    End With

                    If bb.IsSpecAttached = True And bb.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(bb.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.OT_Adjust Then

                Dim adj As Adjust = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                With adj
                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("VarivelControlada")) Then
                        .ControlledObject = Flowsheet.Collections.FlowsheetObjectCollection(.ControlledObjectData.m_ID)
                        .ControlledVariable = .ControlledObjectData.m_Property
                        CType(Flowsheet.Collections.FlowsheetObjectCollection(adj.Name).GraphicObject, AdjustGraphic).ConnectedToCv = .ControlledObject.GraphicObject
                        .ReferenceObject = Nothing
                        .ReferenceVariable = Nothing
                        With .ReferencedObjectData
                            .m_ID = ""
                            .m_Name = ""
                            .m_Property = ""
                            .m_Type = ""
                        End With
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("VarivelManipulada")) Then
                        .ManipulatedObject = Flowsheet.Collections.FlowsheetObjectCollection(.ManipulatedObjectData.m_ID)
                        Dim gr As AdjustGraphic = Flowsheet.Collections.FlowsheetObjectCollection(adj.Name).GraphicObject
                        gr.ConnectedToMv = .ManipulatedObject.GraphicObject
                        .ManipulatedVariable = .ManipulatedObjectData.m_Property
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("ObjetoVariveldeRefer")) Then
                        .ReferenceObject = Flowsheet.Collections.FlowsheetObjectCollection(.ReferencedObjectData.m_ID)
                        .ReferenceVariable = .ReferencedObjectData.m_Property
                        Dim gr As AdjustGraphic = Flowsheet.Collections.FlowsheetObjectCollection(adj.Name).GraphicObject
                        gr.ConnectedToRv = .ReferenceObject.GraphicObject
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Valormnimoopcional")) Then
                        adj.MinVal = Converter.ConvertToSI(adj.ManipulatedObject.GetPropertyUnit(adj.ManipulatedObjectData.m_Property, Flowsheet.Options.SelectedUnitSystem), e.ChangedItem.Value)
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Valormximoopcional")) Then
                        adj.MaxVal = Converter.ConvertToSI(adj.ManipulatedObject.GetPropertyUnit(adj.ManipulatedObjectData.m_Property, Flowsheet.Options.SelectedUnitSystem), e.ChangedItem.Value)
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("ValordeAjusteouOffse")) Then
                        adj.AdjustValue = Converter.ConvertToSI(adj.ControlledObject.GetPropertyUnit(adj.ControlledObjectData.m_Property, Flowsheet.Options.SelectedUnitSystem), e.ChangedItem.Value)
                    End If
                End With

            ElseIf sobj.ObjectType = ObjectType.OT_Spec Then

                Dim spec As Spec = Flowsheet.SimulationObjects.Item(sobj.Name)

                With spec
                    If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("VarivelDestino")) Then
                        .TargetObject = Flowsheet.Collections.FlowsheetObjectCollection(.TargetObjectData.ID)
                        CType(Flowsheet.Collections.FlowsheetObjectCollection(spec.Name).GraphicObject, SpecGraphic).ConnectedToTv = .TargetObject.GraphicObject
                    ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("VarivelFonte")) Then
                        .SourceObject = Flowsheet.Collections.FlowsheetObjectCollection(.SourceObjectData.ID)
                        Dim gr As SpecGraphic = Flowsheet.Collections.FlowsheetObjectCollection(spec.Name).GraphicObject
                        gr.ConnectedToSv = .SourceObject.GraphicObject
                    End If
                End With

            ElseIf sobj.ObjectType = ObjectType.Vessel Then

                Dim vessel As Vessel = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                Dim T, P As Double
                If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Temperatura")) Then
                    If units <> "" Then
                        T = Converter.ConvertToSI(units, value)
                    Else
                        T = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                    End If
                    vessel.FlashTemperature = T
                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Presso")) Then
                    If units <> "" Then
                        P = Converter.ConvertToSI(units, value)
                    Else
                        P = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.pressure, e.ChangedItem.Value)
                    End If
                    vessel.FlashPressure = P
                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Tag = sobj.Tag
                        .Calculated = False
                        .Name = sobj.Name
                        .ObjectType = ObjectType.Vessel
                        .Sender = "PropertyGrid"
                    End With

                    If vessel.IsSpecAttached = True And vessel.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(vessel.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.OT_Recycle Then

                Dim rec As Recycle = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                Dim T, P, W As Double
                If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Temperatura")) Then
                    T = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.deltaT, e.ChangedItem.Value)
                    rec.ConvergenceParameters.Temperatura = T
                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Presso")) Then
                    P = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                    rec.ConvergenceParameters.Pressao = P
                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("mssica")) Then
                    W = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.massflow, e.ChangedItem.Value)
                    rec.ConvergenceParameters.VazaoMassica = W
                End If

            ElseIf sobj.ObjectType = ObjectType.RCT_Conversion Then

                Dim bb As Reactor_Conversion = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Quedadepresso")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.DeltaP = Converter.ConvertToSI(units, value)
                    Else
                        bb.DeltaP = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature")) Then

                    If units <> "" Then
                        bb.OutletTemperature = Converter.ConvertToSI(units, value)
                    Else
                        bb.OutletTemperature = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                    End If

                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = False
                        .Tag = sobj.Tag
                        .Name = sobj.Name
                        .ObjectType = ObjectType.RCT_Conversion
                        .Sender = "PropertyGrid"
                    End With

                    If bb.IsSpecAttached = True And bb.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(bb.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.RCT_Equilibrium Then

                Dim bb As Reactor_Equilibrium = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Quedadepresso")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.DeltaP = Converter.ConvertToSI(units, value)
                    Else
                        bb.DeltaP = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature")) Then

                    If units <> "" Then
                        bb.OutletTemperature = Converter.ConvertToSI(units, value)
                    Else
                        bb.OutletTemperature = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                    End If

                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = False
                        .Tag = sobj.Tag
                        .Name = sobj.Name
                        .ObjectType = ObjectType.RCT_Equilibrium
                        .Sender = "PropertyGrid"
                    End With

                    If bb.IsSpecAttached = True And bb.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(bb.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.RCT_Gibbs Then

                Dim bb As Reactor_Gibbs = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Quedadepresso")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.DeltaP = Converter.ConvertToSI(units, value)
                    Else
                        bb.DeltaP = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature")) Then

                    If units <> "" Then
                        bb.OutletTemperature = Converter.ConvertToSI(units, value)
                    Else
                        bb.OutletTemperature = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                    End If

                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = False
                        .Tag = sobj.Tag
                        .Name = sobj.Name
                        .ObjectType = ObjectType.RCT_Gibbs
                        .Sender = "PropertyGrid"
                    End With

                    If bb.IsSpecAttached = True And bb.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(bb.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.RCT_CSTR Then

                Dim bb As Reactor_CSTR = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("RSCTRIsothermalTemperature")) Then

                    If units <> "" Then
                        bb.IsothermalTemperature = Converter.ConvertToSI(units, value)
                    Else
                        bb.IsothermalTemperature = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature")) Then

                    If units <> "" Then
                        bb.OutletTemperature = Converter.ConvertToSI(units, value)
                    Else
                        bb.OutletTemperature = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Quedadepresso")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.DeltaP = Converter.ConvertToSI(units, value)
                    Else
                        bb.DeltaP = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("CSTRCatalystAmount")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.CatalystAmount = Converter.ConvertToSI(units, value)
                    Else
                        bb.CatalystAmount = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.mass, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("RCSTRPGridItem1")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.Volume = Converter.ConvertToSI(units, value)
                    Else
                        bb.Volume = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.volume, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Headspace")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.Headspace = Converter.ConvertToSI(units, value)
                    Else
                        bb.Headspace = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.volume, e.ChangedItem.Value)
                    End If

                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = False
                        .Tag = sobj.Tag
                        .Name = sobj.Name
                        .ObjectType = ObjectType.RCT_CSTR
                        .Sender = "PropertyGrid"
                    End With

                    If bb.IsSpecAttached = True And bb.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(bb.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.RCT_PFR Then

                Dim bb As Reactor_PFR = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Quedadepresso")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.DeltaP = Converter.ConvertToSI(units, value)
                    Else
                        bb.DeltaP = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature")) Then

                    If units <> "" Then
                        bb.OutletTemperature = Converter.ConvertToSI(units, value)
                    Else
                        bb.OutletTemperature = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("RCSTRPGridItem1")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.Volume = Converter.ConvertToSI(units, value)
                    Else
                        bb.Volume = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.volume, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("PFRLength")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.Length = Converter.ConvertToSI(units, value)
                    Else
                        bb.Length = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.distance, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("PFRCatalystLoading")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.CatalystLoading = Converter.ConvertToSI(units, value)
                    Else
                        bb.CatalystLoading = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.density, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("PFRCatalystParticleDiameter")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.CatalystParticleDiameter = Converter.ConvertToSI(units, value)
                    Else
                        bb.CatalystParticleDiameter = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.diameter, e.ChangedItem.Value)
                    End If

                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = False
                        .Tag = sobj.Tag
                        .Name = sobj.Name
                        .ObjectType = ObjectType.RCT_PFR
                        .Sender = "PropertyGrid"
                    End With

                    If bb.IsSpecAttached = True And bb.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(bb.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.HeatExchanger Then

                Dim bb As HeatExchanger = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("OverallHeatTranferCoefficient")) Then

                    If Convert.ToDouble(e.ChangedItem.Value) < 0.0# Then Throw New InvalidCastException(DWSIM.App.GetLocalString("Ovalorinformadonovli"))
                    If units <> "" Then
                        bb.OverallCoefficient = Converter.ConvertToSI(units, value)
                    Else
                        bb.OverallCoefficient = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.heat_transf_coeff, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Area")) Then

                    If units <> "" Then
                        bb.Area = Converter.ConvertToSI(units, value)
                    Else
                        bb.Area = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.area, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HeatLoad")) Then

                    If units <> "" Then
                        bb.Q = Converter.ConvertToSI(units, value)
                    Else
                        bb.Q = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.heatflow, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HXHotSidePressureDrop")) Then

                    If units <> "" Then
                        bb.HotSidePressureDrop = Converter.ConvertToSI(units, value)
                    Else
                        bb.HotSidePressureDrop = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HXColdSidePressureDrop")) Then

                    If units <> "" Then
                        bb.ColdSidePressureDrop = Converter.ConvertToSI(units, value)
                    Else
                        bb.ColdSidePressureDrop = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.deltaP, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HXTempHotOut")) Then

                    If units <> "" Then
                        bb.HotSideOutletTemperature = Converter.ConvertToSI(units, value)
                    Else
                        bb.HotSideOutletTemperature = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("HXTempColdOut")) Then

                    If units <> "" Then
                        bb.ColdSideOutletTemperature = Converter.ConvertToSI(units, value)
                    Else
                        bb.ColdSideOutletTemperature = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.temperature, e.ChangedItem.Value)
                    End If

                ElseIf e.ChangedItem.Label.Contains("MITA") Then

                    If units <> "" Then
                        bb.MITA = Converter.ConvertToSI(units, value)
                    Else
                        bb.MITA = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.deltaT, e.ChangedItem.Value)
                    End If

                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Tag = sobj.Tag
                        .Calculated = False
                        .Name = sobj.Name
                        .ObjectType = ObjectType.HeatExchanger
                        .Sender = "PropertyGrid"
                    End With

                    If bb.IsSpecAttached = True And bb.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(bb.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.ShortcutColumn Then

                Dim sc As ShortcutColumn = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)
                Dim Pr, Pc As Double

                If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("SCCondenserType")) Then
                    sc.GraphicObject.Shape = sc.condtype
                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("SCCondenserPressure")) Then
                    If units <> "" Then
                        Pc = Converter.ConvertToSI(units, value)
                    Else
                        Pc = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.pressure, e.ChangedItem.Value)
                    End If
                    sc.m_condenserpressure = Pc
                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("SCReboilerPressure")) Then
                    If units <> "" Then
                        Pr = Converter.ConvertToSI(units, value)
                    Else
                        Pr = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.pressure, e.ChangedItem.Value)
                    End If
                    sc.m_boilerpressure = Pr
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("SCLightKey")) Then
                    sc.m_lightkey = e.ChangedItem.Value
                ElseIf e.ChangedItem.Label.Equals(DWSIM.App.GetLocalString("SCHeavyKey")) Then
                    sc.m_heavykey = e.ChangedItem.Value
                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Tag = sobj.Tag
                        .Calculated = False
                        .Name = sobj.Name
                        .ObjectType = ObjectType.ShortcutColumn
                        .Sender = "PropertyGrid"
                    End With

                    If sc.IsSpecAttached = True And sc.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(sc.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.OrificePlate Then

                Dim op As OrificePlate = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("OPOrificeDiameter")) Then
                    If units <> "" Then
                        op.OrificeDiameter = Converter.ConvertToSI(units, value)
                    Else
                        op.OrificeDiameter = Converter.ConvertToSI(Flowsheet.Options.SelectedUnitSystem.diameter, e.ChangedItem.Value)
                    End If
                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("OPBeta")) Then
                    op.Beta = e.ChangedItem.Value
                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("OPCorrectionFactor")) Then
                    op.CorrectionFactor = e.ChangedItem.Value
                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = False
                        .Tag = sobj.Tag
                        .Name = sobj.Name
                        .ObjectType = ObjectType.OrificePlate
                        .Sender = "PropertyGrid"
                    End With

                    If op.IsSpecAttached = True And op.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(op.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If


            ElseIf sobj.ObjectType = ObjectType.ExcelUO Then

                Dim eo As ExcelUO = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)
                Dim P1 As Integer
                Dim L As String
                P1 = InStr(1, e.ChangedItem.Label, "(") - 2
                If P1 > 0 Then
                    L = Strings.Left(e.ChangedItem.Label, P1)
                    If eo.InputParams.ContainsKey(L) Then
                        eo.InputParams(L).Value = e.ChangedItem.Value
                    End If
                End If

                If Settings.CalculatorActivated Then

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = False
                        .Tag = sobj.Tag
                        .Name = sobj.Name
                        .ObjectType = ObjectType.ExcelUO
                        .Sender = "PropertyGrid"
                    End With

                    If eo.IsSpecAttached = True And eo.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(eo.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            ElseIf sobj.ObjectType = ObjectType.DistillationColumn Or sobj.ObjectType = ObjectType.AbsorptionColumn Or sobj.ObjectType = ObjectType.ReboiledAbsorber Or
                sobj.ObjectType = ObjectType.RefluxedAbsorber Or sobj.ObjectType = ObjectType.CapeOpenUO Then

                Dim col As Column = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                Dim su = col.FlowSheet.Options.SelectedUnitSystem

                If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("DCNumbStages")) Then

                    Dim ne As Integer = e.ChangedItem.Value
                    Dim nep As Integer = col.Stages.Count
                    Dim dif As Integer = ne - nep
                    If dif < 0 Then
                        col.Stages.RemoveRange(nep + dif - 1, -dif)
                        With col.InitialEstimates
                            .LiqCompositions.RemoveRange(nep + dif - 1, -dif)
                            .VapCompositions.RemoveRange(nep + dif - 1, -dif)
                            .LiqMolarFlows.RemoveRange(nep + dif - 1, -dif)
                            .VapMolarFlows.RemoveRange(nep + dif - 1, -dif)
                            .StageTemps.RemoveRange(nep + dif - 1, -dif)
                        End With
                    ElseIf dif > 0 Then
                        Dim i As Integer
                        For i = 1 To dif
                            col.Stages.Insert(col.Stages.Count - 1, New Auxiliary.SepOps.Stage(Guid.NewGuid().ToString))
                            col.Stages(col.Stages.Count - 2).Name = DWSIM.App.GetLocalString("DCStage") & "_" & col.Stages.Count - 2
                            With col.InitialEstimates
                                Dim d As New Dictionary(Of String, Parameter)
                                For Each cp As ConstantProperties In col.FlowSheet.Options.SelectedComponents.Values
                                    d.Add(cp.Name, New Parameter)
                                Next
                                .LiqCompositions.Insert(.LiqCompositions.Count - 1, d)
                                .VapCompositions.Insert(.VapCompositions.Count - 1, d)
                                .LiqMolarFlows.Insert(.LiqMolarFlows.Count - 1, New Parameter)
                                .VapMolarFlows.Insert(.VapMolarFlows.Count - 1, New Parameter)
                                .StageTemps.Insert(.StageTemps.Count - 1, New Parameter)
                            End With
                        Next
                    End If

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("DCCondenserDeltaP")) Then

                    col.CondenserDeltaP = Converter.ConvertToSI(su.spmp_deltaP, e.ChangedItem.Value)

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("DCDistillateFlowRate")) Then

                    col.DistillateFlowRate = Converter.ConvertToSI(su.spmp_molarflow, e.ChangedItem.Value)

                ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("DCCondenserType")) Then

                    col.GraphicObject.Shape = col.CondenserType

                    Select Case col.CondenserType
                        Case Column.condtype.Full_Reflux

                            For Each si As StreamInformation In col.MaterialStreams.Values
                                If si.StreamBehavior = StreamInformation.Behavior.Distillate Then
                                    'disconnect and remove from collection
                                    If col.FlowSheet.SimulationObjects.ContainsKey(si.StreamID) Then
                                        Dim idx As Integer = FormFlowsheet.SearchSurfaceObjectsByName(si.StreamID, Flowsheet.FormSurface.FlowsheetSurface).InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                                        Flowsheet.DisconnectObject(col.GraphicObject, FormFlowsheet.SearchSurfaceObjectsByName(si.StreamID, Flowsheet.FormSurface.FlowsheetSurface))
                                        'Me.GraphicObject.OutputConnectors.RemoveAt(idx)
                                    End If
                                    col.MaterialStreams.Remove(si.ID)
                                    Exit For
                                End If
                            Next

                        Case Column.condtype.Partial_Condenser

                        Case Column.condtype.Total_Condenser

                            For Each si As StreamInformation In col.MaterialStreams.Values
                                If si.StreamBehavior = StreamInformation.Behavior.OverheadVapor Then
                                    'disconnect and remove from collection
                                    If col.FlowSheet.Collections.MaterialStreamCollection.ContainsKey(si.StreamID) Then
                                        Dim idx As Integer = FormFlowsheet.SearchSurfaceObjectsByName(si.StreamID, Flowsheet.FormSurface.FlowsheetSurface).InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                                        Flowsheet.DisconnectObject(col.GraphicObject, FormFlowsheet.SearchSurfaceObjectsByName(si.StreamID, Flowsheet.FormSurface.FlowsheetSurface))
                                        'Me.GraphicObject.OutputConnectors.RemoveAt(idx)
                                    End If
                                    col.MaterialStreams.Remove(si.ID)
                                    Exit For
                                End If
                            Next

                    End Select

                ElseIf e.ChangedItem.Label = DWSIM.App.GetLocalString("DCCondenserSpecComp") And e.ChangedItem.Parent.Label = DWSIM.App.GetLocalString("DCCondenserSpecs") Then
                    col.Specs("C").ComponentID = e.ChangedItem.Value
                ElseIf e.ChangedItem.Label = DWSIM.App.GetLocalString("DCReboilerSpecComp") And e.ChangedItem.Parent.Label = DWSIM.App.GetLocalString("DCReboilerSpecs") Then
                    col.Specs("R").ComponentID = e.ChangedItem.Value
                ElseIf e.ChangedItem.Label.Contains("Solving Strategy") Then
                    Dim strategies = New String() {"Ideal K first, then Rigorous", "Ideal H first, then Rigorous", "Ideal K+H first, then Rigorous", "Direct Rigorous"}
                    col.SolverScheme = strategies.ToList.IndexOf(e.ChangedItem.Value)
                End If

                If Settings.CalculatorActivated Then

                    sobj.Calculated = True
                    Flowsheet.FormProps.HandleObjectStatusChanged(sobj)

                    'Call function to calculate flowsheet
                    Dim objargs As New FlowsheetSolver.CalculationArgs
                    With objargs
                        .Calculated = True
                        .Name = sobj.Name
                        .Tag = sobj.Tag
                        .ObjectType = sobj.ObjectType
                        .Sender = "PropertyGrid"
                    End With

                    Dim obj = Flowsheet.Collections.FlowsheetObjectCollection.Item(sobj.Name)

                    If obj.IsSpecAttached = True And obj.SpecVarType = SpecVarType.Source Then Flowsheet.SimulationObjects(obj.AttachedSpecId).Calculate()
                    Flowsheet.CalculationQueue.Enqueue(objargs)

                End If

            End If

        End If

        Flowsheet.FormProps.Enabled = False

        FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Flowsheet, Settings.SolverMode, Settings.TaskCancellationTokenSource, True, False,
                                                       Nothing, Nothing,
                                                       Sub()
                                                           Flowsheet.FormProps.Enabled = True
                                                           Flowsheet.FormSurface.UpdateSelectedObject()
                                                           Flowsheet.FormSurface.Invalidate()
                                                       End Sub)

    End Sub


End Class
