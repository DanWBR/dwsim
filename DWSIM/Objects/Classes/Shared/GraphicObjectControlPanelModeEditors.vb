Imports DWSIM.Interfaces
Imports System.Linq

Public Class GraphicObjectControlPanelModeEditors

    Public Shared Sub SetInputDelegate(gobj As IGraphicObject, myObj As ISimulationObject)

        gobj.ControlPanelModeEditorDisplayDelegate = Sub()
                                                         Dim cpform As FormTextBoxInput = DirectCast(myObj, IControllableObject).ControlPanel
                                                         If cpform Is Nothing Then
                                                             Dim f As New FormTextBoxInput
                                                             Dim SelectedObject = myObj?.GetFlowsheet.SimulationObjects.Values.Where(Function(x2) x2.Name = myObj.SelectedObjectID).FirstOrDefault
                                                             If Not SelectedObject Is Nothing Then
                                                                 If myObj.SelectedProperty = "" Then
                                                                     myObj.GetFlowsheet().ShowMessage("Please select a property to associate with this Input Control.", IFlowsheet.MessageType.GeneralError)
                                                                     Exit Sub
                                                                 End If
                                                                 Dim currentvalue = SystemsOfUnits.Converter.ConvertFromSI(myObj.SelectedPropertyUnits, SelectedObject.GetPropertyValue(myObj.SelectedProperty))
                                                                 f.TextBox1.Text = currentvalue.ToString(myObj?.GetFlowsheet.FlowsheetOptions.NumberFormat)
                                                                 f.Label1.Text = SelectedObject.GraphicObject.Tag
                                                                 f.Label2.Text = DWSIM.App.GetPropertyName(myObj.SelectedProperty)
                                                                 AddHandler f.TextBox1.KeyDown,
                                                                             Sub(s, e)
                                                                                 If e.KeyCode = Keys.Enter Then
                                                                                     SelectedObject = myObj?.GetFlowsheet.SimulationObjects.Values.Where(Function(x2) x2.Name = myObj.SelectedObjectID).FirstOrDefault
                                                                                     If SelectedObject IsNot Nothing Then
                                                                                         Try
                                                                                             SelectedObject.SetPropertyValue(myObj.SelectedProperty, f.TextBox1.Text.ToDoubleFromCurrent().ConvertToSI(myObj.SelectedPropertyUnits))
                                                                                             f.Close()
                                                                                             If Not myObj.GetFlowsheet.DynamicMode Then
                                                                                                 myObj.GetFlowsheet.RequestCalculation3(SelectedObject, False)
                                                                                             End If
                                                                                         Catch ex As Exception
                                                                                             MessageBox.Show(DWSIM.App.GetLocalString("Erro"), ex.Message, MessageBoxButtons.OK, MessageBoxIcon.Error)
                                                                                         End Try
                                                                                     End If
                                                                                 End If
                                                                             End Sub
                                                                 f.StartPosition = FormStartPosition.Manual
                                                                 f.Location = Cursor.Position
                                                                 f.Text = SelectedObject.GraphicObject.Tag
                                                                 f.ChangeDefaultFont()
                                                                 f.TopMost = True
                                                                 f.Show()
                                                             End If
                                                         Else
                                                             If cpform.IsDisposed Then
                                                                 Dim f As New FormTextBoxInput
                                                                 Dim SelectedObject = myObj?.GetFlowsheet.SimulationObjects.Values.Where(Function(x2) x2.Name = myObj.SelectedObjectID).FirstOrDefault
                                                                 If Not SelectedObject Is Nothing Then
                                                                     If myObj.SelectedProperty = "" Then
                                                                         myObj.GetFlowsheet().ShowMessage("Please select a property to associate with this Input Control.", IFlowsheet.MessageType.GeneralError)
                                                                         Exit Sub
                                                                     End If
                                                                     Dim currentvalue = SystemsOfUnits.Converter.ConvertFromSI(myObj.SelectedPropertyUnits, SelectedObject.GetPropertyValue(myObj.SelectedProperty))
                                                                     f.TextBox1.Text = currentvalue.ToString(myObj?.GetFlowsheet.FlowsheetOptions.NumberFormat)
                                                                     f.Label1.Text = SelectedObject.GraphicObject.Tag
                                                                     f.Label2.Text = DWSIM.App.GetPropertyName(myObj.SelectedProperty)
                                                                     AddHandler f.TextBox1.KeyDown,
                                                                             Sub(s, e)
                                                                                 If e.KeyCode = Keys.Enter Then
                                                                                     SelectedObject = myObj?.GetFlowsheet.SimulationObjects.Values.Where(Function(x2) x2.Name = myObj.SelectedObjectID).FirstOrDefault
                                                                                     If SelectedObject IsNot Nothing Then
                                                                                         Try
                                                                                             SelectedObject.SetPropertyValue(myObj.SelectedProperty, f.TextBox1.Text.ToDoubleFromCurrent().ConvertToSI(myObj.SelectedPropertyUnits))
                                                                                             f.Close()
                                                                                             If Not myObj.GetFlowsheet.DynamicMode Then
                                                                                                 myObj.GetFlowsheet.RequestCalculation3(SelectedObject, False)
                                                                                             End If
                                                                                         Catch ex As Exception
                                                                                             MessageBox.Show(DWSIM.App.GetLocalString("Erro"), ex.Message, MessageBoxButtons.OK, MessageBoxIcon.Error)
                                                                                         End Try
                                                                                     End If
                                                                                 End If
                                                                             End Sub
                                                                     f.StartPosition = FormStartPosition.Manual
                                                                     f.Location = Cursor.Position
                                                                     f.Text = SelectedObject.GraphicObject.Tag
                                                                     f.ChangeDefaultFont()
                                                                     f.TopMost = True
                                                                     f.Show()
                                                                 End If
                                                             Else
                                                                 cpform.Show()
                                                                 cpform.BringToFront()
                                                             End If
                                                         End If
                                                     End Sub

    End Sub

    Public Shared Sub SetPIDDelegate(gobj As IGraphicObject, myObj As ISimulationObject)

        gobj.ControlPanelModeEditorDisplayDelegate = Sub()
                                                         Dim cpform As FormPIDCPEditor = DirectCast(myObj, IControllableObject).ControlPanel
                                                         If cpform Is Nothing Then
                                                             Dim f As New FormPIDCPEditor With {.PID = myObj}
                                                             f.StartPosition = FormStartPosition.Manual
                                                             f.Location = Cursor.Position
                                                             f.Text = myObj.GraphicObject.Tag
                                                             DirectCast(myObj, IControllableObject).ControlPanel = f
                                                             f.Show()
                                                         Else
                                                             If cpform.IsDisposed Then
                                                                 Dim f As New FormPIDCPEditor With {.PID = myObj}
                                                                 f.StartPosition = FormStartPosition.Manual
                                                                 f.Location = Cursor.Position
                                                                 f.Text = myObj.GraphicObject.Tag
                                                                 DirectCast(myObj, IControllableObject).ControlPanel = f
                                                                 f.Show()
                                                             Else
                                                                 cpform.Show()
                                                                 cpform.BringToFront()
                                                             End If
                                                         End If
                                                     End Sub

    End Sub

End Class
