Imports WeifenLuo.WinFormsUI.Docking
Imports System.Text
Imports System.Linq
Imports DWSIM.Interfaces
Imports DWSIM.Controls.PropertyGridEx
Imports Controls.PropertyGridEx

Public Class frmProps

    Inherits DockContent

    Public Flowsheet As IFlowsheet

    Private Sub _Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Flowsheet = My.Application.ActiveSimulation

    End Sub

    Public Function ReturnForm(ByVal str As String) As IDockContent

        If str = Me.ToString Then
            Return Me
        Else
            Return Nothing
        End If

    End Function

    Public Sub PGEx1_PropertyValueChanged(ByVal s As Object, ByVal e As System.Windows.Forms.PropertyValueChangedEventArgs) Handles PGEx1.PropertyValueChanged

        If TypeOf Me.ParentForm Is FormFlowsheet Then
            Flowsheet = Me.ParentForm
        Else
            Flowsheet = My.Application.ActiveSimulation
        End If

        Dim sobj As Drawing.SkiaSharp.GraphicObjects.GraphicObject = Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject

        Dim cprop As CustomProperty = (From pd As CustomProperty In PGEx1.Item Select pd Where pd.Name = e.ChangedItem.Label).SingleOrDefault

        If Not cprop Is Nothing Then
            If cprop.Tag2 <> Nothing Then
                If Double.TryParse(e.ChangedItem.Value, New Double) Then
                    Flowsheet.AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.SimulationObjectPropertyChanged,
                                                                        .ObjID = sobj.Name,
                                                                        .OldValue = e.OldValue,
                                                                        .NewValue = e.ChangedItem.Value,
                                                                        .PropertyName = cprop.Tag2,
                                                                        .Tag = Flowsheet.Options.SelectedUnitSystem,
                                                                        .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_FlowsheetObjectPropertyChanged"), sobj.Tag, e.ChangedItem.Label, .OldValue, .NewValue)})
                End If
            End If
        End If

        'handle changes internally

        If Not sobj Is Nothing Then

            If sobj.ObjectType <> ObjectType.GO_Table And sobj.ObjectType <> ObjectType.GO_MasterTable And sobj.ObjectType <> ObjectType.GO_SpreadsheetTable Then

                PropertyChanged.HandlePropertyChange(s, e, Flowsheet.SimulationObjects(sobj.Name))

            ElseIf sobj.ObjectType = ObjectType.GO_MasterTable Then

                Dim mt As Drawing.SkiaSharp.GraphicObjects.Tables.MasterTableGraphic = sobj

                If e.ChangedItem.PropertyDescriptor.Category.Contains(DWSIM.App.GetLocalString("MT_PropertiesToShow")) Then
                    Dim pkey As String = CType(e.ChangedItem.PropertyDescriptor, CustomProperty.CustomPropertyDescriptor).CustomProperty.Tag
                    If Not mt.PropertyList.ContainsKey(pkey) Then
                        mt.PropertyList.Add(pkey, e.ChangedItem.Value)
                    Else
                        mt.PropertyList(pkey) = e.ChangedItem.Value
                    End If
                ElseIf e.ChangedItem.PropertyDescriptor.Category.Contains(DWSIM.App.GetLocalString("MT_ObjectsToShow")) Then
                    If Not mt.ObjectList.ContainsKey(e.ChangedItem.Label) Then
                        mt.ObjectList.Add(e.ChangedItem.Label, e.ChangedItem.Value)
                    Else
                        mt.ObjectList(e.ChangedItem.Label) = e.ChangedItem.Value
                    End If
                End If

                mt.Update()

            End If

        End If

    End Sub

    Public Sub HandleObjectStatusChanged(ByVal obj As Drawing.SkiaSharp.GraphicObjects.GraphicObject)

        If obj.Active = False Then
            LblStatusObj.Text = DWSIM.App.GetLocalString("Inativo")
            LblStatusObj.ForeColor = Color.DimGray
        ElseIf obj.Calculated = False Then
            LblStatusObj.Text = DWSIM.App.GetLocalString("NoCalculado")
            LblStatusObj.ForeColor = Color.Red
        Else
            LblStatusObj.Text = DWSIM.App.GetLocalString("Calculado")
            LblStatusObj.ForeColor = Color.DarkGreen
        End If

    End Sub


    Private Sub PGEx2_PropertyValueChanged(ByVal s As Object, ByVal e As System.Windows.Forms.PropertyValueChangedEventArgs) Handles PGEx2.PropertyValueChanged

        If TypeOf Me.ParentForm Is FormFlowsheet Then
            Flowsheet = Me.ParentForm
        Else
            Flowsheet = My.Application.ActiveSimulation
        End If

        Dim cprop As CustomProperty = (From pd As CustomProperty In PGEx2.Item Select pd Where pd.Name = e.ChangedItem.Label).SingleOrDefault

        Dim sobj As Drawing.SkiaSharp.GraphicObjects.GraphicObject = Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject

        If Not cprop Is Nothing Then
            If cprop.Tag2 <> Nothing Then
                Flowsheet.AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.FlowsheetObjectPropertyChanged,
                                                             .ObjID = Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name,
                                                            .OldValue = e.OldValue,
                                                            .NewValue = e.ChangedItem.Value,
                                                            .PropertyName = cprop.Tag2,
                                                            .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_GraphicObjectPropertyChanged"), sobj.Tag, e.ChangedItem.Label, .OldValue, .NewValue)})
            End If
        End If

        If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("Name")) Then
            Try
                If Not Flowsheet.SimulationObjects(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name).Tabela Is Nothing Then
                    Flowsheet.SimulationObjects(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name).Tabela.HeaderText = Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag
                End If
                Flowsheet.FormObjList.TreeViewObj.Nodes.Find(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name, True)(0).Text = e.ChangedItem.Value
            Catch ex As Exception
                'Flowsheet.WriteToLog(ex.ToString, Color.Red, FormClasses.TipoAviso.Erro)
            Finally
                'CType(FormFlowsheet.SearchSurfaceObjectsByTag(e.OldValue, Flowsheet.FormSurface.FlowsheetDesignSurface), GraphicObject).Tag = e.ChangedItem.Value
                For Each g As IGraphicObject In Flowsheet.FormSurface.FlowsheetDesignSurface.drawingObjects
                    If g.ObjectType = ObjectType.GO_MasterTable Then
                        CType(g, Drawing.SkiaSharp.GraphicObjects.Tables.MasterTableGraphic).Update()
                    End If
                Next
            End Try
            Flowsheet.FormSurface.Invalidate()
        End If
    End Sub

    Private Sub ToolStripButton1_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles ToolStripButton1.Click

        If sfdxml1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Dim str As New StringBuilder
            str.AppendLine(LblNameObj.Text)
            str.AppendLine(LblTipoObj.Text)
            str.AppendLine(LblStatusObj.Text)
            str.AppendLine(DWSIM.App.GetLocalString("Propriedades") & ":")
            For Each item As CustomProperty In PGEx1.Item
                If TypeOf item.Value Is CustomPropertyCollection Then
                    For Each item2 As CustomProperty In item.Value
                        str.AppendLine("[" & item.Category.ToString & "] " & vbTab & item.Name & vbTab & item2.Name & vbTab & item2.Value)
                    Next
                Else
                    str.AppendLine("[" & item.Category.ToString & "] " & vbTab & item.Name & vbTab & item.Value.ToString)
                End If
            Next
            IO.File.WriteAllText(sfdxml1.FileName, str.ToString)
        End If

    End Sub

    Private Sub FloatToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles FloatToolStripMenuItem.Click, DocumentToolStripMenuItem.Click,
                                                                         DockLeftToolStripMenuItem.Click, DockLeftAutoHideToolStripMenuItem.Click,
                                                                         DockRightAutoHideToolStripMenuItem.Click, DockRightToolStripMenuItem.Click,
                                                                         DockTopAutoHideToolStripMenuItem.Click, DockTopToolStripMenuItem.Click,
                                                                         DockBottomAutoHideToolStripMenuItem.Click, DockBottomToolStripMenuItem.Click

        For Each ts As ToolStripMenuItem In dckMenu.Items
            ts.Checked = False
        Next

        sender.Checked = True

        Select Case sender.Name
            Case "FloatToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Float
            Case "DocumentToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Document
            Case "DockLeftToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeft
            Case "DockLeftAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeftAutoHide
            Case "DockRightAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRightAutoHide
            Case "DockRightToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
            Case "DockBottomAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockBottomAutoHide
            Case "DockBottomToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockBottom
            Case "DockTopAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockTopAutoHide
            Case "DockTopToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockTop
            Case "HiddenToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Hidden
        End Select

    End Sub

End Class