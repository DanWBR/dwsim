Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits

Public Class EditingForm_Adjust

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Property SimObject As SpecialOps.Adjust

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Private Sub EditingForm_HeaterCooler_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        UpdateInfo()

    End Sub

    Sub UpdateInfo()

        units = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        nf = SimObject.FlowSheet.FlowsheetOptions.NumberFormat

        Loaded = False

        With SimObject

            'first block

            chkActive.Checked = .GraphicObject.Active

            Me.Text = .GetDisplayName() & ": " & .GraphicObject.Tag

            lblTag.Text = .GraphicObject.Tag

            'connections

            Dim objlist As String() = .FlowSheet.SimulationObjects.Values.Where(Function(x) TypeOf x Is ISimulationObject).Select(Function(m) m.GraphicObject.Tag).ToArray

            cbSourceObj.Items.Clear()
            cbSourceObj.Items.AddRange(objlist)

            cbTargetObj.Items.Clear()
            cbTargetObj.Items.AddRange(objlist)

            If .ManipulatedObjectData.Name <> "" Then
                cbSourceObj.SelectedItem = .ManipulatedObjectData.Name
                cbSourceProp.SelectedItem = .FlowSheet.GetTranslatedString(.ManipulatedObjectData.PropertyName)
            End If
            If .ControlledObjectData.Name <> "" Then
                cbTargetObj.SelectedItem = .ControlledObjectData.Name
                cbTargetProp.SelectedItem = .FlowSheet.GetTranslatedString(.ControlledObjectData.PropertyName)
            End If

            'annotation

            Try
                rtbAnnotations.Rtf = .Annotation
            Catch ex As Exception

            End Try

            'parameters

            tbSetPoint.Text = .AdjustValue.ToString(nf)

        End With

        Loaded = True

    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged
        If Loaded Then SimObject.GraphicObject.Tag = lblTag.Text
        Me.Text = SimObject.GetDisplayName() & ": " & SimObject.GraphicObject.Tag
    End Sub

    Private Sub btnDisconnect1_Click(sender As Object, e As EventArgs)
        If cbSourceObj.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
            cbSourceObj.SelectedItem = Nothing
        End If
    End Sub

    Private Sub btnDisconnectOutlet1_Click(sender As Object, e As EventArgs)
        If cbSourceProp.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo)
            cbSourceProp.SelectedItem = Nothing
        End If
    End Sub

    Sub RequestCalc()

        SimObject.FlowSheet.RequestCalculation(SimObject)

    End Sub

    Private Sub cbInlet1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSourceObj.SelectedIndexChanged

        If SimObject.FlowSheet.SimulationObjects.ContainsKey(SimObject.ManipulatedObjectData.ID) Then
            With SimObject.FlowSheet.SimulationObjects(SimObject.ManipulatedObjectData.ID)
                .IsAdjustAttached = False
                .AttachedAdjustId = ""
                .AdjustVarType = Enums.AdjustVarType.None
            End With
        End If

        Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbSourceObj.SelectedItem.ToString).FirstOrDefault

        If Not obj Is Nothing Then

            With Me.SimObject.ManipulatedObjectData
                .ID = obj.Name
                .Name = obj.GraphicObject.Tag
            End With

            With obj
                .IsAdjustAttached = True
                .AttachedAdjustId = SimObject.Name
                .AdjustVarType = Enums.AdjustVarType.Manipulated
            End With

            Dim props = obj.GetProperties(Enums.PropertyType.WR)

            cbSourceProp.Items.Clear()
            For Each p In props
                cbSourceProp.Items.Add(SimObject.FlowSheet.GetTranslatedString(p))
            Next

            SimObject.ManipulatedObject = SimObject.FlowSheet.SimulationObjects(SimObject.ManipulatedObjectData.ID)
            DirectCast(SimObject.GraphicObject, DrawingTools.GraphicObjects.AdjustGraphic).ConnectedToMv = SimObject.ManipulatedObject.GraphicObject

        End If

    End Sub

    Private Sub rtbAnnotations_RtfChanged(sender As Object, e As EventArgs) Handles rtbAnnotations.RtfChanged
        If Loaded Then SimObject.Annotation = rtbAnnotations.Rtf
    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub cbTargetObj_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbTargetObj.SelectedIndexChanged

        If SimObject.FlowSheet.SimulationObjects.ContainsKey(SimObject.ControlledObjectData.ID) Then
            With SimObject.FlowSheet.SimulationObjects(SimObject.ControlledObjectData.ID)
                .IsAdjustAttached = False
                .AttachedAdjustId = ""
                .AdjustVarType = Enums.AdjustVarType.None
            End With
        End If

        Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbTargetObj.SelectedItem.ToString).FirstOrDefault

        If Not obj Is Nothing Then

            With Me.SimObject.ControlledObjectData
                .ID = obj.Name
                .Name = obj.GraphicObject.Tag
            End With

            With obj
                .IsAdjustAttached = True
                .AttachedAdjustId = SimObject.Name
                .AdjustVarType = Enums.AdjustVarType.Controlled
            End With

            Dim props = obj.GetProperties(Enums.PropertyType.ALL)

            cbTargetProp.Items.Clear()
            For Each p In props
                cbTargetProp.Items.Add(SimObject.FlowSheet.GetTranslatedString(p))
            Next

            SimObject.ControlledObject = SimObject.FlowSheet.SimulationObjects(SimObject.ControlledObjectData.ID)
            DirectCast(SimObject.GraphicObject, DrawingTools.GraphicObjects.AdjustGraphic).ConnectedToCv = SimObject.ControlledObject.GraphicObject

        End If

    End Sub

    Private Sub cbSourceProp_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSourceProp.SelectedIndexChanged

        Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbSourceObj.SelectedItem.ToString).FirstOrDefault

        If Not obj Is Nothing Then

            Dim props = obj.GetProperties(Enums.PropertyType.WR)

            For Each p In props
                If SimObject.FlowSheet.GetTranslatedString(p) = cbSourceProp.SelectedItem.ToString Then
                    SimObject.ManipulatedObjectData.PropertyName = p
                    lblSourceVal.Text = obj.GetPropertyValue(p, units) & " " & obj.GetPropertyUnit(p, units)
                    Exit For
                End If
            Next

        End If

    End Sub

    Private Sub cbTargetProp_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbTargetProp.SelectedIndexChanged

        Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbTargetObj.SelectedItem.ToString).FirstOrDefault

        If Not obj Is Nothing Then

            Dim props = obj.GetProperties(Enums.PropertyType.WR)

            For Each p In props
                If SimObject.FlowSheet.GetTranslatedString(p) = cbTargetProp.SelectedItem.ToString Then
                    SimObject.ControlledObjectData.PropertyName = p
                    lblTargetVal.Text = obj.GetPropertyValue(p, units) & " (" & (Convert.ToDouble(obj.GetPropertyValue(p, units)) - SimObject.AdjustValue).ToString("+#.####;-#.####;0") & ") " & obj.GetPropertyUnit(p, units)
                    lblSPUnits.Text = obj.GetPropertyUnit(p, units)
                    Exit For
                End If
            Next

        End If

    End Sub

    Private Sub chkSolveGlobal_CheckedChanged(sender As Object, e As EventArgs) Handles chkSolveGlobal.CheckedChanged
        SimObject.SimultaneousAdjust = chkSolveGlobal.Checked
        tbSetPoint.Enabled = Not chkSolveGlobal.Checked
        btnOpenControlPanel.Enabled = Not chkSolveGlobal.Checked
    End Sub

    Private Sub btnOpenControlPanel_Click(sender As Object, e As EventArgs) Handles btnOpenControlPanel.Click

        Dim f As New EditingForm_Adjust_ControlPanel() With {.myADJ = SimObject}
        SimObject.FlowSheet.DisplayForm(f)

    End Sub

    Private Sub tbSetPoint_KeyDown(sender As Object, e As KeyEventArgs) Handles tbSetPoint.KeyDown

        If e.KeyCode = Keys.Enter And Loaded Then

            UpdateInfo()

            DirectCast(sender, TextBox).SelectAll()

        End If

    End Sub

    Private Sub tbSetPoint_TextChanged(sender As Object, e As EventArgs) Handles tbSetPoint.TextChanged

        If Loaded Then
            Try
                SimObject.AdjustValue = Double.Parse(tbSetPoint.Text)
            Catch ex As Exception

            End Try
        End If

    End Sub

End Class