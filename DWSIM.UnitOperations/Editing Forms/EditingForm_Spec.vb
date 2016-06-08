Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits

Public Class EditingForm_Spec

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Property SimObject As SpecialOps.Spec

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

            If .SourceObjectData.Name <> "" Then cbSourceObj.SelectedItem = .SourceObjectData.Name
            If .TargetObjectData.Name <> "" Then cbTargetObj.SelectedItem = .TargetObjectData.Name

            'annotation

            Try
                rtbAnnotations.Rtf = .Annotation
            Catch ex As Exception

            End Try

            'parameters

            tbExpression.Text = .Expression
            
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

    Private Sub tb_TextChanged(sender As Object, e As EventArgs) Handles tbExpression.TextChanged

        If Loaded Then

            Dim tbox = DirectCast(sender, TextBox)

            Me.SimObject.Expression = tbox.Text

            Try
                lblResult.Text = "Y = " & SimObject.ParseExpression() & " " & SimObject.FlowSheet.SimulationObjects(SimObject.TargetObjectData.ID).GetPropertyUnit(SimObject.TargetObjectData.PropertyName, units)
                tbox.ForeColor = Drawing.Color.Blue
            Catch ex As Exception
                lblResult.Text = "Error"
                tbox.ForeColor = Drawing.Color.Red
            End Try

        End If

    End Sub

    Private Sub cbInlet1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSourceObj.SelectedIndexChanged

        If Loaded Then

            If SimObject.FlowSheet.SimulationObjects.ContainsKey(SimObject.SourceObjectData.ID) Then
                With SimObject.FlowSheet.SimulationObjects(SimObject.SourceObjectData.ID)
                    .IsSpecAttached = False
                    .AttachedSpecId = ""
                    .SpecVarType = Enums.SpecVarType.None
                End With
            End If

            Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbSourceObj.SelectedItem.ToString).FirstOrDefault

            If Not obj Is Nothing Then

                With Me.SimObject.SourceObjectData
                    .ID = obj.Name
                    .Name = obj.GraphicObject.Tag
                End With

                With obj
                    .IsSpecAttached = True
                    .AttachedSpecId = SimObject.Name
                    .SpecVarType = Enums.SpecVarType.Source
                End With

                Dim props = obj.GetProperties(Enums.PropertyType.ALL)

                cbSourceProp.Items.Clear()
                For Each p In props
                    cbSourceProp.Items.Add(SimObject.FlowSheet.GetTranslatedString(p))
                Next

            End If

        End If

    End Sub

    Private Sub rtbAnnotations_RtfChanged(sender As Object, e As EventArgs) Handles rtbAnnotations.RtfChanged
        If Loaded Then SimObject.Annotation = rtbAnnotations.Rtf
    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub cbTargetObj_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbTargetObj.SelectedIndexChanged

        If Loaded Then

            If SimObject.FlowSheet.SimulationObjects.ContainsKey(SimObject.TargetObjectData.ID) Then
                With SimObject.FlowSheet.SimulationObjects(SimObject.TargetObjectData.ID)
                    .IsSpecAttached = False
                    .AttachedSpecId = ""
                    .SpecVarType = Enums.SpecVarType.None
                End With
            End If

            Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbTargetObj.SelectedItem.ToString).FirstOrDefault

            If Not obj Is Nothing Then

                With Me.SimObject.TargetObjectData
                    .ID = obj.Name
                    .Name = obj.GraphicObject.Tag
                End With

                With obj
                    .IsSpecAttached = True
                    .AttachedSpecId = SimObject.Name
                    .SpecVarType = Enums.SpecVarType.Target
                End With

                Dim props = obj.GetProperties(Enums.PropertyType.WR)

                cbTargetProp.Items.Clear()
                For Each p In props
                    cbTargetProp.Items.Add(SimObject.FlowSheet.GetTranslatedString(p))
                Next

            End If

        End If

    End Sub

    Private Sub cbSourceProp_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSourceProp.SelectedIndexChanged

        If Loaded Then

            Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbSourceObj.SelectedItem.ToString).FirstOrDefault

            If Not obj Is Nothing Then

                Dim props = obj.GetProperties(Enums.PropertyType.WR)

                For Each p In props
                    If SimObject.FlowSheet.GetTranslatedString(p) = cbSourceProp.SelectedItem.ToString Then
                        SimObject.SourceObjectData.PropertyName = p
                        lblSourceVal.Text = obj.GetPropertyValue(p, units) & " " & obj.GetPropertyUnit(p, units)
                        Exit For
                    End If
                Next


            End If


        End If

    End Sub

    Private Sub cbTargetProp_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbTargetProp.SelectedIndexChanged

        If Loaded Then

            Dim obj = Me.SimObject.FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = cbTargetObj.SelectedItem.ToString).FirstOrDefault

            If Not obj Is Nothing Then

                Dim props = obj.GetProperties(Enums.PropertyType.WR)

                For Each p In props
                    If SimObject.FlowSheet.GetTranslatedString(p) = cbTargetProp.SelectedItem.ToString Then
                        SimObject.TargetObjectData.PropertyName = p
                        lblTargetVal.Text = obj.GetPropertyValue(p, units) & " " & obj.GetPropertyUnit(p, units)
                        Exit For
                    End If
                Next

            End If

        End If

    End Sub

End Class