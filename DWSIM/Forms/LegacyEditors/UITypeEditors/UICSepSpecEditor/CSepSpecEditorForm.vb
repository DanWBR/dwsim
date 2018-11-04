Imports DWSIM.DWSIM.SimulationObjects.UnitOps
Imports DWSIM.UnitOperations.UnitOperations

Public Class CSepSpecEditorForm

    Public compspecs As Dictionary(Of String, Auxiliary.ComponentSeparationSpec)
    Public loaded As Boolean = False

    Private Sub CSepSpecEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        For Each comp In My.Application.ActiveSimulation.Options.SelectedComponents.Values
            If Not compspecs.ContainsKey(comp.Name) Then
                Me.compspecs.Add(comp.Name, New Auxiliary.ComponentSeparationSpec(comp.Name, Auxiliary.SeparationSpec.PercentInletMassFlow, 0, "%"))
            End If
        Next

        Dim cbspec, cbunits As New DataGridViewComboBoxCell

        cbspec.Items.AddRange(New Object() {"Mass Flow", "Molar Flow", "% Inlet Mass Flow", "% Inlet Molar Flow"})
        cbunits.Items.AddRange(New Object() {"-", "%", "g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s", "mol/s", "lbmol/h", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"})

        Me.DataGridView1.Columns(2).CellTemplate = cbspec
        Me.DataGridView1.Columns(4).CellTemplate = cbunits

        Dim spec As String = ""

        For Each cs As Auxiliary.ComponentSeparationSpec In Me.compspecs.Values
            Select Case cs.SepSpec.ToString
                Case "MassFlow"
                    spec = "Mass Flow"
                Case "MolarFlow"
                    spec = "Molar Flow"
                Case "PercentInletMassFlow"
                    spec = "% Inlet Mass Flow"
                Case "PercentInletMolarFlow"
                    spec = "% Inlet Molar Flow"
            End Select
            If My.Application.ActiveSimulation.Options.SelectedComponents.ContainsKey(cs.ComponentID) Then Me.DataGridView1.Rows.Add(New Object() {cs.ComponentID, DWSIM.App.GetLocalString(cs.ComponentID), spec, cs.SpecValue, cs.SpecUnit})
        Next

        Me.loaded = True

    End Sub

    Private Sub DataGridView1_CellValueChanged(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles DataGridView1.CellValueChanged
        If Me.loaded Then
            Dim compid As String = Me.DataGridView1.Rows(e.RowIndex).Cells(0).Value
            Dim newval As Object = Me.DataGridView1.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            Select Case e.ColumnIndex
                Case 2
                    Select Case newval.ToString
                        Case "Mass Flow"
                            Me.compspecs(compid).SepSpec = Auxiliary.SeparationSpec.MassFlow
                        Case "Molar Flow"
                            Me.compspecs(compid).SepSpec = Auxiliary.SeparationSpec.MolarFlow
                        Case "% Inlet Mass Flow"
                            Me.compspecs(compid).SepSpec = Auxiliary.SeparationSpec.PercentInletMassFlow
                        Case "% Inlet Molar Flow"
                            Me.compspecs(compid).SepSpec = Auxiliary.SeparationSpec.PercentInletMolarFlow
                    End Select
                Case 3
                    Me.compspecs(compid).SpecValue = newval
                Case 4
                    Me.compspecs(compid).SpecUnit = newval
            End Select
        End If
    End Sub
End Class