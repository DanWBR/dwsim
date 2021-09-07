Imports DWSIM
Imports DWSIM.Interfaces
Imports DWSIM.Thermodynamics.AdvancedEOS
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary
Imports System.Linq

Public Class FormConfig

    Public Loaded = False

    Public PP As PCSAFT2PropertyPackage

    Private Sub FormConfig_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Loaded = False

        chkUsePRH.Checked = PP.UseLeeKeslerEnthalpy
        chkUsePRCp.Checked = PP.UseLeeKeslerCpCv

        Dim compounds As List(Of ICompoundConstantProperties)
        If GlobalSettings.Settings.CAPEOPENMode Then
            compounds = PP._selectedcomps.Values.Select(Function(x) CType(x, ICompoundConstantProperties)).ToList
        Else
            compounds = PP.Flowsheet.SelectedCompounds.Values.ToList
        End If

        For Each cp As ICompoundConstantProperties In compounds
gt0:
            If PP.InteractionParameters.ContainsKey(cp.CAS_Number) Then
                For Each cp2 As ICompoundConstantProperties In compounds
                    If (cp.CAS_Number <> cp2.CAS_Number) Then
                        If Not PP.InteractionParameters(cp.CAS_Number).ContainsKey(cp2.CAS_Number) Then
                            'check if collection has id2 as primary id
                            If PP.InteractionParameters.ContainsKey(cp2.CAS_Number) Then
                                If Not PP.InteractionParameters(cp2.CAS_Number).ContainsKey(cp.CAS_Number) Then
                                    Dim ip = New PCSIP
                                    ip.casno1 = cp.CAS_Number
                                    ip.casno2 = cp2.CAS_Number
                                    ip.compound1 = cp.Name
                                    ip.compound2 = cp2.Name
                                    PP.InteractionParameters(cp.CAS_Number).Add(cp2.CAS_Number, ip)
                                    dgvkij.Rows.Add(New Object() {cp.Name, cp2.Name, ip.kij})
                                    dgvkij.Rows((dgvkij.Rows.Count - 1)).Cells(0).Tag = cp.CAS_Number
                                    dgvkij.Rows((dgvkij.Rows.Count - 1)).Cells(1).Tag = cp2.CAS_Number
                                End If
                            End If
                        Else
                            Dim a12 As Double = PP.InteractionParameters(cp.CAS_Number)(cp2.CAS_Number).kij
                            dgvkij.Rows.Add(New Object() {cp.Name, cp2.Name, a12})
                            dgvkij.Rows((dgvkij.Rows.Count - 1)).Cells(0).Tag = cp.CAS_Number
                            dgvkij.Rows((dgvkij.Rows.Count - 1)).Cells(1).Tag = cp2.CAS_Number
                        End If

                    End If

                Next
            Else
                PP.InteractionParameters.Add(cp.CAS_Number, New Dictionary(Of String, PCSIP))
                GoTo gt0
            End If

        Next
        dgvparams.Rows.Clear()

        For Each cp As ICompoundConstantProperties In compounds
gt1:
            If PP.CompoundParameters.ContainsKey(cp.CAS_Number) Then
                Dim mw As Double = PP.CompoundParameters(cp.CAS_Number).mw
                Dim m As Double = PP.CompoundParameters(cp.CAS_Number).m
                Dim sigma As Double = PP.CompoundParameters(cp.CAS_Number).sigma
                Dim epsilon As Double = PP.CompoundParameters(cp.CAS_Number).epsilon
                Dim assocpar As String = PP.CompoundParameters(cp.CAS_Number).associationparams
                dgvparams.Rows.Add(New Object() {cp.Name, cp.CAS_Number, m, sigma, epsilon})
                dgvparama.Rows.Add(New Object() {cp.Name, assocpar})
                dgvparama.Rows((dgvparama.Rows.Count - 1)).Cells(0).Tag = cp.CAS_Number
            Else
                Dim par = New PCSParam
                par.compound = cp.Name
                par.casno = cp.CAS_Number
                par.mw = cp.Molar_Weight
                PP.CompoundParameters.Add(cp.CAS_Number, par)
                GoTo gt1
            End If

        Next
        Loaded = True

    End Sub

    Private Sub dgvkij_CellEndEdit(sender As Object, e As Windows.Forms.DataGridViewCellEventArgs) Handles dgvkij.CellEndEdit

        If Loaded Then
            Dim id1 As String = dgvkij.Rows(e.RowIndex).Cells(0).Tag.ToString
            Dim id2 As String = dgvkij.Rows(e.RowIndex).Cells(1).Tag.ToString
            Select Case (e.ColumnIndex)
                Case 2
                    Double.TryParse(dgvkij.Rows(e.RowIndex).Cells(e.ColumnIndex).Value.ToString, PP.InteractionParameters(id1)(id2).kij)
            End Select
        End If

    End Sub

    Private Sub dgvparams_CellEndEdit(sender As Object, e As Windows.Forms.DataGridViewCellEventArgs) Handles dgvparams.CellEndEdit

        If Loaded Then
            Dim value As Double
            Double.TryParse(dgvparams.Rows(e.RowIndex).Cells(e.ColumnIndex).Value.ToString, value)
            Dim id As String = dgvparams.Rows(e.RowIndex).Cells(1).Value.ToString
            Select Case (e.ColumnIndex)
                Case 2
                    PP.CompoundParameters(id).m = value
                Case 3
                    PP.CompoundParameters(id).sigma = value
                Case 4
                    PP.CompoundParameters(id).epsilon = value
            End Select
        End If

    End Sub

    Private Sub dgvparama_CellEndEdit(sender As Object, e As Windows.Forms.DataGridViewCellEventArgs) Handles dgvparama.CellEndEdit

        If Loaded Then
            Dim value As String = ""
            If (Not (dgvparama.Rows(e.RowIndex).Cells(e.ColumnIndex).Value) Is Nothing) Then
                value = dgvparama.Rows(e.RowIndex).Cells(e.ColumnIndex).Value.ToString
            End If

            Dim id As String = dgvparama.Rows(e.RowIndex).Cells(0).Tag.ToString
            Select Case (e.ColumnIndex)
                Case 1
                    PP.CompoundParameters(id).associationparams = value
            End Select
        End If

    End Sub

    Private Sub ChkUsePRH_CheckedChanged(sender As Object, e As EventArgs) Handles chkUsePRH.CheckedChanged
        PP.UseLeeKeslerEnthalpy = chkUsePRH.Checked
    End Sub

    Private Sub ChkUsePRCp_CheckedChanged(sender As Object, e As EventArgs) Handles chkUsePRCp.CheckedChanged
        PP.UseLeeKeslerCpCv = chkUsePRCp.Checked
    End Sub

End Class