Imports System.Windows.Forms
Imports DWSIM.Thermodynamics.BaseClasses

Public Class WilsonPPEditor

    Public Property WilsonPP As WilsonPropertyPackage

    Private Loaded As Boolean

    Private Sub WilsonPPEditor_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Loaded = False

        Me.TabPage2.Controls.Add(New PropertyPackageSettingsEditingControl(WilsonPP) With {.Dock = DockStyle.Fill})

        Dim id1, id2 As String

        For Each cp As ConstantProperties In WilsonPP.Flowsheet.SelectedCompounds.Values
            id1 = cp.CAS_Number
gt1:        If WilsonPP.WilsonM.BIPs.ContainsKey(id1) Then
                For Each cp2 As ConstantProperties In WilsonPP.Flowsheet.SelectedCompounds.Values
                    id2 = cp2.CAS_Number
                    If id1 <> id2 Then
                        If Not WilsonPP.WilsonM.BIPs(id1).ContainsKey(id2) Then
                            'check if collection has id2 as primary id
                            If WilsonPP.WilsonM.BIPs.ContainsKey(id2) Then
                                If Not WilsonPP.WilsonM.BIPs(id2).ContainsKey(id1) Then
                                    WilsonPP.WilsonM.BIPs(id1).Add(id2, New Double() {0, 0})
                                    Dim A12 As Double = WilsonPP.WilsonM.BIPs(id1)(id2)(0)
                                    Dim A21 As Double = WilsonPP.WilsonM.BIPs(id1)(id2)(1)
                                    dgvu1.Rows.Add(New Object() {cp.Name, cp2.Name, A12, A21})
                                    With dgvu1.Rows(dgvu1.Rows.Count - 1)
                                        .Cells(0).Tag = id1
                                        .Cells(1).Tag = id2
                                    End With
                                End If
                            End If
                        Else
                            Dim A12 As Double = WilsonPP.WilsonM.BIPs(id1)(id2)(0)
                            Dim A21 As Double = WilsonPP.WilsonM.BIPs(id1)(id2)(1)
                            dgvu1.Rows.Add(New Object() {cp.Name, cp2.Name, A12, A21})
                            With dgvu1.Rows(dgvu1.Rows.Count - 1)
                                .Cells(0).Tag = id1
                                .Cells(1).Tag = id2
                            End With
                        End If
                    End If
                Next
            Else
                WilsonPP.WilsonM.BIPs.Add(id1, New Dictionary(Of String, Double()))
                GoTo gt1
            End If
        Next

        Loaded = True

    End Sub

    Private Sub dgvu1_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgvu1.CellValueChanged

        If Loaded Then
            Dim value1 As Object = dgvu1.Rows(e.RowIndex).Cells(2).Value
            Dim value2 As Object = dgvu1.Rows(e.RowIndex).Cells(3).Value
            Dim id1 As String = dgvu1.Rows(e.RowIndex).Cells(0).Tag.ToString
            Dim id2 As String = dgvu1.Rows(e.RowIndex).Cells(1).Tag.ToString
            Select Case e.ColumnIndex
                Case 2, 3
                    WilsonPP.WilsonM.BIPs(id1)(id2) = New Double() {value1, value2}
            End Select
        End If

    End Sub

End Class