'    Petroleum Assay Manager
'    Copyright 2012 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Imports System.Math
Imports DWSIM.DWSIM.Utilities.PetroleumCharacterization
Imports DWSIM.DWSIM
Imports System.Runtime.Serialization.Formatters


Public Class FormAssayManager

    Private pfd As FormFlowsheet
    Private su As SistemasDeUnidades.Unidades
    Private cv As SistemasDeUnidades.Conversor
    Public currentassay As Assay.Assay
    Private nf As String = ""
    Private loaded As Boolean = False

    Private Sub FormAssayManager_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        loaded = False

        pfd = My.Application.ActiveSimulation
        su = pfd.Options.SelectedUnitSystem
        nf = pfd.Options.NumberFormat
        cv = New SistemasDeUnidades.Conversor

        GroupBoxBulk.Enabled = False
        GroupBoxCurves.Enabled = False

        gridassays.Rows.Clear()

        If pfd.Options.PetroleumAssays Is Nothing Then pfd.Options.PetroleumAssays = New Dictionary(Of String, Assay.Assay)

        For Each s As String In pfd.Options.PetroleumAssays.Keys
            If pfd.Options.PetroleumAssays(s).IsBulk Then
                gridassays.Rows.Add(New Object() {s, pfd.Options.PetroleumAssays(s).Name, "Bulk"})
            Else
                gridassays.Rows.Add(New Object() {s, pfd.Options.PetroleumAssays(s).Name, "Curves"})
            End If
        Next

        If gridassays.Rows.Count > 0 Then
            gridassays.Rows(0).Selected = True
        End If

        LabelNBP.Text = su.spmp_temperature
        LabelT1.Text = su.spmp_temperature
        LabelT2.Text = su.spmp_temperature
        LabelT3.Text = su.spmp_temperature
        LabelV1.Text = su.spmp_cinematic_viscosity
        LabelV2.Text = su.spmp_cinematic_viscosity

        With Me.gridcurves.Columns
            .Item("temp").HeaderText += " (" & su.spmp_temperature & ")"
            .Item("mm").HeaderText += " (" & su.spmp_molecularWeight & ")"
            .Item("visc1").HeaderText += " (" & su.spmp_cinematic_viscosity & ")"
            .Item("visc2").HeaderText += " (" & su.spmp_cinematic_viscosity & ")"
        End With

        loaded = True

    End Sub

    Private Sub gridassays_SelectionChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles gridassays.SelectionChanged

        If gridassays.SelectedRows.Count > 0 Then

            currentassay = pfd.Options.PetroleumAssays(gridassays.SelectedRows(0).Cells(0).Value)

            loaded = False

            With currentassay

                If .IsBulk Then

                    tb_mw.Text = Format(Conversor.ConverterDoSI(su.spmp_molecularWeight, .MW), nf)
                    tb_sg.Text = Format(.SG60, nf)
                    tb_wk.Text = Format(Conversor.ConverterDoSI(su.spmp_temperature, .NBPAVG), nf)
                    tb_t1.Text = Format(Conversor.ConverterDoSI(su.spmp_temperature, .T1), nf)
                    tb_t2.Text = Format(Conversor.ConverterDoSI(su.spmp_temperature, .T2), nf)
                    tb_v1.Text = Format(Conversor.ConverterDoSI(su.spmp_cinematic_viscosity, .V1), nf)
                    tb_v2.Text = Format(Conversor.ConverterDoSI(su.spmp_cinematic_viscosity, .V2), nf)

                    GroupBoxBulk.Enabled = True
                    GroupBoxCurves.Enabled = False

                Else

                    ComboBoxDistMethod.SelectedIndex = .NBPType
                    ComboBoxBasis.SelectedItem = .CurveBasis
                    TextBoxBulkD.Text = Format(.API, nf)
                    TextBoxBulkMW.Text = Format(Conversor.ConverterDoSI(su.spmp_molecularWeight, .MW), nf)
                    TextBoxVT1.Text = Format(Conversor.ConverterDoSI(su.spmp_temperature, .T1), nf)
                    TextBoxVT2.Text = Format(Conversor.ConverterDoSI(su.spmp_temperature, .T2), nf)
                    TextBoxKAPI.Text = Format(.K_API, nf)
                    CheckBoxMW.Checked = .HasMWCurve
                    CheckBoxSG.Checked = .HasSGCurve
                    CheckBoxVISC.Checked = .HasViscCurves
                    If .SGCurveType = "SG20" Then RadioButtonD20.Checked = True Else RadioButtonD60.Checked = True

                    'curves
                    If Me.CheckBoxMW.Checked = False Then Me.gridcurves.Columns("mm").Visible = False Else Me.gridcurves.Columns("mm").Visible = True
                    If Me.CheckBoxSG.Checked = False Then Me.gridcurves.Columns("dens").Visible = False Else Me.gridcurves.Columns("dens").Visible = True
                    If Me.CheckBoxVISC.Checked = False Then
                        Me.gridcurves.Columns("visc1").Visible = False
                        Me.gridcurves.Columns("visc2").Visible = False
                    Else
                        Me.gridcurves.Columns("visc1").Visible = True
                        Me.gridcurves.Columns("visc2").Visible = True
                    End If

                    gridcurves.Rows.Clear()

                    Dim i As Integer = 0
                    Dim idx As Integer
                    For i = 0 To .PX.Count - 1
                        idx = gridcurves.Rows.Add()
                        gridcurves.Rows(idx).Cells("vap").Value = Format(.PX(i) * 100, nf)
                        gridcurves.Rows(idx).Cells("temp").Value = Format(Conversor.ConverterDoSI(su.spmp_temperature, .PY_NBP(i)), nf)
                        If CheckBoxMW.Checked Then
                            gridcurves.Rows(idx).Cells("mm").Value = Format(.PY_MW(i), nf)
                        End If
                        If CheckBoxSG.Checked Then
                            gridcurves.Rows(idx).Cells("dens").Value = Format(.PY_SG(i), nf)
                        End If
                        If CheckBoxVISC.Checked Then
                            gridcurves.Rows(idx).Cells("visc1").Value = Format(Conversor.ConverterDoSI(su.spmp_cinematic_viscosity, .PY_V1(i)), nf)
                            gridcurves.Rows(idx).Cells("visc2").Value = Format(Conversor.ConverterDoSI(su.spmp_cinematic_viscosity, .PY_V2(i)), nf)
                        End If
                    Next

                    GroupBoxBulk.Enabled = False
                    GroupBoxCurves.Enabled = True

                End If

            End With

            loaded = True

        End If

    End Sub

    Private Sub tb_mw_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tb_mw.TextChanged
        If loaded Then
            currentassay.MW = Conversor.ConverterParaSI(su.spmp_molecularWeight, tb_mw.Text)
        End If
    End Sub

    Private Sub tb_sg_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tb_sg.TextChanged
        If loaded Then
            currentassay.SG60 = tb_sg.Text
        End If
    End Sub

    Private Sub tb_wk_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tb_wk.TextChanged
        If loaded Then
            currentassay.NBPAVG = Conversor.ConverterParaSI(su.spmp_temperature, tb_wk.Text)
        End If
    End Sub

    Private Sub tb_t1_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tb_t1.TextChanged
        If loaded Then
            currentassay.T1 = Conversor.ConverterParaSI(su.spmp_temperature, tb_t1.Text)
        End If
    End Sub

    Private Sub tb_t2_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tb_t2.TextChanged
        If loaded Then
            currentassay.T2 = Conversor.ConverterParaSI(su.spmp_temperature, tb_t2.Text)
        End If
    End Sub

    Private Sub tb_v1_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tb_v1.TextChanged
        If loaded Then
            currentassay.V1 = Conversor.ConverterParaSI(su.spmp_cinematic_viscosity, tb_v1.Text)
        End If
    End Sub

    Private Sub tb_v2_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tb_v2.TextChanged
        If loaded Then
            currentassay.V2 = Conversor.ConverterParaSI(su.spmp_cinematic_viscosity, tb_v2.Text)
        End If
    End Sub

    Private Sub ComboBoxDistMethod_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBoxDistMethod.SelectedIndexChanged
        If loaded Then
            currentassay.NBPType = ComboBoxDistMethod.SelectedIndex
        End If
        If Me.ComboBoxDistMethod.SelectedIndex = 2 Then
            Me.TextBoxKAPI.Enabled = True
        Else
            Me.TextBoxKAPI.Enabled = False
        End If
    End Sub

    Private Sub ComboBoxBasis_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBoxBasis.SelectedIndexChanged
        If loaded Then
            currentassay.CurveBasis = ComboBoxBasis.SelectedItem.ToString
        End If
    End Sub

    Private Sub TextBoxVT1_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxVT1.TextChanged
        If loaded Then
            currentassay.T1 = Conversor.ConverterParaSI(su.spmp_temperature, TextBoxVT1.Text)
        End If
    End Sub

    Private Sub TextBoxVT2_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxVT2.TextChanged
        If loaded Then
            currentassay.T2 = Conversor.ConverterParaSI(su.spmp_temperature, TextBoxVT2.Text)
        End If
    End Sub

    Private Sub TextBoxBulkD_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxBulkD.TextChanged
        If loaded Then
            currentassay.API = TextBoxBulkD.Text
        End If
    End Sub

    Private Sub TextBoxBulkMW_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxBulkMW.TextChanged
        If loaded Then
            currentassay.MW = Conversor.ConverterParaSI(su.spmp_molecularWeight, TextBoxBulkMW.Text)
        End If
    End Sub

    Private Sub TextBoxKAPI_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxKAPI.TextChanged
        If loaded Then
            currentassay.K_API = TextBoxKAPI.Text
        End If
    End Sub

    Private Sub ToolStripButton3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton3.Click

        Dim newassay As Assay.Assay = pfd.Options.PetroleumAssays(gridassays.SelectedRows(0).Cells(0).Value).Clone

        newassay.Name += "_CLONE"

        Dim id As String = Guid.NewGuid().ToString

        pfd.Options.PetroleumAssays.Add(id, newassay)

        If newassay.IsBulk Then
            gridassays.Rows.Add(New Object() {id, newassay.Name, "Bulk"})
        Else
            gridassays.Rows.Add(New Object() {id, newassay.Name, "Curves"})
        End If

    End Sub

    Private Sub ToolStripButton4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton4.Click

        pfd.Options.PetroleumAssays.Remove(gridassays.SelectedRows(0).Cells(0).Value)

        gridassays.Rows.Clear()

        For Each s As String In pfd.Options.PetroleumAssays.Keys
            If pfd.Options.PetroleumAssays(s).IsBulk Then
                gridassays.Rows.Add(New Object() {s, pfd.Options.PetroleumAssays(s).Name, "Bulk"})
            Else
                gridassays.Rows.Add(New Object() {s, pfd.Options.PetroleumAssays(s).Name, "Curves"})
            End If
        Next


    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click

        currentassay = pfd.Options.PetroleumAssays(gridassays.SelectedRows(0).Cells(0).Value)

        Me.DialogResult = Windows.Forms.DialogResult.OK

        Me.Hide()

    End Sub

    Private Sub gridcurves_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles gridcurves.CellValueChanged
        If loaded And e.RowIndex >= 0 Then
            Dim value As Double = gridcurves.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            Select Case gridcurves.Columns(e.ColumnIndex).Name
                Case "vap"
                    currentassay.PX(e.RowIndex) = value / 100
                Case "temp"
                    currentassay.PY_NBP(e.RowIndex) = Conversor.ConverterParaSI(su.spmp_temperature, value)
                Case "mm"
                    currentassay.PY_MW(e.RowIndex) = value
                Case "dens"
                    currentassay.PY_SG(e.RowIndex) = value
                Case "visc1"
                    currentassay.PY_V1(e.RowIndex) = Conversor.ConverterParaSI(su.spmp_cinematic_viscosity, value)
                Case "visc2"
                    currentassay.PY_V2(e.RowIndex) = Conversor.ConverterParaSI(su.spmp_cinematic_viscosity, value)
            End Select
        End If
    End Sub

    Private Sub ToolStripButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton1.Click
        Dim myStream As System.IO.FileStream
        If Me.OpenFileDialog1.ShowDialog() = Windows.Forms.DialogResult.OK Then
            myStream = Me.OpenFileDialog1.OpenFile()
            If Not (myStream Is Nothing) Then
                Dim myassay As New Assay.Assay
                Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
                Try
                    myassay = DirectCast(mySerializer.Deserialize(myStream), Assay.Assay)
                    Dim id As String = Guid.NewGuid().ToString
                    pfd.Options.PetroleumAssays.Add(id, myassay)
                    If myassay.IsBulk Then
                        gridassays.Rows.Add(New Object() {id, myassay.Name, "Bulk"})
                    Else
                        gridassays.Rows.Add(New Object() {id, myassay.Name, "Curves"})
                    End If
                Catch ex As System.Runtime.Serialization.SerializationException
                    MessageBox.Show(ex.Message, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                Finally
                    myStream.Close()
                End Try
            End If
        End If
    End Sub

    Private Sub ToolStripButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton2.Click
        Dim myStream As System.IO.FileStream
        If Me.SaveFileDialog1.ShowDialog() = Windows.Forms.DialogResult.OK Then
            myStream = Me.SaveFileDialog1.OpenFile()
            If Not (myStream Is Nothing) Then
                Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
                Try
                    mySerializer.Serialize(myStream, currentassay)
                Catch ex As System.Runtime.Serialization.SerializationException
                    MessageBox.Show(ex.Message, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                Finally
                    myStream.Close()
                End Try
            End If
        End If
    End Sub

End Class