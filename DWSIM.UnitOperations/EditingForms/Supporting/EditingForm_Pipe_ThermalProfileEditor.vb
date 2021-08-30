Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.Pipe
Imports System.Drawing

Public Class PipeThermalProfileEditor

    Inherits UserControl

    Dim su As DWSIM.SharedClasses.SystemsOfUnits.Units

    Public form As IFlowsheet

    Public Profile As ThermalEditorDefinitions

    Dim loaded As Boolean = False

    Private Sub RadioButton9_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton9.CheckedChanged
        If RadioButton9.Checked = True Then

            Profile.TipoPerfil = UnitOperations.Auxiliary.Pipe.ThermalEditorDefinitions.ThermalProfileType.Definir_CGTC
            TextBoxCGTC.Enabled = True
            TextBoxTA.Enabled = True
            TextBoxTAG.Enabled = True

        Else

            TextBoxCGTC.Enabled = False
            TextBoxTA.Enabled = False
            TextBoxTAG.Enabled = False

        End If
    End Sub

    Private Sub RadioButton8_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton8.CheckedChanged
        If RadioButton8.Checked = True Then

            Profile.TipoPerfil = UnitOperations.Auxiliary.Pipe.ThermalEditorDefinitions.ThermalProfileType.Definir_Q
            TextBoxCT.Enabled = True

        Else

            TextBoxCT.Enabled = False

        End If
    End Sub

    Private Sub RadioButton7_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton7.CheckedChanged

        If RadioButton7.Checked = True Then
            Profile.TipoPerfil = UnitOperations.Auxiliary.Pipe.ThermalEditorDefinitions.ThermalProfileType.Estimar_CGTC
            TextBoxTA2.Enabled = True
            TextBoxTAG2.Enabled = True
            CheckBoxIPT.Enabled = True
            CheckBoxICTI.Enabled = True
            CheckBoxII.Enabled = True
            ComboBoxMAT.Enabled = True
            TextBoxCTERM.Enabled = True
            TextBoxESP.Enabled = True
            CheckBoxICTE.Enabled = True
            ComboBoxMAMB.Enabled = True
            TextBoxVEL.Enabled = True

            ComboBoxMAT_SelectedIndexChanged(sender, e)

        Else

            TextBoxTA2.Enabled = False
            TextBoxTAG2.Enabled = False
            CheckBoxIPT.Enabled = False
            CheckBoxICTI.Enabled = False
            CheckBoxII.Enabled = False
            ComboBoxMAT.Enabled = False
            TextBoxCTERM.Enabled = False
            TextBoxESP.Enabled = False
            CheckBoxICTE.Enabled = False
            ComboBoxMAMB.Enabled = False
            TextBoxVEL.Enabled = False

        End If
    End Sub

    Private Sub ComboBoxMAT_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBoxMAT.SelectedIndexChanged

        If loaded Then

            Dim fi As New Globalization.CultureInfo("en-US")

            Try
                Profile.Material = Me.ComboBoxMAT.SelectedIndex
            Catch ex As Exception

            End Try

            If ComboBoxMAT.SelectedIndex = 0 Then

                TextBoxCTERM.Text = Double.Parse("0.7", fi).ToString()
                TextBoxCTERM.ReadOnly = True

            ElseIf ComboBoxMAT.SelectedIndex = 1 Then

                TextBoxCTERM.Text = Double.Parse("1", fi).ToString()
                TextBoxCTERM.ReadOnly = True

            ElseIf ComboBoxMAT.SelectedIndex = 2 Then

                TextBoxCTERM.Text = Double.Parse("0.018", fi).ToString()
                TextBoxCTERM.ReadOnly = True

            ElseIf ComboBoxMAT.SelectedIndex = 3 Then

                TextBoxCTERM.Text = Double.Parse("0.04", fi).ToString()
                TextBoxCTERM.ReadOnly = True

            ElseIf ComboBoxMAT.SelectedIndex = 4 Then

                TextBoxCTERM.Text = Double.Parse("0.035", fi).ToString()
                TextBoxCTERM.ReadOnly = True

            ElseIf ComboBoxMAT.SelectedIndex = 5 Then

                TextBoxCTERM.Text = Double.Parse("0.036", fi).ToString()
                TextBoxCTERM.ReadOnly = True

            ElseIf ComboBoxMAT.SelectedIndex = 6 Then

                TextBoxCTERM.Text = Double.Parse("0.08", fi).ToString()
                TextBoxCTERM.ReadOnly = True

            ElseIf ComboBoxMAT.SelectedIndex = 7 Then

                TextBoxCTERM.Text = Double.Parse("0", fi).ToString()
                TextBoxCTERM.ReadOnly = False

            End If

        End If

    End Sub

    Private Sub ThermalProfileEditorForm_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

        su = form.FlowsheetOptions.SelectedUnitSystem

        Me.Label52.Text = su.heat_transf_coeff
        Me.Label39.Text = su.temperature
        Me.Label25.Text = su.temperature
        Me.Label37.Text = su.heatflow
        Me.Label33.Text = su.thermalConductivity
        Me.Label31.Text = su.thickness
        Me.lblTAGUnits.Text = su.deltaT & "/" & su.distance
        Me.lblTAGUnits2.Text = su.deltaT & "/" & su.distance

        If Profile Is Nothing Then
            Profile = New ThermalEditorDefinitions
            Me.ComboBoxMAMB.SelectedIndex = 0
            Me.ComboBoxMAT.SelectedIndex = 0
        Else
            With Profile
                If Profile.TipoPerfil = UnitOperations.Auxiliary.Pipe.ThermalEditorDefinitions.ThermalProfileType.Definir_CGTC Then Me.RadioButton9.Checked = True
                If Profile.TipoPerfil = UnitOperations.Auxiliary.Pipe.ThermalEditorDefinitions.ThermalProfileType.Definir_Q Then Me.RadioButton8.Checked = True
                If Profile.TipoPerfil = UnitOperations.Auxiliary.Pipe.ThermalEditorDefinitions.ThermalProfileType.Estimar_CGTC Then Me.RadioButton7.Checked = True
                Me.ComboBoxMAMB.SelectedIndex = .Meio
                Me.ComboBoxMAT.SelectedIndex = .Material
                Me.TextBoxCGTC.Text = cv.ConvertFromSI(su.heat_transf_coeff, .CGTC_Definido).ToString()
                Me.TextBoxCT.Text = cv.ConvertFromSI(su.heatflow, .Calor_trocado).ToString()
                Me.TextBoxCTERM.Text = cv.ConvertFromSI(su.thermalConductivity, .Condtermica).ToString()
                Me.TextBoxESP.Text = cv.ConvertFromSI(su.thickness, .Espessura).ToString()
                Me.TextBoxTA.Text = cv.ConvertFromSI(su.temperature, .Temp_amb_definir).ToString()
                Me.TextBoxTA2.Text = cv.ConvertFromSI(su.temperature, .Temp_amb_estimar).ToString()
                Me.TextBoxVEL.Text = .Velocidade.ToString()
                Me.CheckBoxICTE.Checked = .Incluir_cte
                Me.CheckBoxICTI.Checked = .Incluir_cti
                Me.CheckBoxII.Checked = .Incluir_isolamento
                Me.CheckBoxIPT.Checked = .Incluir_paredes
                Me.TextBoxTAG.Text = (cv.ConvertFromSI(su.deltaT, .AmbientTemperatureGradient) / cv.ConvertFromSI(su.distance, 1)).ToString(form.FlowsheetOptions.NumberFormat)
                Me.TextBoxTAG2.Text = (cv.ConvertFromSI(su.deltaT, .AmbientTemperatureGradient_EstimateHTC) / cv.ConvertFromSI(su.distance, 1)).ToString(form.FlowsheetOptions.NumberFormat)
            End With
        End If
        loaded = True
    End Sub

    Private Sub TextBoxCGTC_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxCGTC.TextChanged
        If loaded Then
            Try
                Profile.CGTC_Definido = cv.ConvertToSI(su.heat_transf_coeff, Double.Parse(Me.TextBoxCGTC.Text))
                Me.TextBoxCGTC.ForeColor = Color.Blue
            Catch ex As Exception
                Me.TextBoxCGTC.ForeColor = Color.Red
            End Try
        End If
    End Sub

    Private Sub TextBoxTA_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxTA.TextChanged
        If loaded Then
            Try
                Profile.Temp_amb_definir = cv.ConvertToSI(su.temperature, Double.Parse(Me.TextBoxTA.Text))
                Me.TextBoxTA.ForeColor = Color.Blue
            Catch ex As Exception
                Me.TextBoxTA.ForeColor = Color.Red
            End Try
        End If
    End Sub

    Private Sub TextBoxCT_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxCT.TextChanged
        If loaded Then
            Try
                Profile.Calor_trocado = cv.ConvertToSI(su.heatflow, Double.Parse(Me.TextBoxCT.Text))
                Me.TextBoxCT.ForeColor = Color.Blue
            Catch ex As Exception
                Me.TextBoxCT.ForeColor = Color.Red
            End Try
        End If
    End Sub

    Private Sub TextBoxTA2_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxTA2.TextChanged
        If loaded Then
            Try
                Profile.Temp_amb_estimar = cv.ConvertToSI(su.temperature, Double.Parse(Me.TextBoxTA2.Text))
                Me.TextBoxTA2.ForeColor = Color.Blue
            Catch ex As Exception
                Me.TextBoxTA2.ForeColor = Color.Red
            End Try
        End If
    End Sub

    Private Sub TextBoxCTERM_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxCTERM.TextChanged
        If loaded Then
            Try
                Profile.Condtermica = cv.ConvertToSI(su.thermalConductivity, Double.Parse(Me.TextBoxCTERM.Text))
                Me.TextBoxCTERM.ForeColor = Color.Blue
            Catch ex As Exception
                Me.TextBoxCTERM.ForeColor = Color.Red
            End Try
        End If
    End Sub

    Private Sub TextBoxESP_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxESP.TextChanged
        If loaded Then
            Try
                Profile.Espessura = cv.ConvertToSI(su.thickness, Double.Parse(Me.TextBoxESP.Text))
                Me.TextBoxESP.ForeColor = Color.Blue
            Catch ex As Exception
                Me.TextBoxESP.ForeColor = Color.Red
            End Try
        End If
    End Sub

    Private Sub TextBoxVEL_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxVEL.TextChanged
        If loaded Then
            Try
                Profile.Velocidade = Double.Parse(Me.TextBoxVEL.Text)
                Me.TextBoxVEL.ForeColor = Color.Blue
            Catch ex As Exception
                Me.TextBoxVEL.ForeColor = Color.Red
            End Try
        End If
    End Sub

    Private Sub CheckBoxIPT_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxIPT.CheckedChanged
        Profile.Incluir_paredes = Me.CheckBoxIPT.Checked
    End Sub

    Private Sub CheckBoxICTI_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxICTI.CheckedChanged
        Profile.Incluir_cti = Me.CheckBoxICTI.Checked
    End Sub

    Private Sub CheckBoxII_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxII.CheckedChanged
        Profile.Incluir_isolamento = Me.CheckBoxII.Checked
        If Me.CheckBoxII.Checked = False Then
            Me.ComboBoxMAT.Enabled = False
            Me.TextBoxCTERM.Enabled = False
            Me.TextBoxESP.Enabled = False
        Else
            Me.ComboBoxMAT.Enabled = True
            Me.TextBoxCTERM.Enabled = True
            Me.TextBoxESP.Enabled = True
            ComboBoxMAT_SelectedIndexChanged(sender, e)
        End If
    End Sub

    Private Sub CheckBoxICTE_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxICTE.CheckedChanged
        Profile.Incluir_cte = Me.CheckBoxICTE.Checked
        If Me.CheckBoxICTE.Checked = False Then
            Me.ComboBoxMAMB.Enabled = False
            Me.TextBoxVEL.Enabled = False
        Else
            Me.ComboBoxMAMB.Enabled = True
            Me.TextBoxVEL.Enabled = True
        End If
    End Sub

    Public Sub New()

        ' This call is required by the Windows Form Designer.
        InitializeComponent()

    End Sub

    Private Sub ComboBoxMAMB_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBoxMAMB.SelectedIndexChanged
        If loaded Then

            Try
                Profile.Meio = Me.ComboBoxMAMB.SelectedIndex
            Catch ex As Exception

            End Try
        End If


    End Sub

    Private Sub TextBoxTAG_TextChanged(sender As Object, e As EventArgs) Handles TextBoxTAG.TextChanged
        If loaded Then
            Try
                Profile.AmbientTemperatureGradient = cv.ConvertToSI(su.deltaT, Double.Parse(Me.TextBoxTAG.Text)) / cv.ConvertToSI(su.distance, 1)
                Me.TextBoxTAG.ForeColor = Color.Blue
            Catch ex As Exception
                Me.TextBoxTAG.ForeColor = Color.Red
            End Try
        End If
    End Sub

    Private Sub TextBoxTAG2_TextChanged(sender As Object, e As EventArgs) Handles TextBoxTAG2.TextChanged
        If loaded Then
            Try
                Profile.AmbientTemperatureGradient_EstimateHTC = cv.ConvertToSI(su.deltaT, Double.Parse(Me.TextBoxTAG2.Text)) / cv.ConvertToSI(su.distance, 1)
                Me.TextBoxTAG2.ForeColor = Color.Blue
            Catch ex As Exception
                Me.TextBoxTAG2.ForeColor = Color.Red
            End Try
        End If
    End Sub

End Class

