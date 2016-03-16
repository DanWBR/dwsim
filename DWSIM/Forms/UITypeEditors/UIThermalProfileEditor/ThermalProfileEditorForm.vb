Public Class ThermalProfileEditorForm

    Inherits System.Windows.Forms.Form

    Dim su As DWSIM.SistemasDeUnidades.Unidades
    Dim cv As DWSIM.SistemasDeUnidades.Conversor
    Dim form As FormFlowsheet

    Private Sub RadioButton9_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton9.CheckedChanged
        If RadioButton9.Checked = True Then

            Me.ThermalProfile.Tipo = DWSIM.Editors.PipeEditor.ThermalProfileType.Definir_CGTC
            TextBoxCGTC.Enabled = True
            TextBoxTA.Enabled = True
            'TextBoxCT.Enabled = False
            'TextBoxTA2.Enabled = False
            'CheckBoxIPT.Enabled = False
            'CheckBoxICTI.Enabled = False
            'CheckBoxII.Enabled = False
            'ComboBoxMAT.Enabled = False
            'TextBoxCTERM.Enabled = False
            'TextBoxESP.Enabled = False
            'CheckBoxICTE.Enabled = False
            'ComboBoxMAMB.Enabled = False
            'TextBoxVEL.Enabled = False

        Else

            TextBoxCGTC.Enabled = False
            TextBoxTA.Enabled = False
            'TextBoxCT.Enabled = True
            'TextBoxTA2.Enabled = True
            'CheckBoxIPT.Enabled = True
            'CheckBoxICTI.Enabled = True
            'CheckBoxII.Enabled = True
            'ComboBoxMAT.Enabled = True
            'TextBoxCTERM.Enabled = True
            'TextBoxESP.Enabled = True
            'CheckBoxICTE.Enabled = True
            'ComboBoxMAMB.Enabled = True
            'TextBoxVEL.Enabled = True

        End If
    End Sub

    Private Sub RadioButton8_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton8.CheckedChanged
        If RadioButton8.Checked = True Then
            Me.ThermalProfile.Tipo = DWSIM.Editors.PipeEditor.ThermalProfileType.Definir_Q
            'TextBoxCGTC.Enabled = False
            'TextBoxTA.Enabled = False
            TextBoxCT.Enabled = True
            'TextBoxTA2.Enabled = False
            'CheckBoxIPT.Enabled = False
            'CheckBoxICTI.Enabled = False
            'CheckBoxII.Enabled = False
            'ComboBoxMAT.Enabled = False
            'TextBoxCTERM.Enabled = False
            'TextBoxESP.Enabled = False
            'CheckBoxICTE.Enabled = False
            'ComboBoxMAMB.Enabled = False
            'TextBoxVEL.Enabled = False

        Else

            'TextBoxCGTC.Enabled = True
            'TextBoxTA.Enabled = True
            TextBoxCT.Enabled = False
            'TextBoxTA2.Enabled = True
            'CheckBoxIPT.Enabled = True
            'CheckBoxICTI.Enabled = True
            'CheckBoxII.Enabled = True
            'ComboBoxMAT.Enabled = True
            'TextBoxCTERM.Enabled = True
            'TextBoxESP.Enabled = True
            'CheckBoxICTE.Enabled = True
            'ComboBoxMAMB.Enabled = True
            'TextBoxVEL.Enabled = True

        End If
    End Sub

    Private Sub RadioButton7_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton7.CheckedChanged

        If RadioButton7.Checked = True Then
            Me.ThermalProfile.Tipo = DWSIM.Editors.PipeEditor.ThermalProfileType.Estimar_CGTC
            'TextBoxCGTC.Enabled = False
            'TextBoxTA.Enabled = False
            'TextBoxCT.Enabled = False
            TextBoxTA2.Enabled = True
            CheckBoxIPT.Enabled = True
            CheckBoxICTI.Enabled = True
            CheckBoxII.Enabled = True
            ComboBoxMAT.Enabled = True
            TextBoxCTERM.Enabled = True
            TextBoxESP.Enabled = True
            CheckBoxICTE.Enabled = True
            ComboBoxMAMB.Enabled = True
            TextBoxVEL.Enabled = True

        Else

            'TextBoxCGTC.Enabled = True
            'TextBoxTA.Enabled = True
            'TextBoxCT.Enabled = True
            TextBoxTA2.Enabled = False
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

        Dim fi As New Globalization.CultureInfo("en-US")

        Try
            Me.ThermalProfile.Material = Me.ComboBoxMAT.SelectedIndex
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

    End Sub

    Private Sub ThermalProfileEditorForm_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

        form = My.Application.ActiveSimulation
        su = form.Options.SelectedUnitSystem
        cv = New DWSIM.SistemasDeUnidades.Conversor

        Me.Label52.Text = su.heat_transf_coeff
        Me.Label39.Text = su.spmp_temperature
        Me.Label25.Text = su.spmp_temperature
        Me.Label37.Text = su.spmp_heatflow
        Me.Label33.Text = su.spmp_thermalConductivity
        Me.Label31.Text = su.thickness

        If Me.ThermalProfile Is Nothing Then
            Me.ThermalProfile = New DWSIM.Editors.PipeEditor.ThermalEditorDefinitions
            Me.ComboBoxMAMB.SelectedIndex = 0
            Me.ComboBoxMAT.SelectedIndex = 0
        Else
            With Me.ThermalProfile
                If Me.ThermalProfile.Tipo = DWSIM.Editors.PipeEditor.ThermalProfileType.Definir_CGTC Then Me.RadioButton9.Checked = True
                If Me.ThermalProfile.Tipo = DWSIM.Editors.PipeEditor.ThermalProfileType.Definir_Q Then Me.RadioButton8.Checked = True
                If Me.ThermalProfile.Tipo = DWSIM.Editors.PipeEditor.ThermalProfileType.Estimar_CGTC Then Me.RadioButton7.Checked = True
                Me.ComboBoxMAMB.SelectedIndex = .Meio
                Me.ComboBoxMAT.SelectedIndex = .Material
                Me.TextBoxCGTC.Text = Conversor.ConverterDoSI(su.heat_transf_coeff, .CGTC_Definido).ToString()
                Me.TextBoxCT.Text = Conversor.ConverterDoSI(su.spmp_heatflow, .Calor_trocado).ToString()
                Me.TextBoxCTERM.Text = Conversor.ConverterDoSI(su.spmp_thermalConductivity, .Condtermica).ToString()
                Me.TextBoxESP.Text = Conversor.ConverterDoSI(su.thickness, .Espessura).ToString()
                Me.TextBoxTA.Text = Conversor.ConverterDoSI(su.spmp_temperature, .Temp_amb_definir).ToString()
                Me.TextBoxTA2.Text = Conversor.ConverterDoSI(su.spmp_temperature, .Temp_amb_estimar).ToString()
                Me.TextBoxVEL.Text = .Velocidade.ToString()
                Me.CheckBoxICTE.Checked = .Incluir_cte
                Me.CheckBoxICTI.Checked = .Incluir_cti
                Me.CheckBoxII.Checked = .Incluir_isolamento
                Me.CheckBoxIPT.Checked = .Incluir_paredes
            End With
        End If
    End Sub

    Protected m_thermalprofile As DWSIM.Editors.PipeEditor.ThermalEditorDefinitions

    Property ThermalProfile() As DWSIM.Editors.PipeEditor.ThermalEditorDefinitions
        Get
            Return m_thermalprofile
        End Get
        Set(ByVal value As DWSIM.Editors.PipeEditor.ThermalEditorDefinitions)
            m_thermalprofile = value
        End Set
    End Property

    Private Sub TextBoxCGTC_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxCGTC.TextChanged
        Try
            Me.ThermalProfile.CGTC_Definido = Conversor.ConverterParaSI(su.heat_transf_coeff, Double.Parse(Me.TextBoxCGTC.Text))
            Me.TextBoxCGTC.ForeColor = Color.Blue
        Catch ex As Exception
            Me.TextBoxCGTC.ForeColor = Color.Red
        End Try
    End Sub

    Private Sub TextBoxTA_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxTA.TextChanged
        Try
            Me.ThermalProfile.Temp_amb_definir = Conversor.ConverterParaSI(su.spmp_temperature, Double.Parse(Me.TextBoxTA.Text))
            Me.TextBoxTA.ForeColor = Color.Blue
        Catch ex As Exception
            Me.TextBoxTA.ForeColor = Color.Red
        End Try
    End Sub

    Private Sub TextBoxCT_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxCT.TextChanged
        Try
            Me.ThermalProfile.Calor_trocado = Conversor.ConverterParaSI(su.spmp_heatflow, Double.Parse(Me.TextBoxCT.Text))
            Me.TextBoxCT.ForeColor = Color.Blue
        Catch ex As Exception
            Me.TextBoxCT.ForeColor = Color.Red
        End Try
    End Sub

    Private Sub TextBoxTA2_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxTA2.TextChanged
        Try
            Me.ThermalProfile.Temp_amb_estimar = Conversor.ConverterParaSI(su.spmp_temperature, Double.Parse(Me.TextBoxTA2.Text))
            Me.TextBoxTA2.ForeColor = Color.Blue
        Catch ex As Exception
            Me.TextBoxTA2.ForeColor = Color.Red
        End Try
    End Sub

    Private Sub TextBoxCTERM_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxCTERM.TextChanged
        Try
            Me.ThermalProfile.Condtermica = Conversor.ConverterParaSI(su.spmp_thermalConductivity, Double.Parse(Me.TextBoxCTERM.Text))
            Me.TextBoxCTERM.ForeColor = Color.Blue
        Catch ex As Exception
            Me.TextBoxCTERM.ForeColor = Color.Red
        End Try
    End Sub

    Private Sub TextBoxESP_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxESP.TextChanged
        Try
            Me.ThermalProfile.Espessura = Conversor.ConverterParaSI(su.thickness, Double.Parse(Me.TextBoxESP.Text))
            Me.TextBoxESP.ForeColor = Color.Blue
        Catch ex As Exception
            Me.TextBoxESP.ForeColor = Color.Red
        End Try
    End Sub

    Private Sub TextBoxVEL_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxVEL.TextChanged
        Try
            Me.ThermalProfile.Velocidade = Double.Parse(Me.TextBoxVEL.Text)
            Me.TextBoxVEL.ForeColor = Color.Blue
        Catch ex As Exception
            Me.TextBoxVEL.ForeColor = Color.Red
        End Try
    End Sub

    Private Sub CheckBoxIPT_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxIPT.CheckedChanged
        Me.ThermalProfile.Incluir_paredes = Me.CheckBoxIPT.Checked
    End Sub

    Private Sub CheckBoxICTI_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxICTI.CheckedChanged
        Me.ThermalProfile.Incluir_cti = Me.CheckBoxICTI.Checked
    End Sub

    Private Sub CheckBoxII_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxII.CheckedChanged
        Me.ThermalProfile.Incluir_isolamento = Me.CheckBoxII.Checked
        If Me.CheckBoxII.Checked = False Then
            Me.ComboBoxMAT.Enabled = False
            Me.TextBoxCTERM.Enabled = False
            Me.TextBoxESP.Enabled = False
        Else
            Me.ComboBoxMAT.Enabled = True
            Me.TextBoxCTERM.Enabled = True
            Me.TextBoxESP.Enabled = True
        End If
    End Sub

    Private Sub CheckBoxICTE_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxICTE.CheckedChanged
        Me.ThermalProfile.Incluir_cte = Me.CheckBoxICTE.Checked
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

        ' Add any initialization after the InitializeComponent() call.
        Me.m_thermalprofile = New DWSIM.Editors.PipeEditor.ThermalEditorDefinitions

    End Sub

    Private Sub ComboBoxMAMB_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBoxMAMB.SelectedIndexChanged

        Try
            Me.ThermalProfile.Meio = Me.ComboBoxMAMB.SelectedIndex
        Catch ex As Exception

        End Try


    End Sub

End Class