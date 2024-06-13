Imports System.IO
Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker
Imports DWSIM.Thermodynamics.Databases.DDBStructureLink

Public Class FormImportCompoundFromThermo

    Public compdata As Thermodynamics.BaseClasses.ConstantProperties

    Private searcher As New Thermodynamics.Databases.ChEDLThermoLink.ChEDLThermoParser()
    Private structuredata As Dictionary(Of String, List(Of String()))

    Private Sub LinkLabel2_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel2.LinkClicked

        Process.Start(LinkLabel2.Text)

    End Sub

    Private Sub LinkLabel1_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel1.LinkClicked

        Process.Start(LinkLabel1.Text)

    End Sub

    Private Sub WizardPage1_Commit(sender As Object, e As AeroWizard.WizardPageConfirmEventArgs) Handles WizardPage1.Commit

        Dim searchtext = tbSearchString.Text

        Try

            compdata = Nothing

            Dim result = searcher.SearchCompound(searchtext)

            tbQueryMatch.Text = result(0)

        Catch ex As Exception

            MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)

            e.Cancel = True

        End Try


    End Sub

    Private Sub WizardPage3_Commit(sender As Object, e As AeroWizard.WizardPageConfirmEventArgs) Handles WizardPage3.Commit

        If tbQueryMatch.Text <> "" Then

            Try
                compdata = searcher.GetCompoundData(tbQueryMatch.Text)
            Catch ex As Exception
            End Try

            If compdata IsNot Nothing Then

                'Try
                '    DDBStructureParser.GetData(DDBStructureParser.GetID(compdata.CAS_Number))
                'Catch ex As Exception
                'End Try

                AddPropertiesToGrid()

                tbImportAs.Text = System.Globalization.CultureInfo.CurrentUICulture.TextInfo.ToTitleCase(compdata.Name)

            End If

        End If

    End Sub

    Private Sub WizardPage4_Commit(sender As Object, e As AeroWizard.WizardPageConfirmEventArgs) Handles WizardPage4.Commit

        If Not structuredata Is Nothing Then
            With compdata
                If structuredata.ContainsKey("Original") Then
                    If .UNIFACGroups Is Nothing Then .UNIFACGroups = New SortedList
                    .UNIFACGroups.Clear()
                    For Each item In structuredata("Original")
                        .UNIFACGroups.Add(item(1), item(2))
                    Next
                End If
                If structuredata.ContainsKey("Modified") Then
                    If .MODFACGroups Is Nothing Then .UNIFACGroups = New SortedList
                    .MODFACGroups.Clear()
                    For Each item In structuredata("Modified")
                        .MODFACGroups.Add(item(1), item(2))
                    Next
                    If .NISTMODFACGroups Is Nothing Then .NISTMODFACGroups = New SortedList
                    .NISTMODFACGroups.Clear()
                    For Each sg As String In .MODFACGroups.Keys
                        .NISTMODFACGroups.Add(sg, .MODFACGroups(sg))
                    Next
                End If
            End With
        End If

        compdata.Name = tbImportAs.Text

        DialogResult = DialogResult.OK

    End Sub

    Sub AddPropertiesToGrid()

        Dim okimg = My.Resources.accept
        Dim noimg = My.Resources.cross

        If Not compdata Is Nothing Then

            With compdata

                Me.dgResults.Rows.Add(New Object() {If(.Name <> "", okimg, noimg), "Name"})
                Me.dgResults.Rows.Add(New Object() {If(.CAS_Number <> "", okimg, noimg), "CAS Number"})
                Me.dgResults.Rows.Add(New Object() {If(.Formula <> "", okimg, noimg), "Formula"})

                Me.dgResults.Rows.Add(New Object() {If(.InChI <> "", okimg, noimg), "InChI String"})
                Me.dgResults.Rows.Add(New Object() {If(.SMILES <> "", okimg, noimg), "SMILES String"})

                Me.dgResults.Rows.Add(New Object() {If(.Molar_Weight <> 0.0#, okimg, noimg), "Molecular Weight"})
                Me.dgResults.Rows.Add(New Object() {If(.Normal_Boiling_Point <> 0.0#, okimg, noimg), "Normal Boiling Point"})
                Me.dgResults.Rows.Add(New Object() {If(.TemperatureOfFusion <> 0.0#, okimg, noimg), "Fusion Temperature"})

                Me.dgResults.Rows.Add(New Object() {If(.Critical_Temperature <> 0.0#, okimg, noimg), "Critical Temperature"})
                Me.dgResults.Rows.Add(New Object() {If(.Critical_Pressure <> 0.0#, okimg, noimg), "Critical Pressure"})
                Me.dgResults.Rows.Add(New Object() {If(.Critical_Volume <> 0.0#, okimg, noimg), "Critical Volume"})
                Me.dgResults.Rows.Add(New Object() {If(.Critical_Compressibility <> 0.0#, okimg, noimg), "Critical Compressibility"})
                Me.dgResults.Rows.Add(New Object() {If(.Acentric_Factor <> 0.0#, okimg, noimg), "Acentric Factor"})

                Me.dgResults.Rows.Add(New Object() {If(.Z_Rackett <> 0.0#, okimg, noimg), "Rackett Compressibility Factor"})

                Me.dgResults.Rows.Add(New Object() {If(.IG_Enthalpy_of_Formation_25C <> 0.0#, okimg, noimg), "Enthalpy of Formation (IG)"})
                Me.dgResults.Rows.Add(New Object() {If(.IG_Entropy_of_Formation_25C <> 0.0#, okimg, noimg), "Entropy of Formation (IG)"})
                Me.dgResults.Rows.Add(New Object() {If(.IG_Gibbs_Energy_of_Formation_25C <> 0.0#, okimg, noimg), "Gibbs Energy of Formation (IG)"})

                Me.dgResults.Rows.Add(New Object() {If(.UNIQUAC_Q <> 0.0#, okimg, noimg), "UNIQUAC Q Parameter"})
                Me.dgResults.Rows.Add(New Object() {If(.UNIQUAC_R <> 0.0#, okimg, noimg), "UNIQUAC R Parameter"})

                Me.dgResults.Rows.Add(New Object() {If(.Dipole_Moment <> 0.0#, okimg, noimg), "Dipole Moment"})

                Me.dgResults.Rows.Add(New Object() {If(.Chao_Seader_Solubility_Parameter <> 0.0#, okimg, noimg), "Chao Seader Solubility Parameter"})

                Me.dgResults.Rows.Add(New Object() {If(.Vapor_Pressure_Constant_A <> 0.0#, okimg, noimg), "Vapor Pressure Curve Data"})

                Me.dgResults.Rows.Add(New Object() {If(.Ideal_Gas_Heat_Capacity_Const_A <> 0.0#, okimg, noimg), "Ideal Gas Heat Capacity Curve Data"})

                Me.dgResults.Rows.Add(New Object() {If(.Liquid_Heat_Capacity_Const_A <> 0.0#, okimg, noimg), "Liquid Phase Heat Capacity Curve Data"})

                Me.dgResults.Rows.Add(New Object() {If(.Vapor_Viscosity_Const_A <> 0.0#, okimg, noimg), "Vapor Phase Viscosity Curve Data"})

                Me.dgResults.Rows.Add(New Object() {If(.Liquid_Viscosity_Const_A <> 0.0#, okimg, noimg), "Liquid Phase Viscosity Curve Data"})

                Me.dgResults.Rows.Add(New Object() {If(.Vapor_Thermal_Conductivity_Const_A <> 0.0#, okimg, noimg), "Vapor Phase Thermal Conductivity Curve Data"})

                Me.dgResults.Rows.Add(New Object() {If(.Liquid_Thermal_Conductivity_Const_A <> 0.0#, okimg, noimg), "Liquid Phase Thermal Conductivity Curve Data"})

                Me.dgResults.Rows.Add(New Object() {If(.Surface_Tension_Const_A <> 0.0#, okimg, noimg), "Surface Tension Curve Data"})

                Me.dgResults.Rows.Add(New Object() {If(.Liquid_Density_Const_A <> 0.0#, okimg, noimg), "Liquid Density Data"})

                Me.dgResults.Rows.Add(New Object() {If(.HVap_A <> 0.0#, okimg, noimg), "Heat of Vaporization Data"})

            End With

            If Not structuredata Is Nothing Then

                Me.dgResults.Rows.Add(New Object() {If(structuredata.ContainsKey("Original"), okimg, noimg), "Original UNIFAC Structure Data"})
                Me.dgResults.Rows.Add(New Object() {If(structuredata.ContainsKey("Modified"), okimg, noimg), "Modified UNIFAC (Dortmund) Structure Data"})

            End If

        End If


    End Sub

    Private Sub FormImportCompoundFromThermo_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ChangeDefaultFont()

        ActiveControl = tbSearchString

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        FormMain.AnalyticsProvider?.RegisterEvent("Exporting Compound to JSON", "", Nothing)

        If compdata IsNot Nothing Then

            Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

            Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
                New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON File", "*.json")})

            If handler IsNot Nothing Then
                Using stream As New IO.MemoryStream()
                    Using writer As New StreamWriter(stream) With {.AutoFlush = True}
                        Try
                            Dim jsondata = Newtonsoft.Json.JsonConvert.SerializeObject(compdata, Newtonsoft.Json.Formatting.Indented)
                            writer.Write(jsondata)
                            handler.Write(stream)
                            MessageBox.Show(DWSIM.App.GetLocalString("FileSaved"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Information)
                        Catch ex As Exception
                            MessageBox.Show(DWSIM.App.GetLocalString("Erroaosalvararquivo") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                        End Try
                    End Using
                End Using
            End If

        End If

    End Sub

    Private Sub tbSearchString_KeyPress(sender As Object, e As KeyEventArgs) Handles tbSearchString.KeyDown

        If e.KeyCode = Keys.Enter Then WizardControl1.NextPage()

    End Sub

End Class