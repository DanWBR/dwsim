Imports System.Threading.Tasks
Imports DWSIM.Thermodynamics.Databases.ChEDLThermoLink
Imports DWSIM.Thermodynamics.Databases.DDBStructureLink

Public Class FormImportCompoundChEDLThermo

    Private CurrentPanel As String = ""
    Private compounds As New List(Of String())
    Private structuredata As Dictionary(Of String, List(Of String()))
    Private compound As Global.DWSIM.Thermodynamics.BaseClasses.ConstantProperties

    Public BaseCompound As Global.DWSIM.Thermodynamics.BaseClasses.ConstantProperties

    Private Sub btnCancel_Click(sender As Object, e As EventArgs) Handles btnCancel.Click
        Me.Close()
    End Sub

    Private Sub FormImportCompoundDataKDB_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        CurrentPanel = "Panel1"
        If GlobalSettings.Settings.PythonPath = "" And Not System.Environment.OSVersion.Platform = PlatformID.Unix Then
            MessageBox.Show(DWSIM.App.GetLocalString("ChEDLThermoError"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            Close()
        End If
    End Sub

    Private Sub btnPrev_Click(sender As Object, e As EventArgs) Handles btnPrev.Click
        Select Case CurrentPanel
            Case "Panel1"
            Case "Panel2"
                Panel1.BringToFront()
                CurrentPanel = "Panel1"
                btnPrev.Enabled = False
            Case "Panel3"
                Panel2.BringToFront()
                CurrentPanel = "Panel2"
                btnNext.Text = DWSIM.App.GetLocalString("NextButton")
                btnNext.Enabled = True
        End Select
    End Sub

    Private Sub btnNext_Click(sender As Object, e As EventArgs) Handles btnNext.Click

        Select Case CurrentPanel

            Case "Panel1"

                btnPrev.Enabled = True
                Panel2.BringToFront()
                CurrentPanel = "Panel2"

                Dim fsearch As New FormPythonLink()

                Dim tcs As New Threading.CancellationTokenSource()

                Dim searchtext As String = tbSearchString.Text

                Me.Enabled = False

                fsearch.Show()

                Dim parser = New ChEDLThermoParser()
                Dim t As New Task(Of List(Of String()))(Function()
                                                            Return parser.SearchCompound(searchtext)
                                                        End Function, tcs.Token)

                t.Start()

                t.ContinueWith(Sub()
                                   parser.KillPythonProcess()
                                   parser = Nothing
                                   UIThread(Sub()
                                                fsearch.Close()
                                                If DWSIM.App.IsRunningOnMono Then
                                                    fsearch.Hide()
                                                    fsearch.Close()
                                                End If
                                                Me.Enabled = True
                                                Focus()
                                                If t.Exception Is Nothing Then
                                                    compounds = t.Result
                                                    lbFoundItems.Items.Clear()
                                                    For Each item In t.Result
                                                        lbFoundItems.Items.Add(item(1))
                                                    Next
                                                    If t.Result.Count = 0 Then
                                                        btnNext.Enabled = False
                                                    End If
                                                Else
                                                    MessageBox.Show(t.Exception.GetBaseException.Message, DWSIM.App.GetLocalString("Erro"))
                                                End If
                                            End Sub)
                               End Sub)

                AddHandler fsearch.btnCancel.Click, Sub()
                                                        parser.KillPythonProcess()
                                                        parser = Nothing
                                                        fsearch.Close()
                                                        Me.Enabled = True
                                                        Focus()
                                                        tcs.Cancel()
                                                    End Sub


            Case "Panel2"

                Panel3.BringToFront()
                btnNext.Text = DWSIM.App.GetLocalString("FinishText")
                CurrentPanel = "Panel3"

                Dim fsearch As New FormPythonLink()

                Dim tcs As New Threading.CancellationTokenSource()

                Dim casid As String = compounds(lbFoundItems.SelectedIndex)(0)

                Me.dgResults.Rows.Clear()

                Me.Enabled = False

                fsearch.Show()

                Dim parser = New ChEDLThermoParser()

                Dim t1 As New Task(Of Global.DWSIM.Thermodynamics.BaseClasses.ConstantProperties)(Function()
                                                                                                      Return parser.GetCompoundData(casid)
                                                                                                  End Function, tcs.Token)
                Dim t3 As New Task(Of Dictionary(Of String, List(Of String())))(Function()
                                                                                    Return DDBStructureParser.GetData(DDBStructureParser.GetID(casid))
                                                                                End Function, tcs.Token)


                Task.Factory.StartNew(Sub()
                                          t1.Start()
                                          t3.Start()
                                          Task.WaitAll(t1, t3)
                                      End Sub).ContinueWith(Sub()
                                                                parser.KillPythonProcess()
                                                                parser = Nothing
                                                                UIThread(Sub()
                                                                             fsearch.Close()
                                                                             If DWSIM.App.IsRunningOnMono Then
                                                                                 fsearch.Hide()
                                                                                 fsearch.Close()
                                                                             End If
                                                                             Me.Enabled = True
                                                                             Focus()
                                                                             If Not t1.Status = TaskStatus.WaitingToRun AndAlso t1.Exception Is Nothing Then
                                                                                 compound = t1.Result
                                                                             End If
                                                                             If Not t3.Status = TaskStatus.WaitingToRun AndAlso t3.Exception Is Nothing Then
                                                                                 structuredata = t3.Result
                                                                             End If
                                                                             AddPropertiesToGrid()
                                                                             btnNext.Enabled = compound IsNot Nothing
                                                                         End Sub)
                                                            End Sub)

                AddHandler fsearch.btnCancel.Click, Sub()
                                                        parser.KillPythonProcess()
                                                        parser = Nothing
                                                        fsearch.Close()
                                                        Me.Enabled = True
                                                        Focus()
                                                        tcs.Cancel()
                                                    End Sub

            Case "Panel3"

                BaseCompound = compound.Clone
                BaseCompound.Name = compounds(lbFoundItems.SelectedIndex)(1)

                If Not structuredata Is Nothing Then
                    With BaseCompound
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

                Me.DialogResult = Windows.Forms.DialogResult.OK

                Me.Close()

        End Select
    End Sub

    Private Sub tbSearchString_KeyDown(sender As Object, e As KeyEventArgs) Handles tbSearchString.KeyDown
        If e.KeyCode = Keys.Enter Then btnNext_Click(sender, e)
    End Sub

    Sub AddPropertiesToGrid()

        Dim okimg = My.Resources.accept
        Dim noimg = My.Resources.cross

        If Not compound Is Nothing Then

            With compound

                Me.dgResults.Rows.Add(New Object() {If(.Name <> "", okimg, noimg), "Name"})
                Me.dgResults.Rows.Add(New Object() {If(.CAS_Number <> "", okimg, noimg), "CAS Number"})
                Me.dgResults.Rows.Add(New Object() {If(.Formula <> "", okimg, noimg), "Formula"})

            End With

        End If

        If Not structuredata Is Nothing Then

            Me.dgResults.Rows.Add(New Object() {If(structuredata.ContainsKey("Original"), okimg, noimg), "Original UNIFAC Structure Data"})
            Me.dgResults.Rows.Add(New Object() {If(structuredata.ContainsKey("Modified"), okimg, noimg), "Modified UNIFAC (Dortmund) Structure Data"})

        End If

        If Not compound Is Nothing Then

            With compound

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

        End If

    End Sub

    Private Sub LinkLabel1_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel1.LinkClicked
        Process.Start("https://pypi.python.org/pypi/thermo/")
    End Sub

    Private Sub LinkLabel3_LinkClicked_1(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel3.LinkClicked
        Process.Start("http://www.ddbst.com/unifacga.html")
    End Sub

End Class