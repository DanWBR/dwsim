Imports System.Threading.Tasks
Imports DWSIM.Thermodynamics.Databases.ChemeoLink

Public Class FormImportCompoundDataChemeo

    Private CurrentPanel As String = ""
    Private compounds As New List(Of String())
    Private compound As Global.DWSIM.Thermodynamics.BaseClasses.ConstantProperties

    Public BaseCompound As Global.DWSIM.Thermodynamics.BaseClasses.ConstantProperties

    Private Sub btnCancel_Click(sender As Object, e As EventArgs) Handles btnCancel.Click
        Me.Close()
    End Sub

    Private Sub FormImportCompoundDataKDB_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        CurrentPanel = "Panel1"
    End Sub

    Private Sub btnPrev_Click(sender As Object, e As EventArgs) Handles btnPrev.Click
        Select Case CurrentPanel
            Case "Panel1"
            Case "Panel2"
                Panel1.BringToFront()
                CurrentPanel = "Panel1"
                btnPrev.Enabled = False
                btnNext.Enabled = True
            Case "Panel3"
                Panel2.BringToFront()
                CurrentPanel = "Panel2"
                btnNext.Text = DWSIM.App.GetLocalString("NextButton")
        End Select
    End Sub

    Private Sub btnNext_Click(sender As Object, e As EventArgs) Handles btnNext.Click

        Select Case CurrentPanel

            Case "Panel1"

                btnPrev.Enabled = True
                Panel2.BringToFront()
                CurrentPanel = "Panel2"

                Dim fsearch As New FormSearchingChemeo()

                Dim tcs As New Threading.CancellationTokenSource()

                Dim searchtext As String = tbSearchString.Text

                FormMain.AnalyticsProvider?.RegisterEvent("Online Compound Cheméo Request", searchtext, Nothing)

                Me.Enabled = False

                fsearch.Show()

                Dim t As New Task(Of List(Of String()))(Function()
                                                            Return ChemeoParser.GetCompoundIDs(searchtext, False).GetAwaiter().GetResult()
                                                        End Function, tcs.Token)

                t.ContinueWith(Sub()
                                   UIThread(Sub()
                                                fsearch.Close()
                                                If DWSIM.App.IsRunningOnMono Then
                                                    fsearch.Hide()
                                                    fsearch.Close()
                                                End If
                                                Enabled = True
                                                Focus()
                                                If t.Exception Is Nothing Then
                                                    compounds = t.Result
                                                    lbFoundItems.Items.Clear()
                                                    For Each item In t.Result
                                                        lbFoundItems.Items.Add(item(0).PadRight(10) + item(1))
                                                    Next
                                                    If t.Result.Count = 0 Then
                                                        btnNext.Enabled = False
                                                    End If
                                                Else
                                                    MessageBox.Show(String.Format("{0} [{1}]", DWSIM.App.GetLocalString("CompoundOnlineSourceError"),
                                                                    ExceptionProcessing.ExceptionParser.GetFirstException(t.Exception).Message,
                                                                    DWSIM.App.GetLocalString("Erro")))
                                                End If
                                            End Sub)
                               End Sub)

                AddHandler fsearch.btnCancel.Click, Sub()
                                                        fsearch.Close()
                                                        Me.Enabled = True
                                                        Focus()
                                                        tcs.Cancel()
                                                    End Sub

                t.Start()

            Case "Panel2"

                Panel3.BringToFront()
                btnNext.Text = DWSIM.App.GetLocalString("Import")
                CurrentPanel = "Panel3"

                Dim fsearch As New FormSearchingChemeo()

                Dim tcs As New Threading.CancellationTokenSource()

                Dim id As String = compounds(lbFoundItems.SelectedIndex)(0)

                Me.dgResults.Rows.Clear()

                Me.Enabled = False

                fsearch.Show()

                Dim t As New Task(Of Global.DWSIM.Thermodynamics.BaseClasses.ConstantProperties)(Function()
                                                                                                     Return ChemeoParser.GetCompoundData2(id)
                                                                                                 End Function, tcs.Token)

                t.ContinueWith(Sub()
                                   UIThread(Sub()
                                                fsearch.Close()
                                                If DWSIM.App.IsRunningOnMono Then
                                                    fsearch.Hide()
                                                    fsearch.Close()
                                                End If
                                                Enabled = True
                                                Focus()
                                                If t.Exception Is Nothing Then
                                                    compound = t.Result
                                                    AddPropertiesToGrid()
                                                Else
                                                    MessageBox.Show(String.Format("{0} [{1}]", DWSIM.App.GetLocalString("CompoundOnlineSourceError"),
                                                        ExceptionProcessing.ExceptionParser.GetFirstException(t.Exception).Message,
                                                        DWSIM.App.GetLocalString("Erro")))
                                                End If

                                            End Sub)
                               End Sub)

                AddHandler fsearch.btnCancel.Click, Sub()
                                                        fsearch.Close()
                                                        Me.Enabled = True
                                                        Focus()
                                                        tcs.Cancel()
                                                    End Sub

                t.Start()

            Case "Panel3"

                CopySelectedData()

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

        With compound
            Me.dgResults.Rows.Add(New Object() {If(.CAS_Number <> "", okimg, noimg), "CAS Number", If(.CAS_Number <> "", True, False)})
            Me.dgResults.Rows.Add(New Object() {If(.Formula <> "", okimg, noimg), "Formula", If(.Formula <> "", True, False)})
            Me.dgResults.Rows.Add(New Object() {If(.InChI <> "", okimg, noimg), "InChI String", If(.InChI <> "", True, False)})
            Me.dgResults.Rows.Add(New Object() {If(.SMILES <> "", okimg, noimg), "SMILES String", If(.SMILES <> "", True, False)})
            Me.dgResults.Rows.Add(New Object() {If(.Molar_Weight <> 0.0#, okimg, noimg), "Molecular Weight", If(.Molar_Weight <> 0.0#, True, False)})
            Me.dgResults.Rows.Add(New Object() {If(.Normal_Boiling_Point <> 0.0#, okimg, noimg), "Normal Boiling Point", If(.Normal_Boiling_Point <> 0.0#, True, False)})
            Me.dgResults.Rows.Add(New Object() {If(.TemperatureOfFusion <> 0.0#, okimg, noimg), "Temperature of Fusion", If(.TemperatureOfFusion <> 0.0#, True, False)})
            Me.dgResults.Rows.Add(New Object() {If(.Critical_Temperature <> 0.0#, okimg, noimg), "Critical Temperature", If(.Critical_Temperature <> 0.0#, True, False)})
            Me.dgResults.Rows.Add(New Object() {If(.Critical_Pressure <> 0.0#, okimg, noimg), "Critical Pressure", If(.Critical_Pressure <> 0.0#, True, False)})
            Me.dgResults.Rows.Add(New Object() {If(.Critical_Volume <> 0.0#, okimg, noimg), "Critical Volume", If(.Critical_Volume <> 0.0#, True, False)})
            Me.dgResults.Rows.Add(New Object() {If(.Critical_Compressibility <> 0.0#, okimg, noimg), "Critical Compressibility Factor", If(.Critical_Compressibility <> 0.0#, True, False)})
            Me.dgResults.Rows.Add(New Object() {If(.Acentric_Factor <> 0.0#, okimg, noimg), "Acentric Factor", If(.Acentric_Factor <> 0.0#, True, False)})
            Me.dgResults.Rows.Add(New Object() {If(.Z_Rackett <> 0.0#, okimg, noimg), "Rackett Compressibility", If(.Z_Rackett <> 0.0#, True, False)})
            Me.dgResults.Rows.Add(New Object() {If(.IG_Enthalpy_of_Formation_25C <> 0.0#, okimg, noimg), "Enthalpy of Formation (IG)", If(.IG_Enthalpy_of_Formation_25C <> 0.0#, True, False)})
            Me.dgResults.Rows.Add(New Object() {If(.IG_Entropy_of_Formation_25C <> 0.0#, okimg, noimg), "Entropy of Formation (IG)", If(.IG_Entropy_of_Formation_25C <> 0.0#, True, False)})
            Me.dgResults.Rows.Add(New Object() {If(.IG_Gibbs_Energy_of_Formation_25C <> 0.0#, okimg, noimg), "Gibbs Energy of Formation (IG)", If(.IG_Gibbs_Energy_of_Formation_25C <> 0.0#, True, False)})
            Me.dgResults.Rows.Add(New Object() {If(.EnthalpyOfFusionAtTf <> 0.0#, okimg, noimg), "Enthalpy of Fusion", If(.EnthalpyOfFusionAtTf <> 0.0#, True, False)})
        End With


    End Sub

    Private Sub CopySelectedData()

        With compound
            If dgResults.Rows(0).Cells(2).Value = True Then BaseCompound.CAS_Number = .CAS_Number
            If dgResults.Rows(1).Cells(2).Value = True Then BaseCompound.Formula = .Formula
            If dgResults.Rows(2).Cells(2).Value = True Then BaseCompound.InChI = .InChI
            If dgResults.Rows(3).Cells(2).Value = True Then BaseCompound.SMILES = .SMILES
            If dgResults.Rows(4).Cells(2).Value = True Then BaseCompound.Molar_Weight = .Molar_Weight
            If dgResults.Rows(5).Cells(2).Value = True Then
                BaseCompound.Normal_Boiling_Point = .Normal_Boiling_Point
                BaseCompound.NBP = .NBP
            End If
            If dgResults.Rows(6).Cells(2).Value = True Then BaseCompound.TemperatureOfFusion = .TemperatureOfFusion
            If dgResults.Rows(7).Cells(2).Value = True Then BaseCompound.Critical_Temperature = .Critical_Temperature
            If dgResults.Rows(8).Cells(2).Value = True Then BaseCompound.Critical_Pressure = .Critical_Pressure
            If dgResults.Rows(9).Cells(2).Value = True Then BaseCompound.Critical_Volume = .Critical_Volume
            If dgResults.Rows(10).Cells(2).Value = True Then BaseCompound.Critical_Compressibility = .Critical_Compressibility
            If dgResults.Rows(11).Cells(2).Value = True Then BaseCompound.Acentric_Factor = .Acentric_Factor
            If dgResults.Rows(12).Cells(2).Value = True Then BaseCompound.Z_Rackett = .Z_Rackett
            If dgResults.Rows(13).Cells(2).Value = True Then BaseCompound.IG_Enthalpy_of_Formation_25C = .IG_Enthalpy_of_Formation_25C
            If dgResults.Rows(14).Cells(2).Value = True Then BaseCompound.IG_Entropy_of_Formation_25C = .IG_Entropy_of_Formation_25C
            If dgResults.Rows(15).Cells(2).Value = True Then BaseCompound.IG_Gibbs_Energy_of_Formation_25C = .IG_Gibbs_Energy_of_Formation_25C
            If dgResults.Rows(16).Cells(2).Value = True Then BaseCompound.EnthalpyOfFusionAtTf = .EnthalpyOfFusionAtTf
        End With

    End Sub

End Class