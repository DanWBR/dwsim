Imports System.Threading.Tasks
Imports DWSIM.Thermodynamics.Databases.KDBLink

Public Class FormImportCompoundKDB

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

                Dim fsearch As New FormSearchingKDB()

                Dim tcs As New Threading.CancellationTokenSource()

                Dim searchtext As String = tbSearchString.Text

                FormMain.AnalyticsProvider?.RegisterEvent("Online Compound KDB Request", searchtext, Nothing)

                Me.Enabled = False

                fsearch.Show()

                Dim t As New Task(Of List(Of String()))(Function()
                                                            Return KDBParser.GetCompoundIDs(searchtext, False)
                                                        End Function, tcs.Token)

                t.ContinueWith(Sub()
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

                Dim fsearch As New FormSearchingKDB()

                Dim tcs As New Threading.CancellationTokenSource()

                Dim id As String = compounds(lbFoundItems.SelectedIndex)(0)

                Me.dgResults.Rows.Clear()

                Me.Enabled = False

                fsearch.Show()

                Dim t As New Task(Of Global.DWSIM.Thermodynamics.BaseClasses.ConstantProperties)(Function()
                                                                                                     Return KDBParser.GetCompoundData(Integer.Parse(id))
                                                                                                 End Function, tcs.Token)

                t.ContinueWith(Sub()
                                   UIThread(Sub()
                                                fsearch.Close()
                                                If DWSIM.App.IsRunningOnMono Then
                                                    fsearch.Hide()
                                                    fsearch.Close()
                                                End If
                                                Me.Enabled = True
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
            '0
            Me.dgResults.Rows.Add(New Object() {If(.Name <> "", okimg, noimg), "Name", If(.Name <> "", True, False)})
            '1
            Me.dgResults.Rows.Add(New Object() {If(.CAS_Number <> "", okimg, noimg), "CAS Number", If(.CAS_Number <> "", True, False)})
            '2
            Me.dgResults.Rows.Add(New Object() {If(.Formula <> "", okimg, noimg), "Formula", If(.Formula <> "", True, False)})

            '3
            Me.dgResults.Rows.Add(New Object() {If(.Molar_Weight <> 0.0#, okimg, noimg), "Molecular Weight", If(.Molar_Weight <> 0.0#, True, False)})
            '4
            Me.dgResults.Rows.Add(New Object() {If(.Normal_Boiling_Point <> 0.0#, okimg, noimg), "Normal Boiling Point", If(.Normal_Boiling_Point <> 0.0#, True, False)})
            '5
            Me.dgResults.Rows.Add(New Object() {If(.TemperatureOfFusion <> 0.0#, okimg, noimg), "Fusion Temperature", If(.TemperatureOfFusion <> 0.0#, True, False)})

            '6
            Me.dgResults.Rows.Add(New Object() {If(.Critical_Temperature <> 0.0#, okimg, noimg), "Critical Temperature", If(.Critical_Temperature <> 0.0#, True, False)})
            '7
            Me.dgResults.Rows.Add(New Object() {If(.Critical_Pressure <> 0.0#, okimg, noimg), "Critical Pressure", If(.Critical_Pressure <> 0.0#, True, False)})
            '8
            Me.dgResults.Rows.Add(New Object() {If(.Critical_Volume <> 0.0#, okimg, noimg), "Critical Volume", If(.Critical_Volume <> 0.0#, True, False)})
            '9
            Me.dgResults.Rows.Add(New Object() {If(.Critical_Compressibility <> 0.0#, okimg, noimg), "Critical Compressibility", If(.Critical_Compressibility <> 0.0#, True, False)})

            '10
            Me.dgResults.Rows.Add(New Object() {If(.Acentric_Factor <> 0.0#, okimg, noimg), "Acentric Factor", If(.Acentric_Factor <> 0.0#, True, False)})

            '11
            Me.dgResults.Rows.Add(New Object() {If(.Z_Rackett <> 0.0#, okimg, noimg), "Rackett Compressibility Factor", If(.Z_Rackett <> 0.0#, True, False)})

            '12
            Me.dgResults.Rows.Add(New Object() {If(.IG_Enthalpy_of_Formation_25C <> 0.0#, okimg, noimg), "Enthalpy of Formation (IG)", If(.IG_Enthalpy_of_Formation_25C <> 0.0#, True, False)})
            '13
            Me.dgResults.Rows.Add(New Object() {If(.IG_Entropy_of_Formation_25C <> 0.0#, okimg, noimg), "Entropy of Formation (IG)", If(.IG_Entropy_of_Formation_25C <> 0.0#, True, False)})
            '14
            Me.dgResults.Rows.Add(New Object() {If(.IG_Gibbs_Energy_of_Formation_25C <> 0.0#, okimg, noimg), "Gibbs Energy of Formation (IG)", If(.IG_Gibbs_Energy_of_Formation_25C <> 0.0#, True, False)})

            '15
            Me.dgResults.Rows.Add(New Object() {If(.UNIQUAC_Q <> 0.0#, okimg, noimg), "UNIQUAC Q Parameter", If(.UNIQUAC_Q <> 0.0#, True, False)})
            '16
            Me.dgResults.Rows.Add(New Object() {If(.UNIQUAC_R <> 0.0#, okimg, noimg), "UNIQUAC R Parameter", If(.UNIQUAC_R <> 0.0#, True, False)})

            '17
            Me.dgResults.Rows.Add(New Object() {If(.Dipole_Moment <> 0.0#, okimg, noimg), "Dipole Moment", If(.Dipole_Moment <> 0.0#, True, False)})

            '18
            Me.dgResults.Rows.Add(New Object() {If(.Chao_Seader_Solubility_Parameter <> 0.0#, okimg, noimg), "Chao Seader Solubility Parameter", If(.Chao_Seader_Solubility_Parameter <> 0.0#, True, False)})

            '19
            Me.dgResults.Rows.Add(New Object() {If(.Vapor_Pressure_Constant_A <> 0.0#, okimg, noimg), "Vapor Pressure Curve Data", If(.Vapor_Pressure_Constant_A <> 0.0#, True, False)})

            '20
            Me.dgResults.Rows.Add(New Object() {If(.Ideal_Gas_Heat_Capacity_Const_A <> 0.0#, okimg, noimg), "Ideal Gas Heat Capacity Curve Data", If(.Ideal_Gas_Heat_Capacity_Const_A <> 0.0#, True, False)})

            '21
            Me.dgResults.Rows.Add(New Object() {If(.Liquid_Heat_Capacity_Const_A <> 0.0#, okimg, noimg), "Liquid Phase Heat Capacity Curve Data", If(.Liquid_Heat_Capacity_Const_A <> 0.0#, True, False)})

            '22
            Me.dgResults.Rows.Add(New Object() {If(.Vapor_Viscosity_Const_A <> 0.0#, okimg, noimg), "Vapor Phase Viscosity Curve Data", If(.Vapor_Viscosity_Const_A <> 0.0#, True, False)})

            '23
            Me.dgResults.Rows.Add(New Object() {If(.Liquid_Viscosity_Const_A <> 0.0#, okimg, noimg), "Liquid Phase Viscosity Curve Data", If(.Liquid_Viscosity_Const_A <> 0.0#, True, False)})

            '24
            Me.dgResults.Rows.Add(New Object() {If(.Vapor_Thermal_Conductivity_Const_A <> 0.0#, okimg, noimg), "Vapor Phase Thermal Conductivity Curve Data", If(.Vapor_Thermal_Conductivity_Const_A <> 0.0#, True, False)})

            '25
            Me.dgResults.Rows.Add(New Object() {If(.Liquid_Thermal_Conductivity_Const_A <> 0.0#, okimg, noimg), "Liquid Phase Thermal Conductivity Curve Data", If(.Liquid_Thermal_Conductivity_Const_A <> 0.0#, True, False)})

            '26
            Me.dgResults.Rows.Add(New Object() {If(.Surface_Tension_Const_A <> 0.0#, okimg, noimg), "Surface Tension Curve Data", If(.Surface_Tension_Const_A <> 0.0#, True, False)})

            '27
            Me.dgResults.Rows.Add(New Object() {If(.Liquid_Density_Const_A <> 0.0#, okimg, noimg), "Liquid Density Data", If(.Liquid_Density_Const_A <> 0.0#, True, False)})

            '28
            Me.dgResults.Rows.Add(New Object() {If(.HVap_A <> 0.0#, okimg, noimg), "Heat of Vaporization Data", If(.HVap_A <> 0.0#, True, False)})

        End With


    End Sub

    Private Sub CopySelectedData()

        With compound
            BaseCompound.ID = .ID
            '0
            If dgResults.Rows(0).Cells(2).Value = True Then BaseCompound.Name = .Name
            '1
            If dgResults.Rows(1).Cells(2).Value = True Then BaseCompound.CAS_Number = .CAS_Number
            '2
            If dgResults.Rows(2).Cells(2).Value = True Then BaseCompound.Formula = .Formula
            '3
            If dgResults.Rows(3).Cells(2).Value = True Then BaseCompound.Molar_Weight = .Molar_Weight
            '4
            If dgResults.Rows(4).Cells(2).Value = True Then
                BaseCompound.Normal_Boiling_Point = .Normal_Boiling_Point
                BaseCompound.NBP = .NBP
            End If
            '5
            If dgResults.Rows(5).Cells(2).Value = True Then BaseCompound.TemperatureOfFusion = .TemperatureOfFusion
            '6
            If dgResults.Rows(6).Cells(2).Value = True Then BaseCompound.Critical_Temperature = .Critical_Temperature
            '7
            If dgResults.Rows(7).Cells(2).Value = True Then BaseCompound.Critical_Pressure = .Critical_Pressure
            '8
            If dgResults.Rows(8).Cells(2).Value = True Then BaseCompound.Critical_Volume = .Critical_Volume
            '9
            If dgResults.Rows(9).Cells(2).Value = True Then BaseCompound.Critical_Compressibility = .Critical_Compressibility
            '10
            If dgResults.Rows(10).Cells(2).Value = True Then BaseCompound.Acentric_Factor = .Acentric_Factor
            '11
            If dgResults.Rows(11).Cells(2).Value = True Then BaseCompound.Z_Rackett = .Z_Rackett
            '12
            If dgResults.Rows(12).Cells(2).Value = True Then BaseCompound.IG_Enthalpy_of_Formation_25C = .IG_Enthalpy_of_Formation_25C
            '13
            If dgResults.Rows(13).Cells(2).Value = True Then BaseCompound.IG_Entropy_of_Formation_25C = .IG_Entropy_of_Formation_25C
            '14
            If dgResults.Rows(14).Cells(2).Value = True Then BaseCompound.IG_Gibbs_Energy_of_Formation_25C = .IG_Gibbs_Energy_of_Formation_25C
            '15
            If dgResults.Rows(15).Cells(2).Value = True Then BaseCompound.UNIQUAC_Q = .UNIQUAC_Q
            '16
            If dgResults.Rows(16).Cells(2).Value = True Then BaseCompound.UNIQUAC_R = .UNIQUAC_R
            '17
            If dgResults.Rows(17).Cells(2).Value = True Then BaseCompound.Dipole_Moment = .Dipole_Moment
            '18
            If dgResults.Rows(18).Cells(2).Value = True Then BaseCompound.Chao_Seader_Solubility_Parameter = .Chao_Seader_Solubility_Parameter

            '19
            If dgResults.Rows(19).Cells(2).Value = True Then
                BaseCompound.Vapor_Pressure_TMIN = .Vapor_Pressure_TMIN
                BaseCompound.Vapor_Pressure_TMAX = .Vapor_Pressure_TMAX
                BaseCompound.VaporPressureEquation = .VaporPressureEquation
                BaseCompound.Vapor_Pressure_Constant_A = .Vapor_Pressure_Constant_A
                BaseCompound.Vapor_Pressure_Constant_B = .Vapor_Pressure_Constant_B
                BaseCompound.Vapor_Pressure_Constant_C = .Vapor_Pressure_Constant_C
                BaseCompound.Vapor_Pressure_Constant_D = .Vapor_Pressure_Constant_D
                BaseCompound.Vapor_Pressure_Constant_E = .Vapor_Pressure_Constant_E
            End If

            '20
            If dgResults.Rows(20).Cells(2).Value = True Then
                BaseCompound.IdealgasCpEquation = .IdealgasCpEquation
                BaseCompound.Ideal_Gas_Heat_Capacity_Const_A = .Ideal_Gas_Heat_Capacity_Const_A
                BaseCompound.Ideal_Gas_Heat_Capacity_Const_B = .Ideal_Gas_Heat_Capacity_Const_B
                BaseCompound.Ideal_Gas_Heat_Capacity_Const_C = .Ideal_Gas_Heat_Capacity_Const_C
                BaseCompound.Ideal_Gas_Heat_Capacity_Const_D = .Ideal_Gas_Heat_Capacity_Const_D
                BaseCompound.Ideal_Gas_Heat_Capacity_Const_E = .Ideal_Gas_Heat_Capacity_Const_E
            End If

            '21
            If dgResults.Rows(21).Cells(2).Value = True Then
                BaseCompound.LiquidHeatCapacityEquation = .LiquidHeatCapacityEquation
                BaseCompound.Liquid_Heat_Capacity_Tmin = .Liquid_Heat_Capacity_Tmin
                BaseCompound.Liquid_Heat_Capacity_Tmax = .Liquid_Heat_Capacity_Tmax
                BaseCompound.Liquid_Heat_Capacity_Const_A = .Liquid_Heat_Capacity_Const_A
                BaseCompound.Liquid_Heat_Capacity_Const_B = .Liquid_Heat_Capacity_Const_B
                BaseCompound.Liquid_Heat_Capacity_Const_C = .Liquid_Heat_Capacity_Const_C
                BaseCompound.Liquid_Heat_Capacity_Const_D = .Liquid_Heat_Capacity_Const_D
                BaseCompound.Liquid_Heat_Capacity_Const_E = .Liquid_Heat_Capacity_Const_E
            End If

            '22
            If dgResults.Rows(22).Cells(2).Value = True Then
                BaseCompound.VaporViscosityEquation = .VaporViscosityEquation
                BaseCompound.Vapor_Viscosity_Tmin = .Vapor_Viscosity_Tmin
                BaseCompound.Vapor_Viscosity_Tmax = .Vapor_Viscosity_Tmax
                BaseCompound.Vapor_Viscosity_Const_A = .Vapor_Viscosity_Const_A
                BaseCompound.Vapor_Viscosity_Const_B = .Vapor_Viscosity_Const_B
                BaseCompound.Vapor_Viscosity_Const_C = .Vapor_Viscosity_Const_C
                BaseCompound.Vapor_Viscosity_Const_D = .Vapor_Viscosity_Const_D
                BaseCompound.Vapor_Viscosity_Const_E = .Vapor_Viscosity_Const_E
            End If

            '23
            If dgResults.Rows(23).Cells(2).Value = True Then
                BaseCompound.LiquidViscosityEquation = .LiquidViscosityEquation
                BaseCompound.Liquid_Viscosity_Const_A = .Liquid_Viscosity_Const_A
                BaseCompound.Liquid_Viscosity_Const_B = .Liquid_Viscosity_Const_B
                BaseCompound.Liquid_Viscosity_Const_C = .Liquid_Viscosity_Const_C
                BaseCompound.Liquid_Viscosity_Const_D = .Liquid_Viscosity_Const_D
                BaseCompound.Liquid_Viscosity_Const_E = .Liquid_Viscosity_Const_E
            End If

            '24
            If dgResults.Rows(24).Cells(2).Value = True Then
                BaseCompound.VaporThermalConductivityEquation = .VaporThermalConductivityEquation
                BaseCompound.Vapor_Thermal_Conductivity_Tmin = .Vapor_Thermal_Conductivity_Tmin
                BaseCompound.Vapor_Thermal_Conductivity_Tmax = .Vapor_Thermal_Conductivity_Tmax
                BaseCompound.Vapor_Thermal_Conductivity_Const_A = .Vapor_Thermal_Conductivity_Const_A
                BaseCompound.Vapor_Thermal_Conductivity_Const_B = .Vapor_Thermal_Conductivity_Const_B
                BaseCompound.Vapor_Thermal_Conductivity_Const_C = .Vapor_Thermal_Conductivity_Const_C
                BaseCompound.Vapor_Thermal_Conductivity_Const_D = .Vapor_Thermal_Conductivity_Const_D
                BaseCompound.Vapor_Thermal_Conductivity_Const_E = .Vapor_Thermal_Conductivity_Const_E
            End If

            '25
            If dgResults.Rows(25).Cells(2).Value = True Then
                BaseCompound.LiquidThermalConductivityEquation = .LiquidThermalConductivityEquation
                BaseCompound.Liquid_Thermal_Conductivity_Tmin = .Liquid_Thermal_Conductivity_Tmin
                BaseCompound.Liquid_Thermal_Conductivity_Tmax = .Liquid_Thermal_Conductivity_Tmax
                BaseCompound.Liquid_Thermal_Conductivity_Const_A = .Liquid_Thermal_Conductivity_Const_A
                BaseCompound.Liquid_Thermal_Conductivity_Const_B = .Liquid_Thermal_Conductivity_Const_B
                BaseCompound.Liquid_Thermal_Conductivity_Const_C = .Liquid_Thermal_Conductivity_Const_C
                BaseCompound.Liquid_Thermal_Conductivity_Const_D = .Liquid_Thermal_Conductivity_Const_D
                BaseCompound.Liquid_Thermal_Conductivity_Const_E = .Liquid_Thermal_Conductivity_Const_E
            End If

            '26
            If dgResults.Rows(26).Cells(2).Value = True Then
                BaseCompound.Surface_Tension_Const_A = .Surface_Tension_Const_A
                BaseCompound.Surface_Tension_Const_B = .Surface_Tension_Const_B
            End If

            '27
            If dgResults.Rows(27).Cells(2).Value = True Then
                BaseCompound.Liquid_Density_Const_A = .Liquid_Density_Const_A
                BaseCompound.Liquid_Density_Const_B = .Liquid_Density_Const_B
            End If

            '28
            If dgResults.Rows(28).Cells(2).Value = True Then
                BaseCompound.HVap_A = .HVap_A
                BaseCompound.HVap_B = .HVap_B
            End If


        End With


    End Sub

End Class