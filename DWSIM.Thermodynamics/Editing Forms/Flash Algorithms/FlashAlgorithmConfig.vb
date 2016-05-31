Imports DWSIM.SharedClasses
Imports Microsoft.Win32
Imports DWSIM.Interfaces.Interfaces
Imports CapeOpen

Public Class FlashAlgorithmConfig

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Property Settings As Dictionary(Of Interfaces.Enums.FlashSetting, String)

    Public Property AvailableCompounds As List(Of String)

    Public FlashAlgo As Interfaces.IFlashAlgorithm

    Dim ci As Globalization.CultureInfo

    Public _coes As Object
    Public _selts As CapeOpenObjInfo
    Private _coobjects As New List(Of CapeOpenObjInfo)
    Private _loaded As Boolean = False

    Private Sub FlashAlgorithmConfig_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.Text += " - " & FlashAlgo.Tag

        Select Case FlashAlgo.AlgoType
            Case Interfaces.Enums.FlashMethod.Default_Algorithm, Interfaces.Enums.FlashMethod.Nested_Loops_VLE
                TabControl1.TabPages.Remove(TabPageGM)
                TabControl1.TabPages.Remove(TabPageIO)
                TabControl1.TabPages.Remove(TabPageVLLE)
                TabControl1.TabPages.Remove(TabPageCOES)
            Case Interfaces.Enums.FlashMethod.Nested_Loops_VLLE, Interfaces.Enums.FlashMethod.Nested_Loops_Immiscible_VLLE
                TabControl1.TabPages.Remove(TabPageGM)
                TabControl1.TabPages.Remove(TabPageIO)
                TabControl1.TabPages.Remove(TabPageCOES)
            Case Interfaces.Enums.FlashMethod.Gibbs_Minimization_VLE
                TabControl1.TabPages.Remove(TabPageNL)
                TabControl1.TabPages.Remove(TabPageIO)
                TabControl1.TabPages.Remove(TabPageVLLE)
                TabControl1.TabPages.Remove(TabPageCOES)
            Case Interfaces.Enums.FlashMethod.Gibbs_Minimization_VLLE
                TabControl1.TabPages.Remove(TabPageNL)
                TabControl1.TabPages.Remove(TabPageIO)
                TabControl1.TabPages.Remove(TabPageCOES)
            Case Interfaces.Enums.FlashMethod.Inside_Out_VLE
                TabControl1.TabPages.Remove(TabPageGM)
                TabControl1.TabPages.Remove(TabPageNL)
                TabControl1.TabPages.Remove(TabPageVLLE)
                TabControl1.TabPages.Remove(TabPageCOES)
            Case Interfaces.Enums.FlashMethod.Inside_Out_VLLE
                TabControl1.TabPages.Remove(TabPageGM)
                TabControl1.TabPages.Remove(TabPageNL)
                TabControl1.TabPages.Remove(TabPageCOES)
            Case Interfaces.Enums.FlashMethod.Nested_Loops_SLE_Eutectic, Interfaces.Enums.FlashMethod.Nested_Loops_SLE_SolidSolution, Interfaces.Enums.FlashMethod.Simple_LLE
                TabControl1.TabPages.Remove(TabPageGM)
                TabControl1.TabPages.Remove(TabPageNL)
                TabControl1.TabPages.Remove(TabPageIO)
                TabControl1.TabPages.Remove(TabPageVLLE)
                TabControl1.TabPages.Remove(TabPageCOES)
            Case Interfaces.Enums.FlashMethod.CAPE_OPEN_Equilibrium_Server
                TabControl1.TabPages.Remove(TabPageConvPars)
                TabControl1.TabPages.Remove(TabPageGM)
                TabControl1.TabPages.Remove(TabPageNL)
                TabControl1.TabPages.Remove(TabPageIO)
                TabControl1.TabPages.Remove(TabPageVLLE)
        End Select

        If FlashAlgo.AlgoType = Interfaces.Enums.FlashMethod.CAPE_OPEN_Equilibrium_Server Then

            _coobjects.Clear()

            'CAPE-OPEN 1.1 Equilibrium Calculator {cf51e386-0110-4ed8-acb7-b50cfde6908e}
            SearchCO("{cf51e386-0110-4ed8-acb7-b50cfde6908e}")
            'CAPE-OPEN 1.1 Property Package {cf51e384-0110-4ed8-acb7-b50cfde6908e}
            SearchCO("{cf51e384-0110-4ed8-acb7-b50cfde6908e}")
            'CAPE-OPEN 1.1 Physical Property Package Managers {cf51e383-0110-4ed8-acb7-b50cfde6908e}
            SearchCO("{cf51e383-0110-4ed8-acb7-b50cfde6908e}")

            Me.cbThermoServer.Items.Clear()

            For Each coui As CapeOpenObjInfo In _coobjects
                With coui
                    Me.cbThermoServer.Items.Add(.Name)
                End With
            Next

            If Not _selts Is Nothing Then
                cbThermoServer.SelectedItem = _selts.Name
                If Not _coes Is Nothing Then
                    Dim t As Type = Type.GetTypeFromProgID(_selts.TypeName)
                    _coes = Activator.CreateInstance(t)
                End If
                Dim myppm As CapeOpen.ICapeUtilities = TryCast(_coes, CapeOpen.ICapeUtilities)
                If Not myppm Is Nothing Then
                    Try
                        _coes.Initialize()
                    Catch ex As Exception
                        Dim ecu As CapeOpen.ECapeUser = myppm
                        MessageBox.Show("Error initializing CAPE-OPEN Equilibrium Server - " + ex.Message.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                        MessageBox.Show("CAPE-OPEN Exception " & ecu.code & " at " & ecu.interfaceName & "." & ecu.scope & ". Reason: " & ecu.description)
                    End Try
                End If

                Me.lblName2.Text = _selts.Name
                Me.lblVersion2.Text = _selts.Version
                Me.lblAuthorURL2.Text = _selts.VendorURL
                Me.lblDesc2.Text = _selts.Description
                Me.lblAbout2.Text = _selts.AboutInfo
            End If

            _loaded = True

        Else

            ci = Globalization.CultureInfo.InvariantCulture

            chkReplaceFlashPT.Checked = Settings(Interfaces.Enums.FlashSetting.Replace_PTFlash)
            chkValidateEqCalc.Checked = Settings(Interfaces.Enums.FlashSetting.ValidateEquilibriumCalc)
            tbFlashValidationTolerance.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.ValidationGibbsTolerance), ci).ToString
            chkDoPhaseId.Checked = Settings(Interfaces.Enums.FlashSetting.UsePhaseIdentificationAlgorithm)
            chkCalcBubbleDew.Checked = Settings(Interfaces.Enums.FlashSetting.CalculateBubbleAndDewPoints)

            tbPHExtMaxIt.Text = Integer.Parse(Settings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations), ci).ToString
            tbPHExtMaxTol.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance), ci).ToString
            tbPHIntMaxIt.Text = Integer.Parse(Settings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations), ci).ToString
            tbPHintMaxTol.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance), ci).ToString
            tbPTExtMaxIt.Text = Integer.Parse(Settings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations), ci).ToString
            tbPTExtTol.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance), ci).ToString
            tbPTintMaxIt.Text = Integer.Parse(Settings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations), ci).ToString
            tbPTIntTol.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance), ci).ToString

            chkFastModeNL.Checked = Settings(Interfaces.Enums.FlashSetting.NL_FastMode)

            chkUseBroydenIO.Checked = Settings(Interfaces.Enums.FlashSetting.IO_FastMode)

            Dim minmethods As String() = [Enum].GetNames(New PropertyPackages.Auxiliary.FlashAlgorithms.GibbsMinimization3P().Solver.GetType)
            cbMinMethodGM.Items.Clear()
            cbMinMethodGM.Items.AddRange(minmethods)

            cbMinMethodGM.SelectedItem = Settings(Interfaces.Enums.FlashSetting.GM_OptimizationMethod)

            Select Case Settings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity)
                Case 0
                    rbLow.Checked = True
                Case 1
                    rbMedium.Checked = True
                Case 2
                    rbHigh.Checked = True
            End Select

            SetupKeyCompounds()

        End If

    End Sub

    Private Sub FlashAlgorithmConfig_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing

        Try

            Settings(Interfaces.Enums.FlashSetting.Replace_PTFlash) = chkReplaceFlashPT.Checked
            Settings(Interfaces.Enums.FlashSetting.ValidateEquilibriumCalc) = chkValidateEqCalc.Checked
            Settings(Interfaces.Enums.FlashSetting.UsePhaseIdentificationAlgorithm) = chkDoPhaseId.Checked
            Settings(Interfaces.Enums.FlashSetting.CalculateBubbleAndDewPoints) = chkCalcBubbleDew.Checked

            Settings(Interfaces.Enums.FlashSetting.ValidationGibbsTolerance) = Double.Parse(tbFlashValidationTolerance.Text).ToString(ci)

            Settings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations) = Integer.Parse(tbPHExtMaxIt.Text).ToString(ci)
            Settings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance) = Double.Parse(tbPHExtMaxTol.Text).ToString(ci)
            Settings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations) = Integer.Parse(tbPHIntMaxIt.Text).ToString(ci)
            Settings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance) = Double.Parse(tbPHintMaxTol.Text).ToString(ci)
            Settings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations) = Integer.Parse(tbPTExtMaxIt.Text).ToString(ci)
            Settings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance) = Double.Parse(tbPTExtTol.Text).ToString(ci)
            Settings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations) = Integer.Parse(tbPTintMaxIt.Text).ToString(ci)
            Settings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance) = Double.Parse(tbPTIntTol.Text).ToString(ci)

            Settings(Interfaces.Enums.FlashSetting.NL_FastMode) = chkFastModeNL.Checked

            Settings(Interfaces.Enums.FlashSetting.IO_FastMode) = chkUseBroydenIO.Checked

            Settings(Interfaces.Enums.FlashSetting.GM_OptimizationMethod) = cbMinMethodGM.SelectedItem

            If rbLow.Checked Then Settings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity) = 0
            If rbMedium.Checked Then Settings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity) = 1
            If rbHigh.Checked Then Settings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity) = 2

            Dim comps As String = ""

            For Each lvi As ListViewItem In lvKeyComp.Items
                If lvi.Checked Then comps += lvi.Text + ","
            Next

            Settings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestCompIds) = comps

        Catch ex As Exception

            MessageBox.Show("Error parsing input. Some settings may not have been updated.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)

        End Try

    End Sub

    Private Sub SetupKeyCompounds()

        Dim selected As Array

        selected = Settings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestCompIds).ToArray(ci, Type.GetType("System.String"))

        Me.lvKeyComp.Items.Clear()

        For i As Integer = 0 To AvailableCompounds.Count - 1
            With Me.lvKeyComp.Items.Add(AvailableCompounds(i))
                For Each s As String In selected
                    If s = AvailableCompounds(i) Then
                        .Checked = True
                        Exit For
                    End If
                Next
            End With
        Next

    End Sub

    Sub SearchCO(ByVal CLSID As String)

        Dim keys As String() = My.Computer.Registry.ClassesRoot.OpenSubKey("CLSID", False).GetSubKeyNames()

        For Each k2 In keys
            Dim mykey As RegistryKey = My.Computer.Registry.ClassesRoot.OpenSubKey("CLSID", False).OpenSubKey(k2, False)
            For Each s As String In mykey.GetSubKeyNames()
                If s = "Implemented Categories" Then
                    Dim arr As Array = mykey.OpenSubKey("Implemented Categories").GetSubKeyNames()
                    For Each s2 As String In arr
                        If s2.ToLower = CLSID Then
                            'this is a CAPE-OPEN UO
                            Dim myuo As New CapeOpenObjInfo
                            With myuo
                                .AboutInfo = mykey.OpenSubKey("CapeDescription").GetValue("About")
                                .CapeVersion = mykey.OpenSubKey("CapeDescription").GetValue("CapeVersion")
                                .Description = mykey.OpenSubKey("CapeDescription").GetValue("Description")
                                .HelpURL = mykey.OpenSubKey("CapeDescription").GetValue("HelpURL")
                                .Name = mykey.OpenSubKey("CapeDescription").GetValue("Name")
                                .VendorURL = mykey.OpenSubKey("CapeDescription").GetValue("VendorURL")
                                .Version = mykey.OpenSubKey("CapeDescription").GetValue("ComponentVersion")
                                .ImplementedCategory = CLSID
                                Try
                                    .TypeName = mykey.OpenSubKey("ProgID").GetValue("")
                                Catch ex As Exception
                                End Try
                                Try
                                    .Location = mykey.OpenSubKey("InProcServer32").GetValue("")
                                Catch ex As Exception
                                    .Location = mykey.OpenSubKey("LocalServer32").GetValue("")
                                End Try
                            End With
                            _coobjects.Add(myuo)
                        End If
                    Next
                End If
            Next
            mykey.Close()
        Next

    End Sub


    Private Sub cbThermoServer_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbThermoServer.SelectedIndexChanged

        If _loaded Then

            For Each coui As CapeOpenObjInfo In _coobjects
                If coui.Name = cbThermoServer.SelectedItem.ToString Then
                    _selts = coui
                End If
            Next

            Dim t As Type = Type.GetTypeFromProgID(_selts.TypeName)
            _coes = Activator.CreateInstance(t)

            If TryCast(_coes, IPersistStreamInit) IsNot Nothing Then
                CType(_coes, IPersistStreamInit).InitNew()
            End If
            If TryCast(_coes, ICapeUtilities) IsNot Nothing Then
                CType(_coes, ICapeUtilities).Initialize()
            End If

            Me.lblName2.Text = _selts.Name
            Me.lblVersion2.Text = _selts.Version
            Me.lblAuthorURL2.Text = _selts.VendorURL
            Me.lblDesc2.Text = _selts.Description
            Me.lblAbout2.Text = _selts.AboutInfo

        End If

    End Sub

    Private Sub btnEditThermoServer_Click(sender As Object, e As EventArgs) Handles btnEditThermoServer.Click

        Dim myuo As ICapeUtilities = TryCast(_coes, ICapeUtilities)

        If Not myuo Is Nothing Then
            Try
                myuo.Edit()
            Catch ex As Exception
                MessageBox.Show("Object is not editable.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        Else
            MessageBox.Show("Object is not editable.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End If

    End Sub
End Class