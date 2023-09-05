Imports com.ggasoftware.indigo
'http://www.ggasoftware.com/opensource/indigo

'    Copyright 2011 Daniel Wagner O. de Medeiros
'              2013 Gregor Reichert
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

Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary
Imports DWSIM.Thermodynamics.Utilities.Hypos.Methods
Imports DWSIM.MathOps.MathEx.Common
Imports System.IO
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Math
Imports Microsoft.VisualBasic.FileIO
Imports System.Globalization
Imports DWSIM.Simulate365.Models
Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker

Public Class FormCompoundCreator

    Inherits Form

    Public su As New SystemsOfUnits.Units
    Public nf As String

    Public methods As HYP
    Public jb As New Joback
    Friend m_props As PROPS

    Public mycase As New CompoundGeneratorCase
    Public simulate365File As S365File = Nothing
    Friend loaded As Boolean = False
    Friend PureUNIFACCompound As Boolean = True
    Friend isDWSimSaved As Boolean = True
    Friend isUserDBSaved As Boolean = True
    Private forceclose As Boolean = False
    Private populating As Boolean = False
    Private UNIFAClines, MODFACLines, JOBACKlines, ElementLines As New List(Of String)

    Private Sub FormCompoundCreator_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Me.MenuStrip1.Visible = False

        graphCPIG.GraphPane.Border.IsVisible = False
        graphLIQCP.GraphPane.Border.IsVisible = False
        graphLIQDENS.GraphPane.Border.IsVisible = False
        graphLIQTC.GraphPane.Border.IsVisible = False
        graphLIQVISC.GraphPane.Border.IsVisible = False
        graphPVAP.GraphPane.Border.IsVisible = False
        graphSOLIDCP.GraphPane.Border.IsVisible = False
        graphSOLIDDENS.GraphPane.Border.IsVisible = False

        'Grid UNIFAC

        Dim pathsep = System.IO.Path.DirectorySeparatorChar
        Dim picpath As String = My.Application.Info.DirectoryPath & pathsep & "data" & pathsep & "unifac" & pathsep

        Dim i As Integer
        Dim ID, GroupType, GroupName, S, TT As String
        Dim L As Boolean = True

        Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(jb.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.unifac.txt")
            Using parser As New TextFieldParser(filestr)
                While Not parser.EndOfData
                    UNIFAClines.Add(parser.ReadLine())
                End While
            End Using
        End Using

        L = True

        GroupName = UNIFAClines(2).Split(",")(2)
        With Me.GridUNIFAC.Rows
            .Clear()
            For i = 2 To UNIFAClines.Count - 1
                S = picpath & UNIFAClines(i).Split(",")(7) & ".png"
                If Not My.Computer.FileSystem.FileExists(S) Then S = picpath & "Empty.png"

                .Add(New Object() {" ", " ", CInt(0), Image.FromFile(S)})
                .Item(.Count - 1).HeaderCell.Value = "ID " & UNIFAClines(i).Split(",")(1) 'SubGroup
                .Item(.Count - 1).Cells(0).Value = UNIFAClines(i).Split(",")(2) 'MainGroup
                .Item(.Count - 1).Cells(1).Value = UNIFAClines(i).Split(",")(3) 'SubGroup
                TT = "Rk / Qk: " & UNIFAClines(i).Split(",")(4) & " / " & UNIFAClines(i).Split(",")(5) & vbCrLf &
                                                         "Example Compound: " & UNIFAClines(i).Split(",")(6) & vbCrLf &
                                                         "Joback subgroups: " & UNIFAClines(i).Split(",")(8)
                .Item(.Count - 1).Cells(3).Tag = {S, TT, UNIFAClines(i).Split(",")(1)}

                If GroupName <> UNIFAClines(i).Split(",")(2) Then
                    L = Not L
                    GroupName = UNIFAClines(i).Split(",")(2)
                End If
                If L Then
                    .Item(.Count - 1).Cells(0).Style.BackColor = Color.FromArgb(230, 230, 200)
                    .Item(.Count - 1).Cells(1).Style.BackColor = Color.FromArgb(230, 230, 200)
                Else
                    .Item(.Count - 1).Cells(0).Style.BackColor = Color.FromArgb(200, 230, 230)
                    .Item(.Count - 1).Cells(1).Style.BackColor = Color.FromArgb(200, 230, 230)
                End If
            Next
        End With

        'Grid MODFAC
        L = True

        Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(jb.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.modfac.txt")
            Using parser As New TextFieldParser(filestr)
                While Not parser.EndOfData
                    MODFACLines.Add(parser.ReadLine())
                End While
            End Using
        End Using

        Dim cult As Globalization.CultureInfo = New Globalization.CultureInfo("en-US")
        Dim fields As String()
        Dim delimiter As String = ";"
        Dim maingroup As Integer = 1
        Dim mainname As String = ""

        Me.GridMODFAC.Rows.Clear()

        Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(jb.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.modfac.txt")
            Using parser As New TextFieldParser(filestr)
                parser.SetDelimiters(delimiter)
                parser.ReadLine()

                While Not parser.EndOfData
                    fields = parser.ReadFields()
                    With Me.GridMODFAC.Rows
                        S = picpath & fields(6) & ".png"
                        If Not My.Computer.FileSystem.FileExists(S) Then S = picpath & "Empty.png"

                        .Add(New Object() {CInt(0), CInt(0), CInt(0), Image.FromFile(S)})
                        .Item(.Count - 1).HeaderCell.Value = "ID " & fields(3)
                        .Item(.Count - 1).Cells(0).Value = fields(1)
                        .Item(.Count - 1).Cells(1).Value = fields(2)
                        .Item(.Count - 1).Cells(2).Value = 0
                        TT = "Rk / Qk: " & fields(4) & " / " & fields(5) & vbCrLf &
                                                                 "Example Compound: " & fields(6) & vbCrLf & fields(7)

                        .Item(.Count - 1).Cells(3).Tag = {S, TT, fields(3)}

                        If L Then
                            .Item(.Count - 1).Cells(0).Style.BackColor = Color.FromArgb(230, 230, 200)
                            .Item(.Count - 1).Cells(1).Style.BackColor = Color.FromArgb(230, 230, 200)
                        Else
                            .Item(.Count - 1).Cells(0).Style.BackColor = Color.FromArgb(200, 230, 230)
                            .Item(.Count - 1).Cells(1).Style.BackColor = Color.FromArgb(200, 230, 230)
                        End If
                    End With
                End While
            End Using
        End Using


        'Grid NIST-MODFAC
        L = True

        Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(jb.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.NIST-MODFAC_RiQi.txt")
            Using parser As New TextFieldParser(filestr)
                While Not parser.EndOfData
                    MODFACLines.Add(parser.ReadLine())
                End While
            End Using
        End Using

        delimiter = vbTab
        Me.GridNISTMODFAC.Rows.Clear()

        Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(jb.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.NIST-MODFAC_RiQi.txt")
            Using parser As New TextFieldParser(filestr)
                parser.SetDelimiters(delimiter)
                parser.ReadLine()
                parser.ReadLine()

                While Not parser.EndOfData
                    fields = parser.ReadFields()
                    If fields(0).StartsWith("(") Then
                        maingroup = fields(0).Split(")")(0).Substring(1)
                        mainname = fields(0).Trim().Split(")")(1).Trim
                        L = Not L
                    Else
                        With Me.GridNISTMODFAC.Rows
                            S = picpath & fields(4) & ".png"
                            If Not My.Computer.FileSystem.FileExists(S) Then S = picpath & "Empty.png"

                            .Add(New Object() {CInt(0), CInt(0), CInt(0), Image.FromFile(S)})
                            .Item(.Count - 1).HeaderCell.Value = "ID " & fields(0)
                            .Item(.Count - 1).Cells(0).Value = mainname
                            .Item(.Count - 1).Cells(1).Value = fields(1)
                            .Item(.Count - 1).Cells(2).Value = 0
                            TT = "Rk / Qk: " & fields(2) & " / " & fields(3) & vbCrLf &
                                                                     "Example Compound: " & fields(4) & vbCrLf & fields(5)
                            .Item(.Count - 1).Cells(3).Tag = {S, TT, fields(0)}

                            If L Then
                                .Item(.Count - 1).Cells(0).Style.BackColor = Color.FromArgb(230, 230, 200)
                                .Item(.Count - 1).Cells(1).Style.BackColor = Color.FromArgb(230, 230, 200)
                            Else
                                .Item(.Count - 1).Cells(0).Style.BackColor = Color.FromArgb(200, 230, 230)
                                .Item(.Count - 1).Cells(1).Style.BackColor = Color.FromArgb(200, 230, 230)
                            End If
                        End With
                    End If
                End While
            End Using
        End Using

        'Grid Joback

        Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(jb.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.JobackGroups.txt")
            Using parser As New TextFieldParser(filestr)
                While Not parser.EndOfData
                    JOBACKlines.Add(parser.ReadLine())
                End While
            End Using
        End Using

        GroupType = ""

        With Me.GridJoback.Rows
            .Clear()
            For i = 1 To JOBACKlines.Count - 1

                ID = JOBACKlines(i).Split(";")(0)

                If Not ID = "X" Then
                    .Add(New Object())
                    .Item(.Count - 1).HeaderCell.Value = "ID " & ID
                    .Item(.Count - 1).Cells(0).Value = GroupType
                    GroupName = JOBACKlines(i).Split(";")(1)
                    .Item(.Count - 1).Cells(1).Value = GroupName

                    If L Then
                        .Item(.Count - 1).Cells(0).Style.BackColor = Color.FromArgb(200, 230, 230)
                    Else
                        .Item(.Count - 1).Cells(0).Style.BackColor = Color.FromArgb(230, 230, 200)
                    End If
                Else
                    GroupType = JOBACKlines(i).Split(";")(1)
                    L = Not L
                End If
            Next

        End With

        'Grid addition Elements

        Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(jb.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.Elements.txt")
            Using parser As New TextFieldParser(filestr)
                While Not parser.EndOfData
                    ElementLines.Add(parser.ReadLine())
                End While
            End Using
        End Using

        With Me.AddAtomDataGrid.Rows
            .Clear()
            For i = 1 To ElementLines.Count - 1
                .Add(New Object())
                .Item(.Count - 1).Cells(0).Value = ElementLines(i).Split(";")(2)
                .Item(.Count - 1).Cells(0).ToolTipText = "Element # " & ElementLines(i).Split(";")(0) & vbCrLf &
                                    ElementLines(i).Split(";")(1) & vbCrLf & "MW: " & ElementLines(i).Split(";")(3)
            Next
        End With

        With mycase
            .cp.VaporPressureEquation = 0
            .cp.IdealgasCpEquation = 0
            .cp.LiquidHeatCapacityEquation = 0
            .cp.LiquidDensityEquation = 0
            .cp.LiquidViscosityEquation = 0
            .cp.LiquidThermalConductivityEquation = 0
        End With

        cbEqPVAP.SelectedIndex = 0
        cbEqCPIG.SelectedIndex = 0
        cbEqCPLiquid.SelectedIndex = 0
        cbEqLIQDENS.SelectedIndex = 0
        cbEqLIQVISC.SelectedIndex = 0
        cbEqSolidDENS.SelectedIndex = 0
        cbEqCpS.SelectedIndex = 0
        cbEqTCLiquid.SelectedIndex = 0

        Me.cbUnits.Items.Clear()

        For Each su2 In CType(Me.MdiParent, FormMain).AvailableUnitSystems.Values
            Me.cbUnits.Items.Add(su2.Name)
        Next

        Me.cbUnits.SelectedIndex = 0

        Me.TextBoxID.Text = New Random().Next(100000)

        UpdateUnits()

    End Sub

    Private Sub FormCompoundCreator_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles MyBase.FormClosing

        Dim x = MessageBox.Show(DWSIM.App.GetLocalString("Desejasalvarasaltera"), DWSIM.App.GetLocalString("Fechando") & " " & Me.Text, MessageBoxButtons.YesNoCancel, MessageBoxIcon.Question)

        If x = MsgBoxResult.Yes Then

            FormMain.SaveFile(False)

        ElseIf x = MsgBoxResult.Cancel Then

            FormMain.CancelClosing = True
            e.Cancel = True

        End If

    End Sub

    Sub WriteData()

        Dim i As Integer

        With mycase

            Me.Text = .Filename
            If .su.temperature IsNot Nothing Then
                Me.su = .su
                If Not CType(Me.MdiParent, FormMain).AvailableUnitSystems.ContainsKey(.su.Name) Then
                    If Not TypeOf .su Is SystemsOfUnits.SI And Not _
                        TypeOf .su Is SystemsOfUnits.CGS And Not _
                        TypeOf .su Is SystemsOfUnits.English Then
                        CType(Me.MdiParent, FormMain).AvailableUnitSystems.Add(.su.Name, .su)
                    End If
                    Me.cbUnits.Items.Add(mycase.su.Name)
                Else
                    UpdateUnits()
                End If
            Else
                .su = Me.su
            End If
            If cbUnits.Items.Contains(.su.Name) Then
                cbUnits.SelectedIndex = cbUnits.Items.IndexOf(.su.Name)
            Else
                cbUnits.SelectedIndex = 0
            End If

            UpdateUnits()

            tsmiIgnoreUnsupportedGroups.Checked = .IgnoreUnsupportedGroups

            TextBoxID.Text = .cp.ID
            TextBoxName.Text = .cp.Name
            TextBoxComments.Text = .cp.Comments

            CheckBoxMW.Checked = .CalcMW
            CheckBoxNBP.Checked = .CalcNBP
            CheckBoxAF.Checked = .CalcAF
            CheckBoxCSAF.Checked = .CalcCSAF
            CheckBoxCSLV.Checked = .CalcCSMV
            CheckBoxCSSP.Checked = .CalcCSSP
            CheckBoxTc.Checked = .CalcTC
            CheckBoxPc.Checked = .CalcPC
            CheckBoxZc.Checked = .CalcZC
            CheckBoxZRa.Checked = .CalcZRA
            CheckBoxDHF.Checked = .CalcHF
            CheckBoxDGF.Checked = .CalcGF
            CheckBoxMeltingTemp.Checked = .CalcMP
            CheckBoxEnthOfFusion.Checked = .CalcEM

            'tbDBPath.Text = .database
            TextBoxAF.Text = .cp.Acentric_Factor
            TextBoxCAS.Text = .cp.CAS_Number
            TextBoxCSAF.Text = .cp.Chao_Seader_Acentricity
            TextBoxCSLV.Text = .cp.Chao_Seader_Liquid_Molar_Volume
            TextBoxCSSP.Text = .cp.Chao_Seader_Solubility_Parameter
            TextBoxDGF.Text = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, .cp.IG_Gibbs_Energy_of_Formation_25C).ToString("N2")
            TextBoxDHF.Text = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, .cp.IG_Enthalpy_of_Formation_25C).ToString("N2")
            TextBoxFormula.Text = .cp.Formula
            TextBoxMW.Text = .cp.Molar_Weight
            TextBoxNBP.Text = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, .cp.Normal_Boiling_Point)
            TextBoxPc.Text = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, .cp.Critical_Pressure)
            TextBoxUNIQUAC_Q.Text = .cp.UNIQUAC_Q
            TextBoxUNIQUAC_R.Text = .cp.UNIQUAC_R
            TextBoxTc.Text = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, .cp.Critical_Temperature)
            TextBoxVTCPR.Text = .cp.PR_Volume_Translation_Coefficient
            TextBoxVTCSRK.Text = .cp.SRK_Volume_Translation_Coefficient
            TextBoxZc.Text = .cp.Critical_Compressibility
            TextBoxZRa.Text = .cp.Z_Rackett
            TextBoxMeltingTemp.Text = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, .cp.TemperatureOfFusion)
            TextBoxEnthOfFusion.Text = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, .cp.EnthalpyOfFusionAtTf)
            TextBoxSMILES.Text = .cp.SMILES

            If Not .cp.SMILES = "" Then
                RenderSMILES()
            End If

            If .RegressPVAP Then rbRegressPVAP.Checked = True
            If .RegressCPIG Then rbRegressCPIG.Checked = True
            If .RegressCPLiquid Then rbRegressCPLiquid.Checked = True
            If .RegressLDENS Then rbRegressLIQDENS.Checked = True
            If .RegressLVISC Then rbRegressLIQVISC.Checked = True
            If .RegressCpS Then rbRegressSolidCp.Checked = True
            If .RegressRoS Then rbRegressSolidDens.Checked = True
            If .RegressLTC Then rbRegressTCLiquid.Checked = True

            If .EqPVAP Then rbCoeffPVAP.Checked = True
            If .EqCPIG Then rbCoeffCPIG.Checked = True
            If .EqCPLiquid Then rbCoeffCPLiquid.Checked = True
            If .EqLDENS Then rbCoeffLIQDENS.Checked = True
            If .EqLVISC Then rbCoeffLIQVISC.Checked = True
            If .EqCpS Then rbCoeffSolidCp.Checked = True
            If .EqSDens Then rbCoeffSolidDens.Checked = True
            If .EqLTC Then rbCoeffTCLiquid.Checked = True

            AtomDataGrid.Rows.Clear()
            For i = 0 To .cp.Elements.Count - 1
                AtomDataGrid.Rows.Add(New Object() { .cp.Elements.GetKey(i), .cp.Elements.GetByIndex(i)})
            Next

            If Integer.TryParse(.cp.VaporPressureEquation, New Integer) Then
                For Each it As Object In cbEqPVAP.Items
                    If it.ToString.Split(":")(0) = .cp.VaporPressureEquation Then
                        cbEqPVAP.SelectedIndex = cbEqPVAP.Items.IndexOf(it)
                        Exit For
                    End If
                Next
            Else
                cbEqPVAP.SelectedIndex = cbEqPVAP.Items.Count - 1
                tbUserDefEqPVAP.Text = .cp.VaporPressureEquation
            End If

            If Integer.TryParse(.cp.IdealgasCpEquation, New Integer) Then
                For Each it As Object In cbEqCPIG.Items
                    If it.ToString.Split(":")(0) = .cp.IdealgasCpEquation Then
                        cbEqCPIG.SelectedIndex = cbEqCPIG.Items.IndexOf(it)
                        Exit For
                    End If
                Next
            Else
                cbEqCPIG.SelectedIndex = cbEqCPIG.Items.Count - 1
                tbUserDefCPIGEq.Text = .cp.IdealgasCpEquation
            End If

            If Integer.TryParse(.cp.LiquidHeatCapacityEquation, New Integer) Then
                For Each it As Object In cbEqCPLiquid.Items
                    If it.ToString.Split(":")(0) = .cp.LiquidHeatCapacityEquation Then
                        cbEqCPLiquid.SelectedIndex = cbEqCPLiquid.Items.IndexOf(it)
                        Exit For
                    End If
                Next
            Else
                cbEqCPLiquid.SelectedIndex = cbEqCPLiquid.Items.Count - 1
                tbUserDefCPLEq.Text = .cp.LiquidHeatCapacityEquation
            End If

            If Integer.TryParse(.cp.LiquidDensityEquation, New Integer) Then
                For Each it As Object In cbEqLIQDENS.Items
                    If it.ToString.Split(":")(0) = .cp.LiquidDensityEquation Then
                        cbEqLIQDENS.SelectedIndex = cbEqLIQDENS.Items.IndexOf(it)
                        Exit For
                    End If
                Next
            Else
                cbEqLIQDENS.SelectedIndex = cbEqLIQDENS.Items.Count - 1
                tbUserDefDensLiqEq.Text = .cp.LiquidDensityEquation
            End If

            If Integer.TryParse(.cp.LiquidViscosityEquation, New Integer) Then
                For Each it As Object In cbEqLIQVISC.Items
                    If it.ToString.Split(":")(0) = .cp.LiquidViscosityEquation Then
                        cbEqLIQVISC.SelectedIndex = cbEqLIQVISC.Items.IndexOf(it)
                        Exit For
                    End If
                Next
            Else
                cbEqLIQVISC.SelectedIndex = cbEqLIQVISC.Items.Count - 1
                tbUserDefLiqViscEq.Text = .cp.LiquidViscosityEquation
            End If

            If Integer.TryParse(.cp.LiquidThermalConductivityEquation, New Integer) Then
                For Each it As Object In cbEqTCLiquid.Items
                    If it.ToString.Split(":")(0) = .cp.LiquidThermalConductivityEquation Then
                        cbEqTCLiquid.SelectedIndex = cbEqTCLiquid.Items.IndexOf(it)
                        Exit For
                    End If
                Next
            Else
                cbEqTCLiquid.SelectedIndex = cbEqTCLiquid.Items.Count - 1
                tbUserDefTCEq.Text = .cp.LiquidThermalConductivityEquation
            End If

            For Each it As Object In cbEqCpS.Items
                If it.ToString.Split(":")(0) = .cp.SolidHeatCapacityEquation Then
                    cbEqCpS.SelectedIndex = cbEqCpS.Items.IndexOf(it)
                    Exit For
                End If
            Next

            For Each it As Object In cbEqSolidDENS.Items
                If it.ToString.Split(":")(0) = .cp.SolidDensityEquation Then
                    cbEqSolidDENS.SelectedIndex = cbEqSolidDENS.Items.IndexOf(it)
                    Exit For
                End If
            Next

            tbCpS_A.Text = .cp.Solid_Heat_Capacity_Const_A
            tbCpS_B.Text = .cp.Solid_Heat_Capacity_Const_B
            tbCpS_C.Text = .cp.Solid_Heat_Capacity_Const_C
            tbCpS_D.Text = .cp.Solid_Heat_Capacity_Const_D
            tbCpS_E.Text = .cp.Solid_Heat_Capacity_Const_E

            tbRoS_A.Text = .cp.Solid_Density_Const_A
            tbRoS_B.Text = .cp.Solid_Density_Const_B
            tbRoS_C.Text = .cp.Solid_Density_Const_C
            tbRoS_D.Text = .cp.Solid_Density_Const_D
            tbRoS_E.Text = .cp.Solid_Density_Const_E

            tbPVAP_A.Text = .cp.Vapor_Pressure_Constant_A
            tbPVAP_B.Text = .cp.Vapor_Pressure_Constant_B
            tbPVAP_C.Text = .cp.Vapor_Pressure_Constant_C
            tbPVAP_D.Text = .cp.Vapor_Pressure_Constant_D
            tbPVAP_E.Text = .cp.Vapor_Pressure_Constant_E

            tbCPIG_A.Text = .cp.Ideal_Gas_Heat_Capacity_Const_A
            tbCPIG_B.Text = .cp.Ideal_Gas_Heat_Capacity_Const_B
            tbCPIG_C.Text = .cp.Ideal_Gas_Heat_Capacity_Const_C
            tbCPIG_D.Text = .cp.Ideal_Gas_Heat_Capacity_Const_D
            tbCPIG_E.Text = .cp.Ideal_Gas_Heat_Capacity_Const_E

            tbCPLiquid_A.Text = .cp.Liquid_Heat_Capacity_Const_A
            tbCPLiquid_B.Text = .cp.Liquid_Heat_Capacity_Const_B
            tbCPLiquid_C.Text = .cp.Liquid_Heat_Capacity_Const_C
            tbCPLiquid_D.Text = .cp.Liquid_Heat_Capacity_Const_D
            tbCPLiquid_E.Text = .cp.Liquid_Heat_Capacity_Const_E

            tbLIQDENS_A.Text = .cp.Liquid_Density_Const_A
            tbLIQDENS_B.Text = .cp.Liquid_Density_Const_B
            tbLIQDENS_C.Text = .cp.Liquid_Density_Const_C
            tbLIQDENS_D.Text = .cp.Liquid_Density_Const_D
            tbLIQDENS_E.Text = .cp.Liquid_Density_Const_E

            tbLIQVISC_A.Text = .cp.Liquid_Viscosity_Const_A
            tbLIQVISC_B.Text = .cp.Liquid_Viscosity_Const_B
            tbLIQVISC_C.Text = .cp.Liquid_Viscosity_Const_C
            tbLIQVISC_D.Text = .cp.Liquid_Viscosity_Const_D
            tbLIQVISC_E.Text = .cp.Liquid_Viscosity_Const_E

            tbTCLiquid_A.Text = .cp.Liquid_Thermal_Conductivity_Const_A
            tbTCLiquid_B.Text = .cp.Liquid_Thermal_Conductivity_Const_B
            tbTCLiquid_C.Text = .cp.Liquid_Thermal_Conductivity_Const_C
            tbTCLiquid_D.Text = .cp.Liquid_Thermal_Conductivity_Const_D
            tbTCLiquid_E.Text = .cp.Liquid_Thermal_Conductivity_Const_E

            populating = True
            For Each r As DataGridViewRow In Me.GridUNIFAC.Rows
                If .cp.UNIFACGroups(r.Cells(1).Value) <> "" Then r.Cells(2).Value = .cp.UNIFACGroups(r.Cells(1).Value) 'old file format - Subgroup name
                If .cp.UNIFACGroups(r.Cells(3).Tag(2)) <> "" Then r.Cells(2).Value = .cp.UNIFACGroups(r.Cells(3).Tag(2)) 'new file format - Subgroup ID

                If r.Cells(2).Value > 0 Then
                    r.Cells(2).Style.BackColor = Color.PaleGreen
                Else
                    r.Cells(2).Style.BackColor = Color.White
                End If

            Next
            For Each r As DataGridViewRow In Me.GridMODFAC.Rows
                If .cp.MODFACGroups(r.Cells(1).Value) <> "" Then r.Cells(2).Value = .cp.MODFACGroups(r.Cells(1).Value) 'old file format - Subgroup name
                If .cp.MODFACGroups(r.Cells(3).Tag(2)) <> "" Then r.Cells(2).Value = .cp.MODFACGroups(r.Cells(3).Tag(2)) 'new file format - Subgroup ID

                If r.Cells(2).Value > 0 Then
                    r.Cells(2).Style.BackColor = Color.PaleGreen
                Else
                    r.Cells(2).Style.BackColor = Color.White
                End If
            Next

            If .cp.NISTMODFACGroups Is Nothing Then
                .cp.NISTMODFACGroups = New SortedList
            End If

            For Each r As DataGridViewRow In Me.GridNISTMODFAC.Rows
                If .cp.NISTMODFACGroups(r.Cells(1).Value) <> "" Then r.Cells(2).Value = .cp.NISTMODFACGroups(r.Cells(1).Value) 'old file format - Subgroup name
                If .cp.NISTMODFACGroups(r.Cells(3).Tag(2)) <> "" Then r.Cells(2).Value = .cp.NISTMODFACGroups(r.Cells(3).Tag(2)) 'new file format - Subgroup ID

                If r.Cells(2).Value > 0 Then
                    r.Cells(2).Style.BackColor = Color.PaleGreen
                Else
                    r.Cells(2).Style.BackColor = Color.White
                End If
            Next

            FillUnifacSubGroups()

            'check for updated elements when loading older files and create them if not existing
            If .JobackGroups Is Nothing Then .JobackGroups = New ArrayList()
            If .AdditionalAtoms Is Nothing Then .AdditionalAtoms = New ArrayList
            If .DataRoS Is Nothing Then .DataRoS = New ArrayList
            If .DataCpS Is Nothing Then .DataCpS = New ArrayList
            If .DataCPLiquid Is Nothing Then .DataCPLiquid = New ArrayList
            If .DataLTC Is Nothing Then .DataLTC = New ArrayList

            'populating Joback Grid with additional Joback groups
            For i = 0 To .JobackGroups.Count - 1
                Me.GridJoback.Rows.Item(.JobackGroups.Item(i)(0)).Cells(3).Value = .JobackGroups.Item(i)(1)
                If .JobackGroups.Item(i)(1) > 0 Then PureUNIFACCompound = False
            Next

            'populating AtomGrid with additional Atoms
            For i = 0 To .AdditionalAtoms.Count - 1
                Me.AddAtomDataGrid.Rows(.AdditionalAtoms.Item(i)(0)).Cells(1).Value = .AdditionalAtoms.Item(i)(1)
            Next

            populating = False

            Me.GridExpDataPVAP.Rows.Clear()
            For i = 0 To .DataPVAP.Count - 1
                Me.GridExpDataPVAP.Rows.Add(New Object() {SystemsOfUnits.Converter.ConvertFromSI(su.temperature, .DataPVAP(i)(0)), SystemsOfUnits.Converter.ConvertFromSI(su.pressure, .DataPVAP(i)(1))})
            Next
            Me.GridExpDataCPIG.Rows.Clear()
            For i = 0 To .DataCPIG.Count - 1
                Me.GridExpDataCPIG.Rows.Add(New Object() {SystemsOfUnits.Converter.ConvertFromSI(su.temperature, .DataCPIG(i)(0)), SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, .DataCPIG(i)(1))})
            Next
            Me.GridExpDataCPLiquid.Rows.Clear()
            For i = 0 To .DataCPLiquid.Count - 1
                Me.GridExpDataCPLiquid.Rows.Add(New Object() {SystemsOfUnits.Converter.ConvertFromSI(su.temperature, .DataCPLiquid(i)(0)), SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, .DataCPLiquid(i)(1))})
            Next
            Me.GridExpDataLIQDENS.Rows.Clear()
            For i = 0 To .DataLDENS.Count - 1
                Me.GridExpDataLIQDENS.Rows.Add(New Object() {SystemsOfUnits.Converter.ConvertFromSI(su.temperature, .DataLDENS(i)(0)), SystemsOfUnits.Converter.ConvertFromSI(su.density, .DataLDENS(i)(1))})
            Next
            Me.GridExpDataLIQVISC.Rows.Clear()
            For i = 0 To .DataLVISC.Count - 1
                Me.GridExpDataLIQVISC.Rows.Add(New Object() {SystemsOfUnits.Converter.ConvertFromSI(su.temperature, .DataLVISC(i)(0)), SystemsOfUnits.Converter.ConvertFromSI(su.viscosity, .DataLVISC(i)(1))})
            Next
            Me.GridExpDataTCLiquid.Rows.Clear()
            For i = 0 To .DataLTC.Count - 1
                Me.GridExpDataTCLiquid.Rows.Add(New Object() {SystemsOfUnits.Converter.ConvertFromSI(su.temperature, .DataLTC(i)(0)), SystemsOfUnits.Converter.ConvertFromSI(su.thermalConductivity, .DataLTC(i)(1))})
            Next
            Me.GridExpDataRoS.Rows.Clear()
            For i = 0 To .DataRoS.Count - 1
                Me.GridExpDataRoS.Rows.Add(New Object() {SystemsOfUnits.Converter.ConvertFromSI(su.temperature, .DataRoS(i)(0)), SystemsOfUnits.Converter.ConvertFromSI(su.density, .DataRoS(i)(1)) * .cp.Molar_Weight})
            Next
            Me.GridExpDataCpS.Rows.Clear()
            For i = 0 To .DataCpS.Count - 1
                Me.GridExpDataCpS.Rows.Add(New Object() {SystemsOfUnits.Converter.ConvertFromSI(su.temperature, .DataCpS(i)(0)), SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, .DataCpS(i)(1)) / .cp.Molar_Weight})
            Next
            If .RegressOKPVAP Then tbStatusPVAP.Text = "OK" Else tbStatusPVAP.Text = .ErrorMsgPVAP
            If .RegressOKCPIG Then tbStatusCPIG.Text = "OK" Else tbStatusCPIG.Text = .ErrorMsgCPIG
            If .RegressOKCPLiquid Then tbStatusCPLiquid.Text = "OK" Else tbStatusCPLiquid.Text = .ErrorMsgCPLiquid
            If .RegressOKLDENS Then tbStatusLIQDENS.Text = "OK" Else tbStatusLIQDENS.Text = .ErrorMsgLDENS
            If .RegressOKLVISC Then tbStatusLIQVISC.Text = "OK" Else tbStatusLIQVISC.Text = .ErrorMsgLVISC
            If .RegressOKRoS Then tbStatusSolidDens.Text = "OK" Else tbStatusSolidDens.Text = .ErrorMsgRoS
            If .RegressOKCpS Then tbStatusSolidCp.Text = "OK" Else tbStatusSolidCp.Text = .ErrorMsgCpS
            If .RegressOKLTC Then tbStatusTCLiquid.Text = "OK" Else tbStatusTCLiquid.Text = .ErrorMsgLTC

            CheckDataStatus()

        End With

    End Sub

    Sub FillUnifacSubGroups()

        'fill Joback groups table with UNIFAC subgoups
        Dim k, ugc, usgc, usgid, oc As Integer
        Dim JG, JSG As String

        For Each r As DataGridViewRow In Me.GridJoback.Rows
            r.Cells(2).Value = Nothing
        Next

        For Each r As DataGridViewRow In Me.GridUNIFAC.Rows
            'Joback groups from UNIFAC subgroups
            If r.Cells(2).Value > 0 Then
                ugc = mycase.cp.UNIFACGroups(r.Cells(3).Tag(2))
                JG = UNIFAClines(r.Index + 2).Split(",")(8) 'Joback Subgroup List
                For k = 0 To 3
                    JSG = JG.Split("/")(k)
                    If Not JSG = "" Then
                        usgc = JSG.Split(":")(0) 'Joback subgroup count
                        usgid = JSG.Split(":")(1) 'Joback subgroup ID
                        oc = Me.GridJoback.Rows.Item(usgid - 1).Cells(2).Value
                        Me.GridJoback.Rows.Item(usgid - 1).Cells(2).Value = oc + usgc * ugc
                    End If
                Next
            End If
        Next
    End Sub

    Sub StoreData()

        With mycase

            .su = Me.su

            .cp.ID = TextBoxID.Text
            .cp.Name = TextBoxName.Text
            .cp.Comments = TextBoxComments.Text

            .cp.OriginalDB = "User"
            .cp.CurrentDB = "User"

            .cp.IsBlackOil = False

            .cp.Acentric_Factor = CheckEmptyTextBox(TextBoxAF)
            .cp.CAS_Number = TextBoxCAS.Text
            .cp.CompCreatorStudyFile = .Filename
            .cp.Chao_Seader_Acentricity = CheckEmptyTextBox(TextBoxCSAF)
            .cp.Chao_Seader_Liquid_Molar_Volume = CheckEmptyTextBox(TextBoxCSLV)
            .cp.Chao_Seader_Solubility_Parameter = CheckEmptyTextBox(TextBoxCSSP)
            .cp.IG_Gibbs_Energy_of_Formation_25C = SystemsOfUnits.Converter.ConvertToSI(su.enthalpy, CheckEmptyTextBox(TextBoxDGF))
            .cp.IG_Enthalpy_of_Formation_25C = SystemsOfUnits.Converter.ConvertToSI(su.enthalpy, CheckEmptyTextBox(TextBoxDHF))
            .cp.Formula = TextBoxFormula.Text
            .cp.Molar_Weight = CheckEmptyTextBox(TextBoxMW)
            .cp.Normal_Boiling_Point = SystemsOfUnits.Converter.ConvertToSI(su.temperature, CheckEmptyTextBox(TextBoxNBP))
            .cp.Critical_Pressure = SystemsOfUnits.Converter.ConvertToSI(su.pressure, CheckEmptyTextBox(TextBoxPc))
            .cp.UNIQUAC_Q = CheckEmptyTextBox(TextBoxUNIQUAC_Q)
            .cp.UNIQUAC_R = CheckEmptyTextBox(TextBoxUNIQUAC_R)
            .cp.Critical_Temperature = SystemsOfUnits.Converter.ConvertToSI(su.temperature, CheckEmptyTextBox(TextBoxTc))
            .cp.PR_Volume_Translation_Coefficient = CheckEmptyTextBox(TextBoxVTCPR)
            .cp.SRK_Volume_Translation_Coefficient = CheckEmptyTextBox(TextBoxVTCSRK)
            .cp.Critical_Compressibility = CheckEmptyTextBox(TextBoxZc)
            .cp.Z_Rackett = CheckEmptyTextBox(TextBoxZRa)
            .cp.SMILES = TextBoxSMILES.Text
            .cp.TemperatureOfFusion = SystemsOfUnits.Converter.ConvertToSI(su.temperature, CheckEmptyTextBox(TextBoxMeltingTemp))
            .cp.EnthalpyOfFusionAtTf = SystemsOfUnits.Converter.ConvertToSI(su.enthalpy, CheckEmptyTextBox(TextBoxEnthOfFusion))

            .RegressPVAP = rbRegressPVAP.Checked
            .RegressCPIG = rbRegressCPIG.Checked
            .RegressCPLiquid = rbRegressCPLiquid.Checked
            .RegressLDENS = rbRegressLIQDENS.Checked
            .RegressLVISC = rbRegressLIQVISC.Checked
            .RegressRoS = rbRegressSolidDens.Checked
            .RegressCpS = rbRegressSolidCp.Checked
            .RegressLTC = rbRegressTCLiquid.Checked

            .EqPVAP = rbCoeffPVAP.Checked
            .EqCPIG = rbCoeffCPIG.Checked
            .EqCPLiquid = rbCoeffCPLiquid.Checked
            .EqLDENS = rbCoeffLIQDENS.Checked
            .EqLVISC = rbCoeffLIQVISC.Checked
            .EqSDens = rbCoeffSolidDens.Checked
            .EqCpS = rbCoeffSolidCp.Checked
            .EqLTC = rbCoeffTCLiquid.Checked

            .CalcMW = CheckBoxMW.Checked
            .CalcNBP = CheckBoxNBP.Checked
            .CalcAF = CheckBoxAF.Checked
            .CalcCSAF = CheckBoxCSAF.Checked
            .CalcCSMV = CheckBoxCSLV.Checked
            .CalcCSSP = CheckBoxCSSP.Checked
            .CalcTC = CheckBoxTc.Checked
            .CalcPC = CheckBoxPc.Checked
            .CalcZC = CheckBoxZc.Checked
            .CalcZRA = CheckBoxZRa.Checked
            .CalcHF = CheckBoxDHF.Checked
            .CalcGF = CheckBoxDGF.Checked
            .CalcMP = CheckBoxMeltingTemp.Checked
            .CalcEM = CheckBoxEnthOfFusion.Checked

            .cp.VaporPressureEquation = cbEqPVAP.SelectedItem.ToString.Split(":")(0)
            If .cp.VaporPressureEquation = "1000" Then .cp.VaporPressureEquation = tbUserDefEqPVAP.Text

            .cp.LiquidDensityEquation = cbEqLIQDENS.SelectedItem.ToString.Split(":")(0)
            If .cp.LiquidDensityEquation = "1000" Then .cp.LiquidDensityEquation = tbUserDefDensLiqEq.Text

            .cp.LiquidViscosityEquation = cbEqLIQVISC.SelectedItem.ToString.Split(":")(0)
            If .cp.LiquidViscosityEquation = "1000" Then .cp.LiquidViscosityEquation = tbUserDefLiqViscEq.Text

            .cp.SolidHeatCapacityEquation = cbEqCpS.SelectedIndex.ToString.Split(":")(0)
            .cp.SolidDensityEquation = cbEqSolidDENS.SelectedIndex.ToString.Split(":")(0)

            .cp.IdealgasCpEquation = cbEqCPIG.SelectedItem.ToString.Split(":")(0)
            If .cp.IdealgasCpEquation = "1000" Then .cp.IdealgasCpEquation = tbUserDefCPIGEq.Text

            .cp.LiquidHeatCapacityEquation = cbEqCPLiquid.SelectedItem.ToString.Split(":")(0)
            If .cp.LiquidHeatCapacityEquation = "1000" Then .cp.LiquidHeatCapacityEquation = tbUserDefCPLEq.Text

            .cp.LiquidThermalConductivityEquation = cbEqTCLiquid.SelectedItem.ToString.Split(":")(0)
            If .cp.LiquidThermalConductivityEquation = "1000" Then .cp.LiquidThermalConductivityEquation = tbUserDefTCEq.Text

            .cp.Solid_Heat_Capacity_Const_A = CheckEmptyCell(tbCpS_A.Text)
            .cp.Solid_Heat_Capacity_Const_B = CheckEmptyCell(tbCpS_B.Text)
            .cp.Solid_Heat_Capacity_Const_C = CheckEmptyCell(tbCpS_C.Text)
            .cp.Solid_Heat_Capacity_Const_D = CheckEmptyCell(tbCpS_D.Text)
            .cp.Solid_Heat_Capacity_Const_E = CheckEmptyCell(tbCpS_E.Text)
            .cp.Solid_Heat_Capacity_Tmin = 0
            .cp.Solid_Heat_Capacity_Tmax = .cp.TemperatureOfFusion

            .cp.Solid_Density_Const_A = CheckEmptyCell(tbRoS_A.Text)
            .cp.Solid_Density_Const_B = CheckEmptyCell(tbRoS_B.Text)
            .cp.Solid_Density_Const_C = CheckEmptyCell(tbRoS_C.Text)
            .cp.Solid_Density_Const_D = CheckEmptyCell(tbRoS_D.Text)
            .cp.Solid_Density_Const_E = CheckEmptyCell(tbRoS_E.Text)
            .cp.Solid_Density_Tmin = 0
            .cp.Solid_Density_Tmax = .cp.TemperatureOfFusion

            .cp.Vapor_Pressure_Constant_A = CheckEmptyCell(tbPVAP_A.Text)
            .cp.Vapor_Pressure_Constant_B = CheckEmptyCell(tbPVAP_B.Text)
            .cp.Vapor_Pressure_Constant_C = CheckEmptyCell(tbPVAP_C.Text)
            .cp.Vapor_Pressure_Constant_D = CheckEmptyCell(tbPVAP_D.Text)
            .cp.Vapor_Pressure_Constant_E = CheckEmptyCell(tbPVAP_E.Text)

            .cp.Ideal_Gas_Heat_Capacity_Const_A = CheckEmptyCell(tbCPIG_A.Text)
            .cp.Ideal_Gas_Heat_Capacity_Const_B = CheckEmptyCell(tbCPIG_B.Text)
            .cp.Ideal_Gas_Heat_Capacity_Const_C = CheckEmptyCell(tbCPIG_C.Text)
            .cp.Ideal_Gas_Heat_Capacity_Const_D = CheckEmptyCell(tbCPIG_D.Text)
            .cp.Ideal_Gas_Heat_Capacity_Const_E = CheckEmptyCell(tbCPIG_E.Text)

            .cp.Liquid_Heat_Capacity_Const_A = CheckEmptyCell(tbCPLiquid_A.Text)
            .cp.Liquid_Heat_Capacity_Const_B = CheckEmptyCell(tbCPLiquid_B.Text)
            .cp.Liquid_Heat_Capacity_Const_C = CheckEmptyCell(tbCPLiquid_C.Text)
            .cp.Liquid_Heat_Capacity_Const_D = CheckEmptyCell(tbCPLiquid_D.Text)
            .cp.Liquid_Heat_Capacity_Const_E = CheckEmptyCell(tbCPLiquid_E.Text)

            .cp.Liquid_Density_Const_A = CheckEmptyCell(tbLIQDENS_A.Text)
            .cp.Liquid_Density_Const_B = CheckEmptyCell(tbLIQDENS_B.Text)
            .cp.Liquid_Density_Const_C = CheckEmptyCell(tbLIQDENS_C.Text)
            .cp.Liquid_Density_Const_D = CheckEmptyCell(tbLIQDENS_D.Text)
            .cp.Liquid_Density_Const_E = CheckEmptyCell(tbLIQDENS_E.Text)

            .cp.Liquid_Viscosity_Const_A = CheckEmptyCell(tbLIQVISC_A.Text)
            .cp.Liquid_Viscosity_Const_B = CheckEmptyCell(tbLIQVISC_B.Text)
            .cp.Liquid_Viscosity_Const_C = CheckEmptyCell(tbLIQVISC_C.Text)
            .cp.Liquid_Viscosity_Const_D = CheckEmptyCell(tbLIQVISC_D.Text)
            .cp.Liquid_Viscosity_Const_E = CheckEmptyCell(tbLIQVISC_E.Text)

            .cp.Liquid_Thermal_Conductivity_Const_A = CheckEmptyCell(tbTCLiquid_A.Text)
            .cp.Liquid_Thermal_Conductivity_Const_B = CheckEmptyCell(tbTCLiquid_B.Text)
            .cp.Liquid_Thermal_Conductivity_Const_C = CheckEmptyCell(tbTCLiquid_C.Text)
            .cp.Liquid_Thermal_Conductivity_Const_D = CheckEmptyCell(tbTCLiquid_D.Text)
            .cp.Liquid_Thermal_Conductivity_Const_E = CheckEmptyCell(tbTCLiquid_E.Text)

            .cp.UNIFACGroups.Clear()
            For Each r As DataGridViewRow In Me.GridUNIFAC.Rows
                If CInt(r.Cells(2).Value) <> 0 Then .cp.UNIFACGroups(r.Cells(3).Tag(2)) = r.Cells(2).Value
            Next

            .cp.MODFACGroups.Clear()
            For Each r As DataGridViewRow In Me.GridMODFAC.Rows
                If CInt(r.Cells(2).Value) <> 0 Then .cp.MODFACGroups(r.Cells(3).Tag(2)) = r.Cells(2).Value
            Next

            .cp.NISTMODFACGroups.Clear()
            For Each r As DataGridViewRow In Me.GridNISTMODFAC.Rows
                If CInt(r.Cells(2).Value) <> 0 Then .cp.NISTMODFACGroups(r.Cells(3).Tag(2)) = r.Cells(2).Value
            Next

            Dim JC As Integer
            If mycase.JobackGroups Is Nothing Then .JobackGroups = New ArrayList 'for old file versions where field was not defined
            .JobackGroups.Clear()
            For Each r As DataGridViewRow In Me.GridJoback.Rows
                JC = r.Cells(3).Value
                If JC > 0 Then .JobackGroups.Add(New Integer() {r.Index, JC})
            Next

            If mycase.AdditionalAtoms Is Nothing Then .AdditionalAtoms = New ArrayList
            .AdditionalAtoms.Clear()
            For Each r As DataGridViewRow In AddAtomDataGrid.Rows
                JC = r.Cells(1).Value
                If JC > 0 Then .AdditionalAtoms.Add(New Integer() {r.Index, JC})
            Next

            .cp.Elements.Clear()
            For Each r As DataGridViewRow In Me.AtomDataGrid.Rows
                .cp.Elements.Add(r.Cells(0).Value, r.Cells(1).Value)
            Next


            mycase.DataPVAP.Clear()
            For Each row As DataGridViewRow In Me.GridExpDataPVAP.Rows
                If row.Index < Me.GridExpDataPVAP.Rows.Count - 1 Then mycase.DataPVAP.Add(New Double() {SystemsOfUnits.Converter.ConvertToSI(su.temperature, row.Cells(0).Value), SystemsOfUnits.Converter.ConvertToSI(su.pressure, row.Cells(1).Value)})
            Next

            mycase.DataCPIG.Clear()
            For Each row As DataGridViewRow In Me.GridExpDataCPIG.Rows
                If row.Index < Me.GridExpDataCPIG.Rows.Count - 1 Then mycase.DataCPIG.Add(New Double() {SystemsOfUnits.Converter.ConvertToSI(su.temperature, row.Cells(0).Value), SystemsOfUnits.Converter.ConvertToSI(su.heatCapacityCp, row.Cells(1).Value)})
            Next

            mycase.DataCPLiquid.Clear()
            For Each row As DataGridViewRow In Me.GridExpDataCPLiquid.Rows
                If row.Index < Me.GridExpDataCPLiquid.Rows.Count - 1 Then mycase.DataCPLiquid.Add(New Double() {SystemsOfUnits.Converter.ConvertToSI(su.temperature, row.Cells(0).Value), SystemsOfUnits.Converter.ConvertToSI(su.heatCapacityCp, row.Cells(1).Value)})
            Next

            mycase.DataLDENS.Clear()
            For Each row As DataGridViewRow In Me.GridExpDataLIQDENS.Rows
                If row.Index < Me.GridExpDataLIQDENS.Rows.Count - 1 Then mycase.DataLDENS.Add(New Double() {SystemsOfUnits.Converter.ConvertToSI(su.temperature, row.Cells(0).Value), SystemsOfUnits.Converter.ConvertToSI(su.density, row.Cells(1).Value)})
            Next

            mycase.DataLVISC.Clear()
            For Each row As DataGridViewRow In Me.GridExpDataLIQVISC.Rows
                If row.Index < Me.GridExpDataLIQVISC.Rows.Count - 1 Then mycase.DataLVISC.Add(New Double() {SystemsOfUnits.Converter.ConvertToSI(su.temperature, row.Cells(0).Value), SystemsOfUnits.Converter.ConvertToSI(su.viscosity, row.Cells(1).Value)})
            Next

            mycase.DataRoS.Clear()
            For Each row As DataGridViewRow In Me.GridExpDataRoS.Rows
                If row.Index < Me.GridExpDataRoS.Rows.Count - 1 Then mycase.DataRoS.Add(New Double() {SystemsOfUnits.Converter.ConvertToSI(su.temperature, row.Cells(0).Value), SystemsOfUnits.Converter.ConvertToSI(su.density, row.Cells(1).Value) / .cp.Molar_Weight})
            Next

            mycase.DataCpS.Clear()
            For Each row As DataGridViewRow In Me.GridExpDataCpS.Rows
                If row.Index < Me.GridExpDataCpS.Rows.Count - 1 Then mycase.DataCpS.Add(New Double() {SystemsOfUnits.Converter.ConvertToSI(su.temperature, row.Cells(0).Value), SystemsOfUnits.Converter.ConvertToSI(su.heatCapacityCp, row.Cells(1).Value) * .cp.Molar_Weight})
            Next

            mycase.DataLTC.Clear()
            For Each row As DataGridViewRow In Me.GridExpDataTCLiquid.Rows
                If row.Index < Me.GridExpDataTCLiquid.Rows.Count - 1 Then mycase.DataLTC.Add(New Double() {SystemsOfUnits.Converter.ConvertToSI(su.temperature, row.Cells(0).Value), SystemsOfUnits.Converter.ConvertToSI(su.thermalConductivity, row.Cells(1).Value)})
            Next

        End With

    End Sub

    Function CheckEmptyCell(ByVal val As String) As Double
        Try
            CheckEmptyCell = val
        Catch ex As Exception
            CheckEmptyCell = Nothing
        End Try

    End Function

    Function CheckEmptyTextBox(tb As TextBox) As Double
        If Double.TryParse(tb.Text, New Double) Then Return tb.Text Else Return 0.0#
    End Function

    Function CheckValidDF(ByVal val As String) As Boolean
        Dim DN As Double
        Try
            DN = val
            CheckValidDF = True
        Catch ex As Exception
            CheckValidDF = False
        End Try
    End Function

    Private Sub CalcJobackParams()

        If loaded Then

            loaded = False 'prevent recalculation due to edit field event procedures

            jb = New Joback
            methods = New HYP()

            'get UNIFAC group amounts
            Dim vn As New ArrayList
            For Each row As DataGridViewRow In GridUNIFAC.Rows
                If Not row.Cells(2).Value Is Nothing Then
                    vn.Add(Integer.Parse(row.Cells(2).Value))
                Else
                    vn.Add(0)
                End If
            Next
            Dim vnd As Int32() = vn.ToArray(Type.GetType("System.Int32"))

            'get Joback group amounts
            Dim JC, UC, GC As Integer 'JC=UnifacGroupCount; UC=UnifacGroupCount; GC=GlobalGroupCount
            Dim JG As New ArrayList


            PureUNIFACCompound = True
            For Each r As DataGridViewRow In Me.GridJoback.Rows
                JC = r.Cells(3).Value 'additional Joback groups
                UC = r.Cells(2).Value 'Joback groups from UNIFAC subgoups
                JG.Add(JC + UC)
                GC += JC + UC
                If JC > 0 Then PureUNIFACCompound = False
            Next
            Dim JGD As Int32() = JG.ToArray(Type.GetType("System.Int32"))

            'Calculate atoms list
            Dim ACL As New Dictionary(Of String, Integer) ' => Atom Count List
            Dim Sy As String 'Element Symbol
            Dim AC As Integer 'Atom count
            Dim Formula As String
            Dim SpecialDefinition As Boolean = False 'special definition -> no joback calculation possible

            ACL = jb.GetAtomCountList(JGD) 'Atoms from Joback groups

            'add additional atoms from datatable
            For Each r As DataGridViewRow In Me.AddAtomDataGrid.Rows
                Sy = r.Cells(0).Value
                AC = r.Cells(1).Value
                If AC > 0 Then
                    PureUNIFACCompound = False
                    SpecialDefinition = True
                    If Not ACL.ContainsKey(Sy) Then
                        ACL.Add(Sy, AC)
                    Else
                        ACL.Item(Sy) = ACL.Item(Sy) + AC
                    End If
                End If
            Next

            'add atoms from special UNIFAC groups to list where no Joback subgroup is defined -> e.g. silanes
            Dim i, n, k, AtomCount As Integer
            Dim s, AtomTypeCount, AtomName As String
            For i = 2 To UNIFAClines.Count - 1
                s = UNIFAClines(i).Split(",")(9)
                If Not s = "" And vnd(i - 2) > 0 Then
                    SpecialDefinition = True
                    n = 0
                    For k = 0 To s.Length - 1 'count atom types in group
                        If s.Chars(k) = "/" Then n += 1
                    Next
                    For k = 0 To n 'insert atoms in list
                        AtomTypeCount = s.Split("/")(k) 'get type and count of Atom
                        AtomName = AtomTypeCount.Split(":")(0)
                        AtomCount = AtomTypeCount.Split(":")(1) * vnd(i - 2)

                        If Not ACL.ContainsKey(AtomName) Then
                            ACL.Add(AtomName, AtomCount)
                        Else
                            ACL.Item(AtomName) = ACL.Item(AtomName) + AtomCount
                        End If
                    Next
                End If
            Next

            'Calculation of chemical formula
            Me.AtomDataGrid.Rows.Clear()
            Formula = ""

            For Each A As String In ACL.Keys
                Formula = Formula & A
                AC = ACL.Item(A)
                If AC > 1 Then Formula = Formula & AC
                Me.AtomDataGrid.Rows.Add(New Object() {A, AC})
            Next

            Me.TextBoxFormula.Text = Formula

            Dim Tb, Tc, Pc, Vc, MM, w, Hvb, ZRa, Tf As Double

            If ACL.Count > 0 Then
                MM = jb.CalcMW(ACL)
                If CheckBoxMW.Checked Then Me.TextBoxMW.Text = Format(MM, "N")
                'MM = Me.TextBoxMW.Text
            Else
                If CheckBoxMW.Checked Then Me.TextBoxMW.Text = ""
            End If

            If mycase.IgnoreUnsupportedGroups Then SpecialDefinition = False

            If GC > 0 And Not SpecialDefinition Then

                'boiling point
                Tb = jb.CalcTb(JGD)
                If CheckBoxNBP.Checked Then Me.TextBoxNBP.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Tb), "N")
                If CheckValidDF(Me.TextBoxNBP.Text) Then
                    Tb = SystemsOfUnits.Converter.ConvertToSI(su.temperature, Me.TextBoxNBP.Text)
                Else : Tb = -1
                End If

                'critical temperature
                If Tb > 0 Then
                    Tc = jb.CalcTc(Tb, JGD)
                    If CheckBoxTc.Checked Then Me.TextBoxTc.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Tc), "N")
                    If CheckValidDF(Me.TextBoxTc.Text) Then
                        Tc = SystemsOfUnits.Converter.ConvertToSI(su.temperature, Me.TextBoxTc.Text)
                    Else : Tc = -1
                    End If
                Else
                    Tc = -1
                    If CheckBoxTc.Checked Then Me.TextBoxTc.Text = ""
                End If

                'critical pressure
                Pc = jb.CalcPc(JGD)
                If CheckBoxPc.Checked Then Me.TextBoxPc.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Pc), "N")
                If CheckValidDF(Me.TextBoxPc.Text) Then
                    Pc = SystemsOfUnits.Converter.ConvertToSI(su.pressure, Me.TextBoxPc.Text)
                Else : Pc = -1
                End If

                'critical compressibility
                If Tc > 0 And Pc > 0 Then
                    Vc = jb.CalcVc(JGD)
                    If CheckBoxZc.Checked Then Me.TextBoxZc.Text = Format(Pc * Vc / Tc / 8.314 / 1000, "N")
                    If CheckBoxZRa.Checked Then Me.TextBoxZRa.Text = Format(Pc * Vc / Tc / 8.314 / 1000, "N")
                    ZRa = Me.TextBoxZRa.Text
                Else
                    If CheckBoxZc.Checked Then Me.TextBoxZc.Text = ""
                    If CheckBoxZRa.Checked Then Me.TextBoxZRa.Text = ""
                End If

                'acentric factor
                If Tb > 0 And Tc > 0 And Pc > 0 Then
                    w = (-Math.Log(Pc / 100000) - 5.92714 + 6.09648 / (Tb / Tc) + 1.28862 * Math.Log(Tb / Tc) - 0.169347 * (Tb / Tc) ^ 6) / (15.2518 - 15.6875 / (Tb / Tc) - 13.4721 * Math.Log(Tb / Tc) + 0.43577 * (Tb / Tc) ^ 6)
                    If CheckBoxAF.Checked Then Me.TextBoxAF.Text = Format(w, "N")
                    If CheckValidDF(Me.TextBoxAF.Text) Then
                        w = Me.TextBoxAF.Text
                    Else
                        w = -1
                    End If
                Else
                    w = -1
                    If CheckBoxAF.Checked Then Me.TextBoxAF.Text = ""
                End If


                If CheckBoxDHF.Checked Then Me.TextBoxDHF.Text = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, jb.CalcDHf(JGD) / MM).ToString("N")
                If CheckBoxDGF.Checked Then Me.TextBoxDGF.Text = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, jb.CalcDGf(JGD) / MM).ToString("N")
                If CheckBoxCSAF.Checked Then
                    If w > 0 Then
                        Me.TextBoxCSAF.Text = w
                    Else
                        Me.TextBoxCSAF.Text = ""
                    End If
                End If

                If Tc > 0 And Pc > 0 And Tb > 0 And MM > 0 And w > 0 Then
                    Hvb = methods.DHvb_Vetere(Tc, Pc, Tb) / MM
                    If CheckBoxCSSP.Checked Then Me.TextBoxCSSP.Text = Format(((Hvb * MM - 8.314 * Tb) * 238.846 * PropertyPackages.Auxiliary.PROPS.liq_dens_rackett(Tb, Tc, Pc, w, MM) / MM / 1000000.0) ^ 0.5, "N")
                    If CheckBoxCSLV.Checked Then Me.TextBoxCSLV.Text = Format(1 / PropertyPackages.Auxiliary.PROPS.liq_dens_rackett(Tb, Tc, Pc, w, MM) * MM / 1000 * 1000000.0, "N")
                Else
                    If CheckBoxCSSP.Checked Then Me.TextBoxCSSP.Text = ""
                    If CheckBoxCSLV.Checked Then Me.TextBoxCSLV.Text = ""
                End If


                If CheckBoxMeltingTemp.Checked Then Me.TextBoxMeltingTemp.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, jb.CalcTf(JGD)), "N") 'melting temperature - temperature of fusion
                If CheckBoxEnthOfFusion.Checked Then
                    Me.TextBoxEnthOfFusion.Text = Format(jb.CalcHf(JGD), "N") 'enthalpy of fusion - KJ/mol
                    Me.TextBoxEnthOfFusion2.Text = (jb.CalcHf(JGD) * 1000 / MM).ToString("N") 'enthalpy of fusion - KJ/mol
                End If
                If CheckValidDF(Me.TextBoxPc.Text) Then
                    Tf = SystemsOfUnits.Converter.ConvertToSI(su.temperature, Me.TextBoxMeltingTemp.Text)
                Else : Tf = -1
                End If

                If rbEstimateCPIG.Checked Then
                    Dim result As Object = RegressData(1, True)

                    For Each it As Object In cbEqCPIG.Items
                        If it.ToString.Split(":")(0) = 4 Then
                            cbEqCPIG.SelectedIndex = cbEqCPIG.Items.IndexOf(it)
                            Exit For
                        End If
                    Next

                    tbCPIG_A.Text = result(0)(0) * 1000
                    tbCPIG_B.Text = result(0)(1) * 1000
                    tbCPIG_C.Text = result(0)(2) * 1000
                    tbCPIG_D.Text = result(0)(3) * 1000
                    tbCPIG_E.Text = "0"
                End If

                'estimate solid density - DWSIM-Method
                If rbEstimateSolidDens.Checked Then
                    Dim RoSMP, RoLMP As Double 'solid+liquid density at melting point
                    RoLMP = PropertyPackages.Auxiliary.PROPS.liq_dens_rackett(Tf, Tc, Pc, w, MM, ZRa, 101325, PropertyPackages.Auxiliary.PROPS.Pvp_leekesler(Tf, Tc, Pc, w))
                    RoSMP = RoLMP * 1.0933 + 0.000037886 * RoLMP ^ 2
                    tbRoS_A.Text = RoSMP / MM + 0.005 * Tf
                    tbRoS_B.Text = -0.005
                    tbRoS_C.Text = 0
                    tbRoS_D.Text = 0
                    tbRoS_E.Text = 0
                    For Each it As Object In cbEqSolidDENS.Items
                        If it.ToString.Split(":")(0) = 2 Then
                            cbEqSolidDENS.SelectedIndex = cbEqSolidDENS.Items.IndexOf(it)
                            Exit For
                        End If
                    Next
                End If
            Else
                If CheckBoxNBP.Checked Then Me.TextBoxNBP.Text = ""
                If CheckBoxTc.Checked Then Me.TextBoxTc.Text = ""
                If CheckBoxPc.Checked Then Me.TextBoxPc.Text = ""
                If CheckBoxZc.Checked Then Me.TextBoxZc.Text = ""
                If CheckBoxZRa.Checked Then Me.TextBoxZRa.Text = ""
                If CheckBoxAF.Checked Then Me.TextBoxAF.Text = ""
                If CheckBoxDHF.Checked Then Me.TextBoxDHF.Text = ""
                If CheckBoxDGF.Checked Then Me.TextBoxDGF.Text = ""
                If CheckBoxCSAF.Checked Then Me.TextBoxCSAF.Text = ""
                If CheckBoxCSSP.Checked Then Me.TextBoxCSSP.Text = ""
                If CheckBoxCSLV.Checked Then Me.TextBoxCSLV.Text = ""
                If CheckBoxMeltingTemp.Checked Then Me.TextBoxMeltingTemp.Text = ""
                If CheckBoxEnthOfFusion.Checked Then Me.TextBoxEnthOfFusion.Text = ""
                If rbEstimateCPIG.Checked Then
                    tbCPIG_A.Text = ""
                    tbCPIG_B.Text = ""
                    tbCPIG_C.Text = ""
                    tbCPIG_D.Text = ""
                    tbCPIG_E.Text = ""
                End If
            End If

            loaded = True 'reset old status
        End If
    End Sub

    Private Sub GridUNIFAC_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles GridUNIFAC.CellValueChanged
        If loaded Then
            'get group amounts
            If Not populating Then
                mycase.cp.UNIFACGroups.Clear()
            End If
            For Each r As DataGridViewRow In Me.GridUNIFAC.Rows
                If Not r.Cells(2).Value Is Nothing Then
                    If CInt(r.Cells(2).Value) <> 0 Then
                        If Not populating Then
                            mycase.cp.UNIFACGroups.Add(r.Cells(3).Tag(2), r.Cells(2).Value)
                            r.Cells(2).Style.BackColor = Color.PaleGreen
                        End If
                    End If
                Else
                    r.Cells(2).Style.BackColor = Color.White
                End If
            Next
            loaded = False
            FillUnifacSubGroups()
            loaded = True
            CalcJobackParams()
            CheckDataStatus()
        End If
    End Sub
    Private Sub GridMODFAC_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles GridMODFAC.CellValueChanged
        If loaded Then
            Dim c As DataGridViewCell = Me.GridMODFAC.Rows(e.RowIndex).Cells(e.ColumnIndex)
            If c.Value > 0 Then
                c.Style.BackColor = Color.PaleGreen
            Else
                c.Style.BackColor = Color.White
            End If

        End If
    End Sub
    Private Sub GridNISTMODFAC_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles GridNISTMODFAC.CellValueChanged
        If loaded Then
            Dim c As DataGridViewCell = Me.GridNISTMODFAC.Rows(e.RowIndex).Cells(e.ColumnIndex)
            If c.Value > 0 Then
                c.Style.BackColor = Color.PaleGreen
            Else
                c.Style.BackColor = Color.White
            End If

        End If
    End Sub
    Private Sub Grid_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles GridJoback.CellValueChanged, AddAtomDataGrid.CellValueChanged
        If loaded Then
            CalcJobackParams()
        End If
    End Sub

    Sub UpdateUnits()

        With su

            lblTc.Text = .temperature
            lblPc.Text = .pressure
            lblNBP.Text = .temperature
            lblMW.Text = .molecularWeight
            lblDHF.Text = .enthalpy
            lblDGF.Text = .enthalpy
            lblMeltingTemp.Text = .temperature
            lblEnthOfFusion.Text = .enthalpy

            Me.GridExpDataPVAP.Columns(0).HeaderText = "T [" & su.temperature & "]"
            Me.GridExpDataCPIG.Columns(0).HeaderText = "T [" & su.temperature & "]"
            Me.GridExpDataCPLiquid.Columns(0).HeaderText = "T [" & su.temperature & "]"
            Me.GridExpDataLIQDENS.Columns(0).HeaderText = "T [" & su.temperature & "]"
            Me.GridExpDataLIQVISC.Columns(0).HeaderText = "T [" & su.temperature & "]"
            Me.GridExpDataCpS.Columns(0).HeaderText = "T [" & su.temperature & "]"
            Me.GridExpDataRoS.Columns(0).HeaderText = "T [" & su.temperature & "]"
            Me.GridExpDataTCLiquid.Columns(0).HeaderText = "T [" & su.temperature & "]"

            Me.GridExpDataPVAP.Columns(1).HeaderText = "Pvap [" & su.pressure & "]"
            Me.GridExpDataCPIG.Columns(1).HeaderText = "Cpig [" & su.heatCapacityCp & "]"
            Me.GridExpDataCPLiquid.Columns(1).HeaderText = "Cp Liquid [" & su.heatCapacityCp & "]"
            Me.GridExpDataLIQDENS.Columns(1).HeaderText = "Dens [" & su.density & "]"
            Me.GridExpDataLIQVISC.Columns(1).HeaderText = "Visc [" & su.viscosity & "]"
            Me.GridExpDataCpS.Columns(1).HeaderText = "CpS [" & su.heatCapacityCp & "]"
            Me.GridExpDataRoS.Columns(1).HeaderText = "DensS [" & su.density & "]"
            Me.GridExpDataTCLiquid.Columns(1).HeaderText = "LiqTC [" & su.thermalConductivity & "]"

        End With
    End Sub

    Public Function RegressData(ByVal tipo As Integer, ByVal calcular As Boolean)

        Dim obj As Object = Nothing
        Dim lmfit As New Utilities.PetroleumCharacterization.LMFit

        jb = New Joback

        m_props = New PROPS()

        Dim c_pv(4), c_cp(4), c_cpl(4), c_vi(4), c_de(4), c_sd(4), c_scp(4) As Double
        Dim r_cp, r_cpl, r_vi, r_pv, r_de, r_sd, r_scp, n_cp, n_cpl, n_pv, n_de, n_vi, n_sd, n_scp As Double

        c_pv(0) = 25
        c_pv(1) = 2000
        c_pv(2) = -5.245
        c_pv(3) = 0.0#
        c_pv(4) = 0.0#

        c_cp(0) = 33.7
        c_cp(1) = 0.249
        c_cp(2) = 0.000253
        c_cp(3) = -0.000000384
        c_cp(4) = 0.000000000129

        c_cpl(0) = 3
        c_cpl(1) = 0.01
        c_cpl(2) = 0.0001
        c_cpl(3) = 0
        c_cpl(4) = 0

        c_vi(0) = -17.255
        c_vi(1) = 1576
        c_vi(2) = 0.86191
        c_vi(3) = 0
        c_vi(4) = 0

        c_de(3) = 1.0#
        c_de(2) = 647.3
        c_de(1) = 0.14056
        c_de(0) = 1.0#

        c_sd(0) = 11
        c_sd(1) = -0.005
        c_sd(2) = 0.0#
        c_sd(3) = 0.0#
        c_sd(4) = 0.0#

        c_scp(0) = 0
        c_scp(1) = 0.1
        c_scp(2) = 0
        c_scp(3) = 0
        c_scp(4) = 0

        Select Case tipo
            Case 0
                'regressão dos dados
                obj = lmfit.GetCoeffs(CopyToVector(mycase.DataPVAP, 0), CopyToVector(mycase.DataPVAP, 1), c_pv.Clone, Utilities.PetroleumCharacterization.LMFit.FitType.Pvap, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                c_pv = obj(0)
                r_pv = obj(2)
                n_pv = obj(3)
            Case 1
                If calcular Then
                    'get Joback group amounts
                    Dim JC, UC As Integer
                    Dim JG As New ArrayList

                    For Each r As DataGridViewRow In Me.GridJoback.Rows
                        JC = r.Cells(3).Value 'additional Joback groups
                        UC = r.Cells(2).Value 'Joback groups from UNIFAC subgoups
                        JG.Add(JC + UC)
                    Next
                    Dim JGD As Int32() = JG.ToArray(Type.GetType("System.Int32"))

                    c_cp(0) = Me.jb.CalcCpA(JGD)
                    c_cp(1) = Me.jb.CalcCpB(JGD)
                    c_cp(2) = Me.jb.CalcCpC(JGD)
                    c_cp(3) = Me.jb.CalcCpD(JGD)
                    obj = New Integer() {0, 0, 0, 10}
                Else
                    'regressão dos dados
                    obj = lmfit.GetCoeffs(CopyToVector(mycase.DataCPIG, 0), CopyToVector(mycase.DataCPIG, 1), c_cp, Utilities.PetroleumCharacterization.LMFit.FitType.Cp, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                    c_cp = obj(0)
                    r_cp = obj(2)
                    n_cp = obj(3)
                End If

            Case 2
                'regressão dos dados
                obj = lmfit.GetCoeffs(CopyToVector(mycase.DataLVISC, 0), CopyToVector(mycase.DataLVISC, 1), c_vi, Utilities.PetroleumCharacterization.LMFit.FitType.LiqVisc, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                c_vi = obj(0)
                r_vi = obj(2)
                n_vi = obj(3)

            Case 3

                Dim x1, x2, y1, y2, rhoc, al, bl As Double

                rhoc = mycase.cp.Molar_Weight / mycase.cp.Critical_Compressibility * 8.314 * mycase.cp.Critical_Temperature / mycase.cp.Critical_Pressure * 1000

                If Not Double.IsNaN(rhoc) Then

                    x1 = Log(1 - mycase.DataLDENS(0)(0) / mycase.cp.Critical_Temperature)
                    x2 = Log(1 - mycase.DataLDENS(1)(0) / mycase.cp.Critical_Temperature)
                    y1 = Log(Log(mycase.DataLDENS(0)(1) / rhoc))
                    y2 = Log(Log(mycase.DataLDENS(1)(1) / rhoc))

                    al = (y2 - y1) / (x2 - x1)
                    bl = y1 - al * x1

                    c_de(3) = al
                    c_de(2) = mycase.cp.Critical_Temperature
                    c_de(1) = 1 / Exp(Exp(bl))
                    c_de(0) = c_de(1) * rhoc

                End If

                'regressão dos dados
                obj = lmfit.GetCoeffs(CopyToVector(mycase.DataLDENS, 0), CopyToVector(mycase.DataLDENS, 1), c_de, Utilities.PetroleumCharacterization.LMFit.FitType.LiqDens, 0.00001, 0.00001, 0.00001, 10000)
                c_de = obj(0)
                r_de = obj(2)
                n_de = obj(3)

            Case 4
                'regressão dos dados - solid density
                obj = lmfit.GetCoeffs(CopyToVector(mycase.DataRoS, 0), CopyToVector(mycase.DataRoS, 1), c_sd, Utilities.PetroleumCharacterization.LMFit.FitType.Cp, 0.0000000000001, 0.0000000000001, 0.0000000000001, 10000)
                c_sd = obj(0)
                r_sd = obj(2)
                n_sd = obj(3)

            Case 5
                'regressão dos dados - solid heat capacity
                obj = lmfit.GetCoeffs(CopyToVector(mycase.DataCpS, 0), CopyToVector(mycase.DataCpS, 1), c_scp, Utilities.PetroleumCharacterization.LMFit.FitType.Cp, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                c_scp = obj(0)
                r_scp = obj(2)
                n_scp = obj(3)
            Case 6
                'regressão dos dados - liquid heat capacity
                obj = lmfit.GetCoeffs(CopyToVector(mycase.DataCPLiquid, 0), CopyToVector(mycase.DataCPLiquid, 1), c_cpl, Utilities.PetroleumCharacterization.LMFit.FitType.Cp, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                c_cpl = obj(0)
                r_cpl = obj(2)
                n_cpl = obj(3)

            Case 7
                'regressão dos dados - liquid thermal conductivity
                obj = lmfit.GetCoeffs(CopyToVector(mycase.DataLTC, 0), CopyToVector(mycase.DataLTC, 1), c_cpl, Utilities.PetroleumCharacterization.LMFit.FitType.Cp, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                c_cpl = obj(0)
                r_cpl = obj(2)
                n_cpl = obj(3)

        End Select

        Select Case tipo
            Case 0
                Return New Object() {c_pv, r_pv, n_pv, obj(1)}
            Case 1
                Return New Object() {c_cp, r_cp, n_cp, obj(1)}
            Case 2
                Return New Object() {c_vi, r_vi, n_vi, obj(1)}
            Case 3
                Return New Object() {c_de, r_de, n_de, obj(1)}
            Case 4
                Return New Object() {c_sd, r_sd, n_sd, obj(1)}
            Case 5
                Return New Object() {c_scp, r_scp, n_scp, obj(1)}
            Case 6
                Return New Object() {c_cpl, r_cpl, n_cpl, obj(1)}
            Case 7
                Return New Object() {c_cpl, r_cpl, n_cpl, obj(1)}
            Case Else
                Return Nothing
        End Select

    End Function

    Sub StorePVAPData()
        mycase.DataPVAP.Clear()
        For Each row As DataGridViewRow In Me.GridExpDataPVAP.Rows
            If row.Index < Me.GridExpDataPVAP.Rows.Count - 1 Then
                Try
                    mycase.DataPVAP.Add(New Double() {SystemsOfUnits.Converter.ConvertToSI(su.temperature, row.Cells(0).Value), SystemsOfUnits.Converter.ConvertToSI(su.pressure, row.Cells(1).Value)})
                Catch ex As Exception

                End Try
            End If
        Next
    End Sub

    Private Sub btnRegressPVAP_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRegressPVAP.Click

        loaded = False

        StorePVAPData()

        Dim result As Object = RegressData(0, False)
        tbStatusPVAP.Text = GetInfo(result(3))

        With mycase.cp
            .VaporPressureEquation = 101

            For Each it As Object In cbEqPVAP.Items
                If it.ToString.Split(":")(0) = .VaporPressureEquation Then
                    cbEqPVAP.SelectedIndex = cbEqPVAP.Items.IndexOf(it)
                    Exit For
                End If
            Next

            .Vapor_Pressure_Constant_A = result(0)(0)
            .Vapor_Pressure_Constant_B = result(0)(1)
            .Vapor_Pressure_Constant_C = result(0)(2)
            .Vapor_Pressure_Constant_D = result(0)(3)
            .Vapor_Pressure_Constant_E = result(0)(4)

            tbPVAP_A.Text = .Vapor_Pressure_Constant_A
            tbPVAP_B.Text = .Vapor_Pressure_Constant_B
            tbPVAP_C.Text = .Vapor_Pressure_Constant_C
            tbPVAP_D.Text = .Vapor_Pressure_Constant_D
            tbPVAP_E.Text = .Vapor_Pressure_Constant_E

            CheckDataStatus()

        End With

        rbRegressPVAP.Checked = True
        loaded = True

    End Sub

    Sub StoreSolidCpData()
        Dim MW As Double = Me.TextBoxMW.Text
        Dim XL, YL As Double
        mycase.DataCpS.Clear()
        For Each row As DataGridViewRow In Me.GridExpDataCpS.Rows
            If row.Index < Me.GridExpDataCpS.Rows.Count - 1 Then
                XL = SystemsOfUnits.Converter.ConvertToSI(su.temperature, row.Cells(0).Value)
                YL = SystemsOfUnits.Converter.ConvertToSI(su.heatCapacityCp, row.Cells(1).Value) * MW * 1000
                mycase.DataCpS.Add(New Double() {XL, YL})
            End If
        Next
    End Sub

    Private Sub btnRegressSolidCp_Click(sender As System.Object, e As System.EventArgs) Handles btnRegressSolidCp.Click

        loaded = False

        StoreSolidCpData()

        Dim result As Object = RegressData(5, False)
        tbStatusSolidCp.Text = GetInfo(result(3))

        With mycase.cp
            .SolidHeatCapacityEquation = 5

            For Each it As Object In cbEqCpS.Items
                If it.ToString.Split(":")(0) = .SolidHeatCapacityEquation Then
                    cbEqCpS.SelectedIndex = cbEqCpS.Items.IndexOf(it)
                    Exit For
                End If
            Next

            .Solid_Heat_Capacity_Const_A = result(0)(0)
            .Solid_Heat_Capacity_Const_B = result(0)(1)
            .Solid_Heat_Capacity_Const_C = result(0)(2)
            .Solid_Heat_Capacity_Const_D = result(0)(3)
            .Solid_Heat_Capacity_Const_E = result(0)(4)

            tbCpS_A.Text = .Solid_Heat_Capacity_Const_A
            tbCpS_B.Text = .Solid_Heat_Capacity_Const_B
            tbCpS_C.Text = .Solid_Heat_Capacity_Const_C
            tbCpS_D.Text = .Solid_Heat_Capacity_Const_D
            tbCpS_E.Text = .Solid_Heat_Capacity_Const_E

        End With
        rbRegressSolidCp.Checked = True
        loaded = True
    End Sub

    Sub StoreSolidDensData()

        Dim MW As Double = Me.TextBoxMW.Text
        Dim XL, YL As Double
        mycase.DataRoS.Clear()
        For Each row As DataGridViewRow In Me.GridExpDataRoS.Rows
            If row.Index < Me.GridExpDataRoS.Rows.Count - 1 Then
                XL = SystemsOfUnits.Converter.ConvertToSI(su.temperature, row.Cells(0).Value)
                YL = SystemsOfUnits.Converter.ConvertToSI(su.density, row.Cells(1).Value) / MW
                mycase.DataRoS.Add(New Double() {XL, YL})
            End If
        Next

    End Sub

    Private Sub btnRegressSolidDens_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRegressSolidDens.Click

        loaded = False

        StoreSolidDensData()

        Dim result As Object = RegressData(4, False)
        tbStatusSolidDens.Text = GetInfo(result(3))

        With mycase.cp
            .SolidDensityEquation = 5

            For Each it As Object In cbEqSolidDENS.Items
                If it.ToString.Split(":")(0) = .SolidDensityEquation Then
                    cbEqSolidDENS.SelectedIndex = cbEqSolidDENS.Items.IndexOf(it)
                    Exit For
                End If
            Next

            .Solid_Density_Const_A = result(0)(0)
            .Solid_Density_Const_B = result(0)(1)
            .Solid_Density_Const_C = result(0)(2)
            .Solid_Density_Const_D = result(0)(3)
            .Solid_Density_Const_E = result(0)(4)

            tbRoS_A.Text = .Solid_Density_Const_A
            tbRoS_B.Text = .Solid_Density_Const_B
            tbRoS_C.Text = .Solid_Density_Const_C
            tbRoS_D.Text = .Solid_Density_Const_D
            tbRoS_E.Text = .Solid_Density_Const_E

        End With
        rbRegressSolidDens.Checked = True
        loaded = True
    End Sub

    Sub StoreCPIGData()

        mycase.DataCPIG.Clear()
        Dim MW As Double = Me.TextBoxMW.Text
        For Each row As DataGridViewRow In Me.GridExpDataCPIG.Rows
            Try
                If row.Index < Me.GridExpDataCPIG.Rows.Count - 1 Then mycase.DataCPIG.Add(New Double() {SystemsOfUnits.Converter.ConvertToSI(su.temperature, row.Cells(0).Value), SystemsOfUnits.Converter.ConvertToSI(su.heatCapacityCp, row.Cells(1).Value) * MW})
            Catch ex As Exception
            End Try
        Next

    End Sub


    Private Sub btnRegressCPIG_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRegressCPIG.Click

        loaded = False

        StoreCPIGData()


        Dim result As Object = RegressData(1, False)

        tbStatusCPIG.Text = GetInfo(result(3))

        With mycase.cp
            .IdealgasCpEquation = 5

            For Each it As Object In cbEqCPIG.Items
                If it.ToString.Split(":")(0) = .IdealgasCpEquation Then
                    cbEqCPIG.SelectedIndex = cbEqCPIG.Items.IndexOf(it)
                    Exit For
                End If
            Next

            .Ideal_Gas_Heat_Capacity_Const_A = result(0)(0) * 1000
            .Ideal_Gas_Heat_Capacity_Const_B = result(0)(1) * 1000
            .Ideal_Gas_Heat_Capacity_Const_C = result(0)(2) * 1000
            .Ideal_Gas_Heat_Capacity_Const_D = result(0)(3) * 1000
            .Ideal_Gas_Heat_Capacity_Const_E = result(0)(4) * 1000

            tbCPIG_A.Text = .Ideal_Gas_Heat_Capacity_Const_A
            tbCPIG_B.Text = .Ideal_Gas_Heat_Capacity_Const_B
            tbCPIG_C.Text = .Ideal_Gas_Heat_Capacity_Const_C
            tbCPIG_D.Text = .Ideal_Gas_Heat_Capacity_Const_D
            tbCPIG_E.Text = .Ideal_Gas_Heat_Capacity_Const_E

        End With
        rbRegressCPIG.Checked = True
        loaded = True
    End Sub

    Sub StoreLiqDensData()

        mycase.DataLDENS.Clear()
        For Each row As DataGridViewRow In Me.GridExpDataLIQDENS.Rows
            Try
                If row.Index < Me.GridExpDataLIQDENS.Rows.Count - 1 Then mycase.DataLDENS.Add(New Double() {SystemsOfUnits.Converter.ConvertToSI(su.temperature, row.Cells(0).Value), SystemsOfUnits.Converter.ConvertToSI(su.density, row.Cells(1).Value)})
            Catch ex As Exception
            End Try
        Next

    End Sub

    Private Sub btnRegressLIQDENS_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRegressLIQDENS.Click

        loaded = False

        StoreLiqDensData()

        Dim result As Object = RegressData(3, False)

        tbStatusLIQDENS.Text = GetInfo(result(3))

        With mycase.cp
            .LiquidDensityEquation = 105

            For Each it As Object In cbEqLIQDENS.Items
                If it.ToString.Split(":")(0) = .LiquidDensityEquation Then
                    cbEqLIQDENS.SelectedIndex = cbEqLIQDENS.Items.IndexOf(it)
                    Exit For
                End If
            Next

            .Liquid_Density_Const_A = result(0)(0)
            .Liquid_Density_Const_B = result(0)(1)
            .Liquid_Density_Const_C = result(0)(2)
            .Liquid_Density_Const_D = result(0)(3)
            .Liquid_Density_Const_E = result(0)(4)

            tbLIQDENS_A.Text = .Liquid_Density_Const_A
            tbLIQDENS_B.Text = .Liquid_Density_Const_B
            tbLIQDENS_C.Text = .Liquid_Density_Const_C
            tbLIQDENS_D.Text = .Liquid_Density_Const_D
            tbLIQDENS_E.Text = .Liquid_Density_Const_E

        End With
        rbRegressLIQDENS.Checked = True
        loaded = True
    End Sub

    Sub StoreLiqTCData()

        mycase.DataLTC.Clear()
        For Each row As DataGridViewRow In Me.GridExpDataTCLiquid.Rows
            Try
                If row.Index < Me.GridExpDataTCLiquid.Rows.Count - 1 Then mycase.DataLTC.Add(New Double() {SystemsOfUnits.Converter.ConvertToSI(su.temperature, row.Cells(0).Value), SystemsOfUnits.Converter.ConvertToSI(su.thermalConductivity, row.Cells(1).Value)})
            Catch ex As Exception
            End Try
        Next

    End Sub

    Private Sub btnRegressTCLiquid_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRegressTCLiquid.Click

        loaded = False

        StoreLiqTCData()

        Dim result As Object = RegressData(7, False)

        tbStatusTCLiquid.Text = GetInfo(result(3))

        With mycase.cp

            .LiquidThermalConductivityEquation = 5

            For Each it As Object In cbEqTCLiquid.Items
                If it.ToString.Split(":")(0) = .LiquidThermalConductivityEquation Then
                    cbEqTCLiquid.SelectedIndex = cbEqTCLiquid.Items.IndexOf(it)
                    Exit For
                End If
            Next

            .Liquid_Thermal_Conductivity_Const_A = result(0)(0)
            .Liquid_Thermal_Conductivity_Const_B = result(0)(1)
            .Liquid_Thermal_Conductivity_Const_C = result(0)(2)
            .Liquid_Thermal_Conductivity_Const_D = result(0)(3)
            .Liquid_Thermal_Conductivity_Const_E = result(0)(4)

            tbTCLiquid_A.Text = .Liquid_Thermal_Conductivity_Const_A
            tbTCLiquid_B.Text = .Liquid_Thermal_Conductivity_Const_B
            tbTCLiquid_C.Text = .Liquid_Thermal_Conductivity_Const_C
            tbTCLiquid_D.Text = .Liquid_Thermal_Conductivity_Const_D
            tbTCLiquid_E.Text = .Liquid_Thermal_Conductivity_Const_E

        End With
        rbRegressTCLiquid.Checked = True
        loaded = True

    End Sub

    Sub StoreLIQVData()

        mycase.DataLVISC.Clear()
        For Each row As DataGridViewRow In Me.GridExpDataLIQVISC.Rows
            If row.Index < Me.GridExpDataLIQVISC.Rows.Count - 1 Then
                Try
                    mycase.DataLVISC.Add(New Double() {SystemsOfUnits.Converter.ConvertToSI(su.temperature, row.Cells(0).Value), SystemsOfUnits.Converter.ConvertToSI(su.viscosity, row.Cells(1).Value)})
                Catch ex As Exception
                End Try
            End If
        Next

    End Sub

    Private Sub btnRegressLIQVISC_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRegressLIQVISC.Click

        loaded = False

        StoreLIQVData()

        Dim result As Object = RegressData(2, False)

        tbStatusLIQVISC.Text = GetInfo(result(3))

        With mycase.cp

            .LiquidViscosityEquation = 101

            For Each it As Object In cbEqLIQVISC.Items
                If it.ToString.Split(":")(0) = .LiquidViscosityEquation Then
                    cbEqLIQVISC.SelectedIndex = cbEqLIQVISC.Items.IndexOf(it)
                    Exit For
                End If
            Next

            .Liquid_Viscosity_Const_A = result(0)(0)
            .Liquid_Viscosity_Const_B = result(0)(1)
            .Liquid_Viscosity_Const_C = result(0)(2)
            .Liquid_Viscosity_Const_D = result(0)(3)
            .Liquid_Viscosity_Const_E = result(0)(4)

            tbLIQVISC_A.Text = .Liquid_Viscosity_Const_A
            tbLIQVISC_B.Text = .Liquid_Viscosity_Const_B
            tbLIQVISC_C.Text = .Liquid_Viscosity_Const_C
            tbLIQVISC_D.Text = .Liquid_Viscosity_Const_D
            tbLIQVISC_E.Text = .Liquid_Viscosity_Const_E

        End With
        rbRegressLIQVISC.Checked = True
        loaded = True
    End Sub


    Private Sub GridExpData_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles GridExpDataRoS.KeyDown, GridExpDataCpS.KeyDown, GridExpDataPVAP.KeyDown, GridExpDataLIQVISC.KeyDown, GridExpDataLIQDENS.KeyDown, GridExpDataCPLiquid.KeyDown, GridExpDataCPIG.KeyDown

        If e.KeyCode = Keys.Delete And e.Modifiers = Keys.Shift Then
            Dim toremove As New ArrayList
            For Each c As DataGridViewCell In CType(sender, DataGridView).SelectedCells
                If Not toremove.Contains(c.RowIndex) Then toremove.Add(c.RowIndex)
            Next
            Try
                For Each i As Integer In toremove
                    CType(sender, DataGridView).Rows.RemoveAt(i)
                Next
            Catch ex As Exception

            End Try
        ElseIf e.KeyCode = Keys.V And e.Modifiers = Keys.Control Then
            PasteData(sender)
        ElseIf e.KeyCode = Keys.Delete Then
            For Each c As DataGridViewCell In CType(sender, DataGridView).SelectedCells
                c.Value = ""
            Next
        End If

    End Sub

    Private Sub rbEstimatePVAP_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles rbEstimatePVAP.CheckedChanged
        If rbEstimatePVAP.Checked Then
            mycase.cp.VaporPressureEquation = 0
            tbStatusPVAP.Text = "OK"
            If loaded Then cbEqPVAP.SelectedIndex = 0
        End If
    End Sub

    Private Sub rbEstimateCPIG_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles rbEstimateCPIG.CheckedChanged
        If loaded Then
            If rbEstimateCPIG.Checked Then
                mycase.cp.IdealgasCpEquation = 0
                tbStatusCPIG.Text = "OK"
                CalcJobackParams()
            End If
        End If
    End Sub

    Private Sub rbEstimateLIQDENS_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles rbEstimateLIQDENS.CheckedChanged
        If loaded Then
            If rbEstimateLIQDENS.Checked Then
                mycase.cp.LiquidDensityEquation = 0
                tbStatusLIQDENS.Text = "OK"
                CalcJobackParams()
            End If
        End If
    End Sub

    Private Sub rbEstimateLiquidTC_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles rbEstimateLiquidTC.CheckedChanged
        If loaded Then
            If rbEstimateLiquidTC.Checked Then
                mycase.cp.LiquidThermalConductivityEquation = 0
                tbStatusTCLiquid.Text = "OK"
                CalcJobackParams()
            End If
        End If
    End Sub

    Private Sub rbEstimateLIQVISC_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles rbEstimateLIQVISC.CheckedChanged
        If loaded Then
            If rbEstimateLIQVISC.Checked Then
                mycase.cp.LiquidViscosityEquation = 0
                tbStatusLIQVISC.Text = "OK"
                CalcJobackParams()
            End If
        End If
    End Sub

    Private Sub rbEstimateSolidDens_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles rbEstimateSolidDens.CheckedChanged
        If loaded Then
            If rbEstimateSolidDens.Checked Then
                CalcJobackParams()
            End If
        End If
    End Sub

    Private Sub btnViewPVAP_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnViewPVAP.Click

        Dim mytext As New System.Text.StringBuilder
        Dim px, py1, py2, py3, LT As New ArrayList, x, y, T, T1, T2, dT As Double
        Dim Eq, Heading1, Heading2 As String
        Dim CurveCount As Integer
        Dim pp As New PropertyPackages.RaoultPropertyPackage(False)
        Dim frc As New FormChart

        StoreData()
        Eq = cbEqPVAP.SelectedItem.Split(":")(0)

        'Fill x() table and add experimental data if available
        If mycase.DataPVAP.Count = 0 Then
            T2 = TextBoxTc.Text
            T1 = If(TextBoxMeltingTemp.Text = "", T2 * 0.3, TextBoxMeltingTemp.Text.ToDoubleFromCurrent())
            dT = (T2 - T1) / 25
            T = T1
            Do
                px.Add(T)
                T += dT
            Loop While T <= T2
            Heading1 = "T"
            Heading2 = "[" & su.temperature & "]"
        Else
            For Each d As Object In mycase.DataPVAP
                x = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d(0))
                px.Add(x)
                y = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, d(1))
                py1.Add(y)
            Next
            CurveCount = +1
            frc.y1ctitle = "Experimental Data"
            Heading1 = "T" & vbTab & "yExp"
            Heading2 = "[" & su.temperature & "]" & vbTab & "[" & su.pressure & "]"
            LT.Add(1) 'Point type curve
        End If

        'Add calculated Lee-Kesler Data
        For k2 = 0 To px.Count - 1
            T = SystemsOfUnits.Converter.ConvertToSI(su.temperature, px(k2))
            y = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PropertyPackages.Auxiliary.PROPS.Pvp_leekesler(T, SystemsOfUnits.Converter.ConvertToSI(su.temperature, TextBoxTc.Text), SystemsOfUnits.Converter.ConvertToSI(su.pressure, TextBoxPc.Text), TextBoxAF.Text))
            Select Case CurveCount
                Case 0
                    py1.Add(y)
                Case 1
                    py2.Add(y)
            End Select
        Next
        Select Case CurveCount
            Case 0
                frc.y1ctitle = "Lee-Kesler"
            Case 1
                frc.y2ctitle = "Lee-Kesler"
        End Select
        CurveCount += 1
        Heading1 = Heading1 & vbTab & vbTab & "Y Lee-Kesler"
        Heading2 = Heading2 & vbTab & vbTab & "[" & su.pressure & "]"
        LT.Add(4) 'dashed line type curve

        'Add regressed/user curve
        If Not Eq = "0" Then
            For k2 = 0 To px.Count - 1
                T = SystemsOfUnits.Converter.ConvertToSI(su.temperature, px(k2))
                y = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, pp.CalcCSTDepProp(Eq, tbPVAP_A.Text, tbPVAP_B.Text, tbPVAP_C.Text, tbPVAP_D.Text, tbPVAP_E.Text, T, 0))

                Select Case CurveCount
                    Case 1
                        py2.Add(y)
                    Case 2
                        py3.Add(y)
                End Select
            Next
            Select Case CurveCount
                Case 1
                    frc.y2ctitle = "Regression/User"
                Case 2
                    frc.y3ctitle = "Regression/User"
            End Select
            CurveCount += 1
            Heading1 = Heading1 & vbTab & "YCalc"
            Heading2 = Heading2 & vbTab & vbTab & "[" & su.pressure & "]"
            LT.Add(3) 'solid line type curve
        End If

        'fill data log box
        mytext.AppendLine(Heading1)
        mytext.AppendLine(Heading2)
        For k2 = 0 To px.Count - 1
            If CurveCount = 1 Then mytext.AppendLine(FormatNumber(px(k2), 2) & vbTab & FormatNumber(py1(k2), 2))
            If CurveCount = 2 Then mytext.AppendLine(FormatNumber(px(k2), 2) & vbTab & FormatNumber(py1(k2), 2) & vbTab & vbTab & FormatNumber(py2(k2), 2))
            If CurveCount = 3 Then mytext.AppendLine(FormatNumber(px(k2), 2) & vbTab & FormatNumber(py1(k2), 2) & vbTab & vbTab & FormatNumber(py2(k2), 2) & vbTab & vbTab & FormatNumber(py3(k2), 2))
        Next

        With frc
            .py1 = py1
            If CurveCount > 1 Then .py2 = py2
            If CurveCount > 2 Then .py3 = py3
            Select Case CurveCount
                Case 1
                    .ycurvetypes = New ArrayList(New Integer() {LT(0)})
                Case 2
                    .ycurvetypes = New ArrayList(New Integer() {LT(0), LT(1)})
                Case 3
                    .ycurvetypes = New ArrayList(New Integer() {LT(0), LT(1), LT(2)})
            End Select

            .title = "Vapor Pressure"
            .tbtext = mytext.ToString
            .px = px
            .xformat = 1
            .yformat = 2
            .ytitle = "Pvap [" & su.pressure & "]"
            .xtitle = "T [" & su.temperature & "]"
            .DrawChart()
            graphPVAP.MasterPane = .graph.MasterPane.Clone()
            graphPVAP.MasterPane.Rect = New RectangleF(-1, -1, graphPVAP.Width + 1, graphPVAP.Height + 1)
            graphPVAP.GraphPane.Border.IsVisible = False
            graphPVAP.MasterPane.AxisChange(CreateGraphics)
            graphPVAP.MasterPane.DoLayout(CreateGraphics)
            graphPVAP.Invalidate()
        End With
        frc.Dispose()
    End Sub

    Private Sub btnViewCPIG_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnViewCPIG.Click

        Dim mytext As New System.Text.StringBuilder
        Dim px, py1, py2 As New ArrayList, x, y1, y2, T As Double
        Dim pp As New PropertyPackages.RaoultPropertyPackage(False)
        Dim frc As New FormChart
        StoreData()
        ' in case of missing experimental data - draw only calculated curve
        If mycase.DataCPIG.Count = 0 Then
            mytext.AppendLine("T" & vbTab & "yCALC")
            mytext.AppendLine("[" & su.temperature & "]" & vbTab & "[" & su.heatCapacityCp & "]")
            Dim T1, T2, DT As Double
            T1 = mycase.cp.Critical_Temperature * 0.2
            T2 = mycase.cp.Critical_Temperature
            DT = 25
            For T = T1 To T2 Step DT
                x = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T)
                px.Add(x)
                y1 = SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, pp.CalcCSTDepProp(cbEqCPIG.SelectedItem.Split(":")(0), tbCPIG_A.Text, tbCPIG_B.Text, tbCPIG_C.Text, tbCPIG_D.Text, tbCPIG_E.Text, T, 0) / 1000) / TextBoxMW.Text
                py1.Add(y1)
                mytext.AppendLine(FormatNumber(x, 2) & vbTab & FormatNumber(y1, 2))
            Next

            With frc
                .px = px
                .py1 = py1
                .ycurvetypes = New ArrayList(New Integer() {3})
                .y1ctitle = "Regressed/Input Equation"
                .title = "Ideal Gas Heat Capacity Estimation Results"
            End With
        Else
            mytext.AppendLine("T" & vbTab & "yEXP" & vbTab & vbTab & "yCALC")
            mytext.AppendLine("[" & su.temperature & "]" & vbTab & "[" & su.heatCapacityCp & "]" & vbTab & "[" & su.heatCapacityCp & "]")
            For Each d As Object In mycase.DataCPIG
                x = d(0)
                px.Add(x)
                y1 = d(1)
                py1.Add(y1)
                T = SystemsOfUnits.Converter.ConvertToSI(su.temperature, x)
                y2 = SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, pp.CalcCSTDepProp(cbEqCPIG.SelectedItem.Split(":")(0), tbCPIG_A.Text, tbCPIG_B.Text, tbCPIG_C.Text, tbCPIG_D.Text, tbCPIG_E.Text, T, 0) / 1000) / TextBoxMW.Text
                py2.Add(y2)
                mytext.AppendLine(FormatNumber(x, 2) & vbTab & FormatNumber(y1, 2) & vbTab & vbTab & FormatNumber(y2, 2))
            Next
            With frc
                .px = px
                .py1 = py1
                .py2 = py2
                .ycurvetypes = New ArrayList(New Integer() {1, 3})
                .y1ctitle = "Experimental Data"
                .y2ctitle = "Regressed/Input Equation"
                .title = "Ideal Gas Heat Capacity"
            End With
        End If

        With frc
            .tbtext = mytext.ToString
            .xformat = 1
            .yformat = 1
            .ytitle = "Cpig [" & su.heatCapacityCp & "]"
            .xtitle = "T [" & su.temperature & "]"
            .title = "Ideal Gas Heat Capacity Fitting Results"
            .DrawChart()
            graphCPIG.MasterPane = .graph.MasterPane.Clone()
            graphCPIG.MasterPane.Rect = New RectangleF(-1, -1, graphCPIG.Width, graphCPIG.Height)
            graphCPIG.GraphPane.Border.IsVisible = False
            graphCPIG.MasterPane.AxisChange(CreateGraphics)
            graphCPIG.MasterPane.DoLayout(CreateGraphics)
            graphCPIG.Invalidate()
        End With

    End Sub
    Private Sub btnViewLIQDENS_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnViewLIQDENS.Click
        Dim mytext As New System.Text.StringBuilder
        Dim px, py1, py2, py3, LT As New ArrayList, y, T, T1, T2, dT, PV As Double
        Dim Eq, Heading1, Heading2 As String
        Dim CurveCount As Integer
        Dim pp As New PropertyPackages.RaoultPropertyPackage(False)
        Dim frc As New FormChart

        StoreData()
        Eq = cbEqLIQDENS.SelectedItem.Split(":")(0)

        'Fill x() table and add experimental data if available
        If mycase.DataLDENS.Count = 0 Then
            T2 = TextBoxTc.Text * 0.999
            T1 = If(TextBoxMeltingTemp.Text = "", T2 * 0.3, TextBoxMeltingTemp.Text.ToDoubleFromCurrent())
            dT = (T2 - T1) / 25
            T = T1
            Do
                px.Add(T)
                T += dT
            Loop While T <= T2
            Heading1 = "T"
            Heading2 = "[" & su.temperature & "]"
        Else
            For Each d As Object In mycase.DataLDENS
                px.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d(0)))
                py1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.density, d(1)))
            Next
            CurveCount = +1
            frc.y1ctitle = "Experimental Data"
            Heading1 = "T" & vbTab & "yExp"
            Heading2 = "[" & su.temperature & "]" & vbTab & "[" & su.density & "]"
            LT.Add(1) 'Point type curve
        End If

        'Add calculated Rackett Data
        For k2 = 0 To px.Count - 1
            T = SystemsOfUnits.Converter.ConvertToSI(su.temperature, px(k2))
            Try
                PV = PropertyPackages.Auxiliary.PROPS.Pvp_leekesler(T, SystemsOfUnits.Converter.ConvertToSI(su.temperature, TextBoxTc.Text), SystemsOfUnits.Converter.ConvertToSI(su.pressure, TextBoxPc.Text), TextBoxAF.Text)
                y = SystemsOfUnits.Converter.ConvertFromSI(su.density, PropertyPackages.Auxiliary.PROPS.liq_dens_rackett(T, SystemsOfUnits.Converter.ConvertToSI(su.temperature, TextBoxTc.Text), SystemsOfUnits.Converter.ConvertToSI(su.pressure, TextBoxPc.Text), TextBoxAF.Text, TextBoxMW.Text, TextBoxZRa.Text, 101325, PV))
            Catch ex As Exception
                y = 0.0#
            End Try

            Select Case CurveCount
                Case 0
                    py1.Add(y)
                Case 1
                    py2.Add(y)
            End Select
        Next
        Select Case CurveCount
            Case 0
                frc.y1ctitle = "Rackett"
            Case 1
                frc.y2ctitle = "Rackett"
        End Select
        CurveCount += 1
        Heading1 = Heading1 & vbTab & "Y Rackett"
        Heading2 = Heading2 & vbTab & "[" & su.density & "]"
        LT.Add(4) 'dashed line type curve

        'Add regressed/user curve
        If Not Eq = "0" Then
            For k2 = 0 To px.Count - 1
                T = SystemsOfUnits.Converter.ConvertToSI(su.temperature, px(k2))
                y = SystemsOfUnits.Converter.ConvertFromSI(su.density, pp.CalcCSTDepProp(Eq, tbLIQDENS_A.Text, tbLIQDENS_B.Text, tbLIQDENS_C.Text, tbLIQDENS_D.Text, tbLIQDENS_E.Text, T, 0))

                Select Case CurveCount
                    Case 1
                        py2.Add(y)
                    Case 2
                        py3.Add(y)
                End Select
            Next
            Select Case CurveCount
                Case 1
                    frc.y2ctitle = "Regression/User"
                Case 2
                    frc.y3ctitle = "Regression/User"
            End Select
            CurveCount += 1
            Heading1 = Heading1 & vbTab & "YCalc"
            Heading2 = Heading2 & vbTab & vbTab & "[" & su.density & "]"
            LT.Add(3) 'solid line type curve
        End If

        'fill data log box
        mytext.AppendLine(Heading1)
        mytext.AppendLine(Heading2)
        For k2 = 0 To px.Count - 1
            If CurveCount = 1 Then mytext.AppendLine(FormatNumber(px(k2), 2) & vbTab & FormatNumber(py1(k2), 2))
            If CurveCount = 2 Then mytext.AppendLine(FormatNumber(px(k2), 2) & vbTab & FormatNumber(py1(k2), 2) & vbTab & FormatNumber(py2(k2), 2))
            If CurveCount = 3 Then mytext.AppendLine(FormatNumber(px(k2), 2) & vbTab & FormatNumber(py1(k2), 2) & vbTab & FormatNumber(py2(k2), 2) & vbTab & vbTab & FormatNumber(py3(k2), 2))
        Next

        With frc
            .py1 = py1
            If CurveCount > 1 Then .py2 = py2
            If CurveCount > 2 Then .py3 = py3
            Select Case CurveCount
                Case 1
                    .ycurvetypes = New ArrayList(New Integer() {LT(0)})
                Case 2
                    .ycurvetypes = New ArrayList(New Integer() {LT(0), LT(1)})
                Case 3
                    .ycurvetypes = New ArrayList(New Integer() {LT(0), LT(1), LT(2)})
            End Select

            .title = "Liquid Density"
            .tbtext = mytext.ToString
            .px = px
            .xformat = 1
            .yformat = 1
            .ytitle = "Rho [" & su.density & "]"
            .xtitle = "T [" & su.temperature & "]"
            .DrawChart()
            graphLIQDENS.MasterPane = .graph.MasterPane.Clone()
            graphLIQDENS.MasterPane.Rect = New RectangleF(-1, -1, graphLIQDENS.Width + 1, graphLIQDENS.Height + 1)
            graphLIQDENS.GraphPane.Border.IsVisible = False
            graphLIQDENS.MasterPane.AxisChange(CreateGraphics)
            graphLIQDENS.MasterPane.DoLayout(CreateGraphics)
            graphLIQDENS.Invalidate()
        End With
    End Sub

    Private Sub btnViewSolidCp_Click(sender As System.Object, e As System.EventArgs) Handles btnViewSolidCp.Click

        Dim mytext As New System.Text.StringBuilder
        Dim px, py1, py2 As New ArrayList, x, y1, y2, T, dT As Double
        Dim pp As New PropertyPackages.RaoultPropertyPackage(False)
        Dim frc As New FormChart
        Dim Eq As String

        StoreData()
        Eq = cbEqCpS.SelectedItem.ToString.Split(":")(0)
        If mycase.DataCpS.Count = 0 Then
            T = 0
            mytext.AppendLine("T" & vbTab & "yCALC")
            mytext.AppendLine("[" & su.temperature & "]" & vbTab & "[" & su.heatCapacityCp & "]")
            dT = SystemsOfUnits.Converter.ConvertToSI(su.temperature, TextBoxMeltingTemp.Text) / 25
            Do
                x = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T)
                px.Add(x)
                If Eq = "0" Then
                    y1 = 0
                Else
                    y1 = SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, pp.CalcCSTDepProp(Eq, tbCpS_A.Text, tbCpS_B.Text, tbCpS_C.Text, tbCpS_D.Text, tbCpS_E.Text, T, 0)) / TextBoxMW.Text / 1000
                End If
                py1.Add(y1)
                mytext.AppendLine(FormatNumber(x, 2) & vbTab & FormatNumber(y1, 2))
                T += dT
            Loop Until T > SystemsOfUnits.Converter.ConvertToSI(su.temperature, TextBoxMeltingTemp.Text)
            With frc
                .px = px
                .py1 = py1
                .ycurvetypes = New ArrayList(New Integer() {3})
                .y1ctitle = "Regressed/Input Equation"
                .title = "Solid Heat Capacity"
            End With
        Else
            mytext.AppendLine("T" & vbTab & "yEXP" & vbTab & vbTab & "yCALC")
            mytext.AppendLine("[" & su.temperature & "]" & vbTab & "[" & su.heatCapacityCp & "]" & vbTab & "[" & su.heatCapacityCp & "]")
            For Each d As Object In mycase.DataCpS
                x = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d(0))
                px.Add(x)
                y1 = SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, d(1)) / TextBoxMW.Text
                py1.Add(y1)
                If Eq = "0" Then
                    y2 = 0
                Else
                    y2 = SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, pp.CalcCSTDepProp(Eq, tbCpS_A.Text, tbCpS_B.Text, tbCpS_C.Text, tbCpS_D.Text, tbCpS_E.Text, d(0), 0)) / TextBoxMW.Text / 1000
                End If
                py2.Add(y2)
                mytext.AppendLine(x & vbTab & FormatNumber(y1, 2) & vbTab & vbTab & FormatNumber(y2, 2))
            Next
            With frc
                .px = px
                .py1 = py1
                .py2 = py2
                .ycurvetypes = New ArrayList(New Integer() {1, 3})
                .y1ctitle = "Experimental Data"
                .y2ctitle = "Regressed/Input Equation"
                .title = "Solid Heat Capacity Fitting Results"
            End With
        End If

        With frc
            .tbtext = mytext.ToString
            .xformat = 1
            .yformat = 1
            .ytitle = "Cp [ " & su.heatCapacityCp & " ]"
            .xtitle = "T [ " & su.temperature & " ]"
            .DrawChart()
            graphSOLIDCP.MasterPane = .graph.MasterPane.Clone()
            graphSOLIDCP.MasterPane.Rect = New RectangleF(-1, -1, graphSOLIDCP.Width + 1, graphSOLIDCP.Height + 1)
            graphSOLIDCP.GraphPane.Border.IsVisible = False
            graphSOLIDCP.MasterPane.AxisChange(CreateGraphics)
            graphSOLIDCP.MasterPane.DoLayout(CreateGraphics)
            graphSOLIDCP.Invalidate()
        End With
    End Sub
    Private Sub btnViewSolidDens_Click(sender As System.Object, e As System.EventArgs) Handles btnViewSolidDens.Click
        Dim mytext As New System.Text.StringBuilder
        Dim px, py1, py2 As New ArrayList, x, y1, y2, T, dT As Double
        Dim pp As New PropertyPackages.RaoultPropertyPackage(False)
        Dim frc As New FormChart
        Dim Eq As String
        Eq = cbEqSolidDENS.SelectedItem.ToString.Split(":")(0)
        If mycase.DataRoS.Count = 0 Then
            T = 0
            mytext.AppendLine("T" & vbTab & "yCALC")
            mytext.AppendLine("[" & su.temperature & "]" & vbTab & "[" & su.density & "]")
            dT = SystemsOfUnits.Converter.ConvertToSI(su.temperature, TextBoxMeltingTemp.Text) / 25
            Do
                x = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T)
                px.Add(x)
                If Eq = "0" Then
                    y1 = 0
                Else
                    y1 = SystemsOfUnits.Converter.ConvertFromSI(su.density, pp.CalcCSTDepProp(Eq, tbRoS_A.Text, tbRoS_B.Text, tbRoS_C.Text, tbRoS_D.Text, tbRoS_E.Text, T, 0) * TextBoxMW.Text)
                End If
                py1.Add(y1)
                mytext.AppendLine(FormatNumber(x, 2) & vbTab & FormatNumber(y1, 2))
                T += dT
            Loop Until T > SystemsOfUnits.Converter.ConvertToSI(su.temperature, TextBoxMeltingTemp.Text)
            With frc
                .py1 = py1
                .ycurvetypes = New ArrayList(New Integer() {3})
                .y1ctitle = "Regressed/Input Equation"
                .title = "Solid Density Formula"
            End With
        Else
            mytext.AppendLine("T" & vbTab & "yEXP" & vbTab & "  yCALC")
            mytext.AppendLine("[" & su.temperature & "]" & vbTab & "[" & su.density & "]" & vbTab & "  [" & su.density & "]")
            For Each d As Object In mycase.DataRoS
                x = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d(0))
                px.Add(x)
                y1 = SystemsOfUnits.Converter.ConvertFromSI(su.density, d(1)) * TextBoxMW.Text
                py1.Add(y1)
                If Eq = "0" Then
                    y2 = 0
                Else
                    y2 = SystemsOfUnits.Converter.ConvertFromSI(su.density, pp.CalcCSTDepProp(Eq, tbRoS_A.Text, tbRoS_B.Text, tbRoS_C.Text, tbRoS_D.Text, tbRoS_E.Text, d(0), 0) * TextBoxMW.Text)
                End If
                py2.Add(y2)
                mytext.AppendLine(FormatNumber(x, 2) & vbTab & FormatNumber(y1, 2) & vbTab & "  " & FormatNumber(y2, 2))
            Next
            With frc
                .py1 = py1
                .py2 = py2
                .y1ctitle = "Experimental Data"
                .y2ctitle = "Regressed/Input Equation"
                .ycurvetypes = New ArrayList(New Integer() {1, 3})
                .title = "Solid Density"
            End With
        End If

        With frc
            .tbtext = mytext.ToString
            .px = px
            .xformat = 1
            .yformat = 1
            .ytitle = "Rho [" & su.density & "]"
            .xtitle = "T [" & su.temperature & "]"
            .DrawChart()
            graphSOLIDDENS.MasterPane = .graph.MasterPane.Clone()
            graphSOLIDDENS.MasterPane.Rect = New RectangleF(-1, -1, graphSOLIDDENS.Width + 1, graphSOLIDDENS.Height + 1)
            graphSOLIDDENS.GraphPane.Border.IsVisible = False
            graphSOLIDDENS.MasterPane.AxisChange(CreateGraphics)
            graphSOLIDDENS.MasterPane.DoLayout(CreateGraphics)
            graphSOLIDDENS.Invalidate()
        End With
    End Sub
    Private Sub btnViewLIQVISC_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnViewLIQVISC.Click
        Dim mytext As New System.Text.StringBuilder
        Dim px, py1, py2, py3, LT As New ArrayList, y, T, T1, T2, dT As Double
        Dim Eq, Heading1, Heading2 As String
        Dim CurveCount As Integer
        Dim pp As New PropertyPackages.RaoultPropertyPackage(False)
        Dim frc As New FormChart

        StoreData()
        Eq = cbEqLIQVISC.SelectedItem.Split(":")(0)

        'Fill x() table and add experimental data if available
        If mycase.DataLVISC.Count = 0 Then
            T2 = TextBoxNBP.Text * 0.999
            T1 = If(TextBoxMeltingTemp.Text = "", T2 * 0.3, TextBoxMeltingTemp.Text.ToDoubleFromCurrent())
            dT = (T2 - T1) / 25
            T = T1
            Do
                px.Add(T)
                T += dT
            Loop While T <= T2
            Heading1 = "T"
            Heading2 = "[" & su.temperature & "]"
        Else
            For Each d As Object In mycase.DataLVISC
                px.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d(0)))
                py1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.viscosity, d(1)))
            Next
            CurveCount = +1
            frc.y1ctitle = "Experimental Data"
            Heading1 = "T" & vbTab & "yExp"
            Heading2 = "[" & su.temperature & "]" & vbTab & "[" & su.viscosity & "]"
            LT.Add(1) 'Point type curve
        End If

        'Add calculated Letsou-Stiel Data
        For k2 = 0 To px.Count - 1
            T = SystemsOfUnits.Converter.ConvertToSI(su.temperature, px(k2))
            y = SystemsOfUnits.Converter.ConvertFromSI(su.viscosity, PropertyPackages.Auxiliary.PROPS.viscl_letsti(T, SystemsOfUnits.Converter.ConvertToSI(su.temperature, TextBoxTc.Text), SystemsOfUnits.Converter.ConvertToSI(su.pressure, TextBoxPc.Text), TextBoxAF.Text, TextBoxMW.Text))
            Select Case CurveCount
                Case 0
                    py1.Add(y)
                Case 1
                    py2.Add(y)
            End Select
        Next
        Select Case CurveCount
            Case 0
                frc.y1ctitle = "Letsou-Stiel"
            Case 1
                frc.y2ctitle = "Letsou-Stiel"
        End Select
        CurveCount += 1
        Heading1 = Heading1 & vbTab & "Y Letsou-Stiel"
        Heading2 = Heading2 & vbTab & "[" & su.viscosity & "]"
        LT.Add(4) 'dashed line type curve

        'Add regressed/user curve
        If Not Eq = "0" Then
            For k2 = 0 To px.Count - 1
                T = SystemsOfUnits.Converter.ConvertToSI(su.temperature, px(k2))
                y = SystemsOfUnits.Converter.ConvertFromSI(su.viscosity, pp.CalcCSTDepProp(Eq, tbLIQVISC_A.Text, tbLIQVISC_B.Text, tbLIQVISC_C.Text, tbLIQVISC_D.Text, tbLIQVISC_E.Text, T, 0))

                Select Case CurveCount
                    Case 1
                        py2.Add(y)
                    Case 2
                        py3.Add(y)
                End Select
            Next
            Select Case CurveCount
                Case 1
                    frc.y2ctitle = "Regression/User"
                Case 2
                    frc.y3ctitle = "Regression/User"
            End Select
            CurveCount += 1
            Heading1 = Heading1 & vbTab & "YCalc"
            Heading2 = Heading2 & vbTab & vbTab & "[" & su.viscosity & "]"
            LT.Add(3) 'solid line type curve
        End If

        'fill data log box
        mytext.AppendLine(Heading1)
        mytext.AppendLine(Heading2)
        For k2 = 0 To px.Count - 1
            If CurveCount = 1 Then mytext.AppendLine(FormatNumber(px(k2), 2) & vbTab & FormatNumber(py1(k2), 5))
            If CurveCount = 2 Then mytext.AppendLine(FormatNumber(px(k2), 2) & vbTab & FormatNumber(py1(k2), 5) & vbTab & FormatNumber(py2(k2), 5))
            If CurveCount = 3 Then mytext.AppendLine(FormatNumber(px(k2), 2) & vbTab & FormatNumber(py1(k2), 5) & vbTab & FormatNumber(py2(k2), 5) & vbTab & vbTab & FormatNumber(py3(k2), 5))
        Next

        With frc
            .py1 = py1
            If CurveCount > 0 Then .py2 = py2
            If CurveCount > 1 Then .py3 = py3
            Select Case CurveCount
                Case 1
                    .ycurvetypes = New ArrayList(New Integer() {LT(0)})
                Case 2
                    .ycurvetypes = New ArrayList(New Integer() {LT(0), LT(1)})
                Case 3
                    .ycurvetypes = New ArrayList(New Integer() {LT(0), LT(1), LT(2)})
            End Select

            .title = "Liquid Viscosity"
            .tbtext = mytext.ToString
            .px = px
            .xformat = 1
            .yformat = 1
            .ytitle = "Visc. [" & su.viscosity & "]"
            .xtitle = "T [" & su.temperature & "]"
            .DrawChart()
            graphLIQVISC.MasterPane = .graph.MasterPane.Clone()
            graphLIQVISC.MasterPane.Rect = New RectangleF(-1, -1, graphLIQVISC.Width + 1, graphLIQVISC.Height + 1)
            graphLIQVISC.GraphPane.Border.IsVisible = False
            graphLIQVISC.MasterPane.AxisChange(CreateGraphics)
            graphLIQVISC.MasterPane.DoLayout(CreateGraphics)
            graphLIQVISC.Invalidate()
        End With
    End Sub

    Function GetInfo(ByVal code As Integer) As String

        Select Case code
            Case -1
                Return "Error - Wrong parameters were specified"
            Case 0
                Return "Error - Interrupted by user"
            Case 1
                Return "OK - Relative decrease of sum of function values squares (real and predicted on the base  of extrapolation) is less or equal EpsF."
            Case 2
                Return "OK - Relative change of solution is less or equal EpsX."
            Case 3
                Return "OK - Relative decrease of sum of function values squares (real and predicted on the base  of extrapolation) is less or equal EpsF / Relative change of solution is less or equal EpsX."
            Case 4
                Return "OK - Cosine of the angle between vector of function values and each of the Jacobian columns is less or equal EpsG by absolute value."
            Case 5
                Return "Number of iterations exceeds MaxIts."
            Case 6
                Return "EpsF is too small. It is impossible to get a better result."
            Case 7
                Return "EpsX is too small. It is impossible to get a better result"
            Case 8
                Return "EpsG is too small. Vector of functions is orthogonal to Jacobian columns with near-machine precision."
            Case Else
                Return "OK"
        End Select

    End Function

    Public Sub PasteData(ByRef dgv As DataGridView)
        Dim tArr() As String
        Dim arT() As String
        Dim i, ii As Integer
        Dim c, cc, r As Integer

        tArr = Clipboard.GetText().Split(Environment.NewLine)

        If dgv.SelectedCells.Count > 0 Then
            r = dgv.SelectedCells(0).RowIndex
            c = dgv.SelectedCells(0).ColumnIndex
        Else
            r = 0
            c = 0
        End If
        For i = 0 To tArr.Length - 1
            If tArr(i) <> "" Then
                arT = tArr(i).Split(Char.ConvertFromUtf32(9))
                For ii = 0 To arT.Length - 1
                    If r > dgv.Rows.Count - 1 Then
                        dgv.Rows.Add()
                        dgv.Rows(0).Cells(0).Selected = True
                    End If
                Next
                r = r + 1
            End If
        Next
        If dgv.SelectedCells.Count > 0 Then
            r = dgv.SelectedCells(0).RowIndex
            c = dgv.SelectedCells(0).ColumnIndex
        Else
            r = 0
            c = 0
        End If
        For i = 0 To tArr.Length - 1
            If tArr(i) <> "" Then
                arT = tArr(i).Split(Char.ConvertFromUtf32(9))
                cc = c
                For ii = 0 To arT.Length - 1
                    cc = GetNextVisibleCol(dgv, cc)
                    If cc > dgv.ColumnCount - 1 Then Exit For
                    dgv.Item(cc, r).Value = arT(ii).TrimStart
                    cc = cc + 1
                Next
                r = r + 1
            End If
        Next

    End Sub

    Private Function GetNextVisibleCol(ByRef dgv As DataGridView, ByVal stidx As Integer) As Integer

        Dim i As Integer

        For i = stidx To dgv.ColumnCount - 1
            If dgv.Columns(i).Visible Then Return i
        Next

        Return Nothing

    End Function

    Private Sub rbCoeffPVAP_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles rbCoeffPVAP.CheckedChanged
        mycase.EqPVAP = rbCoeffPVAP.Checked
    End Sub

    Private Sub rbCoeffCPIG_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles rbCoeffCPIG.CheckedChanged
        mycase.EqCPIG = rbCoeffCPIG.Checked
    End Sub

    Private Sub rbCoeffLIQDENS_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles rbCoeffLIQDENS.CheckedChanged
        mycase.EqLDENS = rbCoeffLIQDENS.Checked
    End Sub

    Private Sub rbCoeffLIQVISC_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles rbCoeffLIQVISC.CheckedChanged
        mycase.EqLVISC = rbCoeffLIQVISC.Checked
    End Sub
    Private Sub cbEqCpS_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbEqCpS.SelectedIndexChanged
        If mycase.EqCpS Then mycase.cp.SolidHeatCapacityEquation = cbEqCpS.SelectedItem.ToString.Split(":")(0)
    End Sub
    Private Sub cbEqSolidDENS_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbEqSolidDENS.SelectedIndexChanged
        If mycase.EqSDens Then mycase.cp.SolidDensityEquation = cbEqSolidDENS.SelectedItem.ToString.Split(":")(0)
    End Sub
    Private Sub cbEqPVAP_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbEqPVAP.SelectedIndexChanged
        If mycase.EqPVAP Then
            mycase.cp.VaporPressureEquation = cbEqPVAP.SelectedItem.ToString.Split(":")(0)
            If mycase.cp.VaporPressureEquation = "1000" Then
                tbUserDefEqPVAP.Enabled = True
            Else
                tbUserDefEqPVAP.Enabled = False
            End If
        End If
    End Sub

    Private Sub cbEqCPIG_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbEqCPIG.SelectedIndexChanged
        If mycase.EqCPIG Then
            mycase.cp.IdealgasCpEquation = cbEqCPIG.SelectedItem.ToString.Split(":")(0)
            If mycase.cp.IdealgasCpEquation = "1000" Then
                tbUserDefCPIGEq.Enabled = True
            Else
                tbUserDefCPIGEq.Enabled = False
            End If
        End If
    End Sub

    Private Sub cbEqCPLiquid_SelectedIndexChanged(sender As System.Object, e As System.EventArgs) Handles cbEqCPLiquid.SelectedIndexChanged
        If mycase.EqCPLiquid Then mycase.cp.LiquidHeatCapacityEquation = cbEqCPLiquid.SelectedItem.ToString.Split(":")(0)
    End Sub

    Private Sub cbEqLIQDENS_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbEqLIQDENS.SelectedIndexChanged
        If mycase.EqLDENS Then
            mycase.cp.LiquidDensityEquation = cbEqLIQDENS.SelectedItem.ToString.Split(":")(0)
            If mycase.cp.LiquidDensityEquation = "1000" Then
                tbUserDefDensLiqEq.Enabled = True
            Else
                tbUserDefDensLiqEq.Enabled = False
            End If
        End If
    End Sub

    Private Sub cbEqLIQVISC_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbEqLIQVISC.SelectedIndexChanged
        If mycase.EqLVISC Then
            mycase.cp.LiquidViscosityEquation = cbEqLIQVISC.SelectedItem.ToString.Split(":")(0)
            If mycase.cp.LiquidViscosityEquation = "1000" Then
                tbUserDefLiqViscEq.Enabled = True
            Else
                tbUserDefLiqViscEq.Enabled = False
            End If
        End If
    End Sub

    Private Sub CheckBoxTc_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxTc.CheckedChanged
        If CheckBoxTc.Checked = True Then
            TextBoxTc.Enabled = False
            CalcJobackParams()
        Else
            TextBoxTc.Enabled = True
        End If
    End Sub

    Private Sub CheckBoxPc_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxPc.CheckedChanged
        If CheckBoxPc.Checked = True Then
            TextBoxPc.Enabled = False
            CalcJobackParams()
        Else
            TextBoxPc.Enabled = True
        End If
    End Sub

    Private Sub CheckBoxZc_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxZc.CheckedChanged
        If CheckBoxZc.Checked = True Then
            TextBoxZc.Enabled = False
            CalcJobackParams()
        Else
            TextBoxZc.Enabled = True
        End If
    End Sub

    Private Sub CheckBoxZRa_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxZRa.CheckedChanged
        If CheckBoxZRa.Checked = True Then
            TextBoxZRa.Enabled = False
            CalcJobackParams()
        Else
            TextBoxZRa.Enabled = True
        End If
    End Sub

    Private Sub CheckBoxAF_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxAF.CheckedChanged
        If CheckBoxAF.Checked = True Then
            TextBoxAF.Enabled = False
            CalcJobackParams()
        Else
            TextBoxAF.Enabled = True
        End If
    End Sub

    Private Sub CheckBoxMW_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxMW.CheckedChanged
        If CheckBoxMW.Checked = True Then
            TextBoxMW.Enabled = False
            CalcJobackParams()
        Else
            TextBoxMW.Enabled = True
        End If
    End Sub

    Private Sub CheckBoxDHF_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxDHF.CheckedChanged
        If CheckBoxDHF.Checked = True Then
            TextBoxDHF.Enabled = False
            CalcJobackParams()
        Else
            TextBoxDHF.Enabled = True
        End If
    End Sub

    Private Sub CheckBoxDGF_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxDGF.CheckedChanged
        If CheckBoxDGF.Checked = True Then
            TextBoxDGF.Enabled = False
            CalcJobackParams()
        Else
            TextBoxDGF.Enabled = True
        End If
    End Sub

    Private Sub CheckBoxNBP_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxNBP.CheckedChanged
        If CheckBoxNBP.Checked = True Then
            TextBoxNBP.Enabled = False
            CalcJobackParams()
        Else
            TextBoxNBP.Enabled = True
        End If
    End Sub

    Private Sub CheckBoxCSAF_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxCSAF.CheckedChanged
        If CheckBoxCSAF.Checked = True Then
            TextBoxCSAF.Enabled = False
            CalcJobackParams()
        Else
            TextBoxCSAF.Enabled = True
        End If
    End Sub

    Private Sub CheckBoxCSSP_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxCSSP.CheckedChanged
        If CheckBoxCSSP.Checked = True Then
            TextBoxCSSP.Enabled = False
            CalcJobackParams()
        Else
            TextBoxCSSP.Enabled = True
        End If
    End Sub

    Private Sub CheckBoxCSLV_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBoxCSLV.CheckedChanged
        If CheckBoxCSLV.Checked = True Then
            TextBoxCSLV.Enabled = False
            CalcJobackParams()
        Else
            TextBoxCSLV.Enabled = True
        End If
    End Sub

    Private Sub CheckBoxMeltingTemp_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If CheckBoxMeltingTemp.Checked = True Then
            TextBoxMeltingTemp.Enabled = False
            CalcJobackParams()
        Else
            TextBoxMeltingTemp.Enabled = True
        End If
    End Sub
    Private Sub CheckBoxEnthOfFusion_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If CheckBoxEnthOfFusion.Checked = True Then
            TextBoxEnthOfFusion.Enabled = False
            CalcJobackParams()
        Else
            TextBoxEnthOfFusion.Enabled = True
        End If
    End Sub

    Private Sub rb_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles rbCoeffLIQDENS.CheckedChanged,
                                    rbRegressLIQVISC.CheckedChanged, rbRegressLIQDENS.CheckedChanged,
                                    rbCoeffLIQVISC.CheckedChanged, rbRegressPVAP.CheckedChanged,
                                    rbCoeffPVAP.CheckedChanged, rbRegressCPLiquid.CheckedChanged, rbRegressCPIG.CheckedChanged, rbCoeffCPLiquid.CheckedChanged, rbCoeffCPIG.CheckedChanged
        If loaded Then
            StoreData()
        End If
    End Sub

    Private Sub FormCompoundCreator_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Shown
        loaded = True
        FormMain.TranslateFormFunction?.Invoke(Me)
    End Sub

    Private Sub cbUnits_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles cbUnits.SelectedIndexChanged
        If loaded Then
            StoreData()
        End If
        If CType(Me.MdiParent, FormMain).AvailableUnitSystems.ContainsKey(cbUnits.SelectedItem.ToString) Then
            su = CType(Me.MdiParent, FormMain).AvailableUnitSystems(cbUnits.SelectedItem.ToString)
        End If
        If loaded Then
            Try
                UpdateRegressionDataUnits(mycase.su, su)
            Catch ex As Exception
            End Try
            mycase.su = su
            UpdateUnits()
            WriteData()
        End If
    End Sub

    Sub UpdateRegressionDataUnits(prevsu As Interfaces.IUnitsOfMeasure, newsu As Interfaces.IUnitsOfMeasure)

        For Each row As DataGridViewRow In GridExpDataPVAP.Rows
            row.Cells(0).Value = SystemsOfUnits.Converter.Convert(prevsu.temperature, newsu.temperature, row.Cells(0).Value)
            row.Cells(1).Value = SystemsOfUnits.Converter.Convert(prevsu.pressure, newsu.pressure, row.Cells(1).Value)
        Next

        For Each row As DataGridViewRow In GridExpDataCPIG.Rows
            row.Cells(0).Value = SystemsOfUnits.Converter.Convert(prevsu.temperature, newsu.temperature, row.Cells(0).Value)
            row.Cells(1).Value = SystemsOfUnits.Converter.Convert(prevsu.heatCapacityCp, newsu.heatCapacityCp, row.Cells(1).Value)
        Next

        For Each row As DataGridViewRow In GridExpDataCPLiquid.Rows
            row.Cells(0).Value = SystemsOfUnits.Converter.Convert(prevsu.temperature, newsu.temperature, row.Cells(0).Value)
            row.Cells(1).Value = SystemsOfUnits.Converter.Convert(prevsu.heatCapacityCp, newsu.heatCapacityCp, row.Cells(1).Value)
        Next

        For Each row As DataGridViewRow In GridExpDataCpS.Rows
            row.Cells(0).Value = SystemsOfUnits.Converter.Convert(prevsu.temperature, newsu.temperature, row.Cells(0).Value)
            row.Cells(1).Value = SystemsOfUnits.Converter.Convert(prevsu.heatCapacityCp, newsu.heatCapacityCp, row.Cells(1).Value)
        Next

        For Each row As DataGridViewRow In GridExpDataLIQVISC.Rows
            row.Cells(0).Value = SystemsOfUnits.Converter.Convert(prevsu.temperature, newsu.temperature, row.Cells(0).Value)
            row.Cells(1).Value = SystemsOfUnits.Converter.Convert(prevsu.viscosity, newsu.viscosity, row.Cells(1).Value)
        Next

        For Each row As DataGridViewRow In GridExpDataLIQDENS.Rows
            row.Cells(0).Value = SystemsOfUnits.Converter.Convert(prevsu.temperature, newsu.temperature, row.Cells(0).Value)
            row.Cells(1).Value = SystemsOfUnits.Converter.Convert(prevsu.density, newsu.density, row.Cells(1).Value)
        Next

        For Each row As DataGridViewRow In GridExpDataRoS.Rows
            row.Cells(0).Value = SystemsOfUnits.Converter.Convert(prevsu.temperature, newsu.temperature, row.Cells(0).Value)
            row.Cells(1).Value = SystemsOfUnits.Converter.Convert(prevsu.density, newsu.density, row.Cells(1).Value)
        Next

    End Sub

    Sub RenderSMILES()
        'definition available, render molecule
        Try
            Dim ind As New Indigo()
            Dim mol As IndigoObject = ind.loadMolecule(TextBoxSMILES.Text.Trim().TrimStart(vbCrLf).TrimEnd(vbCrLf).Trim())
            Dim renderer As New IndigoRenderer(ind)
            With renderer
                ind.setOption("render-image-size", pbRender.Size.Width, pbRender.Size.Height)
                ind.setOption("render-margins", 15, 15)
                ind.setOption("render-coloring", True)
                ind.setOption("render-background-color", Color.White)
            End With
            pbRender.Image = renderer.renderToBitmap(mol)
            btnRenderSMILES.Enabled = False
        Catch ex As Exception
            MessageBox.Show(ex.Message.ToString, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub

    Private Sub TextBoxSMILES_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxSMILES.TextChanged
        btnRenderSMILES.Enabled = True
    End Sub

    Private Sub btnRenderSMILES_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRenderSMILES.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            RenderSMILES()
        End If
    End Sub

    Private Sub GridExpData_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles GridExpDataPVAP.CellValueChanged,
                GridExpDataLIQVISC.CellValueChanged, GridExpDataLIQDENS.CellValueChanged, GridExpDataRoS.CellValueChanged, GridExpDataCpS.CellValueChanged, GridExpDataCPLiquid.CellValueChanged, GridExpDataCPIG.CellValueChanged
        If loaded Then
            Try
                StoreCPIGData()
                StoreCPLData()
                StoreLiqDensData()
                StoreLIQVData()
                StorePVAPData()
                StoreSolidCpData()
                StoreSolidDensData()
                CheckDataStatus()
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub TextBoxEnthOfFusion_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Try
            TextBoxEnthOfFusion2.Text = SystemsOfUnits.Converter.ConvertToSI(su.enthalpy, TextBoxEnthOfFusion.Text) / Me.TextBoxMW.Text * 1000
            CheckDataStatus()
        Catch ex As Exception
            TextBoxEnthOfFusion2.Text = ""
        End Try
    End Sub

    Private Sub TextBoxChanged_recalc(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBoxMW.TextChanged
        CalcJobackParams()
        CheckDataStatus()
    End Sub

    Private Sub LinkPubChem_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkPubChem.LinkClicked
        System.Diagnostics.Process.Start("http://pubchem.ncbi.nlm.nih.gov/edit2/index.html")
    End Sub

    Private Sub LinkLabel1_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel1.LinkClicked
        System.Diagnostics.Process.Start("http://webbook.nist.gov/cgi/cbook.cgi?ID=" & TextBoxCAS.Text)
    End Sub

    Private Sub LinkLabel2_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabel2.LinkClicked, LinkLabel4.LinkClicked
        System.Diagnostics.Process.Start("http://www.ddbst.com/unifacga.html")
    End Sub
    Private Sub LinkLabel3_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs)
        System.Diagnostics.Process.Start("http://chemeo.com/")
    End Sub


    Private Sub btnViewCPLiquid_Click(sender As System.Object, e As System.EventArgs) Handles btnViewCPLiquid.Click
        Dim mytext As New System.Text.StringBuilder
        Dim px, py1, py2 As New ArrayList, x, y1, y2, T As Double
        Dim pp As New PropertyPackages.RaoultPropertyPackage(False)
        Dim frc As New FormChart
        StoreData()
        ' in case of missing experimental data - draw only calculated curve
        If mycase.DataCPLiquid.Count = 0 Then
            mytext.AppendLine("T" & vbTab & "yCALC")
            mytext.AppendLine("[" & su.temperature & "]" & vbTab & "[" & su.heatCapacityCp & "]")
            Dim T1, T2, DT As Double
            T1 = mycase.cp.Normal_Boiling_Point * 0.3
            T2 = mycase.cp.Normal_Boiling_Point
            DT = 25
            For T = T1 To T2 Step DT
                x = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T)
                px.Add(x)
                y1 = SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, pp.CalcCSTDepProp(cbEqCPLiquid.SelectedItem.Split(":")(0), tbCPLiquid_A.Text, tbCPLiquid_B.Text, tbCPLiquid_C.Text, tbCPLiquid_D.Text, tbCPLiquid_E.Text, T, 0) / 1000) / TextBoxMW.Text
                py1.Add(y1)
                mytext.AppendLine(FormatNumber(x, 2) & vbTab & FormatNumber(y1, 2))
            Next

            With frc
                .px = px
                .py1 = py1
                .ycurvetypes = New ArrayList(New Integer() {3})
                .y1ctitle = "Regressed/Input Equation"
                .title = "Liquid Heat Capacity"
            End With
        Else
            mytext.AppendLine("T" & vbTab & "yEXP" & vbTab & vbTab & "yCALC")
            mytext.AppendLine("[" & su.temperature & "]" & vbTab & "[" & su.heatCapacityCp & "]" & vbTab & "[" & su.heatCapacityCp & "]")
            For Each d As Object In mycase.DataCPLiquid
                x = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d(0))
                px.Add(x)
                y1 = SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, d(1))
                py1.Add(y1)
                T = d(0)
                y2 = SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, pp.CalcCSTDepProp(cbEqCPLiquid.SelectedItem.Split(":")(0), tbCPLiquid_A.Text, tbCPLiquid_B.Text, tbCPLiquid_C.Text, tbCPLiquid_D.Text, tbCPLiquid_E.Text, T, 0) / 1000) / TextBoxMW.Text.ToDoubleFromCurrent()
                py2.Add(y2)
                mytext.AppendLine(FormatNumber(x, 2) & vbTab & FormatNumber(y1, 2) & vbTab & vbTab & FormatNumber(y2, 2))
            Next
            With frc
                .px = px
                .py1 = py1
                .py2 = py2
                .ycurvetypes = New ArrayList(New Integer() {1, 3})
                .y1ctitle = "Experimental Data"
                .y2ctitle = "Regressed/Input Equation"
                .title = "Liquid Heat Capacity Calculation Results"
            End With
        End If

        With frc
            .tbtext = mytext.ToString
            .xformat = 1
            .yformat = 1
            .ytitle = "Cp Liquid [" & su.heatCapacityCp & "]"
            .xtitle = "T [" & su.temperature & "]"
            .title = "Liquid Heat Capacity Fitting Results"
            .DrawChart()
            graphLIQCP.MasterPane = .graph.MasterPane.Clone()
            graphLIQCP.MasterPane.Rect = New RectangleF(-1, -1, graphLIQCP.Width + 1, graphLIQCP.Height + 1)
            graphLIQCP.GraphPane.Border.IsVisible = False
            graphLIQCP.MasterPane.AxisChange(CreateGraphics)
            graphLIQCP.MasterPane.DoLayout(CreateGraphics)
            graphLIQCP.Invalidate()
        End With
    End Sub

    Private Sub btnVieTCLiquid_Click(sender As System.Object, e As System.EventArgs) Handles btnViewTCLiquid.Click

        Dim mytext As New System.Text.StringBuilder
        Dim px, py1, py2 As New ArrayList, x, y1, y2, T As Double
        Dim pp As New PropertyPackages.RaoultPropertyPackage(False)
        Dim frc As New FormChart
        StoreData()
        ' in case of missing experimental data - draw only calculated curve
        If mycase.DataLTC.Count = 0 Then
            mytext.AppendLine("T" & vbTab & "yCALC")
            mytext.AppendLine("[" & su.temperature & "]" & vbTab & "[" & su.thermalConductivity & "]")
            Dim t0 As Integer = mycase.cp.Normal_Boiling_Point * 0.3
            Dim t1 As Integer = mycase.cp.Normal_Boiling_Point
            Dim stp As Integer = (t1 - t0) / 50
            For T = t0 To t1 Step stp
                x = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T)
                px.Add(x)
                y1 = SystemsOfUnits.Converter.ConvertFromSI(su.thermalConductivity, PROPS.condl_latini(T, mycase.cp.Normal_Boiling_Point,
                                                                                                       mycase.cp.Critical_Temperature,
                                                                                                        mycase.cp.Molar_Weight, "X"))
                py1.Add(y1)
                mytext.AppendLine(FormatNumber(x, 2) & vbTab & FormatNumber(y1, 2))
            Next

            With frc
                .px = px
                .py1 = py1
                .ycurvetypes = New ArrayList(New Integer() {3})
                .y1ctitle = "Regressed/Input Equation"
                .title = "Liquid Thermal Conductivity"
            End With
        Else
            mytext.AppendLine("T" & vbTab & "yEXP" & vbTab & vbTab & "yCALC")
            mytext.AppendLine("[" & su.temperature & "]" & vbTab & "[" & su.thermalConductivity & "]" & vbTab & "[" & su.thermalConductivity & "]")
            For Each d As Object In mycase.DataLTC
                x = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d(0))
                px.Add(x)
                y1 = SystemsOfUnits.Converter.ConvertFromSI(su.thermalConductivity, d(1))
                py1.Add(y1)
                T = d(0)
                y2 = SystemsOfUnits.Converter.ConvertFromSI(su.thermalConductivity, pp.CalcCSTDepProp(cbEqTCLiquid.SelectedItem.Split(":")(0), tbTCLiquid_A.Text, tbTCLiquid_B.Text, tbTCLiquid_C.Text, tbTCLiquid_D.Text, tbTCLiquid_E.Text, T, 0))
                py2.Add(y2)
                mytext.AppendLine(FormatNumber(x, 2) & vbTab & FormatNumber(y1, 2) & vbTab & vbTab & FormatNumber(y2, 2))
            Next
            With frc
                .px = px
                .py1 = py1
                .py2 = py2
                .ycurvetypes = New ArrayList(New Integer() {1, 3})
                .y1ctitle = "Experimental Data"
                .y2ctitle = "Regressed/Input Equation"
                .title = "Liquid Thermal Conductivity Calculation Results"
            End With
        End If

        With frc
            .tbtext = mytext.ToString
            .xformat = 1
            .yformat = 1
            .ytitle = "TC Liquid [" & su.thermalConductivity & "]"
            .xtitle = "T [" & su.temperature & "]"
            .title = "Liquid Thermal Conductivity Fitting Results"
            .DrawChart()
            graphLIQTC.MasterPane = .graph.MasterPane.Clone()
            graphLIQTC.MasterPane.Rect = New RectangleF(-1, -1, graphLIQTC.Width + 1, graphLIQTC.Height + 1)
            graphLIQTC.GraphPane.Border.IsVisible = False
            graphLIQTC.MasterPane.AxisChange(CreateGraphics)
            graphLIQTC.MasterPane.DoLayout(CreateGraphics)
            graphLIQTC.Invalidate()
        End With
    End Sub

    Sub StoreCPLData()

        mycase.DataCPLiquid.Clear()
        Dim MW As Double = Me.TextBoxMW.Text
        For Each row As DataGridViewRow In Me.GridExpDataCPLiquid.Rows
            Try
                If row.Index < Me.GridExpDataCPLiquid.Rows.Count - 1 Then
                    mycase.DataCPLiquid.Add(New Double() {SystemsOfUnits.Converter.ConvertToSI(su.temperature, row.Cells(0).Value), SystemsOfUnits.Converter.ConvertToSI(su.heatCapacityCp, row.Cells(1).Value)})
                End If
            Catch ex As Exception
            End Try
        Next

    End Sub

    Private Sub btnRegressCPLiquid_Click(sender As System.Object, e As System.EventArgs) Handles btnRegressCPLiquid.Click

        loaded = False

        StoreCPLData()

        Dim result As Object = RegressData(6, False)

        tbStatusCPLiquid.Text = GetInfo(result(3))

        With mycase.cp

            .LiquidHeatCapacityEquation = 5

            For Each it As Object In cbEqCPLiquid.Items
                If it.ToString.Split(":")(0) = .LiquidHeatCapacityEquation Then
                    cbEqCPLiquid.SelectedIndex = cbEqCPLiquid.Items.IndexOf(it)
                    Exit For
                End If
            Next

            .Liquid_Heat_Capacity_Const_A = result(0)(0) * 1000 * TextBoxMW.Text.ToDoubleFromCurrent
            .Liquid_Heat_Capacity_Const_B = result(0)(1) * 1000 * TextBoxMW.Text.ToDoubleFromCurrent
            .Liquid_Heat_Capacity_Const_C = result(0)(2) * 1000 * TextBoxMW.Text.ToDoubleFromCurrent
            .Liquid_Heat_Capacity_Const_D = result(0)(3) * 1000 * TextBoxMW.Text.ToDoubleFromCurrent
            .Liquid_Heat_Capacity_Const_E = result(0)(4) * 1000 * TextBoxMW.Text.ToDoubleFromCurrent

            tbCPLiquid_A.Text = .Liquid_Heat_Capacity_Const_A
            tbCPLiquid_B.Text = .Liquid_Heat_Capacity_Const_B
            tbCPLiquid_C.Text = .Liquid_Heat_Capacity_Const_C
            tbCPLiquid_D.Text = .Liquid_Heat_Capacity_Const_D
            tbCPLiquid_E.Text = .Liquid_Heat_Capacity_Const_E

        End With
        rbRegressCPLiquid.Checked = True
        loaded = True
    End Sub

    Private Sub GridMODFAC_CellMouseEnter(sender As Object, e As DataGridViewCellEventArgs) Handles GridMODFAC.CellMouseEnter
        If e.RowIndex >= 0 Then
            PicMODFAC.Image = Image.FromFile(GridMODFAC.Rows(e.RowIndex).Cells(3).Tag(0))
            TBModfac.Text = GridMODFAC.Rows(e.RowIndex).Cells(3).Tag(1)
        End If
    End Sub

    Private Sub GridMODFAC_CellMouseLeave(sender As Object, e As DataGridViewCellEventArgs) Handles GridMODFAC.CellMouseLeave
        PicMODFAC.Image = Nothing
        TBModfac.Text = ""
    End Sub

    Private Sub GridUNIFAC_CellMouseEnter(sender As Object, e As DataGridViewCellEventArgs) Handles GridUNIFAC.CellMouseEnter
        If e.RowIndex >= 0 Then
            PicUNIFAC.Image = Image.FromFile(GridUNIFAC.Rows(e.RowIndex).Cells(3).Tag(0))
            TBUnifac.Text = GridUNIFAC.Rows(e.RowIndex).Cells(3).Tag(1)
        End If
    End Sub

    Private Sub GridUNIFAC_CellMouseLeave(sender As Object, e As DataGridViewCellEventArgs) Handles GridUNIFAC.CellMouseLeave
        PicUNIFAC.Image = Nothing
        TBUnifac.Text = ""
    End Sub
    Private Sub GridNISTMODFAC_CellMouseEnter(sender As Object, e As DataGridViewCellEventArgs) Handles GridNISTMODFAC.CellMouseEnter
        If e.RowIndex >= 0 Then
            PicNISTMODFAC.Image = Image.FromFile(GridNISTMODFAC.Rows(e.RowIndex).Cells(3).Tag(0))
            TbNISTMODFAC.Text = GridNISTMODFAC.Rows(e.RowIndex).Cells(3).Tag(1)
        End If
    End Sub
    Private Sub GridNISTMODFAC_CellMouseLeave(sender As Object, e As DataGridViewCellEventArgs) Handles GridNISTMODFAC.CellMouseLeave
        PicNISTMODFAC.Image = Nothing
        TbNISTMODFAC.Text = ""
    End Sub
    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.

    End Sub

    Private Sub GridExpDataPVAP_CellValidating(sender As Object, e As DataGridViewCellValidatingEventArgs) Handles GridExpDataPVAP.CellValidating
        DirectCast(sender, DataGridView).ValidateCellForDouble(e)
    End Sub

    Private Sub GridExpDataCPIG_CellValidating(sender As Object, e As DataGridViewCellValidatingEventArgs) Handles GridExpDataCPIG.CellValidating
        DirectCast(sender, DataGridView).ValidateCellForDouble(e)
    End Sub

    Private Sub GridExpDataCPLiquid_CellValidating(sender As Object, e As DataGridViewCellValidatingEventArgs) Handles GridExpDataCPLiquid.CellValidating
        DirectCast(sender, DataGridView).ValidateCellForDouble(e)
    End Sub

    Private Sub GridExpDataLIQDENS_CellValidating(sender As Object, e As DataGridViewCellValidatingEventArgs) Handles GridExpDataLIQDENS.CellValidating
        DirectCast(sender, DataGridView).ValidateCellForDouble(e)
    End Sub

    Private Sub GridExpDataLIQVISC_CellValidating(sender As Object, e As DataGridViewCellValidatingEventArgs) Handles GridExpDataLIQVISC.CellValidating
        DirectCast(sender, DataGridView).ValidateCellForDouble(e)
    End Sub

    Private Sub GridExpDataRoS_CellValidating(sender As Object, e As DataGridViewCellValidatingEventArgs) Handles GridExpDataRoS.CellValidating
        DirectCast(sender, DataGridView).ValidateCellForDouble(e)
    End Sub

    Private Sub GridExpDataCpS_CellValidating(sender As Object, e As DataGridViewCellValidatingEventArgs) Handles GridExpDataCpS.CellValidating
        DirectCast(sender, DataGridView).ValidateCellForDouble(e)
    End Sub

    Private Sub tbUserDefEqPVAP_TextChanged(sender As Object, e As EventArgs) Handles tbUserDefEqPVAP.TextChanged
        If loaded Then mycase.cp.VaporPressureEquation = tbUserDefEqPVAP.Text
    End Sub

    Private Sub tbUserDefCPIGEq_TextChanged(sender As Object, e As EventArgs) Handles tbUserDefCPIGEq.TextChanged
        If loaded Then mycase.cp.IdealgasCpEquation = tbUserDefCPIGEq.Text
    End Sub

    Private Sub tbUserDefCPLEq_TextChanged(sender As Object, e As EventArgs) Handles tbUserDefCPLEq.TextChanged
        If loaded Then mycase.cp.LiquidHeatCapacityEquation = tbUserDefCPLEq.Text
    End Sub

    Private Sub tbUserDefDensLiqEq_TextChanged(sender As Object, e As EventArgs) Handles tbUserDefDensLiqEq.TextChanged
        If loaded Then mycase.cp.LiquidDensityEquation = tbUserDefDensLiqEq.Text
    End Sub

    Private Sub tbUserDefLiqViscEq_TextChanged(sender As Object, e As EventArgs) Handles tbUserDefLiqViscEq.TextChanged
        If loaded Then mycase.cp.LiquidViscosityEquation = tbUserDefLiqViscEq.Text
    End Sub

    Private Sub SalvarNoBancoDeDadosToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SalvarNoBancoDeDadosToolStripMenuItem.Click

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("XML File", "*.xml")})

        If handler IsNot Nothing Then
            Try

                StoreData()

                mycase.database = handler.FullPath

                'In case of additionalt Joback groups no UNIFAC calculation is possible anymore.
                'Delete UNIFAC groups to prevent wrong calculations.
                If Not PureUNIFACCompound Then
                    mycase.cp.UNIFACGroups.Clear()
                    mycase.cp.MODFACGroups.Clear()
                    mycase.cp.NISTMODFACGroups.Clear()
                End If

                mycase.cp.OriginalDB = "User"
                mycase.cp.CurrentDB = "User"
                Using stream As New MemoryStream
                    If handler.Exists() Then
                        Using str = handler.OpenRead()
                            str.CopyTo(stream)
                            stream.Position = 0
                        End Using
                    End If
                    Global.DWSIM.Thermodynamics.Databases.UserDB.AddCompounds(New BaseClasses.ConstantProperties() {mycase.cp}, stream, True)
                    handler.Write(stream)
                End Using

            Catch ex As Exception

                MessageBox.Show(DWSIM.App.GetLocalString("ErroCompSaveDB") & ex.Message.ToString, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)

            End Try
        End If

    End Sub

    Private Sub BancoDeDadosKDBCHERICToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles BancoDeDadosKDBCHERICToolStripMenuItem.Click

        Dim f As New FormImportCompoundKDB
        StoreData()
        f.BaseCompound = mycase.cp
        f.tbSearchString.Text = TextBoxName.Text
        If f.ShowDialog(Me) = DialogResult.OK Then

            mycase.CalcMW = False
            mycase.CalcNBP = False
            mycase.CalcAF = False
            mycase.CalcCSSP = False
            mycase.CalcTC = False
            mycase.CalcPC = False
            mycase.CalcZC = False
            mycase.CalcZRA = False
            mycase.CalcHF = False
            mycase.CalcGF = False

            loaded = False
            WriteData()
            loaded = True

        End If

    End Sub

    Private Sub DBOpenDlg_FileOk(sender As Object, e As System.ComponentModel.CancelEventArgs)

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim f As New FormPureComp() With {.Flowsheet = Nothing, .Added = False, .MyCompound = mycase.cp, .SUnits = su}
        f.ShowDialog(Me)
    End Sub

    Private Sub tsmiIgnoreUnsupportedGroups_Click(sender As Object, e As EventArgs) Handles tsmiIgnoreUnsupportedGroups.Click
        mycase.IgnoreUnsupportedGroups = tsmiIgnoreUnsupportedGroups.Checked
    End Sub

    Private Sub cbEqTCLiquid_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbEqTCLiquid.SelectedIndexChanged
        If mycase.EqLTC Then
            mycase.cp.LiquidThermalConductivityEquation = cbEqTCLiquid.SelectedItem.ToString.Split(":")(0)
            If mycase.cp.LiquidThermalConductivityEquation = "1000" Then
                tbUserDefTCEq.Enabled = True
            Else
                tbUserDefTCEq.Enabled = False
            End If
        End If
    End Sub

    Private Sub BancoDeDadosChemeoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles BancoDeDadosChemeoToolStripMenuItem.Click

        Dim f As New FormImportCompoundDataChemeo
        StoreData()
        f.BaseCompound = mycase.cp
        f.tbSearchString.Text = TextBoxName.Text
        If f.ShowDialog(Me) = DialogResult.OK Then

            mycase.CalcMW = False
            mycase.CalcNBP = False
            mycase.CalcTC = False
            mycase.CalcPC = False
            mycase.CalcHF = False
            mycase.CalcGF = False

            loaded = False
            WriteData()
            loaded = True

        End If

    End Sub

    Private Sub TextBoxDHF_TextChanged(sender As Object, e As EventArgs) Handles TextBoxTc.TextChanged, TextBoxPc.TextChanged, TextBoxNBP.TextChanged, TextBoxMeltingTemp.TextChanged, TextBoxEnthOfFusion.TextChanged, TextBoxDHF.TextChanged, TextBoxDGF.TextChanged, TextBoxAF.TextChanged
        CheckDataStatus()
        TextBoxEnthOfFusion2.Text = (mycase.cp.EnthalpyOfFusionAtTf * 1000 / mycase.cp.Molar_Weight).ToString("N2")
    End Sub

    Private Sub TextBoxUNIQUAC_Q_TextChanged(sender As Object, e As EventArgs) Handles TextBoxUNIQUAC_R.TextChanged, TextBoxUNIQUAC_Q.TextChanged
        CheckDataStatus()
    End Sub

    Private Sub TextBoxName_TextChanged(sender As Object, e As EventArgs) Handles TextBoxName.TextChanged
        mycase.cp.Name = TextBoxName.Text
    End Sub

    Private Sub ExportarDadosParaArquivoJSONToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ExportarDadosParaArquivoJSONToolStripMenuItem.Click


        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON File", "*.json")})

        If handler IsNot Nothing Then
            Try
                mycase.cp.OriginalDB = "User"
                mycase.cp.CurrentDB = "User"
                Dim jsondata = Newtonsoft.Json.JsonConvert.SerializeObject(mycase.cp, Newtonsoft.Json.Formatting.Indented)
                Using stream As New IO.MemoryStream()
                    Using writer As New StreamWriter(stream) With {.AutoFlush = True}
                        writer.Write(jsondata)
                        handler.Write(stream)
                    End Using
                End Using
                MessageBox.Show(DWSIM.App.GetLocalString("FileSaved"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Information)
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erroaosalvararquivo") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try

        End If

    End Sub

    Private Sub ImportarDeArquivoJSONToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ImportarDeArquivoJSONToolStripMenuItem.Click

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowOpenDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON File", "*.json")})

        If handler IsNot Nothing Then
            Dim jsondata = handler.ReadAllText()
            Try
                Dim comp = Newtonsoft.Json.JsonConvert.DeserializeObject(Of BaseClasses.ConstantProperties)(jsondata)
                StoreData()
                mycase.cp = comp
                mycase.CalcMW = False
                mycase.CalcNBP = False
                mycase.CalcAF = False
                mycase.CalcCSSP = False
                mycase.CalcTC = False
                mycase.CalcPC = False
                mycase.CalcZC = False
                mycase.CalcZRA = False
                mycase.CalcHF = False
                mycase.CalcGF = False
                loaded = False
                WriteData()
                loaded = True
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erro") + ": " + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If

    End Sub

    Private Sub EstruturaUNIFACMODFACDDBToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles EstruturaUNIFACMODFACDDBToolStripMenuItem.Click
        Dim f As New FormImportCompoundDataDDB
        StoreData()
        f.BaseCompound = mycase.cp
        f.tbSearchString.Text = TextBoxCAS.Text
        If f.ShowDialog(Me) = DialogResult.OK Then
            loaded = False
            WriteData()
            loaded = True
            CalcJobackParams()
        End If
    End Sub

    Sub CheckDataStatus()

        If loaded Then StoreData()

        Dim sb As New Text.StringBuilder

        sb.Append("")

        With mycase.cp

            If .Molar_Weight = 0.0 Then sb.Append("Please enter the Molecular Weight; ")
            If .Normal_Boiling_Point = 0.0 Then sb.Append("Please enter the Normal Boiling Point; ")
            If .Critical_Pressure = 0.0 And .Critical_Temperature = 0.0 And .Acentric_Factor = 0.0 Then
                sb.Append("Please enter the Critical Properties (Temperature, Pressure, Acentric Factor); ")
                If .VaporPressureEquation = "" Then sb.Append("Needs Vapor Pressure eq. coeffs or Tc/Pc/w to estimate using Lee-Kesler; ")
            ElseIf .Critical_Pressure = 0.0 And .Critical_Temperature = 0.0 And .Acentric_Factor > 0.0 Then
                sb.Append("Please enter the Critical Properties (Temperature, Pressure); ")
                If .VaporPressureEquation = "" Then sb.Append("Needs Vapor Pressure eq. coeffs or Tc/Pc/w to estimate using Lee-Kesler; ")
            ElseIf .Critical_Pressure = 0.0 And .Critical_Temperature > 0.0 And .Acentric_Factor = 0.0 Then
                sb.Append("Please enter the Critical Properties (Pressure, Acentric Factor); ")
                If .VaporPressureEquation = "" Then sb.Append("Needs Vapor Pressure eq. coeffs or Tc/Pc/w to estimate using Lee-Kesler; ")
            ElseIf .Critical_Pressure > 0.0 And .Critical_Temperature = 0.0 And .Acentric_Factor = 0.0 Then
                sb.Append("Please enter the Critical Properties (Temperature, Acentric Factor); ")
                If .VaporPressureEquation = "" Then sb.Append("Needs Vapor Pressure eq. coeffs or Tc/Pc/w to estimate using Lee-Kesler; ")
            ElseIf .Critical_Pressure > 0.0 And .Critical_Temperature > 0.0 And .Acentric_Factor = 0.0 Then
                sb.Append("Please enter the Acentric Factor; ")
                If .VaporPressureEquation = "" Then sb.Append("Needs Vapor Pressure eq. coeffs or Tc/Pc/w to estimate using Lee-Kesler; ")
            ElseIf .Critical_Pressure = 0.0 And .Critical_Temperature > 0.0 And .Acentric_Factor > 0.0 Then
                sb.Append("Please enter the Critical Pressure; ")
                If .VaporPressureEquation = "" Then sb.Append("Needs Vapor Pressure eq. coeffs or Tc/Pc/w to estimate using Lee-Kesler; ")
            ElseIf .Critical_Pressure > 0.0 And .Critical_Temperature = 0.0 And .Acentric_Factor > 0.0 Then
                sb.Append("Please enter the Critical Temperature; ")
                If .VaporPressureEquation = "" Then sb.Append("Needs Vapor Pressure eq. coeffs or Tc/Pc/w to estimate using Lee-Kesler; ")
            End If
            If .IG_Enthalpy_of_Formation_25C = 0.0 And .IG_Gibbs_Energy_of_Formation_25C Then
                sb.Append("No formation data defined (DHf/DGf), won't be able to reaction heat balances; ")
            End If
            If .IdealgasCpEquation = "" Then sb.Append("Ideal gas Cp: no equation or UNIFAC structure defined; ")
            If .EnthalpyOfFusionAtTf = 0.0 And .TemperatureOfFusion = 0.0 Then
                sb.Append("No solid phase data defined (Tf/Hf), won't be able to calculate SLE; ")
            End If
            If .UNIQUAC_Q = 0.0 And .UNIQUAC_R = 0.0 Then sb.Append("No UNIQUAC parameters defined, won't work with UNIQUAC model; ")
            If .UNIFACGroups.Count = 0.0 Then sb.Append("No UNIFAC structure defined, won't work with UNIFAC-based models; ")

            Dim text = sb.ToString

            If text = "" Then text = "All OK!"

            tbStatus.Text = text

        End With


    End Sub

    Private Sub TextBoxID_TextChanged(sender As Object, e As EventArgs) Handles TextBoxID.TextChanged
        Try
            mycase.cp.ID = Integer.Parse(TextBoxID.Text)
        Catch ex As Exception

        End Try
    End Sub

End Class

<System.Serializable()> Public Class CompoundGeneratorCase

    Sub New()
        cp = New BaseClasses.ConstantProperties
        su = New SystemsOfUnits.SI
    End Sub

    Public Filename As String = ""

    Public cp As BaseClasses.ConstantProperties
    Public database As String = ""
    Public su As SystemsOfUnits.Units

    Public IgnoreUnsupportedGroups As Boolean = False

    Public nf As String = My.Computer.Info.InstalledUICulture.NumberFormat.ToString

    Public CalcMW As Boolean = True
    Public CalcTC As Boolean = True
    Public CalcPC As Boolean = True
    Public CalcZC As Boolean = True
    Public CalcZRA As Boolean = True
    Public CalcAF As Boolean = True
    Public CalcHF As Boolean = True
    Public CalcGF As Boolean = True
    Public CalcNBP As Boolean = True
    Public CalcCSAF As Boolean = True
    Public CalcCSSP As Boolean = True
    Public CalcCSMV As Boolean = True
    Public CalcMP As Boolean = True
    Public CalcEM As Boolean = True

    Public RegressPVAP As Boolean = False
    Public RegressCPIG As Boolean = False
    Public RegressCPLiquid As Boolean = False
    Public RegressLVISC As Boolean = False
    Public RegressLDENS As Boolean = False
    Public RegressLTC As Boolean = False
    Public RegressCpS As Boolean = False
    Public RegressRoS As Boolean = False

    Public EqPVAP As Boolean = False
    Public EqCPIG As Boolean = False
    Public EqCPLiquid As Boolean = False
    Public EqLVISC As Boolean = False
    Public EqLDENS As Boolean = False
    Public EqLTC As Boolean = False
    Public EqCpS As Boolean = False
    Public EqSDens As Boolean = False

    Public RegressOKPVAP As Boolean = False
    Public RegressOKCPIG As Boolean = False
    Public RegressOKCPLiquid As Boolean = False
    Public RegressOKLVISC As Boolean = False
    Public RegressOKLDENS As Boolean = False
    Public RegressOKLTC As Boolean = False
    Public RegressOKCpS As Boolean = False
    Public RegressOKRoS As Boolean = False

    Public ErrorMsgPVAP As String = ""
    Public ErrorMsgCPIG As String = ""
    Public ErrorMsgCPLiquid As String = ""
    Public ErrorMsgLVISC As String = ""
    Public ErrorMsgLDENS As String = ""
    Public ErrorMsgLTC As String = ""
    Public ErrorMsgCpS As String = ""
    Public ErrorMsgRoS As String = ""

    Public JobackGroups As New ArrayList

    Public DataPVAP As New ArrayList
    Public DataCPIG As New ArrayList
    Public DataCPLiquid As New ArrayList
    Public DataLVISC As New ArrayList
    Public DataLDENS As New ArrayList
    Public DataLTC As New ArrayList
    Public DataCpS As New ArrayList
    Public DataRoS As New ArrayList
    Public AdditionalAtoms As ArrayList

End Class