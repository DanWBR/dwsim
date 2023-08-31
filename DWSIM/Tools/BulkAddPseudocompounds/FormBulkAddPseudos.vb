Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Thermodynamics.PetroleumCharacterization.Methods
Imports prop = DWSIM.Thermodynamics.PropertyPackages.Auxiliary.PROPS
Imports DWSIM.ExtensionMethods
Imports System.IO
Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker

Public Class FormBulkAddPseudos

    Public Flowsheet As Interfaces.IFlowsheet

    Dim compounds As New List(Of ConstantProperties)

    Private Sub FormBulkAddPseudos_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ChangeDefaultFont()

        grid1.Worksheets(0).SetScale(Settings.DpiScale)

        grid1.Worksheets(0).SetCols(7)
        grid1.Worksheets(0).SetRows(1000)

        With grid1.Worksheets(0).ColumnHeaders
            .Item(0).Text = "Name"
            .Item(1).Text = "MW"
            .Item(2).Text = "NBP"
            .Item(3).Text = "SG"
            .Item(4).Text = "TC"
            .Item(5).Text = "PC"
            .Item(6).Text = "AF"
        End With

        grid1.Worksheets(0).SetRangeStyles(0, 0, 1000, 19,
                                           New unvell.ReoGrid.WorksheetRangeStyle() With {
                                           .Flag = unvell.ReoGrid.PlainStyleFlag.FontSize + unvell.ReoGrid.PlainStyleFlag.FontName,
                                           .FontName = SystemFonts.MessageBoxFont.Name, .FontSize = SystemFonts.MessageBoxFont.Size})

        grid1.Worksheets(0).SetRangeDataFormat(0, 1, 1000, 18, unvell.ReoGrid.DataFormat.CellDataFormatFlag.Number,
                                               New unvell.ReoGrid.DataFormat.NumberDataFormatter.NumberFormatArgs() With
                                                {
                                                .DecimalPlaces = 4,
                                                .NegativeStyle = unvell.ReoGrid.DataFormat.NumberDataFormatter.NumberNegativeStyle.Minus,
                                                .UseSeparator = False
                                                })

        cbNBPUnits.SelectedItem = "C"
        cbTCUnits.SelectedItem = "C"
        cbPCUnits.SelectedItem = "Pa"

        ComboBoxAF.SelectedIndex = 0
        ComboBoxMW.SelectedIndex = 0
        ComboBoxSG.SelectedIndex = 0
        ComboBoxTC.SelectedIndex = 0
        ComboBoxPC.SelectedIndex = 0

        btnAdd.Visible = Flowsheet IsNot Nothing

        FormMain.TranslateFormFunction?.Invoke(Me)

    End Sub

    Private Sub btnEstimate_Click(sender As Object, e As EventArgs) Handles btnEstimate.Click

        Try

            EstimateProps()

            cbViewComp.Items.Clear()
            For Each comp In compounds
                cbViewComp.Items.Add(comp.Name)
            Next

            btnExport.Enabled = True
            btnExporttoXML.Enabled = True
            btnAdd.Enabled = True

        Catch ex As Exception

            MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)

        End Try

    End Sub

    Private Sub EstimateProps()

        compounds = New List(Of ConstantProperties)()

        Dim prop2 As New Utilities.PetroleumCharacterization.Methods.GL

        Dim sheet = grid1.Worksheets(0)
        Dim data As Object

        For i = 0 To sheet.Rows - 1

            'data validation
            Dim mw, sg, tb, tc, pc, af As Object
            Dim mwflag, sgflag, tbflag, tcflag, pcflag, afflag As Boolean

            data = sheet.Cells(i, 0).Data
            If data Is Nothing Then
                Exit For
            End If
            mw = sheet.Cells(i, 1).Data
            If mw IsNot Nothing AndAlso Not Double.TryParse(mw.ToString(), New Double) Then
                Throw New Exception(String.Format("Error in row {0}: MW is not a valid number.", i + 1))
            End If
            tb = sheet.Cells(i, 2).Data
            If tb IsNot Nothing AndAlso Not Double.TryParse(tb.ToString(), New Double) Then
                Throw New Exception(String.Format("Error in row {0}: NBP is not a valid number.", i + 1))
            End If
            sg = sheet.Cells(i, 3).Data
            If sg IsNot Nothing AndAlso Not Double.TryParse(sg.ToString(), New Double) Then
                Throw New Exception(String.Format("Error in row {0}: SG is not a valid number.", i + 1))
            End If
            tc = sheet.Cells(i, 4).Data
            If tc IsNot Nothing AndAlso Not Double.TryParse(tc.ToString(), New Double) Then
                Throw New Exception(String.Format("Error in row {0}: TC is not a valid number.", i + 1))
            End If
            pc = sheet.Cells(i, 5).Data
            If pc IsNot Nothing AndAlso Not Double.TryParse(pc.ToString(), New Double) Then
                Throw New Exception(String.Format("Error in row {0}: PC is not a valid number.", i + 1))
            End If
            af = sheet.Cells(i, 6).Data
            If af IsNot Nothing AndAlso Not Double.TryParse(af.ToString(), New Double) Then
                Throw New Exception(String.Format("Error in row {0}: AF is not a valid number.", i + 1))
            End If

            If mw Is Nothing Then mwflag = True
            If tb Is Nothing Then tbflag = True
            If sg Is Nothing Then sgflag = True
            If tc Is Nothing Then tcflag = True
            If pc Is Nothing Then pcflag = True
            If af Is Nothing Then afflag = True

            If tb IsNot Nothing Then
                tb = tb.ToString().ToDoubleFromCurrent().ConvertToSI(cbNBPUnits.SelectedItem)
            End If

            'calculations

            Dim comp As New ConstantProperties()

            comp.Name = sheet.Cells(i, 0).Data.ToString()

            If mw IsNot Nothing Then
                If sg IsNot Nothing And tb IsNot Nothing Then
                ElseIf sg Is Nothing And tb IsNot Nothing Then
                    sg = PropertyMethods.d15_Riazi(mw)
                ElseIf sg Is Nothing And tb Is Nothing Then
                    sg = PropertyMethods.d15_Riazi(mw)
                    tb = 1080 - Math.Exp(6.97996 - 0.01964 * mw ^ (2 / 3))
                ElseIf sg IsNot Nothing And tb Is Nothing Then
                    tb = 1080 - Math.Exp(6.97996 - 0.01964 * mw ^ (2 / 3))
                End If
            ElseIf sg IsNot Nothing Then
                If mw Is Nothing And tb IsNot Nothing Then
                    Select Case Me.ComboBoxMW.SelectedItem.ToString
                        Case "Winn (1956)"
                            mw = PropertyMethods.MW_Winn(tb, sg)
                        Case "Riazi (1986)"
                            mw = PropertyMethods.MW_Riazi(tb, sg)
                        Case "Lee-Kesler (1974)"
                            mw = PropertyMethods.MW_LeeKesler(tb, sg)
                        Case Else
                            mw = PropertyMethods.MW_Riazi(tb, sg)
                    End Select
                ElseIf mw Is Nothing And tb Is Nothing Then
                    mw = ((Math.Log(1.07 - sg) - 3.56073) / (-2.93886)) ^ 10
                    tb = 1080 - Math.Exp(6.97996 - 0.01964 * mw ^ (2 / 3))
                    Select Case Me.ComboBoxMW.SelectedItem.ToString
                        Case "Winn (1956)"
                            mw = PropertyMethods.MW_Winn(tb, sg)
                        Case "Riazi (1986)"
                            mw = PropertyMethods.MW_Riazi(tb, sg)
                        Case "Lee-Kesler (1974)"
                            mw = PropertyMethods.MW_LeeKesler(tb, sg)
                        Case Else
                            mw = PropertyMethods.MW_Riazi(tb, sg)
                    End Select
                End If
                mw = PropertyMethods.MW_Winn(tb, sg)
            ElseIf tb IsNot Nothing Then
                tb = Convert.ToDouble(tb).ConvertToSI(cbNBPUnits.SelectedItem)
                If mw IsNot Nothing And sg IsNot Nothing Then
                    mw = mw
                    sg = sg
                ElseIf mw Is Nothing And sg IsNot Nothing Then
                    sg = sg
                    Select Case Me.ComboBoxMW.SelectedItem.ToString
                        Case "Winn (1956)"
                            mw = PropertyMethods.MW_Winn(tb, sg)
                        Case "Riazi (1986)"
                            mw = PropertyMethods.MW_Riazi(tb, sg)
                        Case "Lee-Kesler (1974)"
                            mw = PropertyMethods.MW_LeeKesler(tb, sg)
                        Case Else
                            mw = PropertyMethods.MW_Riazi(tb, sg)
                    End Select
                ElseIf mw Is Nothing And sg Is Nothing Then
                    Select Case Me.ComboBoxMW.SelectedItem.ToString
                        Case "Winn (1956)"
                            mw = PropertyMethods.MW_Winn(tb, sg)
                        Case "Riazi (1986)"
                            mw = PropertyMethods.MW_Riazi(tb, sg)
                        Case "Lee-Kesler (1974)"
                            mw = PropertyMethods.MW_LeeKesler(tb, sg)
                        Case Else
                            mw = PropertyMethods.MW_Riazi(tb, sg)
                    End Select
                    sg = PropertyMethods.d15_Riazi(mw)
                ElseIf mw IsNot Nothing And sg Is Nothing Then
                    mw = mw
                    sg = PropertyMethods.d15_Riazi(mw)
                End If
            End If

            comp.Molar_Weight = mw
            comp.NBP = tb
            comp.Normal_Boiling_Point = tb

            Dim v37, v98, va, vb, T1, T2 As Double

            T1 = 37.8 + 273.15
            T2 = 98.9 + 273.15

            v37 = PropertyMethods.Visc37_Abbott(tb, sg)
            v98 = PropertyMethods.Visc98_Abbott(tb, sg)

            va = PropertyMethods.ViscWaltherASTM_A(T1, v37, T2, v98)
            vb = PropertyMethods.ViscWaltherASTM_B(T1, v37, T2, v98)

            comp.PF_Tv1 = T1
            comp.PF_Tv2 = T2
            comp.PF_v1 = v37
            comp.PF_v2 = v98
            comp.PF_vA = va
            comp.PF_vB = vb
            comp.PF_SG = sg
            comp.PF_MM = mw

            comp.IsPF = 1

            'Tc
            If tc Is Nothing Then
                Select Case Me.ComboBoxTC.SelectedItem.ToString
                    Case "Riazi-Daubert (1985)"
                        tc = PropertyMethods.Tc_RiaziDaubert(comp.NBP, comp.PF_SG)
                    Case "Riazi (2005)"
                        tc = PropertyMethods.Tc_Riazi(comp.NBP, comp.PF_SG)
                    Case "Lee-Kesler (1976)"
                        tc = PropertyMethods.Tc_LeeKesler(comp.NBP, comp.PF_SG)
                    Case "Farah (2006)"
                        tc = PropertyMethods.Tc_Farah(comp.PF_vA, comp.PF_vB, comp.NBP, comp.PF_SG)
                End Select
            Else
                tc = tc.ToString().ToDoubleFromCurrent().ConvertToSI(cbTCUnits.SelectedItem)
            End If

            'Pc
            If pc Is Nothing Then
                Select Case Me.ComboBoxPC.SelectedItem.ToString
                    Case "Riazi-Daubert (1985)"
                        pc = PropertyMethods.Pc_RiaziDaubert(comp.NBP, comp.PF_SG)
                    Case "Riazi (2005)"
                        pc = PropertyMethods.Pc_Riazi(comp.NBP, comp.PF_SG)
                    Case "Lee-Kesler (1976)"
                        pc = PropertyMethods.Pc_LeeKesler(comp.NBP, comp.PF_SG)
                    Case "Farah (2006)"
                        pc = PropertyMethods.Pc_Farah(comp.PF_vA, comp.PF_vB, comp.NBP, comp.PF_SG)
                End Select
            Else
                pc = pc.ToString().ToDoubleFromCurrent().ConvertToSI(cbPCUnits.SelectedItem)
            End If

            'Af
            If af Is Nothing Then
                Select Case Me.ComboBoxAF.SelectedItem.ToString
                    Case "Lee-Kesler (1976)"
                        af = PropertyMethods.AcentricFactor_LeeKesler(tc, pc, tb)
                    Case "Korsten (2000)"
                        af = PropertyMethods.AcentricFactor_Korsten(tc, pc, tb)
                End Select
            End If

            comp.Critical_Temperature = tc
            comp.Critical_Pressure = pc
            comp.Acentric_Factor = af

            comp.PF_Watson_K = (1.8 * comp.NBP.GetValueOrDefault) ^ (1 / 3) / comp.PF_SG.GetValueOrDefault
            comp.Critical_Compressibility = prop.Zc1(comp.Acentric_Factor)
            comp.Critical_Volume = 8314 * comp.Critical_Compressibility * comp.Critical_Temperature / comp.Critical_Pressure
            comp.Z_Rackett = prop.Zc1(comp.Acentric_Factor)
            If comp.Z_Rackett < 0 Then comp.Z_Rackett = 0.2

            Dim tmp = prop2.calculate_Hf_Sf(comp.PF_SG, comp.Molar_Weight, comp.NBP)
            comp.Formula = "C" & CDbl(tmp(2)).ToString("N2") & "H" & CDbl(tmp(3)).ToString("N2")

            comp.IG_Enthalpy_of_Formation_25C = tmp(0)
            comp.IG_Entropy_of_Formation_25C = tmp(1)
            comp.IG_Gibbs_Energy_of_Formation_25C = tmp(0) - 298.15 * tmp(1)

            Dim methods2 As New Thermodynamics.PropertyPackages.Auxiliary.PROPS
            Dim methods As New Utilities.Hypos.Methods.HYP

            comp.HVap_A = methods.DHvb_Vetere(comp.Critical_Temperature, comp.Critical_Pressure, comp.Normal_Boiling_Point) / comp.Molar_Weight

            comp.Chao_Seader_Acentricity = comp.Acentric_Factor
            comp.Chao_Seader_Solubility_Parameter = ((comp.HVap_A * comp.Molar_Weight - 8.314 * comp.Normal_Boiling_Point) * 238.846 * prop.liq_dens_rackett(comp.Normal_Boiling_Point, comp.Critical_Temperature, comp.Critical_Pressure, comp.Acentric_Factor, comp.Molar_Weight) / comp.Molar_Weight / 1000000.0) ^ 0.5
            comp.Chao_Seader_Liquid_Molar_Volume = 1 / prop.liq_dens_rackett(comp.Normal_Boiling_Point, comp.Critical_Temperature, comp.Critical_Pressure, comp.Acentric_Factor, comp.Molar_Weight) * comp.Molar_Weight / 1000 * 1000000.0

            comp.ID = New Random(i).Next(1000000)

            Dim cstyle As New unvell.ReoGrid.WorksheetRangeStyle() With {
                                       .Flag = unvell.ReoGrid.PlainStyleFlag.TextColor,
                                       .TextColor = unvell.ReoGrid.Graphics.SolidColor.Blue}

            If mwflag Then
                sheet.Cells(i, 1).Data = mw
                sheet.SetRangeStyles(i, 1, 1, 1, cstyle)
            End If
            If tbflag Then
                sheet.Cells(i, 2).Data = Convert.ToDouble(tb).ConvertFromSI(cbNBPUnits.SelectedItem)
                sheet.SetRangeStyles(i, 2, 1, 1, cstyle)
            End If
            If sgflag Then
                sheet.Cells(i, 3).Data = sg
                sheet.SetRangeStyles(i, 3, 1, 1, cstyle)
            End If
            If tcflag Then
                sheet.Cells(i, 4).Data = Convert.ToDouble(tc).ConvertFromSI(cbTCUnits.SelectedItem)
                sheet.SetRangeStyles(i, 4, 1, 1, cstyle)
            End If
            If pcflag Then
                sheet.Cells(i, 5).Data = Convert.ToDouble(pc).ConvertFromSI(cbPCUnits.SelectedItem)
                sheet.SetRangeStyles(i, 5, 1, 1, cstyle)
            End If
            If afflag Then
                sheet.Cells(i, 6).Data = af
                sheet.SetRangeStyles(i, 6, 1, 1, cstyle)
            End If

            compounds.Add(comp)

        Next

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles btnExporttoXML.Click

        Try

            EstimateProps()

            Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

            Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
                                            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("XML File", "*.xml")})

            If handler IsNot Nothing Then
                Using stream As New MemoryStream()
                    Databases.UserDB.AddCompounds(compounds.ToArray(), stream, True)
                    stream.Position = 0
                    Using writer As New StreamWriter(stream) With {.AutoFlush = True}
                        handler.Write(stream)
                    End Using
                    MessageBox.Show(DWSIM.App.GetLocalString("FileSaved"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Information)
                End Using
            End If

        Catch ex As Exception

            MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)

        End Try

    End Sub

    Private Sub btnExport_Click(sender As Object, e As EventArgs) Handles btnExport.Click

        Try

            EstimateProps()

            For Each comp In compounds

                Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

                filePickerForm.SuggestedFilename = comp.Name

                Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
                New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON File", "*.json")})

                If handler IsNot Nothing Then
                    Using stream As New IO.MemoryStream()
                        Using writer As New StreamWriter(stream) With {.AutoFlush = True}
                            Dim jsondata = Newtonsoft.Json.JsonConvert.SerializeObject(comp, Newtonsoft.Json.Formatting.Indented)
                            writer.Write(jsondata)
                            handler.Write(stream)
                        End Using
                    End Using
                End If

            Next

        Catch ex As Exception

            MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)

        End Try

    End Sub

    Private Sub btnAdd_Click(sender As Object, e As EventArgs) Handles btnAdd.Click

        Try

            EstimateProps()

            Dim frm = DirectCast(Flowsheet, FormFlowsheet)

            If Not frm.FrmStSim1.initialized Then frm.FrmStSim1.Init()

            For Each comp In compounds
                frm.Options.NotSelectedComponents.Add(comp.Name, comp)
                frm.FrmStSim1.AddCompToGrid(comp)
                frm.FrmStSim1.AddCompToSimulation(comp.Name)
            Next

            Close()

        Catch ex As Exception

            MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)

        End Try

    End Sub

    Private Sub btnClose_Click(sender As Object, e As EventArgs) Handles btnClose.Click

        Close()

    End Sub

    Private Sub btnViewComp_Click(sender As Object, e As EventArgs) Handles btnViewComp.Click

        If cbViewComp.SelectedIndex >= 0 Then
            Dim f As New FormPureComp() With {.Flowsheet = Flowsheet, .Added = False, .MyCompound = compounds(cbViewComp.SelectedIndex)}
            f.ShowDialog()
        End If

    End Sub

    Private Sub cbViewComp_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbViewComp.SelectedIndexChanged

        If cbViewComp.SelectedIndex >= 0 Then btnViewComp.Enabled = True Else btnViewComp.Enabled = False

    End Sub

End Class