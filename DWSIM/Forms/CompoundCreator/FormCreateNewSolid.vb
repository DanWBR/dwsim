Imports System.IO
Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker
Imports DWSIM.Thermodynamics.BaseClasses

Public Class FormCreateNewSolid
    Private Sub FormCreateNewSolid_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Dim cmp As New ConstantProperties

        cmp.IsSolid = True
        cmp.TemperatureOfFusion = 5000
        cmp.Normal_Boiling_Point = 7000
        cmp.NBP = 7000
        cmp.Critical_Temperature = 9000
        cmp.Critical_Pressure = 10000000000.0
        cmp.Acentric_Factor = 0.5

        cmp.VaporPressureEquation = "2"
        cmp.Vapor_Pressure_Constant_A = 0.0001
        cmp.Vapor_Pressure_Constant_B = 0.0000001

        cmp.SolidHeatCapacityEquation = "1"

        cmp.Name = TextBoxName.Text
        cmp.Formula = TextBoxFormula.Text
        cmp.Molar_Weight = Convert.ToDouble(TextBoxMW.Text)
        cmp.SolidDensityAtTs = Convert.ToDouble(TextBoxDensity.Text)
        cmp.Solid_Heat_Capacity_Const_A = Convert.ToDouble(TextBoxCp.Text) * 1000.0 * cmp.Molar_Weight
        cmp.IG_Enthalpy_of_Formation_25C = Convert.ToDouble(TextBoxDHF.Text)
        cmp.IG_Gibbs_Energy_of_Formation_25C = Convert.ToDouble(TextBoxDGF.Text)

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON File", "*.json")})

        If handler IsNot Nothing Then
            Try
                cmp.ID = New Random().Next()
                cmp.OriginalDB = "User"
                cmp.CurrentDB = "User"
                Dim jsondata = Newtonsoft.Json.JsonConvert.SerializeObject(cmp, Newtonsoft.Json.Formatting.Indented)
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

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Close()
    End Sub
End Class