Public Class FormPropertyPackageInfo

    Public PP As PropertyPackages.PropertyPackage

    Private Sub FormPropertyPackageInfo_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ChangeDefaultFont()

        Dim name = PP.DisplayName

        Dim desc = PP.DisplayDescription

        If name = "" Then name = PP.ComponentName

        Label1.Text = name
        Label2.Text = desc

        With DataGridView1

            .Rows.Clear()

            .Rows.Add("Vapor Phase Properties", "")
            .Rows.Add("Fugacity", PP.PropertyMethodsInfo.Vapor_Fugacity)
            .Rows.Add("Enthalpy/Entropy/Cp", PP.PropertyMethodsInfo.Vapor_Enthalpy_Entropy_CpCv)
            .Rows.Add("Density", PP.PropertyMethodsInfo.Vapor_Density)
            .Rows.Add("Viscosity", PP.PropertyMethodsInfo.Vapor_Viscosity)
            .Rows.Add("Thermal Conductivity", PP.PropertyMethodsInfo.Vapor_Thermal_Conductivity)
            .Rows.Add("Liquid Phase Properties", "")
            .Rows.Add("Fugacity", PP.PropertyMethodsInfo.Liquid_Fugacity)
            .Rows.Add("Enthalpy/Entropy/Cp", PP.PropertyMethodsInfo.Liquid_Enthalpy_Entropy_CpCv)
            .Rows.Add("Density", PP.PropertyMethodsInfo.Liquid_Density)
            .Rows.Add("Viscosity", PP.PropertyMethodsInfo.Liquid_Viscosity)
            .Rows.Add("Thermal Conductivity", PP.PropertyMethodsInfo.Liquid_ThermalConductivity)
            .Rows.Add("Surface Tension", PP.PropertyMethodsInfo.SurfaceTension)
            .Rows.Add("Solid Phase Properties", "")
            .Rows.Add("Density", PP.PropertyMethodsInfo.Solid_Density)
            .Rows.Add("Enthalpy/Entropy/Cp", PP.PropertyMethodsInfo.Solid_Enthalpy_Entropy_CpCv)

            .Rows(0).Cells(0).Style.BackColor = Color.LightGray
            .Rows(0).Cells(1).Style.BackColor = Color.LightGray

            .Rows(6).Cells(0).Style.BackColor = Color.LightGray
            .Rows(6).Cells(1).Style.BackColor = Color.LightGray

            .Rows(13).Cells(0).Style.BackColor = Color.LightGray
            .Rows(13).Cells(1).Style.BackColor = Color.LightGray

            .ClearSelection()

        End With

        FormMain.TranslateFormFunction?.Invoke(Me)

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Close()

    End Sub

End Class