Imports DWSIM.ExtensionMethods
Imports DWSIM.Interfaces

Public Class Form1

    Public fsheet As IFlowsheet

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        lblUnits.Text = fsheet.FlowsheetOptions.SelectedUnitSystem.enthalpy
        lblUnits2.Text = fsheet.FlowsheetOptions.SelectedUnitSystem.molar_enthalpy

        Dim streams = fsheet.SimulationObjects.Values.Where(Function(x) TypeOf x Is IMaterialStream).Select(Function(x2) x2.GraphicObject.Tag).ToArray()

        cbStream.Items.Clear()
        cbStream.Items.Add("")
        cbStream.Items.AddRange(streams)


    End Sub

    Private Sub cbStream_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbStream.SelectedIndexChanged


        Dim stream = fsheet.GetFlowsheetSimulationObject(cbStream.SelectedItem.ToString)

        If stream IsNot Nothing Then

            Dim ms = DirectCast(stream, IMaterialStream)
            Dim pp = DirectCast(ms.GetPropertyPackageObject(), IPropertyPackage)

            Dim compounds = fsheet.SelectedCompounds.Values.ToList()
            Dim molar_composition = ms.GetOverallComposition()
            Dim mass_composition = ms.GetOverallComposition()
            pp.CurrentMaterialStream = ms
            Dim mw = pp.AUX_MMM(Enums.PhaseLabel.Mixture)

            Dim lhv_mass, lhv_molar, t1, t2 As Double

            'CxHyNzOn (std.) + O2 (g, xs.) → x CO2 (g) + ​y⁄2 H2O (l) + ​z⁄2 N2 (g)

            Dim i As Integer = 0
            Dim n As Integer = compounds.Count - 1
            For i = 0 To n
                If molar_composition(i) > 0.0 Then
                    If compounds(i).StandardHeatOfCombustion_LHV = 0.0 Then
                        stream.GetFlowsheet().ShowMessage(String.Format("Warning: {0} Standard Net Heat of Combustion = 0", compounds(i).Name), IFlowsheet.MessageType.Warning)
                    End If
                    mass_composition(i) = molar_composition(i) * compounds(i).Molar_Weight / mw
                    t1 = molar_composition(i) * compounds(i).StandardHeatOfCombustion_LHV * compounds(i).Molar_Weight
                    t2 = mass_composition(i) * compounds(i).StandardHeatOfCombustion_LHV
                    lhv_molar += t1
                    lhv_mass += t2
                End If
            Next

            tbMassLHV.Text = -lhv_mass.ConvertFromSI(fsheet.FlowsheetOptions.SelectedUnitSystem.enthalpy).ToString(fsheet.FlowsheetOptions.NumberFormat)
            tbMolarLHV.Text = -lhv_molar.ConvertFromSI(fsheet.FlowsheetOptions.SelectedUnitSystem.molar_enthalpy).ToString(fsheet.FlowsheetOptions.NumberFormat)

        End If

    End Sub

End Class