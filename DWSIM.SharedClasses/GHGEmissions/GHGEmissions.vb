Public Class GHGEmitter

    Implements Interfaces.IGHGEmitter, ICustomXMLSerialization

    Private Const MethaneCO2eq As Double = 28.0

    Private Const NitrousOxideCO2eq As Double = 256.0

    Public Property Active As Boolean = True Implements IGHGEmitter.Active

    Public Property GHGEmissionMassFlow As Double Implements IGHGEmitter.GHGEmissionMassFlow

    Public Property GHGEmissionMolarFlow As Double Implements IGHGEmitter.GHGEmissionMolarFlow

    Public Property CO2eqEmissionMassFlow As Double Implements IGHGEmitter.CO2eqEmissionMassFlow

    Public Property CO2eqEmissionMolarFlow As Double Implements IGHGEmitter.CO2eqEmissionMolarFlow

    ''' <summary>
    ''' Emission Factor in Default SI Units ([kg/s]/kW)
    ''' </summary>
    ''' <returns></returns>
    Public Property GHGEmissionFactor As Double = Double.NaN Implements IGHGEmitter.GHGEmissionFactor

    Public Property GHGEmissionCompositionID As String = "" Implements IGHGEmitter.GHGEmissionCompositionID

    Public Property OwnerID As String = "" Implements IGHGEmitter.OwnerID

    <Xml.Serialization.XmlIgnore>
    Public Property Flowsheet As IFlowsheet Implements IGHGEmitter.Flowsheet

    Public Property EmissionFactorIsInCO2eq As Boolean = False Implements IGHGEmitter.EmissionFactorIsInCO2eq

    Public Property UsesUserDefinedEnergyConsumption As Boolean = False Implements IGHGEmitter.UsesUserDefinedEnergyConsumption

    Public Property UserDefinedEnergyConsumption As Double Implements IGHGEmitter.UserDefinedEnergyConsumption

    Public Sub Update() Implements IGHGEmitter.Update

        CO2eqEmissionMassFlow = 0.0
        CO2eqEmissionMolarFlow = 0.0

        GHGEmissionMassFlow = 0.0
        GHGEmissionMolarFlow = 0.0

        If OwnerID = "" Then Exit Sub
        If Flowsheet Is Nothing Then Exit Sub

        If Not Flowsheet.SimulationObjects.ContainsKey(OwnerID) Then Exit Sub

        Dim owner = Flowsheet.SimulationObjects(OwnerID)

        If TypeOf owner Is IMaterialStream Then

            Dim ch4, n2o, co2, h2o As Double
            Dim ch4w, n2ow, co2w, h2ow As Double

            Dim stream = DirectCast(owner, IMaterialStream)

            Dim w = stream.GetMassFlow()
            Dim m = stream.GetMolarFlow()

            If stream.Phases(0).Compounds.ContainsKey("Methane") Then
                ch4 = stream.Phases(0).Compounds("Methane").MoleFraction.GetValueOrDefault()
                ch4w = stream.Phases(0).Compounds("Methane").MassFraction.GetValueOrDefault()
            End If
            If stream.Phases(0).Compounds.ContainsKey("Nitrous oxide") Then
                n2o = stream.Phases(0).Compounds("Nitrous oxide").MoleFraction.GetValueOrDefault()
                n2ow = stream.Phases(0).Compounds("Nitrous oxide").MassFraction.GetValueOrDefault()
            End If
            If stream.Phases(0).Compounds.ContainsKey("Carbon dioxide") Then
                co2 = stream.Phases(0).Compounds("Carbon dioxide").MoleFraction.GetValueOrDefault()
                co2w = stream.Phases(0).Compounds("Carbon dioxide").MassFraction.GetValueOrDefault()
            End If
            If stream.Phases(0).Compounds.ContainsKey("Water") Then
                h2o = stream.Phases(0).Compounds("Water").MoleFraction.GetValueOrDefault()
                h2ow = stream.Phases(0).Compounds("Water").MassFraction.GetValueOrDefault()
            End If

            If EmissionFactorIsInCO2eq Then

                Dim t1 = ch4 / MethaneCO2eq
                Dim t2 = n2o / NitrousOxideCO2eq
                Dim t1w = ch4w / MethaneCO2eq
                Dim t2w = n2ow / NitrousOxideCO2eq

                CO2eqEmissionMassFlow = w * (t1w + t2w + co2w + h2ow) 'kg/s
                CO2eqEmissionMolarFlow = m * (t1 + t2 + co2 + h2o) 'mol/s

                GHGEmissionMassFlow = w * (ch4w + n2ow + co2w + h2ow) 'kg/s
                GHGEmissionMolarFlow = m * (ch4 + n2o + co2 + h2o) 'mol/s

            Else

                Dim t1 = ch4 * MethaneCO2eq
                Dim t2 = n2o * NitrousOxideCO2eq
                Dim t1w = ch4w * MethaneCO2eq
                Dim t2w = n2ow * NitrousOxideCO2eq

                GHGEmissionMassFlow = w * (t1w + t2w + co2w + h2ow) 'kg/s
                GHGEmissionMolarFlow = m * (t1 + t2 + co2 + h2o) 'mol/s

                CO2eqEmissionMassFlow = w * (ch4w + n2ow + co2w + h2ow) 'kg/s
                CO2eqEmissionMolarFlow = m * (ch4 + n2o + co2 + h2o) 'mol/s

            End If

        Else

            Dim ec As Double = UserDefinedEnergyConsumption 'kW

            If Not UsesUserDefinedEnergyConsumption Then ec = owner.GetEnergyConsumption() 'kW

            If Not Flowsheet.GHGEmissionCompositions.ContainsKey(GHGEmissionCompositionID) Then Exit Sub

            Dim ghgas = Flowsheet.GHGEmissionCompositions(GHGEmissionCompositionID)

            Dim t1 = ghgas.Methane / MethaneCO2eq
            Dim t2 = ghgas.NitrousOxide / NitrousOxideCO2eq
            Dim t3 = ghgas.CarbonDioxide
            Dim t4 = ghgas.Water

            Dim corrected = GHGEmissionFactor * (t1 + t2 + t3 + t4) '[kg/s]/kW

            If EmissionFactorIsInCO2eq Then

                CO2eqEmissionMassFlow = GHGEmissionFactor * ec 'kg/s
                CO2eqEmissionMolarFlow = GHGEmissionFactor * ec / ghgas.GetMolecularWeight() * 1000.0 'mol/s

                GHGEmissionMassFlow = corrected * ec 'kg/s
                GHGEmissionMolarFlow = corrected * ec / ghgas.GetMolecularWeight() * 1000.0 'mol/s

            Else

                GHGEmissionMassFlow = GHGEmissionFactor * ec 'kg/s
                GHGEmissionMolarFlow = GHGEmissionFactor * ec / ghgas.GetMolecularWeight() * 1000.0 'mol/s

                CO2eqEmissionMassFlow = corrected * ec 'kg/s
                CO2eqEmissionMolarFlow = corrected * ec / ghgas.GetMolecularWeight() * 1000.0 'mol/s

            End If

        End If

    End Sub

    Public Function GetCompoundMassEmission(compound As String, units As String) As Double Implements IGHGEmitter.GetCompoundMassEmission

        Dim owner = Flowsheet.SimulationObjects(OwnerID)

        If TypeOf owner Is IMaterialStream Then

            Dim stream = DirectCast(owner, IMaterialStream)

            Dim w = stream.GetMassFlow()

            If stream.Phases(0).Compounds.ContainsKey("Methane") Then
                Return stream.Phases(0).Compounds("Methane").MassFraction.GetValueOrDefault() * w
            End If
            If stream.Phases(0).Compounds.ContainsKey("Nitrous oxide") Then
                Return stream.Phases(0).Compounds("Nitrous oxide").MassFraction.GetValueOrDefault() * w
            End If
            If stream.Phases(0).Compounds.ContainsKey("Carbon dioxide") Then
                Return stream.Phases(0).Compounds("Carbon dioxide").MassFraction.GetValueOrDefault() * w
            End If
            If stream.Phases(0).Compounds.ContainsKey("Water") Then
                Return stream.Phases(0).Compounds("Water").MassFraction.GetValueOrDefault() * w
            End If

        Else

            Dim ghgas = Flowsheet.GHGEmissionCompositions(GHGEmissionCompositionID)

            Select Case compound
                Case "Carbon dioxide"
                    Return ghgas.CarbonDioxide * GHGEmissionMolarFlow / 1000.0 * 44.01
                Case "Nitrous oxide"
                    Return ghgas.NitrousOxide * GHGEmissionMolarFlow / 1000.0 * 44.013
                Case "Methane"
                    Return ghgas.Methane * GHGEmissionMolarFlow / 1000.0 * 16.04
                Case "Water"
                    Return ghgas.Water * GHGEmissionMolarFlow / 1000.0 * 18
            End Select

        End If

        Return 0.0

    End Function

    Public Function GetCompoundMolarEmission(compound As String, units As String) As Double Implements IGHGEmitter.GetCompoundMolarEmission

        Dim owner = Flowsheet.SimulationObjects(OwnerID)

        If TypeOf owner Is IMaterialStream Then

            Dim stream = DirectCast(owner, IMaterialStream)

            Dim m = stream.GetMolarFlow()

            If stream.Phases(0).Compounds.ContainsKey("Methane") Then
                Return stream.Phases(0).Compounds("Methane").MoleFraction.GetValueOrDefault() * m
            End If
            If stream.Phases(0).Compounds.ContainsKey("Nitrous oxide") Then
                Return stream.Phases(0).Compounds("Nitrous oxide").MoleFraction.GetValueOrDefault() * m
            End If
            If stream.Phases(0).Compounds.ContainsKey("Carbon dioxide") Then
                Return stream.Phases(0).Compounds("Carbon dioxide").MoleFraction.GetValueOrDefault() * m
            End If
            If stream.Phases(0).Compounds.ContainsKey("Water") Then
                Return stream.Phases(0).Compounds("Water").MoleFraction.GetValueOrDefault() * m
            End If

        Else

            Dim ghgas = Flowsheet.GHGEmissionCompositions(GHGEmissionCompositionID)

            Select Case compound
                Case "Carbon dioxide"
                    Return ghgas.CarbonDioxide * GHGEmissionMolarFlow
                Case "Nitrous oxide"
                    Return ghgas.NitrousOxide * GHGEmissionMolarFlow
                Case "Methane"
                    Return ghgas.Methane * GHGEmissionMolarFlow
                Case "Water"
                    Return ghgas.Water * GHGEmissionMolarFlow
            End Select

        End If

        Return 0.0

    End Function

    Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData
        Return XMLSerializer.XMLSerializer.Serialize(Me)
    End Function

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData
        Return XMLSerializer.XMLSerializer.Deserialize(Me, data)
    End Function

End Class

Public Class GHGEmissionComposition

    Implements Interfaces.IGHGComposition, ICustomXMLSerialization

    Public Property ID As String = "" Implements IGHGComposition.ID

    Public Property Name As String = "" Implements IGHGComposition.Name

    Public Property CarbonDioxide As Double Implements IGHGComposition.CarbonDioxide

    Public Property Methane As Double Implements IGHGComposition.Methane

    Public Property NitrousOxide As Double Implements IGHGComposition.NitrousOxide

    Public Property Water As Double Implements IGHGComposition.Water

    Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData
        Return XMLSerializer.XMLSerializer.Serialize(Me)
    End Function

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData
        Return XMLSerializer.XMLSerializer.Deserialize(Me, data)
    End Function

    Public Function GetMolecularWeight() As Double Implements IGHGComposition.GetMolecularWeight

        Return CarbonDioxide * 44.01 + Methane * 16.04 + Water * 18.0 + NitrousOxide * 44.013

    End Function

End Class

Public Class GHGEmissionsSummary

    Implements IGHGEmissionsSummary, ICustomXMLSerialization

    Public Property TotalGHGMassEmission As Double Implements IGHGEmissionsSummary.TotalGHGMassEmission

    Public Property TotalGHGMolarEmission As Double Implements IGHGEmissionsSummary.TotalGHGMolarEmission

    Public Property TotalMethaneMassEmission As Double Implements IGHGEmissionsSummary.TotalMethaneMassEmission

    Public Property TotalMethaneMolarEmission As Double Implements IGHGEmissionsSummary.TotalMethaneMolarEmission

    Public Property TotalCarbonDioxideMassEmission As Double Implements IGHGEmissionsSummary.TotalCarbonDioxideMassEmission

    Public Property TotalCarbonDioxideMolarEmission As Double Implements IGHGEmissionsSummary.TotalCarbonDioxideMolarEmission

    Public Property TotalNitrousOxideMassEmission As Double Implements IGHGEmissionsSummary.TotalNitrousOxideMassEmission

    Public Property TotalNitrousOxideMolarEmission As Double Implements IGHGEmissionsSummary.TotalNitrousOxideMolarEmission

    Public Property TotalWaterMassEmission As Double Implements IGHGEmissionsSummary.TotalWaterMassEmission

    Public Property TotalWaterMolarEmission As Double Implements IGHGEmissionsSummary.TotalWaterMolarEmission

    Public Property TotalCO2eqMassEmission As Double Implements IGHGEmissionsSummary.TotalCO2eqMassEmission

    Public Property TotalCO2eqMolarEmission As Double Implements IGHGEmissionsSummary.TotalCO2eqMolarEmission

    Public Property UserDefinedGHGMassEmission As Double Implements IGHGEmissionsSummary.UserDefinedGHGMassEmission

    Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData
        Return XMLSerializer.XMLSerializer.Serialize(Me)
    End Function

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData
        Return XMLSerializer.XMLSerializer.Deserialize(Me, data)
    End Function

End Class
