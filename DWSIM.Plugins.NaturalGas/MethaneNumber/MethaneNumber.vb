' Class GasCombustionProperties
' Purpose: Calculate combustion properties of natural gas fuels including higher heating value,
' lower heating value, specific gravity, stoichiometric air/fuel ratio,and methane number.
' Developed by Gary Choquette, Optimized Technical Solutions, LLC
'
' Copyright 2014, Optimized Technical Solutions, LLC

'    This program is free software: you can redistribute it and/or modify
'    it under the terms of the GNU Lesser General Public License as published by
'    the Free Software Foundation, version 3 of the License.
'    http://opensource.org/licenses/GPL-3.0

'    This program is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU Lesser General Public License for more details.

Public Enum GasCompositionType
    moleFraction
    molePercent
End Enum

Public Enum GasCompositionErrors
    noErrors = 0
    compositionZero = 1
    compositionSumLow = 2
    compositionSumHigh = 4
    allCarbon = 8
    allHydrogen = 16
    allInerts = 32
End Enum

Public Class MethaneNumber

    Public gctCompositionType As GasCompositionType

    Public gceErrors As GasCompositionErrors

    Public strCompositionName As String
    Public strErrorNotes As String

    'normalized mole fractions
    Public dblH2 As Double
    Public dblH2S As Double
    Public dblCO As Double
    Public dblCO2 As Double
    Public dblN2 As Double
    Public dblO2 As Double
    Public dblHE As Double
    Public dblC1 As Double
    Public dblC2 As Double
    Public dblC3 As Double
    Public dblIC4 As Double
    Public dblNC4 As Double
    Public dblIC5 As Double
    Public dblNC5 As Double
    Public dblNEC5 As Double
    Public dblC6 As Double
    Public dblC7 As Double
    Public dblC8 As Double
    Public dblC9 As Double
    Public dblH2O As Double

    Private dblBasePressue As Double

    'raw values
    Public dblCarbonDioxide As Double
    Public dblCarbonMonoxide As Double
    Public dblEthane As Double
    Public dblNitrogen As Double
    Public dblHelium As Double
    Public dblHeptane As Double
    Public dblHexane As Double
    Public dblHydrogen As Double
    Public dblHydrogenSulfide As Double
    Public dblIsoButane As Double
    Public dblIsoPentane As Double
    Public dblMethane As Double
    Public dblNeoPentane As Double
    Public dblNonane As Double
    Public dblNormalButane As Double
    Public dblNormalPentane As Double
    Public dblOctane As Double
    Public dblOxygen As Double
    Public dblPropane As Double
    Public dblWater As Double

    Private isDirty As Boolean

    'Outputs
    Public dblInerts As Double
    Public dblHHV As Double 'lower heating value in btu/scf
    Public dblLHV As Double 'higher heating value in btu/scf
    Public dblStoichAFR As Double 'stoich AF by volume
    Public dblSPGrav As Double 'specific gravity
    Public dblMSTOICH As Double 'stoich af by mass
    Public dblMethaneNumber As Double
    Public dblH2SAdj As Double
    Public dblCO2Adj As Double
    Public dblWobbeIndex As Double 'wobbe index

    'locals
    Private dblMNRaw As Double
    Private carbonDioxideAirAdjustment As Double
    Private nitrogenAdjustmentFactor As Double

    'percent of combustables
    Private dblCMethane As Double
    Private dblCEthane As Double
    Private dblCPropane As Double
    Private dblCPropylene As Double
    Private dblCIsoButane As Double
    Private dblCNormalButane As Double
    Private dblCIsoPentane As Double
    Private dblCNormalPentane As Double
    Private dblCNeoPentane As Double
    Private dblCHexane As Double
    Private dblCHeptane As Double
    Private dblCOctane As Double
    Private dblCNonane As Double
    Private dblCCarbonMonoxide As Double
    Private dblCEthylene As Double
    Private dblCHydrogenSulfide As Double
    Private dblCHydrogen As Double

    Public ReadOnly Property ErrorFlags() As GasCompositionErrors
        Get
            If isDirty Then
                CalculateProperties()
            End If
            ErrorFlags = gceErrors
        End Get
    End Property

    Public ReadOnly Property ErrorNotes() As String
        Get
            If isDirty Then
                CalculateProperties()
            End If
            ErrorNotes = strErrorNotes
        End Get
    End Property

    Public Sub CalculateProperties()

        Dim compMult As Double
        Dim stoichFactor As Double
        Dim rawHHV As Double
        Dim rawLHV As Double
        Dim compressibility As Double 'gas compressibility

        Dim zfactor As Double
        'clear errors
        gceErrors = GasCompositionErrors.noErrors

        Dim rawComponentSum As Double
        rawComponentSum = dblMethane + dblEthane + dblPropane + dblIsoButane + dblNormalButane + dblIsoPentane + dblNormalPentane _
      + dblNeoPentane + dblHexane + dblHeptane + dblOctane + dblHeptane + dblCarbonMonoxide _
      + dblHydrogen + dblHydrogenSulfide + dblOxygen + dblCarbonDioxide + dblNitrogen + dblHelium + dblWater

        'normalize the components
        If rawComponentSum <= 0 Then
            gceErrors = GasCompositionErrors.compositionZero
        End If
        If Not gceErrors = GasCompositionErrors.compositionZero Then
            If gctCompositionType = GasCompositionType.moleFraction Then
                If rawComponentSum < 0.99999999 Then
                    gceErrors = GasCompositionErrors.compositionSumLow
                ElseIf rawComponentSum > 1.00000001 Then
                    gceErrors = GasCompositionErrors.compositionSumHigh
                End If
            Else
                If rawComponentSum < 99.999999 Then
                    gceErrors = GasCompositionErrors.compositionSumLow
                ElseIf rawComponentSum > 100.000001 Then
                    gceErrors = GasCompositionErrors.compositionSumHigh
                End If
            End If
            'normalize components
            compMult = 1 / rawComponentSum
            dblC1 = dblMethane * compMult
            dblC2 = dblEthane * compMult
            dblC3 = dblPropane * compMult
            dblIC4 = dblIsoButane * compMult
            dblNC4 = dblNormalButane * compMult
            dblIC5 = dblIsoPentane * compMult
            dblNC5 = dblNormalPentane * compMult
            dblNEC5 = dblNeoPentane * compMult
            dblC6 = dblHexane * compMult
            dblC7 = dblHeptane * compMult
            dblC8 = dblOctane * compMult
            dblC9 = dblNonane * compMult
            dblCO = dblCarbonMonoxide * compMult
            dblCO2 = dblCarbonDioxide * compMult
            dblH2 = dblHydrogen * compMult
            dblO2 = dblOxygen * compMult
            dblN2 = dblNitrogen * compMult
            dblHE = dblHelium * compMult
            dblH2S = dblHydrogenSulfide * compMult
            dblH2O = dblWater * compMult

            'calculate heating values
            rawLHV = dblC1 * 909.4 + dblC2 * 1618.7 + dblC3 * 2314.9 + dblIC4 * 3000.4 + dblNC4 * 3010.8 _
        + dblIC5 * 3699 + dblNC5 * 3703.9 + dblNEC5 * 3683 + dblC6 * 4403.9 + dblC7 * 5100.3 _
        + dblC8 * 5796.2 + dblC9 * 6493.6 + dblCO * 320.5 + dblH2 * 273.93 + dblH2S * 586.8
            rawHHV = dblC1 * 1010 + dblC2 * 1769.7 + dblC3 * 2516.1 + dblIC4 * 3251.9 + dblNC4 * 3262.3 _
        + dblIC5 * 4000.9 + dblNC5 * 4008.9 + dblNEC5 * 3985 + dblC6 * 4755.9 + dblC7 * 5502.5 _
        + dblC8 * 6248.9 + dblC9 * 6996.5 + dblCO * 320.5 + dblH2 * 324.2 + dblH2S * 637.1
            zfactor = dblC1 * 0.0116 + dblC2 * 0.0239 + dblC3 * 0.0344 + dblIC4 * 0.0458 + dblNC4 * 0.0478 _
        + dblIC5 * 0.0581 + dblNC5 * 0.0631 + dblNEC5 * 0 + dblC6 * 0.0802 + dblC7 * 0.0944 _
        + dblC8 * 0.1137 + dblC9 * 0.1331 + dblCO * 0.0053 + dblCO2 * 0.0197 + dblH2 * 0 + dblO2 * 0.0073 _
        + dblN2 * 0.0044 + dblH2S * 0.0253 + dblH2O * 0.0623
            compressibility = 1 - zfactor ^ 2 * dblBasePressue
            dblLHV = rawLHV / compressibility
            dblHHV = rawHHV / compressibility

            'calculate air/fuel ratios
            dblStoichAFR = dblC1 * 9.528 + dblC2 * 16.675 + dblC3 * 23.821 + (dblIC4 + dblNC4) * 30.967 _
        + (dblIC5 + dblNC5 + dblNEC5) * 38.114 + dblC6 * 45.26 + dblC7 * 52.406 + dblC8 * 59.552 _
        + dblC9 * 66.7 + dblCO * 2.382 + dblH2 * 2.382 + dblH2S * 7.146
            Dim oxygenAdjustment As Double
            Dim oxygenAdjustmentFactor As Double
            oxygenAdjustmentFactor = 1
            If Not (dblO2 > 0 And dblN2 > 0) Then
                stoichFactor = rawLHV / (dblStoichAFR + 1)
            Else
                If dblO2 * 3.764 > dblN2 Then
                    oxygenAdjustment = dblN2 * 1.26567481402763
                Else
                    oxygenAdjustment = dblO2 * 4.764
                End If
                oxygenAdjustmentFactor = (1 - oxygenAdjustment)
                If oxygenAdjustmentFactor <> 0 Then
                    stoichFactor = rawLHV / oxygenAdjustmentFactor / (dblStoichAFR / oxygenAdjustmentFactor + 1)
                Else
                    stoichFactor = 0
                End If
            End If
            'calculate specific gravity
            dblSPGrav = dblC1 * 0.55392 + dblC2 * 1.0382 + dblC3 * 1.5226 + dblIC4 * 2.0068 + dblNC4 * 2.0068 _
        + (dblIC5 + dblNC5 + dblNEC5) * 2.4912 + dblC6 * 2.9755 + dblC7 * 3.4598 + dblC8 * 3.9441 + dblC9 * 4.4284 _
        + dblCO * 0.96711 + dblCO2 * 1.5196 + dblH2 * 0.0696 + dblO2 * 1.1048 + dblN2 * 0.96723 + dblHE * 0.1382 _
        + dblH2S * 1.1767 + dblH2O * 0.62202
            'calculate Wobbe index
            dblWobbeIndex = dblHHV / dblSPGrav ^ 0.5
            'calculate mehane adjustment
            dblInerts = dblCO2 + dblO2 + dblN2 + dblHE + dblH2O
            Dim allCombustablesFraction As Double
            allCombustablesFraction = (1 - dblInerts)

            If allCombustablesFraction = 0 Then
                'all inerts
                gceErrors = gceErrors Or GasCompositionErrors.allInerts
                dblMethaneNumber = -9999
            Else
                Dim cC1 As Double
                Dim cC2 As Double
                Dim cC3 As Double
                Dim cC4 As Double
                Dim cC5 As Double
                Dim cC6 As Double
                Dim cC7 As Double
                Dim cC8 As Double
                Dim cC9 As Double
                Dim cCO As Double
                Dim cH2 As Double
                'normalize combustables, lump hydrogen components w/ methane
                cC1 = (dblC1 + dblH2 + dblH2S) / allCombustablesFraction
                cC2 = dblC2 / allCombustablesFraction
                cC3 = dblC3 / allCombustablesFraction
                cC4 = (dblIC4 + dblNC4) / allCombustablesFraction
                cC5 = (dblIC5 + dblNC5 + dblNEC5) / allCombustablesFraction
                cC6 = dblC6 / allCombustablesFraction
                cC7 = dblC7 / allCombustablesFraction
                cC8 = dblC8 / allCombustablesFraction
                cCO = dblCO / allCombustablesFraction
                cH2 = dblH2 / allCombustablesFraction
                Dim carbonSum As Double
                Dim hydrogenSum As Double
                carbonSum = cC1 + cC2 * 2 + cC3 * 3 + cC4 * 4 + cC5 * 5 + cC6 * 6 + cC7 * 7 + cC8 * 8 + cC9 * 9 + cCO
                hydrogenSum = cC1 * 4 + cC2 * 6 + cC3 * 8 + cC4 * 10 + cC5 * 12 + cC6 * 14 + cC7 * 16 + cC8 * 18 + cC9 * 20
                If carbonSum = 0 Then
                    gceErrors = gceErrors Or GasCompositionErrors.allHydrogen
                ElseIf hydrogenSum = 0 Then
                    gceErrors = gceErrors Or GasCompositionErrors.allCarbon
                Else
                    Dim hcrAdj As Double
                    Dim sgAdj As Double
                    hcrAdj = hydrogenSum / carbonSum
                    sgAdj = cC1 * 0.55392 + cC2 * 1.0382 + cC3 * 1.5226 + cC4 * 2.0068 + cC5 * 2.4912 + cC6 * 2.9755 _
            + cC7 * 3.4598 + cC8 * 3.9441 + cC9 * 4.4284 + cCO * 0.96711
                    dblMNRaw = 34.26085951 * hcrAdj + 0.000000002944058733 * sgAdj * (21.30716314 * hcrAdj ^ 2) ^ hcrAdj - 59.1854841
                End If

                CalcCO2Adjustment()
                CalcH2SAdjustment()
                dblMethaneNumber = dblMNRaw + dblCO2Adj + dblH2SAdj - cH2 * 100
            End If 'all inerts
        End If 'compositionzero
        'build error string
        If gceErrors = GasCompositionErrors.noErrors Then
            strErrorNotes = "No calculation errors or warnings."
        Else
            strErrorNotes = ""
            If (gceErrors And GasCompositionErrors.compositionZero) > 0 Then
                strErrorNotes = "Gas compositions are less than or equal to zero."
            End If
            If (gceErrors And GasCompositionErrors.compositionSumHigh) > 0 Then
                If gctCompositionType = GasCompositionType.moleFraction Then
                    strErrorNotes = "Gas compositions sum to more than 1.0; "
                Else
                    strErrorNotes = "Gas compositions sum to more than 100.0; "
                End If
            End If
            If (gceErrors And GasCompositionErrors.compositionSumLow) > 0 Then
                If gctCompositionType = GasCompositionType.moleFraction Then
                    strErrorNotes = "Gas compositions sum to less than 1.0; "
                Else
                    strErrorNotes = "Gas compositions sum to less than 100.0; "
                End If
            End If
            If (gceErrors And GasCompositionErrors.allCarbon) > 0 Then
                strErrorNotes = strErrorNotes & "Gas contains all carbon components; "
            End If
            If (gceErrors And GasCompositionErrors.allCarbon) > 0 Then
                strErrorNotes = strErrorNotes & "Gas contains all carbon components; "
            End If
            If (gceErrors And GasCompositionErrors.allCarbon) > 0 Then
                strErrorNotes = strErrorNotes & "Gas contains all hydrogen components; "
            End If
            If (gceErrors And GasCompositionErrors.allInerts) > 0 Then
                strErrorNotes = strErrorNotes & "Gas contains all inerts; "
            End If
        End If

        isDirty = False
    End Sub

    Private Sub CalcH2SAdjustment()
        If dblCHydrogenSulfide <> 0 Then
            dblH2SAdj = -14.83 * dblH2S - 1.048 * dblH2S ^ 0.002036
        Else
            dblH2SAdj = 0
        End If
    End Sub

    Private Sub CalcCO2Adjustment()
        Dim mnAdj As Double
        If dblCarbonDioxide <= 0 Then
            dblCO2Adj = 0
        Else
            If dblMNRaw < 70 Then
                mnAdj = 70
            Else
                mnAdj = dblMNRaw
            End If
            dblCO2Adj = 57.0952 * dblCO2 ^ 2 * mnAdj + 0.0000011611 * dblCO2 * mnAdj ^ 4 - 0.081104 - 45.2077 * dblCO2 - 2153.3903 * dblCO2 ^ 2 - 253.9675 * dblCO2 ^ 3 - 0.337730332431225 * dblCO2 ^ 2 * mnAdj ^ 2
        End If
    End Sub

    Private Sub Class_Initialize()
        dblBasePressue = 14.73
    End Sub


End Class
