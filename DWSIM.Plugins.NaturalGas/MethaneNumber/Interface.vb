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

' EXCEL VBA interface to classes to estimate methane number and other gas composition properties

Public Class MethaneNumberInterface

    Public Function Calculate(C1 As Double, Optional C2 As Double = 0, Optional C3 As Double = 0, Optional IC4 As Double = 0, Optional NC4 As Double = 0, Optional IC5 As Double = 0, Optional NC5 As Double = 0, Optional C6 As Double = 0, Optional C7 As Double = 0, Optional C8 As Double = 0, Optional C9 As Double = 0, Optional N2 As Double = 0, Optional CO2 As Double = 0, Optional He As Double = 0, Optional CO As Double = 0, Optional H2 As Double = 0, Optional H2S As Double = 0) As MethaneNumber
        Dim oMNGEC As New MethaneNumber
        With oMNGEC
            .gctCompositionType = GasCompositionType.molePercent
            .dblMethane = C1 * 100
            .dblEthane = C2 * 100
            .dblPropane = C3 * 100
            .dblIsoButane = IC4 * 100
            .dblNormalButane = NC4 * 100
            .dblIsoPentane = IC5 * 100
            .dblNormalPentane = NC5 * 100
            .dblHexane = C6 * 100
            .dblHeptane = C7 * 100
            .dblOctane = C8 * 100
            .dblNonane = C9 * 100
            .dblNitrogen = N2 * 100
            .dblCarbonDioxide = CO2 * 100
            .dblHelium = He * 100
            .dblCarbonMonoxide = CO * 100
            .dblHydrogen = H2 * 100
            .dblHydrogenSulfide = H2S * 100
            .CalculateProperties()
        End With
        Return oMNGEC
    End Function

End Class
