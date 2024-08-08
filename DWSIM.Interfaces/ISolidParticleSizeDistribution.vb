Public Interface ISolidParticleData

    Property UniqueID As String

    Property Name As String

    Property Description As String

    Property Distributions As Dictionary(Of String, String)

    Property InternalDistributions As Dictionary(Of String, ISolidParticleSizeDistribution)

    Property Calculated As Boolean

    Function Clone() As ISolidParticleData

End Interface

Public Interface ISolidParticleSizeDistribution

    Property UniqueID As String

    Property Name As String

    Property Description As String

    Property Curves As List(Of ISolidShapeCurve)

    Function Clone() As ISolidParticleSizeDistribution

End Interface

Public Interface ISolidShapeCurve

    Property UniqueID As String

    Property Name As String

    Property Description As String

    Property Shape As String

    Property Data As List(Of ISolidParticleSize)

    Function GetMeanDiameter() As Double

    Function GetDiameterStdDev() As Double

    Function GetAverageDiameter() As Double

    Function GetValue(x As Double) As Double

    Function Clone() As ISolidShapeCurve

End Interface

Public Interface ISolidParticleSize

    ''' <summary>
    ''' Size in meters
    ''' </summary>
    ''' <returns></returns>
    Property Size As Double

    Property MassFraction As Double

End Interface


