Public Interface ISolidParticleData

    Property UniqueID As String

    Property Name As String

    Property Description As String

    Property CompoundCurves As Dictionary(Of String, ISolidParticleSizeDistribution)

    Function Clone() As ISolidParticleData

End Interface

Public Interface ISolidParticleSizeDistribution

    Property UniqueID As String

    Property Name As String

    Property Description As String

    Property Curve As ISolidShapeCurve

End Interface

Public Interface ISolidShapeCurve

    Property UniqueID As String

    Property Name As String

    Property Description As String

    Property Data As List(Of ISolidParticleSize)

End Interface

Public Interface ISolidParticleSize

    ''' <summary>
    ''' Size in meters
    ''' </summary>
    ''' <returns></returns>
    Property Size As Double

    Property MassFraction As Double

End Interface


