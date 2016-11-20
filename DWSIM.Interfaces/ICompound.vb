Public Interface ICompound

    Property ConstantProperties As ICompoundConstantProperties

    Property lnKvalue() As Double

    Property Kvalue() As Double

    Property PetroleumFraction() As Boolean

    Property MoleFraction() As Nullable(Of Double)

    Property MassFraction() As Nullable(Of Double)

    Property Molarity() As Nullable(Of Double)

    Property Molality() As Nullable(Of Double)

    Property MolarFlow() As Nullable(Of Double)

    Property MassFlow() As Nullable(Of Double)

    Property FugacityCoeff() As Nullable(Of Double)

    Property ActivityCoeff() As Nullable(Of Double)

    Property PartialVolume() As Nullable(Of Double)

    Property PartialPressure() As Nullable(Of Double)

    Property VolumetricFlow() As Nullable(Of Double)

    Property VolumetricFraction() As Nullable(Of Double)

    Property Name() As String

    Property DiffusionCoefficient As Nullable(Of Double)

End Interface
