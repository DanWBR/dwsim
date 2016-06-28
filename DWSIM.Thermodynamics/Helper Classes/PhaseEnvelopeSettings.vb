Namespace PropertyPackages
    <Serializable> Public Class PhaseEnvelopeOptions

        Implements Interfaces.IPhaseEnvelopeOptions, ICloneable

        Public Property BubbleCurveDeltaP As Double = 101325.0 Implements Interfaces.IPhaseEnvelopeOptions.BubbleCurveDeltaP

        Public Property BubbleCurveDeltaT As Double = 5.0 Implements Interfaces.IPhaseEnvelopeOptions.BubbleCurveDeltaT

        Public Property BubbleCurveInitialFlash As String = "PVF" Implements Interfaces.IPhaseEnvelopeOptions.BubbleCurveInitialFlash

        Public Property BubbleCurveInitialPressure As Double = 101325.0# Implements Interfaces.IPhaseEnvelopeOptions.BubbleCurveInitialPressure

        Public Property BubbleCurveInitialTemperature As Double = 200.0# Implements Interfaces.IPhaseEnvelopeOptions.BubbleCurveInitialTemperature

        Public Property BubbleCurveMaximumPoints As Integer = 300 Implements Interfaces.IPhaseEnvelopeOptions.BubbleCurveMaximumPoints

        Public Property CheckLiquidInstability As Boolean Implements Interfaces.IPhaseEnvelopeOptions.CheckLiquidInstability

        Public Property DewCurveDeltaP As Double = 101325.0 Implements Interfaces.IPhaseEnvelopeOptions.DewCurveDeltaP

        Public Property DewCurveDeltaT As Double = 5.0 Implements Interfaces.IPhaseEnvelopeOptions.DewCurveDeltaT

        Public Property DewCurveInitialFlash As String = "PVF" Implements Interfaces.IPhaseEnvelopeOptions.DewCurveInitialFlash

        Public Property DewCurveInitialPressure As Double = 101325.0# Implements Interfaces.IPhaseEnvelopeOptions.DewCurveInitialPressure

        Public Property DewCurveInitialTemperature As Double = 250.0# Implements Interfaces.IPhaseEnvelopeOptions.DewCurveInitialTemperature

        Public Property DewCurveMaximumPoints As Integer = 300 Implements Interfaces.IPhaseEnvelopeOptions.DewCurveMaximumPoints

        Public Property Hydrate As Boolean = False Implements Interfaces.IPhaseEnvelopeOptions.Hydrate

        Public Property HydrateModel As Integer = 0 Implements Interfaces.IPhaseEnvelopeOptions.HydrateModel

        Public Property HydrateVaporOnly As Boolean = False Implements Interfaces.IPhaseEnvelopeOptions.HydrateVaporOnly

        Public Property OperatingPoint As Boolean = True Implements Interfaces.IPhaseEnvelopeOptions.OperatingPoint

        Public Property PhaseIdentificationCurve As Boolean = False Implements Interfaces.IPhaseEnvelopeOptions.PhaseIdentificationCurve

        Public Property QualityLine As Boolean = False Implements Interfaces.IPhaseEnvelopeOptions.QualityLine

        Public Property QualityValue As Double = 0.5# Implements Interfaces.IPhaseEnvelopeOptions.QualityValue

        Public Property StabilityCurve As Boolean = False Implements Interfaces.IPhaseEnvelopeOptions.StabilityCurve

        Public Property BubbleCurveMaximumTemperature As Double = 1000.0# Implements Interfaces.IPhaseEnvelopeOptions.BubbleCurveMaximumTemperature

        Public Property BubbleUseCustomParameters As Boolean = False Implements Interfaces.IPhaseEnvelopeOptions.BubbleUseCustomParameters

        Public Property DewCurveMaximumTemperature As Double = 1000.0# Implements Interfaces.IPhaseEnvelopeOptions.DewCurveMaximumTemperature

        Public Property DewUseCustomParameters As Boolean = False Implements Interfaces.IPhaseEnvelopeOptions.DewUseCustomParameters

        Public Function Clone() As Object Implements ICloneable.Clone
            Return Me.MemberwiseClone
        End Function

    End Class

End Namespace

