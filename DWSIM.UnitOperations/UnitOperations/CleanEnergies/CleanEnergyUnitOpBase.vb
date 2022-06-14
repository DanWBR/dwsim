Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums

Namespace UnitOperations

    Public MustInherit Class CleanEnergyUnitOpBase

        Inherits DWSIM.UnitOperations.UnitOperations.UnitOpBaseClass

        Implements DWSIM.Interfaces.IExternalUnitOperation

        Public Property UseUserDefinedWeather As Boolean = False

        Public Overrides ReadOnly Property IsSource As Boolean = True

        Public Overrides Property ComponentName As String = GetDisplayName()

        Public Overrides Property ComponentDescription As String = GetDisplayDescription()

        Private ReadOnly Property IExternalUnitOperation_Name As String = GetDisplayName() Implements IExternalUnitOperation.Name

        Public MustOverride Property Prefix As String Implements IExternalUnitOperation.Prefix

        Public ReadOnly Property Description As String = GetDisplayDescription() Implements IExternalUnitOperation.Description

        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.CleanPowerSources

        Public Overrides ReadOnly Property MobileCompatible As Boolean = False

        Public MustOverride Function ReturnInstance(typename As String) As Object Implements IExternalUnitOperation.ReturnInstance

        Public MustOverride Sub Draw(g As Object) Implements IExternalUnitOperation.Draw

        Public MustOverride Sub CreateConnectors() Implements IExternalUnitOperation.CreateConnectors

        Public Sub New(ByVal Name As String, ByVal Description As String)

            MyBase.CreateNew()
            Me.ComponentName = Name
            Me.ComponentDescription = Description

        End Sub

        Public Sub New()

            MyBase.New()

        End Sub

        Public Overrides Sub PerformPostCalcValidation()

        End Sub

        Public MustOverride Sub PopulateEditorPanel(ctner As Object) Implements IExternalUnitOperation.PopulateEditorPanel

        Private Sub CallSolverIfNeeded()
            If GlobalSettings.Settings.CallSolverOnEditorPropertyChanged Then
                FlowSheet.RequestCalculation()
            End If
        End Sub

    End Class

End Namespace
