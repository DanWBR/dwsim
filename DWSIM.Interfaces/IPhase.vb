<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IPhase

    Property ComponentDescription As String
    Property ComponentName As String
    Property Name As String
    Property Compounds As Dictionary(Of String, ICompound)
    ReadOnly Property Properties As IPhaseProperties

End Interface
