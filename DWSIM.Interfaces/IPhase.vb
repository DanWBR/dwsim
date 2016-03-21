Public Interface IPhase

    Property ComponentDescription As String
    Property ComponentName As String
    Property Name As String
    Property Compounds As Dictionary(Of String, Interfaces.ICompound)

End Interface
