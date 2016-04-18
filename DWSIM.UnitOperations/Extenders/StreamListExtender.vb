Imports DWSIM.Thermodynamics.Streams

Module StreamListExtender

    <System.Runtime.CompilerServices.Extension()> _
    Public Function GetInletMaterialStream(ByVal unitop As SharedClasses.UnitOperations.UnitOpBaseClass, index As Integer) As MaterialStream

        If unitop.GraphicObject.InputConnectors(index).IsAttached Then
            Return unitop.FlowSheet.SimulationObjects(unitop.GraphicObject.InputConnectors(index).AttachedConnector.AttachedFrom.Name)
        Else
            Return Nothing
        End If

    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function GetOutletMaterialStream(ByVal unitop As SharedClasses.UnitOperations.UnitOpBaseClass, index As Integer) As MaterialStream

        If unitop.GraphicObject.OutputConnectors(index).IsAttached Then
            Return unitop.FlowSheet.SimulationObjects(unitop.GraphicObject.OutputConnectors(index).AttachedConnector.AttachedTo.Name)
        Else
            Return Nothing
        End If

    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function GetInletEnergyStream(ByVal unitop As SharedClasses.UnitOperations.UnitOpBaseClass, index As Integer) As Streams.EnergyStream

        If unitop.GraphicObject.InputConnectors(index).IsAttached Then
            Return unitop.FlowSheet.SimulationObjects(unitop.GraphicObject.InputConnectors(index).AttachedConnector.AttachedFrom.Name)
        Else
            Return Nothing
        End If

    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function GetOutletEnergyStream(ByVal unitop As SharedClasses.UnitOperations.UnitOpBaseClass, index As Integer) As Streams.EnergyStream

        If unitop.GraphicObject.OutputConnectors(index).IsAttached Then
            Return unitop.FlowSheet.SimulationObjects(unitop.GraphicObject.OutputConnectors(index).AttachedConnector.AttachedTo.Name)
        Else
            Return Nothing
        End If

    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function GetEnergyStream(ByVal unitop As SharedClasses.UnitOperations.UnitOpBaseClass) As Streams.EnergyStream
        For Each obj In unitop.GraphicObject.OutputConnectors
            If obj.IsAttached Then
                If obj.AttachedConnector.AttachedTo.ObjectType = Enums.GraphicObjects.ObjectType.EnergyStream Then
                    Return unitop.FlowSheet.SimulationObjects(obj.AttachedConnector.AttachedTo.Name)
                Else
                    Return Nothing
                End If
            Else
                Return Nothing
            End If
        Next
        Return Nothing
    End Function


End Module
