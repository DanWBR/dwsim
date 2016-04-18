Imports DWSIM.Thermodynamics.Streams

Module StreamListExtender

    <System.Runtime.CompilerServices.Extension()> _
    Public Function GetInletMaterialStreams(ByVal unitop As SharedClasses.UnitOperations.UnitOpBaseClass) As List(Of MaterialStream)
        Dim mylist As New List(Of MaterialStream)
        For Each obj In unitop.GraphicObject.InputConnectors
            If obj.IsAttached Then
                If obj.AttachedConnector.AttachedFrom.ObjectType = Enums.GraphicObjects.ObjectType.MaterialStream Then
                    mylist.Add(unitop.FlowSheet.SimulationObjects(obj.AttachedConnector.AttachedFrom.Name))
                Else
                    mylist.Add(Nothing)
                End If
            Else
                mylist.Add(Nothing)
            End If
        Next
        Return mylist
    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function GetOutletMaterialStreams(ByVal unitop As SharedClasses.UnitOperations.UnitOpBaseClass) As List(Of MaterialStream)
        Dim mylist As New List(Of MaterialStream)
        For Each obj In unitop.GraphicObject.OutputConnectors
            If obj.IsAttached Then
                If obj.AttachedConnector.AttachedTo.ObjectType = Enums.GraphicObjects.ObjectType.MaterialStream Then
                    mylist.Add(unitop.FlowSheet.SimulationObjects(obj.AttachedConnector.AttachedTo.Name))
                Else
                    mylist.Add(Nothing)
                End If
            Else
                mylist.Add(Nothing)
            End If
        Next
        Return mylist
    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function GetInletEnergyStreams(ByVal unitop As SharedClasses.UnitOperations.UnitOpBaseClass) As List(Of Streams.EnergyStream)
        Dim mylist As New List(Of Streams.EnergyStream)
        For Each obj In unitop.GraphicObject.InputConnectors
            If obj.IsAttached Then
                If obj.AttachedConnector.AttachedFrom.ObjectType = Enums.GraphicObjects.ObjectType.EnergyStream Then
                    mylist.Add(unitop.FlowSheet.SimulationObjects(obj.AttachedConnector.AttachedFrom.Name))
                Else
                    mylist.Add(Nothing)
                End If
            Else
                mylist.Add(Nothing)
            End If
        Next
        Return mylist
    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function GetOutletEnergyStreams(ByVal unitop As SharedClasses.UnitOperations.UnitOpBaseClass) As List(Of Streams.EnergyStream)
        Dim mylist As New List(Of Streams.EnergyStream)
        For Each obj In unitop.GraphicObject.OutputConnectors
            If obj.IsAttached Then
                If obj.AttachedConnector.AttachedTo.ObjectType = Enums.GraphicObjects.ObjectType.EnergyStream Then
                    mylist.Add(unitop.FlowSheet.SimulationObjects(obj.AttachedConnector.AttachedTo.Name))
                Else
                    mylist.Add(Nothing)
                End If
            Else
                mylist.Add(Nothing)
            End If
        Next
        Return mylist
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
