Imports DWSIM.Thermodynamics.Streams

Public Module StreamListExtender

    <System.Runtime.CompilerServices.Extension()>
    Public Function GetInletMaterialStream(ByVal unitop As UnitOperations.UnitOpBaseClass, index As Integer) As MaterialStream

        If unitop.GraphicObject.InputConnectors(index).IsAttached Then
            Try
                Return unitop.FlowSheet.SimulationObjects(unitop.GraphicObject.InputConnectors(index).AttachedConnector.AttachedFrom.Name)
            Catch ex As Exception
                Return Nothing
            End Try
        Else
            Return Nothing
        End If

    End Function

    <System.Runtime.CompilerServices.Extension()>
    Public Function GetOutletMaterialStream(ByVal unitop As UnitOperations.UnitOpBaseClass, index As Integer) As MaterialStream

        If unitop.GraphicObject.OutputConnectors(index).IsAttached Then
            Try
                Return unitop.FlowSheet.SimulationObjects(unitop.GraphicObject.OutputConnectors(index).AttachedConnector.AttachedTo.Name)
            Catch ex As Exception
                Return Nothing
            End Try
        Else
            Return Nothing
        End If

    End Function

    <System.Runtime.CompilerServices.Extension()>
    Public Function GetInletEnergyStream(ByVal unitop As UnitOperations.UnitOpBaseClass, index As Integer) As Streams.EnergyStream

        If unitop.GraphicObject.InputConnectors(index).IsAttached Then
            Try
                Return unitop.FlowSheet.SimulationObjects(unitop.GraphicObject.InputConnectors(index).AttachedConnector.AttachedFrom.Name)
            Catch ex As Exception
                Return Nothing
            End Try
        Else
            Return Nothing
        End If

    End Function

    <System.Runtime.CompilerServices.Extension()>
    Public Function GetOutletEnergyStream(ByVal unitop As UnitOperations.UnitOpBaseClass, index As Integer) As Streams.EnergyStream

        If unitop.GraphicObject.OutputConnectors(index).IsAttached Then
            Try
                Return unitop.FlowSheet.SimulationObjects(unitop.GraphicObject.OutputConnectors(index).AttachedConnector.AttachedTo.Name)
            Catch ex As Exception
                Return Nothing
            End Try
        Else
            Return Nothing
        End If

    End Function

    <System.Runtime.CompilerServices.Extension()>
    Public Function GetEnergyStream(ByVal unitop As UnitOperations.UnitOpBaseClass) As Streams.EnergyStream
        If unitop.GraphicObject.EnergyConnector.IsAttached Then
            If unitop.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.ObjectType = Enums.GraphicObjects.ObjectType.EnergyStream Then
                Try
                    Return unitop.FlowSheet.SimulationObjects(unitop.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Name)
                Catch ex As Exception
                    Return Nothing
                End Try
            Else
                Return Nothing
            End If
        Else
            Return Nothing
        End If
        Return Nothing
    End Function


End Module
