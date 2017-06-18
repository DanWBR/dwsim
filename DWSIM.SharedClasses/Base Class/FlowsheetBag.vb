Namespace Flowsheet

    Public Class FlowsheetBag

        Implements IFlowsheetBag

        Dim _so As Dictionary(Of String, ISimulationObject)
        Dim _go As Dictionary(Of String, IGraphicObject)
        Dim _co As Dictionary(Of String, ICompoundConstantProperties)
        Dim _pp As Dictionary(Of String, IPropertyPackage)
        Dim _re As Dictionary(Of String, IReaction)
        Dim _rs As Dictionary(Of String, IReactionSet)

        Dim _surface As Object

        Sub New(so As Dictionary(Of String, ISimulationObject),
                go As Dictionary(Of String, IGraphicObject),
                co As Dictionary(Of String, ICompoundConstantProperties),
                pp As Dictionary(Of String, IPropertyPackage),
                re As Dictionary(Of String, IReaction),
                rs As Dictionary(Of String, IReactionSet))

            _so = so
            _go = go
            _co = co
            _pp = pp
            _re = re
            _rs = rs

        End Sub

        Public Property SimulationObjects As Dictionary(Of String, ISimulationObject) Implements IFlowsheetBag.SimulationObjects
            Get
                Return _so
            End Get
            Set(value As Dictionary(Of String, ISimulationObject))

            End Set
        End Property

        Public Property GraphicObjects As Dictionary(Of String, IGraphicObject) Implements IFlowsheetBag.GraphicObjects
            Get
                Return _go
            End Get
            Set(value As Dictionary(Of String, IGraphicObject))

            End Set
        End Property

        Public Property Compounds As Dictionary(Of String, ICompoundConstantProperties) Implements IFlowsheetBag.Compounds
            Get
                Return _co
            End Get
            Set(value As Dictionary(Of String, ICompoundConstantProperties))

            End Set
        End Property

        Public Property PropertyPackages As Dictionary(Of String, IPropertyPackage) Implements IFlowsheetBag.PropertyPackages
            Get
                Return _pp
            End Get
            Set(value As Dictionary(Of String, IPropertyPackage))

            End Set
        End Property

        Public Property Reactions As Dictionary(Of String, IReaction) Implements IFlowsheetBag.Reactions
            Get
                Return _re
            End Get
            Set(value As Dictionary(Of String, IReaction))

            End Set
        End Property

        Public Property ReactionSets As Dictionary(Of String, IReactionSet) Implements IFlowsheetBag.ReactionSets
            Get
                Return _rs
            End Get
            Set(value As Dictionary(Of String, IReactionSet))

            End Set
        End Property

        Public Property SimulationObjectsArray As ISimulationObject() Implements IFlowsheetBag.SimulationObjectsArray
            Get
                Return _so.Values.ToArray
            End Get
            Set(value As ISimulationObject())

            End Set
        End Property

        Public Property GraphicObjectsArray As IGraphicObject() Implements IFlowsheetBag.GraphicObjectsArray
            Get
                Return _go.Values.ToArray
            End Get
            Set(value As IGraphicObject())

            End Set
        End Property

        Public Property CompoundsArray As ICompoundConstantProperties() Implements IFlowsheetBag.CompoundsArray
            Get
                Return _co.Values.ToArray
            End Get
            Set(value As ICompoundConstantProperties())

            End Set
        End Property

        Public Property PropertyPackagesArray As IPropertyPackage() Implements IFlowsheetBag.PropertyPackagesArray
            Get
                Return _pp.Values.ToArray
            End Get
            Set(value As IPropertyPackage())

            End Set
        End Property

        Public Property ReactionsArray As IReaction() Implements IFlowsheetBag.ReactionsArray
            Get
                Return _re.Values.ToArray
            End Get
            Set(value As IReaction())

            End Set
        End Property

        Public Property ReactionSetsArray As IReactionSet() Implements IFlowsheetBag.ReactionSetsArray
            Get
                Return _rs.Values.ToArray
            End Get
            Set(value As IReactionSet())

            End Set
        End Property

        Public Overridable Sub SaveToXML(xfile As String) Implements IFlowsheetBag.SaveToXML
            Throw New NotImplementedException
        End Sub

        Public Overridable Sub UpdateProcessData(xdoc As XDocument) Implements IFlowsheetBag.UpdateProcessData
            Throw New NotImplementedException
        End Sub

        Public Function GetSurface() As Object Implements IFlowsheetBag.GetSurface
            Return _surface
        End Function

    End Class

End Namespace
