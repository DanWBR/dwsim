'    Adjust Calculation Routines 
'    Copyright 2008 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Imports Microsoft.MSDN.Samples.GraphicObjects
Imports System.ComponentModel
Imports PropertyGridEx
Imports System.Linq

Namespace DWSIM.SimulationObjects.SpecialOps

    <System.Serializable()> Public Class Adjust

        Inherits DWSIM.SimulationObjects.UnitOperations.SpecialOpBaseClass

        Protected m_ManipulatedObject As DWSIM.SimulationObjects.UnitOperations.BaseClass
        Protected m_ControlledObject As DWSIM.SimulationObjects.UnitOperations.BaseClass
        Protected m_ReferenceObject As DWSIM.SimulationObjects.UnitOperations.BaseClass

        Protected m_ManipulatedVariable As String = ""
        Protected m_ControlledVariable As String = ""
        Protected m_ReferenceVariable As String = ""

        Protected m_Status As String = ""

        Protected m_AdjustValue As Double = 1.0#

        Protected m_IsReferenced As Boolean = False
        Protected m_IsSimultAdjustEnabled As Boolean = False

        Protected m_StepSize As Double = 0.1
        Protected m_Tolerance As Double = 0.0001
        Protected m_MaxIterations As Integer = 10

        Protected m_ManipulatedObjectData As New DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.ManipulatedObjectInfo
        Protected m_ControlledObjectData As New DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.ControlledObjectInfo
        Protected m_ReferencedObjectData As New DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.ReferenceObjectInfo

        Protected m_CV_OK As Boolean = False
        Protected m_MV_OK As Boolean = False
        Protected m_RV_OK As Boolean = False

        Protected m_minVal As Nullable(Of Double) = Nothing
        Protected m_maxVal As Nullable(Of Double) = Nothing
        Protected m_initialEstimate As Nullable(Of Double) = Nothing

        Public Property SimultaneousAdjust() As Boolean
            Get
                Return m_IsSimultAdjustEnabled
            End Get
            Set(ByVal value As Boolean)
                m_IsSimultAdjustEnabled = value
            End Set
        End Property

        Public Property InitialEstimate() As Nullable(Of Double)
            Get
                Return m_initialEstimate
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_initialEstimate = value
            End Set
        End Property

        Public Property MaxVal() As Nullable(Of Double)
            Get
                Return m_maxVal
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_maxVal = value
            End Set
        End Property

        Public Property MinVal() As Nullable(Of Double)
            Get
                Return m_minVal
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_minVal = value
            End Set
        End Property

        Public Property RvOk() As Boolean
            Get
                Return m_RV_OK
            End Get
            Set(ByVal value As Boolean)
                m_RV_OK = value
            End Set
        End Property

        Public Property MvOk() As Boolean
            Get
                Return m_MV_OK
            End Get
            Set(ByVal value As Boolean)
                m_MV_OK = value
            End Set
        End Property

        Public Property CvOk() As Boolean
            Get
                Return m_CV_OK
            End Get
            Set(ByVal value As Boolean)
                m_CV_OK = value
            End Set
        End Property

        Public Property ManipulatedObjectData() As DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.ManipulatedObjectInfo
            Get
                Return Me.m_ManipulatedObjectData
            End Get
            Set(ByVal value As DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.ManipulatedObjectInfo)
                Me.m_ManipulatedObjectData = value
            End Set
        End Property

        Public Property ControlledObjectData() As DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.ControlledObjectInfo
            Get
                Return Me.m_ControlledObjectData
            End Get
            Set(ByVal value As DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.ControlledObjectInfo)
                Me.m_ControlledObjectData = value
            End Set
        End Property

        Public Property ReferencedObjectData() As DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.ReferenceObjectInfo
            Get
                Return Me.m_ReferencedObjectData
            End Get
            Set(ByVal value As DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.ReferenceObjectInfo)
                Me.m_ReferencedObjectData = value
            End Set
        End Property

        <Xml.Serialization.XmlIgnore()> Public Property ManipulatedObject() As DWSIM.SimulationObjects.UnitOperations.BaseClass
            Get
                Return Me.m_ManipulatedObject
            End Get
            Set(ByVal value As DWSIM.SimulationObjects.UnitOperations.BaseClass)
                Me.m_ManipulatedObject = value
            End Set
        End Property

        <Xml.Serialization.XmlIgnore()> Public Property ControlledObject() As DWSIM.SimulationObjects.UnitOperations.BaseClass
            Get
                Return Me.m_ControlledObject
            End Get
            Set(ByVal value As DWSIM.SimulationObjects.UnitOperations.BaseClass)
                Me.m_ControlledObject = value
            End Set
        End Property

        <Xml.Serialization.XmlIgnore()> Public Property ReferenceObject() As DWSIM.SimulationObjects.UnitOperations.BaseClass
            Get
                Return Me.m_ReferenceObject
            End Get
            Set(ByVal value As DWSIM.SimulationObjects.UnitOperations.BaseClass)
                Me.m_ReferenceObject = value
            End Set
        End Property

        Public Property ManipulatedVariable() As String
            Get
                Return Me.m_ManipulatedVariable
            End Get
            Set(ByVal value As String)
                Me.m_ManipulatedVariable = value
            End Set
        End Property

        Public Property ControlledVariable() As String
            Get
                Return Me.m_ControlledVariable
            End Get
            Set(ByVal value As String)
                Me.m_ControlledVariable = value
            End Set
        End Property

        Public Property ReferenceVariable() As String
            Get
                Return Me.m_ReferenceVariable
            End Get
            Set(ByVal value As String)
                Me.m_ReferenceVariable = value
            End Set
        End Property

        Public Property Status() As String
            Get
                Return Me.m_Status
            End Get
            Set(ByVal value As String)
                Me.m_Status = value
            End Set
        End Property

        Public Property AdjustValue() As Double
            Get
                Return Me.m_AdjustValue
            End Get
            Set(ByVal value As Double)
                Me.m_AdjustValue = value
            End Set
        End Property

        Public Property Referenced() As Boolean
            Get
                Return Me.m_IsReferenced
            End Get
            Set(ByVal value As Boolean)
                Me.m_IsReferenced = value
            End Set
        End Property

        Public Property StepSize() As Double
            Get
                Return Me.m_StepSize
            End Get
            Set(ByVal value As Double)
                Me.m_StepSize = value
            End Set
        End Property

        Public Property Tolerance() As Double
            Get
                Return Me.m_Tolerance
            End Get
            Set(ByVal value As Double)
                Me.m_Tolerance = value
            End Set
        End Property

        Public Property MaximumIterations() As Integer
            Get
                Return Me.m_MaxIterations
            End Get
            Set(ByVal value As Integer)
                Me.m_MaxIterations = value
            End Set
        End Property

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            MyBase.LoadData(data)

            Dim xel As XElement

            xel = (From xel2 As XElement In data Select xel2 Where xel2.Name = "ManipulatedObjectData").SingleOrDefault

            If Not xel Is Nothing Then

                With m_ManipulatedObjectData
                    .m_ID = xel.@ID
                    .m_Name = xel.@Name
                    .m_Property = xel.@Property
                    .m_Type = xel.@Type
                End With

            End If

            xel = (From xel2 As XElement In data Select xel2 Where xel2.Name = "ControlledObjectData").SingleOrDefault

            If Not xel Is Nothing Then

                With m_ControlledObjectData
                    .m_ID = xel.@ID
                    .m_Name = xel.@Name
                    .m_Property = xel.@Property
                    .m_Type = xel.@Type
                End With

            End If

            xel = (From xel2 As XElement In data Select xel2 Where xel2.Name = "ReferencedObjectData").SingleOrDefault

            If Not xel Is Nothing Then

                With m_ReferencedObjectData
                    .m_ID = xel.@ID
                    .m_Name = xel.@Name
                    .m_Property = xel.@Property
                    .m_Type = xel.@Type
                End With

            End If

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Add(New XElement("ManipulatedObjectData", New XAttribute("ID", m_ManipulatedObjectData.m_ID),
                                  New XAttribute("Name", m_ManipulatedObjectData.m_Name),
                                  New XAttribute("Property", m_ManipulatedObjectData.m_Property),
                                  New XAttribute("Type", m_ManipulatedObjectData.m_Type)))
                .Add(New XElement("ControlledObjectData", New XAttribute("ID", m_ControlledObjectData.m_ID),
                                  New XAttribute("Name", m_ControlledObjectData.m_Name),
                                  New XAttribute("Property", m_ControlledObjectData.m_Property),
                                  New XAttribute("Type", m_ControlledObjectData.m_Type)))
                .Add(New XElement("ReferencedObjectData", New XAttribute("ID", m_ReferencedObjectData.m_ID),
                                  New XAttribute("Name", m_ReferencedObjectData.m_Name),
                                  New XAttribute("Property", m_ReferencedObjectData.m_Property),
                                  New XAttribute("Type", m_ReferencedObjectData.m_Type)))
            End With

            Return elements

        End Function

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            m_ManipulatedObjectData = New DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.ManipulatedObjectInfo
            m_ControlledObjectData = New DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.ControlledObjectInfo
            m_ReferencedObjectData = New DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust.ReferenceObjectInfo
            Me.m_ComponentName = name
            Me.m_ComponentDescription = descricao
            Me.FillNodeItems()
            Me.QTFillNodeItems()
            Me.ShowQuickTable = False

        End Sub

        Public Overrides Sub QTFillNodeItems()

        End Sub

        Public Overrides Sub UpdatePropertyNodes(ByVal su As SystemsOfUnits.Units, ByVal nf As String)

        End Sub

        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As SystemsOfUnits.Units)

            Dim value As Double
            Dim Conversor As New DWSIM.SystemsOfUnits.Converter

            With pgrid

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True
                .Item.Clear()

                Dim cpc As New CustomPropertyCollection
                cpc.Add(DWSIM.App.GetLocalString("VarivelControlada"), Me, "ControlledObjectData", True, DWSIM.App.GetLocalString("VarivelControlada"), "", True)
                With cpc.Item(cpc.Count - 1)
                    .IsBrowsable = False
                    .CustomEditor = New DWSIM.Editors.SpecialOps.UICVSelectorEditor
                End With
                cpc.Add(DWSIM.App.GetLocalString("TipodoObjeto"), Me.ControlledObjectData, "m_Type", True, DWSIM.App.GetLocalString("VarivelControlada"), "", True)
                cpc.Add(DWSIM.App.GetLocalString("Objeto"), Me.ControlledObjectData, "m_Name", True, DWSIM.App.GetLocalString("VarivelControlada"), "", True)
                cpc.Add(DWSIM.App.GetLocalString("Propriedade"), DWSIM.App.GetPropertyName(Me.ControlledObjectData.m_Property), True, DWSIM.App.GetLocalString("VarivelControlada"), "", True)
                .Item.Add(DWSIM.App.GetLocalString("VarivelControlada"), cpc, False, DWSIM.App.GetLocalString("Configuraes1"), DWSIM.App.GetLocalString("Selecioneavarivelase"))
                With .Item(.Item.Count - 1)
                    .IsBrowsable = True
                    .BrowsableLabelStyle = BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

                Dim cpc2 As New CustomPropertyCollection
                cpc2.Add(DWSIM.App.GetLocalString("VarivelManipulada"), Me, "ManipulatedObjectData", True, DWSIM.App.GetLocalString("VarivelManipulada"), "", True)
                With cpc2.Item(cpc2.Count - 1)
                    .IsBrowsable = False
                    .CustomEditor = New DWSIM.Editors.SpecialOps.UIMVSelectorEditor
                End With
                cpc2.Add(DWSIM.App.GetLocalString("TipodoObjeto"), Me.ManipulatedObjectData, "m_Type", True, DWSIM.App.GetLocalString("VarivelManipulada"), "", True)
                cpc2.Add(DWSIM.App.GetLocalString("Objeto"), Me.ManipulatedObjectData, "m_Name", True, DWSIM.App.GetLocalString("VarivelManipulada"), "", True)
                cpc2.Add(DWSIM.App.GetLocalString("Propriedade"), DWSIM.App.GetPropertyName(Me.ManipulatedObjectData.m_Property), True, DWSIM.App.GetLocalString("VarivelManipulada"), "", True)

                If Me.ManipulatedObject IsNot Nothing Then
                    value = Format(Converter.ConvertFromSI(Me.ManipulatedObject.GetPropertyUnit(ManipulatedObjectData.m_Property, su), Me.MinVal.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                    cpc2.Add(FT(DWSIM.App.GetLocalString("Valormnimoopcional"), Me.ManipulatedObject.GetPropertyUnit(ManipulatedObjectData.m_Property, su)), value, False, DWSIM.App.GetLocalString("VarivelManipulada"), "", True)
                    value = Format(Converter.ConvertFromSI(Me.ManipulatedObject.GetPropertyUnit(ManipulatedObjectData.m_Property, su), Me.MaxVal.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                    cpc2.Add(FT(DWSIM.App.GetLocalString("Valormximoopcional"), Me.ManipulatedObject.GetPropertyUnit(ManipulatedObjectData.m_Property, su)), value, False, DWSIM.App.GetLocalString("VarivelManipulada"), "", True)
                End If
                .Item.Add(DWSIM.App.GetLocalString("VarivelManipulada"), cpc2, False, DWSIM.App.GetLocalString("Configuraes1"), DWSIM.App.GetLocalString("Selecioneavarivelase2"))
                With .Item(.Item.Count - 1)
                    .IsBrowsable = True
                    .BrowsableLabelStyle = BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

                .Item.Add(DWSIM.App.GetLocalString("UsaObjetocomoRefernc"), Me, "Referenced", False, DWSIM.App.GetLocalString("Configuraes1"), "", True)

                If Me.Referenced Then
                    Dim cpc3 As New CustomPropertyCollection
                    cpc3.Add(DWSIM.App.GetLocalString("ObjetoVariveldeRefer"), Me, "ReferencedObjectData", True, DWSIM.App.GetLocalString("VarivelManipulada"), "", True)
                    With cpc3.Item(cpc3.Count - 1)
                        .IsBrowsable = False
                        .CustomEditor = New DWSIM.Editors.SpecialOps.UIRVSelectorEditor
                    End With
                    cpc3.Add(DWSIM.App.GetLocalString("Objeto"), Me.ReferencedObjectData, "m_Name", True, DWSIM.App.GetLocalString("ObjetoVariveldeRefer"), "", True)
                    .Item.Add(DWSIM.App.GetLocalString("ObjetoVariveldeRefer"), cpc3, False, DWSIM.App.GetLocalString("Configuraes1"), DWSIM.App.GetLocalString("Selecioneavarivelase3"))
                    With .Item(.Item.Count - 1)
                        .IsBrowsable = True
                        .BrowsableLabelStyle = BrowsableTypeConverter.LabelStyle.lsEllipsis
                        .CustomEditor = New System.Drawing.Design.UITypeEditor
                    End With
                End If

                If Me.ControlledObject IsNot Nothing Then
                    value = Format(Converter.ConvertFromSI(Me.ControlledObject.GetPropertyUnit(ControlledObjectData.m_Property, su), Me.AdjustValue), FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("ValordeAjusteouOffse"), Me.ControlledObject.GetPropertyUnit(ControlledObjectData.m_Property, su)), value, False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("SetpointdoAjusteouov"), True)
                End If
                .Item.Add(DWSIM.App.GetLocalString("NmeroMximodeIteraes"), Me, "MaximumIterations", False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("Nmeromximodeiteraesa"), True)
                .Item.Add(DWSIM.App.GetLocalString("Tolerncia"), Me, "Tolerance", False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("Diferenamximaentreos"), True)
                .Item.Add(DWSIM.App.GetLocalString("DeltaStepsize"), Me, "StepSize", False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("Variaoinicialnovalor"), True)
                .Item.Add(DWSIM.App.GetLocalString("SimultaneousAdjust"), Me, "SimultaneousAdjust", False, DWSIM.App.GetLocalString("Parmetros2"), "Puts the adjust under control of the Simultaneous Adjust Solver.", True)
                .Item.Add(DWSIM.App.GetLocalString("PaineldeControle"), Me, "Status", True, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("CliqueparaexibiroPai"), True)
                With .Item(.Item.Count - 1)
                    .IsBrowsable = False
                    .CustomEditor = New DWSIM.Editors.SpecialOps.Adjust.UI_AdjControlPanelFormEditor
                End With

                If Not Me.Annotation Is Nothing Then
                    .Item.Add(DWSIM.App.GetLocalString("Anotaes"), Me, "Annotation", False, DWSIM.App.GetLocalString("Outros"), DWSIM.App.GetLocalString("Cliquenobotocomretic"), True)
                    With .Item(.Item.Count - 1)
                        .IsBrowsable = False
                        .CustomEditor = New DWSIM.Editors.Annotation.UIAnnotationEditor
                    End With
                End If

                .ExpandAllGridItems()

            End With


        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            Return 0
        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As DWSIM.SimulationObjects.UnitOperations.BaseClass.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As DWSIM.SystemsOfUnits.Units = Nothing) As Object
            Return 0
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            Return 0
        End Function
    End Class

End Namespace

Namespace DWSIM.SimulationObjects.SpecialOps.Helpers.Adjust

    Public Enum TipoVar
        Manipulada
        Controlada
        Referencia
        Nenhum
    End Enum

    <System.Serializable()> Public Class ManipulatedObjectInfo

        Public m_Type As String = ""
        Public m_Name As String = ""
        Public m_ID As String = ""
        Public m_Property As String = ""

        Sub New()

        End Sub

        Overrides Function ToString() As String
            Return DWSIM.App.GetLocalString("Cliqueparaselecionar")
        End Function

    End Class

    <System.Serializable()> Public Class ControlledObjectInfo

        Public m_Type As String = ""
        Public m_Name As String = ""
        Public m_ID As String = ""
        Public m_Property As String = ""

        Sub New()

        End Sub

        Overrides Function ToString() As String
            Return DWSIM.App.GetLocalString("Cliqueparaselecionar")
        End Function

    End Class

    <System.Serializable()> Public Class ReferenceObjectInfo

        Public m_Type As String = ""
        Public m_Name As String = ""
        Public m_ID As String = ""
        Public m_Property As String = ""

        Sub New()

        End Sub

        Overrides Function ToString() As String
            Return DWSIM.App.GetLocalString("Cliqueparaselecionar")
        End Function

    End Class

End Namespace



