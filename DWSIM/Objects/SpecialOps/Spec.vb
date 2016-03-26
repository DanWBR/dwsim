'    Specification Calculation Routines 
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

Imports DWSIM.DrawingTools.GraphicObjects
Imports ciloci.Flee
Imports DWSIM.DWSIM.SimulationObjects.SpecialOps.Helpers.Spec
Imports System.Linq

Namespace DWSIM.SimulationObjects.SpecialOps

    <System.Serializable()> Public Class Spec

        Inherits DWSIM.SimulationObjects.UnitOperations.SpecialOpBaseClass

        Protected m_SourceObjectData As New SourceObjectInfo
        Protected m_TargetObjectData As New TargetObjectInfo

        Protected m_CalculateTargetObject As Boolean = False

        Protected m_CV_OK As Boolean = False
        Protected m_MV_OK As Boolean = False

        Protected m_SourceObject As DWSIM.SimulationObjects.UnitOperations.BaseClass
        Protected m_TargetObject As DWSIM.SimulationObjects.UnitOperations.BaseClass

        Protected m_SourceVariable As String = ""
        Protected m_TargetVariable As String = ""

        Protected m_Expression As String = ""

        Protected m_Status As String = ""

        Protected m_minVal As Nullable(Of Double) = Nothing
        Protected m_maxVal As Nullable(Of Double) = Nothing

        <System.NonSerialized()> Protected m_e As IGenericExpression(Of Double)
        <System.NonSerialized()> Protected m_eopt As ExpressionContext

        <System.NonSerialized()> Protected formC As Global.DWSIM.FormFlowsheet
        Protected su As SystemsOfUnits.Units
        Protected cv As New SystemsOfUnits.Converter
        Protected nf As String = ""

        Public Property CalculateTargetObject() As Boolean
            Get
                Return Me.m_CalculateTargetObject
            End Get
            Set(ByVal value As Boolean)
                Me.m_CalculateTargetObject = value
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

        Public Property Expr() As IGenericExpression(Of Double)
            Get
                Return m_e
            End Get
            Set(ByVal value As IGenericExpression(Of Double))
                m_e = value
            End Set
        End Property

        Public Property ExpContext() As ExpressionContext
            Get
                Return m_eopt
            End Get
            Set(ByVal value As ExpressionContext)
                m_eopt = value
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

        Public Property Expression() As String
            Get
                Return Me.m_Expression
            End Get
            Set(ByVal value As String)
                Me.m_Expression = value
            End Set
        End Property

        Public Property SourceObjectData() As DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.SourceObjectInfo
            Get
                Return Me.m_SourceObjectData
            End Get
            Set(ByVal value As DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.SourceObjectInfo)
                Me.m_SourceObjectData = value
            End Set
        End Property

        Public Property TargetObjectData() As DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TargetObjectInfo
            Get
                Return Me.m_TargetObjectData
            End Get
            Set(ByVal value As DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TargetObjectInfo)
                Me.m_TargetObjectData = value
            End Set
        End Property

        <Xml.Serialization.XmlIgnore()> Public Property SourceObject() As DWSIM.SimulationObjects.UnitOperations.BaseClass
            Get
                Return Me.m_SourceObject
            End Get
            Set(ByVal value As DWSIM.SimulationObjects.UnitOperations.BaseClass)
                Me.m_SourceObject = value
            End Set
        End Property

        <Xml.Serialization.XmlIgnore()> Public Property TargetObject() As DWSIM.SimulationObjects.UnitOperations.BaseClass
            Get
                Return Me.m_TargetObject
            End Get
            Set(ByVal value As DWSIM.SimulationObjects.UnitOperations.BaseClass)
                Me.m_TargetObject = value
            End Set
        End Property

        Public Property SourceVariable() As String
            Get
                Return Me.m_SourceVariable
            End Get
            Set(ByVal value As String)
                Me.m_SourceVariable = value
            End Set
        End Property

        Public Property TargetVariable() As String
            Get
                Return Me.m_TargetVariable
            End Get
            Set(ByVal value As String)
                Me.m_TargetVariable = value
            End Set
        End Property

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            MyBase.LoadData(data)

            Dim xel As XElement

            xel = (From xel2 As XElement In data Select xel2 Where xel2.Name = "SourceObjectData").SingleOrDefault

            If Not xel Is Nothing Then

                With m_SourceObjectData
                    .m_ID = xel.@ID
                    .m_Name = xel.@Name
                    .m_Property = xel.@Property
                    .m_Type = xel.@Type
                End With

            End If

            xel = (From xel2 As XElement In data Select xel2 Where xel2.Name = "TargetObjectData").SingleOrDefault

            If Not xel Is Nothing Then

                With m_TargetObjectData
                    .m_ID = xel.@ID
                    .m_Name = xel.@Name
                    .m_Property = xel.@Property
                    .m_Type = xel.@Type
                End With

            End If

            Try
                Me.SourceObject = Me.FlowSheet.Collections.FlowsheetObjectCollection(Me.SourceObjectData.m_ID)
                If Not Me.SourceObject Is Nothing Then Me.SourceObject.IsSpecAttached = True
            Catch ex As Exception

            End Try
            Try
                Me.TargetObject = Me.FlowSheet.Collections.FlowsheetObjectCollection(Me.TargetObjectData.m_ID)
                If Not Me.TargetObject Is Nothing Then Me.TargetObject.IsSpecAttached = True
            Catch ex As Exception

            End Try

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Add(New XElement("SourceObjectData", New XAttribute("ID", m_SourceObjectData.m_ID),
                                  New XAttribute("Name", m_SourceObjectData.m_Name),
                                  New XAttribute("Property", m_SourceObjectData.m_Property),
                                  New XAttribute("Type", m_SourceObjectData.m_Type)))
                .Add(New XElement("TargetObjectData", New XAttribute("ID", m_TargetObjectData.m_ID),
                                  New XAttribute("Name", m_TargetObjectData.m_Name),
                                  New XAttribute("Property", m_TargetObjectData.m_Property),
                                  New XAttribute("Type", m_TargetObjectData.m_Type)))
            End With

            Return elements

        End Function

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()

            m_SourceObjectData = New DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.SourceObjectInfo
            m_TargetObjectData = New DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TargetObjectInfo

            m_eopt = New ExpressionContext
            With m_eopt
                .Imports.AddType(GetType(System.Math))
            End With

            Me.ComponentName = name
            Me.ComponentDescription = description



            '// Define the context of our expression
            'ExpressionContext context = new ExpressionContext();
            '// Import all members of the Math type into the default namespace
            'context.Imports.ImportStaticMembers(typeof(Math));

            '// Define an int variable
            'context.Variables.DefineVariable(DWSIM.App.GetLocalString("a"), typeof(int));
            'context.Variables.SetVariableValue(DWSIM.App.GetLocalString("a"), 100);

            '// Create a dynamic expression that evaluates to an Object
            'IDynamicExpression eDynamic = ExpressionFactory.CreateDynamic("sqrt(a) + 1", context);
            '// Create a generic expression that evaluates to a double
            'IGenericExpression<double> eGeneric = ExpressionFactory.CreateGeneric<double>("sqrt(a) + 1", context);

            '// Evaluate the expressions
            'double result = (double)eDynamic.Evaluate();
            'result = eGeneric.Evaluate();

            '// Update the value of our variable
            'context.Variables.SetVariableValue(DWSIM.App.GetLocalString("a"), 144);
            '// Evaluate again to get the updated result
            'result = eGeneric.Evaluate();

        End Sub

        Public Function GetTargetVarValue()

            formC = Me.Flowsheet
            Me.su = formC.Options.SelectedUnitSystem
            Me.nf = formC.Options.NumberFormat

            If Not Me.TargetObjectData Is Nothing Then

                If formC.Collections.FlowsheetObjectCollection.ContainsKey(Me.TargetObjectData.m_ID) Then

                    With Me.TargetObjectData
                        Return Me.formC.Collections.FlowsheetObjectCollection(.m_ID).GetPropertyValue(.m_Property, su)
                    End With

                Else

                    Return Nothing

                End If


            Else

                Return Nothing

            End If


        End Function

        Public Function GetSourceVarValue()

            formC = Me.Flowsheet
            Me.su = formC.Options.SelectedUnitSystem
            Me.nf = formC.Options.NumberFormat

            If Not Me.SourceObjectData Is Nothing Then

                If formC.Collections.FlowsheetObjectCollection.ContainsKey(Me.SourceObjectData.m_ID) Then

                    With Me.SourceObjectData
                        Return Me.formC.Collections.FlowsheetObjectCollection(.m_ID).GetPropertyValue(.m_Property, su)
                    End With

                Else

                    Return Nothing

                End If


            Else

                Return Nothing

            End If
        End Function

        Public Function SetTargetVarValue(ByVal val As Nullable(Of Double))

            formC = Me.Flowsheet
            Me.su = formC.Options.SelectedUnitSystem
            Me.nf = formC.Options.NumberFormat

            If Not Me.TargetObjectData Is Nothing Then

                If formC.Collections.FlowsheetObjectCollection.ContainsKey(Me.TargetObjectData.m_ID) Then

                    With Me.TargetObjectData
                        Return Me.formC.Collections.FlowsheetObjectCollection(.m_ID).SetPropertyValue(.m_Property, val, su)
                    End With

                Else

                    Return 0

                End If

            Else

                Return 0

            End If

            Return 1

        End Function

        Public Function GetTargetVarUnit()

            Return Me.FlowSheet.Collections.FlowsheetObjectCollection(Me.TargetObjectData.m_ID).GetPropertyUnit(Me.TargetObjectData.m_Property, Me.FlowSheet.Options.SelectedUnitSystem)

        End Function

        Public Function GetSourceVarUnit()

            Return Me.FlowSheet.Collections.FlowsheetObjectCollection(Me.SourceObjectData.m_ID).GetPropertyUnit(Me.SourceObjectData.m_Property, Me.FlowSheet.Options.SelectedUnitSystem)

        End Function

        Public Shadows Function Calculate() As Integer

            If Me.GraphicObject.Active Then

                Me.ExpContext = New Ciloci.Flee.ExpressionContext
                Me.ExpContext.Imports.AddType(GetType(System.Math))

                If Not Me.GetSourceVarValue Is Nothing And Not Me.GetTargetVarValue Is Nothing Then

                    With Me

                        .ExpContext.Variables.Add("X", Double.Parse(.GetSourceVarValue))
                        .ExpContext.Variables.Add("Y", Double.Parse(.GetTargetVarValue))
                        .Expr = .ExpContext.CompileGeneric(Of Double)(.Expression)

                        Dim val = .Expr.Evaluate

                        If Not Me.MaxVal.HasValue And Not Me.MinVal.HasValue Then
                            Me.SetTargetVarValue(val)
                        Else
                            If val < Me.MinVal.Value Then
                                Me.SetTargetVarValue(Me.MinVal.Value)
                            ElseIf val > Me.MaxVal.Value Then
                                Me.SetTargetVarValue(Me.MaxVal.Value)
                            Else
                                Me.SetTargetVarValue(val)
                            End If
                            Exit Function
                        End If

                    End With

                    Me.GraphicObject.Calculated = True

                    Dim form As Global.DWSIM.FormFlowsheet = Me.FlowSheet

                    Me.TargetObject = form.Collections.FlowsheetObjectCollection(Me.TargetObjectData.m_ID)
                    Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
                    'Call function to calculate flowsheet
                    With objargs
                        .Calculated = False
                        .Name = Me.TargetObject.Name
                        .Tag = Me.TargetObject.GraphicObject.Tag
                        .ObjectType = Me.TargetObject.GraphicObject.ObjectType
                        .Sender = "Spec"
                    End With
                    form.CalculationQueue.Enqueue(objargs)

                Else

                    Me.GraphicObject.Calculated = True
                    Throw New Exception(DWSIM.App.GetLocalString("Existeumerronaconfig"))

                End If


            End If


        End Function

        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As SystemsOfUnits.Units)
            Dim Conversor As New SystemsOfUnits.Converter

            With FlowSheet.FormProps.PGEx1

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True
                .Item.Clear()

                Dim cpc As New PropertyGridEx.CustomPropertyCollection
                cpc.Add(DWSIM.App.GetLocalString("VarivelFonte"), Me, "SourceObjectData", True, DWSIM.App.GetLocalString("VarivelFonte"), "", True)
                With cpc.Item(cpc.Count - 1)
                    .IsBrowsable = False
                    .CustomEditor = New DWSIM.Editors.SpecialOps.UISVSelectorEditor
                End With
                cpc.Add(DWSIM.App.GetLocalString("TipodoObjeto"), Me.SourceObjectData, "m_Type", True, DWSIM.App.GetLocalString("VarivelFonte"), "", True)
                cpc.Add(DWSIM.App.GetLocalString("Objeto"), Me.SourceObjectData, "m_Name", True, DWSIM.App.GetLocalString("VarivelFonte"), "", True)
                cpc.Add(DWSIM.App.GetLocalString("Propriedade"), DWSIM.App.GetPropertyName(Me.SourceObjectData.m_Property), True, DWSIM.App.GetLocalString("VarivelFonte"), "", True)
                .Item.Add(DWSIM.App.GetLocalString("VarivelFonte"), cpc, False, DWSIM.App.GetLocalString("Configuraes1"), DWSIM.App.GetLocalString("Selecioneavarivelase4"))
                With .Item(.Item.Count - 1)
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

                Dim cpc2 As New PropertyGridEx.CustomPropertyCollection
                cpc2.Add(DWSIM.App.GetLocalString("VarivelDestino"), Me, "TargetObjectData", True, DWSIM.App.GetLocalString("VarivelDestino"), "", True)
                With cpc2.Item(cpc2.Count - 1)
                    .IsBrowsable = False
                    .CustomEditor = New DWSIM.Editors.SpecialOps.UITVSelectorEditor
                End With
                cpc2.Add(DWSIM.App.GetLocalString("TipodoObjeto"), Me.TargetObjectData, "m_Type", True, DWSIM.App.GetLocalString("VarivelDestino"), "", True)
                cpc2.Add(DWSIM.App.GetLocalString("Objeto"), Me.TargetObjectData, "m_Name", True, DWSIM.App.GetLocalString("VarivelDestino"), "", True)
                cpc2.Add(DWSIM.App.GetLocalString("Propriedade"), DWSIM.App.GetPropertyName(Me.TargetObjectData.m_Property), True, DWSIM.App.GetLocalString("VarivelDestino"), "", True)
                With cpc2.Item(cpc2.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With
                .Item.Add(DWSIM.App.GetLocalString("VarivelDestino"), cpc2, False, DWSIM.App.GetLocalString("Configuraes1"), DWSIM.App.GetLocalString("Selecioneavarivelase5"))
                With .Item(.Item.Count - 1)
                    .IsBrowsable = True
                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                End With

                .Item.Add(DWSIM.App.GetLocalString("Ativo"), Me.GraphicObject, "Active", False, DWSIM.App.GetLocalString("Configuraes1"), DWSIM.App.GetLocalString("SelecioLiquidrueparaati"), True)
                With .Item(.Item.Count - 1)
                    .IsBrowsable = False
                End With

                .Item.Add(DWSIM.App.GetLocalString("Calcularobjetodestin"), Me, "CalculateTargetObject", False, DWSIM.App.GetLocalString("Configuraes1"), DWSIM.App.GetLocalString("SelecioLiquidrueparafaz"), True)
                With .Item(.Item.Count - 1)
                    .IsBrowsable = False
                End With

                .Item.Add(DWSIM.App.GetLocalString("Expresso"), Me, "Expression", True, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("Expressoquerelaciona"), True)
                With .Item(.Item.Count - 1)
                    .IsBrowsable = False
                End With

                .Item.Add(DWSIM.App.GetLocalString("Valormnimoopcional"), Me, "MinVal", False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("Valormnimoparaavariv"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With
                .Item.Add(DWSIM.App.GetLocalString("Valormximoopcional"), Me, "MaxVal", False, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("Valormximoparaavariv"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                .Item.Add(DWSIM.App.GetLocalString("PaineldeControle"), Me, "Status", True, DWSIM.App.GetLocalString("Parmetros2"), DWSIM.App.GetLocalString("CliqueparaexibiroPai"), True)
                With .Item(.Item.Count - 1)
                    .IsBrowsable = False
                    .CustomEditor = New DWSIM.Editors.SpecialOps.Spec.UI_SpecControlPanelFormEditor
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

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object
            Return 0

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean
            Return 0

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            Return 0

        End Function
    End Class

End Namespace




