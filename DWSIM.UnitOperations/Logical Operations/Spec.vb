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
Imports Ciloci.Flee
Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses
Imports System.Windows.Forms
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Interfaces.Enums
Imports System.Linq

Namespace SpecialOps

    <System.Serializable()> Public Class Spec

        Inherits SharedClasses.UnitOperations.SpecialOpBaseClass

        Protected m_SourceObjectData As New SharedClasses.SpecialOps.Helpers.SpecialOpObjectInfo
        Protected m_TargetObjectData As New SharedClasses.SpecialOps.Helpers.SpecialOpObjectInfo

        Protected m_CalculateTargetObject As Boolean = False

        Protected m_CV_OK As Boolean = False
        Protected m_MV_OK As Boolean = False

        Protected m_SourceObject As SharedClasses.UnitOperations.BaseClass
        Protected m_TargetObject As SharedClasses.UnitOperations.BaseClass

        Protected m_SourceVariable As String = ""
        Protected m_TargetVariable As String = ""

        Protected m_Expression As String = ""

        Protected m_Status As String = ""

        Protected m_minVal As Nullable(Of Double) = Nothing
        Protected m_maxVal As Nullable(Of Double) = Nothing

        <System.NonSerialized()> Protected m_e As IGenericExpression(Of Double)
        <System.NonSerialized()> Protected m_eopt As ExpressionContext

        <System.NonSerialized()> Protected formC As IFlowsheet
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

        Public Property SourceObjectData() As SharedClasses.SpecialOps.Helpers.SpecialOpObjectInfo
            Get
                Return Me.m_SourceObjectData
            End Get
            Set(ByVal value As SharedClasses.SpecialOps.Helpers.SpecialOpObjectInfo)
                Me.m_SourceObjectData = value
            End Set
        End Property

        Public Property TargetObjectData() As SharedClasses.SpecialOps.Helpers.SpecialOpObjectInfo
            Get
                Return Me.m_TargetObjectData
            End Get
            Set(ByVal value As SharedClasses.SpecialOps.Helpers.SpecialOpObjectInfo)
                Me.m_TargetObjectData = value
            End Set
        End Property

        <Xml.Serialization.XmlIgnore()> Public Property SourceObject() As SharedClasses.UnitOperations.BaseClass
            Get
                Return Me.m_SourceObject
            End Get
            Set(ByVal value As SharedClasses.UnitOperations.BaseClass)
                Me.m_SourceObject = value
            End Set
        End Property

        <Xml.Serialization.XmlIgnore()> Public Property TargetObject() As SharedClasses.UnitOperations.BaseClass
            Get
                Return Me.m_TargetObject
            End Get
            Set(ByVal value As SharedClasses.UnitOperations.BaseClass)
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
                    .ID = xel.@ID
                    .Name = xel.@Name
                    .PropertyName = xel.@Property
                    .Type = xel.@Type
                End With

            End If

            xel = (From xel2 As XElement In data Select xel2 Where xel2.Name = "TargetObjectData").SingleOrDefault

            If Not xel Is Nothing Then

                With m_TargetObjectData
                    .ID = xel.@ID
                    .Name = xel.@Name
                    .PropertyName = xel.@Property
                    .Type = xel.@Type
                End With

            End If

            Try
                Me.SourceObject = Me.FlowSheet.SimulationObjects(Me.SourceObjectData.ID)
                If Not Me.SourceObject Is Nothing Then Me.SourceObject.IsSpecAttached = True
            Catch ex As Exception

            End Try
            Try
                Me.TargetObject = Me.FlowSheet.SimulationObjects(Me.TargetObjectData.ID)
                If Not Me.TargetObject Is Nothing Then Me.TargetObject.IsSpecAttached = True
            Catch ex As Exception

            End Try
            Return True
        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Add(New XElement("SourceObjectData", New XAttribute("ID", m_SourceObjectData.ID),
                                  New XAttribute("Name", m_SourceObjectData.Name),
                                  New XAttribute("Property", m_SourceObjectData.PropertyName),
                                  New XAttribute("Type", m_SourceObjectData.Type)))
                .Add(New XElement("TargetObjectData", New XAttribute("ID", m_TargetObjectData.ID),
                                  New XAttribute("Name", m_TargetObjectData.Name),
                                  New XAttribute("Property", m_TargetObjectData.PropertyName),
                                  New XAttribute("Type", m_TargetObjectData.Type)))
            End With

            Return elements

        End Function

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()

            m_SourceObjectData = New SharedClasses.SpecialOps.Helpers.SpecialOpObjectInfo
            m_TargetObjectData = New SharedClasses.SpecialOps.Helpers.SpecialOpObjectInfo

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
            'context.Variables.DefineVariable(Flowsheet.GetTranslatedString("a"), typeof(int));
            'context.Variables.SetVariableValue(Flowsheet.GetTranslatedString("a"), 100);

            '// Create a dynamic expression that evaluates to an Object
            'IDynamicExpression eDynamic = ExpressionFactory.CreateDynamic("sqrt(a) + 1", context);
            '// Create a generic expression that evaluates to a double
            'IGenericExpression<double> eGeneric = ExpressionFactory.CreateGeneric<double>("sqrt(a) + 1", context);

            '// Evaluate the expressions
            'double result = (double)eDynamic.Evaluate();
            'result = eGeneric.Evaluate();

            '// Update the value of our variable
            'context.Variables.SetVariableValue(Flowsheet.GetTranslatedString("a"), 144);
            '// Evaluate again to get the updated result
            'result = eGeneric.Evaluate();

        End Sub

        Public Function GetTargetVarValue()

            formC = Me.FlowSheet
            Me.su = formC.FlowsheetOptions.SelectedUnitSystem
            Me.nf = formC.FlowsheetOptions.NumberFormat

            If Not Me.TargetObjectData Is Nothing Then

                If formC.SimulationObjects.ContainsKey(Me.TargetObjectData.ID) Then

                    With Me.TargetObjectData
                        Return Me.formC.SimulationObjects(.ID).GetPropertyValue(.PropertyName, su)
                    End With

                Else

                    Return Nothing

                End If

            Else

                Return Nothing

            End If


        End Function

        Public Function GetSourceVarValue()

            formC = Me.FlowSheet
            Me.su = formC.FlowsheetOptions.SelectedUnitSystem
            Me.nf = formC.FlowsheetOptions.NumberFormat

            If Not Me.SourceObjectData Is Nothing Then

                If formC.SimulationObjects.ContainsKey(Me.SourceObjectData.ID) Then

                    With Me.SourceObjectData
                        Return Me.formC.SimulationObjects(.ID).GetPropertyValue(.PropertyName, su)
                    End With

                Else

                    Return Nothing

                End If


            Else

                Return Nothing

            End If
        End Function

        Public Function SetTargetVarValue(ByVal val As Nullable(Of Double))

            formC = Me.FlowSheet
            Me.su = formC.FlowsheetOptions.SelectedUnitSystem
            Me.nf = formC.FlowsheetOptions.NumberFormat

            If Not Me.TargetObjectData Is Nothing Then

                If formC.SimulationObjects.ContainsKey(Me.TargetObjectData.ID) Then

                    With Me.TargetObjectData
                        Return Me.formC.SimulationObjects(.ID).SetPropertyValue(.PropertyName, val, su)
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

            Return Me.FlowSheet.SimulationObjects(Me.TargetObjectData.ID).GetPropertyUnit(Me.TargetObjectData.PropertyName, Me.FlowSheet.FlowsheetOptions.SelectedUnitSystem)

        End Function

        Public Function GetSourceVarUnit()

            Return Me.FlowSheet.SimulationObjects(Me.SourceObjectData.ID).GetPropertyUnit(Me.SourceObjectData.PropertyName, Me.FlowSheet.FlowsheetOptions.SelectedUnitSystem)

        End Function

        Public Shadows Sub Calculate()

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
                            Exit Sub
                        End If

                    End With

                    Me.GraphicObject.Calculated = True

                Else

                    Me.GraphicObject.Calculated = True
                    Throw New Exception(FlowSheet.GetTranslatedString("Existeumerronaconfig"))

                End If


            End If


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

        Public Overrides Sub DisplayEditForm()

        End Sub

        Public Overrides Sub UpdateEditForm()

        End Sub
    End Class

End Namespace




