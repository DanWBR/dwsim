'    Flowsheet Object Base Classes 
'    Copyright 2008-2014 Daniel Wagner O. de Medeiros
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

Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Runtime.Serialization
Imports System.IO
Imports System.Linq
Imports CapeOpen
Imports System.Runtime.Serialization.Formatters
Imports System.Runtime.InteropServices.Marshal
Imports System.Runtime.InteropServices
Imports System.Text
Imports DWSIM.Interfaces.Interfaces
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.Interfaces.Enums
Imports System.Windows.Forms

Namespace UnitOperations

    <System.Serializable()> <ComVisible(True)> Public MustInherit Class BaseClass

        Implements ICloneable, IDisposable, XMLSerializer.Interfaces.ICustomXMLSerialization

        Implements ICapeIdentification, Interfaces.ISimulationObject

        Public Const ClassId As String = ""

        <System.NonSerialized()> Protected Friend m_flowsheet As Interfaces.IFlowsheet

#Region "    Constructors"

        Public Sub New()

        End Sub

        Sub CreateNew()

        End Sub

#End Region

#Region "    ICapeIdentification"

        Public Property ComponentDescription() As String = "" Implements CapeOpen.ICapeIdentification.ComponentDescription

        Public Property ComponentName() As String = "" Implements CapeOpen.ICapeIdentification.ComponentName

#End Region

#Region "    ISimulationObject"

        Public Overridable Function GetVersion() As Version Implements ISimulationObject.GetVersion
            Return Me.GetType.Assembly.GetName.Version
        End Function

        Public MustOverride Function GetDisplayName() As String Implements ISimulationObject.GetDisplayName

        Public MustOverride Function GetDisplayDescription() As String Implements ISimulationObject.GetDisplayDescription

        Public MustOverride Function GetIconBitmap() As Object Implements ISimulationObject.GetIconBitmap

        Public Property AttachedUtilities As New List(Of IAttachedUtility) Implements ISimulationObject.AttachedUtilities

        Public Property PreferredFlashAlgorithmTag As String = "" Implements ISimulationObject.PreferredFlashAlgorithmTag
        Public Property Calculated As Boolean = False Implements Interfaces.ISimulationObject.Calculated

        Public Property DebugMode As Boolean = False Implements Interfaces.ISimulationObject.DebugMode

        Public Property DebugText As String = "" Implements Interfaces.ISimulationObject.DebugText

        <Xml.Serialization.XmlIgnore> Public Property LastUpdated As New Date Implements Interfaces.ISimulationObject.LastUpdated

        ''' <summary>
        ''' Calculates the object.
        ''' </summary>
        ''' <param name="args"></param>
        ''' <remarks>Use 'Solve()' to calculate the object instead.</remarks>
        Public Overridable Sub Calculate(Optional ByVal args As Object = Nothing) Implements ISimulationObject.Calculate
            Throw New NotImplementedException
        End Sub

        Public Sub DeCalculate(Optional args As Object = Nothing) Implements ISimulationObject.DeCalculate
            Throw New NotImplementedException
        End Sub

        Public MustOverride Sub DisplayEditForm() Implements ISimulationObject.DisplayEditForm

        Public MustOverride Sub UpdateEditForm() Implements ISimulationObject.UpdateEditForm


        ''' <summary>
        ''' Energy Flow property. Only implemented for Energy Streams.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Xml.Serialization.XmlIgnore()> Overridable Property EnergyFlow() As Nullable(Of Double)
            Get
                Throw New NotImplementedException()
            End Get
            Set(ByVal value As Nullable(Of Double))
                Throw New NotImplementedException()
            End Set
        End Property

        ''' <summary>
        ''' Phase collection, only implemented for Material Streams.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Xml.Serialization.XmlIgnore()> Public Overridable ReadOnly Property Phases() As Dictionary(Of Integer, Interfaces.IPhase)
            Get
                Throw New NotImplementedException
            End Get
        End Property

        ''' <summary>
        ''' Validates the object, checking its connections and other parameters.
        ''' </summary>
        ''' <remarks></remarks>
        Public Overridable Sub Validate() Implements Interfaces.ISimulationObject.Validate

            Dim vForm As Interfaces.IFlowsheet = FlowSheet
            Dim vCon As Interfaces.IConnectionPoint

            'Validate input connections.
            For Each vCon In Me.GraphicObject.InputConnectors
                If Not vCon.IsAttached Then
                    Throw New Exception(Me.FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
                End If
            Next

            'Validate output connections.
            For Each vCon In Me.GraphicObject.OutputConnectors
                If Not vCon.IsAttached Then
                    Throw New Exception(Me.FlowSheet.GetTranslatedString("Verifiqueasconexesdo"))
                End If
            Next

        End Sub

        Public Overridable Function GetDebugReport() As String Implements Interfaces.ISimulationObject.GetDebugReport
            Return "Error - function not implemented"
        End Function

        Public Sub AppendDebugLine(text As String) Implements Interfaces.ISimulationObject.AppendDebugLine
            DebugText += text & vbCrLf & vbCrLf
        End Sub

        ''' <summary>
        ''' Gets or sets the error message regarding the last calculation attempt.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property ErrorMessage() As String Implements Interfaces.ISimulationObject.ErrorMessage

        ''' <summary>
        ''' Checks if a value is valid.
        ''' </summary>
        ''' <param name="val">Value to be checked.</param>
        ''' <param name="onlypositive">Value should be a positive double or not.</param>
        ''' <param name="paramname">Name of the parameter (ex. P, T, W, H etc.)</param>
        ''' <remarks></remarks>
        Public Sub CheckSpec(val As Double, onlypositive As Boolean, paramname As String) Implements Interfaces.ISimulationObject.CheckSpec

            If Not val.IsValid Then Throw New ArgumentException(Me.FlowSheet.GetTranslatedString("ErrorInvalidUOSpecValue") & " (name: " & paramname & ", value: " & val & ")")
            If onlypositive Then If val.IsNegative Then Throw New ArgumentException(Me.FlowSheet.GetTranslatedString("ErrorInvalidUOSpecValue") & " (name: " & paramname & ", value: " & val & ")")

        End Sub

        Public Property Annotation() As String = "" Implements Interfaces.ISimulationObject.Annotation

        ''' <summary>
        ''' Checks if an Adjust operation is attached to this object.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property IsAdjustAttached() As Boolean = False Implements Interfaces.ISimulationObject.IsAdjustAttached

        ''' <summary>
        ''' If an Adjust object is attached to this object, returns its ID.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property AttachedAdjustId() As String = "" Implements Interfaces.ISimulationObject.AttachedAdjustId

        ''' <summary>
        ''' If an Adjust object is attached to this object, returns a variable describing how this object is used by it (manipulated, controlled or reference).
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property AdjustVarType() As Interfaces.Enums.AdjustVarType Implements Interfaces.ISimulationObject.AdjustVarType

        ''' <summary>
        ''' Checks if an Specification operation is attached to this object.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property IsSpecAttached() As Boolean = False Implements Interfaces.ISimulationObject.IsSpecAttached

        ''' <summary>
        ''' If an Specification object is attached to this object, returns its ID.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property AttachedSpecId() As String = "" Implements Interfaces.ISimulationObject.AttachedSpecId

        ''' <summary>
        ''' If an Specification object is attached to this object, returns a variable describing how this object is used by it (target or source).
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Property SpecVarType() As Interfaces.Enums.SpecVarType Implements Interfaces.ISimulationObject.SpecVarType

        ''' <summary>
        ''' Gets or sets the graphic object representation of this object in the flowsheet.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Xml.Serialization.XmlIgnore> Public Property GraphicObject() As Interfaces.IGraphicObject Implements Interfaces.ISimulationObject.GraphicObject

        ''' <summary>
        ''' Object's Unique ID (Name)
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks>This property is the same as the graphic object 'Name' property.</remarks>
        Public Property Name() As String Implements Interfaces.ISimulationObject.Name
            Get
                Return ComponentName
            End Get
            Set(value As String)
                ComponentName = value
            End Set
        End Property

        Public Overridable Function GetProperties(proptype As PropertyType) As String() Implements Interfaces.ISimulationObject.GetProperties

            For Each item In AttachedUtilities
                Return item.GetPropertyList().ToArray
            Next

            Return New String() {}

        End Function

        Public Overridable Function GetPropertyUnit(prop As String, Optional su As Interfaces.IUnitsOfMeasure = Nothing) As String Implements Interfaces.ISimulationObject.GetPropertyUnit

            For Each item In AttachedUtilities
                For Each prop1 In item.GetPropertyList()
                    If prop1 = prop Then Return item.GetPropertyUnits(prop)
                Next
            Next

            Return "NF"

        End Function

        Public Overridable Function GetPropertyValue(prop As String, Optional su As Interfaces.IUnitsOfMeasure = Nothing) As Object Implements Interfaces.ISimulationObject.GetPropertyValue

            For Each item In AttachedUtilities
                For Each prop1 In item.GetPropertyList()
                    If prop1 = prop Then Return item.GetPropertyValue(prop)
                Next
            Next

            Return Nothing

        End Function

        Public Overridable Function SetPropertyValue(prop As String, propval As Object, Optional su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean Implements Interfaces.ISimulationObject.SetPropertyValue

            For Each item In AttachedUtilities
                For Each prop1 In item.GetPropertyList()
                    If prop1 = prop Then
                        item.SetPropertyValue(prop, propval)
                        Exit For
                    End If
                Next
            Next

            Return False

        End Function

        Public Overridable Function GetDefaultProperties() As String() Implements ISimulationObject.GetDefaultProperties
            Return GetProperties(PropertyType.ALL)
        End Function

        Public Overridable Property PropertyPackage As IPropertyPackage Implements ISimulationObject.PropertyPackage

        Public Function GetFlowsheet() As IFlowsheet Implements ISimulationObject.GetFlowsheet
            Return FlowSheet
        End Function

#End Region

#Region "    ICloneable"

        ''' <summary>
        ''' Clones the current object, returning a new one with identical properties.
        ''' </summary>
        ''' <returns>An object of the same type with the same properties.</returns>
        ''' <remarks>Properties and fields marked with the 'NonSerializable' attribute aren't cloned.</remarks>
        Public Overridable Function Clone() As Object Implements System.ICloneable.Clone

            Return ObjectCopy(Me)

        End Function

        Function ObjectCopy(ByVal obj As UnitOperations.BaseClass) As Object

            Dim objMemStream As New MemoryStream(250000)
            Dim objBinaryFormatter As New BinaryFormatter(Nothing, New StreamingContext(StreamingContextStates.Clone))

            objBinaryFormatter.Serialize(objMemStream, obj)

            objMemStream.Seek(0, SeekOrigin.Begin)

            ObjectCopy = objBinaryFormatter.Deserialize(objMemStream)

            objMemStream.Close()

        End Function

#End Region

#Region "    IDisposable Support "

        Public disposedValue As Boolean = False        ' To detect redundant calls

        Protected Overridable Sub Dispose(ByVal disposing As Boolean)
            Me.disposedValue = True
        End Sub

        ' This code added by Visual Basic to correctly implement the disposable pattern.
        Public Sub Dispose() Implements IDisposable.Dispose

            ' Do not change this code.  Put cleanup code in Dispose(ByVal disposing As Boolean) above.
            Dispose(True)
            GC.SuppressFinalize(Me)

        End Sub
#End Region

#Region "    IXMLSerialization"

        ''' <summary>
        ''' Loads object data stored in a collection of XML elements.
        ''' </summary>
        ''' <param name="data"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Overridable Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)
            If Me.Annotation = "DWSIM.DWSIM.Outros.Annotation" Then Me.Annotation = ""
            Return True

        End Function

        ''' <summary>
        ''' Saves object data in a collection of XML elements.
        ''' </summary>
        ''' <returns>A List of XML elements containing object data.</returns>
        ''' <remarks></remarks>
        Public Overridable Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me)

            Return elements

        End Function

#End Region

#Region "    Extras"

        ''' <summary>
        ''' Copies the object properties to the Clipboard.
        ''' </summary>
        ''' <param name="su">Units system to use.</param>
        ''' <param name="nf">Number format to use.</param>
        ''' <remarks></remarks>
        Public Sub CopyDataToClipboard(su As SystemsOfUnits.Units, nf As String)

            Dim DT As New DataTable
            DT.Columns.Clear()
            DT.Columns.Add(("Propriedade"), GetType(System.String))
            DT.Columns.Add(("Valor"), GetType(System.String))
            DT.Columns.Add(("Unidade"), GetType(System.String))
            DT.Rows.Clear()

            Dim baseobj As BaseClass
            Dim properties() As String
            Dim description As String
            Dim objtype As ObjectType
            Dim propidx, r1, r2, r3, r4, r5, r6 As Integer
            r1 = 5
            r2 = 12
            r3 = 30
            r4 = 48
            r5 = 66
            r6 = 84

            baseobj = Me
            properties = baseobj.GetProperties(Interfaces.Enums.PropertyType.ALL)
            objtype = baseobj.GraphicObject.ObjectType
            description = Me.FlowSheet.GetTranslatedString(baseobj.GraphicObject.Description)
            If objtype = ObjectType.MaterialStream Then
                Dim value As String
                For propidx = 0 To r1 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                For propidx = r1 To r2 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString("FraomolarnaMistura"), "", ""})
                For Each subst As Interfaces.ICompound In CType(Me, Interfaces.IMaterialStream).Phases(0).Compounds.Values
                    DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(subst.Name), Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                Next
                For propidx = r2 To r3 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString("FraomolarnaPhaseVapor"), "", ""})
                For Each subst As Interfaces.ICompound In CType(Me, Interfaces.IMaterialStream).Phases(2).Compounds.Values
                    DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(subst.Name), Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                Next
                For propidx = r3 To r4 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString("FraomolarnaPhaseLquid"), "", ""})
                For Each subst As Interfaces.ICompound In CType(Me, Interfaces.IMaterialStream).Phases(1).Compounds.Values
                    DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(subst.Name), Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                Next
                For propidx = r4 To r5 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString("FraomolarnaPhaseLquid"), "", ""})
                For Each subst As Interfaces.ICompound In CType(Me, Interfaces.IMaterialStream).Phases(3).Compounds.Values
                    DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(subst.Name), Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                Next
                For propidx = r5 To r6 - 1
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString("FraomolarnaPhaseLquid"), "", ""})
                For Each subst As Interfaces.ICompound In CType(Me, Interfaces.IMaterialStream).Phases(4).Compounds.Values
                    DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(subst.Name), Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                Next
                For propidx = r6 To 101
                    value = baseobj.GetPropertyValue(properties(propidx), su)
                    If Double.TryParse(value, New Double) Then
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), Format(Double.Parse(value), nf), baseobj.GetPropertyUnit(properties(propidx), su)})
                    Else
                        DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(properties(propidx)), value, baseobj.GetPropertyUnit(properties(propidx), su)})
                    End If
                Next
                DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString("FraomolarnaPhaseLquid"), "", ""})
                For Each subst As Interfaces.ICompound In CType(Me, Interfaces.IMaterialStream).Phases(6).Compounds.Values
                    DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(subst.Name), Format(subst.MoleFraction.GetValueOrDefault, nf), ""})
                Next
            Else
                For Each prop As String In properties
                    DT.Rows.Add(New String() {Me.FlowSheet.GetTranslatedString(prop), Format(baseobj.GetPropertyValue(prop, su), nf), baseobj.GetPropertyUnit(prop, su)})
                Next
            End If

            Dim st As New StringBuilder(Me.FlowSheet.GetTranslatedString(Me.ComponentDescription) & ": " & Me.GraphicObject.Tag & vbCrLf)
            For Each r As DataRow In DT.Rows
                Dim l As String = ""
                For Each o As Object In r.ItemArray
                    l += o.ToString() & vbTab
                Next
                st.AppendLine(l)
            Next

            Clipboard.SetText(st.ToString())

            DT.Clear()
            DT.Dispose()
            DT = Nothing

        End Sub

        ''' <summary>
        ''' Formats a property string, adding its units in parenthesis.
        ''' </summary>
        ''' <param name="prop">Property string</param>
        ''' <param name="unit">Property units</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function FT(ByRef prop As String, ByVal unit As String)
            Return prop & " (" & unit & ")"
        End Function

        ''' <summary>
        ''' Sets the Flowsheet to which this object belongs to.
        ''' </summary>
        ''' <param name="flowsheet">Flowsheet instance.</param>
        ''' <remarks></remarks>
        Public Sub SetFlowsheet(ByVal flowsheet As Object) Implements Interfaces.ISimulationObject.SetFlowsheet
            m_flowsheet = flowsheet
        End Sub

        ''' <summary>
        ''' Gets the current flowsheet where this object is.
        ''' </summary>
        ''' <value></value>
        ''' <returns>Flowsheet instance.</returns>
        ''' <remarks></remarks>
        Public Overridable ReadOnly Property FlowSheet() As Interfaces.IFlowsheet
            Get
                If Not m_flowsheet Is Nothing Then
                    Return m_flowsheet
                Else
                    Return Nothing
                End If
            End Get
        End Property

#End Region

        Public MustOverride Sub CloseEditForm() Implements ISimulationObject.CloseEditForm

    End Class

    <System.Serializable()> <ComVisible(True)> Public MustInherit Class UnitOpBaseClass

        Inherits UnitOperations.BaseClass

        'CAPE-OPEN Unit Operation Support
        Implements ICapeIdentification, ICapeUnit, ICapeUtilities, ICapeUnitReport

        'CAPE-OPEN Persistence Interface
        Implements IPersistStreamInit

        'CAPE-OPEN Error Interfaces
        Implements ECapeUser, ECapeUnknown, ECapeRoot

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public _pp As Interfaces.IPropertyPackage
        Public _ppid As String = ""

        Protected _capeopenmode As Boolean = False

        Public Sub New()
            MyBase.CreateNew()
        End Sub

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)

            Try
                Me._ppid = (From xel As XElement In data Select xel Where xel.Name = "PropertyPackage").SingleOrDefault.Value
            Catch

            End Try

            Return True

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As List(Of XElement) = MyBase.SaveData()

            Dim ppid As String = ""
            If _ppid <> "" Then
                ppid = _ppid
            ElseIf Not _pp Is Nothing Then
                ppid = _pp.Name
            Else
                ppid = ""
            End If
            elements.Add(New XElement("PropertyPackage", ppid))

            Return elements

        End Function

#Region "   DWSIM Specific"

        Public Overrides Function GetDebugReport() As String

            Me.DebugMode = True
            Me.DebugText = ""

            Try

                Calculate(Nothing)

                Me.DebugText += vbCrLf & vbCrLf & "Calculated OK."

            Catch ex As Exception

                Dim st As New StackTrace(ex, True)
                Dim frame As StackFrame = st.GetFrame(0)
                Dim fileName As String = Path.GetFileName(frame.GetFileName)
                Dim methodName As String = frame.GetMethod().Name
                Dim line As Integer = frame.GetFileLineNumber()

                AppendDebugLine(String.Format("ERROR: exception raised on file {0}, method {1}, line {2}:", fileName, methodName, line))
                AppendDebugLine(ex.Message.ToString)

            Finally

                Me.DebugMode = False

            End Try

            Return DebugText

        End Function

        ''' <summary>
        ''' Gets or sets the property package associated with this object.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Xml.Serialization.XmlIgnore()> Public Overrides Property PropertyPackage() As Interfaces.IPropertyPackage
            Get
                If Not _pp Is Nothing Then Return _pp
                If _ppid Is Nothing Then _ppid = ""
                If FlowSheet.PropertyPackages.ContainsKey(_ppid) Then
                    Return FlowSheet.PropertyPackages(_ppid)
                Else
                    For Each pp As Interfaces.IPropertyPackage In Me.FlowSheet.PropertyPackages.Values
                        _ppid = pp.UniqueID
                        Return pp
                        Exit For
                    Next
                End If
                Return Nothing
            End Get
            Set(ByVal value As Interfaces.IPropertyPackage)
                If value IsNot Nothing Then
                    _ppid = value.UniqueID
                    _pp = value
                Else
                    _pp = Nothing
                End If
            End Set
        End Property

        ''' <summary>
        ''' Calculates the object.
        ''' </summary>
        ''' <param name="args"></param>
        ''' <remarks></remarks>
        Public Sub Solve(Optional ByVal args As Object = Nothing)

            Calculated = False

            Calculate(args)

            Calculated = True
            LastUpdated = Date.Now

        End Sub

        ''' <summary>
        ''' Decalculates the object.
        ''' </summary>
        ''' <remarks></remarks>
        Public Overridable Overloads Sub DeCalculate()
            MyBase.DeCalculate()
        End Sub

        ''' <summary>
        ''' Decalculates the object.
        ''' </summary>
        ''' <remarks></remarks>
        Public Sub Unsolve()

            DeCalculate()

            Calculated = False

        End Sub

        Sub CreateCOPorts()
            _ports = New CapeOpen.PortCollection()
            For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.InputConnectors
                Select Case c.Type
                    Case ConType.ConIn
                        _ports.Add(New UnitPort("Inlet" + Me.GraphicObject.InputConnectors.IndexOf(c).ToString, "", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                        With _ports(_ports.Count - 1)
                            If c.IsAttached Then
                                .Connect(Me.FlowSheet.GetFlowsheetSimulationObject(c.AttachedConnector.AttachedFrom.Tag))
                            End If
                        End With
                    Case ConType.ConEn
                        _ports.Add(New UnitPort("Inlet" + Me.GraphicObject.InputConnectors.IndexOf(c).ToString, "", CapePortDirection.CAPE_INLET, CapePortType.CAPE_ENERGY))
                        With _ports(_ports.Count - 1)
                            If c.IsAttached Then
                                .Connect(Me.FlowSheet.GetFlowsheetSimulationObject(c.AttachedConnector.AttachedFrom.Tag))
                            End If
                        End With
                End Select
            Next
            For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.OutputConnectors
                Select Case c.Type
                    Case ConType.ConOut
                        _ports.Add(New UnitPort("Outlet" + Me.GraphicObject.OutputConnectors.IndexOf(c).ToString, "", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                        With _ports(_ports.Count - 1)
                            If c.IsAttached Then
                                .Connect(Me.FlowSheet.GetFlowsheetSimulationObject(c.AttachedConnector.AttachedTo.Tag))
                            End If
                        End With
                    Case ConType.ConEn
                        _ports.Add(New UnitPort("Outlet" + Me.GraphicObject.OutputConnectors.IndexOf(c).ToString, "", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_ENERGY))
                        With _ports(_ports.Count - 1)
                            If c.IsAttached Then
                                .Connect(Me.FlowSheet.GetFlowsheetSimulationObject(c.AttachedConnector.AttachedTo.Tag))
                            End If
                        End With
                End Select
            Next
        End Sub

        Sub CreateCOParameters()
            _parameters = New CapeOpen.ParameterCollection
            Dim props() = Me.GetProperties(PropertyType.ALL)
            For Each s As String In props
                _parameters.Add(New CapeOpen.RealParameter(s, Me.GetPropertyValue(s), 0.0#, Me.GetPropertyUnit(s)))
                With _parameters.Item(_parameters.Count - 1)
                    .Mode = CapeParamMode.CAPE_OUTPUT
                End With
            Next
        End Sub

#End Region

#Region "   CAPE-OPEN Unit Operation internal support"

        Protected _ports As CapeOpen.PortCollection
        Protected _parameters As CapeOpen.ParameterCollection
        Protected _simcontext As Object = Nothing

        ''' <summary>
        ''' Calculates the Unit Operation.
        ''' </summary>
        ''' <remarks>The Flowsheet Unit performs its calculation, that is, computes the variables that are missing at
        ''' this stage in the complete description of the input and output streams and computes any public
        ''' parameter value that needs to be displayed. Calculate will be able to do progress monitoring
        ''' and checks for interrupts as required using the simulation context. At present, there are no
        ''' standards agreed for this.
        ''' It is recommended that Flowsheet Units perform a suitable flash calculation on all output
        ''' streams. In some cases a Simulation Executive will be able to perform a flash calculation but
        ''' the writer of a Flowsheet Unit is in the best position to decide the correct flash to use.
        ''' Before performing the calculation, this method should perform any final validation tests that
        ''' are required. For example, at this point the validity of Material Objects connected to ports can
        ''' be checked.
        ''' There are no input or output arguments for this method.</remarks>
        Public Overridable Overloads Sub Calculate1() Implements CapeOpen.ICapeUnit.Calculate
            'do CAPE calculation here
            MyBase.Calculate()
        End Sub

        ''' <summary>
        ''' Return an interface to a collection containing the list of unit ports (e.g. ICapeUnitCollection).
        ''' </summary>
        ''' <value></value>
        ''' <returns>A reference to the interface on the collection containing the specified ports</returns>
        ''' <remarks>Return the collection of unit ports (i.e. ICapeUnitCollection). These are delivered as a
        ''' collection of elements exposing the interfaces ICapeUnitPort</remarks>
        <Xml.Serialization.XmlIgnore()> Public ReadOnly Property ports() As Object Implements CapeOpen.ICapeUnit.ports
            Get
                If Not Me._capeopenmode Then
                    If Not Me.GraphicObject.ObjectType = ObjectType.CapeOpenUO Then
                        _ports = New CapeOpen.PortCollection
                        For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.InputConnectors
                            If c.Type = ConType.ConIn Then
                                _ports.Add(New UnitPort(c.ConnectorName, "", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                            ElseIf c.Type = ConType.ConEn Then
                                _ports.Add(New UnitPort(c.ConnectorName, "", CapePortDirection.CAPE_INLET, CapePortType.CAPE_ENERGY))
                            End If
                            With _ports(_ports.Count - 1)
                                If c.IsAttached And Not c.AttachedConnector Is Nothing Then .Connect(Me.FlowSheet.SimulationObjects(c.AttachedConnector.AttachedFrom.Name))
                            End With
                        Next
                        For Each c As Interfaces.IConnectionPoint In Me.GraphicObject.OutputConnectors
                            If c.Type = ConType.ConOut Then
                                _ports.Add(New UnitPort(c.ConnectorName, "", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                            ElseIf c.Type = ConType.ConEn Then
                                _ports.Add(New UnitPort(c.ConnectorName, "", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_ENERGY))
                            End If
                            With _ports(_ports.Count - 1)
                                If c.IsAttached And Not c.AttachedConnector Is Nothing Then .Connect(Me.FlowSheet.SimulationObjects(c.AttachedConnector.AttachedTo.Name))
                            End With
                        Next
                    End If
                End If
                Return _ports
            End Get
        End Property

        ''' <summary>Validates the Unit Operation.</summary>
        ''' <param name="message">An optional message describing the cause of the validation failure.</param>
        ''' <returns>TRUE if the Unit is valid</returns>
        ''' <remarks>Sets the flag that indicates whether the Flowsheet Unit is valid by validating the ports and
        ''' parameters of the Flowsheet Unit. For example, this method could check that all mandatory
        ''' ports have connections and that the values of all parameters are within bounds.
        ''' Note that the Simulation Executive can call the Validate routine at any time, in particular it
        ''' may be called before the executive is ready to call the Calculate method. This means that
        ''' Material Objects connected to unit ports may not be correctly configured when Validate is
        ''' called. The recommended approach is for this method to validate parameters and ports but not
        ''' Material Object configuration. A second level of validation to check Material Objects can be
        ''' implemented as part of Calculate, when it is reasonable to expect that the Material Objects
        ''' connected to ports will be correctly configured.</remarks>
        Public Overridable Function Validate1(ByRef message As String) As Boolean Implements CapeOpen.ICapeUnit.Validate
            'do CAPE validation here
            message = "Validation OK"
            Return True
        End Function

        ''' <summary>
        ''' Get the flag that indicates whether the Flowsheet Unit is valid (e.g. some parameter values
        ''' have changed but they have not been validated by using Validate). It has three possible
        ''' values:
        ''' ·  notValidated(CAPE_NOT_VALIDATED): the unit’s validate() method has not been
        ''' called since the last operation that could have changed the validation status of the unit, for
        ''' example an update to a parameter value of a connection to a port.
        ''' ·  invalid(CAPE_INVALID): the last time the unit’s validate() method was called it returned
        ''' false.
        ''' ·  valid(CAPE_VALID): the last time the unit’s validate() method was called it returned true.
        ''' </summary>
        ''' <value>CAPE_VALID meaning the Validate method returned success; CAPE_INVALID meaing the Validate 
        ''' method returned failure; CAPE_NOT_VALIDATED meaning that the Validate method needs to be called 
        ''' to determine whether the unit is valid or not.</value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Overridable ReadOnly Property ValStatus() As CapeOpen.CapeValidationStatus Implements CapeOpen.ICapeUnit.ValStatus
            Get
                Return CapeValidationStatus.CAPE_VALID
            End Get
        End Property

        ''' <summary>
        ''' The PMC displays its user interface and allows the Flowsheet User to interact with it. If no user interface is
        ''' available it returns an error.</summary>
        ''' <remarks></remarks>
        Public Overridable Sub Edit1() Implements CapeOpen.ICapeUtilities.Edit
            Throw New CapeNoImplException("ICapeUtilities.Edit() Method not implemented.")
        End Sub

        ''' <summary>
        ''' Initially, this method was only present in the ICapeUnit interface. Since ICapeUtilities.Initialize is now
        ''' available for any kind of PMC, ICapeUnit. Initialize is deprecated.
        ''' The PME will order the PMC to get initialized through this method. Any initialisation that could fail must be
        ''' placed here. Initialize is guaranteed to be the first method called by the client (except low level methods such
        ''' as class constructors or initialization persistence methods). Initialize has to be called once when the PMC is
        ''' instantiated in a particular flowsheet.
        ''' When the initialization fails, before signalling an error, the PMC must free all the resources that were
        ''' allocated before the failure occurred. When the PME receives this error, it may not use the PMC anymore.
        ''' The method terminate of the current interface must not either be called. Hence, the PME may only release
        ''' the PMC through the middleware native mechanisms.
        ''' </summary>
        ''' <remarks></remarks>
        Public Overridable Sub Initialize() Implements CapeOpen.ICapeUtilities.Initialize
            'do CAPE Initialization here
            CreateCOPorts()
            CreateCOParameters()
        End Sub

        ''' <summary>
        ''' Returns an ICapeCollection interface.
        ''' </summary>
        ''' <value></value>
        ''' <returns>CapeInterface (ICapeCollection)</returns>
        ''' <remarks>This interface will contain a collection of ICapeParameter interfaces.
        ''' This method allows any client to access all the CO Parameters exposed by a PMC. Initially, this method was
        ''' only present in the ICapeUnit interface. Since ICapeUtilities.GetParameters is now available for any kind of
        ''' PMC, ICapeUnit.GetParameters is deprecated. Consult the “Open Interface Specification: Parameter
        ''' Common Interface” document for more information about parameter. Consult the “Open Interface
        ''' Specification: Collection Common Interface” document for more information about collection.
        ''' If the PMC does not support exposing its parameters, it should raise the ECapeNoImpl error, instead of
        ''' returning a NULL reference or an empty Collection. But if the PMC supports parameters but has for this call
        ''' no parameters, it should return a valid ICapeCollection reference exposing zero parameters.</remarks>
        <Xml.Serialization.XmlIgnore()> Public ReadOnly Property parameters() As Object Implements CapeOpen.ICapeUtilities.parameters
            Get
                Return _parameters
            End Get
        End Property

        ''' <summary>
        ''' Allows the PME to convey the PMC a reference to the former’s simulation context. 
        ''' </summary>
        ''' <value>The reference to the PME’s simulation context class. For the PMC to
        ''' use this class, this reference will have to be converted to each of the
        ''' defined CO Simulation Context interfaces.</value>
        ''' <remarks>The simulation context
        ''' will be PME objects which will expose a given set of CO interfaces. Each of these interfaces will allow the
        ''' PMC to call back the PME in order to benefit from its exposed services (such as creation of material
        ''' templates, diagnostics or measurement unit conversion). If the PMC does not support accessing the
        ''' simulation context, it is recommended to raise the ECapeNoImpl error.
        ''' Initially, this method was only present in the ICapeUnit interface. Since ICapeUtilities.SetSimulationContext
        ''' is now available for any kind of PMC, ICapeUnit. SetSimulationContext is deprecated.</remarks>
        <Xml.Serialization.XmlIgnore()> Public WriteOnly Property simulationContext() As Object Implements CapeOpen.ICapeUtilities.simulationContext
            Set(ByVal value As Object)
                _simcontext = value
            End Set
        End Property

        ''' <summary>
        ''' Initially, this method was only present in the ICapeUnit interface. Since ICapeUtilities.Terminate is now
        ''' available for any kind of PMC, ICapeUnit.Terminate is deprecated.
        ''' The PME will order the PMC to get destroyed through this method. Any uninitialization that could fail must
        ''' be placed here. ‘Terminate’ is guaranteed to be the last method called by the client (except low level methods
        ''' such as class destructors). ‘Terminate’ may be called at any time, but may be only called once.
        ''' When this method returns an error, the PME should report the user. However, after that the PME is not
        ''' allowed to use the PMC anymore.
        ''' The Unit specification stated that “Terminate may check if the data has been saved and return an error if
        ''' not.” It is suggested not to follow this recommendation, since it’s the PME responsibility to save the state of
        ''' the PMC before terminating it. In the case that a user wants to close a simulation case without saving it, it’s
        ''' better to leave the PME to handle the situation instead of each PMC providing a different implementation.
        ''' </summary>
        ''' <remarks></remarks>
        Public Overridable Sub Terminate1() Implements CapeOpen.ICapeUtilities.Terminate
            If Not _simcontext Is Nothing Then
                If IsComObject(_simcontext) Then
                    ReleaseComObject(_simcontext)
                Else
                    _simcontext = Nothing
                End If
            End If
            Me.Dispose()
        End Sub

#End Region

#Region "   CAPE-OPEN Persistence Implementation"

        Protected m_dirty As Boolean = True

        Public Sub GetClassID(ByRef pClassID As System.Guid) Implements IPersistStreamInit.GetClassID
            pClassID = New Guid(UnitOpBaseClass.ClassId)
        End Sub

        Public Sub GetSizeMax(ByRef pcbSize As Long) Implements IPersistStreamInit.GetSizeMax
            pcbSize = 1024 * 1024
        End Sub

        Public Sub InitNew() Implements IPersistStreamInit.InitNew
            'do nothing
        End Sub

        Public Function IsDirty() As Integer Implements IPersistStreamInit.IsDirty
            Return m_dirty
        End Function

        Public Overridable Sub Load(ByVal pStm As System.Runtime.InteropServices.ComTypes.IStream) Implements IPersistStreamInit.Load

            ' Read the length of the string  
            Dim arrLen As Byte() = New [Byte](3) {}
            pStm.Read(arrLen, arrLen.Length, IntPtr.Zero)

            ' Calculate the length  
            Dim cb As Integer = BitConverter.ToInt32(arrLen, 0)

            ' Read the stream to get the string    
            Dim bytes As Byte() = New Byte(cb - 1) {}
            Dim pcb As New IntPtr()
            pStm.Read(bytes, bytes.Length, pcb)
            If System.Runtime.InteropServices.Marshal.IsComObject(pStm) Then System.Runtime.InteropServices.Marshal.ReleaseComObject(pStm)

            ' Deserialize byte array    

            Dim memoryStream As New System.IO.MemoryStream(bytes)

            Try

                Dim domain As AppDomain = AppDomain.CurrentDomain
                AddHandler domain.AssemblyResolve, New ResolveEventHandler(AddressOf MyResolveEventHandler)

                Dim myarr As ArrayList

                Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
                myarr = mySerializer.Deserialize(memoryStream)

                Me._parameters = myarr(0)
                Me._ports = myarr(1)

                myarr = Nothing
                mySerializer = Nothing

                RemoveHandler domain.AssemblyResolve, New ResolveEventHandler(AddressOf MyResolveEventHandler)

            Catch p_Ex As System.Exception

                System.Windows.Forms.MessageBox.Show(p_Ex.ToString())

            End Try

            memoryStream.Close()

        End Sub

        Public Overridable Sub Save(ByVal pStm As System.Runtime.InteropServices.ComTypes.IStream, ByVal fClearDirty As Boolean) Implements IPersistStreamInit.Save

            Dim props As New ArrayList

            With props

                .Add(_parameters)
                .Add(_ports)

            End With

            Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
            Dim mstr As New MemoryStream
            mySerializer.Serialize(mstr, props)
            Dim bytes As Byte() = mstr.ToArray()
            mstr.Close()

            ' construct length (separate into two separate bytes)    

            Dim arrLen As Byte() = BitConverter.GetBytes(bytes.Length)
            Try

                ' Save the array in the stream    
                pStm.Write(arrLen, arrLen.Length, IntPtr.Zero)
                pStm.Write(bytes, bytes.Length, IntPtr.Zero)
                If System.Runtime.InteropServices.Marshal.IsComObject(pStm) Then System.Runtime.InteropServices.Marshal.ReleaseComObject(pStm)

            Catch p_Ex As System.Exception

                System.Windows.Forms.MessageBox.Show(p_Ex.ToString())

            End Try

            If fClearDirty Then
                m_dirty = False
            End If

        End Sub

        Protected Function MyResolveEventHandler(ByVal sender As Object, ByVal args As ResolveEventArgs) As System.Reflection.Assembly
            Return Me.[GetType]().Assembly
        End Function

#End Region

#Region "   CAPE-OPEN Error Interfaces"

        Sub ThrowCAPEException(ByRef ex As Exception, ByVal name As String, ByVal description As String, ByVal interf As String, ByVal moreinfo As String, ByVal operation As String, ByVal scope As String, ByVal code As Integer)

            _code = code
            _description = description
            _interfacename = interf
            _moreinfo = moreinfo
            _operation = operation
            _scope = scope

            Throw ex

        End Sub

        Private _name, _description, _interfacename, _moreinfo, _operation, _scope As String, _code As Integer

        Public ReadOnly Property Name2() As String Implements CapeOpen.ECapeRoot.Name
            Get
                Return Me.Name
            End Get
        End Property

        Public ReadOnly Property code() As Integer Implements CapeOpen.ECapeUser.code
            Get
                Return _code
            End Get
        End Property

        Public ReadOnly Property description() As String Implements CapeOpen.ECapeUser.description
            Get
                Return _description
            End Get
        End Property

        Public ReadOnly Property interfaceName() As String Implements CapeOpen.ECapeUser.interfaceName
            Get
                Return _interfacename
            End Get
        End Property

        Public ReadOnly Property moreInfo() As String Implements CapeOpen.ECapeUser.moreInfo
            Get
                Return _moreinfo
            End Get
        End Property

        Public ReadOnly Property operation() As String Implements CapeOpen.ECapeUser.operation
            Get
                Return _operation
            End Get
        End Property

        Public ReadOnly Property scope1() As String Implements CapeOpen.ECapeUser.scope
            Get
                Return _scope
            End Get
        End Property

#End Region

#Region "   CAPE-OPEN Reports"

        Protected _reports As String() = New String() {"log", "last run", "validation results"}
        Protected _selreport As String = ""
        Protected _calclog As String = ""
        Protected _lastrun As String = ""
        Protected _valres As String = ""

        Public Sub ProduceReport(ByRef message As String) Implements CapeOpen.ICapeUnitReport.ProduceReport
            Select Case _selreport
                Case "log"
                    message = _calclog
                Case "last run"
                    message = _lastrun
                Case "validation results"
                    message = _valres
            End Select
        End Sub

        Public ReadOnly Property reports() As Object Implements CapeOpen.ICapeUnitReport.reports
            Get
                Return _reports
            End Get
        End Property

        Public Property selectedReport() As String Implements CapeOpen.ICapeUnitReport.selectedReport
            Get
                Return _selreport
            End Get
            Set(ByVal value As String)
                _selreport = value
            End Set
        End Property

#End Region

    End Class

    <System.Serializable()> Public MustInherit Class SpecialOpBaseClass

        Inherits BaseClass

        Public Sub New()
            MyBase.CreateNew()
        End Sub

    End Class

End Namespace

Namespace SpecialOps.Helpers

    <System.Serializable()> Public Class SpecialOpObjectInfo

        Implements ISpecialOpObjectInfo

        Sub New()

        End Sub

        Public Property ID As String = "" Implements ISpecialOpObjectInfo.ID

        Public Property Name As String = "" Implements ISpecialOpObjectInfo.Name

        Public Property PropertyName As String = "" Implements ISpecialOpObjectInfo.PropertyName

        Public Property Type As String = "" Implements ISpecialOpObjectInfo.Type

    End Class

End Namespace

