'    Sensitivity Analysis Classes
'    Copyright 2009-2014 Daniel Wagner O. de Medeiros
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

Imports System.Xml
Imports DWSIM.DWSIM.SimulationObjects.PropertyPackages
Imports DWSIM.DWSIM.SimulationObjects.Streams
Imports DWSIM.DWSIM.Thermodynamics.BaseClasses
Imports System.IO
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Runtime.Serialization
Imports Ciloci.Flee
Imports System.Linq

Namespace DWSIM.Optimization

    <System.Serializable()> Public Class SensitivityAnalysisCase

        Implements ICloneable, XMLSerializer.Interfaces.ICustomXMLSerialization

        Public iv1 As New SAVariable
        Public iv2 As New SAVariable
        Public dv As New SAVariable

        Public name As String = ""
        Public description As String = ""
        <Xml.Serialization.XmlIgnore()> Public results As ArrayList
        Public stats As String = ""
        Public numvar As Integer = 1

        Public depvartype As SADependentVariableType = SADependentVariableType.Variable

        Public expression As String = ""
        <System.NonSerialized()> Public exbase As IGenericExpression(Of Double)
        <System.NonSerialized()> Public econtext As ExpressionContext

        Public variables As New Dictionary(Of String, SAVariable)
        Public depvariables As New Dictionary(Of String, SAVariable)

        Sub New()
            iv1 = New SAVariable
            iv2 = New SAVariable
            dv = New SAVariable
            variables = New Dictionary(Of String, SAVariable)
            depvariables = New Dictionary(Of String, SAVariable)
            results = New ArrayList()
        End Sub

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Return ObjectCopy(Me)

        End Function

        Function ObjectCopy(ByVal obj As Object) As Object

            Dim objMemStream As New MemoryStream(50000)
            Dim objBinaryFormatter As New BinaryFormatter(Nothing, New StreamingContext(StreamingContextStates.Clone))

            objBinaryFormatter.Serialize(objMemStream, obj)

            objMemStream.Seek(0, SeekOrigin.Begin)

            ObjectCopy = objBinaryFormatter.Deserialize(objMemStream)

            objMemStream.Close()

        End Function

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data, True)

            Dim xel As XElement = (From xel2 As XElement In data Select xel2 Where xel2.Name = "IV1").SingleOrDefault
            iv1.LoadData(xel.Elements.ToList)
            xel = (From xel2 As XElement In data Select xel2 Where xel2.Name = "IV2").SingleOrDefault
            iv2.LoadData(xel.Elements.ToList)
            xel = (From xel2 As XElement In data Select xel2 Where xel2.Name = "DV").SingleOrDefault
            dv.LoadData(xel.Elements.ToList)

            For Each xel0 As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "Variables").SingleOrDefault.Elements.ToList
                Dim savar As New SAVariable
                savar.LoadData(xel0.Elements.ToList)
                variables.Add(xel0.@Key, savar)
            Next

            For Each xel0 As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "DepVariables").SingleOrDefault.Elements.ToList
                Dim savar As New SAVariable
                savar.LoadData(xel0.Elements.ToList)
                depvariables.Add(xel0.@Key, savar)
            Next

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As List(Of XElement) = XMLSerializer.XMLSerializer.Serialize(Me, True)

            With elements
                .Add(New XElement("IV1", iv1.SaveData.ToArray))
                .Add(New XElement("IV2", iv2.SaveData.ToArray))
                .Add(New XElement("DV", dv.SaveData.ToArray))
                .Add(New XElement("Variables"))
                For Each kvp As KeyValuePair(Of String, SAVariable) In variables
                    .Item(.Count - 1).Add(New XElement("Variable", New XAttribute("Key", kvp.Key), kvp.Value.SaveData.ToArray))
                Next
                .Add(New XElement("DepVariables"))
                For Each kvp As KeyValuePair(Of String, SAVariable) In depvariables
                    .Item(.Count - 1).Add(New XElement("Variable", New XAttribute("Key", kvp.Key), kvp.Value.SaveData.ToArray))
                Next
            End With

            Return elements

        End Function

    End Class

    <System.Serializable()> Public Class SAVariable

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

        Public objectID As String = ""
        Public objectTAG As String = ""
        Public propID As String = ""
        Public unit As String = ""
        Public points As Integer = 0
        Public name As String = ""
        Public id As String = ""
        Public currentvalue As Double = 0.0#
        Public lowerlimit As Nullable(Of Double)
        Public upperlimit As Nullable(Of Double)

        Sub New()
            points = 5
        End Sub

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData
            XMLSerializer.XMLSerializer.Deserialize(Me, data, True)
        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData
            Return XMLSerializer.XMLSerializer.Serialize(Me, True)
        End Function

    End Class

    Public Enum SADependentVariableType
        Variable = 0
        Expression = 1
    End Enum

End Namespace
