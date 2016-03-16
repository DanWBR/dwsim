'    Optimization Classes
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
Imports DWSIM.DWSIM.ClassesBasicasTermodinamica
Imports System.IO
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Runtime.Serialization
Imports Ciloci.Flee
Imports System.Linq

Namespace DWSIM.Optimization

    <System.Serializable()> Public Class OptimizationCase

        Implements ICloneable, XMLSerializer.Interfaces.ICustomXMLSerialization

        Public name As String = ""
        Public description As String = ""
        <Xml.Serialization.XmlIgnore()> Public results As ArrayList
        Public stats As String = ""

        Public solvm As SolvingMethod = SolvingMethod.AL_BRENT
        Public maxits As Integer = 100
        Public tolerance As Double = 0.000001
        Public epsX As Double = 0.001
        Public epsF As Double = 0.001
        Public epsG As Double = 0.001
        Public epsilon As Double = 0.001
        Public barriermultiplier As Double = 0.0001
        Public numdevscheme = 2

        Public boundvariables As Boolean = False

        Public objfunctype As OPTObjectiveFunctionType = OPTObjectiveFunctionType.Variable
        Public type As OPTType = OPTType.Minimization

        Public expression As String = ""
        <System.NonSerialized()> Public exbase As IGenericExpression(Of Double)
        <System.NonSerialized()> Public econtext As ExpressionContext

        Public variables As New Dictionary(Of String, OPTVariable)

        Public Enum SolvingMethod
            AL_BRENT = 0
            AL_BRENT_B = 1
            AL_LBFGS = 2
            AL_LBFGS_B = 3
            DN_TRUNCATED_NEWTON = 4
            DN_NELDERMEAD_SIMPLEX = 5
            DN_LBFGS = 6
            DN_TRUNCATED_NEWTON_B = 7
            DN_NELDERMEAD_SIMPLEX_B = 8
            DN_LBFGS_B = 9
            IPOPT = 10
        End Enum

        Sub New()

            variables = New Dictionary(Of String, OPTVariable)
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

            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "Variables").SingleOrDefault.Elements.ToList
                Dim optvar As New OPTVariable
                optvar.LoadData(xel.Elements.ToList)
                variables.Add(xel.@Key, optvar)
            Next

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As List(Of XElement) = XMLSerializer.XMLSerializer.Serialize(Me, True)

            With elements
                .Add(New XElement("Variables"))
                For Each kvp As KeyValuePair(Of String, OPTVariable) In variables
                    .Item(.Count - 1).Add(New XElement("Variable", New XAttribute("Key", kvp.Key), kvp.Value.SaveData.ToArray))
                Next
            End With

            Return elements

        End Function

    End Class

    <System.Serializable()> Public Class OPTVariable

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

        Public objectID As String = ""
        Public objectTAG As String = ""
        Public propID As String = ""
        Public unit As String = ""
        Public name As String = ""
        Public id As String = ""
        Public lowerlimit As Nullable(Of Double)
        Public upperlimit As Nullable(Of Double)
        Public currentvalue As Double = 0.0#
        Public initialvalue As Double = 0.0#
        Public type As OPTVariableType = OPTVariableType.Independent
        Public boundtype As BoundType = boundtype.None

        Sub New()

        End Sub

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData
            XMLSerializer.XMLSerializer.Deserialize(Me, data, True)
        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData
            Return XMLSerializer.XMLSerializer.Serialize(Me, True)
        End Function

    End Class

    Public Enum OPTVariableType
        Dependent = 0
        Independent = 1
        Auxiliary = 2
        Constraint = 3
    End Enum

    Public Enum OPTObjectiveFunctionType
        Variable = 0
        Expression = 1
    End Enum

    Public Enum OPTType
        Minimization = 0
        Maximization = 1
    End Enum

    Public Enum BoundType
        None = 0
        Lower = 1
        Upper = 3
        LowerAndUpper = 2
    End Enum

End Namespace
