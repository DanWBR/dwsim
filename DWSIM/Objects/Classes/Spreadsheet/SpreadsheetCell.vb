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
Imports System.IO
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Runtime.Serialization

Namespace DWSIM.Utilities.Spreadsheet
    Public Enum VarType
        Read = 0
        Write = 1
        Expression = 2
    End Enum

    <System.Serializable()> Public Class SpreadsheetCellParameters

        Implements ICloneable, Interfaces.ICustomXMLSerialization

        Public CellType As VarType = VarType.Expression
        Public RelativeTolerance As Double = 0.01
        Public ObjectID As String = ""
        Public PropID As String = ""
        Public PropUnit As String = ""
        Public Expression As String = ""
        Public CurrVal As String = ""
        Public PrevVal As String = ""
        Public CalcOrder As Integer = 0
        Public References As List(Of String)
        Public ToolTipText As String = ""

        Sub New()
            References = New List(Of String)
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

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData
            XMLSerializer.XMLSerializer.Deserialize(Me, data, True)
            ToolTipText = Xml.XmlConvert.DecodeName(ToolTipText)
        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData
            ToolTipText = Xml.XmlConvert.EncodeName(ToolTipText)
            If Expression <> "" Then
                Return XMLSerializer.XMLSerializer.Serialize(Me, True)
            Else
                Return New List(Of XElement)
            End If
        End Function
    End Class

End Namespace

