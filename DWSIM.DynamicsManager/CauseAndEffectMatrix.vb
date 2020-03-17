'    DWSIM Dynamics Library
'    Copyright 2020 Daniel Wagner O. de Medeiros
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

Imports DWSIM.Interfaces

Public Class CauseAndEffectMatrix

    Implements IDynamicsCauseAndEffectMatrix, ICustomXMLSerialization

    Public Property ID As String Implements IDynamicsCauseAndEffectMatrix.ID

    Public Property Description As String Implements IDynamicsCauseAndEffectMatrix.Description

    Public Property Items As Dictionary(Of String, IDynamicsCauseAndEffectItem) = New Dictionary(Of String, IDynamicsCauseAndEffectItem) Implements IDynamicsCauseAndEffectMatrix.Items

    Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData
        Dim data = XMLSerializer.XMLSerializer.Serialize(Me)
        Dim e1 = New XElement("Items")
        For Each kvp As KeyValuePair(Of String, IDynamicsCauseAndEffectItem) In Items
            e1.Add(New XElement(kvp.Key,
                                DirectCast(kvp.Value, ICustomXMLSerialization).SaveData))
        Next
        data.Add(e1)
        Return data
    End Function

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData
        XMLSerializer.XMLSerializer.Deserialize(Me, data)
        Dim elm As XElement = (From xel2 As XElement In data Select xel2 Where xel2.Name = "Items").LastOrDefault
        If Not elm Is Nothing Then
            Items = New Dictionary(Of String, IDynamicsCauseAndEffectItem)
            For Each xel2 As XElement In elm.Elements
                Dim cei = New CauseAndEffectItem
                DirectCast(cei, ICustomXMLSerialization).LoadData(xel2.Elements)
                Items.Add(cei.ID, cei)
            Next
        End If
        Return True
    End Function

End Class
