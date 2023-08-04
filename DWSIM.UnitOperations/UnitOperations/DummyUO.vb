'    ISO 5167 Orifice Plate Calculation Routines 
'    Copyright 2010 Daniel Wagner O. de Medeiros
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

Imports DWSIM.Interfaces.Enums

Namespace UnitOperations

    <System.Serializable()> Public Class DummyUnitOperation

        Inherits UnitOperations.UnitOpBaseClass

        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.Other

        Public Overrides ReadOnly Property SupportsDynamicMode As Boolean = False

        Public Overrides ReadOnly Property HasPropertiesForDynamicMode As Boolean = False

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New DummyUnitOperation()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of DummyUnitOperation)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Sub New()
            MyBase.New()
        End Sub

        Public Overrides Sub RunDynamicModel()


        End Sub

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)



        End Sub

        Public Overrides Sub DeCalculate()

        End Sub

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.square_240px
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ""
        End Function

        Public Overrides Function GetDisplayName() As String
            Return "Dummy Unit Operation"
        End Function

        Public Overrides Sub CloseEditForm()

        End Sub

        Public Overrides Sub DisplayEditForm()

        End Sub

        Public Overrides Sub UpdateEditForm()

        End Sub

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

    End Class

End Namespace


