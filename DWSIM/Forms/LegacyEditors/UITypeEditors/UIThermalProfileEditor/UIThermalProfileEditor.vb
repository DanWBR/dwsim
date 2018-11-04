'    UITypeEditor for Pipe Thermal Profile
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

Imports System.Windows.Forms.Design
Imports System.Drawing.Design
Imports System.ComponentModel

Namespace DWSIM.Editors.PipeEditor

    Public Enum ThermalProfileType
        Definir_CGTC
        Definir_Q
        Estimar_CGTC
    End Enum

    <System.Serializable()> Public Class UIThermalProfileEditor

        Inherits System.Drawing.Design.UITypeEditor

        Private editorService As IWindowsFormsEditorService

        Public Overrides Function GetEditStyle(ByVal context As System.ComponentModel.ITypeDescriptorContext) As UITypeEditorEditStyle
            Return UITypeEditorEditStyle.Modal
        End Function

        Public Overrides Function EditValue(ByVal context As System.ComponentModel.ITypeDescriptorContext, ByVal provider As IServiceProvider, ByVal value As Object) As Object

            If (provider IsNot Nothing) Then
                editorService = CType(provider.GetService(GetType(IWindowsFormsEditorService)), _
                IWindowsFormsEditorService)
            End If

            If (editorService IsNot Nothing) Then

                Dim selectionControl As New Global.DWSIM.UnitOperations.PipeThermalProfileEditor

                selectionControl.PipeOp = value
                selectionControl.Dock = DockStyle.Fill

                Dim f As New Form With {.Width = 300, .Height = 500}
                f.Controls.Add(selectionControl)
                f.Size = selectionControl.Size
                f.Text = "Thermal Profile Editor"
                f.FormBorderStyle = FormBorderStyle.SizableToolWindow

                editorService.ShowDialog(f)

            End If

            Return value

        End Function

    End Class

    <System.Serializable()> Public Class ThermalProfileEditorConverter
        Inherits System.ComponentModel.TypeConverter

        Public Overloads Overrides Function CanConvertFrom(ByVal context As ITypeDescriptorContext, ByVal sourceType As Type) As Boolean
            Return MyBase.CanConvertFrom(context, sourceType)
        End Function

        ' Overrides the ConvertFrom method of TypeConverter.
        Public Overloads Overrides Function ConvertFrom(ByVal context As ITypeDescriptorContext, ByVal culture As Globalization.CultureInfo, ByVal value As Object) As Object
            'If TypeOf value Is String Then
            '    Dim v As String() = CStr(value).Split(New Char() {","c})
            '    Return New Point(Integer.Parse(v(0)), Integer.Parse(v(1)))
            'End If
            Return MyBase.ConvertFrom(context, culture, value)
        End Function

        ' Overrides the ConvertTo method of TypeConverter.
        Public Overloads Overrides Function ConvertTo(ByVal context As ITypeDescriptorContext, ByVal culture As Globalization.CultureInfo, ByVal value As Object, ByVal destinationType As Type) As Object
            'If destinationType Is GetType(String) Then
            '    Return CType(value, Point).X & "," & CType(value, Point).Y
            'End If
            Return MyBase.ConvertTo(context, culture, value, destinationType)
        End Function

    End Class

    <System.Serializable()> Public Class ThermalEditorDefinitions

        Implements Interfaces.ICustomXMLSerialization

        Protected m_type As ThermalProfileType = ThermalProfileType.Definir_CGTC
        Protected m_cgtc_definido, m_temp_amb_definir, m_calor_trocado, m_temp_amb_estimar, _
                    m_condtermica, m_espessura, m_velocidade As Double
        Protected m_material As Integer = 4
        Protected m_meio As Integer = 0
        Protected m_incluir_paredes, m_incluir_cti, m_incluir_cte, m_incluir_isolamento As Boolean

        Public Sub New()
            With Me
                .m_temp_amb_definir = 298.15
                .m_temp_amb_estimar = 298.15
            End With
        End Sub

        Public Property Incluir_isolamento() As Boolean
            Get
                Return m_incluir_isolamento
            End Get
            Set(ByVal value As Boolean)
                m_incluir_isolamento = value
            End Set
        End Property

        Public Property Incluir_cte() As Boolean
            Get
                Return m_incluir_cte
            End Get
            Set(ByVal value As Boolean)
                m_incluir_cte = value
            End Set
        End Property

        Public Property Incluir_cti() As Boolean
            Get
                Return m_incluir_cti
            End Get
            Set(ByVal value As Boolean)
                m_incluir_cti = value
            End Set
        End Property

        Public Property Incluir_paredes() As Boolean
            Get
                Return m_incluir_paredes
            End Get
            Set(ByVal value As Boolean)
                m_incluir_paredes = value
            End Set
        End Property

        Public Property Meio() As String
            Get
                Return m_meio
            End Get
            Set(ByVal value As String)
                m_meio = value
            End Set
        End Property

        Public Property Material() As String
            Get
                Return m_material
            End Get
            Set(ByVal value As String)
                m_material = value
            End Set
        End Property

        Public Property Velocidade() As Double
            Get
                Return m_velocidade
            End Get
            Set(ByVal value As Double)
                m_velocidade = value
            End Set
        End Property


        Public Property Espessura() As Double
            Get
                Return m_espessura
            End Get
            Set(ByVal value As Double)
                m_espessura = value
            End Set
        End Property

        Public Property Condtermica() As Double
            Get
                Return m_condtermica
            End Get
            Set(ByVal value As Double)
                m_condtermica = value
            End Set
        End Property

        Public Property Temp_amb_estimar() As Double
            Get
                Return m_temp_amb_estimar
            End Get
            Set(ByVal value As Double)
                m_temp_amb_estimar = value
            End Set
        End Property

        Public Property Calor_trocado() As Double
            Get
                Return m_calor_trocado
            End Get
            Set(ByVal value As Double)
                m_calor_trocado = value
            End Set
        End Property

        Public Property Temp_amb_definir() As Double
            Get
                Return m_temp_amb_definir
            End Get
            Set(ByVal value As Double)
                m_temp_amb_definir = value
            End Set
        End Property

        Public Property CGTC_Definido() As Double
            Get
                Return m_cgtc_definido
            End Get
            Set(ByVal value As Double)
                m_cgtc_definido = value
            End Set
        End Property

        Public Property Tipo() As ThermalProfileType
            Get
                Return m_type
            End Get
            Set(ByVal value As ThermalProfileType)
                m_type = value
            End Set
        End Property

        Public Overrides Function ToString() As String
            Return DWSIM.App.GetLocalString("Cliqueparaeditar")
        End Function

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me)

            Return elements

        End Function

    End Class

End Namespace


