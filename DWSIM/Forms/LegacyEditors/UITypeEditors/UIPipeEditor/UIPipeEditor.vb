'    UITypeEditor for Pipe Profile
'    Copyright 2008 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU Lesser General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Imports System.Windows.Forms.Design
Imports System.Drawing.Design
Imports System.ComponentModel

Namespace DWSIM.Editors.PipeEditor

    Public Enum PipeEditorStatus
        OK
        Erro
        Definir
    End Enum

    <System.Serializable()> Public Class UIPipeEditor

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

                Dim selectionControl As New UnitOperations.PipeHydraulicProfileEditor

                selectionControl.PipeOp = value
                selectionControl.Dock = DockStyle.Fill

                Dim f As New Form With {.Width = selectionControl.Width * Settings.DpiScale, .Height = selectionControl.Height * Settings.DpiScale}
                f.Controls.Add(selectionControl)
                f.Text = "Hydraulic Profile Editor"
                f.FormBorderStyle = FormBorderStyle.SizableToolWindow

                editorService.ShowDialog(f)

            End If

            Return value

        End Function

    End Class

    <System.Serializable()> Public Class PipeEditorConverter
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

End Namespace

