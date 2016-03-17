'    UITypeEditors for Gibbs Reactor
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

Imports System.Windows.Forms.Design
Imports System.Drawing.Design
Imports System.ComponentModel

Namespace DWSIM.Editors.Reactors

    <System.Serializable()> Public Class UIElementMatrixEditor

        Inherits System.Drawing.Design.UITypeEditor

        Private editorService As IWindowsFormsEditorService

        Public Overrides Function GetEditStyle(ByVal context As System.ComponentModel.ITypeDescriptorContext) As UITypeEditorEditStyle
            Return UITypeEditorEditStyle.Modal
        End Function

        Public Overrides Function EditValue(ByVal context As System.ComponentModel.ITypeDescriptorContext, ByVal provider As IServiceProvider, ByVal value As Object) As Object

            If (provider IsNot Nothing) Then
                editorService = CType(provider.GetService(GetType(IWindowsFormsEditorService)),  _
                IWindowsFormsEditorService)
            End If

            Dim form As FormFlowsheet = My.Application.ActiveSimulation

            If (editorService IsNot Nothing) Then

                Dim selectionControl As New ElementMatrixEditorForm

                selectionControl.elmat = DirectCast(form.Collections.FlowsheetObjectCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name), Reactor_Gibbs).ElementMatrix

                selectionControl.Text = form.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag & " - " & DWSIM.App.GetLocalString("RGEditElementMatrix")

                editorService.ShowDialog(selectionControl)

                value = selectionControl.elmat

            End If

            Return value

        End Function

    End Class

End Namespace

