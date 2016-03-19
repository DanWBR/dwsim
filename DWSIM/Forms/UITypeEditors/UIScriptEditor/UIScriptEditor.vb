'    UITypeEditor for Custom UO Script
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
Imports DWSIM.DWSIM.SimulationObjects

Namespace DWSIM.Editors.CustomUO

    <System.Serializable()> Public Class UIScriptEditor

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

                If Not DWSIM.App.IsRunningOnMono Then
                    Dim selectionControl As New ScriptEditorForm
                    Dim ctx As PropertyGridEx.CustomProperty.CustomPropertyDescriptor = context.PropertyDescriptor
                    Dim obj As DWSIM.SimulationObjects.UnitOperations.UnitOpBaseClass = form.Collections.FlowsheetObjectCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
                    With DirectCast(form.Collections.FlowsheetObjectCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name), UnitOperations.CustomUO)
                        selectionControl.scripttext = .ScriptText
                        selectionControl.fontname = .FontName
                        selectionControl.fontsize = .FontSize
                        selectionControl.includes = .Includes
                        selectionControl.highlightspaces = .HighlightSpaces
                    End With
                    selectionControl.Text = form.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag & " - " & DWSIM.App.GetLocalString("ScriptEditor")
                    editorService.ShowDialog(selectionControl)
                    With DirectCast(form.Collections.FlowsheetObjectCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name), UnitOperations.CustomUO)
                        .FontName = selectionControl.tscb1.SelectedItem
                        .FontSize = selectionControl.tscb2.SelectedItem
                        .Includes = selectionControl.includes
                        .HighlightSpaces = selectionControl.highlightspaces
                    End With
                    value = selectionControl.scripttext
                    selectionControl = Nothing
                Else
                    Dim selectionControl As New ScriptEditorFormMono
                    Dim ctx As PropertyGridEx.CustomProperty.CustomPropertyDescriptor = context.PropertyDescriptor
                    Dim obj As DWSIM.SimulationObjects.UnitOperations.UnitOpBaseClass = form.Collections.FlowsheetObjectCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
                    Dim cuo As UnitOperations.CustomUO = DirectCast(form.Collections.FlowsheetObjectCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name), UnitOperations.CustomUO)
                    selectionControl.scripttext = cuo.ScriptText
                    selectionControl.fontname = cuo.FontName
                    selectionControl.fontsize = cuo.FontSize
                    selectionControl.includes = cuo.Includes
                    selectionControl.Text = form.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag & " - " & DWSIM.App.GetLocalString("ScriptEditor")
                    editorService.ShowDialog(selectionControl)
                    cuo.FontName = selectionControl.tscb1.SelectedItem
                    cuo.FontSize = selectionControl.tscb2.SelectedItem
                    cuo.Includes = selectionControl.includes
                    value = selectionControl.txtScript.Text
                    selectionControl = Nothing
                End If

            End If

            Return value

        End Function

    End Class

End Namespace
