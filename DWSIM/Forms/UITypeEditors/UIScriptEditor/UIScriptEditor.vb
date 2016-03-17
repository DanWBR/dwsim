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
                    If ctx.CustomProperty.Tag Is Nothing Then
                        With DirectCast(form.Collections.FlowsheetObjectCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name), UnitOperations.CustomUO)
                            selectionControl.scripttext = .ScriptText
                            selectionControl.language = .Language
                            selectionControl.fontname = .FontName
                            selectionControl.fontsize = .FontSize
                            selectionControl.includes = .Includes
                            selectionControl.highlightspaces = .HighlightSpaces
                        End With
                    Else
                        If ctx.CustomProperty.Tag = "B" Then selectionControl.scripttext = obj.ScriptExt_ScriptTextB
                        If ctx.CustomProperty.Tag = "A" Then selectionControl.scripttext = obj.ScriptExt_ScriptTextA
                        selectionControl.language = obj.ScriptExt_Language
                        selectionControl.fontname = obj.ScriptExt_FontName
                        selectionControl.fontsize = obj.ScriptExt_FontSize
                        selectionControl.includes = obj.ScriptExt_Includes
                    End If
                    selectionControl.Text = form.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag & " - " & DWSIM.App.GetLocalString("ScriptEditor")
                    editorService.ShowDialog(selectionControl)
                    If ctx.CustomProperty.Tag Is Nothing Then
                        With DirectCast(form.Collections.FlowsheetObjectCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name), UnitOperations.CustomUO)
                            .FontName = selectionControl.tscb1.SelectedItem
                            .FontSize = selectionControl.tscb2.SelectedItem
                            .Includes = selectionControl.includes
                            .HighlightSpaces = selectionControl.highlightspaces
                        End With
                   Else
                        obj.ScriptExt_FontName = selectionControl.tscb1.SelectedItem
                        obj.ScriptExt_FontSize = selectionControl.tscb2.SelectedItem
                        obj.ScriptExt_Includes = selectionControl.includes
                    End If
                    value = selectionControl.scripttext
                    selectionControl = Nothing
                Else
                    Dim selectionControl As New ScriptEditorFormMono
                    Dim ctx As PropertyGridEx.CustomProperty.CustomPropertyDescriptor = context.PropertyDescriptor
                    Dim obj As DWSIM.SimulationObjects.UnitOperations.UnitOpBaseClass = form.Collections.FlowsheetObjectCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
                    If ctx.CustomProperty.Tag Is Nothing Then
                        Dim cuo As UnitOperations.CustomUO = DirectCast(form.Collections.FlowsheetObjectCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name), UnitOperations.CustomUO)
                        selectionControl.scripttext = cuo.ScriptText
                        selectionControl.language = cuo.Language
                        selectionControl.fontname = cuo.FontName
                        selectionControl.fontsize = cuo.FontSize
                        selectionControl.includes = cuo.Includes
                    Else
                        If ctx.CustomProperty.Tag = "B" Then selectionControl.scripttext = obj.ScriptExt_ScriptTextB
                        If ctx.CustomProperty.Tag = "A" Then selectionControl.scripttext = obj.ScriptExt_ScriptTextA
                        selectionControl.language = obj.ScriptExt_Language
                        selectionControl.fontname = obj.ScriptExt_FontName
                        selectionControl.fontsize = obj.ScriptExt_FontSize
                        selectionControl.includes = obj.ScriptExt_Includes
                    End If
                    selectionControl.Text = form.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag & " - " & DWSIM.App.GetLocalString("ScriptEditor")
                    editorService.ShowDialog(selectionControl)
                    If ctx.CustomProperty.Tag Is Nothing Then
                        Dim cuo As UnitOperations.CustomUO = DirectCast(form.Collections.FlowsheetObjectCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name), UnitOperations.CustomUO)
                        cuo.FontName = selectionControl.tscb1.SelectedItem
                        cuo.FontSize = selectionControl.tscb2.SelectedItem
                        cuo.Includes = selectionControl.includes
                    Else
                        obj.ScriptExt_FontName = selectionControl.tscb1.SelectedItem
                        obj.ScriptExt_FontSize = selectionControl.tscb2.SelectedItem
                        obj.ScriptExt_Includes = selectionControl.includes
                    End If
                    value = selectionControl.txtScript.Text
                    selectionControl = Nothing
                End If

            End If

            Return value

        End Function

    End Class

End Namespace
