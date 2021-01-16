﻿'    UITypeEditor for Column Initial Estimates
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

Namespace DWSIM.Editors.Distillation

    <System.Serializable()> Public Class UIInitialEstimates

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

                Dim selectionControl As New UnitOperations.EditingForm_Column_InitialEstimates

                selectionControl.dc = My.Application.ActiveSimulation.GetSelectedFlowsheetSimulationObject("")

                selectionControl.Dock = DockStyle.Fill

                Dim f As New Form With {.Width = selectionControl.Width * Settings.DpiScale, .Height = selectionControl.Height * Settings.DpiScale}
                f.Controls.Add(selectionControl)
                f.Size = selectionControl.Size

                editorService.ShowDialog(f)

            End If

            Dim dc As Column
            Dim form As FormFlowsheet
            form = My.Application.ActiveSimulation
            dc = form.SimulationObjects(form.FormSurface.FlowsheetSurface.SelectedObject.Name)
            Application.DoEvents()
            'dc.CheckCalc2()

            Return value

        End Function

    End Class

End Namespace

