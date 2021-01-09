'    UITypeEditor for Pump Curves
'    Copyright 2010 Daniel Wagner O. de Medeiros
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

Namespace DWSIM.Editors.Pump

    <System.Serializable()> Public Class UIPumpCurvesEditor

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

                Dim selectionControl As New PumpCurvesEditorForm

                If context.PropertyDescriptor.DisplayName = DWSIM.App.GetLocalString("PumpSetupCurves") Then
                    selectionControl.curveeditorshowmode = 0
                    selectionControl.Text = form.FormSurface.FlowsheetSurface.SelectedObject.Tag & " - " & DWSIM.App.GetLocalString("PumpSetupCurves")
                Else
                    selectionControl.curveeditorshowmode = 1
                    selectionControl.Text = form.FormSurface.FlowsheetSurface.SelectedObject.Tag & " - " & DWSIM.App.GetLocalString("PumpViewCurves")
                End If

                Dim myPump As UnitOperations.UnitOperations.Pump = form.SimulationObjects(form.FormSurface.FlowsheetSurface.SelectedObject.Name)

                selectionControl.selectedpump = myPump

                editorService.ShowDialog(selectionControl)

               Call form.FormSurface.UpdateSelectedObject()
                Call form.FormSurface.Invalidate()
                Application.DoEvents()

                Return selectionControl.selectedpump.Curves

            End If

            Return Nothing

        End Function

    End Class

End Namespace



