'    UITypeEditor for Compositions
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
Imports DWSIM.DWSIM.Flowsheet.FlowSheetSolver

Namespace DWSIM.Editors.Composition

    <System.Serializable()> Public Class UICompositionEditor

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

            Dim form As FormFlowsheet = My.Application.ActiveSimulation

            If (editorService IsNot Nothing) Then

                Dim selectionControl As New CompositionEditorForm
                selectionControl.Text = form.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag & DWSIM.App.GetLocalString("EditComp")
                selectionControl.Compounds = value
                selectionControl.NF = form.Options.NumberFormat
                selectionControl.SU = form.Options.SelectedUnitSystem
                selectionControl.Q = form.Collections.CLCS_MaterialStreamCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name).Phases(0).Properties.molarflow.GetValueOrDefault
                selectionControl.W = form.Collections.CLCS_MaterialStreamCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name).Phases(0).Properties.massflow.GetValueOrDefault
                selectionControl.Solvent = form.Collections.CLCS_MaterialStreamCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name).ReferenceSolvent
                selectionControl.InitialComposition = form.Collections.CLCS_MaterialStreamCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name).InputComposition
                selectionControl.Stream = form.Collections.CLCS_MaterialStreamCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
                selectionControl.tbTag.Text = form.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag

                editorService.ShowDialog(selectionControl)

                form.Collections.CLCS_MaterialStreamCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name).Phases(0).Properties.molarflow = selectionControl.Q
                form.Collections.CLCS_MaterialStreamCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name).Phases(0).Properties.massflow = selectionControl.W
                form.Collections.CLCS_MaterialStreamCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name).ReferenceSolvent = selectionControl.Solvent
                form.Collections.CLCS_MaterialStreamCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name).InputComposition = selectionControl.InitialComposition

                value = selectionControl.Compounds

            End If

            'Call function to calculate flowsheet
            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
            With objargs
                .Calculado = False
                .Tag = form.Collections.CLCS_MaterialStreamCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name).GraphicObject.Tag
                .Nome = form.Collections.CLCS_MaterialStreamCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name).Nome
                .Tipo = form.Collections.CLCS_MaterialStreamCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name).GraphicObject.TipoObjeto
                .Emissor = "PropertyGrid"
            End With

            form.CalculationQueue.Enqueue(objargs)

            CalculateAll2(form, My.Settings.SolverMode, , True)

            Return value

        End Function

    End Class

End Namespace


