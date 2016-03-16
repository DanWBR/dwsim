'    UITypeEditor for Component Selection
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
Imports DWSIM.DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.DWSIM.SimulationObjects.Reactors

Namespace DWSIM.Editors.Reactors

    <System.Serializable()> Public Class UIGibbsComponentSelector

        Inherits System.Drawing.Design.UITypeEditor

        Private editorService As IWindowsFormsEditorService

        Public selectedcomps() As String
        Public _sel As New ArrayList
        Dim loaded As Boolean = False
        Dim form As FormFlowsheet
        Dim gr As Reactor_Gibbs

        Public WithEvents ListView2 As System.Windows.Forms.ListView
        Public WithEvents ColumnHeader3 As System.Windows.Forms.ColumnHeader

        Public Overrides Function GetEditStyle(ByVal context As System.ComponentModel.ITypeDescriptorContext) As UITypeEditorEditStyle
            If Not context Is Nothing AndAlso Not context.Instance Is Nothing Then
                Return UITypeEditorEditStyle.DropDown
            End If
            Return UITypeEditorEditStyle.None
        End Function

        Public Overrides Function EditValue(ByVal context As System.ComponentModel.ITypeDescriptorContext, ByVal provider As IServiceProvider, ByVal value As Object) As Object

            If (provider IsNot Nothing) Then
                editorService = CType(provider.GetService(GetType(IWindowsFormsEditorService)),  _
                IWindowsFormsEditorService)
            End If

            If (editorService IsNot Nothing) Then

                form = My.Application.ActiveSimulation
                gr = form.Collections.ObjectCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)

                Me.ListView2 = New System.Windows.Forms.ListView
                Me.ColumnHeader3 = New System.Windows.Forms.ColumnHeader
                '
                'ListView2
                '
                Me.ListView2.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader3})
                Me.ListView2.Dock = System.Windows.Forms.DockStyle.Fill
                Me.ListView2.FullRowSelect = True
                Me.ListView2.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.None
                Me.ListView2.HideSelection = True
                Me.ListView2.CheckBoxes = True
                Me.ListView2.LabelWrap = False
                Me.ListView2.Location = New System.Drawing.Point(0, 0)
                Me.ListView2.MultiSelect = False
                Me.ListView2.Name = "ListView2"
                Me.ListView2.ShowGroups = False
                Me.ListView2.TabIndex = 1
                Me.ListView2.UseCompatibleStateImageBehavior = False
                Me.ListView2.View = System.Windows.Forms.View.Details
                '
                'ColumnHeader3
                '
                Me.ColumnHeader3.Text = "Nome"
                Me.ColumnHeader3.Width = Me.ListView2.Width

                form = My.Application.ActiveSimulation

                Me.ListView2.Items.Clear()
                For Each comp As ConstantProperties In form.Options.SelectedComponents.Values
                    Dim lvi As New ListViewItem()
                    With lvi
                        .Text = DWSIM.App.GetComponentName(comp.Name)
                        .Tag = comp.Name
                        .Name = comp.Name
                    End With
                    ListView2.Items.Add(lvi)
                Next
                Me.ListView2.SelectedItems.Clear()

                For Each lvi As ListViewItem In Me.ListView2.Items
                    If gr.ComponentIDs.Contains(lvi.Tag) Then lvi.Checked = True
                Next

                editorService.DropDownControl(ListView2)

                Dim i As Integer = 0
                For i = 0 To gr.ComponentIDs.Count - 1
                    If Not form.Options.SelectedComponents.ContainsKey(gr.ComponentIDs(i)) Then
                        gr.ComponentIDs.RemoveAt(i)
                        gr.InitialEstimates.RemoveAt(i)
                        i -= 1
                    End If
                Next

                value = gr.ComponentIDs

            End If

            For Each lvi As ListViewItem In Me.ListView2.Items
                If lvi.Checked Then
                    If Not gr.ComponentIDs.Contains(lvi.Tag) Then
                        gr.InitialEstimates.Add(0.0#)
                    End If
                Else
                    If gr.ComponentIDs.Contains(lvi.Tag) Then
                        gr.InitialEstimates.RemoveAt(gr.ComponentIDs.IndexOf(lvi.Tag))
                    End If
                End If
            Next

            Return value

        End Function

        Private Sub ListView2_ItemChecked(ByVal sender As Object, ByVal e As System.Windows.Forms.ItemCheckedEventArgs) Handles ListView2.ItemChecked
            If loaded Then
                For Each lvi As ListViewItem In Me.ListView2.Items
                    If lvi.Checked Then
                        If Not gr.ComponentIDs.Contains(lvi.Tag) Then
                            gr.ComponentIDs.Add(lvi.Tag)
                        End If
                    Else
                        If gr.ComponentIDs.Contains(lvi.Tag) Then
                            gr.ComponentIDs.Remove(lvi.Tag)
                        End If
                    End If
                Next
            End If
            Me.loaded = True
        End Sub
    End Class

End Namespace


