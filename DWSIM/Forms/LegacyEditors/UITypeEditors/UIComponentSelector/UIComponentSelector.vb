'    UITypeEditors for Material/Energy Stream Selection
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
Imports DWSIM.Thermodynamics.BaseClasses

Namespace DWSIM.Editors.Components

    <System.Serializable()> Public Class UIComponentSelector

        Inherits System.Drawing.Design.UITypeEditor

        Private editorService As IWindowsFormsEditorService

        Public selectedCompID As String = ""
        Public selectedCompName As String = ""
        Dim loaded As Boolean = False
        Dim form As FormFlowsheet

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
                editorService = CType(provider.GetService(GetType(IWindowsFormsEditorService)),
                IWindowsFormsEditorService)
            End If

            If (editorService IsNot Nothing) Then

                Me.ListView2 = New System.Windows.Forms.ListView
                Me.ColumnHeader3 = New System.Windows.Forms.ColumnHeader
                '
                'ListView2
                '
                Me.ListView2.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader3})
                Me.ListView2.Dock = System.Windows.Forms.DockStyle.Fill
                Me.ListView2.FullRowSelect = True
                Me.ListView2.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.None
                Me.ListView2.HideSelection = False
                Me.ListView2.LabelWrap = False
                Me.ListView2.Location = New System.Drawing.Point(0, 0)
                Me.ListView2.MultiSelect = False
                Me.ListView2.Name = "ListView2"
                Me.ListView2.ShowGroups = False
                Me.ListView2.TabIndex = 1
                Me.ListView2.UseCompatibleStateImageBehavior = False
                Me.ListView2.View = System.Windows.Forms.View.Details
                Me.ListView2.BorderStyle = BorderStyle.None
                Me.ListView2.Margin = New System.Windows.Forms.Padding(0)
                '
                'ColumnHeader3
                '
                Me.ColumnHeader3.Text = "Name"
                Me.ColumnHeader3.AutoResize(ColumnHeaderAutoResizeStyle.None)
                Me.ColumnHeader3.Width = Me.ListView2.Width

                form = My.Application.ActiveSimulation

                Me.ListView2.Items.Clear()
                For Each comp As ConstantProperties In form.Options.SelectedComponents.Values
                    Dim lvi As New ListViewItem()
                    With lvi
                        .Text = DWSIM.App.GetLocalString(comp.Name)
                        .Tag = comp.Name
                        .Name = comp.Name
                    End With
                    ListView2.Items.Add(lvi)
                Next
                Me.ListView2.SelectedItems.Clear()

                'Me.ListView2.Items(selectedRSID).Selected = True

                Me.loaded = True

                Me.selectedCompName = value

                editorService.DropDownControl(ListView2)

                value = Me.selectedCompID

            End If

            Return value

        End Function

        Private Sub ListView2_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ListView2.SelectedIndexChanged
            If Me.loaded Then
                If Me.ListView2.SelectedItems.Count > 0 Then
                    Me.selectedCompID = Me.ListView2.SelectedItems(0).Name
                    Me.selectedCompName = Me.ListView2.SelectedItems(0).Text
                    Me.editorService.CloseDropDown()
                End If
            End If
        End Sub

        Private Sub ListView2_VisibleChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ListView2.VisibleChanged
            Me.ColumnHeader3.Width = Me.ListView2.Width
        End Sub
    End Class

End Namespace



