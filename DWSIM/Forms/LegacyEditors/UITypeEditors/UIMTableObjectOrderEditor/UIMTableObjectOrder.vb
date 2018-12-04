'    UITypeEditor for Master Table Objects' Custom Ordering
'    Copyright 2012 Daniel Wagner O. de Medeiros
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
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects.Tables

Namespace DWSIM.Editors.MasterTable

    <System.Serializable()> Public Class UIMTableObjectOrderEditor

        Inherits System.Drawing.Design.UITypeEditor

        Private editorService As IWindowsFormsEditorService

        Dim loaded As Boolean = False
        Dim form As FormFlowsheet

        Public WithEvents ListView2 As System.Windows.Forms.ListView
        Public WithEvents Button1 As System.Windows.Forms.Button
        Public WithEvents Button2 As System.Windows.Forms.Button
        Public Panel1 As System.Windows.Forms.Panel
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
                Me.Panel1 = New System.Windows.Forms.Panel
                Me.Button1 = New System.Windows.Forms.Button
                Me.Button2 = New System.Windows.Forms.Button

                'Button1
                '
                Me.Button1.AccessibleDescription = Nothing
                Me.Button1.AccessibleName = Nothing
                Me.Button1.BackgroundImage = Nothing
                Me.Button1.Font = Nothing
                Me.Button1.Name = "Button1"
                Me.Button1.UseVisualStyleBackColor = True
                Me.Button1.Text = "Move Up"
                Me.Button1.Dock = DockStyle.Top
                '
                'Button2
                '
                Me.Button2.AccessibleDescription = Nothing
                Me.Button2.AccessibleName = Nothing
                Me.Button2.BackgroundImage = Nothing
                Me.Button2.Font = Nothing
                Me.Button2.Name = "Button2"
                Me.Button2.UseVisualStyleBackColor = True
                Me.Button2.Text = "Move Down"
                Me.Button2.Dock = DockStyle.Bottom
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
                Me.ListView2.BorderStyle = Windows.Forms.BorderStyle.None
                Me.ListView2.Margin = New System.Windows.Forms.Padding(0)
                '
                'ColumnHeader3
                '
                Me.ColumnHeader3.Text = "Name"
                Me.ColumnHeader3.AutoResize(ColumnHeaderAutoResizeStyle.None)
                Me.ColumnHeader3.Width = Me.ListView2.Width

                'Panel1
                '
                Me.Panel1.AccessibleDescription = Nothing
                Me.Panel1.AccessibleName = Nothing
                Me.Panel1.BackgroundImage = Nothing
                Me.Panel1.Controls.Add(ListView2)
                Me.Panel1.Controls.Add(Button1)
                Me.Panel1.Controls.Add(Button2)
                Me.Panel1.Font = Nothing
                Me.Panel1.Name = "Panel1"
                Me.Panel1.TabStop = False
                Me.Panel1.Height = 200

                form = My.Application.ActiveSimulation

                Dim mt As MasterTableGraphic = form.FormSurface.FlowsheetSurface.SelectedObject

                Me.ListView2.Items.Clear()
                If mt.SortedList.Count = 0 Then
                    For Each obj As String In mt.ObjectList.Keys
                        If mt.ObjectList(obj) Then
                            Dim lvi As New ListViewItem()
                            With lvi
                                .Text = obj
                                .Tag = obj
                                .Name = obj
                            End With
                            ListView2.Items.Add(lvi)
                        End If
                    Next
                Else
                    For Each obj As String In mt.ObjectList.Keys
                        If Not mt.SortedList.Contains(obj) Then mt.SortedList.Add(obj)
                    Next
                    For Each obj As String In mt.SortedList
                        If mt.ObjectList(obj) Then
                            Dim lvi As New ListViewItem()
                            With lvi
                                .Text = obj
                                .Tag = obj
                                .Name = obj
                            End With
                            ListView2.Items.Add(lvi)
                        End If
                    Next
                End If
                Me.ListView2.SelectedItems.Clear()

                Me.loaded = True

                editorService.DropDownControl(Panel1)

                Dim list As New List(Of String)
                For Each lvi As ListViewItem In Me.ListView2.Items
                    list.Add(lvi.Text)
                Next

                value = list

            End If

            Return value

        End Function

        Private Sub ListView2_VisibleChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ListView2.VisibleChanged
            Me.ColumnHeader3.Width = Me.ListView2.Width
        End Sub

        Private Sub Button1_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles Button1.Click
            Dim index As Integer = 0
            If Me.ListView2.SelectedItems.Count > 0 Then
                index = Me.ListView2.SelectedItems(0).Index
                If index <> 0 Then
                    Dim lvi As ListViewItem = Me.ListView2.SelectedItems(0).Clone
                    Me.ListView2.SelectedItems(0).Remove()
                    Me.ListView2.Items.Insert(index - 1, lvi)
                    Me.ListView2.Items(index - 1).Selected = True
                End If
            End If
        End Sub

        Private Sub Button2_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles Button2.Click
            Dim index As Integer = 0
            If Me.ListView2.SelectedItems.Count > 0 Then
                index = Me.ListView2.SelectedItems(0).Index
                If index <> Me.ListView2.Items.Count - 1 Then
                    Dim lvi As ListViewItem = Me.ListView2.SelectedItems(0).Clone
                    Me.ListView2.SelectedItems(0).Remove()
                    Me.ListView2.Items.Insert(index + 1, lvi)
                    Me.ListView2.Items(index + 1).Selected = True
                End If
            End If
        End Sub
    End Class

End Namespace
