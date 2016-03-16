Imports DWSIM.DWSIM.SimulationObjects

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

Public Class FormConfigureTable

    Inherits System.Windows.Forms.Form

    Private nitems As System.Collections.Generic.Dictionary(Of Integer, DWSIM.Outros.NodeItem)
    Public objname As String

    Public ReadOnly Property NodeItems() As System.Collections.Generic.Dictionary(Of Integer, DWSIM.Outros.NodeItem)
        Get
            Return nitems
        End Get
    End Property

    Sub New()

        ' This call is required by the Windows Form Designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
        nitems = New System.Collections.Generic.Dictionary(Of Integer, DWSIM.Outros.NodeItem)

    End Sub

    Private Sub FormConfigureTable_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        'populate treeview
        Dim ni As DWSIM.Outros.NodeItem

        Me.TreeView1.Nodes.Clear()

        For Each ni In Me.NodeItems.Values
            Me.TreeView1.Nodes.Add(ni.Key, DWSIM.App.GetPropertyName(ni.Text)).Checked = ni.Checked
        Next

    End Sub

    Private Sub KryptonButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton1.Click

        Dim Flowsheet As FormFlowsheet = My.Application.ActiveSimulation

        Dim obj As DWSIM.SimulationObjects.UnitOperations.BaseClass = Flowsheet.Collections.ObjectCollection(Me.objname)

        Dim ni As DWSIM.Outros.NodeItem
        For Each ni In obj.NodeTableItems.Values
            ni.Checked = Me.TreeView1.Nodes.Find(ni.Key, True)(0).Checked
        Next

        Me.Close()

    End Sub
End Class