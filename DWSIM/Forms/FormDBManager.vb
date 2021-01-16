'    Copyright 2014 Gregor Reichert
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

Imports System.Xml.Serialization
Imports DWSIM.Thermodynamics.BaseClasses
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Runtime.Serialization.Formatters
Imports System.IO

Public Class FormDBManager
    Dim pathsep As Char = Path.DirectorySeparatorChar
    Dim componentes As ConstantProperties()

    Public DBPath As String

    Private Sub FormDBManager_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load
        Dim i As Integer
        LblDBPath.Text = DBPath

        'user databases
        CBDBName.Items.Clear()
        If Not My.Settings.UserDatabases Is Nothing Then
            For Each path As String In My.Settings.UserDatabases
                If File.Exists(path) Then
                    i += 1
                    CBDBName.Items.Add("User " & i)
                    If DBPath = path Then
                        CBDBName.SelectedIndex = CBDBName.Items.Count - 1
                    End If
                End If
            Next
            DGrComps.Sort(DGrComps.Columns(0), System.ComponentModel.ListSortDirection.Ascending)
        End If
    End Sub
    Private Sub CBDBName_SelectedIndexChanged(sender As System.Object, e As System.EventArgs) Handles CBDBName.SelectedIndexChanged
        Dim i As Integer

        i = CBDBName.SelectedIndex
        DBPath = My.Settings.UserDatabases(i)
        LblDBPath.Text = DBPath

        'write components to table
        componentes = Databases.UserDB.ReadComps(DBPath)
        DGrComps.Rows.Clear()
        For Each cp As ConstantProperties In componentes
            DGrComps.Rows.Add(cp.ID, cp.Name, cp.CompCreatorStudyFile)
        Next
    End Sub
   
    Private Sub BtnDelComponent_Click(sender As System.Object, e As System.EventArgs) Handles BtnDelComponent.Click
        componentes = Databases.UserDB.ReadComps(DBPath)
        Dim idx As String = DGrComps.CurrentRow.Index
        Dim CompID As String = DGrComps.Rows(idx).Cells("ID").Value
        Dim CompName As String = DGrComps.Rows(idx).Cells("CompName").Value

        Dim result = MessageBox.Show(CompName & " -> " & DWSIM.App.GetLocalString("ExcluirComponente") & " ?", DWSIM.App.GetLocalString("Pergunta"), MessageBoxButtons.OKCancel, MessageBoxIcon.Question, MessageBoxDefaultButton.Button1)
        If result = DialogResult.OK Then
            Databases.UserDB.RemoveCompound(DBPath, CompID)

            DGrComps.Rows.Clear()
            componentes = Databases.UserDB.ReadComps(DBPath)
            For Each cp As ConstantProperties In componentes
                DGrComps.Rows.Add(cp.ID, cp.Name, cp.CompCreatorStudyFile)
            Next
        End If

    End Sub
  
    Private Sub BtnEditComponent_Click(sender As System.Object, e As System.EventArgs) Handles BtnEditComponent.Click
        Dim idx As String = DGrComps.CurrentRow.Index
        Dim CompID As String = DGrComps.Rows(idx).Cells("ID").Value
        Dim CompName As String = DGrComps.Rows(idx).Cells("CompName").Value
        Dim CompStudyFile As String = DGrComps.Rows(idx).Cells("StudyFile").Value

        If CompStudyFile <> "" Then
            MessageBox.Show(CompStudyFile)

            Dim NewMDIChild As New FormCompoundCreator()
            NewMDIChild.MdiParent = FormMain
            NewMDIChild.Show()
            Dim objStreamReader As New FileStream(CompStudyFile, FileMode.Open)
            Dim x As New BinaryFormatter()
            NewMDIChild.mycase = x.Deserialize(objStreamReader)
            NewMDIChild.mycase.Filename = CompStudyFile
            objStreamReader.Close()
            NewMDIChild.WriteData()

        Else
            MessageBox.Show(DWSIM.App.GetLocalString("DBManagerNoFile"))
        End If
    End Sub

  
    Private Sub FormDBManager_HelpRequested(sender As System.Object, hlpevent As System.Windows.Forms.HelpEventArgs) Handles MyBase.HelpRequested
        DWSIM.App.HelpRequested("CONF_DBManager.htm")
    End Sub
End Class