Imports System.IO

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

Public NotInheritable Class SplashScreen

    'TODO: This form can easily be set as the splash screen for the application by going to the "Application" tab
    '  of the Project Designer ("Properties" under the "Project" menu).


    Private Sub SplashScreen_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

        Dim updfile = My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "version.info"
        lblVersion.Text = "Version " & My.Application.Info.Version.Major & "." & My.Application.Info.Version.Minor
        If File.Exists(updfile) Then
            Dim vinfo As Integer = 0
            Integer.TryParse(File.ReadAllText(updfile), vinfo)
            If vinfo > 0 Then lblVersion.Text += " Update " & vinfo
        End If
        If My.Settings.PreviewVersion <> "" Then lblVersion.Text += " " & My.Settings.PreviewVersion

        lblCopyright.Text = My.Application.Info.Copyright

        lblAppVersion.Text = "Application Version: " + My.Application.Info.Version.ToString
        lblFrameworkVersion.Text = "Framework Version: " + Environment.Version.ToString

    End Sub

    Protected Overrides Sub OnPaint(ByVal e As System.Windows.Forms.PaintEventArgs)
        ' Do nothing here!
    End Sub

    Protected Overrides Sub OnPaintBackground(ByVal pevent As System.Windows.Forms.PaintEventArgs)

        pevent.Graphics.DrawImage(My.Resources.DWSIM_splash, New Rectangle(0, 0, Me.Width, Me.Height))

    End Sub

End Class
