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

      
        'Set up the dialog text at runtime according to the application's assembly information.  

        'TODO: Customize the application's assembly information in the "Application" pane of the project 
        '  properties dialog (under the "Project" menu).

        'Format the version information using the text set into the Version control at design time as the
        '  formatting string.  This allows for effective localization if desired.
        '  Build and revision information could be included by using the following code and changing the 
        '  Version control's designtime text to "Version {0}.{1:00}.{2}.{3}" or something similar.  See
        '  String.Format() in Help for more information.
        '
        '    Version.Text = System.String.Format(Version.Text, My.Application.Info.Version.Major, My.Application.Info.Version.Minor, My.Application.Info.Version.Build, My.Application.Info.Version.Revision)
        Dim dt As DateTime = CType("01/01/2000", DateTime). _
                            AddDays(My.Application.Info.Version.Build). _
                            AddSeconds(My.Application.Info.Version.Revision * 2)
        Version.Text = "Version " & My.Application.Info.Version.Major & "." & My.Application.Info.Version.Minor
        Version.Text += ", Build " & My.Application.Info.Version.Build & " [git-" & DWSIM.App.GetGitHash() & "] (" & Format(dt, "dd/MM/yyyy") & ")"

        'Copyright info
        'Copyright.Text = My.Application.Info.Copyright
    End Sub

    Protected Overrides Sub OnPaint(ByVal e As System.Windows.Forms.PaintEventArgs)
        ' Do nothing here!
    End Sub

    'Protected Overrides Sub OnPaintBackground(ByVal pevent As System.Windows.Forms.PaintEventArgs)
    '    Dim gfx As Graphics = pevent.Graphics
    '    Dim mypng As System.Drawing.Bitmap = My.Resources.dwsim_3_bg
    '    mypng.MakeTransparent(Color.Red)
    '    gfx.DrawImage(mypng, New Rectangle(0, 0, Me.Width, Me.Height))
    'End Sub

End Class
