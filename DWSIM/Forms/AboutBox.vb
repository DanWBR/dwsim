Imports System.Reflection
Imports System.Text.RegularExpressions
Imports System.IO
Imports Yeppp
Imports System.Linq
Imports Cudafy
Imports Cudafy.Host

Public Class AboutBox

    Private _IsPainted As Boolean = False
    Private _EntryAssemblyName As String
    Private _CallingAssemblyName As String
    Private _ExecutingAssemblyName As String
    Private _EntryAssembly As System.Reflection.Assembly
    Private _EntryAssemblyAttribCollection As Specialized.NameValueCollection

    Private Sub AboutBox_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        TextBox1.Font = New Font("Consolas", 9, GraphicsUnit.Point)
        tbAcknowledgements.Font = New Font("Consolas", 9, GraphicsUnit.Point)

        Dim updfile = My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "version.info"

        Version.Text = "Version " & My.Application.Info.Version.Major & "." & My.Application.Info.Version.Minor & "." & My.Application.Info.Version.Build

#If DEBUG Then
        Version.Text += "-" + IO.File.GetLastWriteTimeUtc(Assembly.GetExecutingAssembly().Location).ToString()
#End If

        tbAcknowledgements.Text = "A HUGE thank you to the following Patrons/Sponsors who made this release possible:" + vbCrLf + vbCrLf +
            Patrons.GetList() + vbCrLf + vbCrLf + tbAcknowledgements.Text

        Copyright.Text = My.Application.Info.Copyright

        LblOSInfo.Text = My.Computer.Info.OSFullName & ", Version " & My.Computer.Info.OSVersion & ", " & My.Computer.Info.OSPlatform & " Platform"
        LblCLRInfo.Text = SharedClasses.Utility.GetRuntimeVersion()
        Lblmem.Text = (GC.GetTotalMemory(False) / 1024 / 1024).ToString("#") & " MB managed, " & (My.Application.Info.WorkingSet / 1024 / 1024).ToString("#") & " MB total"

        Lblcpuinfo.Text = "Retrieving CPU info..."

        If Not DWSIM.App.IsRunningOnMono Then

            Threading.Tasks.Task.Factory.StartNew(Function()
                                                      Dim scrh As New System.Management.ManagementObjectSearcher("select * from Win32_Processor")
                                                      Dim text1 As String = System.Environment.GetEnvironmentVariable("PROCESSOR_IDENTIFIER")
                                                      For Each qinfo In scrh.Get()
                                                          text1 += " / " & qinfo.Properties("Name").Value.ToString
                                                      Next
                                                      text1 += " (" & Yeppp.Library.GetProcessABI().Description & ")"
                                                      Return text1
                                                  End Function).ContinueWith(Sub(t)
                                                                                 Lblcpuinfo.Text = t.Result
                                                                             End Sub, Threading.Tasks.TaskScheduler.FromCurrentSynchronizationContext)

        Else

            Threading.Tasks.Task.Factory.StartNew(Function()
                                                      Dim sinfo As New ProcessStartInfo With {.FileName = "lshw", .Arguments = "-c CPU", .RedirectStandardOutput = True, .UseShellExecute = False}
                                                      Dim p As New Process With {.StartInfo = sinfo}
                                                      p.Start()
                                                      Dim output As String = p.StandardOutput.ReadToEnd
                                                      p.WaitForExit()
                                                      Dim lbltext As String = ""
                                                      For Each l In output.Split(New Char() {vbCrLf, vbLf, vbCr})
                                                          If l.Contains("product") Then
                                                              lbltext = l.Split(":")(1).TrimStart(" ")
                                                          End If
                                                          If l.Contains("vendor") Then
                                                              lbltext += " / " & l.Split(": ")(1).TrimStart(" ")
                                                              Exit For
                                                          End If
                                                      Next
                                                      Return lbltext
                                                  End Function).ContinueWith(Sub(t)
                                                                                 Lblcpuinfo.Text = t.Result
                                                                                 Lblcpuinfo.Text += " (" & Yeppp.Library.GetProcessABI().Description & ")"
                                                                             End Sub, Threading.Tasks.TaskScheduler.FromCurrentSynchronizationContext)

        End If

        Lblcpusimd.Text = "Querying CPU SIMD capabilities..."

        Threading.Tasks.Task.Factory.StartNew(Function()
                                                  Dim text1 As String = ""
                                                  For Each item In Library.GetCpuArchitecture.CpuSimdFeatures
                                                      text1 += item.ToString & " "
                                                  Next
                                                  Return text1
                                              End Function).ContinueWith(Sub(t)
                                                                             Lblcpusimd.Text = t.Result
                                                                         End Sub, Threading.Tasks.TaskScheduler.FromCurrentSynchronizationContext)

        With Me.DataGridView1.Rows
            .Clear()
            .Add(New Object() {"GERG 2008", "2.0", "2017", "E. W. Lemmon, V. Heinemann, J. Lu, I. Bell", "https://github.com/usnistgov/AGA8", "NIST/17 U.S.C. 105", "https://github.com/usnistgov/AGA8/blob/master/LICENSE"})
            .Add(New Object() {"pyeq2", "10.1", "2013", "James R. Phillips", "https://code.google.com/p/pyeq2/", "BSD 3", "http://opensource.org/licenses/BSD-3-Clause"})
            .Add(New Object() {"IPOPT", "3.9.2", "2011", "COIN-OR", "https://projects.coin-or.org/Ipopt", "Eclipse Public License", "http://www.eclipse.org/legal/epl-v10.html"})
            .Add(New Object() {"lp_solve", "5.5", "2009", "Michel Berkelaar, Kjell Eikland, Peter Notebaert", "http://lpsolve.sourceforge.net", "LGPLv2", "http://www.gnu.org/licenses/lgpl.html"})
            .Add(New Object() {"CoolProp", "6.0.0", "2016-2018", "Ian H. Bell", "http://wwww.coolprop.org", "MIT-style License", "https://github.com/ibell/coolprop/blob/master/LICENSE"})
            .Add(New Object() {"ChemSep Database", "8.01", "2018", "Harry Kooijman, Ross Taylor", "http://www.chemsep.org", "Perl Artistic License v2", "http://www.perlfoundation.org/artistic_license_2_0"})
            .Add(New Object() {"Flee", "0.9.14", "2009", "Eugene Ciloci", "https://flee.codeplex.com", "LGPLv2", "http://www.gnu.org/licenses/lgpl.html"})
            .Add(New Object() {"CUDAfy", "1.25.4963.10126", "2013", "Hybrid DSP", "https://cudafy.codeplex.com", "LGPLv2", "http://www.gnu.org/licenses/lgpl.html"})
            .Add(New Object() {"DotNumerics", "1.0", "2009", "Jose Antonio De Santiago Castillo", "http://www.dotnumerics.com", "GPLv3", "http://www.gnu.org/licenses/gpl.html"})
            .Add(New Object() {"CSIPOPT", "1.0", "2012", "Anders Gustafsson, Cureos AB", "https://github.com/cureos/csipopt", "Eclipse Public License", "http://www.eclipse.org/legal/epl-v10.html"})
            .Add(New Object() {"NetOffice", "1.6", "2011", "Sebastian Lange", "https://netoffice.codeplex.com/", "MIT License", "https://netoffice.codeplex.com/license"})
            .Add(New Object() {"GemBox.Spreadsheet", "39.3.30.1037", "2015", "GemBox Software", "http://www.gemboxsoftware.com/spreadsheet/overview", "EULA", "http://www.gemboxsoftware.com/Spreadsheet/Eula.rtf"})
            .Add(New Object() {"Indigo", "1.1", "2013", "GGA Software Services LLC", "http://www.ggasoftware.com/opensource/indigo", "GPLv3", "http://www.gnu.org/licenses/gpl.html"})
            .Add(New Object() {"Nini", "1.1", "2010", "Brent R. Matzelle", "https://sourceforge.net/projects/nini", "MIT License", "http://www.opensource.org/licenses/mit-license.html"})
            .Add(New Object() {"Jolt.NET", "0.4", "2009", "Steve Guidi", "https://github.com/jacobslusser/scintillaNET", "New BSD License (BSD)", "http://jolt.codeplex.com/license"})
            .Add(New Object() {"Yeppp!", "1.0.0.1", "2014", "Marat Dukhan", "http://www.yeppp.info", "Yeppp! License", "http://www.yeppp.info/resources/yeppp-license.txt"})
            .Add(New Object() {"ExcelDNA", "0.34", "2017", "Govert van Drimmelen", "http://excel-dna.net/", "MIT License", "http://www.opensource.org/licenses/mit-license.html"})
            .Add(New Object() {"AODL", "1.4.0.3", "2011", "Chris Constantin", "https://bitbucket.org/chrisc/aodl", "Apache License v2", "https://wiki.openoffice.org/wiki/OpenOffice.org_Wiki:Copyrights"})
            .Add(New Object() {"Eto.Forms", "custom version", "2019", "", "https://github.com/DanWBR/Eto", "BSD-style License", "https://github.com/DanWBR/Eto/blob/develop/LICENSE.txt"})
            .Add(New Object() {"Eto.Platform.Gtk2", "custom version", "2019", "", "https://github.com/DanWBR/Eto", "BSD-style License", "https://github.com/DanWBR/Eto/blob/develop/LICENSE.txt"})
            .Add(New Object() {"Eto.Platform.Windows", "custom version", "2019", "", "https://github.com/DanWBR/Eto", "BSD-style License", "https://github.com/DanWBR/Eto/blob/develop/LICENSE.txt"})
            .Add(New Object() {"Eto.Platform.Wpf", "custom version", "2019", "", "https://github.com/DanWBR/Eto", "BSD-style License", "https://github.com/DanWBR/Eto/blob/develop/LICENSE.txt"})
            .Add(New Object() {"Eto.Platform.XamMac2", "custom version", "2019", "", "https://github.com/DanWBR/Eto", "BSD-style License", "https://github.com/DanWBR/Eto/blob/develop/LICENSE.txt"})
            .Add(New Object() {"ReoGrid", "custom version", "2019", "", "https://github.com/DanWBR/ReoGrid", "MIT-style License", "https://github.com/DanWBR/ReoGrid/blob/master/LICENSE"})
        End With
        Me.DataGridView1.Sort(Me.DataGridView1.Columns(0), System.ComponentModel.ListSortDirection.Ascending)

        With Me.DataGridView2.Rows
            .Clear()
            .Add(New Object() {"DynamicLanguageRuntime", "1.2.3", "https://github.com/IronLanguages/dlr/blob/master/LICENSE"})
            .Add(New Object() {"IronPython", "2.7.10", "https://github.com/IronLanguages/ironpython2/blob/master/LICENSE"})
            .Add(New Object() {"IronPython.StdLib", "2.7.10", "http://docs.python.org/license.html"})
            .Add(New Object() {"jacobslusser.ScintillaNET", "3.6.3", ""})
            .Add(New Object() {"MathNet.Numerics", "4.7.0", "https://numerics.mathdotnet.com/License.html"})
            .Add(New Object() {"Newtonsoft.Json", "12.0.3", "https://raw.github.com/JamesNK/Newtonsoft.Json/master/LICENSE.md"})
            .Add(New Object() {"OpenTK", "3.0.", "https://github.com/opentk/opentk/blob/master/License.txt"})
            .Add(New Object() {"OpenTK.GLControl", "3.0.1", "http://github.com/opentk/opentk/blob/master/License.txt"})
            .Add(New Object() {"OxyPlot.Core", "2.0.0-unstable0956", "https://raw.githubusercontent.com/oxyplot/oxyplot/master/LICENSE"})
            .Add(New Object() {"OxyPlot.Wpf", "2.0.0-unstable0956", "https://raw.githubusercontent.com/oxyplot/oxyplot/master/LICENSE"})
            .Add(New Object() {"SharpDX", "4.0.1", "http://sharpdx.org/License.txt"})
            .Add(New Object() {"SharpDX.Direct2D1", "4.0.1", "http://sharpdx.org/License.txt"})
            .Add(New Object() {"SharpDX.DXGI", "4.0.1", "http://sharpdx.org/License.txt"})
            .Add(New Object() {"SharpDX.Mathematics", "4.0.1", "http://sharpdx.org/License.txt"})
            .Add(New Object() {"SharpZipLib", "1.1.0", "https://github.com/icsharpcode/SharpZipLib/blob/master/LICENSE.txt"})
            .Add(New Object() {"SkiaSharp", "1.68.2.1", "https://github.com/mono/SkiaSharp/blob/master/LICENSE.md"})
            .Add(New Object() {"SkiaSharp.Extended", "1.68.2.1", "https://github.com/mono/SkiaSharp.Extended/blob/master/LICENSE"})
            .Add(New Object() {"System.ComponentModel", "4.3.0", "http://go.microsoft.com/fwlink/?LinkId=329770"})
            .Add(New Object() {"System.Runtime.Serialization.Primitives", "4.3.0", "http://go.microsoft.com/fwlink/?LinkId=329770"})
        End With

    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Me.Close()
    End Sub


    ''' <summary>
    ''' populate a listview with the specified key and value strings
    ''' </summary>
    Private Sub Populate(ByVal lvw As ListView, ByVal Key As String, ByVal Value As String)
        If Value = "" Then Return
        Dim lvi As New ListViewItem
        lvi.Text = Key
        lvi.SubItems.Add(Value)
        lvw.Items.Add(lvi)
    End Sub

    Private Sub AboutBox_Shown(sender As Object, e As EventArgs) Handles Me.Shown
        FormMain.TranslateFormFunction?.Invoke(Me)
    End Sub
End Class