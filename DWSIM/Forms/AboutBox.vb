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

        Dim updfile = My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "version.info"

        Version.Text = "Version " & My.Application.Info.Version.Major & "." & My.Application.Info.Version.Minor & "." & My.Application.Info.Version.Build

#If DEBUG Then
        Version.Text += "-" + IO.File.GetLastWriteTimeUtc(Assembly.GetExecutingAssembly().Location).ToString()
#End If

        lblCurrentVersion.Text = Version.Text + " (Classic UI)"

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
            .Add(New Object() {"cef.redist.x64", "75.1.14", "https://raw.github.com/cefsharp/cef-binary/master/LICENSE.txt"})
            .Add(New Object() {"cef.redist.x86", "75.1.14", "https://raw.github.com/cefsharp/cef-binary/master/LICENSE.txt"})
            .Add(New Object() {"CefSharp.Common", "75.1.14", "https://raw.github.com/cefsharp/CefSharp/master/LICENSE"})
            .Add(New Object() {"CefSharp.WinForms", "75.1.14", "https://raw.github.com/cefsharp/CefSharp/master/LICENSE"})
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

        'get DWSIM components' versions

        Dim assnames = New String() {"DWSIM.exe", "DWSIM.Drawing.SkiaSharp.dll", "DWSIM.DynamicsManager.dll",
            "DWSIM.ExtensionMethods.dll", "DWSIM.FileStorage.dll", "DWSIM.FlowsheetSolver.dll", "DWSIM.GlobalSettings.dll",
            "DWSIM.Interfaces.dll", "DWSIM.MathOps.dll", "DWSIM.SharedClasses.dll", "DWSIM.Thermodynamics.dll",
            "DWSIM.UnitOperations.dll", "DWSIM.XMLSerializer.dll"}

        dgvDWSIMComponents.Rows.Clear()
        For Each assn In assnames
            Dim assemb = Assembly.LoadFile(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & assn)
            If Not assemb Is Nothing Then
                Dim assdesc = assemb.GetCustomAttributes(Type.GetType("System.Reflection.AssemblyDescriptionAttribute"), False).FirstOrDefault().Description
                dgvDWSIMComponents.Rows.Add(New Object() {assn, assemb.GetName.Version.ToString, AssemblyBuildDate(assemb).ToShortDateString, assdesc})
                assemb = Nothing
            End If
        Next

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

    ''' <summary>
    ''' populate Assembly Information listview with ALL assemblies
    ''' </summary>
    Private Sub PopulateAssemblies()
        For Each a As [Assembly] In AppDomain.CurrentDomain.GetAssemblies
            Try
                PopulateAssemblySummary(a)
            Catch ex As Exception
            End Try
        Next
        AssemblyNamesComboBox.SelectedIndex = AssemblyNamesComboBox.FindStringExact(_EntryAssemblyName)
    End Sub

    ''' <summary>
    ''' populate Assembly Information listview with summary view for a specific assembly
    ''' </summary>
    Private Sub PopulateAssemblySummary(ByVal a As [Assembly])
        Dim nvc As Specialized.NameValueCollection = AssemblyAttribs(a)

        Dim strAssemblyName As String = a.GetName.Name

        Dim lvi As New ListViewItem
        With lvi
            .Text = strAssemblyName
            .Tag = strAssemblyName
            If strAssemblyName = _CallingAssemblyName Then
                .Text &= " (calling)"
            End If
            If strAssemblyName = _ExecutingAssemblyName Then
                .Text &= " (executing)"
            End If
            If strAssemblyName = _EntryAssemblyName Then
                .Text &= " (entry)"
            End If
            .SubItems.Add(nvc.Item("version"))
            .SubItems.Add(nvc.Item("builddate"))
            .SubItems.Add(nvc.Item("codebase"))
            '.SubItems.Add(AssemblyVersion(a))
            '.SubItems.Add(AssemblyBuildDateString(a, True))
            '.SubItems.Add(AssemblyCodeBase(a))
        End With
        AssemblyInfoListView.Items.Add(lvi)
        AssemblyNamesComboBox.Items.Add(strAssemblyName)
    End Sub

    ''' <summary>
    ''' retrieves a cached value from the entry assembly attribute lookup collection
    ''' </summary>
    Private Function EntryAssemblyAttrib(ByVal strName As String) As String
        If _EntryAssemblyAttribCollection(strName) = "" Then
            Return "<Assembly: Assembly" & strName & "("""")>"
        Else
            Return _EntryAssemblyAttribCollection(strName).ToString
        End If
    End Function

    ''' <summary>
    ''' perform assemblyinfo to string replacements on labels
    ''' </summary>
    Private Function ReplaceTokens(ByVal s As String) As String
        s = s.Replace("%title%", EntryAssemblyAttrib("title"))
        s = s.Replace("%copyright%", EntryAssemblyAttrib("copyright"))
        s = s.Replace("%description%", EntryAssemblyAttrib("description"))
        s = s.Replace("%company%", EntryAssemblyAttrib("company"))
        s = s.Replace("%product%", EntryAssemblyAttrib("product"))
        s = s.Replace("%trademark%", EntryAssemblyAttrib("trademark"))
        s = s.Replace("%year%", DateTime.Now.Year.ToString)
        s = s.Replace("%version%", EntryAssemblyAttrib("version"))
        s = s.Replace("%builddate%", EntryAssemblyAttrib("builddate"))
        Return s
    End Function

    ''' <summary>
    ''' populate details for a single assembly
    ''' </summary>
    Private Sub PopulateAssemblyDetails(ByVal a As System.Reflection.Assembly, ByVal lvw As ListView)
        lvw.Items.Clear()

        '-- this assembly property is only available in framework versions 1.1+
        Populate(lvw, "Image Runtime Version", a.ImageRuntimeVersion)
        Populate(lvw, "Loaded from GAC", a.GlobalAssemblyCache.ToString)

        Dim nvc As Specialized.NameValueCollection = AssemblyAttribs(a)
        For Each strKey As String In nvc
            Populate(lvw, strKey, nvc.Item(strKey))
        Next
    End Sub

    ''' <summary>
    ''' matches assembly by Assembly.GetName.Name; returns nothing if no match
    ''' </summary>
    Private Function MatchAssemblyByName(ByVal AssemblyName As String) As [Assembly]
        For Each a As [Assembly] In AppDomain.CurrentDomain.GetAssemblies
            If a.GetName.Name = AssemblyName Then
                Return a
            End If
        Next
        Return Nothing
    End Function

    ''' <summary>
    ''' returns string name / string value pair of all attribs
    ''' for specified assembly
    ''' </summary>
    ''' <remarks>
    ''' note that Assembly* values are pulled from AssemblyInfo file in project folder
    '''
    ''' Trademark       = AssemblyTrademark string
    ''' Debuggable      = True
    ''' GUID            = 7FDF68D5-8C6F-44C9-B391-117B5AFB5467
    ''' CLSCompliant    = True
    ''' Product         = AssemblyProduct string
    ''' Copyright       = AssemblyCopyright string
    ''' Company         = AssemblyCompany string
    ''' Description     = AssemblyDescription string
    ''' Title           = AssemblyTitle string
    ''' </remarks>
    Private Function AssemblyAttribs(ByVal a As System.Reflection.Assembly) As Specialized.NameValueCollection
        Dim TypeName As String
        Dim Name As String
        Dim Value As String
        Dim nvc As New Specialized.NameValueCollection
        Dim r As New Regex("(\.Assembly|\.)(?<Name>[^.]*)Attribute$", RegexOptions.IgnoreCase)

        For Each attrib As Object In a.GetCustomAttributes(False)
            TypeName = attrib.GetType().ToString
            Name = r.Match(TypeName).Groups("Name").ToString
            Value = ""
            Select Case TypeName
                Case "System.CLSCompliantAttribute"
                    Value = CType(attrib, CLSCompliantAttribute).IsCompliant
                Case "System.Diagnostics.DebuggableAttribute"
                    Value = CType(attrib, Diagnostics.DebuggableAttribute).IsJITTrackingEnabled
                Case "System.Reflection.AssemblyCompanyAttribute"
                    Value = CType(attrib, AssemblyCompanyAttribute).Company
                Case "System.Reflection.AssemblyConfigurationAttribute"
                    Value = CType(attrib, AssemblyConfigurationAttribute).Configuration
                Case "System.Reflection.AssemblyCopyrightAttribute"
                    Value = CType(attrib, AssemblyCopyrightAttribute).Copyright
                Case "System.Reflection.AssemblyDefaultAliasAttribute"
                    Value = CType(attrib, AssemblyDefaultAliasAttribute).DefaultAlias
                Case "System.Reflection.AssemblyDelaySignAttribute"
                    Value = CType(attrib, AssemblyDelaySignAttribute).DelaySign
                Case "System.Reflection.AssemblyDescriptionAttribute"
                    Value = CType(attrib, AssemblyDescriptionAttribute).Description
                Case "System.Reflection.AssemblyInformationalVersionAttribute"
                    Value = CType(attrib, AssemblyInformationalVersionAttribute).InformationalVersion
                Case "System.Reflection.AssemblyKeyFileAttribute"
                    Value = CType(attrib, AssemblyKeyFileAttribute).KeyFile
                Case "System.Reflection.AssemblyProductAttribute"
                    Value = CType(attrib, AssemblyProductAttribute).Product
                Case "System.Reflection.AssemblyTrademarkAttribute"
                    Value = CType(attrib, AssemblyTrademarkAttribute).Trademark
                Case "System.Reflection.AssemblyTitleAttribute"
                    Value = CType(attrib, AssemblyTitleAttribute).Title
                Case "System.Resources.NeutralResourcesLanguageAttribute"
                    Value = CType(attrib, Resources.NeutralResourcesLanguageAttribute).CultureName
                Case "System.Resources.SatelliteContractVersionAttribute"
                    Value = CType(attrib, Resources.SatelliteContractVersionAttribute).Version
                Case "System.Runtime.InteropServices.ComCompatibleVersionAttribute"
                    Dim x As Runtime.InteropServices.ComCompatibleVersionAttribute
                    x = CType(attrib, Runtime.InteropServices.ComCompatibleVersionAttribute)
                    Value = x.MajorVersion & "." & x.MinorVersion & "." & x.RevisionNumber & "." & x.BuildNumber
                Case "System.Runtime.InteropServices.ComVisibleAttribute"
                    Value = CType(attrib, Runtime.InteropServices.ComVisibleAttribute).Value
                Case "System.Runtime.InteropServices.GuidAttribute"
                    Value = CType(attrib, Runtime.InteropServices.GuidAttribute).Value
                Case "System.Runtime.InteropServices.TypeLibVersionAttribute"
                    Dim x As Runtime.InteropServices.TypeLibVersionAttribute
                    x = CType(attrib, Runtime.InteropServices.TypeLibVersionAttribute)
                    Value = x.MajorVersion & "." & x.MinorVersion
                Case "System.Security.AllowPartiallyTrustedCallersAttribute"
                    Value = "(Present)"
                Case Else
                    '-- debug.writeline("** unknown assembly attribute '" & TypeName & "'")
                    Value = TypeName
            End Select

            If nvc.Item(Name) = "" Then
                nvc.Add(Name, Value)
            End If
        Next

        '-- add some extra values that are not in the AssemblyInfo, but nice to have
        With nvc
            '-- codebase
            Try
                .Add("CodeBase", a.CodeBase.Replace("file:///", ""))
            Catch ex As System.NotSupportedException
                .Add("CodeBase", "(not supported)")
            End Try
            '-- build date
            Dim dt As DateTime = AssemblyBuildDate(a)
            If dt = DateTime.MaxValue Then
                .Add("BuildDate", "(unknown)")
            Else
                .Add("BuildDate", dt.ToString("yyyy-MM-dd hh:mm tt"))
            End If
            '-- location
            Try
                .Add("Location", a.Location)
            Catch ex As System.NotSupportedException
                .Add("Location", "(not supported)")
            End Try
            '-- version
            Try
                If a.GetName.Version.Major = 0 And a.GetName.Version.Minor = 0 Then
                    .Add("Version", "(unknown)")
                Else
                    .Add("Version", a.GetName.Version.ToString)
                End If
            Catch ex As Exception
                .Add("Version", "(unknown)")
            End Try

            .Add("FullName", a.FullName)
        End With

        Return nvc
    End Function

    ''' <summary>
    ''' exception-safe retrieval of LastWriteTime for this assembly.
    ''' </summary>
    ''' <returns>File.GetLastWriteTime, or DateTime.MaxValue if exception was encountered.</returns>
    Private Shared Function AssemblyLastWriteTime(ByVal a As System.Reflection.Assembly) As DateTime
        Try
            Return File.GetLastWriteTime(a.Location)
        Catch ex As Exception
            Return DateTime.MaxValue
        End Try
    End Function

    ''' <summary>
    ''' Returns DateTime this Assembly was last built. Will attempt to calculate from build number, if possible. 
    ''' If not, the actual LastWriteTime on the assembly file will be returned.
    ''' </summary>
    ''' <param name="a">Assembly to get build date for</param>
    ''' <param name="ForceFileDate">Don't attempt to use the build number to calculate the date</param>
    ''' <returns>DateTime this assembly was last built</returns>
    Private Shared Function AssemblyBuildDate(ByVal a As System.Reflection.Assembly,
        Optional ByVal ForceFileDate As Boolean = False) As DateTime

        Dim AssemblyVersion As System.Version = a.GetName.Version
        Dim dt As DateTime

        If ForceFileDate Then
            dt = AssemblyLastWriteTime(a)
        Else
            dt = CType("01/01/2000", DateTime).
                AddDays(AssemblyVersion.Build).
                AddSeconds(AssemblyVersion.Revision * 2)
            If TimeZone.IsDaylightSavingTime(dt, TimeZone.CurrentTimeZone.GetDaylightChanges(dt.Year)) Then
                dt = dt.AddHours(1)
            End If
            If dt > DateTime.Now Or AssemblyVersion.Build < 730 Or AssemblyVersion.Revision = 0 Then
                dt = AssemblyLastWriteTime(a)
            End If
        End If

        Return dt
    End Function

    ''' <summary>
    ''' if a new assembly is selected from the combo box, show details for that assembly
    ''' </summary>
    Private Sub AssemblyNamesComboBox_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AssemblyNamesComboBox.SelectedIndexChanged
        Dim strAssemblyName As String = Convert.ToString(AssemblyNamesComboBox.SelectedItem)
        PopulateAssemblyDetails(MatchAssemblyByName(strAssemblyName), AssemblyDetailsListView)
    End Sub

    Private Sub AboutBoxNET_Shown(sender As Object, e As EventArgs) Handles Me.Shown
        PopulateAssemblies()
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        If DWSIM.App.IsRunningOnMono Then
            Dim p As New Process()
            With p
                .StartInfo.FileName = "xdg-open"
                .StartInfo.Arguments = My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "readme.txt"
                .StartInfo.UseShellExecute = False
                .Start()
            End With
        Else
            Process.Start(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "readme.txt")
        End If
    End Sub
End Class