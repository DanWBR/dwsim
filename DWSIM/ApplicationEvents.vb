Imports Cudafy
Imports System.Linq
Imports System.IO
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Threading
Imports System.Runtime.InteropServices
Imports Microsoft.AppCenter
Imports Microsoft.AppCenter.Crashes
Imports Microsoft.AppCenter.Analytics
Imports System.Reflection

Namespace My

    ' The following events are availble for MyApplication:
    ' 
    ' Startup: Raised when the application starts, before the startup form is created.
    ' Shutdown: Raised after all application forms are closed.  This event is not raised if the application terminates abnormally.
    ' UnhandledException: Raised if the application encounters an unhandled exception.
    ' StartupNextInstance: Raised when launching a single-instance application and the application is already active. 
    ' NetworkAvailabilityChanged: Raised when the network connection is connected or disconnected.
    Partial Friend Class MyApplication

        <DllImport("kernel32.dll", SetLastError:=True)> Private Shared Function AddDllDirectory(lpPathName As String) As Boolean

        End Function

        Public _ResourceManager As System.Resources.ResourceManager

        Public _HelpManager As System.Resources.ResourceManager

        Public _PropertyNameManager As System.Resources.ResourceManager

        Public _CultureInfo As System.Globalization.CultureInfo

        Public Property CalculatorStopRequested As Boolean = False
        Public Property MasterCalculatorStopRequested As Boolean = False
        Public Property CommandLineMode As Boolean = False

        Public Property MainThreadId As Integer
        Public Property CalculatorBusy As Boolean = False

        Public UtilityPlugins As New Dictionary(Of String, Interfaces.IUtilityPlugin)

        Public Property PushUndoRedoAction As Boolean = True

        Public Property ActiveSimulation As FormFlowsheet

        Public Property UserUnitSystems As Dictionary(Of String, SystemsOfUnits.Units)

        Private Sub MyApplication_Shutdown(sender As Object, e As EventArgs) Handles Me.Shutdown

            'save user unit systems

            Dim xdoc As New XDocument()
            Dim xel As XElement

            xdoc = New XDocument
            xdoc.Add(New XElement("Units"))

            For Each su2 As SystemsOfUnits.Units In UserUnitSystems.Values
                xdoc.Element("Units").Add(New XElement(XmlConvert.EncodeName(su2.Name)))
                xel = xdoc.Element("Units").Element(XmlConvert.EncodeName(su2.Name))
                xel.Add(su2.SaveData())
            Next

            Using sw As New StringWriter()
                Using xw As New XmlTextWriter(sw)
                    xdoc.Save(xw)
                    My.Settings.UserUnits = sw.ToString
                End Using
            End Using

            If Not DWSIM.App.IsRunningOnMono Then
                My.Settings.Save()
            End If

        End Sub

        Private Sub MyApplication_Startup(ByVal sender As Object, ByVal e As Microsoft.VisualBasic.ApplicationServices.StartupEventArgs) Handles Me.Startup

            Directory.SetCurrentDirectory(Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly().Location))

            Control.CheckForIllegalCrossThreadCalls = False

            'upgrade settings from previous build, if applicable.
            If My.Settings.UpgradeRequired Then
                My.Settings.Upgrade()
                My.Settings.UpgradeRequired = False
            End If

            'check if the user wants to reset settings.
            If My.Computer.Keyboard.ShiftKeyDown Then
                My.Settings.Reset()
                MessageBox.Show("The settings were reset successfully.")
            End If

            'loads the current language
            If My.Settings.CultureInfo = "de" Or My.Settings.CultureInfo = "es" Or My.Settings.CultureInfo = "en-US" Then
                My.Settings.CultureInfo = "en"
            End If
            _CultureInfo = New Globalization.CultureInfo(My.Settings.CultureInfo)
            My.Application.ChangeUICulture(My.Settings.CultureInfo)

            'loads the resource manager
            _ResourceManager = New System.Resources.ResourceManager("DWSIM.DWSIM", System.Reflection.Assembly.GetExecutingAssembly())

            'loads the help manager
            _HelpManager = New System.Resources.ResourceManager("DWSIM.Tips", System.Reflection.Assembly.GetExecutingAssembly())

            'loads the property name manager
            _PropertyNameManager = New System.Resources.ResourceManager("DWSIM.Properties", System.Reflection.Assembly.GetExecutingAssembly())

            For Each s As String In My.Application.CommandLineArgs
                If s.ToLower = "-commandline" Then
                    My.Application.CommandLineMode = True
                    e.Cancel = True
                End If
                If s.ToLower = "-locale" Then
                    Dim clcult As String = My.Application.CommandLineArgs(My.Application.CommandLineArgs.IndexOf(s) + 1)
                    _CultureInfo = New Globalization.CultureInfo(clcult)
                    My.Application.ChangeUICulture(clcult)
                End If
            Next

            DWSIM.App.InitializeSettings()

            If My.Settings.PythonPath <> "" Then
                AddDllDirectory(My.Settings.PythonPath)
            Else
                My.Settings.PythonPath = Path.Combine(Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly().Location), "python\python-3.6.8.amd64\")
                If Directory.Exists(My.Settings.PythonPath) Then
                    GlobalSettings.Settings.PythonPath = My.Settings.PythonPath
                    AddDllDirectory(My.Settings.PythonPath)
                End If
            End If

            If e.Cancel Then
                'command line processor
                AttachConsole(ATTACH_PARENT_PROCESS)
                Dim standardOutput As New StreamWriter(Console.OpenStandardOutput())
                standardOutput.AutoFlush = True
                Console.SetOut(standardOutput)
                Dim f1 As New FormMain
            End If

            If Settings.SendCrashAndUsageAnalytics Then
                'enable analytics
                Dim ca = Assembly.Load("Microsoft.AppCenter.Crashes")
                Dim aa = Assembly.Load("Microsoft.AppCenter.Analytics")
                Dim at = aa.GetType("Microsoft.AppCenter.Analytics.Analytics")
                Dim ct = ca.GetType("Microsoft.AppCenter.Crashes.Crashes")
                AppCenter.Start("b28eb1b8-3d8f-4be5-a888-e272018d604d", at, ct)

                Crashes.GetErrorAttachments = Function(report)
                                                  Dim email = Settings.UserEmail
                                                  If email <> "" Then
                                                      Return New ErrorAttachmentLog() {ErrorAttachmentLog.AttachmentWithText(email, "useremail.txt")}
                                                  Else
                                                      Return Nothing
                                                  End If
                                              End Function

                Crashes.SetEnabledAsync(True)

                AddHandler FlowsheetSolver.FlowsheetSolver.FlowsheetCalculationFinished,
                    Sub(esender, eargs, data)
                        If TypeOf data Is Double Then
                            Dim datadict As New Dictionary(Of String, String)
                            datadict.Add("Time Taken (s)", data.ToString())
                            Analytics.TrackEvent("Flowsheet Calculation Finished", datadict)
                        Else
                            Analytics.TrackEvent("Flowsheet Calculation Finished with Errors")
                            Dim errorlist As List(Of Exception) = data
                            For Each er In errorlist
                                Crashes.TrackError(er)
                            Next
                        End If
                    End Sub

                AddHandler FlowsheetSolver.FlowsheetSolver.CalculationError,
                    Sub(esender, eargs, data)
                        Dim calcargs As CalculationArgs = esender
                        If data IsNot Nothing Then
                            Dim datadict As New Dictionary(Of String, String)
                            datadict.Add("Object Type", calcargs.ObjectType.ToString())
                            Crashes.TrackError(data, datadict)
                        End If
                    End Sub

                AddHandler FlowsheetSolver.FlowsheetSolver.UnitOpCalculationStarted,
                    Sub(esender, eargs, data)
                        Dim fsheet As Interfaces.IFlowsheet = esender
                        Dim calcargs As CalculationArgs = data
                        Dim ppname = fsheet.SimulationObjects(calcargs.Name).PropertyPackage.ComponentName
                        Dim ncomp = fsheet.SelectedCompounds.Count
                        Dim datadict As New Dictionary(Of String, String)
                        datadict.Add("Object Type", calcargs.ObjectType.ToString())
                        datadict.Add("Property Package", ppname)
                        datadict.Add("Compounds", ncomp)
                        Analytics.TrackEvent("Object Calculation Started", datadict)
                    End Sub

            End If

        End Sub

        Private Sub MyApplication_UnhandledException(ByVal sender As Object, ByVal e As Microsoft.VisualBasic.ApplicationServices.UnhandledExceptionEventArgs) Handles Me.UnhandledException
            If Not CommandLineMode Then
                If Not GlobalSettings.Settings.AutomationMode Then
                    Dim frmEx As New FormUnhandledException
                    frmEx.TextBox1.Text = e.Exception.ToString
                    frmEx.ex = e.Exception
                    frmEx.ShowDialog()
                    e.ExitApplication = False
                End If
            Else
                If Not GlobalSettings.Settings.AutomationMode Then
                    Console.Write(e.Exception.ToString)
                    e.ExitApplication = True
                End If
            End If
        End Sub

        <System.Runtime.InteropServices.DllImport("kernel32.dll")>
        Private Shared Function AttachConsole(dwProcessId As Integer) As Boolean
        End Function
        Private Const ATTACH_PARENT_PROCESS As Integer = -1

        <System.Runtime.InteropServices.DllImport("kernel32.dll", SetLastError:=True)>
        Friend Shared Function FreeConsole() As Integer
        End Function

    End Class

End Namespace
