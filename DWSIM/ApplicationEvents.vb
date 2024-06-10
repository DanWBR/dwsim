Imports System.IO
Imports System.Runtime.InteropServices
Imports System.Reflection
Imports System.Linq
Imports DWSIM.Interfaces

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

        Public Property ActiveSimulation As FormFlowsheet

        Public Property MainWindowForm As FormMain

        Public Property UserUnitSystems As Dictionary(Of String, SystemsOfUnits.Units)

        Private Sub MyApplication_Startup(ByVal sender As Object, ByVal e As Microsoft.VisualBasic.ApplicationServices.StartupEventArgs) Handles Me.Startup

            AddHandler AppDomain.CurrentDomain.AssemblyResolve, New ResolveEventHandler(AddressOf LoadFromExtensionsFolder)

            'Set default file picker
            'SharedClassesCSharp.FilePicker.FilePickerService.GetInstance().SetFilePickerFactory(Function() New Simulate365.FormFactories.S365FilePickerForm())

            If Environment.OSVersion.Version >= New Version(6, 3, 0) Then
                'win 8.1 added support for per monitor dpi
                If (Environment.OSVersion.Version >= New Version(10, 0, 15063)) Then
                    'creators update added support For per monitor v2
                    NativeMethods.SetProcessDpiAwarenessContext(NativeMethods.DPI_AWARENESS_CONTEXT.DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2)
                Else
                    NativeMethods.SetProcessDpiAwareness(NativeMethods.PROCESS_DPI_AWARENESS.Process_Per_Monitor_DPI_Aware)
                End If
            Else
                NativeMethods.SetProcessDPIAware()
            End If

            Directory.SetCurrentDirectory(Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly().Location))

            Control.CheckForIllegalCrossThreadCalls = True

            'upgrade settings from previous build, if applicable.
            If My.Settings.UpgradeRequired Then
                My.Settings.Upgrade()
                My.Settings.UpgradeRequired = False
            End If

            'check if the user wants to reset settings.
            If My.Computer.Keyboard.ShiftKeyDown Then
                My.Settings.Reset()
                'MessageBox.Show("Application Settings successfully reset.")
            End If

            'loads the current language
            My.Settings.CultureInfo = "en"
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
            Next

            DWSIM.App.InitializeSettings()

            If e.Cancel Then

                'command line processor

                AttachConsole(ATTACH_PARENT_PROCESS)

                Dim standardOutput As New StreamWriter(Console.OpenStandardOutput())
                standardOutput.AutoFlush = True

                Console.SetOut(standardOutput)

                Dim f1 As New FormMain

            End If

            System.Net.ServicePointManager.ServerCertificateValidationCallback =
            New Net.Security.RemoteCertificateValidationCallback(Function(o, c, ch, ssl)
                                                                     Return True
                                                                 End Function)

            System.Net.ServicePointManager.CheckCertificateRevocationList = False

        End Sub

        Private Shared Function LoadFromExtensionsFolder(ByVal sender As Object, ByVal args As ResolveEventArgs) As Assembly

            Dim assemblyPath1 As String = Path.Combine(Utility.GetDwsimRootDirectory(), New AssemblyName(args.Name).Name + ".dll")
            Dim assemblyPath2 As String = Path.Combine(Utility.GetExtendersRootDirectory(), New AssemblyName(args.Name).Name + ".dll")

            If Not File.Exists(assemblyPath1) Then
                If Not File.Exists(assemblyPath2) Then
                    Return Nothing
                Else
                    Dim assembly As Assembly = Assembly.LoadFrom(assemblyPath2)
                    Return assembly
                End If
            Else
                Dim assembly As Assembly = Assembly.LoadFrom(assemblyPath1)
                Return assembly
            End If

        End Function

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

        Private Sub MyApplication_UnhandledException(ByVal sender As Object, ByVal e As Microsoft.VisualBasic.ApplicationServices.UnhandledExceptionEventArgs) Handles Me.UnhandledException
            Logging.Logger.LogUnhandled("Unhandled Exception", e.Exception)
            If Not CommandLineMode Then
                If Not GlobalSettings.Settings.AutomationMode Then
                    If FormMain.AnalyticsProvider IsNot Nothing Then
                        FormMain.AnalyticsProvider.RegisterError("Unhandled Exception", "", e.Exception, Nothing)
                    End If
                    If ActiveSimulation IsNot Nothing Then
                        Dim euid As String = Guid.NewGuid().ToString()
                        ExceptionProcessing.ExceptionList.Exceptions.Add(euid, e.Exception)
                        ActiveSimulation.ShowMessage(String.Format("Caught unhandled exception: {0}", e.Exception.Message), IFlowsheet.MessageType.GeneralError, euid)
                    Else
                        Dim frmEx As New FormUnhandledException
                        frmEx.ex = e.Exception
                        frmEx.ShowDialog()
                    End If
                    e.ExitApplication = False
                End If
            Else
                If Not GlobalSettings.Settings.AutomationMode Then
                    Console.Write(e.Exception.ToString)
                    e.ExitApplication = True
                End If
            End If
        End Sub

        Private Function GetSplashScreen() As Form

            Dim splfile = Path.Combine(Utility.GetExtendersRootDirectory(), "SplashScreen.dll")

            If File.Exists(splfile) Then

                Dim types = Assembly.LoadFrom(splfile).GetExportedTypes()

                Dim tList As List(Of Type) = types.ToList().FindAll(Function(t) t.GetInterfaces().Contains(GetType(ISplashScreen)))

                Dim lst = tList.ConvertAll(Function(t As Type) TryCast(Activator.CreateInstance(t), ISplashScreen))

                Return lst(0).GetSplashScreen()

            Else

                Return Global.DWSIM.SplashScreen

            End If

        End Function

        <System.Runtime.InteropServices.DllImport("kernel32.dll")>
        Private Shared Function AttachConsole(dwProcessId As Integer) As Boolean
        End Function

        Private Const ATTACH_PARENT_PROCESS As Integer = -1

        <System.Runtime.InteropServices.DllImport("kernel32.dll", SetLastError:=True)>
        Friend Shared Function FreeConsole() As Integer
        End Function

    End Class

End Namespace
