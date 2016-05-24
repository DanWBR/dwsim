Imports Cudafy
Imports System.Linq
Imports System.IO
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Threading

Namespace My

    ' The following events are availble for MyApplication:
    ' 
    ' Startup: Raised when the application starts, before the startup form is created.
    ' Shutdown: Raised after all application forms are closed.  This event is not raised if the application terminates abnormally.
    ' UnhandledException: Raised if the application encounters an unhandled exception.
    ' StartupNextInstance: Raised when launching a single-instance application and the application is already active. 
    ' NetworkAvailabilityChanged: Raised when the network connection is connected or disconnected.
    Partial Friend Class MyApplication

        Public _ResourceManager As System.Resources.ResourceManager
        Public _HelpManager As System.Resources.ResourceManager
        Public _PropertyNameManager As System.Resources.ResourceManager
        Public _CultureInfo As System.Globalization.CultureInfo
        Public Property CalculatorStopRequested As Boolean = False
        Public Property MasterCalculatorStopRequested As Boolean = False
        Public Property CommandLineMode As Boolean = False

        Public Property MainThreadId As Integer
        Public Property CalculatorBusy As Boolean = False

        Public UtilityPlugins As Dictionary(Of String, Interfaces.IUtilityPlugin)

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
                Thermodynamics.NativeLibraries.Files.RemoveLibraries()
            End If

        End Sub

        Private Sub MyApplication_Startup(ByVal sender As Object, ByVal e As Microsoft.VisualBasic.ApplicationServices.StartupEventArgs) Handles Me.Startup

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
                    'Stop the start form from loading.
                    e.Cancel = True
                    CommandLineMode = True
                End If
                If s.ToLower = "-locale" Then
                    Dim clcult As String = My.Application.CommandLineArgs(My.Application.CommandLineArgs.IndexOf(s) + 1)
                    _CultureInfo = New Globalization.CultureInfo(clcult)
                    My.Application.ChangeUICulture(clcult)
                End If
            Next

            'direct console output to collection
            If My.Settings.RedirectOutput And Not CommandLineMode Then
                'Dim txtwriter As New ConsoleRedirection.TextBoxStreamWriter()
                'Console.SetOut(txtwriter)
            Else
                If Not DWSIM.App.IsRunningOnMono Then
                    AttachConsole(ATTACH_PARENT_PROCESS)
                End If
                Dim standardOutput As New StreamWriter(Console.OpenStandardOutput())
                standardOutput.AutoFlush = True
                Console.SetOut(standardOutput)
            End If

            InitializeSettings()

        End Sub

        Public Sub InitializeSettings()

            'set CUDA params
            CudafyModes.Compiler = eGPUCompiler.All
            CudafyModes.Target = My.Settings.CudafyTarget

            'set global settings
            GlobalSettings.Settings.MaxDegreeOfParallelism = My.Settings.MaxDegreeOfParallelism
            GlobalSettings.Settings.EnableParallelProcessing = My.Settings.EnableParallelProcessing
            GlobalSettings.Settings.EnableGPUProcessing = My.Settings.EnableGPUProcessing
            GlobalSettings.Settings.CudafyTarget = My.Settings.CudafyTarget
            GlobalSettings.Settings.CudafyDeviceID = My.Settings.CudafyDeviceID
            GlobalSettings.Settings.UseSIMDExtensions = My.Settings.UseSIMDExtensions
            GlobalSettings.Settings.DebugLevel = My.Settings.DebugLevel
            GlobalSettings.Settings.DefaultEditFormLocation = My.Settings.DefaultEditorLocation
            GlobalSettings.Settings.SolverBreakOnException = My.Settings.SolverBreakOnException

            GlobalSettings.Settings.CalculatorActivated = True

            If GlobalSettings.Settings.EnableGPUProcessing Then Calculator.InitComputeDevice()

            Thermodynamics.NativeLibraries.Files.InitLibraries()

        End Sub

        Private Sub MyApplication_UnhandledException(ByVal sender As Object, ByVal e As Microsoft.VisualBasic.ApplicationServices.UnhandledExceptionEventArgs) Handles Me.UnhandledException
            If Not CommandLineMode Then
                Dim frmEx As New FormUnhandledException
                frmEx.TextBox1.Text = e.Exception.ToString
                frmEx.ex = e.Exception
                frmEx.ShowDialog()
                e.ExitApplication = False
            Else
                Console.Write(e.Exception.ToString)
                e.ExitApplication = True
            End If
        End Sub

        <System.Runtime.InteropServices.DllImport("kernel32.dll")> _
        Private Shared Function AttachConsole(dwProcessId As Integer) As Boolean
        End Function
        Private Const ATTACH_PARENT_PROCESS As Integer = -1

        <System.Runtime.InteropServices.DllImport("kernel32.dll", SetLastError:=True)> _
        Friend Shared Function FreeConsole() As Integer
        End Function

    End Class

End Namespace
