Imports CapeOpen
Imports DWSIM.Thermodynamics.PropertyPackages
Imports Cudafy
Imports System.Runtime.InteropServices
Imports System.IO
Imports System.Reflection

<System.Serializable()>
<ComClass(CAPEOPENManager.ClassId, CAPEOPENManager.InterfaceId, CAPEOPENManager.EventsId)>
Public Class CAPEOPENManager

    Implements ICapeIdentification, ICapeThermoPropertyPackageManager, ICapeUtilities

    'CAPE-OPEN Error Interfaces
    Implements ECapeUser, ECapeUnknown, ECapeRoot

    Implements IDisposable

    Public Const ClassId As String = "7f5822f2-098d-46dd-9b89-0189d666edb1"
    Public Const InterfaceId As String = "54dd580a-9931-48f9-b139-e6279a4bfc06"
    Public Const EventsId As String = "cc6f7907-aad1-41a5-adab-24825cd73c05"

    Private _name, _description As String
    Private _params As ParameterCollection

    Private _scontext As Object

    Private folderPath As String = ""

    Sub New()

        _name = "DWSIM Property Package Manager"
        _description = "Exposes DWSIM Property Packages to clients using CAPE-OPEN Thermodynamic Interface Definitions"

    End Sub

    Public Function GetPropertyPackage(ByVal PackageName As String) As Object Implements ICapeThermoPropertyPackageManager.GetPropertyPackage
        Dim pp As PropertyPackage = Nothing
        Select Case PackageName
            Case "CoolProp"
                pp = New CoolPropPropertyPackage(True)
                pp.ComponentDescription = Calculator.GetLocalString("DescCPPP")
            Case "Peng-Robinson (PR)"
                pp = New PengRobinsonPropertyPackage(True)
                pp.ComponentDescription = Calculator.GetLocalString("DescPengRobinsonPP")
            Case "Peng-Robinson 1978 (PR78)"
                pp = New PengRobinson1978PropertyPackage(True)
            Case "Peng-Robinson-Stryjek-Vera 2 (PRSV2-M)", "Peng-Robinson-Stryjek-Vera 2 (PRSV2)"
                pp = New PRSV2PropertyPackage(True)
                pp.ComponentDescription = Calculator.GetLocalString("DescPRSV2PP")
            Case "Peng-Robinson-Stryjek-Vera 2 (PRSV2-VL)"
                pp = New PRSV2VLPropertyPackage(True)
                pp.ComponentDescription = Calculator.GetLocalString("DescPRSV2VLPP")
            Case "Soave-Redlich-Kwong (SRK)"
                pp = New SRKPropertyPackage(True)
                pp.ComponentDescription = Calculator.GetLocalString("DescSoaveRedlichKwongSRK")
            Case "UNIFAC"
                pp = New UNIFACPropertyPackage(True)
                pp.ComponentDescription = Calculator.GetLocalString("DescUPP")
            Case "UNIFAC-LL"
                pp = New UNIFACLLPropertyPackage(True)
                pp.ComponentDescription = Calculator.GetLocalString("DescUPP")
            Case "NRTL"
                pp = New NRTLPropertyPackage(True)
                pp.ComponentDescription = Calculator.GetLocalString("DescNRTLPP")
            Case "UNIQUAC"
                pp = New UNIQUACPropertyPackage(True)
                pp.ComponentDescription = Calculator.GetLocalString("DescUNIQUACPP")
            Case "Wilson"
                pp = New WilsonPropertyPackage()
                pp.ComponentDescription = Calculator.GetLocalString("Wilson Property Package")
            Case "Modified UNIFAC (Dortmund)"
                pp = New MODFACPropertyPackage(True)
                pp.ComponentDescription = Calculator.GetLocalString("DescMUPP")
            Case "Modified UNIFAC (NIST)"
                pp = New NISTMFACPropertyPackage(True)
                pp.ComponentDescription = Calculator.GetLocalString("DescNUPP")
            Case "Chao-Seader"
                pp = New ChaoSeaderPropertyPackage(True)
                pp.ComponentDescription = Calculator.GetLocalString("DescCSLKPP")
            Case "Grayson-Streed"
                pp = New GraysonStreedPropertyPackage(True)
                pp.ComponentDescription = Calculator.GetLocalString("DescGSLKPP")
            Case "Lee-Kesler-Plöcker"
                pp = New LKPPropertyPackage(True)
                pp.ComponentDescription = Calculator.GetLocalString("DescLKPPP")
            Case "Raoult's Law"
                pp = New RaoultPropertyPackage(True)
                pp.ComponentDescription = Calculator.GetLocalString("DescRPP")
            Case "IAPWS-IF97 Steam Tables"
                pp = New SteamTablesPropertyPackage(True)
                pp.ComponentDescription = Calculator.GetLocalString("DescSteamTablesPP")
            Case "IAPWS-08 Seawater"
                pp = New SeawaterPropertyPackage(True)
                pp.ComponentDescription = Calculator.GetLocalString("DescSEAPP")
            Case Else
                Dim otherpps = SharedClasses.Utility.LoadAdditionalPropertyPackages()
                Dim p0 = otherpps.Where(Function(x) DirectCast(x, ICapeIdentification).ComponentName = PackageName)
                If p0.Count > 0 Then
                    pp = DirectCast(p0(0), PropertyPackage)
                    Settings.CAPEOPENMode = True
                    pp.InitCO()
                    pp.Initialize()
                Else
                    Throw New CapeBadArgumentException("Property Package not found.")
                End If
        End Select
        If Not pp Is Nothing Then pp.ComponentName = PackageName
        Return pp
    End Function

    Public Function GetPropertyPackageList() As Object Implements ICapeThermoPropertyPackageManager.GetPropertyPackageList
        Dim l As New List(Of String)({"CoolProp", "Peng-Robinson (PR)", "Peng-Robinson 1978 (PR78)", "Peng-Robinson-Stryjek-Vera 2 (PRSV2-M)", "Peng-Robinson-Stryjek-Vera 2 (PRSV2-VL)", "Soave-Redlich-Kwong (SRK)",
                             "UNIFAC", "UNIFAC-LL", "Modified UNIFAC (Dortmund)", "Modified UNIFAC (NIST)", "NRTL", "UNIQUAC",
                            "Chao-Seader", "Grayson-Streed", "Lee-Kesler-Plöcker", "Raoult's Law", "IAPWS-IF97 Steam Tables", "IAPWS-08 Seawater"})
        Try
            Dim otherpps = SharedClasses.Utility.LoadAdditionalPropertyPackages()
            For Each pp In otherpps
                l.Add(DirectCast(pp, CapeOpen.ICapeIdentification).ComponentName)
            Next
        Catch ex As Exception
            MsgBox(ex.ToString)
        End Try
        Return l.ToArray
    End Function

    Public Property ComponentDescription() As String Implements ICapeIdentification.ComponentDescription
        Get
            Return _description
        End Get
        Set(ByVal value As String)
            _description = value
        End Set
    End Property

    Public Property ComponentName() As String Implements ICapeIdentification.ComponentName
        Get
            Return _name
        End Get
        Set(ByVal value As String)
            _name = value
        End Set
    End Property

    Public Sub Edit() Implements ICapeUtilities.Edit
        Throw New CapeNoImplException("Edit() not implemented.")
    End Sub

    Public Sub Initialize() Implements ICapeUtilities.Initialize

        If Not Settings.InitializedCOPPM Then

            folderPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

            Application.EnableVisualStyles()

            My.Application.ChangeCulture("en")
            My.Application.ChangeUICulture("en")

            _params = New ParameterCollection()

            'set CUDA params

            CudafyModes.Compiler = eGPUCompiler.All
            CudafyModes.Target = GlobalSettings.Settings.CudafyTarget

            'load settings

            Try
                Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"
                If File.Exists(inifile) Then GlobalSettings.Settings.LoadExcelSettings(inifile)
            Catch ex As Exception
            End Try

            'handler for unhandled exceptions

            Try
                Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException)

                AddHandler Application.ThreadException, AddressOf UnhandledException

                AddHandler AppDomain.CurrentDomain.UnhandledException, AddressOf UnhandledException2

                AddHandler AppDomain.CurrentDomain.AssemblyResolve, Function(sender, args)
                                                                        Dim aname = New AssemblyName(args.Name).Name
                                                                        If aname = "Microsoft.WindowsAPICodePack.Shell" Then Return Nothing
                                                                        If aname = "Microsoft.WindowsAPICodePack" Then Return Nothing
                                                                        Dim assemblyPath As String = Path.Combine(folderPath, aname + ".dll")
                                                                        If Not File.Exists(assemblyPath) Then
                                                                            Return Assembly.Load(args.Name)
                                                                        Else
                                                                            Return Assembly.LoadFrom(assemblyPath)
                                                                        End If
                                                                    End Function
            Catch ex As Exception
            End Try

            Settings.InitializedCOPPM = True

        End If

    End Sub

    Private Sub UnhandledException(ByVal sender As Object, ByVal e As System.Threading.ThreadExceptionEventArgs)

        Try
            Dim frmEx As New FormUnhandledException
            frmEx.TextBox1.Text = e.Exception.ToString
            frmEx.ex = e.Exception
            frmEx.ShowDialog()
        Finally
        End Try

        If Settings.CAPEOPENMode Then
            Dim hcode As Integer = 0
            Dim comEx As COMException = New COMException(e.Exception.Message.ToString, e.Exception)
            If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
            ThrowCAPEException(e.Exception, "Error", e.Exception.Message, "UnhandledException", e.Exception.Source, e.Exception.StackTrace, "UnhandledException", hcode)
        End If

    End Sub

    Private Sub UnhandledException2(ByVal sender As Object, ByVal e As System.UnhandledExceptionEventArgs)

        Try
            Dim frmEx As New FormUnhandledException
            frmEx.TextBox1.Text = e.ExceptionObject.ToString
            frmEx.ex = e.ExceptionObject
            frmEx.ShowDialog()
        Catch ex As Exception
        End Try

        If Settings.CAPEOPENMode Then
            Dim hcode As Integer = 0
            Dim comEx As COMException = e.ExceptionObject
            If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
            ThrowCAPEException(e.ExceptionObject, "Error", e.ExceptionObject.ToString, "UnhandledException", e.ExceptionObject.ToString, "", "UnhandledException", hcode)
        End If

    End Sub

    Public ReadOnly Property parameters() As Object Implements ICapeUtilities.parameters
        Get
            Return _params
        End Get
    End Property

    <Runtime.InteropServices.ComVisible(False)> Public WriteOnly Property simulationContext() As Object Implements ICapeUtilities.simulationContext
        Set(ByVal value As Object)
            _scontext = value
        End Set
    End Property

    Public Sub Terminate() Implements ICapeUtilities.Terminate

        If Not _scontext Is Nothing Then
            If System.Runtime.InteropServices.Marshal.IsComObject(_scontext) Then
                System.Runtime.InteropServices.Marshal.ReleaseComObject(_scontext)
            End If
        End If

        Me.simulationContext = Nothing

        Me.Dispose()

    End Sub

    Private disposedValue As Boolean = False        ' To detect redundant calls

    ' IDisposable
    Protected Overridable Sub Dispose(ByVal disposing As Boolean)
        If Not Me.disposedValue Then
            If disposing Then
                ' TODO: free other state (managed objects).
            End If
        End If
        Me.disposedValue = True
    End Sub

#Region " IDisposable Support "
    ' This code added by Visual Basic to correctly implement the disposable pattern.
    Public Sub Dispose() Implements IDisposable.Dispose
        ' Do not change this code.  Put cleanup code in Dispose(ByVal disposing As Boolean) above.
        Dispose(True)
        GC.SuppressFinalize(Me)
    End Sub
#End Region

#Region "CAPE-OPEN Error Interfaces"

    Sub ThrowCAPEException(ByVal ex As Exception, ByVal name As String, ByVal description As String, ByVal interf As String, ByVal moreinfo As String, ByVal operation As String, ByVal scope As String, ByVal code As Integer)

        _code = code
        _edescription = description
        _interfacename = interf
        _moreinfo = moreinfo
        _operation = operation
        _scope = scope

        Throw New CapeOpen.CapeUnknownException(ex.Message.ToArray, ex)

    End Sub

    Private _edescription, _interfacename, _moreinfo, _operation, _scope As String, _code As Integer

    Public ReadOnly Property Name2() As String Implements CapeOpen.ECapeRoot.Name
        Get
            Return Me.ComponentName
        End Get
    End Property

    Public ReadOnly Property code() As Integer Implements CapeOpen.ECapeUser.code
        Get
            Return _code
        End Get
    End Property

    Public ReadOnly Property description() As String Implements CapeOpen.ECapeUser.description
        Get
            Return _edescription
        End Get
    End Property

    Public ReadOnly Property interfaceName() As String Implements CapeOpen.ECapeUser.interfaceName
        Get
            Return _interfacename
        End Get
    End Property

    Public ReadOnly Property moreInfo() As String Implements CapeOpen.ECapeUser.moreInfo
        Get
            Return _moreinfo
        End Get
    End Property

    Public ReadOnly Property operation() As String Implements CapeOpen.ECapeUser.operation
        Get
            Return _operation
        End Get
    End Property

    Public ReadOnly Property scope() As String Implements CapeOpen.ECapeUser.scope
        Get
            Return _scope
        End Get
    End Property

#End Region

    <System.Runtime.InteropServices.ComRegisterFunction()> _
    Private Shared Sub RegisterFunction(ByVal t As Type)

        Dim keyname As String = String.Concat("CLSID\\{", t.GUID.ToString, "}\\Implemented Categories")
        Dim key As Microsoft.Win32.RegistryKey = Microsoft.Win32.Registry.ClassesRoot.OpenSubKey(keyname, True)
        If key Is Nothing Then
            key = Microsoft.Win32.Registry.ClassesRoot.CreateSubKey(keyname)
        End If
        key.CreateSubKey("{CF51E383-0110-4ed8-ACB7-B50CFDE6908E}") ' CAPE-OPEN 1.1 PPM
        'key.CreateSubKey("{678c09a3-7d66-11d2-a67d-00105a42887f}") ' CAPE-OPEN 1.0 TS
        key.CreateSubKey("{678C09A1-7D66-11D2-A67D-00105A42887F}") ' CAPE-OPEN Object 
        keyname = String.Concat("CLSID\\{", t.GUID.ToString, "}\\CapeDescription")
        key = Microsoft.Win32.Registry.ClassesRoot.CreateSubKey(keyname)
        key.SetValue("Name", "DWSIM Property Package Manager")
        key.SetValue("Description", "DWSIM CAPE-OPEN Property Package Manager")
        key.SetValue("CapeVersion", "1.1")
        key.SetValue("ComponentVersion", My.Application.Info.Version.ToString)
        key.SetValue("VendorURL", "http://dwsim.inforside.com.br")
        key.SetValue("HelpURL", "http://dwsim.inforside.com.br")
        key.SetValue("About", "DWSIM is open-source software, released under the GPL v3 license. (c) 2011-2017 Daniel Medeiros.")
        key.Close()

    End Sub

    <System.Runtime.InteropServices.ComUnregisterFunction()> _
    Private Shared Sub UnregisterFunction(ByVal t As Type)
        Try

            Dim keyname As String = String.Concat("CLSID\\{", t.GUID.ToString, "}")
            Dim key As Microsoft.Win32.RegistryKey = Microsoft.Win32.Registry.ClassesRoot.OpenSubKey(keyname, True)
            Dim keyNames() As String = key.GetSubKeyNames
            For Each kn As String In keyNames
                key.DeleteSubKeyTree(kn)
            Next
            Dim valueNames() As String = key.GetValueNames
            For Each valueName As String In valueNames
                key.DeleteValue(valueName)
            Next

        Catch ex As Exception

        End Try
    End Sub

End Class
