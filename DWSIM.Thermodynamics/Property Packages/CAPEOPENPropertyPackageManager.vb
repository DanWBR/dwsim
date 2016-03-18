Imports CapeOpen
Imports DWSIM.Thermodynamics.PropertyPackages

<System.Serializable()> _
<System.Runtime.InteropServices.Guid("FB964392-7410-432d-A650-C611D09A0C62")> _
Public Class CAPEOPENPropertyPackageManager

    Implements ICapeIdentification, ICapeThermoPropertyPackageManager, ICapeUtilities
    Implements IDisposable

    Private _name, _description As String
    Private _params As ParameterCollection

    Sub New()
        _name = "DWSIM Property Package Manager"
        _description = "Exposes DWSIM Property Packages to clients using CAPE-OPEN Thermodynamic Interface Definitions"
    End Sub

    Public Function GetPropertyPackage(ByVal PackageName As String) As Object Implements ICapeThermoPropertyPackageManager.GetPropertyPackage
        Dim pp As PropertyPackage = Nothing
        Select Case PackageName
            Case "FPROPS"
                pp = New FPROPSPropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescFPP")
            Case "CoolProp"
                pp = New CoolPropPropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescCPPP")
            Case "PC-SAFT"
                pp = New PCSAFTPropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescPCSAFTPP")
            Case "Peng-Robinson (PR)"
                pp = New PengRobinsonPropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescPengRobinsonPP")
            Case "Peng-Robinson-Stryjek-Vera 2 (PRSV2-M)", "Peng-Robinson-Stryjek-Vera 2 (PRSV2)"
                pp = New PRSV2PropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescPRSV2PP")
            Case "Peng-Robinson-Stryjek-Vera 2 (PRSV2-VL)"
                pp = New PRSV2VLPropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescPRSV2VLPP")
            Case "Soave-Redlich-Kwong (SRK)"
                pp = New SRKPropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescSoaveRedlichKwongSRK")
            Case "Peng-Robinson / Lee-Kesler (PR/LK)"
                pp = New PengRobinsonLKPropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescPRLK")
            Case "UNIFAC"
                pp = New UNIFACPropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescUPP")
            Case "UNIFAC-LL"
                pp = New UNIFACLLPropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescUPP")
            Case "NRTL"
                pp = New NRTLPropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescNRTLPP")
            Case "UNIQUAC"
                pp = New UNIQUACPropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescUNIQUACPP")
            Case "Modified UNIFAC (Dortmund)"
                pp = New MODFACPropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescMUPP")
            Case "Modified UNIFAC (NIST)"
                pp = New NISTMFACPropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescNUPP")
            Case "Chao-Seader"
                pp = New ChaoSeaderPropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescCSLKPP")
            Case "Grayson-Streed"
                pp = New GraysonStreedPropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescGSLKPP")
            Case "Lee-Kesler-Plöcker"
                pp = New LKPPropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescLKPPP")
            Case "Raoult's Law"
                pp = New RaoultPropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescRPP")
            Case "IAPWS-IF97 Steam Tables"
                pp = New SteamTablesPropertyPackage(True)
                pp.ComponentDescription = App.GetLocalString("DescSteamTablesPP")
            Case Else
                Throw New CapeBadArgumentException("Property Package not found.")
        End Select
        If Not pp Is Nothing Then pp.ComponentName = PackageName
        Return pp
    End Function

    Public Function GetPropertyPackageList() As Object Implements ICapeThermoPropertyPackageManager.GetPropertyPackageList
        Return New String() {"FPROPS", "CoolProp", "PC-SAFT", "Peng-Robinson (PR)", "Peng-Robinson-Stryjek-Vera 2 (PRSV2-M)", "Peng-Robinson-Stryjek-Vera 2 (PRSV2-VL)", "Soave-Redlich-Kwong (SRK)", "Peng-Robinson / Lee-Kesler (PR/LK)", _
                             "UNIFAC", "UNIFAC-LL", "Modified UNIFAC (Dortmund)", "Modified UNIFAC (NIST)", "NRTL", "UNIQUAC", _
                            "Chao-Seader", "Grayson-Streed", "Lee-Kesler-Plöcker", "Raoult's Law", "COSMO-SAC (JCOSMO)", "IAPWS-IF97 Steam Tables"}
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
        _params = New ParameterCollection()
    End Sub

    Public ReadOnly Property parameters() As Object Implements ICapeUtilities.parameters
        Get
            Return _params
        End Get
    End Property

    Public WriteOnly Property simulationContext() As Object Implements ICapeUtilities.simulationContext
        Set(ByVal value As Object)
            'do nothing
        End Set
    End Property

    Public Sub Terminate() Implements ICapeUtilities.Terminate
        Me.Dispose()
    End Sub

    Private disposedValue As Boolean = False        ' To detect redundant calls

    ' IDisposable
    Protected Overridable Sub Dispose(ByVal disposing As Boolean)
        If Not Me.disposedValue Then
            If disposing Then
                ' TODO: free other state (managed objects).
            End If

            ' TODO: free your own state (unmanaged objects).
            ' TODO: set large fields to null.
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
        key.SetValue("About", "DWSIM is open-source software, released under the GPL v3 license. (c) 2011-2015 Daniel Medeiros.")
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
