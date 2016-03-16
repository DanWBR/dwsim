'Natural Gas Properties Plugin for DWSIM
'Copyright 2010 Daniel Medeiros

<System.Serializable()> Public Class Plugin

    Implements DWSIM.Interfaces.IUtilityPlugin

    'this variable will reference the active flowsheet in DWSIM, set before plugin's window is opened.
    Public fsheet As DWSIM.FormFlowsheet

    Public ReadOnly Property Author() As String Implements DWSIM.Interfaces.IUtilityPlugin.Author
        Get
            Return "Daniel Medeiros"
        End Get
    End Property

    Public ReadOnly Property ContactInfo() As String Implements DWSIM.Interfaces.IUtilityPlugin.ContactInfo
        Get
            Return "danielwag@gmail.com"
        End Get
    End Property

    Public ReadOnly Property CurrentFlowsheet() As DWSIM.FormFlowsheet Implements DWSIM.Interfaces.IUtilityPlugin.CurrentFlowsheet
        Get
            Return fsheet
        End Get
    End Property

    Public ReadOnly Property Description() As String Implements DWSIM.Interfaces.IUtilityPlugin.Description
        Get
            Return "Utility for calculation of Natural Gas Properties"
        End Get
    End Property

    Public ReadOnly Property DisplayMode() As DWSIM.Interfaces.IUtilityPlugin.DispMode Implements DWSIM.Interfaces.IUtilityPlugin.DisplayMode
        Get
            Return DWSIM.Interfaces.IUtilityPlugin.DispMode.Dockable
        End Get
    End Property

    Public ReadOnly Property Name() As String Implements DWSIM.Interfaces.IUtilityPlugin.Name
        Get
            Return "Natural Gas Properties"
        End Get
    End Property

    Public Function SetFlowsheet(ByRef form As DWSIM.FormFlowsheet) As Boolean Implements DWSIM.Interfaces.IUtilityPlugin.SetFlowsheet
        fsheet = form
        Return True
    End Function

    Public ReadOnly Property UniqueID() As String Implements DWSIM.Interfaces.IUtilityPlugin.UniqueID
        Get
            Return "B002A8DB-0F94-48fa-8844-C6713855B1BB"
        End Get
    End Property

    'this is called by DWSIM to open the form, so we need to pass the reference to the flowsheet to the form BEFORE returning it.
    Public ReadOnly Property UtilityForm() As System.Windows.Forms.Form Implements DWSIM.Interfaces.IUtilityPlugin.UtilityForm
        Get
            Dim f As New Form1
            f.fsheet = Me.fsheet
            Return f
        End Get
    End Property

    Public ReadOnly Property WebSite() As String Implements DWSIM.Interfaces.IUtilityPlugin.WebSite
        Get
            Return "http://dwsim.inforside.com.br"
        End Get
    End Property

End Class
