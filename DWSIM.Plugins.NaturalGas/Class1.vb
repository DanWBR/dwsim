'Natural Gas Properties Plugin for DWSIM
'Copyright 2010-2017 Daniel Wagner

Imports DWSIM.Interfaces

<System.Serializable()> Public Class Plugin

    Implements DWSIM.Interfaces.IUtilityPlugin, DWSIM.Interfaces.IUtilityPlugin5

    'this variable will reference the active flowsheet in DWSIM, set before plugin's window is opened.
    Public fsheet As DWSIM.FormFlowsheet
    Public fsheet2 As DWSIM.FlowsheetBase.FlowsheetBase

    Public ReadOnly Property Author() As String Implements DWSIM.Interfaces.IUtilityPlugin.Author, IUtilityPlugin5.Author
        Get
            Return "Daniel Wagner"
        End Get
    End Property

    Public ReadOnly Property ContactInfo() As String Implements DWSIM.Interfaces.IUtilityPlugin.ContactInfo, IUtilityPlugin5.ContactInfo
        Get
            Return "danielwag@gmail.com"
        End Get
    End Property

    Public ReadOnly Property CurrentFlowsheet() As IFlowsheet Implements DWSIM.Interfaces.IUtilityPlugin.CurrentFlowsheet, IUtilityPlugin5.CurrentFlowsheet
        Get
            Return fsheet
        End Get
    End Property

    Public ReadOnly Property Description() As String Implements DWSIM.Interfaces.IUtilityPlugin.Description, IUtilityPlugin5.Description
        Get
            Return "Utility for calculation of Natural Gas Properties"
        End Get
    End Property

    Public ReadOnly Property DisplayMode() As DWSIM.Interfaces.IUtilityPlugin.DispMode Implements DWSIM.Interfaces.IUtilityPlugin.DisplayMode
        Get
            Return DWSIM.Interfaces.IUtilityPlugin.DispMode.Dockable
        End Get
    End Property

    Public ReadOnly Property Name() As String Implements DWSIM.Interfaces.IUtilityPlugin.Name, IUtilityPlugin5.Name
        Get
            Return "Natural Gas Properties"
        End Get
    End Property

    Public Function SetFlowsheet(form As IFlowsheet) As Boolean Implements DWSIM.Interfaces.IUtilityPlugin.SetFlowsheet, IUtilityPlugin5.SetFlowsheet
        If TypeOf form Is DWSIM.FlowsheetBase.FlowsheetBase Then
            fsheet2 = CType(form, DWSIM.FlowsheetBase.FlowsheetBase)
        Else
            fsheet = CType(form, DWSIM.FormFlowsheet)
        End If
        Return True
    End Function

    Public ReadOnly Property UniqueID() As String Implements DWSIM.Interfaces.IUtilityPlugin.UniqueID, IUtilityPlugin5.UniqueID
        Get
            Return "B002A8DB-0F94-48fa-8844-C6713855B1BB"
        End Get
    End Property

    'this is called by DWSIM to open the form, so we need to pass the reference to the flowsheet to the form BEFORE returning it.
    Public ReadOnly Property UtilityForm() As Object Implements DWSIM.Interfaces.IUtilityPlugin.UtilityForm, IUtilityPlugin5.UtilityForm
        Get
            If Not fsheet Is Nothing Then
                Dim f As New Form1
                f.fsheet = Me.fsheet
                Return f
            Else
                Return New EtoForm(fsheet2).GetForm()
            End If
        End Get
    End Property

    Public ReadOnly Property WebSite() As String Implements DWSIM.Interfaces.IUtilityPlugin.WebSite, IUtilityPlugin5.WebSite
        Get
            Return "http://dwsim.inforside.com.br"
        End Get
    End Property

    Public Function Run(args As Object) As Object Implements IUtilityPlugin5.Run
        Return Nothing
    End Function

End Class
