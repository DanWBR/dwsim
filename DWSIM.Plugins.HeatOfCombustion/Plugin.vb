'Copyright 2020-2022 Daniel Wagner

Imports DWSIM.Interfaces

<System.Serializable()> Public Class Plugin

    Implements IUtilityPlugin, IUtilityPlugin5

    'this variable will reference the active flowsheet in DWSIM, set before plugin's window is opened.
    Public fsheet As IFlowsheet

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
            Return "Utility for calculation of Heat of Combustion of Hydrocarbons in a Material Stream"
        End Get
    End Property

    Public ReadOnly Property DisplayMode() As DWSIM.Interfaces.IUtilityPlugin.DispMode Implements DWSIM.Interfaces.IUtilityPlugin.DisplayMode
        Get
            Return DWSIM.Interfaces.IUtilityPlugin.DispMode.Normal
        End Get
    End Property

    Public ReadOnly Property Name() As String Implements DWSIM.Interfaces.IUtilityPlugin.Name, IUtilityPlugin5.Name
        Get
            Return "Hydrocarbon Heat of Combustion Calculator"
        End Get
    End Property

    Public Function SetFlowsheet(form As IFlowsheet) As Boolean Implements DWSIM.Interfaces.IUtilityPlugin.SetFlowsheet, IUtilityPlugin5.SetFlowsheet
        fsheet = form
        Return True
    End Function

    Public ReadOnly Property UniqueID() As String Implements DWSIM.Interfaces.IUtilityPlugin.UniqueID, IUtilityPlugin5.UniqueID
        Get
            Return "46BB84DD-88C1-46AB-A66A-17089904FA7F"
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
                Return Nothing
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
