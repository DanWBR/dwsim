Imports System.Net.Http
Imports System.Text
Imports System.Windows.Forms
Imports DWSIM.Interfaces
Imports DWSIM.Simulate365.Models
Imports DWSIM.Simulate365.Services
Imports DWSIM.UI.Web.Settings
Imports Newtonsoft.Json

Public Class FormPortal
    Private IsLoadingLicenseInfo As Boolean = False
    Private TrialLicenseCreated As Boolean = False
    Private NotEligibleForTrial As Boolean = False
    Private TrialLicenseCreatedMessage As String = ""
    Private OpeningMessage As String = ""
    Private TrialLicenseCreatedMessageCount As Int32 = 0
    Private ShowInfoMessage As Boolean = False
    Private FileSavingInProgress As Boolean = False
    Private Timer1 As New Timer With {.Interval = TimeSpan.FromSeconds(5).Milliseconds}
    Public fsheet As DWSIM.FlowsheetBase.FlowsheetBase

    Private TrialLicenseCreatedMessages As New List(Of String) From {
        "Assigning DWSIM Pro license...",
        "Setting up the virtual licensing environment...",
        "Finalizing setup..."
    }

    Sub New(fs As IFlowsheet)
        fsheet = CType(fs, DWSIM.FlowsheetBase.FlowsheetBase)
    End Sub
    Private Sub FormPortal_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        AddHandler FileManagementService.GetInstance().OnFileSavedToDashboard, AddressOf FileManagementService_FileSavedToDashboard
        OnInitialize()
    End Sub

    Private Async Sub OnInitialize()
        Dim licenseInfo As LicenseResponseModel = Await GetLicenseInfo()
        If licenseInfo IsNot Nothing Then
            If licenseInfo.hasExistingLicense.HasValue And licenseInfo.hasExistingLicense.Value Then
                SaveFlowsheet()
            ElseIf licenseInfo.notEligibleForTrial.HasValue And licenseInfo.notEligibleForTrial.Value Then
                LoadingPanel.Visible = False
                NoLicensePanel.Visible = True
            ElseIf licenseInfo.trialLicenseCreated And licenseInfo.trialLicenseCreated.Value Then
                TrialLicenseCreatedLogic()
            End If
        End If
    End Sub

    Private Sub TrialLicenseCreatedLogic()
        AddHandler Timer1.Tick, AddressOf Timer1_Tick
        Timer1.Start()
    End Sub
    Private Sub Timer1_Tick()
        If TrialLicenseCreatedMessageCount + 1 <= TrialLicenseCreatedMessages.Count Then
            StatusMessage.Text = TrialLicenseCreatedMessages(TrialLicenseCreatedMessageCount)
            TrialLicenseCreatedMessageCount += 1
            ProgressBar1.Value = Convert.ToInt32((TrialLicenseCreatedMessageCount / TrialLicenseCreatedMessages.Count) * 100)
        Else
            Timer1.Stop()
            SaveFlowsheet()
        End If

    End Sub

    Private Async Sub FileManagementService_FileSavedToDashboard(sender As Object, e As EventArgs)
        If FileSavingInProgress Then
            FileSavingInProgress = False
            Dim actionSaved As Boolean = Await SaveDwsimProStartupAction()
            RedirectToDWSIMPro()
        End If
    End Sub

    Private Async Function GetLicenseInfo() As Task(Of LicenseResponseModel)
        StatusMessage.Text = "Checking for DWSIM Pro license..."
        Dim url As String = $"{DashboardSettings.DashboardServiceUrl}/api/licensing/license-info"

        Try
            IsLoadingLicenseInfo = True
            Dim httpClient As New HttpClient()

            Dim response As HttpResponseMessage = Await httpClient.PostAsync(url, Nothing)

            If response.IsSuccessStatusCode Then
                ' Read response content as string
                Dim responseBody As String = Await response.Content.ReadAsStringAsync()
                Dim licensingResponse As LicenseResponseModel = JsonConvert.DeserializeObject(Of LicenseResponseModel)(responseBody)

                Return licensingResponse
            Else
                MessageBox.Show("Error: " & response.StatusCode.ToString() & " - " & response.ReasonPhrase)
            End If
        Catch ex As Exception
            MessageBox.Show("An error occurred: " & ex.Message)
        Finally
            IsLoadingLicenseInfo = False
        End Try
        Return Nothing
    End Function

    Private Sub RedirectToDWSIMPro()
        StatusMessage.Text = "Opening DWSIM Pro..."
        Process.Start("https://vm.simulate365.com")
    End Sub

    Private Sub SaveFlowsheet()
        Dim currentFlowsheet = fsheet.Options.VirtualFile

        FileSavingInProgress = True
        'We fire event to save file and continue on FileManagementService_FileSavedToDashboard
        FileManagementService.GetInstance().SaveFileToDashboard()

    End Sub
    Private Async Function SaveDwsimProStartupAction() As Task(Of Boolean)
        Try
            Dim currentFlowsheet As S365File = fsheet.Options.VirtualFile

            Dim loadFileAction As New DWSIMProStartupAction With {
                .Action = "load-flowsheet",
                .Data = New Dictionary(Of String, String) From {
                {"fileUniqueIdentifier", currentFlowsheet.FileUniqueIdentifier}
            }
            }

            Dim url As String = $"{DashboardSettings.DashboardServiceUrl}/api/dwsim-pro-startup-actions"
            Dim httpClient As New HttpClient()

            Dim jsonContent As String = JsonConvert.SerializeObject(loadFileAction)
            Dim content As New StringContent(jsonContent, Encoding.UTF8, "application/json")

            Dim response As HttpResponseMessage = Await httpClient.PostAsync(url, content)

            If response.IsSuccessStatusCode Then
                Return True
            Else
                Return False
            End If
        Catch ex As Exception
            Return False
        End Try

    End Function

End Class