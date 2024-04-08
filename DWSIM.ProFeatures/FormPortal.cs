using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using DWSIM.Interfaces;
using DWSIM.Simulate365.FormFactories;
using DWSIM.Simulate365.Models;
using DWSIM.Simulate365.Services;
using DWSIM.UI.Web.Settings;
using Newtonsoft.Json;
using DWSIM.ExtensionMethods;
using Microsoft.Win32;

namespace DWSIM.ProFeatures
{

    public partial class FormPortal : Form
    {
        private bool IsLoadingLicenseInfo = false;
        private int TrialLicenseCreatedMessageCount = 0;
        private bool FileSavingInProgress = false;
        private Timer Timer1 = new Timer() { Interval = 5000 };
        public IFlowsheet fsheet;

        private List<string> TrialLicenseCreatedMessages = new List<string>() { "Assigning DWSIM Pro license...", "Setting up the virtual licensing environment...", "Finalizing setup..." };

        public FormPortal(IFlowsheet fs)
        {
            InitializeComponent();
            fsheet = fs;
            FileManagementService.GetInstance().OnFileSavedToDashboard += FileManagementService_FileSavedToDashboard;
            UserService.GetInstance().OnUserLoggedIn += UserService_OnUserLoggedIn;
        }

        private void UserService_OnUserLoggedIn(object sender, EventArgs e)
        {
            var isLoggedIn = UserService.GetInstance()._IsLoggedIn();
            if (isLoggedIn)
            {
                OnInitialize();
            }
        }

        private void FormPortal_Load(object sender, EventArgs e)
        {
            this.ChangeDefaultFont();
            OnInitialize();
        }

        private async Task OnInitialize()
        {
            var isLoggedIn = UserService.GetInstance()._IsLoggedIn();
            if (!isLoggedIn)
            {
                notLoggedInPanel.Visible = true;
                return;
            }
            else
            {
                notLoggedInPanel.Visible = false;
                LoadingPanel.Visible = true;
            }
            StatusMessage.Text = "Checking for DWSIM Pro license...";
            var licenseInfo = await GetLicenseInfo();
            if (licenseInfo is not null)
            {
                if (licenseInfo.hasExistingLicense.HasValue & licenseInfo.hasExistingLicense.Value)
                {
                    SaveFlowsheet();
                }
                else if (licenseInfo.notEligibleForTrial.HasValue & licenseInfo.notEligibleForTrial.Value)
                {
                    LoadingPanel.Visible = false;
                    NoLicensePanel.Visible = true;
                }
                else if ((licenseInfo.trialLicenseCreated & licenseInfo.trialLicenseCreated.Value) == true)
                {
                    TrialLicenseCreatedLogic();
                }

            }
        }

        private void TrialLicenseCreatedLogic()
        {
            Timer1.Tick += (_, __) => Timer1_Tick();
            Timer1.Start();
        }
        private void Timer1_Tick()
        {
            if (TrialLicenseCreatedMessageCount + 1 <= TrialLicenseCreatedMessages.Count)
            {
                TrialLicenseCreatedMessageCount += 1;
                this.UIThread(() =>
                {
                    StatusMessage.Text = TrialLicenseCreatedMessages[TrialLicenseCreatedMessageCount];
                });
            }
            else
            {
                Timer1.Stop();
                SaveFlowsheet();
            }
        }

        private void FileManagementService_FileSavedToDashboard(object sender, EventArgs e)
        {
            SaveStartupActionAndRedirect();

        }
        private async void SaveStartupActionAndRedirect()
        {
            if (this.InvokeRequired)
            {
                this.Invoke(new Action(SaveStartupActionAndRedirect));
            }
            else
            {
                if (FileSavingInProgress)
                {
                    FileSavingInProgress = false;
                    bool actionSaved = await SaveDwsimProStartupAction();
                    ShowSuccessPanel();
                }
            }
        }

        private async Task<LicenseResponseModel> GetLicenseInfo()
        {

            string url = $"{DashboardSettings.DashboardServiceUrl}/api/licensing/license-info";
            var token = UserService.GetInstance().GetUserToken();
            try
            {
                IsLoadingLicenseInfo = true;
                var httpClient = new HttpClient();
                httpClient.DefaultRequestHeaders.Add("Authorization", $"Bearer {token}");

                var response = await httpClient.PostAsync(url, null);

                if (response.IsSuccessStatusCode)
                {
                    // Read response content as string
                    string responseBody = await response.Content.ReadAsStringAsync();
                    var licensingResponse = JsonConvert.DeserializeObject<LicenseResponseModel>(responseBody);

                    return licensingResponse;
                }
                else
                {
                    MessageBox.Show("Error: " + response.StatusCode.ToString() + " - " + response.ReasonPhrase);
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show("An error occurred: " + ex.Message);
            }
            finally
            {
                IsLoadingLicenseInfo = false;
            }
            return null;
        }

        private void ShowSuccessPanel()
        {
            if (this.InvokeRequired)
            {
                this.Invoke(new Action(ShowSuccessPanel));
            }
            else
            {
                LoadingPanel.Visible = false;
                SuccessPanel.Visible = true;
            }
        }

        private void SaveFlowsheet()
        {
            if (this.InvokeRequired)
            {
                this.Invoke(new Action(SaveFlowsheet));
            }
            else
            {
                StatusMessage.Text = "Save work to continue in DWSIM Pro...";
            }

            this.FileSavingInProgress = true;
            // We fire event to save file and continue on FileManagementService_FileSavedToDashboard
            FileManagementService.GetInstance().SaveFileToDashboard();

        }
        private async Task<bool> SaveDwsimProStartupAction()
        {
            try
            {
                S365File currentFlowsheet = (S365File)fsheet.FlowsheetOptions.VirtualFile;

                if (string.IsNullOrWhiteSpace(currentFlowsheet?.FileUniqueIdentifier))
                {
                    return false;
                }

                var loadFileAction = new DWSIMProStartupAction()
                {
                    Action = "load-flowsheet",
                    Data = new Dictionary<string, string>() {
                        { "fileUniqueIdentifier", currentFlowsheet.FileUniqueIdentifier },
                        {"filename",currentFlowsheet.Filename }
                    }
                };
                var postData = new CreateStartupActionPostModel { Actions = new List<DWSIMProStartupAction> { loadFileAction } };
                string jsonContent = JsonConvert.SerializeObject(postData);
                var token = UserService.GetInstance().GetUserToken();
                string url = $"{DashboardSettings.DashboardServiceUrl}/api/dwsim-pro-startup-actions";
                var httpClient = new HttpClient();
                httpClient.DefaultRequestHeaders.Add("Authorization", $"Bearer {token}");


                var content = new StringContent(jsonContent, Encoding.UTF8, "application/json");

                var response = await httpClient.PostAsync(url, content);

                if (response.IsSuccessStatusCode)
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
            catch (Exception ex)
            {
                return false;
            }

        }

        private void LinkLabel1_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            Process.Start("https://simulate365.com/shop/");
        }

        private void linkLabel2_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            var loginForm = new LoginForm();
            loginForm.ShowDialog();
        }

        private void openInDefaultBrowserLink_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            Process.Start("https://vm.simulate365.com");
        }

        private void openInIncognitoLink_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            string privateModeParam = string.Empty;
            var url = "https://vm.simulate365.com";
            var browser = GetStandardBrowser();
            if (string.IsNullOrWhiteSpace(browser))
            {
                Process.Start(url);
            }
            else
            {
                switch (browser)
                {
                    case "firefox":
                        privateModeParam = " -private-window";
                        break;
                    case "iexplorer":
                    case "opera":
                        privateModeParam = " -private";
                        break;
                    case "chrome":
                        privateModeParam = " -incognito";
                        break;
                    case "edge":
                        privateModeParam = " -inprivate";
                        break;
                }
                Process.Start(browser, $"{privateModeParam} {url}");
            }
        }

        private static string GetStandardBrowser()
        {
            string userChoice = @"Software\Microsoft\Windows\Shell\Associations\UrlAssociations\http\UserChoice";
            string progId;
            using (RegistryKey userChoiceKey = Registry.CurrentUser.OpenSubKey(userChoice))
            {
                if (userChoiceKey == null)
                {
                    return string.Empty;
                }
                object progIdValue = userChoiceKey.GetValue("Progid");
                if (progIdValue == null)
                {
                    return string.Empty;
                }
                progId = progIdValue.ToString();
                switch (progId)
                {
                    case "IE.HTTP":
                        return "iexplorer";
                    case "FirefoxURL":
                        return "firefox";

                    case "ChromeHTML":
                        return "chrome";

                    case "OperaStable":
                        return "Opera";
                    case "MSEdgeHTM": // Newer Edge version.
                        return "edge";
                    default:
                        return string.Empty;
                }
            }
        }
    }
}