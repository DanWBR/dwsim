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
using System.Text.RegularExpressions;

namespace DWSIM.ProFeatures
{

    public partial class FormPortal : UserControl
    {
        private bool IsLoadingLicenseInfo = false;
        public IAnalyticsProvider AnalyticsProvider;
        private int TrialLicenseCreatedMessageCount = 0;
        private bool FileSavingInProgress = false;
        private Timer Timer1 = new Timer() { Interval = 5000 };
        public IFlowsheet fsheet;

        private List<string> TrialLicenseCreatedMessages = new List<string>() { "Assigning DWSIM Pro license...", "Setting up the virtual licensing environment...", "Finalizing setup..." };

        public FormPortal()
        {
            InitializeComponent();

            FileManagementService.GetInstance().OnFileSavedToDashboard += FileManagementService_FileSavedToDashboard;
            UserService.GetInstance().OnUserLoggedIn += UserService_OnUserLoggedIn;
        }
        public void SetFlowsheet(IFlowsheet fs)
        {
            fsheet = fs;
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

        }

        public async Task OnInitialize()
        {
            // Debugger.Launch();
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
                    AnalyticsProvider?.RegisterEvent("Portal Window 2: License Eligible For Trial", "", null);
                    SaveFlowsheet();
                }
                else if (licenseInfo.notEligibleForTrial.HasValue & licenseInfo.notEligibleForTrial.Value)
                {
                    AnalyticsProvider?.RegisterEvent("Portal Window 2: License Not Eligible For Trial", "", null);
                    LoadingPanel.Visible = false;
                    NoLicensePanel.Visible = true;
                }
                else if ((licenseInfo.trialLicenseCreated & licenseInfo.trialLicenseCreated.Value) == true)
                {
                    AnalyticsProvider?.RegisterEvent("Portal Window 2: Trial License Created Successfully", "", null);
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
            if (TrialLicenseCreatedMessageCount + 1 < TrialLicenseCreatedMessages.Count)
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
            saveToDashboardBtn.Visible = false;
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
                    AnalyticsProvider?.RegisterEvent("Portal Window 2: License Check Failed", response.ReasonPhrase, null);
                    MessageBox.Show("Error: " + response.StatusCode.ToString() + " - " + response.ReasonPhrase);
                }
            }
            catch (Exception ex)
            {
                AnalyticsProvider?.RegisterEvent("Portal Window 2: License Check Failed", ex.Message, null);
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
            AnalyticsProvider?.RegisterEvent("Portal Window 2: Open-Source to Pro Workflow Finished Sucessfully", "", null);
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
                if (fsheet != null)
                {
                    StatusMessage.Text = "To continue to DWSIM Pro, you must save your file to your Simulate 365 Dashboard.";
                    this.saveToDashboardBtn.Visible = true;
                }
                else
                {
                    ShowSuccessPanel();
                }
            }




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
            AnalyticsProvider?.RegisterEvent("Portal Window 2: User Clicked 'Go to Shop'", "", null);
            Process.Start("https://simulate365.com/shop/");
        }

        private void linkLabel2_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            AnalyticsProvider?.RegisterEvent("Portal Window 2: User Clicked 'Login'", "", null);
            var loginForm = new LoginForm(true);
            loginForm.ShowDialog();
        }

        private void openInDefaultBrowserLink_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            AnalyticsProvider?.RegisterEvent("Portal Window 2: User Clicked 'Open Browser (Normal)'", "", null);
            Process.Start("https://vm.simulate365.com");
        }

        private void openInIncognitoLink_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {

        }

        private string GetDefaultBrowserLocation()
        {
            var progId = GetStandardBrowserProgId();
            using var command = Registry.ClassesRoot.OpenSubKey($"{progId}\\shell\\open\\command");
            var runCommand = command?.GetValue(null) as string;
            if (!string.IsNullOrWhiteSpace(runCommand))
            {
                var splitCommand = Regex.Split(runCommand, ".exe");
                var browserLocation = splitCommand[0].Replace("\"", "");
                return browserLocation + ".exe";

            }
            return string.Empty;
        }


        private static string GetStandardBrowserProgId()
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
                return progId;
            }
        }

        private void button1_Click(object sender, EventArgs e)
        {
            AnalyticsProvider?.RegisterEvent("Portal Window 2: User Clicked 'Go to Shop'", "", null);
            Process.Start("https://simulate365.com/shop/");
        }

        private void button3_Click(object sender, EventArgs e)
        {
            AnalyticsProvider?.RegisterEvent("Portal Window 2: User Clicked 'Open Browser in Private Mode'", "", null);
            string privateModeParam = string.Empty;
            var url = "https://vm.simulate365.com";
            var browserProgId = GetStandardBrowserProgId()?.ToLower();
            if (string.IsNullOrWhiteSpace(browserProgId))
            {
                Process.Start(url);
            }
            else
            {

                if (browserProgId.Contains("firefox"))
                {
                    privateModeParam = " --private-window";
                }
                else if (browserProgId.Contains("ie.http"))
                {
                    privateModeParam = " -private";
                }
                else if (browserProgId.Contains("chrome") || browserProgId.Contains("opera"))
                {
                    privateModeParam = " /incognito";
                }
                else if (browserProgId.Contains("edge"))
                {
                    privateModeParam = " -inprivate";
                }

                var browserLocation = GetDefaultBrowserLocation();


                Process.Start(browserLocation, $"{privateModeParam} {url}");
            }
        }

        private void button2_Click(object sender, EventArgs e)
        {
            AnalyticsProvider?.RegisterEvent("Portal Window 2: User Clicked 'Open Browser (Normal)'", "", null);
            Process.Start("https://vm.simulate365.com");
        }

        private void saveToDashboardBtn_Click(object sender, EventArgs e)
        {
            AnalyticsProvider?.RegisterEvent("Portal Window 2: User Clicked 'Save to Dashboard'", "", null);
            this.FileSavingInProgress = true;
            // We fire event to save file and continue on FileManagementService_FileSavedToDashboard
            FileManagementService.GetInstance().SaveFileToDashboard();
        }
    }
}