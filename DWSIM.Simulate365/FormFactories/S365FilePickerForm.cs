using DWSIM.Simulate365.Models;
using DWSIM.Simulate365.Services;
using DWSIM.UI.Web;
using Microsoft.Web.WebView2.Core;
using Microsoft.Web.WebView2.WinForms;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.Simulate365.FormFactories
{
    public class S365FilePickerForm
    {
        private WebUIForm _webUIForm;
        private readonly FilePickerService _filePickerService;

        public S365FilePickerForm()
        {
            _filePickerService = new FilePickerService();
            _filePickerService.S3365DashboardFileOpenStarted += FilePickerService_S3365DashboardFileOpenStarted;
            _filePickerService.S365DashboardSaveFileClicked += FilePickerService_S365DashboardSaveFileClicked;
            _filePickerService.S365DashboardFolderCreated += _filePickerService_S365DashboardFolderCreated;
        }

        private void _filePickerService_S365DashboardFolderCreated(object sender, EventArgs e)
        {
            _webUIForm.RealoadPage();
        }

        private void FilePickerService_S365DashboardSaveFileClicked(object sender, S365DashboardSaveFile e)
        {
            // Close window
            _webUIForm?.Close();
            _webUIForm?.Dispose();
        }

        private void FilePickerService_S3365DashboardFileOpenStarted(object sender, EventArgs e)
        {
            // Close window
            _webUIForm?.Close();
            _webUIForm?.Dispose();
        }

        public S365DashboardSaveFile ShowSaveDialog(List<string> fileFormats = null)
        {

            var navigationPath = "filepicker/save";
            if (fileFormats != null && fileFormats.Count > 0)
            {
                navigationPath += $"/{string.Join("_", fileFormats)}";
            }
            var initialUrl = $"{navigationPath}";
            string title = "Save file to Simulate 365 Dashboard";
            _webUIForm = new WebUIForm(initialUrl, title, true)
            {
                Width = 1300,
                Height = 800
            };

            _webUIForm.SubscribeToInitializationCompleted(Browser_CoreWebView2InitializationCompleted);

            _webUIForm.ShowDialog();

            return _filePickerService.SelectedSaveFile;
        }


        public S365File ShowOpenDialog(List<string> fileFormats = null)
        {
            var navigationPath = "filepicker/open";
            if (fileFormats != null && fileFormats.Count > 0)
            {
                navigationPath += $"/{string.Join("_", fileFormats)}";
            }
            var initialUrl = $"{navigationPath}";
            string title = "Open file from Simulate 365 Dashboard";
            _webUIForm = new WebUIForm(initialUrl, title, true)
            {
                Width = 1300,
                Height = 800
            };

            _webUIForm.SubscribeToInitializationCompleted(Browser_CoreWebView2InitializationCompleted);

            _webUIForm.ShowDialog();

            return _filePickerService.SelectedOpenFile;
        }


        private void Browser_CoreWebView2InitializationCompleted(object sender, CoreWebView2InitializationCompletedEventArgs e)
        {
            try
            {
                var webView = sender as WebView2;
                if (webView.CoreWebView2 != null)
                {
                    webView.CoreWebView2.AddHostObjectToScript("authService", new AuthService());
                    webView.CoreWebView2.AddHostObjectToScript("filePickerService", _filePickerService);
                }
            }
            catch (Exception ex)
            {

                //  throw;
            }
        }
    }
}
