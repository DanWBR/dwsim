using DWSIM.Interfaces;
using DWSIM.Simulate365.Models;
using DWSIM.Simulate365.Services;
using DWSIM.UI.Web;
using Microsoft.Web.WebView2.Core;
using Microsoft.Web.WebView2.WinForms;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Web;

namespace DWSIM.Simulate365.FormFactories
{
    public class S365FilePickerForm : IFilePicker
    {
        private WebUIForm _webUIForm;
        private readonly FilePickerService _filePickerService;

        public string SuggestedDirectory { get; set; }
        public string SuggestedFilename { get; set; }

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



        public S365File ShowSaveDialog(List<string> fileFormats = null)
        {

            var navigationPath = "filepicker/save";
            var queryParams = new Dictionary<string, string>();
            if (fileFormats != null && fileFormats.Count > 0)
            {
                queryParams.Add("extensions", string.Join("_", fileFormats));
            }
            if (!string.IsNullOrWhiteSpace(SuggestedDirectory))
            {
                queryParams.Add("directory", HttpUtility.UrlEncode(SuggestedDirectory));
            }

            if (!string.IsNullOrWhiteSpace(SuggestedFilename))
            {
                queryParams.Add("filename", HttpUtility.UrlEncode(SuggestedFilename));
            }

            var initialUrl = $"{navigationPath}";
            if (queryParams.Any())
            {
                initialUrl = initialUrl + string.Join("", queryParams.Select(x =>
                {
                    var param = $"{x.Key}={x.Value}";
                    return queryParams.First().Key == x.Key ? $"?{param}" : $"&{param}";

                }).ToList());
            }

            string title = "Save file to Simulate 365 Dashboard";
            _webUIForm = new WebUIForm(initialUrl, title, true)
            {
                Width = 1300,
                Height = 800
            };

            _webUIForm.SubscribeToInitializationCompleted(Browser_CoreWebView2InitializationCompleted);

            _webUIForm.ShowDialog();

            return _filePickerService.SelectedSaveFile != null ?
                new S365File(null)
                {
                    DriveId = _filePickerService.SelectedSaveFile.FlowsheetsDriveId,
                    FileId = null,
                    Filename = _filePickerService.SelectedSaveFile.Filename,
                    ParentDriveId = _filePickerService.SelectedSaveFile.ParentDriveId,
                    FullPath = _filePickerService.SelectedSaveFile.SimulatePath
                } : null;
        }


        public S365File ShowOpenDialog(List<string> fileFormats = null)
        {
            var navigationPath = "filepicker/open";
            var queryParams = new Dictionary<string, string>();
            if (fileFormats != null && fileFormats.Count > 0)
            {
                queryParams.Add("extensions", string.Join("_", fileFormats));
            }
            if (!string.IsNullOrWhiteSpace(SuggestedDirectory))
            {
                queryParams.Add("directory", HttpUtility.UrlEncode(SuggestedDirectory));
            }

            var initialUrl = $"{navigationPath}";
            if (queryParams.Any())
            {
                initialUrl = initialUrl + string.Join("", queryParams.Select(x =>
                {
                    var param = $"{x.Key}={x.Value}";
                    return queryParams.First().Key == x.Key ? $"?{param}" : $"&{param}";

                }).ToList());
            }
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

        #region FilePickerService

        public IVirtualFile ShowOpenDialog(IEnumerable<IFilePickerAllowedType> allowedTypes)
        {
            List<string> fileFormats = null;
            if (allowedTypes != null && allowedTypes.Count() > 0)
            {
                fileFormats = allowedTypes.SelectMany(t => t.AllowedExtensions.Select(e => ReplateLeadingStarDot(e))).Distinct().ToList();
            }

            var file = ShowOpenDialog(fileFormats);
            return file;
        }

        public IVirtualFile ShowSaveDialog(IEnumerable<IFilePickerAllowedType> allowedTypes)
        {
            List<string> fileFormats = null;
            if (allowedTypes != null && allowedTypes.Count() > 0)
            {
                fileFormats = allowedTypes.SelectMany(t => t.AllowedExtensions.Select(e => ReplateLeadingStarDot(e))).Distinct().ToList();
            }

            var file = ShowSaveDialog(fileFormats);
            return file;
        }

        private string ReplateLeadingStarDot(string input)
        {
            return Regex.Replace(input, @"^\*{0,1}\.", "");
        }

        #endregion
    }
}
