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

        public S365FilePickerForm()
        {
            FilePickerService.S3365DashboardFileOpenStarted += FilePickerService_S3365DashboardFileOpenStarted;
        }

        private void FilePickerService_S3365DashboardFileOpenStarted(object sender, EventArgs e)
        {
            // Close window
            _webUIForm?.Close();
            _webUIForm?.Dispose();
        }

        public void ShowDialog()
        {
            var initialUrl = $"/";
            _webUIForm = new WebUIForm(initialUrl, "Open file from Simulate 365 Dashboard", true)
            {
                Width = 1300,
                Height = 800
            };

            _webUIForm.SubscribeToInitializationCompleted(Browser_CoreWebView2InitializationCompleted);

            _webUIForm.ShowDialog();
        }

        private void Browser_CoreWebView2InitializationCompleted(object sender, CoreWebView2InitializationCompletedEventArgs e)
        {
            try
            {
                var webView = sender as WebView2;
                if (webView.CoreWebView2 != null)
                {        
                    webView.CoreWebView2.AddHostObjectToScript("authService", new AuthService());
                    webView.CoreWebView2.AddHostObjectToScript("filePickerService", new FilePickerService());
                }
            }
            catch (Exception ex)
            {

                //  throw;
            }
        }
    }
}
