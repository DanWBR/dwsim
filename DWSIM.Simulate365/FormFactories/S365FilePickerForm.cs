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

        public void ShowDialog()
        {
            var initialUrl = $"/";
            _webUIForm = new WebUIForm(initialUrl, "Open file from Simulate 365 Dashboard", true)
            {
                Width = 1200,
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
                    webView.CoreWebView2.AddHostObjectToScript("usersService", UserService.GetInstance());   
                }
            }
            catch (Exception ex)
            {

                //  throw;
            }
        }
    }
}
