using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;



namespace DWSIM.UI.Web
{
    public partial class WebUIForm : Form
    {
        public string InitialUrl { get; private set; }
        public string Title { get; private set; }

        public static string UserDataFolder = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "DWSIM/S365BrowserData");

        public WebUIForm(string initialUrl, string title = null)
        {
            InitializeComponent();

            // Title
            if (!String.IsNullOrWhiteSpace(title))
                this.Text = title;
            else
                this.Text = "Web UI";

            // Initialize WebView2
            InitializeAsync();

            // Set initial URL
            this.webView.Source = new Uri(initialUrl);

            this.webView.NavigationStarting += WebView_NavigationStarting;


        }
        public void ClearCookies()
        {
            webView.CoreWebView2.CookieManager.DeleteAllCookies();
        }

        private void WebView_NavigationStarting(object sender, Microsoft.Web.WebView2.Core.CoreWebView2NavigationStartingEventArgs e)
        {
            if (e.Uri.StartsWith("https://dwsim-login-return.simulate365.com"))
            {
                // OAuth callback
            }
        }
        public static Task StartSTATask(Action func)
        {
            var tcs = new TaskCompletionSource<object>();
            var thread = new Thread(() =>
            {
                try
                {
                    func();
                    tcs.SetResult(null);
                }
                catch (Exception e)
                {
                    tcs.SetException(e);
                }
            });
            thread.SetApartmentState(ApartmentState.STA);
            thread.Start();
            return tcs.Task;
        }
        void InitializeAsync()
        {
            var environment = Microsoft.Web.WebView2.Core.CoreWebView2Environment.CreateAsync(null, UserDataFolder, null).Result;
            webView.EnsureCoreWebView2Async(environment).Wait();
        }

        public void SubscribeToNavigationStarting(EventHandler<Microsoft.Web.WebView2.Core.CoreWebView2NavigationStartingEventArgs> callback)
        {
            this.webView.NavigationStarting += callback;
        }
    }
}