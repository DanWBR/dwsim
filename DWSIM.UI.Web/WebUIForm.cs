using DWSIM.UI.Web.Services;
using Microsoft.Web.WebView2.Core;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Windows.Forms;



namespace DWSIM.UI.Web
{
    public partial class WebUIForm : Form
    {
        public static string USER_DATA_FOLDER = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "DWSIM", "S365BrowserData");

        public static string LOCAL_WEB_UI_DOMAIN = "https://dwsim.webui";
        public static string LOCAL_WEB_UI_URL = $"{LOCAL_WEB_UI_DOMAIN}/index.html#";

        public string InitialUrl { get; private set; }

        public string Title { get; private set; }

        public bool UseLocalUI { get; private set; }


        public WebUIForm(string initialUrl, string title = null, bool userLocalUI = false)
        {
            // If userLocalUI == false, then real URL must be provided
            if (!userLocalUI && (String.IsNullOrWhiteSpace(initialUrl) || !Regex.IsMatch(initialUrl, "https*://")))
                throw new Exception("When not using local UI, real URL must be provided.");

            this.UseLocalUI = userLocalUI;

            // If using local UI, prepand virtual domain
            if (userLocalUI)
                this.InitialUrl = $"{LOCAL_WEB_UI_URL}/{initialUrl}";
            else
                this.InitialUrl = initialUrl;

            // After all variables are set, then initialize form components
            InitializeComponent();

            // Title
            // Must be called after initialize components
            if (!String.IsNullOrWhiteSpace(title))
                this.Text = title;
            else
                this.Text = "Web UI";

            this.webView.CoreWebView2InitializationCompleted += WebView_CoreWebView2InitializationCompleted;
        }

        public void RealoadPage()
        {
            this.webView.CoreWebView2.Reload();
        }
        public void Navigate(string url)
        {
            if (this.webView != null && this.webView.CoreWebView2 != null)
                this.webView.CoreWebView2.Navigate(url);
        }


        private void WebView_CoreWebView2InitializationCompleted(object sender, Microsoft.Web.WebView2.Core.CoreWebView2InitializationCompletedEventArgs e)
        {
            if (webView.CoreWebView2 != null)
            {
                // Disable right click menu
                webView.CoreWebView2.Settings.AreDefaultContextMenusEnabled = false;

                // Add system service
                webView.CoreWebView2.AddHostObjectToScript("systemService", new SystemService());
            }
        }

        async Task InitializeAsync()
        {
            var environment = await Microsoft.Web.WebView2.Core.CoreWebView2Environment.CreateAsync(null, USER_DATA_FOLDER, null);
            await webView.EnsureCoreWebView2Async(environment);

            if (UseLocalUI)
            {
                var assemblyDir = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location);
                var webUiDir = Path.Combine(assemblyDir, "dwsim-web-ui");
                if (!Directory.Exists(webUiDir))
                    throw new Exception($"Directory {webUiDir} doesn't exist.");

                this.webView.CoreWebView2.SetVirtualHostNameToFolderMapping("dwsim.webui", webUiDir, CoreWebView2HostResourceAccessKind.Allow);
            }
        }

        public void SubscribeToNavigationStarting(EventHandler<Microsoft.Web.WebView2.Core.CoreWebView2NavigationStartingEventArgs> callback)
        {
            this.webView.NavigationStarting += callback;
        }
        public void SubscribeToInitializationCompleted(EventHandler<Microsoft.Web.WebView2.Core.CoreWebView2InitializationCompletedEventArgs> callback)
        {
            this.webView.CoreWebView2InitializationCompleted += callback;
        }

        private async void WebUIForm_Load(object sender, EventArgs e)
        {
            // Initialize WebView2
            await InitializeAsync();

            // Set initial URL
            this.webView.Source = new Uri(InitialUrl);
        }

        protected override void OnClosing(CancelEventArgs e)
        {
            base.OnClosing(e);
            if (this.webView != null)
            {
                this.webView.Stop();
                this.webView.Dispose();
            }
        }
    }
}