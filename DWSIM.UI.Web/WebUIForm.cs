using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;



namespace DWSIM.UI.Web
{
    public partial class WebUIForm : Form
    {
        public static string UserDataFolder = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "DWSIM", "S365BrowserData");

        public string InitialUrl { get; private set; }

        public string Title { get; private set; }


        public WebUIForm(string initialUrl, string title = null)
        {
            InitializeComponent();

            this.InitialUrl = initialUrl;
            this.webView.CoreWebView2InitializationCompleted += WebView_CoreWebView2InitializationCompleted;
            // Title
            if (!String.IsNullOrWhiteSpace(title))
                this.Text = title;
            else
                this.Text = "Web UI";
        }

        private void WebView_CoreWebView2InitializationCompleted(object sender, Microsoft.Web.WebView2.Core.CoreWebView2InitializationCompletedEventArgs e)
        {

            if (webView.CoreWebView2 != null)
            {
                webView.CoreWebView2.Settings.AreDefaultContextMenusEnabled = false;
            }
        }

        async Task InitializeAsync()
        {
            var environment = await Microsoft.Web.WebView2.Core.CoreWebView2Environment.CreateAsync(null, UserDataFolder, null);
            await webView.EnsureCoreWebView2Async(environment);
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
    }
}