using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;



namespace DWSIM.UI.Web
{
    public partial class WebUIForm : Form
    {
        public string InitialUrl { get; private set; }
        public string Title { get; private set; }



        public WebUIForm(string initialUrl, string title = null)
        {
            InitializeComponent();
            // Title
            if (!String.IsNullOrWhiteSpace(title))
                this.Text = title;
            else
                this.Text = "Web UI";

            // Set initial URL
            this.webView.Source = new Uri(initialUrl);

            // Initialize WebView2
            InitializeAsync();
        }



        async void InitializeAsync()
        {
            await webView.EnsureCoreWebView2Async(null);
        }
    }
}