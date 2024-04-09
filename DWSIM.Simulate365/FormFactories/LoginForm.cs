using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;
using System.Web;
using System.Windows.Forms;
using DWSIM.Simulate365.Enums;
using DWSIM.Simulate365.Models;
using DWSIM.Simulate365.Services;
using DWSIM.UI.Web;
using Microsoft.Web.WebView2.Core;
using Microsoft.Web.WebView2.WinForms;
using Newtonsoft.Json;

namespace DWSIM.Simulate365.FormFactories
{
    public class LoginForm
    {
        const string TENANT_ID = "eb2542b8-5a5d-4f61-a9b5-6ce7dbc4ebfd";
        const string CLIENT_ID = "d18e5f18-7709-4ef0-913e-3c8eeecd7d60";
        const string SCOPE = "profile openid offline_access";
        const string RETURN_URL = "https://dwsim-login-return.simulate365.com";

        private WebUIForm _webUIForm;
        private AuthService _authService;

        public LoginForm(bool? hideIntro = false)
        {
            _authService = new AuthService();
            _authService.OnNavigateToLoginPage += (s, e) => RedirectToLoginPage();

            var initalUrl = GetLoginPageUrl();
            var useLocalUI = false;
            if (!IsProVersion() && (hideIntro == null || hideIntro == false))
            {
                initalUrl = "login/intro";
                useLocalUI = true;
            }

            _webUIForm = new WebUIForm(initalUrl, "Log in with Simulate 365 account", useLocalUI)
            {
                Width = (int)(500 * GlobalSettings.Settings.DpiScale),
                Height = (int)(600 * GlobalSettings.Settings.DpiScale)
            };

            _webUIForm.SubscribeToNavigationStarting(WebView_NavigationStarting);
            _webUIForm.SubscribeToInitializationCompleted(Browser_CoreWebView2InitializationCompleted);
        }

        public bool IsProVersion()
        {
            return System.AppDomain.CurrentDomain.GetAssemblies().Any(a => a.FullName.Contains("ProExtensions.AboutMenu"));
        }

        public void ShowDialog()
        {
            _webUIForm.Show();
        }

        public void Close()
        {
            // Close window
            _webUIForm?.Close();
            _webUIForm?.Dispose();
        }

        private void Browser_CoreWebView2InitializationCompleted(object sender, CoreWebView2InitializationCompletedEventArgs e)
        {
            try
            {
                var webView = sender as WebView2;
                if (webView.CoreWebView2 != null)
                {
                    webView.CoreWebView2.AddHostObjectToScript("authService", _authService);
                }
            }
            catch (Exception ex)
            {
                // throw;
            }
        }

        private string GetLoginPageUrl()
        {
            return $"https://login.microsoftonline.com/{TENANT_ID}/oauth2/authorize?client_id={CLIENT_ID}&response_type=code&scope={HttpUtility.UrlEncode(SCOPE)}";
        }

        public void RedirectToLoginPage()
        {
            var loginUrl = GetLoginPageUrl();
            _webUIForm?.Navigate(loginUrl);
        }



        private void WebView_NavigationStarting(object sender, Microsoft.Web.WebView2.Core.CoreWebView2NavigationStartingEventArgs e)
        {
            if (e.Uri.StartsWith(RETURN_URL))
            {
                try
                {
                    var webView = sender as WebView2;

                    // Split URL by #
                    var splitted = e.Uri.Split('?');
                    if (splitted.Count() != 2)
                        throw new Exception("Return URL doesn't contain ?. Can't read response values.");

                    // Parse query params
                    var queryParams = HttpUtility.ParseQueryString(splitted[1]);

                    // Check if it's error
                    if (queryParams.AllKeys.Contains("error"))
                    {
                        // Check if back button was clicked
                        if (queryParams.AllKeys.Contains("error_subcode") && queryParams["error_subcode"] == "cancel")
                        {
                            e.Cancel = true;

                            if (!IsProVersion())
                                _webUIForm?.Navigate(WebUIForm.LOCAL_WEB_UI_URL + "login/intro");
                            else
                                Close();

                            return;
                        }
                        else
                        {
                            var errorMessage = "Login failed. Server returned error.";
                            if (queryParams.AllKeys.Contains("error_description") && !String.IsNullOrWhiteSpace(queryParams["error_description"]))
                                errorMessage = HttpUtility.UrlDecode(queryParams["error_description"]);

                            throw new Exception(errorMessage);
                        }
                    }

                    // Extract token
                    if (!queryParams.AllKeys.Contains("code") || String.IsNullOrWhiteSpace(queryParams["code"]))
                        throw new Exception("Code not found in response parameters.");

                    var code = queryParams["code"];

                    // Redeem code for access token
                    var token = Task.Run(() => RedeemCodeForAuthToken(code)).Result;

                    // Store token
                    UserService.GetInstance()
                                .SetAccessToken(AccessTokenType.MsGraph, token.AccessToken, token.RefreshToken, DateTime.Now.AddSeconds(token.ExpiresIn - 30));

                    UserService.GetInstance().OnUserLoggedIn?.Invoke(this, new EventArgs());

                    // Close window
                    Close();
                }
                catch (Exception ex)
                {
                    var errorMessage = ex.Message;
                    if (ex is AggregateException aggEx)
                        errorMessage = String.Join(Environment.NewLine, aggEx.InnerExceptions.Select(o => o.Message));

                    MessageBox.Show(errorMessage, "Login error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
                finally
                {
                    // If callback URL is loaded, always cancel request
                    e.Cancel = true;
                }
            }
        }

        private async Task<OAuthTokenResponse> RedeemCodeForAuthToken(string code)
        {
            var tokenUrl = $"https://login.microsoftonline.com/{TENANT_ID}/oauth2/v2.0/token";

            var formData = new Dictionary<string, string>()
            {
                ["client_id"] = CLIENT_ID,
                ["code"] = code,
                ["scope"] = SCOPE,
                ["grant_type"] = "authorization_code",
            };

            using (HttpClient client = new HttpClient())
            {
                client.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));

                HttpResponseMessage response = await client.PostAsync(tokenUrl, new FormUrlEncodedContent(formData));
                var responseStr = await response.Content.ReadAsStringAsync();

                // Check for error
                var errorResponse = JsonConvert.DeserializeObject<OAuthErrorResponse>(responseStr);
                if (errorResponse != null && !String.IsNullOrWhiteSpace(errorResponse.Error))
                {
                    var errorMessage = "An error occured while redeeming code for authorizaton token.";
                    if (!String.IsNullOrWhiteSpace(errorResponse.ErrorDescription))
                        errorMessage = errorResponse.ErrorDescription;

                    throw new Exception(errorMessage);
                }

                // Deserialize
                var token = JsonConvert.DeserializeObject<OAuthTokenResponse>(responseStr);

                return token;
            }
        }
    }
}
