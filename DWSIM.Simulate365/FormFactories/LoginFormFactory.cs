﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;
using System.Web;
using System.Windows.Forms;
using DWSIM.Simulate365.Models;
using DWSIM.Simulate365.Services;
using DWSIM.UI.Web;
using Newtonsoft.Json;

namespace DWSIM.Simulate365.FormFactories
{
    public class LoginFormFactory
    {
        const string TENANT_ID = "eb2542b8-5a5d-4f61-a9b5-6ce7dbc4ebfd";
        const string CLIENT_ID = "d18e5f18-7709-4ef0-913e-3c8eeecd7d60";
        const string SCOPE = "profile openid offline_access";
        const string RETURN_URL = "https://dwsim-login-return.simulate365.com";

        private WebUIForm _webUIForm;

        public void ShowDialog()
        {
            var initialUrl = $"https://login.microsoftonline.com/{TENANT_ID}/oauth2/authorize?client_id={CLIENT_ID}&response_type=code&scope={HttpUtility.UrlEncode(SCOPE)}";
            _webUIForm = new WebUIForm(initialUrl, "Login with Simulate 365 account")
            {
                Width = 500,
                Height = 600
            };

            _webUIForm.SubscribeToNavigationStarting(WebView_NavigationStarting);

            _webUIForm.ShowDialog();
        }

        private void WebView_NavigationStarting(object sender, Microsoft.Web.WebView2.Core.CoreWebView2NavigationStartingEventArgs e)
        {
            if (e.Uri.StartsWith(RETURN_URL))
            {
                try
                {
                    // Split URL by #
                    var splitted = e.Uri.Split('?');
                    if (splitted.Count() != 2)
                        throw new Exception("Return URL doesn't contain ?. Can't read response values.");

                    // Parse query params
                    var queryParams = HttpUtility.ParseQueryString(splitted[1]);

                    // Check if it's error
                    if (queryParams.AllKeys.Contains("error"))
                    {
                        var errorMessage = "Login failed. Server returned error.";
                        if (queryParams.AllKeys.Contains("error_description") && !String.IsNullOrWhiteSpace(queryParams["error_description"]))
                            errorMessage = HttpUtility.UrlDecode(queryParams["error_description"]);

                        throw new Exception(errorMessage);
                    }

                    // Extract token
                    if (!queryParams.AllKeys.Contains("code") || String.IsNullOrWhiteSpace(queryParams["code"]))
                        throw new Exception("Code not found in response parameters.");

                    var code = queryParams["code"];

                    // Redeem code for access token
                    var token = Task.Run(() => RedeemCodeForAuthToken(code)).Result;

                    // Store token
                    UserService.GetInstance()
                                .SetAccessToken(token.AccessToken, token.RefreshToken, DateTime.Now.AddSeconds(token.ExpiresIn - 30));                  

                    // Close window
                    _webUIForm?.Close();
                    _webUIForm?.Dispose();
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