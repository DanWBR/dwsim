using DWSIM.Simulate365.Models;
using DWSIM.UI.Web;
using Microsoft.Win32;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace DWSIM.Simulate365.Services
{

    public class UserService
    {

        const string TENANT_ID = "eb2542b8-5a5d-4f61-a9b5-6ce7dbc4ebfd";
        const string CLIENT_ID = "d18e5f18-7709-4ef0-913e-3c8eeecd7d60";
        const string SCOPE = "profile openid offline_access";
        const string RETURN_URL = "https://dwsim-login-return.simulate365.com";

        private static UserService _singletonInstance;

        private UserDetailsModel _currentUser = null;
        public EventHandler OnUserLoggedIn;
        private string _accessToken = null;
        private string _refreshToken = null;
        private DateTime _accessTokenExpiresAt = DateTime.MinValue;
        private System.Timers.Timer refreshTokenTimer;
        private static string AccessTokenField = "AccessTokenV2";
        private static string RefreshTokenField = "RefreshTokenV2";
        private static string AccessTokenExpiresAtField = "AccessTokenExpiresAtV2";

        #region Public events

        public event EventHandler<UserDetailsModel> UserDetailsLoaded;
        public event EventHandler UserLoggedOut;
        public event EventHandler ShowLoginForm;

        #endregion

        public UserDetailsModel CurrentUser { get { return _currentUser; } }
        private UserService()
        {
            // Read from registry
            RegistryKey key = Registry.CurrentUser.CreateSubKey(@"SOFTWARE\DWSIM");
            if (key != null)
            {
                this._accessToken = key.GetValue(AccessTokenField)?.ToString();
                this._refreshToken = key.GetValue(RefreshTokenField)?.ToString();
                var expiresAtValue = key.GetValue(AccessTokenExpiresAtField);
                if (expiresAtValue != null)
                    DateTime.TryParse(key.GetValue(AccessTokenExpiresAtField).ToString(), out this._accessTokenExpiresAt);

                Task.Run(() => LoadUserDetails());
            }
            //Intended use to refresh token on page startup
#pragma warning disable
            if (!string.IsNullOrWhiteSpace(this._refreshToken))
            {
                RefreshToken();
            }


            refreshTokenTimer = new System.Timers.Timer();
            refreshTokenTimer.Elapsed += async (sender, args) =>
            {
                if (this._accessTokenExpiresAt != DateTime.MinValue && this._accessTokenExpiresAt.AddMinutes(-5) < DateTime.Now)
                {
                    await GetInstance().RefreshToken();
                }
            };
            refreshTokenTimer.AutoReset = true;
            refreshTokenTimer.Interval = TimeSpan.FromMinutes(1).TotalMilliseconds;
            refreshTokenTimer.Start();
        }

        public bool _IsLoggedIn()
        {
            return this._accessToken != null && this._accessTokenExpiresAt > DateTime.Now;
        }

        public static UserService GetInstance()
        {
            if (_singletonInstance == null)
                _singletonInstance = new UserService();

            return _singletonInstance;
        }

        public static void Logout()
        {
            _singletonInstance.ClearInstance();
            _singletonInstance.UserLoggedOut?.Invoke(_singletonInstance, new EventArgs());
        }

        private void ClearInstance()
        {
            _accessToken = null;
            _refreshToken = null;
            _currentUser = null;
            _accessTokenExpiresAt = DateTime.MinValue;

            RegistryKey key = Registry.CurrentUser.OpenSubKey(@"SOFTWARE\DWSIM", true);
            if (key != null)
            {
                try
                {
                    key.DeleteValue(AccessTokenField);
                    key.DeleteValue(RefreshTokenField);
                    key.DeleteValue(AccessTokenExpiresAtField);
                }
                catch (Exception)
                {

                }

                try
                {
                    Directory.Delete(WebUIForm.USER_DATA_FOLDER, true);
                }
                catch (Exception ex)
                {
                }

                UserLoggedOut?.Invoke(this, null);
            }


        }

        public string GetUserToken()
        {
            return this._accessToken;
        }
        public void SetAccessToken(string accessToken, string refreshToken, DateTime expiresAt)
        {
            _accessToken = accessToken;
            _refreshToken = refreshToken;
            _accessTokenExpiresAt = expiresAt;

            // Save to registry

            RegistryKey key = Registry.CurrentUser.OpenSubKey(@"SOFTWARE\DWSIM", true);
            if (key == null)
                key = Registry.CurrentUser.CreateSubKey(@"SOFTWARE\DWSIM", true);

            //storing the values  
            key.SetValue(AccessTokenField, _accessToken);
            key.SetValue(RefreshTokenField, _refreshToken);
            key.SetValue(AccessTokenExpiresAtField, _accessTokenExpiresAt.ToString());
            key.Close();

            Task.Run(() => LoadUserDetails());
        }

        private async Task LoadUserDetails()
        {
            try
            {
                if (string.IsNullOrWhiteSpace(this._accessToken))
                    return;

                var graphClient = GraphClientFactory.CreateClient(_accessToken);
                var user = await graphClient.Me.Request().GetAsync();

                _currentUser = new UserDetailsModel
                {
                    DisplayName = $"{user.GivenName} {user.Surname}",
                    FirstName = user.GivenName,
                    LastName = user.Surname,
                    Id = user.Id,
                    UserPrincipalName = user.UserPrincipalName
                };

                // Trigger event, swallow all errors
                try
                {
                    UserDetailsLoaded?.Invoke(this, _currentUser);
                }
                catch { }
            }
            catch (Exception e)
            {

            }
        }

        public void ShowLogin()
        {
            ShowLoginForm?.Invoke(this, new EventArgs());
        }

        public async Task RefreshToken()
        {
            try
            {
                using (HttpClient client = new HttpClient())
                {
                    client.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));

                    string refreshUrl = $"https://login.microsoftonline.com/{TENANT_ID}/oauth2/v2.0/token";
                    var formData = new Dictionary<string, string>()
                    {
                        ["client_id"] = CLIENT_ID,
                        ["refresh_token"] = _refreshToken,
                        ["scope"] = SCOPE,
                        ["redirect_uri"] = RETURN_URL,
                        ["grant_type"] = "refresh_token",
                    };

                    HttpResponseMessage response = await client.PostAsync(refreshUrl, new FormUrlEncodedContent(formData));
                    var responseStr = await response.Content.ReadAsStringAsync();

                    // Check for error
                    var errorResponse = JsonConvert.DeserializeObject<OAuthErrorResponse>(responseStr);
                    if (errorResponse != null && !String.IsNullOrWhiteSpace(errorResponse.Error))
                    {
                        var errorMessage = "An error occured while refreshing authorizaton token.";
                        if (!String.IsNullOrWhiteSpace(errorResponse.ErrorDescription))
                            errorMessage = errorResponse.ErrorDescription;

                        throw new Exception(errorMessage);
                    }

                    // Deserialize
                    var token = JsonConvert.DeserializeObject<OAuthTokenResponse>(responseStr);
                    SetAccessToken(token.AccessToken, token.RefreshToken, DateTime.Now.AddSeconds(token.ExpiresIn - 30));

                }
            }
            catch (Exception ex)
            {

                //  UserLoggedOut?.Invoke(this, new EventArgs());
            }
        }

    }
}
