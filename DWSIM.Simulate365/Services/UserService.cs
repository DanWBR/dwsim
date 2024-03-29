using DWSIM.Simulate365.Enums;
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

        private static UserService _singletonInstance;

        private UserDetailsModel _currentUser = null;
        public EventHandler OnUserLoggedIn;
        public EventHandler<bool> AutoLoginInProgress;
        private string _accessToken = null;
        private string _refreshToken = null;
        private AccessTokenType _accessTokenType = AccessTokenType.MsGraph;
        private DateTime _accessTokenExpiresAt = DateTime.MinValue;
        private System.Timers.Timer refreshTokenTimer;
        private static string AccessTokenTypeField = "AccessTokenType";
        private static string AccessTokenField = "AccessTokenV2";
        private static string RefreshTokenField = "RefreshTokenV2";
        private static string AccessTokenExpiresAtField = "AccessTokenExpiresAtV2";

        private Dictionary<AccessTokenType, IUserProvider> _userProviders = new Dictionary<AccessTokenType, IUserProvider> {
            {AccessTokenType.MsGraph,new MsGraphUserProvider() }
        };

        #region Public events

        public event EventHandler<UserDetailsModel> UserDetailsLoaded;
        public event EventHandler<bool> AutoLoginInProgressChanged;
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
                this._accessTokenType = (AccessTokenType)Enum.Parse(typeof(AccessTokenType), key.GetValue(AccessTokenTypeField)?.ToString() ?? "0");

                var expiresAtValue = key.GetValue(AccessTokenExpiresAtField);
                if (expiresAtValue != null)
                    DateTime.TryParse(key.GetValue(AccessTokenExpiresAtField).ToString(), out this._accessTokenExpiresAt);

                AutoLoginInProgress += (s, e) => { AutoLoginInProgressChanged.Invoke(s, e); };

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
        public void SetAccessToken(AccessTokenType accessTokenType, string accessToken, string refreshToken, DateTime expiresAt)
        {
            _accessToken = accessToken;
            _refreshToken = refreshToken;
            _accessTokenExpiresAt = expiresAt;
            _accessTokenType = accessTokenType;

            // Save to registry

            RegistryKey key = Registry.CurrentUser.OpenSubKey(@"SOFTWARE\DWSIM", true);
            if (key == null)
                key = Registry.CurrentUser.CreateSubKey(@"SOFTWARE\DWSIM", true);

            //storing the values  
            key.SetValue(AccessTokenTypeField, accessTokenType);
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
                if (!_userProviders.ContainsKey(_accessTokenType))
                    throw new Exception($"User provider not registered for type {_accessTokenType}.");

                var userProvider = _userProviders[_accessTokenType];
                _currentUser = await userProvider.GetUserDetailsAsync(_accessToken);

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

        public void RegisterUserDetailsProvider(AccessTokenType accessTokenType, IUserProvider userDetailsProvider)
        {
            if (!_userProviders.ContainsKey(accessTokenType))
            {
                _userProviders.Add(accessTokenType, userDetailsProvider);
            }
        }


        public void ShowLogin()
        {
            ShowLoginForm?.Invoke(this, new EventArgs());
        }

        public async Task<bool> RefreshToken()
        {
            try
            {
                if (!_userProviders.ContainsKey(_accessTokenType))
                    throw new Exception($"User provider not registered for type {_accessTokenType}.");

                // Deserialize
                var userProvider = _userProviders[_accessTokenType];

                var tokenResp = await userProvider.RefreshTokenAsync(_refreshToken);
                if (tokenResp == null)
                    return false;

                SetAccessToken(tokenResp.AccessTokenType, tokenResp.AccessToken, tokenResp.RefreshToken, DateTime.Now.AddSeconds(tokenResp.ExpiresIn - 30));

                return true;
            }
            catch (Exception ex)
            {

               return false;
            }
            
        }

    }
}
