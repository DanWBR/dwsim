using DWSIM.Simulate365.Models;
using DWSIM.UI.Web;
using Microsoft.Win32;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace DWSIM.Simulate365.Services
{
    public class UserService
    {
        private static UserService _singletonInstance;

        private UserDetailsModel _currentUser = null;

        private string _accessToken = null;
        private string _refreshToken = null;
        private DateTime _accessTokenExpiresAt = DateTime.MinValue;

        #region Public events

        public event EventHandler<UserDetailsModel> UserDetailsLoaded;
        public event EventHandler UserLoggedOut;

        #endregion

        private UserService()
        {
            // Read from registry
            RegistryKey key = Registry.CurrentUser.CreateSubKey(@"SOFTWARE\DWSIM");
            if (key != null)
            {
                this._accessToken = key.GetValue("AccessToken")?.ToString();
                this._refreshToken = key.GetValue("RefreshToken")?.ToString();
                var expiresAtValue = key.GetValue("AccessTokenExpiresAt");
                if (expiresAtValue != null)
                    DateTime.TryParse(key.GetValue("AccessTokenExpiresAt").ToString(), out this._accessTokenExpiresAt);

                Task.Run(() => LoadUserDetails());
            }

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
        }

        private  void ClearInstance()
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
                    key.DeleteValue("AccessToken");
                    key.DeleteValue("RefreshToken");
                    key.DeleteValue("AccessTokenExpiresAt");
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
         
                UserLoggedOut?.Invoke(this,null);
            }
         

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
            key.SetValue("AccessToken", _accessToken);
            key.SetValue("RefreshToken", _refreshToken);
            key.SetValue("AccessTokenExpiresAt", _accessTokenExpiresAt.ToString());
            key.Close();

            Task.Run(() => LoadUserDetails());
        }

        private async Task LoadUserDetails()
        {
            try
            {
                var graphClient = GraphClientFactory.CreateClient(_accessToken);
                var user = await graphClient.Me.Request().GetAsync();

                _currentUser = new UserDetailsModel
                {
                    DisplayName = user.DisplayName,
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
    }
}
