using DWSIM.Simulate365.Models;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
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

        #endregion

        private UserService()
        {
            // Read from registry
        }

        public static UserService GetInstance()
        {
            if (_singletonInstance == null)
                _singletonInstance = new UserService();

            return _singletonInstance;
        }

        public void SetAccessToken(string accessToken, string refreshToken, DateTime expiresAt)
        {
            _accessToken = accessToken;
            _refreshToken = refreshToken;
            _accessTokenExpiresAt = expiresAt;

            // Save to registry

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
