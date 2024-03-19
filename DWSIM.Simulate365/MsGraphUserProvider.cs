using Azure.Core;
using DWSIM.Simulate365.Enums;
using DWSIM.Simulate365.Models;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.IdentityModel;
using System.Linq;
using System.Net.Http.Headers;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.Simulate365
{
    public class MsGraphUserProvider : IUserProvider
    {

        const string TENANT_ID = "eb2542b8-5a5d-4f61-a9b5-6ce7dbc4ebfd";
        const string CLIENT_ID = "d18e5f18-7709-4ef0-913e-3c8eeecd7d60";
        const string SCOPE = "profile openid offline_access";
        const string RETURN_URL = "https://dwsim-login-return.simulate365.com";

        public async Task<UserDetailsModel> GetUserDetailsAsync(string accessToken)
        {
            var graphClient = GraphClientFactory.CreateClient(accessToken);
            var user = await graphClient.Me.Request().GetAsync();

            var currentUser = new UserDetailsModel
            {
                DisplayName = $"{user.GivenName} {user.Surname}",
                FirstName = user.GivenName,
                LastName = user.Surname,
                Id = user.Id,
                UserPrincipalName = user.UserPrincipalName
            };
            return currentUser;
        }

        public async Task<OAuthTokenResponse> RefreshTokenAsync(string refreshToken)
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
                        ["refresh_token"] = refreshToken,
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
                    token.AccessTokenType = AccessTokenType.MsGraph;
                    return token;

                }
            }
            catch (Exception ex)
            {

                //  UserLoggedOut?.Invoke(this, new EventArgs());
                return null;
            }
        }
    }
}
