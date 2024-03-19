using DWSIM.Simulate365.Models;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.Simulate365
{
    public interface IUserProvider
    {
        Task<UserDetailsModel> GetUserDetailsAsync(string accessToken);
        Task<OAuthTokenResponse> RefreshTokenAsync(string refreshToken); 
    }
}
