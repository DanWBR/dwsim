using Microsoft.Graph;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.Simulate365
{
    public class GraphClientFactory
    {
        public static GraphServiceClient CreateClient(string accessToken)
        {
            return new GraphServiceClient(new DelegateAuthenticationProvider((requestMessage) =>
            {
                requestMessage
                    .Headers
                    .Authorization = new AuthenticationHeaderValue("Bearer", accessToken);

                return Task.FromResult(0);
            }));
        }
    }
}
