using DWSIM.Simulate365.Models;
using DWSIM.UI.Web.Settings;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.Simulate365.Services
{
    public class FileDownloadService
    {
        public static Stream GetFileBySimulatePath(string simulatePath)
        {
            if (simulatePath.StartsWith("//Simulate 365 Dashboard/"))
                simulatePath = simulatePath.Substring(24);
            var token = UserService.GetInstance().GetUserToken();
            var client = GetDashboardClient(token);
            // Get drive item
            var stream = Task.Run(async () => await client.GetStreamAsync($"/api/files/download-by-path?filePath={simulatePath}")).Result;
            return stream;
        }

        private static HttpClient GetDashboardClient(string token)
        {
            var client = new HttpClient();
            client.BaseAddress = new Uri(DashboardSettings.DashboardServiceUrl);
            client.DefaultRequestHeaders.Add("Authorization", $"Bearer {token}");

            return client;

        }
    }
}
