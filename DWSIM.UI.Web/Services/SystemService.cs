#define S365_STAGING
using DWSIM.UI.Web.Models;
using DWSIM.UI.Web.Settings;
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.UI.Web.Services
{
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ComVisible(true)]
    public class SystemService
    {
        public void OpenUrlInLocalBrowser(string url)
        {
            Process.Start(url);
        }

        public string GetUrlSettings()
        {

            return JsonConvert.SerializeObject(new UrlSettingsResponseModel
            {
                DashboardServiceUrl = DashboardSettings.DashboardServiceUrl,
                ExcelRunnerServiceUrl = DashboardSettings.ExcelRunnerServiceUrl,
                SensitivityStudiesServiceUrl = DashboardSettings.SensitivityStudiesServiceUrl,
                TakeHomeExamsServiceUrl = DashboardSettings.TakeHomeExamsServiceUrl,
                Environment=DashboardSettings.Environment
            }, new JsonSerializerSettings
            {
                ContractResolver = new CamelCasePropertyNamesContractResolver()
            });





        }
    }
}
