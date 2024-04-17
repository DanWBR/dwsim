using System;
using System.Collections.Generic;
using System.Configuration;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.UI.Web.Settings
{
    public static class DashboardSettings
    {
        public static string DashboardServiceUrl = "https://dashboard-service.simulate365.com";
        public static string ExcelRunnerServiceUrl = "https://excel-runner.azurewebsites.net";
        public static string SensitivityStudiesServiceUrl = "https://sensitivity-studies.azurewebsites.net";
        public static string TakeHomeExamsServiceUrl = "https://take-home-exams.azurewebsites.net";
        public static string Environment = "Production";

        static DashboardSettings()
        {

            var s365Environment = ConfigurationManager.AppSettings.Get("S365Environment");
            if (!String.IsNullOrEmpty(s365Environment) && s365Environment.ToLowerInvariant() == "staging")
            {
                DashboardServiceUrl = "https://s365-dashboard-v2-service-staging.azurewebsites.net";
                ExcelRunnerServiceUrl = "https://excel-runner-staging.azurewebsites.net";
                SensitivityStudiesServiceUrl = "https://sensitivity-studies-staging.azurewebsites.net";
                TakeHomeExamsServiceUrl = "https://take-home-exams-staging.azurewebsites.net";
                Environment = "Staging";
            }

        }
    }
}
