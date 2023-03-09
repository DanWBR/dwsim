#define S365_STAGING
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.UI.Web.Settings
{
    public static class DashboardSettings
    {
#if S365_STAGING
        public static string DashboardServiceUrl = "https://s365-dashboard-v2-service-staging.azurewebsites.net";
        public static string ExcelRunnerServiceUrl = "https://excel-runner-staging.azurewebsites.net";
        public static string SensitivityStudiesServiceUrl = "https://sensitivity-studies-staging.azurewebsites.net";
        public static string TakeHomeExamsServiceUrl = "https://take-home-exams-staging.azurewebsites.net";   

#else
        public static string DashboardServiceUrl = "https://s365-dashboard-v2-service.azurewebsites.net";
        public static string ExcelRunnerServiceUrl = "https://excel-runner.azurewebsites.net";
        public static string SensitivityStudiesServiceUrl = "https://sensitivity-studies.azurewebsites.net";
        public static string TakeHomeExamsServiceUrl = "https://take-home-exams.azurewebsites.net";

#endif
    }
}
