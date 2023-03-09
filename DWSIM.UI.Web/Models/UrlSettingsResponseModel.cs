using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.UI.Web.Models
{
    public class UrlSettingsResponseModel
    {
        public string DashboardServiceUrl { get; set; }
        public string ExcelRunnerServiceUrl { get; set; }
        public string SensitivityStudiesServiceUrl { get; set; }
        public string TakeHomeExamsServiceUrl { get; set; }
    }
}
