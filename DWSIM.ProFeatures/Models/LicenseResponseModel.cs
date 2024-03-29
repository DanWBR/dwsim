
namespace DWSIM.ProFeatures
{
    public class LicenseResponseModel
    {
        public bool? hasExistingLicense { get; set; }
        public bool? trialLicenseCreated { get; set; }
        public bool? notEligibleForTrial { get; set; }
    }
}