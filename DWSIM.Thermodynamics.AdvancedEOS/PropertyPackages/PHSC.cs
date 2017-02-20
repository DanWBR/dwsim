namespace DWSIM.Thermodynamics.AdvancedEOS
{
    public class PHSCPropertyPackage : AdvEOSPropertyPackageBase
    {
        public PHSCPropertyPackage()
        {
            PropertyPackageModel = Model.PHSC;
        }

        protected override string GetModelSpecificParameters()
        {
            return "";
        }
    }
}

