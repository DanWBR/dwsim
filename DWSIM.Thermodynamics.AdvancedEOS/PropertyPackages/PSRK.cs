namespace DWSIM.Thermodynamics.AdvancedEOS
{
    public class PSRKPropertyPackage : AdvEOSPropertyPackageBase
    {
        public PSRKPropertyPackage()
        {
            PropertyPackageModel = Model.PSRK;
        }

        protected override string GetModelSpecificParameters()
        {
            return "";
        }
    }
}

