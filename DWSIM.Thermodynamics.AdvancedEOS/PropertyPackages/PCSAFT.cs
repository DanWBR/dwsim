namespace DWSIM.Thermodynamics.AdvancedEOS
{
    public class PCSAFTPropertyPackage : AdvEOSPropertyPackageBase
    {
        public PCSAFTPropertyPackage()
        {
            PropertyPackageModel = Model.PC_SAFT;
        }

        protected override string GetModelSpecificParameters()
        {
            return "";
        }
    }
}

