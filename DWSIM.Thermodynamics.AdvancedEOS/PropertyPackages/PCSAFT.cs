namespace DWSIM.Thermodynamics.AdvancedEOS
{
    public class PCSAFTPropertyPackage : AdvEOSPropertyPackageBase
    {
        public PCSAFTPropertyPackage()
        {
            PropertyPackageModel = Model.PC_SAFT;

            ComponentName = "PC-SAFT (with Association Support)";
            ComponentDescription = "";

            IsConfigurable = true;

        }

        protected override string GetModelSpecificParameters()
        {
            return "";
        }
    }
}

