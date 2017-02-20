namespace DWSIM.Thermodynamics.AdvancedEOS
{
    public class PRWSPropertyPackage : AdvEOSPropertyPackageBase
    {
        public PRWSPropertyPackage()
        {
            PropertyPackageModel = Model.PRWS;
        }

        protected override string GetModelSpecificParameters()
        {
            return "";
        }
    }
}


