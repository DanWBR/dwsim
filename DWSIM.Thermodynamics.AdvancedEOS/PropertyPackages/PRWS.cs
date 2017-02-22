namespace DWSIM.Thermodynamics.AdvancedEOS
{
    public class PRWSPropertyPackage : AdvEOSPropertyPackageBase
    {
        public PRWSPropertyPackage()
        {
            PropertyPackageModel = Model.PRWS;
            ComponentName = "Peng-Robinson w/ Wong-Sandler Mixing Rules (PRWS)";
            ComponentDescription = "";
            IsConfigurable = true;
        }

        protected override string GetModelSpecificParameters()
        {
            return "";
        }
    }
}


