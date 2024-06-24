using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.Thermodynamics.BaseClasses;
using DWSIM.UI.Desktop.Shared;
using Eto.Drawing;
using Eto.Forms;
using c = DWSIM.UI.Shared.Common;
using DWSIM.UI.Shared;

using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;
using DWSIM.Thermodynamics.Streams;
using System.IO;

namespace DWSIM.UI.Desktop.Editors
{
    public class CompoundViewer : TabControl
    {
        private Flowsheet flowsheet;
        private ConstantProperties compound;
        private MaterialStream MatStream;
        private DWSIM.Thermodynamics.PropertyPackages.RaoultPropertyPackage pp;

        private List<double> vxCp, vyCp, vxPvap, vyPvap, vxVisc, vyVisc, vxDHvap, vyDHvap, vxLD, vyLD, vxSD, vySD, vxSCP, vySCP, vxVapVisc,
                vyVapVisc, vxVapThCond, vyVapThCond, vxLiqThCond, vyLiqThCond, vxSurfTens, vySurfTens, vxLiqCp, vyLiqCp;

        public CompoundViewer(Flowsheet fs, ICompoundConstantProperties cp) :
            base()
        {
            flowsheet = fs;
            compound = (ConstantProperties)cp;
            Init();
        }

        void Init()
        {

            pp = new DWSIM.Thermodynamics.PropertyPackages.RaoultPropertyPackage(false);

            MatStream = new MaterialStream("", "", flowsheet, pp);

            //add compound to the dummy material stream
            foreach (var phase in MatStream.Phases.Values)
            {
                phase.Compounds.Add(compound.Name, new Compound(compound.Name, ""));
                phase.Compounds[compound.Name].ConstantProperties = compound;
            }
            MatStream.EqualizeOverallComposition();

            pp.CurrentMaterialStream = MatStream;

            var container1 = new DynamicLayout { Padding = new Padding(10), Tag = "Constant" };

            AddProperties(container1);

            var container2 = new DynamicLayout { Padding = new Padding(10), Tag = "Molecular" };

            AddMolecularProperties(container2);

            SetupGraphicalData();

            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;

            Pages.Add(new TabPage { Content = new Scrollable { Content = container1 }, Text = (string)container1.Tag });
            Pages.Add(new TabPage { Content = container2, Text = (string)container2.Tag });

            if (!compound.IsSalt && !compound.IsIon && !compound.IsBlackOil)
            {
                var tabl = new TabControl();
                tabl.Pages.Add(new TabPage
                {
                    Text = "Heat Capacity",
                    Content = new Eto.OxyPlot.Plot
                    {
                        Model = c.CreatePlotModel(vxLiqCp.ToArray(), vyLiqCp.ToArray(),
                            "Heat Capacity", "", "Temperature" + " (" + su.temperature + ")", "Heat Capacity" + " (" + su.heatCapacityCp + ")")
                    }
                });
                tabl.Pages.Add(new TabPage
                {
                    Text = "Vapor Pressure",
                    Content = new Eto.OxyPlot.Plot
                    {
                        Model = c.CreatePlotModel(vxPvap.ToArray(), vyPvap.ToArray(),
                            "Vapor Pressure", "", "Temperature" + " (" + su.temperature + ")", "Vapor Pressure" + " (" + su.vaporPressure + ")")
                    }
                });
                tabl.Pages.Add(new TabPage
                {
                    Text = "Heat of Vaporization",
                    Content = new Eto.OxyPlot.Plot
                    {
                        Model = c.CreatePlotModel(vxDHvap.ToArray(), vyDHvap.ToArray(),
                            "Heat of Vaporization", "", "Temperature" + " (" + su.temperature + ")", "Heat of Vaporization" + " (" + su.enthalpy + ")")
                    }
                });
                tabl.Pages.Add(new TabPage
                {
                    Text = "Density",
                    Content = new Eto.OxyPlot.Plot
                    {
                        Model = c.CreatePlotModel(vxLD.ToArray(), vyLD.ToArray(),
                            "Density", "", "Temperature", "Density" + " (" + su.density + ")")
                    }
                });
                tabl.Pages.Add(new TabPage
                {
                    Text = "Viscosity",
                    Content = new Eto.OxyPlot.Plot
                    {
                        Model = c.CreatePlotModel(vxVisc.ToArray(), vyVisc.ToArray(),
                            "Viscosity", "", "Temperature" + " (" + su.temperature + ")", "Viscosity" + " (" + su.viscosity + ")")
                    }
                });
                tabl.Pages.Add(new TabPage
                {
                    Text = "Thermal Conductivity",
                    Content = new Eto.OxyPlot.Plot
                    {
                        Model = c.CreatePlotModel(vxLiqThCond.ToArray(), vyLiqThCond.ToArray(),
                            "Thermal Conductivity", "", "Temperature", "Thermal Conductivity" + " (" + su.thermalConductivity + ")")
                    }
                });
                tabl.Pages.Add(new TabPage
                {
                    Text = "Surface Tension",
                    Content = new Eto.OxyPlot.Plot
                    {
                        Model = c.CreatePlotModel(vxSurfTens.ToArray(), vySurfTens.ToArray(),
                            "Surface Tension", "", "Temperature" + " (" + su.temperature + ")", "Surface Tension" + " (" + su.surfaceTension + ")")
                    }
                });

                var tabv = new TabControl();

                tabv.Pages.Add(new TabPage
                {
                    Text = "Ideal Gas Heat Capacity",
                    Content = new Eto.OxyPlot.Plot
                    {
                        Model = c.CreatePlotModel(vxCp.ToArray(), vyCp.ToArray(),
                            "Ideal Gas Heat Capacity", "", "Temperature" + " (" + su.temperature + ")", "Ideal Gas Heat Capacity" + " (" + su.heatCapacityCp + ")")
                    }
                });

                tabv.Pages.Add(new TabPage
                {
                    Text = "Viscosity",
                    Content = new Eto.OxyPlot.Plot
                    {
                        Model = c.CreatePlotModel(vxVapVisc.ToArray(), vyVapVisc.ToArray(),
                            "Viscosity", "", "Temperature" + " (" + su.temperature + ")", "Viscosity" + " (" + su.viscosity + ")")
                    }
                });

                tabv.Pages.Add(new TabPage
                {
                    Text = "Thermal Conductivity",
                    Content = new Eto.OxyPlot.Plot
                    {
                        Model = c.CreatePlotModel(vxVapThCond.ToArray(), vyVapThCond.ToArray(),
                            "Thermal Conductivity", "", "Temperature" + " (" + su.temperature + ")", "Thermal Conductivity" + " (" + su.thermalConductivity + ")")
                    }
                });

                var tabs = new TabControl();

                tabs.Pages.Add(new TabPage
                {
                    Text = "Heat Capacity",
                    Content = new Eto.OxyPlot.Plot
                    {
                        Model = c.CreatePlotModel(vxSCP.ToArray(), vySCP.ToArray(),
                            "Heat Capacity", "", "Temperature" + " (" + su.temperature + ")", "Heat Capacity" + " (" + su.heatCapacityCp + ")")
                    }
                });

                tabs.Pages.Add(new TabPage
                {
                    Text = "Density",
                    Content = new Eto.OxyPlot.Plot
                    {
                        Model = c.CreatePlotModel(vxSD.ToArray(), vySD.ToArray(),
                            "Density", "", "Temperature" + " (" + su.temperature + ")", "Density" + " (" + su.density + ")")
                    }
                });
                Pages.Add(new TabPage { Content = tabl, Text = "Liquid" });
                Pages.Add(new TabPage { Content = tabv, Text = "Vapor" });
                Pages.Add(new TabPage { Content = tabs, Text = "Solid" });
            }

        }

        private void AddProperties(DynamicLayout container)
        {

            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;
            var nf = flowsheet.FlowsheetOptions.NumberFormat;

            container.CreateAndAddLabelRow("Constant Properties");

            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("Database"), compound.OriginalDB);
            container.CreateAndAddTwoLabelsRow("ID", compound.ID.ToString());
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("CASNumber"), compound.CAS_Number);
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("Massamolar") + " (" + su.molecularWeight + ")", compound.Molar_Weight.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("TemperaturaCrtica") + " (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, compound.Critical_Temperature).ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("PressoCrtica") + " (" + su.pressure + ")", cv.ConvertFromSI(su.pressure, compound.Critical_Pressure).ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("VolumeCrtico") + " (" + su.molar_volume + ")", cv.ConvertFromSI(su.molar_volume, compound.Critical_Volume).ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("CompressibilidadeCrt"), compound.Critical_Compressibility.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("FatorAcntrico"), compound.Acentric_Factor.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("EntalpiadeFormaodoGs") + " (" + su.enthalpy + ")", cv.ConvertFromSI(su.enthalpy, compound.IG_Enthalpy_of_Formation_25C).ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("EnergyFlowdeGibbsdeForm2") + " (" + su.enthalpy + ")", cv.ConvertFromSI(su.entropy, compound.IG_Gibbs_Energy_of_Formation_25C).ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("PontoNormaldeEbulio") + " (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, compound.Normal_Boiling_Point).ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("TemperatureOfFusion") + " (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, compound.TemperatureOfFusion).ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("EnthalpyOfFusionAtTf") + " (kJ/mol)", compound.EnthalpyOfFusionAtTf.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("ChaoSeaderAcentricFactor"), compound.Chao_Seader_Acentricity.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("ChaoSeaderSolubilityParameter"), compound.Chao_Seader_Solubility_Parameter.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("ChaoSeaderLiquidMolarVolume") + " (mL/mol)", compound.Chao_Seader_Liquid_Molar_Volume.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("RackettCompressibility"), compound.Z_Rackett.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("PengRobinsonVolumeTranslationCoefficient"), compound.PR_Volume_Translation_Coefficient.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("SRKVolumeTranslationCoefficient"), compound.SRK_Volume_Translation_Coefficient.ToString(nf));
            container.CreateAndAddTwoLabelsRow("UNIQUAC R", compound.UNIQUAC_R.ToString(nf));
            container.CreateAndAddTwoLabelsRow("UNIQUAC Q", compound.UNIQUAC_Q.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("Charge"), compound.Charge.ToString("#;-#"));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("HydrationNumber"), compound.HydrationNumber.ToString());
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("PositiveIon"), compound.PositiveIon);
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("NegativeIon"), compound.NegativeIon);
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("TemperatureOfSolidDensity_Ts") + " (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, compound.SolidTs).ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("SolidDensityAtTs") + " (" + su.density + ")", cv.ConvertFromSI(su.density, compound.SolidDensityAtTs).ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("Electrolyte_DelGF") + " (kJ/mol)", compound.Electrolyte_DelGF.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("Electrolyte_DelHF") + " (kJ/mol)", compound.Electrolyte_DelHF.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("Electrolyte_Cp0") + " (kJ/[mol.K])", compound.Electrolyte_Cp0.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("Electrolyte_StdStateMolVol") + " (cm3/mol)", compound.StandardStateMolarVolume.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("BlackOil_SGG"), compound.BO_SGG.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("BlackOil_SGO"), compound.BO_SGO.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("BlackOil_GOR") + " (" + su.gor + ")", cv.ConvertFromSI(su.gor, compound.BO_GOR).ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("BlackOil_BSW"), compound.BO_BSW.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("BlackOil_V1") + " (" + su.cinematic_viscosity + ")", cv.ConvertFromSI(su.cinematic_viscosity, compound.BO_OilVisc1).ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("BlackOil_T1") + " (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, compound.BO_OilViscTemp1).ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("BlackOil_V2") + " (" + su.cinematic_viscosity + ")", cv.ConvertFromSI(su.cinematic_viscosity, compound.BO_OilVisc2).ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("BlackOil_T2") + " (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, compound.BO_OilViscTemp2).ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("BlackOil_PNA_P"), compound.BO_PNA_P.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("BlackOil_PNA_N"), compound.BO_PNA_N.ToString(nf));
            container.CreateAndAddTwoLabelsRow(flowsheet.GetTranslatedString("BlackOil_PNA_A"), compound.BO_PNA_A.ToString(nf));


        }

        private void AddMolecularProperties(DynamicLayout container)
        {

            //UNIFAC
            string tbUNIFAC = "";
            if ((compound.UNIFACGroups != null))
            {
                Thermodynamics.PropertyPackages.Auxiliary.Unifac unif = new Thermodynamics.PropertyPackages.Auxiliary.Unifac();
                foreach (string s in compound.UNIFACGroups.Keys)
                {
                    tbUNIFAC += unif.ID2Group(int.Parse(s)) + " " + compound.UNIFACGroups[s] + ", ";
                }
                tbUNIFAC = tbUNIFAC.TrimEnd(new char[] { ',', ' ' });
            }

            //MODFAC
            string tbMODFAC = "";
            if ((compound.MODFACGroups != null))
            {
                Thermodynamics.PropertyPackages.Auxiliary.Modfac unif = new Thermodynamics.PropertyPackages.Auxiliary.Modfac();
                foreach (string s in compound.MODFACGroups.Keys)
                {
                    tbMODFAC += unif.ID2Group(int.Parse(s)) + " " + compound.MODFACGroups[s] + ", ";
                }
                tbMODFAC = tbMODFAC.TrimEnd(new char[] { ',', ' ' });
            }

            //MODFAC
            string tbMODFACNIST = "";
            if ((compound.NISTMODFACGroups != null))
            {
                Thermodynamics.PropertyPackages.Auxiliary.NISTMFAC unif = new Thermodynamics.PropertyPackages.Auxiliary.NISTMFAC();
                foreach (string s in compound.NISTMODFACGroups.Keys)
                {
                    tbMODFACNIST += unif.ID2Group(int.Parse(s)) + " " + compound.NISTMODFACGroups[s] + ", ";
                }
                tbMODFACNIST = tbMODFACNIST.TrimEnd(new char[] { ',', ' ' });
            }

            string tbFormula = compound.Formula;
            string tbSMILES = compound.SMILES;
            string tbInChI = compound.InChI;

            container.CreateAndAddLabelRow("Molecular Properties");
            container.CreateAndAddStringEditorRow2("Formula", "", tbFormula, null).Enabled = false;
            container.CreateAndAddStringEditorRow2("UNIFAC Groups", "", tbUNIFAC, null).Enabled = false;
            container.CreateAndAddStringEditorRow2("MODFAC-Do Groups", "", tbMODFAC, null).Enabled = false;
            container.CreateAndAddStringEditorRow2("MODFAC-NIST Groups", "", tbMODFACNIST, null).Enabled = false;
            container.CreateAndAddStringEditorRow2("SMILES String", "", tbSMILES, null).Enabled = false;
            container.CreateAndAddStringEditorRow2("InChI String", "", tbInChI, null).Enabled = false;

        }

        private void SetupGraphicalData()
        {

            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;
            var nf = flowsheet.FlowsheetOptions.NumberFormat;

            vxCp = new List<double>();
            vyCp = new List<double>();
            vxPvap = new List<double>();
            vyPvap = new List<double>();
            vxVisc = new List<double>();
            vyVisc = new List<double>();
            vxDHvap = new List<double>();
            vyDHvap = new List<double>();
            vxLD = new List<double>();
            vyLD = new List<double>();
            vxSD = new List<double>();
            vySD = new List<double>();
            vxSCP = new List<double>();
            vySCP = new List<double>();
            vxVapVisc = new List<double>();
            vyVapVisc = new List<double>();
            vxVapThCond = new List<double>();
            vyVapThCond = new List<double>();
            vxLiqThCond = new List<double>();
            vyLiqThCond = new List<double>();
            vxSurfTens = new List<double>();
            vySurfTens = new List<double>();
            vxLiqCp = new List<double>();
            vyLiqCp = new List<double>();

            double TD, VD;
            int Row = 0;

            if (!compound.IsBlackOil)
            {
                //setting up curves
                double T = 0;
                double Tmin = 0;
                double Tmax = 0;
                double delta = 0;

                //ideal gas heat capacity
                Tmin = 200;
                Tmax = 1500;
                delta = (Tmax - Tmin) / 50;
                T = Tmin;
                vxCp.Clear();
                vyCp.Clear();

                if (!compound.IsIon & !compound.IsSalt)
                {
                    do
                    {
                        try
                        {
                            TD = cv.ConvertFromSI(su.temperature, T);
                            VD = cv.ConvertFromSI(su.heatCapacityCp, pp.AUX_CPi(compound.Name, T));
                            vxCp.Add(TD);
                            vyCp.Add(VD);
                        }
                        catch (Exception) { }
                        T += delta;
                    } while (!(T > Tmax));
                }

                //vapor viscosity
                var _with3 = compound;
                if (_with3.Vapor_Viscosity_Tmin != 0)
                {
                    Tmin = 0.6 * _with3.Vapor_Viscosity_Tmin;
                    Tmax = _with3.Vapor_Viscosity_Tmax;
                }
                else
                {
                    Tmin = 0.6 * _with3.Critical_Temperature;
                    Tmax = _with3.Critical_Temperature;
                }
                delta = (Tmax - Tmin) / 50;
                T = Tmin;
                Row = 0;
                vxVapVisc.Clear();
                vyVapVisc.Clear();
                if (!compound.IsIon & !compound.IsSalt)
                {
                    do
                    {
                        try
                        {
                            TD = cv.ConvertFromSI(su.temperature, T);
                            VD = cv.ConvertFromSI(su.viscosity, pp.AUX_VAPVISCi(compound, T));
                            vxVapVisc.Add(TD);
                            vyVapVisc.Add(VD);
                        }
                        catch (Exception) { }
                        T += delta;
                        Row += 1;
                    } while (!(Row == 51));
                }

                //vapor thermal conductivity
                var _with6 = compound;
                Tmin = _with6.Vapor_Thermal_Conductivity_Tmin;
                Tmax = _with6.Vapor_Thermal_Conductivity_Tmax;
                if (Tmin == 0)
                    Tmin = _with6.Normal_Boiling_Point;
                if (Tmax == 0)
                    Tmax = _with6.Critical_Temperature;
                delta = (Tmax - Tmin) / 50;
                T = Tmin;
                Row = 0;
                vxVapThCond.Clear();
                vyVapThCond.Clear();
                if (!compound.IsIon & !compound.IsSalt)
                {
                    do
                    {
                        try
                        {
                            TD = cv.ConvertFromSI(su.temperature, T);
                            VD = cv.ConvertFromSI(su.thermalConductivity, pp.AUX_VAPTHERMCONDi(compound, T, 101325));
                            vxVapThCond.Add(TD);
                            vyVapThCond.Add(VD);
                        }
                        catch (Exception) { }
                        T += delta;
                        Row += 1;
                    } while (!(Row == 51));
                }

                //======== liquid properties ===================================================================

                //liquid heat capacity
                var _with9 = compound;
                Tmin = _with9.Liquid_Heat_Capacity_Tmin;
                Tmax = _with9.Liquid_Heat_Capacity_Tmax;
                if (Tmin == 0)
                    Tmin = _with9.TemperatureOfFusion;
                if (Tmin == 0)
                    Tmin = _with9.Normal_Boiling_Point * 0.6;
                if (Tmax == 0)
                    Tmax = _with9.Normal_Boiling_Point * 0.99;
                delta = (Tmax - Tmin) / 50;
                T = Tmin;
                Row = 0;
                vxLiqCp.Clear();
                vyLiqCp.Clear();

                if (!compound.IsIon & !compound.IsSalt)
                {
                    do
                    {
                        try
                        {
                            TD = cv.ConvertFromSI(su.temperature, T);
                            VD = cv.ConvertFromSI(su.heatCapacityCp, pp.AUX_LIQ_Cpi(compound, T));
                            vxLiqCp.Add(TD);
                            vyLiqCp.Add(VD);
                        }
                        catch (Exception) { }
                        T += delta;
                        Row += 1;
                    } while (!(T > Tmax));
                }

                //vaporization enthalpy
                var _with12 = compound;
                Tmin = _with12.HVap_TMIN;
                Tmax = _with12.HVap_TMAX;
                if (Tmin == 0)
                    Tmin = 0.6 * _with12.Critical_Temperature;
                if (Tmax == 0)
                    Tmax = _with12.Critical_Temperature * 0.999;
                delta = (Tmax - Tmin) / 50;
                T = Tmin;
                Row = 0;
                vxDHvap.Clear();
                vyDHvap.Clear();
                if (!compound.IsIon & !compound.IsSalt)
                {
                    do
                    {
                        try
                        {
                            TD = cv.ConvertFromSI(su.temperature, T);
                            VD = cv.ConvertFromSI(su.enthalpy, (double)pp.AUX_HVAPi(compound.Name, T));
                            vxDHvap.Add(TD);
                            vyDHvap.Add(VD);
                        }
                        catch (Exception) { }
                        T += delta;
                        Row += 1;
                    } while (!(Row == 51));
                }

                //vapor pressure
                var _with15 = compound;
                Tmin = _with15.Vapor_Pressure_TMIN;
                Tmax = _with15.Vapor_Pressure_TMAX;
                if (Tmin == 0)
                    Tmin = 0.4 * _with15.Critical_Temperature;
                if (Tmax == 0)
                    Tmax = _with15.Critical_Temperature;
                delta = (Tmax - Tmin) / 50;
                T = Tmin;
                Row = 0;
                vxPvap.Clear();
                vyPvap.Clear();
                if (!compound.IsIon & !compound.IsSalt)
                {
                    do
                    {
                        try
                        {
                            TD = cv.ConvertFromSI(su.temperature, T);
                            VD = cv.ConvertFromSI(su.pressure, (double)pp.AUX_PVAPi(compound.Name, T));
                            vxPvap.Add(TD);
                            vyPvap.Add(VD);
                        }
                        catch (Exception) { }
                        T += delta;
                        Row += 1;
                    } while (!(Row == 51));
                }

                //liquid surface tension
                var _with18 = compound;
                Tmin = _with18.Surface_Tension_Tmin;
                Tmax = _with18.Surface_Tension_Tmax;
                if (Tmin == 0)
                    Tmin = _with18.TemperatureOfFusion;
                if (Tmin == 0)
                    Tmin = _with18.Normal_Boiling_Point * 0.6;
                if (Tmax == 0)
                    Tmax = _with18.Normal_Boiling_Point * 0.999;
                delta = (Tmax - Tmin) / 50;
                T = Tmin;
                Row = 0;
                vxSurfTens.Clear();
                vySurfTens.Clear();
                if (!compound.IsIon & !compound.IsSalt)
                {
                    do
                    {
                        try
                        {
                            TD = cv.ConvertFromSI(su.temperature, T);
                            VD = cv.ConvertFromSI(su.surfaceTension, pp.AUX_SURFTi(compound, T));
                            vxSurfTens.Add(TD);
                            vySurfTens.Add(VD);
                        }
                        catch (Exception) { }
                        T += delta;
                        Row += 1;
                    } while (!(Row == 51));
                }

                //liquid viscosity
                var _with21 = compound;
                Tmin = 0.6 * _with21.Critical_Temperature;
                Tmax = _with21.Critical_Temperature;
                delta = (Tmax - Tmin) / 50;
                T = Tmin;
                Row = 0;
                vxVisc.Clear();
                vyVisc.Clear();
                if (!compound.IsIon & !compound.IsSalt)
                {
                    do
                    {
                        try
                        {
                            TD = cv.ConvertFromSI(su.temperature, T);
                            VD = cv.ConvertFromSI(su.viscosity, (double)pp.AUX_LIQVISCi(compound.Name, T, 101325));
                            vxVisc.Add(TD);
                            vyVisc.Add(VD);
                        }
                        catch (Exception) { }
                        T += delta;
                        Row += 1;
                    } while (!(Row == 51));
                }

                //liquid density
                var _with24 = compound;
                Tmin = _with24.Liquid_Density_Tmin;
                Tmax = _with24.Liquid_Density_Tmax;
                if (Tmin == 0)
                    Tmin = _with24.TemperatureOfFusion;
                if (Tmin == 0)
                    Tmin = _with24.Normal_Boiling_Point * 0.6;
                if (Tmax == 0)
                    Tmax = _with24.Normal_Boiling_Point * 0.999;
                delta = (Tmax - Tmin) / 50;
                T = Tmin;
                Row = 0;
                vxLD.Clear();
                vyLD.Clear();
                if (!compound.IsIon & !compound.IsSalt)
                {
                    do
                    {
                        try
                        {
                            TD = cv.ConvertFromSI(su.temperature, T);
                            VD = cv.ConvertFromSI(su.density, pp.AUX_LIQDENSi(compound, T));
                            vxLD.Add(TD);
                            vyLD.Add(VD);
                        }
                        catch (Exception) { }
                        T += delta;
                        Row += 1;
                    } while (!(Row == 51));
                }

                //liquid thermal conductivity
                var _with27 = compound;
                Tmin = _with27.Liquid_Thermal_Conductivity_Tmin;
                Tmax = _with27.Liquid_Thermal_Conductivity_Tmax;
                if (Tmin == 0)
                    Tmin = _with27.TemperatureOfFusion;
                if (Tmin == 0)
                    Tmin = _with27.Normal_Boiling_Point * 0.6;
                if (Tmax == 0)
                    Tmax = _with27.Normal_Boiling_Point * 0.999;
                delta = (Tmax - Tmin) / 50;
                T = Tmin;
                Row = 0;
                vxLiqThCond.Clear();
                vyLiqThCond.Clear();
                if (!compound.IsIon & !compound.IsSalt)
                {
                    do
                    {
                        try
                        {
                            TD = cv.ConvertFromSI(su.temperature, T);
                            VD = cv.ConvertFromSI(su.thermalConductivity, pp.AUX_LIQTHERMCONDi(compound, T));
                            vxLiqThCond.Add(TD);
                            vyLiqThCond.Add(VD);
                        }
                        catch (Exception) { }
                        T += delta;
                        Row += 1;
                    } while (!(Row == 51));
                }

                //======== solid properties ====================================================================
                //solid density
                var _with30 = compound;
                Tmin = _with30.Solid_Density_Tmin;
                Tmax = _with30.Solid_Density_Tmax;
                if (Tmin == 0)
                    Tmin = 50;
                if (Tmax == 0)
                    Tmax = _with30.TemperatureOfFusion;
                if (_with30.TemperatureOfFusion == 0)
                    Tmax = _with30.Normal_Boiling_Point * 0.3;
                delta = (Tmax - Tmin) / 50;
                T = Tmin;
                Row = 0;
                vxSD.Clear();
                vySD.Clear();
                if (!compound.IsIon & !compound.IsSalt)
                {
                    do
                    {
                        try
                        {
                            TD = cv.ConvertFromSI(su.temperature, T);
                            VD = cv.ConvertFromSI(su.density, pp.AUX_SOLIDDENSi(compound, T));
                            vxSD.Add(TD);
                            vySD.Add(VD);
                        }
                        catch (Exception) { }
                        T += delta;
                        Row += 1;
                    } while (!(Row == 51));
                }

                //solid heat capacity
                var _with33 = compound;
                Tmin = _with33.Solid_Heat_Capacity_Tmin;
                Tmax = _with33.Solid_Heat_Capacity_Tmax;
                if (Tmin == 0)
                    Tmin = 50;
                if (Tmax == 0)
                    Tmax = _with33.TemperatureOfFusion;
                if (_with33.TemperatureOfFusion == 0)
                    Tmax = _with33.Normal_Boiling_Point * 0.3;
                delta = (Tmax - Tmin) / 50;
                T = Tmin;
                Row = 0;
                vxSCP.Clear();
                vySCP.Clear();
                if (!compound.IsIon & !compound.IsSalt)
                {
                    do
                    {
                        try
                        {
                            TD = cv.ConvertFromSI(su.temperature, T);
                            VD = cv.ConvertFromSI(su.heatCapacityCp, pp.AUX_SolidHeatCapacity(compound, T));
                            vxSCP.Add(TD);
                            vySCP.Add(VD);
                        }
                        catch (Exception) { }
                        T += delta;
                        Row += 1;
                    } while (!(Row == 51));
                }

            }

        }


        public void ExportToJSON(DynamicLayout layout)
        {
            layout.CreateAndAddLabelAndButtonRow("Export Compound to JSON File", "Export to JSON", null, (arg1, arg2) =>
            {
                var dialog = new SaveFileDialog();
                dialog.Title = "Save Compound to JSON File";
                dialog.Filters.Add(new FileFilter("JSON File", new[] { ".json" }));
                dialog.CurrentFilterIndex = 0;
                if (dialog.ShowDialog(layout) == DialogResult.Ok)
                {
                    try
                    {
                        File.WriteAllText(dialog.FileName, Newtonsoft.Json.JsonConvert.SerializeObject(compound, Newtonsoft.Json.Formatting.Indented));
                        flowsheet.ShowMessage("Compound '" + compound.Name + "' successfully saved to JSON file.", IFlowsheet.MessageType.Information);
                    }
                    catch (Exception ex)
                    {
                        flowsheet.ShowMessage("Error saving compound to JSON file: " + ex.ToString(), IFlowsheet.MessageType.GeneralError);
                    }
                }
            });
        }

    }
}
