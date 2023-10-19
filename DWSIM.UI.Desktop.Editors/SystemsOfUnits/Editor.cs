using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.SharedClasses.SystemsOfUnits;
using DWSIM.Thermodynamics.BaseClasses;
using Eto.Drawing;
using Eto.Forms;
using s = DWSIM.UI.Shared.Common;

namespace DWSIM.UI.Desktop.Editors
{
    public class UnitSetEditorView : DynamicLayout
    {

        int contID = new Random().Next();

        private Units userunits = new Units();

        public UnitSetEditorView(Units units)
            : base()
        {
            userunits = units;
            Init();
        }

        protected void Init()
        {

            Padding = new Padding(10);

            s.CreateAndAddLabelRow(this, "Identification");

            s.CreateAndAddStringEditorRow2(this, "Name", "", userunits.Name, (sender, e) => userunits.Name = sender.Text);

            s.CreateAndAddLabelRow(this, "Units");

            var arr1 = userunits.GetUnitSet(UnitOfMeasure.temperature);
            s.CreateAndAddDropDownRow(this, "Temperature", arr1, Array.IndexOf(arr1.ToArray(), userunits.temperature),
                         (arg3, arg2) =>
                         {
                             userunits.temperature = arr1[arg3.SelectedIndex];
                         });

            var arr2 = userunits.GetUnitSet(UnitOfMeasure.pressure);
            s.CreateAndAddDropDownRow(this, "Pressure", arr2, Array.IndexOf(arr2.ToArray(), userunits.pressure),
                         (arg3, arg2) =>
                         {
                             userunits.pressure = arr2[arg3.SelectedIndex];
                         });

            var arr3 = userunits.GetUnitSet(UnitOfMeasure.massflow);
            s.CreateAndAddDropDownRow(this, "Mass Flow", arr3, Array.IndexOf(arr3.ToArray(), userunits.massflow),
                         (arg3, arg2) =>
                         {
                             userunits.massflow = arr3[arg3.SelectedIndex];
                         });

            var arr4 = userunits.GetUnitSet(UnitOfMeasure.molarflow);
            s.CreateAndAddDropDownRow(this, "Molar Flow", arr4, Array.IndexOf(arr4.ToArray(), userunits.molarflow),
                         (arg3, arg2) =>
                         {
                             userunits.molarflow = arr4[arg3.SelectedIndex];
                         });


            var arr5 = userunits.GetUnitSet(UnitOfMeasure.volumetricFlow);
            s.CreateAndAddDropDownRow(this, "Volumetric Flow", arr5, Array.IndexOf(arr5.ToArray(), userunits.volumetricFlow),
                         (arg3, arg2) =>
                         {
                             userunits.volumetricFlow = arr5[arg3.SelectedIndex];
                         });

            var arr6 = userunits.GetUnitSet(UnitOfMeasure.deltaT);
            s.CreateAndAddDropDownRow(this, "Temperature diff.", arr6, Array.IndexOf(arr6.ToArray(), userunits.deltaT),
                         (arg3, arg2) =>
                         {
                             userunits.deltaT = arr6[arg3.SelectedIndex];
                         });

            var arr7 = userunits.GetUnitSet(UnitOfMeasure.deltaP);
            s.CreateAndAddDropDownRow(this, "Pressure diff.", arr7, Array.IndexOf(arr7.ToArray(), userunits.deltaP),
                         (arg3, arg2) =>
                         {
                             userunits.deltaP = arr7[arg3.SelectedIndex];
                         });

            var arr8 = userunits.GetUnitSet(UnitOfMeasure.heatflow);
            s.CreateAndAddDropDownRow(this, "Heat Flow/Power", arr8, Array.IndexOf(arr8.ToArray(), userunits.heatflow),
                         (arg3, arg2) =>
                         {
                             userunits.heatflow = arr8[arg3.SelectedIndex];
                         });

            var arr8a = userunits.GetUnitSet(UnitOfMeasure.heat);
            s.CreateAndAddDropDownRow(this, "Heat/Energy", arr8a, Array.IndexOf(arr8a.ToArray(), userunits.heat),
                         (arg3, arg2) =>
                         {
                             userunits.heat = arr8a[arg3.SelectedIndex];
                         });

            var arr9 = userunits.GetUnitSet(UnitOfMeasure.enthalpy);
            s.CreateAndAddDropDownRow(this, "Mass Enthalpy", arr9, Array.IndexOf(arr9.ToArray(), userunits.enthalpy),
                         (arg3, arg2) =>
                         {
                             userunits.enthalpy = arr9[arg3.SelectedIndex];
                         });

            var arr10 = userunits.GetUnitSet(UnitOfMeasure.entropy);
            s.CreateAndAddDropDownRow(this, "Mass Entropy", arr10, Array.IndexOf(arr10.ToArray(), userunits.entropy),
                         (arg3, arg2) =>
                         {
                             userunits.entropy = arr10[arg3.SelectedIndex];
                         });

            var arr11 = userunits.GetUnitSet(UnitOfMeasure.viscosity);
            s.CreateAndAddDropDownRow(this, "Dynamic Viscosity", arr11, Array.IndexOf(arr11.ToArray(), userunits.viscosity),
                         (arg3, arg2) =>
                         {
                             userunits.viscosity = arr11[arg3.SelectedIndex];
                         });

            var arr12 = userunits.GetUnitSet(UnitOfMeasure.cinematic_viscosity);
            s.CreateAndAddDropDownRow(this, "Kinematic Viscosity", arr12, Array.IndexOf(arr12.ToArray(), userunits.cinematic_viscosity),
                         (arg3, arg2) =>
                         {
                             userunits.cinematic_viscosity = arr12[arg3.SelectedIndex];
                         });

            var arr13 = userunits.GetUnitSet(UnitOfMeasure.thermalConductivity);
            s.CreateAndAddDropDownRow(this, "Thermal Conductivity", arr13, Array.IndexOf(arr13.ToArray(), userunits.thermalConductivity),
                         (arg3, arg2) =>
                         {
                             userunits.thermalConductivity = arr13[arg3.SelectedIndex];
                         });

            var arr14 = userunits.GetUnitSet(UnitOfMeasure.heatCapacityCp);
            s.CreateAndAddDropDownRow(this, "Heat Capacity", arr14, Array.IndexOf(arr14.ToArray(), userunits.heatCapacityCp),
                         (arg3, arg2) =>
                         {
                             userunits.heatCapacityCp = arr14[arg3.SelectedIndex];
                         });

            var arr15 = userunits.GetUnitSet(UnitOfMeasure.heat_transf_coeff);
            s.CreateAndAddDropDownRow(this, "Heat Transf. Coeff.", arr15, Array.IndexOf(arr15.ToArray(), userunits.heat_transf_coeff),
                         (arg3, arg2) =>
                         {
                             userunits.heat_transf_coeff = arr15[arg3.SelectedIndex];
                         });

            var arr16 = userunits.GetUnitSet(UnitOfMeasure.area);
            s.CreateAndAddDropDownRow(this, "Area", arr16, Array.IndexOf(arr16.ToArray(), userunits.area),
                         (arg3, arg2) =>
                         {
                             userunits.area = arr16[arg3.SelectedIndex];
                         });

            var arr17 = userunits.GetUnitSet(UnitOfMeasure.diameter);
            s.CreateAndAddDropDownRow(this, "Diameter", arr17, Array.IndexOf(arr17.ToArray(), userunits.diameter),
                         (arg3, arg2) =>
                         {
                             userunits.diameter = arr17[arg3.SelectedIndex];
                         });

            var arr18 = userunits.GetUnitSet(UnitOfMeasure.density);
            s.CreateAndAddDropDownRow(this, "Density", arr18, Array.IndexOf(arr18.ToArray(), userunits.density),
                         (arg3, arg2) =>
                         {
                             userunits.density = arr18[arg3.SelectedIndex];
                         });

            var uarr1 = userunits.GetUnitSet(UnitOfMeasure.volume);
            s.CreateAndAddDropDownRow(this, "Volume", uarr1, Array.IndexOf(uarr1.ToArray(), userunits.volume),
                         (arg3, arg2) =>
                         {
                             userunits.volume = uarr1[arg3.SelectedIndex];
                         });
            var uarr2 = userunits.GetUnitSet(UnitOfMeasure.reac_rate);
            s.CreateAndAddDropDownRow(this, "Reaction Rate", uarr2, Array.IndexOf(uarr2.ToArray(), userunits.reac_rate),
                         (arg3, arg2) =>
                         {
                             userunits.reac_rate = uarr2[arg3.SelectedIndex];
                         });
            var uarr3 = userunits.GetUnitSet(UnitOfMeasure.mass_conc);
            s.CreateAndAddDropDownRow(this, "Mass Concentration", uarr3, Array.IndexOf(uarr3.ToArray(), userunits.mass_conc),
                         (arg3, arg2) =>
                         {
                             userunits.mass_conc = uarr3[arg3.SelectedIndex];
                         });
            var uarr4 = userunits.GetUnitSet(UnitOfMeasure.molar_conc);
            s.CreateAndAddDropDownRow(this, "Molar Concentration", uarr4, Array.IndexOf(uarr4.ToArray(), userunits.molar_conc),
                         (arg3, arg2) =>
                         {
                             userunits.molar_conc = uarr4[arg3.SelectedIndex];
                         });
            var uarr5 = userunits.GetUnitSet(UnitOfMeasure.molecularWeight);
            s.CreateAndAddDropDownRow(this, "Molar Weight", uarr5, Array.IndexOf(uarr5.ToArray(), userunits.molecularWeight),
                         (arg3, arg2) =>
                         {
                             userunits.molecularWeight = uarr5[arg3.SelectedIndex];
                         });
            var uarr6 = userunits.GetUnitSet(UnitOfMeasure.time);
            s.CreateAndAddDropDownRow(this, "Time", uarr6, Array.IndexOf(uarr6.ToArray(), userunits.time),
                         (arg3, arg2) =>
                         {
                             userunits.time = uarr6[arg3.SelectedIndex];
                         });
            var uarr7 = userunits.GetUnitSet(UnitOfMeasure.distance);
            s.CreateAndAddDropDownRow(this, "Length/Distance", uarr7, Array.IndexOf(uarr7.ToArray(), userunits.distance),
                         (arg3, arg2) =>
                         {
                             userunits.distance = uarr7[arg3.SelectedIndex];
                         });
            var uarr8 = userunits.GetUnitSet(UnitOfMeasure.compressibility);
            s.CreateAndAddDropDownRow(this, "Compressibility", uarr8, Array.IndexOf(uarr8.ToArray(), userunits.compressibility),
                         (arg3, arg2) =>
                         {
                             userunits.compressibility = uarr8[arg3.SelectedIndex];
                         });
            var uarr9 = userunits.GetUnitSet(UnitOfMeasure.jouleThomsonCoefficient);
            s.CreateAndAddDropDownRow(this, "Joule-Thomson Coeff.", uarr9, Array.IndexOf(uarr9.ToArray(), userunits.jouleThomsonCoefficient),
                         (arg3, arg2) =>
                         {
                             userunits.jouleThomsonCoefficient = uarr9[arg3.SelectedIndex];
                         });
            var uarr10 = userunits.GetUnitSet(UnitOfMeasure.speedOfSound);
            s.CreateAndAddDropDownRow(this, "Speed of Sound", uarr10, Array.IndexOf(uarr10.ToArray(), userunits.speedOfSound),
                         (arg3, arg2) =>
                         {
                             userunits.speedOfSound = uarr10[arg3.SelectedIndex];
                         });
            var uarr11 = userunits.GetUnitSet(UnitOfMeasure.heat);
            s.CreateAndAddDropDownRow(this, "Heat/Energy", uarr11, Array.IndexOf(uarr11.ToArray(), userunits.heat),
                         (arg3, arg2) =>
                         {
                             userunits.heat = uarr11[arg3.SelectedIndex];
                         });
            var uarr12 = userunits.GetUnitSet(UnitOfMeasure.emission_factor);
            s.CreateAndAddDropDownRow(this, "Emission Factor", uarr12, Array.IndexOf(uarr12.ToArray(), userunits.emission_factor),
                         (arg3, arg2) =>
                         {
                             userunits.emission_factor = uarr12[arg3.SelectedIndex];
                         });

        }

    }
}
