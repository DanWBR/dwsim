using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using Eto.Forms;

using s = DWSIM.UI.Shared.Common;
using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;

using DWSIM.UnitOperations.UnitOperations.Auxiliary.Pipe;
using DWSIM.UnitOperations.UnitOperations;
using DWSIM.ExtensionMethods;
using DWSIM.CrossPlatform.UI.Controls.ReoGrid.DataFormat;
using DWSIM.CrossPlatform.UI.Controls.ReoGrid;
using DWSIM.GlobalSettings;

namespace DWSIM.UI.Desktop.Editors
{
    public class PipeThermalProfile
    {

        public ThermalEditorDefinitions thermalprops;

        public DynamicLayout container;

        public ISimulationObject SimObject;

        public PipeThermalProfile(ISimulationObject obj, ThermalEditorDefinitions definitions, DynamicLayout layout)
        {

            SimObject = obj;
            thermalprops = definitions;
            container = layout;

            Initialize();

        }

        void Initialize()
        {

            ThermalEditorDefinitions profile = thermalprops;

            var flowsheet = SimObject.GetFlowsheet();

            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;
            var nf = flowsheet.FlowsheetOptions.NumberFormat;

            s.CreateAndAddLabelRow(container, "Definitions");

            int position = 0;
            switch (profile.TipoPerfil)
            {
                case ThermalEditorDefinitions.ThermalProfileType.Definir_CGTC:
                    position = 0;
                    break;
                case ThermalEditorDefinitions.ThermalProfileType.Estimar_CGTC:
                    position = 2;
                    break;
                case ThermalEditorDefinitions.ThermalProfileType.Definir_Q:
                    position = 1;
                    break;
            }

            s.CreateAndAddDropDownRow(container, "Profile Type", Shared.StringArrays.thermalprofiletype().ToList(),
                                     position, (sender, e) =>
                                     {
                                         switch (sender.SelectedIndex)
                                         {
                                             case 0:
                                                 profile.TipoPerfil = ThermalEditorDefinitions.ThermalProfileType.Definir_CGTC;
                                                 break;
                                             case 1:
                                                 profile.TipoPerfil = ThermalEditorDefinitions.ThermalProfileType.Definir_Q;
                                                 break;
                                             case 2:
                                                 profile.TipoPerfil = ThermalEditorDefinitions.ThermalProfileType.Estimar_CGTC;
                                                 break;
                                         }
                                     });

            s.CreateAndAddLabelRow(container, "Defined HTC Parameters");

            s.CreateAndAddTextBoxRow(container, nf, "Overall HTC (" + su.heat_transf_coeff + ")", cv.ConvertFromSI(su.heat_transf_coeff, profile.CGTC_Definido),
                                     (sender, e) =>
                                     {
                                         if (s.IsValidDouble(sender.Text))
                                         {
                                             profile.CGTC_Definido = cv.ConvertToSI(su.heat_transf_coeff, double.Parse(sender.Text));
                                         }
                                     }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)SimObject.GetFlowsheet()).HighLevelSolve.Invoke(); });

            s.CreateAndAddTextBoxRow(container, nf, "Ambient Temperature (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, profile.Temp_amb_definir),
                                     (sender, e) =>
                                     {
                                         if (s.IsValidDouble(sender.Text))
                                         {
                                             profile.Temp_amb_definir = cv.ConvertToSI(su.temperature, double.Parse(sender.Text));
                                         }
                                     }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)SimObject.GetFlowsheet()).HighLevelSolve.Invoke(); });

            s.CreateAndAddTextBoxRow(container, nf, "Ambient Temperature Gradient (" + su.deltaT + "/" + su.distance + ")", cv.ConvertFromSI(su.deltaT, profile.AmbientTemperatureGradient) / cv.ConvertFromSI(su.distance, 1),
                         (sender, e) =>
                         {
                             if (s.IsValidDouble(sender.Text))
                             {
                                 profile.AmbientTemperatureGradient = cv.ConvertToSI(su.deltaT, double.Parse(sender.Text)) / cv.ConvertToSI(su.distance, 1);
                             }
                         }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)SimObject.GetFlowsheet()).HighLevelSolve.Invoke(); });

            s.CreateAndAddCheckBoxRow(container, "Use Tabulated Data", profile.UseUserDefinedU, (chk, e) => profile.UseUserDefinedU = chk.Checked.GetValueOrDefault());

            var gridcontrol = GridControl.GetGridControl();

            var grid = gridcontrol.GridControl;

            grid.Width = (int)(400 * GlobalSettings.Settings.DpiScale);

            var sheet = grid.Worksheets[0];

            sheet.SetRows(100);
            sheet.SetCols(3);
            sheet.SetColumnsWidth(0, 3, (ushort)(120 * Settings.DpiScale));
            sheet.SetRowsHeight(0, 100, (ushort)(20 * Settings.DpiScale));

            sheet.ColumnHeaders[0].Text = String.Format("Length/Depth ({0})", su.distance);
            sheet.ColumnHeaders[1].Text = String.Format("Ambient Temp. ({0})", su.temperature);
            sheet.ColumnHeaders[2].Text = String.Format("Overall HTC ({0})", su.heat_transf_coeff);

            sheet.SetRangeDataFormat(0, 0, 100, 3, CellDataFormatFlag.Number,
            new NumberDataFormatter.NumberFormatArgs
            {
                DecimalPlaces = 4,
                UseSeparator = true,
                NegativeStyle = NumberDataFormatter.NumberNegativeStyle.Minus
            });
            sheet.SetRangeStyles(0, 0, 100, 3, new WorksheetRangeStyle
            {
                Flag = PlainStyleFlag.HorizontalAlign,
                HAlign = ReoGridHorAlign.Right
            });
            sheet.SetRangeStyles(0, 0, 100, 3, new WorksheetRangeStyle
            {
                Flag = PlainStyleFlag.Padding,
                HAlign = ReoGridHorAlign.Right
            });

            grid.Height = 300;

            s.CreateAndAddControlRow(container, gridcontrol);

            for (int i = 0; i < profile.UserDefinedU_Length.Count; i++)
            {
                sheet.SetCellData(i, 0, profile.UserDefinedU_Length[i].ConvertFromSI(su.distance));
            }
            for (int i = 0; i < profile.UserDefinedU_Temp.Count; i++)
            {
                sheet.SetCellData(i, 1, profile.UserDefinedU_Temp[i].ConvertFromSI(su.temperature));
            }
            for (int i = 0; i < profile.UserDefinedU_U.Count; i++)
            {
                sheet.SetCellData(i, 2, profile.UserDefinedU_U[i].ConvertFromSI(su.heat_transf_coeff));
            }

            sheet.CellDataChanged += (s, e) =>
            {
                profile.UserDefinedU_Length.Clear();
                profile.UserDefinedU_Temp.Clear();
                profile.UserDefinedU_U.Clear();
                for (int i = 0; i < 100; i++)
                {
                    var data1 = sheet.GetCellData(i, 0);
                    var data2 = sheet.GetCellData(i, 1);
                    var data3 = sheet.GetCellData(i, 2);
                    if (data1 != null && data2 != null && data3 != null)
                    {
                        try
                        {
                            profile.UserDefinedU_Length.Add(data1.ToString().ToDoubleFromCurrent().ConvertToSI(su.distance));
                            profile.UserDefinedU_Temp.Add(data2.ToString().ToDoubleFromCurrent().ConvertToSI(su.temperature));
                            profile.UserDefinedU_U.Add(data3.ToString().ToDoubleFromCurrent().ConvertToSI(su.heat_transf_coeff));
                        }
                        catch (Exception ex)
                        {
                            MessageBox.Show(String.Format("Error on data table: {0}", ex.Message), "Error", MessageBoxButtons.OK, MessageBoxType.Error);
                        }
                    }
                }
            };

            s.CreateAndAddLabelRow(container, "Defined Heat Exchange Parameters");

            s.CreateAndAddTextBoxRow(container, nf, "Heat Exchanged (" + su.heatflow + ")", cv.ConvertFromSI(su.heatflow, profile.Calor_trocado),
             (sender, e) =>
             {
                 if (s.IsValidDouble(sender.Text))
                 {
                     profile.Calor_trocado = cv.ConvertToSI(su.heatflow, double.Parse(sender.Text));
                 }
             }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)SimObject.GetFlowsheet()).HighLevelSolve.Invoke(); });

            s.CreateAndAddLabelRow(container, "Calculated HTC Parameters");

            s.CreateAndAddTextBoxRow(container, nf, "Ambient Temperature (" + su.temperature + ")", cv.ConvertFromSI(su.temperature, profile.Temp_amb_estimar),
                                     (sender, e) =>
                                     {
                                         if (s.IsValidDouble(sender.Text))
                                         {
                                             profile.Temp_amb_estimar = cv.ConvertToSI(su.temperature, double.Parse(sender.Text));
                                         }
                                     }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)SimObject.GetFlowsheet()).HighLevelSolve.Invoke(); });

            s.CreateAndAddTextBoxRow(container, nf, "Ambient Temperature Gradient (" + su.deltaT + "/" + su.distance + ")", cv.ConvertFromSI(su.deltaT, profile.AmbientTemperatureGradient_EstimateHTC) / cv.ConvertFromSI(su.distance, 1),
                         (sender, e) =>
                         {
                             if (s.IsValidDouble(sender.Text))
                             {
                                 profile.AmbientTemperatureGradient_EstimateHTC = cv.ConvertToSI(su.deltaT, double.Parse(sender.Text)) / cv.ConvertToSI(su.distance, 1);
                             }
                         }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)SimObject.GetFlowsheet()).HighLevelSolve.Invoke(); });

            s.CreateAndAddCheckBoxRow(container, "Include Pipe Walls", profile.Incluir_paredes, (sender, e) =>
            {
                profile.Incluir_paredes = sender.Checked.GetValueOrDefault();
            });

            s.CreateAndAddCheckBoxRow(container, "Include Internal HTC", profile.Incluir_cti, (sender, e) =>
            {
                profile.Incluir_cti = sender.Checked.GetValueOrDefault();
            });

            s.CreateAndAddCheckBoxRow(container, "Include Insulation", profile.Incluir_isolamento, (sender, e) =>
            {
                profile.Incluir_isolamento = sender.Checked.GetValueOrDefault();
            });

            s.CreateAndAddDropDownRow(container, "Insulation Material", Shared.StringArrays.insulationmaterial().ToList(),
                                    profile.Material, (sender, e) =>
                                     {
                                         profile.Material = sender.SelectedIndex;
                                         switch (sender.SelectedIndex)
                                         {
                                             case 0:
                                                 profile.Condtermica = 0.7;
                                                 break;
                                             case 1:
                                                 profile.Condtermica = 1.0;
                                                 break;
                                             case 2:
                                                 profile.Condtermica = 0.018;
                                                 break;
                                             case 3:
                                                 profile.Condtermica = 0.04;
                                                 break;
                                             case 4:
                                                 profile.Condtermica = 0.035;
                                                 break;
                                             case 5:
                                                 profile.Condtermica = 0.036;
                                                 break;
                                             case 6:
                                                 profile.Condtermica = 0.08;
                                                 break;
                                             case 7:
                                                 break;
                                         }
                                     });

            s.CreateAndAddTextBoxRow(container, nf, "Insulation Thickness (" + su.thickness + ")", cv.ConvertFromSI(su.thickness, profile.Espessura),
             (sender, e) =>
             {
                 if (s.IsValidDouble(sender.Text))
                 {
                     profile.Espessura = cv.ConvertToSI(su.thickness, double.Parse(sender.Text));
                 }
             }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)SimObject.GetFlowsheet()).HighLevelSolve.Invoke(); });

            s.CreateAndAddTextBoxRow(container, nf, "User-Def. Thermal Cond. (" + su.thermalConductivity + ")", cv.ConvertFromSI(su.thermalConductivity, profile.Condtermica),
             (sender, e) =>
             {
                 if (s.IsValidDouble(sender.Text))
                 {
                     profile.Condtermica = cv.ConvertToSI(su.thermalConductivity, double.Parse(sender.Text));
                 }
             }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)SimObject.GetFlowsheet()).HighLevelSolve.Invoke(); });

            s.CreateAndAddCheckBoxRow(container, "Include External HTC", profile.Incluir_cte, (sender, e) =>
            {
                profile.Incluir_cte = sender.Checked.GetValueOrDefault();
            });

            s.CreateAndAddDropDownRow(container, "External Environment", Shared.StringArrays.external_env().ToList(),
                                     profile.Meio, (sender, e) =>
                                     {
                                         profile.Meio = sender.SelectedIndex;
                                     });

            s.CreateAndAddTextBoxRow(container, nf, "Velocity/Deepness (m/s | m)", profile.Velocidade,
             (sender, e) =>
             {
                 if (s.IsValidDouble(sender.Text))
                 {
                     profile.Velocidade = double.Parse(sender.Text);
                 }
             }, () => { if (GlobalSettings.Settings.CallSolverOnEditorPropertyChanged) ((Shared.Flowsheet)SimObject.GetFlowsheet()).HighLevelSolve.Invoke(); });
            s.CreateAndAddEmptySpace(container);
            s.CreateAndAddEmptySpace(container);

        }
    }
}