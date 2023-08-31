Imports DWSIM.Thermodynamics.PropertyPackages

Public Class PropertyPackageSettingsEditingControl

    Public PropPack As PropertyPackage

    Public Sub New(pp As PropertyPackage)

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
        PropPack = pp

    End Sub

    Private Sub PropertyPackageSettingsEditingControl_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        chkIgnoreIPs.Enabled = TypeOf PropPack Is NRTLPropertyPackage Or TypeOf PropPack Is UNIQUACPropertyPackage

        chkIgnoreSalLim.Enabled = TypeOf PropPack Is SeawaterPropertyPackage

        chkIgnoreVapFracLim.Enabled = TypeOf PropPack Is SourWaterPropertyPackage

        cbHSCpCalcMode.Enabled = TypeOf PropPack Is ActivityCoefficientPropertyPackage Or TypeOf PropPack Is RaoultPropertyPackage Or PropPack.PackageType = PackageType.ActivityCoefficient

        cbEOSLiqEnthMethod.Enabled = TypeOf PropPack Is PRSV2PropertyPackage Or TypeOf PropPack Is PRSV2VLPropertyPackage

        chkVapFugIdeal.Enabled = TypeOf PropPack Is ActivityCoefficientPropertyPackage Or PropPack.PackageType = PackageType.ActivityCoefficient

        chkAutoEstimateNU.Enabled = TypeOf PropPack Is NRTLPropertyPackage Or TypeOf PropPack Is UNIQUACPropertyPackage

        cbLiqDens.SelectedIndex = PropPack.LiquidDensityCalculationMode_Subcritical

        cbLiqVisc.SelectedIndex = PropPack.LiquidViscosityCalculationMode_Subcritical

        cbLiqVIscMixRule.SelectedIndex = PropPack.LiquidViscosity_MixingRule

        cbHSCpCalcMode.SelectedIndex = PropPack.EnthalpyEntropyCpCvCalculationMode

        chkLiqDensPCorr.Checked = PropPack.LiquidDensity_CorrectExpDataForPressure

        chkLiqDensPeneloux.Checked = PropPack.LiquidDensity_UsePenelouxVolumeTranslation

        chkLiqViscPCorr.Checked = PropPack.LiquidViscosity_CorrectExpDataForPressure

        chkLiqFugPoynt.Checked = PropPack.LiquidFugacity_UsePoyntingCorrectionFactor

        chkUseSolidCp.Checked = PropPack.SolidPhaseEnthalpy_UsesCp

        chkVapFugIdeal.Checked = Not PropPack.VaporPhaseFugacityCalculationMode

        chkIgnoreIPs.Checked = PropPack.ActivityCoefficientModels_IgnoreMissingInteractionParameters

        chkIgnoreSalLim.Checked = PropPack.IgnoreSalinityLimit

        chkIgnoreVapFracLim.Checked = PropPack.IgnoreVaporFractionLimit

        chkCalcAdditionalProps.Checked = PropPack.CalculateAdditionalMaterialStreamProperties

        tbSPCheckThres.Text = PropPack.SingleCompoundCheckThreshold

        chkAutoEstimateNU.Checked = PropPack.AutoEstimateMissingNRTLUNIQUACParameters

        chkUseHenry.Checked = PropPack.UseHenryConstants

        cbEOSLiqEnthMethod.SelectedIndex = PropPack.LiquidEnthalpyEntropyCpCvCalculationMode_EOS

        AddHandler tbSPCheckThres.TextChanged, Sub()
                                                   Try
                                                       PropPack.SingleCompoundCheckThreshold = tbSPCheckThres.Text.ToDoubleFromCurrent()
                                                       tbSPCheckThres.ForeColor = System.Drawing.Color.Blue
                                                   Catch ex As Exception
                                                       tbSPCheckThres.ForeColor = System.Drawing.Color.Red
                                                   End Try
                                               End Sub

        AddHandler cbLiqDens.SelectedIndexChanged, Sub()
                                                       PropPack.LiquidDensityCalculationMode_Subcritical = cbLiqDens.SelectedIndex
                                                       PropPack.LiquidDensityCalculationMode_Supercritical = cbLiqDens.SelectedIndex
                                                   End Sub

        AddHandler cbLiqVisc.SelectedIndexChanged, Sub()
                                                       PropPack.LiquidViscosityCalculationMode_Subcritical = cbLiqVisc.SelectedIndex
                                                       PropPack.LiquidViscosityCalculationMode_Supercritical = cbLiqVisc.SelectedIndex
                                                   End Sub

        AddHandler cbLiqVIscMixRule.SelectedIndexChanged, Sub()
                                                              PropPack.LiquidViscosity_MixingRule = cbLiqVIscMixRule.SelectedIndex
                                                          End Sub

        AddHandler cbHSCpCalcMode.SelectedIndexChanged, Sub()
                                                            PropPack.EnthalpyEntropyCpCvCalculationMode = cbHSCpCalcMode.SelectedIndex
                                                        End Sub

        AddHandler cbEOSLiqEnthMethod.SelectedIndexChanged, Sub()
                                                                PropPack.LiquidEnthalpyEntropyCpCvCalculationMode_EOS = cbEOSLiqEnthMethod.SelectedIndex
                                                            End Sub

        AddHandler chkIgnoreIPs.CheckedChanged, Sub()
                                                    PropPack.ActivityCoefficientModels_IgnoreMissingInteractionParameters = chkIgnoreIPs.Checked
                                                End Sub

        AddHandler chkIgnoreSalLim.CheckedChanged, Sub()
                                                       PropPack.IgnoreSalinityLimit = chkIgnoreSalLim.Checked
                                                   End Sub

        AddHandler chkIgnoreVapFracLim.CheckedChanged, Sub()
                                                           PropPack.IgnoreVaporFractionLimit = chkIgnoreVapFracLim.Checked
                                                       End Sub

        AddHandler chkLiqDensPCorr.CheckedChanged, Sub()
                                                       PropPack.LiquidDensity_CorrectExpDataForPressure = chkLiqDensPCorr.Checked
                                                   End Sub

        AddHandler chkLiqDensPeneloux.CheckedChanged, Sub()
                                                          PropPack.LiquidDensity_UsePenelouxVolumeTranslation = chkLiqDensPeneloux.Checked
                                                      End Sub

        AddHandler chkLiqFugPoynt.CheckedChanged, Sub()
                                                      PropPack.LiquidFugacity_UsePoyntingCorrectionFactor = chkLiqFugPoynt.Checked
                                                  End Sub

        AddHandler chkLiqViscPCorr.CheckedChanged, Sub()
                                                       PropPack.LiquidViscosity_CorrectExpDataForPressure = chkLiqViscPCorr.Checked
                                                   End Sub

        AddHandler chkVapFugIdeal.CheckedChanged, Sub()
                                                      PropPack.VaporPhaseFugacityCalculationMode = Not chkVapFugIdeal.Checked
                                                  End Sub

        AddHandler chkUseSolidCp.CheckedChanged, Sub()
                                                     PropPack.SolidPhaseEnthalpy_UsesCp = chkUseSolidCp.Checked
                                                 End Sub

        AddHandler chkCalcAdditionalProps.CheckedChanged, Sub()
                                                              PropPack.CalculateAdditionalMaterialStreamProperties = chkCalcAdditionalProps.Checked
                                                          End Sub

        AddHandler chkUseHenry.CheckedChanged, Sub()
                                                   PropPack.UseHenryConstants = chkUseHenry.Checked
                                               End Sub

        AddHandler chkAutoEstimateNU.CheckedChanged, Sub()
                                                         PropPack.AutoEstimateMissingNRTLUNIQUACParameters = chkAutoEstimateNU.Checked
                                                     End Sub

    End Sub

    Private Sub LinkLabel1_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs)
        Process.Start("https://github.com/DanWBR/dwsim/blob/windows/DWSIM.Thermodynamics/Assets/henry.txt")
    End Sub

End Class
