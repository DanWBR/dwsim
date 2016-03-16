    'Contains routines based on Seawater Ice Air (SIA) Version 3.01.2 (21 August 2015)
    'Version history: http://www.teos-10.org/sia_version_history.html
    'Website: http://www.teos-10.org/software.htm

    'License for the use of the Gibbs SeaWater (GSW) Oceanographic Toolbox

    'Copyright (c) 2011, SCOR/IAPSO WG127 (Scientific Committee on Oceanic Research/ International Association for the Physical Sciences of the Oceans, Working Group 127).

    'All rights reserved.

    'Redistribution and use, in source and binary forms, without modification, is permitted provided that the following conditions are met:

    '• Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

    '• Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation 
    'and/or other materials provided with the distribution.

    '• Neither the name of SCOR/IAPSO WG127 nor the names of its contributors may be used to endorse or promote products derived from this software without 
    'specific prior written permission.

    'THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, 
    'THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS 
    'BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE 
    'GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
    'STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    'The software is available from http://www.TEOS-10.org

    '================================================================================================================

    'Contains routines based on SEAWATER THERMOPHYSICAL PROPERTIES LIBRARY

    '% Version 2.0   2012-06-06
    '% Version 1.0   2009-12-18
    '%
    '%
    '%           ***********************************************
    '%           *               SEAWATER PROPERTIES           *
    '%           *                                             *
    '%           *        John H. Lienhard V, Ph.D., P.E.      *
    '%           * Collins Professor of Mechanical Engineering *
    '%           *           MechE Executive Officer           *
    '%           *    Director, Center for Clean Water and     *
    '%           *       Clean Energy at MIT and KFUPM         *
    '%           *    MASSACHUSETTS INSTITUTE OF TECHNOLOGY    *
    '%           *  77 Massachusetts Ave., Cambridge MA 02139  *
    '%           *              lienhard@mit.edu               *
    '%           *                                             *
    '%           *           Mostafa H. Sharqawy, Ph.D.        *
    '%           *            Postdoctoral  Associate          *
    '%           *     Department of Mechanical Engineering    *
    '%           *    Massachusetts Institute of  Technology   *
    '%           *                mhamed@mit.edu               *
    '%           *                                             *
    '%           ***********************************************

    Imports System.Math

    Namespace DWSIM.SimulationObjects.PropertyPackages.Auxiliary

    <System.Serializable> Public Class Seawater

        Sub New()

        End Sub

#Region "Constants_0"

        '#########################################################################

        'This module is self-contained and does not require other library modules

        '#########################################################################


        'This modules contains the definitions of basic constants of the library

        'The values of these constants are consistent with the functions and
        'constants used in this source code library, rather than being independent entities.
        'Future updates are only possible if this consistency is carefully maintained


        'Impose necessary parameter limits
        'For the check, initialize initial_check_limits_value to 1. It is used internally
        'to turn all limit checking off within the iterative solution routines:
        'Public Const initial_check_limits_value = 1
        'To suppress all restrictive checks, initialize initial_check_limits_value to zero:
        Public Const initial_check_limits_value = 0

        Private check_limits_value As Integer 'this is initialized in Property Get check_limits()

        'parameter ranges used for check_limits:
        Public Const dry_air_tmin = 60.0#
        Public Const dry_air_tmax = 873.0#
        Public Const dry_air_dmin = 0.0#
        Public Const dry_air_dmax = 1035.0#

        Public Const mix_air_tmin = 193.0#
        Public Const mix_air_tmax = 473.0#
        Public Const mix_air_dmin = 0.0#
        Public Const mix_air_dmax = 100.0#

        Public Const flu_tmin = 50.0#
        Public Const flu_tmax = 1273.0#
        Public Const flu_dmin = 0.0#
        Public Const flu_dmax = 1240.0#

        Public Const ice_tmin = 0.0#
        Public Const ice_tmax = 273.16
        Public Const ice_pmin = 0.0#
        Public Const ice_pmax = 200000000.0#

        Public Const sal_tmin = 262.0#
        Public Const sal_tmax = 380.0# ' 353.0#
        Public Const sal_pmin = 100.0#
        Public Const sal_pmax = 100000000.0#
        Public Const sal_smin = 0.0#
        Public Const sal_smax = 0.12

        'To extend the Gibbs function as described in these two papers:

        'R. Feistel
        'Extended Equation of State for Seawater at Elevated Temperature and Salinity
        'Desalination, 250, 14-18, 2010
        '
        'to cover the measurements of
        'F. Millero and F. Huang
        'Ocean Sci. Discuss. 6 (2009) 153-169.
        'http://www.ocean-sci-discuss.net/6/153/2009/

        'set this constant to True:
        'Public Const IsExtension2010 = True
        'Note that this optional Gibbs function is not endorsed by IAPWS
        Public Const IsExtension2010 = False

        'procedure return codes
        Public Const ErrorReturn = Double.PositiveInfinity
        Public Const IsOK = -1

        'Mathematical constants
        Public Const PI = 3.14159265358979

        'Fundamental physical constants
        Public Const Gas_constant_molar_si = 8.314472                  'molar gas constant J mol-1 K-1, IAPWS 2005
        Public Const Gas_constant_molar_L2000 = 8.31451                'molar gas constant J mol-1 K-1, Lemmon et al. 2000

        Public Const Molar_mass_H2O_si = 0.018015268                   'molar mass of H2O in kg mol-1,      IAPWS 2009
        Public Const Molar_mass_seasalt_si = 0.0314038218              'molar mass of sea salt in kg mol-1, Millero et al. 2008
        Public Const Molar_mass_air_si = 0.02896546                    'molar mass of dry air in kg mol-1,  Picard et al. 2008

        'Version 1.0:
        'Public Const Molar_mass_air_L2000 = 0.0289586                  'molar mass of dry air in kg mol-1,  Lemmon et al. 2000

        'Version 1.1:
        'Lemmon et al. (2000) decided to use the latest molar mass with their molar formulation.
        'For consistency with IAPWS-10, the SIA libary version 1.1 is also changed to this value
        '27 May 2010
        Public Const Molar_mass_air_L2000 = Molar_mass_air_si

        Public Const Gas_constant_H2O_si = Gas_constant_molar_si / Molar_mass_H2O_si            'specific gas constant of H2O in J kg-1 K-1, IAPWS 2005
        Public Const Gas_constant_H2O_IAPWS95 = 461.51805                                       'specific gas constant of H2O in J kg-1 K-1, IAPWS-95
        Public Const Gas_constant_air_si = Gas_constant_molar_si / Molar_mass_air_si            'specific gas constant of air in J kg-1 K-1, Picard et al. 2008
        Public Const Gas_constant_air_L2000 = Gas_constant_molar_L2000 / Molar_mass_air_L2000   'specific gas constant of air in J kg-1 K-1, Lemmon et al. 2000

        Public Const Sealevel_pressure_si = 101325                     'Pa
        Public Const Celsius_temperature_si = 273.15                   'K,          ITS-90 definition

        'Standard ocean properties, Millero et al. 2008
        Public Const SO_salinity_si = 0.03516504                       'kg kg-1,    of KCl-normalised seawater, Millero et al. 2008
        Public Const SO_temperature_si = Celsius_temperature_si        'K
        Public Const SO_pressure_si = Sealevel_pressure_si             'Pa

        'Critical point of pure water, IAPWS-95
        Public Const CP_density_si = 322.0#                              'kg m-3
        Public Const CP_temperature_si = 647.096                       'K
        Public Const CP_pressure_si = 22064000.0#                        'Pa
        Public Const CP_chempot_si = -767471.156792841                 'in J kg-1

        'Triple Point of pure water
        Public Const TP_temperature_si = 273.16                                   'K,      ITS-90 definition
        Public Const TP_pressure_exp_si = 611.657                                 'Pa,     experimental triple point pressure, IAPWS-95
        'Public Const TP_pressure_IAPWS95_si =611.6547710078944264442598E-6        Pa,     from quadruple-precision computation, Feistel et al. 2008
        Public Const TP_pressure_IAPWS95_si = 611.654771007894                    'Pa
        'Public Const TP_density_vap_IAPWS95_si =4.854575724778588417176210E-3     kg m-3, from quadruple-precision computation, Feistel et al. 2008
        Public Const TP_density_vap_IAPWS95_si = 0.00485457572477859             'kg m-3
        'Public Const TP_density_liq_IAPWS95_si =999.792520031620646603898354735   kg m-3, from quadruple-precision computation, Feistel et al. 2008
        Public Const TP_density_liq_IAPWS95_si = 999.792520031621                 'kg m-3
        Public Const TP_density_ice_IAPWS95_si = 916.709492199488                 'kg m-3
        Public Const TP_enthalpy_vap_si = 2500915.1914657                         'J kg-1
        Public Const TP_enthalpy_ice_si = -333444.253967839                       'J kg-1

        'References:

        'Feistel et al. 2008:
        'Feistel, R., Wright, D.G., Miyagawa, K., Harvey, A.H., Hruby, J., Jackett, D.R., McDougall, T.J. and Wagner, W.:
        'Mutually Consistent Thermodynamic Potentials for Fluid Water, Ice, and Seawater: A New Standard for Oceanography,
        'Ocean Sci., 4, 275-291, 2008, available at: www.ocean-sci.net/4/275/2008/

        'IAPWS-95:
        'Release on the IAPWS Formulation 1995 for the Thermodynamic Properties of Ordinary Water Substance for General and Scientific Use
        'The International Association for the Properties of Water and Steam
        'Doorwerth, The Netherlands, September 2009
        '(original Release: Fredericia, Denmark, September 1996)
        'available at:  http://www.iapws.org

        'IAPWS 2005:
        'IAPWS: Guideline on the Use of Fundamental Physical Constants and Basic Constants of Water.
        'The International Association for the Properties of Water and Steam. Gaithersburg, Mayland, USA, September 2001.
        'Revision July 2005, available at: http://www.iapws.org

        'Lemmon et al. 2000:
        'Lemmon, E.W., Jacobsen, R.T., Penoncello, S.G. and Friend, D.G.:
        'Thermodynamic Properties of Air and Mixtures of Nitrogen, Argon and Oxygen From 60 to 2000 K at Pressures to 2000 MPa,
        'J. Phys. Chem. Ref. Data, 29, 331-362, 2000

        'Millero et al. 2008:
        'Millero, F.J., Feistel, R., Wright, D.G. and McDougall, T.J.:
        'The composition of Standard Seawater and the definition of the Reference-Composition Salinity Scale,
        'Deep-Sea Res. I, 55, 50-72, 2008

        'Picard et al. 2008:
        'Picard, A., Davis, R.S., Gläser, M. and Fujii, K.:
        'Revised formula for the density of moist air (CIPM-2007),
        'Metrologia, 45, 149-155, 2008

        'IAPWS: Guideline on an Equation of State for Humid Air in Contact with Seawater and Ice,
        'Consistent with the IAPWS Formulation 2008 for the Thermodynamic Properties of Seawater.
        'The International Association for the Properties of Water and Steam,
        'Niagara Falls, Canada, September 2010, to be adopted

        Private initialized As Integer

        Private Const Version = "27 May 2010"

        Public Property check_limits() As Integer
            Get
                If initialized <> 1 Then 'initialize only once
                    initialized = 1
                    'this initializes a more restrictive checking of validity ranges
                    'during the iteration loops of several modules
                    check_limits_value = initial_check_limits_value
                End If
                check_limits = check_limits_value
            End Get
            Set(value As Integer)
                check_limits_value = value
                initialized = 1        'do not overwrite later this setting by initializing
            End Set
        End Property

#End Region

#Region "Convert_0"

        '#########################################################################

        'This module requires the library module
        '     Constants_0_Mdl, file Constants_0.bas

        '#########################################################################


        'This module implements conversion formulas between different scales and units

        'Implementation of the TEOS-10 SIA Library in VB6 by Rainer Feistel
        'for publication in Ocean Science, as described in the papers

        'Feistel, R., Wright, D.G., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part I: Background and Equations.
        'Ocean Science 6, 633–677, 2010, www.ocean-sci.net/6/633/2010/

        'Wright, D.G., Feistel, R., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part II: The Library Routines,
        'Ocean Science 6, 695-718, 2010, www.ocean-sci.net/6/695/2010/

        'SIA Code updates available at www.teos-10.org


        'verison update to SIA v. 3.0 as of 23 Feb 2011:

        'SIA v. 3.0 reads the file "gsw_data_v3_0.dat"


        'Private Const ErrorReturn = 9.99999999E+98

        Private Const mw = Molar_mass_H2O_si       'molar mass of H2O in kg/mol
        Private Const ma = Molar_mass_air_L2000    'molar mass of air in kg/mol used by Lemmon et al. 2000

        ''' <summary>
        ''' returns the molar mass of humid air in kg mol-1 as a function of the air mass fraction, a_si, in kg/kg
        ''' </summary>
        ''' <param name="a_si"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>

        Public Function air_molar_mass_si(ByVal a_si As Double) As Double

            'check value:
            'air_molar_mass_si(0.5) = 2.22122197773792E-02  v. 1.0
            'air_molar_mass_si(0.5) = 2.22142374908826E-02  v. 1.1

            air_molar_mass_si = ErrorReturn

            If a_si < 0 Or a_si > 1.0# Then Exit Function

            air_molar_mass_si = 1.0# / ((1.0# - a_si) / mw + a_si / ma)

        End Function

        ''' <summary>
        ''' returns the mole fraction of vapour in humid air in mol mol-1
        ''' as a function of the air mass fraction, a_si, in kg/kg
        ''' </summary>
        ''' <param name="a_si"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function air_molfraction_vap_si(ByVal a_si As Double) As Double

            'returns the mole fraction of vapour in humid air in mol mol-1
            'as a function of the air mass fraction, a_si, in kg/kg

            'check value:
            'air_molfraction_vap_si(0.5) = 0.616483190185658   v. 1.0
            'air_molfraction_vap_si(0.5) = 0.616539190282449   v. 1.1

            air_molfraction_vap_si = ErrorReturn

            If a_si < 0 Or a_si > 1.0# Then Exit Function

            air_molfraction_vap_si = (1.0# - a_si) / (1.0# - a_si * (1.0# - mw / ma))

        End Function

        ''' <summary>
        ''' returns the mole fraction of air in humid air in mol mol-1
        ''' as a function of the air mass fraction, a_si, in kg/kg
        ''' </summary>
        ''' <param name="a_si"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function air_molfraction_air_si(ByVal a_si As Double) As Double

            'returns the mole fraction of air in humid air in mol mol-1
            'as a function of the air mass fraction, a_si, in kg/kg

            'check value:
            'air_molfraction_air_si(0.5) = 0.383516809814342  v. 1.0
            'air_molfraction_air_si(0.5) = 0.383460809717551  v. 1.1

            air_molfraction_air_si = ErrorReturn

            If a_si < 0 Or a_si > 1.0# Then Exit Function

            air_molfraction_air_si = (a_si * mw / ma) / (1.0# - a_si * (1.0# - mw / ma))

        End Function

        ''' <summary>
        ''' returns the mass fraction a_si in kg/kg of air in humid air as a function of the mole fraction x_si in mol/mol of air in humid air
        ''' </summary>
        ''' <param name="x_si"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function air_massfraction_air_si(ByVal x_si As Double) As Double

            'returns the mass fraction a_si in kg/kg of air in humid air as a function of the mole fraction x_si in mol/mol of air in humid air

            'check value:
            'air_massfraction_air_si(0.5) = 0.616483190185658  v. 1.0
            'air_massfraction_air_si(0.5) = 0.616539190282449  v. 1.1

            air_massfraction_air_si = ErrorReturn

            If x_si < 0 Or x_si > 1.0# Then Exit Function

            air_massfraction_air_si = x_si / (1.0# - (1.0# - x_si) * (1.0# - mw / ma))

        End Function

        ''' <summary>
        ''' returns the mass fraction of vapour in humid air in kg/kg as a function of the mole fraction of air, x_si, in mol mol-1
        ''' </summary>
        ''' <param name="x_si"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function air_massfraction_vap_si(ByVal x_si As Double) As Double

            'returns the mass fraction of vapour in humid air in kg/kg as a function of the mole fraction of air, x_si, in mol mol-1

            'check value:
            'air_massfraction_vap_si(0.5) = 0.383516809814342  v. 1.0
            'air_massfraction_vap_si(0.5) = 0.383460809717551  v. 1.1

            air_massfraction_vap_si = ErrorReturn

            If x_si < 0 Or x_si > 1.0# Then Exit Function

            air_massfraction_vap_si = (1.0# - x_si) / (1.0# - x_si * (1.0# - ma / mw))

        End Function

        ' THIS IS THE GSW ABSOLUTE SALINITY ALGORITHM TRANSLATED FROM FORTRAN TO VB

        '
        ' AS DEFINED IN
        '
        ' MCDOUGALL, T.J., JACKETT, D.R. AND MILLERO, F.J., 2009:
        ' AN ALGORITHM FOR ESTIMATING ABSOLUTE SALINITY IN THE GLOBAL OCEAN,
        ' Ocean Sci. Discuss., 6, 215-242, 2009
        '
        '
        ' DAVID JACKETT
        ' JANUARY 2009
        ' adapted by D.G.Wright and translated to VB by R. Feistel
        '

        Public Function asal_from_psal(ByVal sp As Double, _
                                    ByVal lon0 As Double, _
                                    ByVal lat0 As Double, _
                                    ByVal p_si As Double) As Double


            On Error GoTo Errortrap

            ' CONVERT PRACTICAL SALINITY TO ABSOLUTE SALINITY
            '
            ' SP                  : PRACTICAL SALINITY                 [PSU]
            ' P_SI                : ABSOLUTE PRESSURE                  [PA]
            ' LON0                : LONGITUDE                          [DEG E]
            ' LAT0                : LATITUDE                           [DEG N]
            '
            ' asal_from_psal      : ABSOLUTE SALINITY                  [KG/KG]


            ' SIA Version 3.0:
            ' code translated to VB by R. Feistel, 22 Feb 2011,
            ' from:
            ' F90 code modified 1st February 2011, by Paul Barker.
            ' This version of the GSW software is comparible to the
            ' GSW Oceanographic Toolbox version 3.0


            'CHECK VALUE (version 3.0)
            'asal_from_psal(35.527515065427778, 201, -21, 101325 + 10230000#) = 0.0357
            'asal_from_psal(35, 180, 40, 101325 + 20000000#) = 3.51890932889958E-02  'north Pacific silicate
            'asal_from_psal(8, 20, 57, 101325) = 8.10483771428571E-03                'central Baltic Sea


            'CHECK VALUE (obsolete versions 1.0 and 2.0)
            'asal_from_psal(35.5276443777339, 201, -21, 101325 + 10230000#) = 3.57000000000001E-02
            'asal_from_psal(35, 180, 40, 101325 + 20000000#) = 3.51888478029404E-02 'north Pacific silicate
            'asal_from_psal(8, 20, 57, 101325) = 8.13338057142857E-03               'central Baltic Sea

            'if the file "gsw_data.dat" is missing, SA = SR is computed in the ocean:
            'asal_from_psal(35.5276443777339, 201, -21, 101325 + 10230000#) = 3.56951724471082E-02
            'asal_from_psal(35, 180, 40, 101325 + 20000000#) = 0.03516504            'north Pacific (without silicate)
            'asal_from_psal(8, 20, 57, 101325) = 8.13338057142857E-03                'central Baltic Sea


            Dim flag&
            Dim sa#, p_dbar#, saar#

            asal_from_psal = ErrorReturn

            p_dbar = (p_si - 101325.0#) / 10000.0#

            'versions 1.0, 2.0:
            'sa = (35.16504 / 35#) * sp + gsw_delta_sa(longs0, lats0, P0)

            'version 3.0:
            saar = gsw_saar(lon0, lat0, p_dbar)
            If saar = ErrorReturn Then Exit Function

            sa = (35.16504 / 35.0#) * sp * (1.0# + saar)

            flag = 1
            asal_from_psal = 0.001 * gsw_adjust_baltic(sa, sp, lon0, lat0, flag)
            Exit Function

Errortrap:
        End Function


        Public Function psal_from_asal(ByVal sa_si As Double, _
                                    ByVal lon0 As Double, _
                                    ByVal lat0 As Double, _
                                    ByVal p_si As Double) As Double


            On Error GoTo Errortrap

            ' CONVERT ABSOLUTE SALINITY TO PRACTICAL SALINITY
            '
            ' SA_SI               : ABSOLUTE SALINITY                  [KG/KG]
            ' P_SI                : ABSOLUTE PRESSURE                  [PA]
            ' LON0                : LONGITUDE                          [DEG E]
            ' LAT0                : LATITUDE                           [DEG N]
            '
            ' PSAL_FROM_ASAL      : PRACTICAL SALINITY                 [PSU]


            ' SIA Version 3.0:
            ' code translated to VB by R. Feistel, 22 Feb 2011,
            ' from:
            ' F90 code modified 1st February 2011, by Paul Barker.
            ' This version of the GSW software is comparible to the
            ' GSW Oceanographic Toolbox version 3.0


            'CHECK VALUE (version 3.0)
            'psal_from_asal(0.0357, 201, -21, 101325 + 10230000#) = 35.5275150654278


            'CHECK VALUE (obsolete versions 1.0 and 2.0)
            'psal_from_asal(0.0357, 201, -21, 101325 + 10230000#) = 35.5276443777339

            'if the file "gsw_data.dat" is missing, SA = SR is computed in the ocean:
            'psal_from_asal(0.0357, 201, -21, 101325 + 10230000#) = 35.532449273483


            Dim flag&
            Dim sa#, sp#, p_dbar#, saar#

            psal_from_asal = ErrorReturn

            p_dbar = (p_si - 101325.0#) / 10000.0#
            sa = 1000.0# * sa_si

            'version 1.0, 2.0:
            'psal = (35# / 35.16504) * (sa - gsw_delta_sa(longs0, lats0, P0))

            'version 3.0:
            saar = gsw_saar(lon0, lat0, p_dbar)
            If saar = ErrorReturn Then Exit Function

            sp = (35.0# / 35.16504) * sa / (1.0# + saar)

            flag = 2
            psal_from_asal = gsw_adjust_baltic(sa, sp, lon0, lat0, flag)
            Exit Function

Errortrap:
        End Function


        Private Function gsw_saar(ByVal lon0_in As Double, _
                                ByVal lat0 As Double, _
                                ByVal P0 As Double) As Double



            ' SIA Version 3.0:
            ' code translated to VB by R. Feistel, 22 Feb 2011,
            ' from:
            ' F90 code modified 1st February 2011, by Paul Barker.
            ' This version of the GSW software is comparible to the
            ' GSW Oceanographic Toolbox version 3.0


            ' CALCULATE THE ABSOLUTE SALINITY ANOMALY RATIO
            '
            ' P0                  : ABSOLUTE pressure                  [DBAR]
            ' LON0                : LONGITUDE                          [DEG E]
            ' LAT0                : LATITUDE                           [DEG N]
            '
            ' RESULT              : ABSOLUTE SALINITY ANOMALY RATIO    [unitless]

            Const nx = 91, ny = 45, nz = 45

            Dim indx0&, indy0&, indz0&, i&, j&
            Dim k&, deli&(4), delj&(4), nmean&
            Dim p0_original#, lon0#, lonint&, sa_upper#, sa_lower#
            Dim dlon#, dlat#, r1#, s1#, t1#, saar_mean#, saar#(4), ndepth_max#

            Static lons#(nx), lats#(ny), p#(nz), saar_ref#(nz, ny, nx), ndepth#(ny, nx), icalled

            On Error GoTo Errortrap

            'data deli/0,1,1,0/, delj/0,0,1,1/
            deli(2) = 1
            deli(3) = 1
            delj(3) = 1
            delj(4) = 1

            'data icalled/0/
            'icalled = 0

            'save icalled, lons, lats, p, ndepth, saar_ref

            gsw_saar = 0.0#

            If (lat0 < -82.0# Or lat0 > 90.0#) Then Return Nothing
            lonint = Int(lon0_in)                       'integer part
            lon0 = lon0_in - lonint                     'fraction part
            lonint = ((lonint Mod 360) + 360) Mod 360  'results in 0 <= lonint < 360
            lon0 = lon0 + lonint
            If (lon0 >= 360.0#) Then lon0 = lon0 - 360.0# 'this should never happen, actually
            If (lon0 = 0.0#) Then lon0 = 0.000000000000001

            If (icalled = 0) Then
                icalled = 1
                If Read_gsw_data(nx, ny, nz, lons, lats, p, ndepth, saar_ref) = ErrorReturn Then
                    gsw_saar = 0
                    icalled = 0
                    'saar_ref = 0#
                    Exit Function
                End If
                'open(10,file='gsw_data_v3_0.dat',status='old',err=1)
                'flag_saar = 1
                'read(10,*) (lons(i), i=1,nx)
                'read(10,*) (lats(i), i=1,ny)
                'read(10,*) (p(i), i=1,nz)
                'read(10,*) ((ndepth(j,i), j=1,ny), i=1,nx)
                'read(10,*) (((saar_ref(k,j,i), k=1,nz), j=1,ny), i=1,nx)
                'Close (10)
                '   GoTo 2
                '1  saar_ref = 0#
                '   flag_saar = 0
                '2  continue
            End If

            'if (flag_saar.eq.0) then
            '   write(*,*) "*** gsw_data_v3_0.dat is missing ''' ***"
            '   write(*,*) "Set the full path of gsw_data_v3_0.dat in Convert_0.F90 on line 290"
            'End If

            'SET GSW_SAAR = 0 AND RETURN IF THERE IS NO DATA FILE PRESENT
            'if(flag_saar == 0) then; gsw_saar = 0d0; return; endif

            dlon = lons(2) - lons(1)
            dlat = lats(2) - lats(1)

            If lons(nx) = lons(1) Then Exit Function
            indx0 = Int(1 + (nx - 1) * (lon0 - lons(1)) / (lons(nx) - lons(1)))
            If (indx0 = nx) Then
                indx0 = nx - 1
            End If

            If lats(ny) = lats(1) Then Exit Function
            indy0 = Int(1 + (ny - 1) * (lat0 - lats(1)) / (lats(ny) - lats(1)))
            If (indy0 = ny) Then
                indy0 = ny - 1
            End If

            ndepth_max = -1
            For k = 1 To 4
                If (ndepth(indy0 + delj(k), indx0 + deli(k)) > 0) Then
                    ndepth_max = max(ndepth_max, ndepth(indy0 + delj(k), indx0 + deli(k)))
                End If
            Next k

            If (ndepth_max = -1.0#) Then
                gsw_saar = 0.0#
                Exit Function
            End If

            p0_original = P0
            If (P0 > p(Int(ndepth_max))) Then P0 = p(Int(ndepth_max))
            Call indx(p, nz, P0, indz0)

            r1 = (lon0 - lons(indx0)) / (lons(indx0 + 1) - lons(indx0))
            s1 = (lat0 - lats(indy0)) / (lats(indy0 + 1) - lats(indy0))
            t1 = (P0 - p(indz0)) / (p(indz0 + 1) - p(indz0))

            For k = 1 To 4
                saar(k) = saar_ref(indz0, indy0 + delj(k), indx0 + deli(k))
            Next k

            If (260.0# <= lon0 And lon0 <= 291.999 And _
                3.4 <= lat0 And lat0 <= 19.55) Then
                Call add_barrier(saar, saar, lon0, lat0, lons(indx0), lats(indy0), dlon, dlat)
            ElseIf (Abs(sum(saar)) >= 10000000000.0#) Then
                Call saar_add_mean(saar, lon0, lat0)
            End If

            sa_upper = (1.0# - s1) * (saar(1) + r1 * (saar(2) - saar(1))) + _
                                s1 * (saar(4) + r1 * (saar(3) - saar(4)))

            For k = 1 To 4
                saar(k) = saar_ref(indz0 + 1, indy0 + delj(k), indx0 + deli(k))
            Next k

            If (260.0# <= lon0 And lon0 <= 291.999 And _
                3.4 <= lat0 And lat0 <= 19.55) Then
                Call add_barrier(saar, saar, lon0, lat0, lons(indx0), lats(indy0), dlon, dlat)
            ElseIf (Abs(sum(saar)) >= 10000000000.0#) Then
                Call saar_add_mean(saar, lon0, lat0)
            End If

            sa_lower = (1.0# - s1) * (saar(1) + r1 * (saar(2) - saar(1))) + _
                                s1 * (saar(4) + r1 * (saar(3) - saar(4)))
            If (Abs(sa_lower) >= 10000000000.0#) Then sa_lower = sa_upper
            gsw_saar = sa_upper + t1 * (sa_lower - sa_upper)

            'dbg
            If (Abs(gsw_saar) >= 10000000000.0#) Then
                'write(*,*)"gsw_saar = errorreturn has been replaced by 0"
                gsw_saar = 0.0#
            End If
            'dbg
            P0 = p0_original

            Exit Function

Errortrap:
            gsw_saar = ErrorReturn

        End Function


        Private Function gsw_adjust_baltic(ByVal sa As Double, _
                                        ByVal sp As Double, _
                                        ByVal lons As Double, _
                                        ByVal lats As Double, _
                                        ByVal flag As Long) As Double


            ' FOR THE BALTIC SEA, OVERWRITE ABSOLUTE SALINITY WITH A VALUE
            ' COMPUTED ANALYTICALLY FROM PRACTICAL SALINITY, OR VICE VERSA
            '
            ' SA                  : ABSOLUTE SALINITY                  [G/KG]
            ' SP                  : PRACTICAL SALINITY                 [PSU]
            ' LONS                : LONGITUDE                          [DEG E]
            ' LATS                : LATITUDE                           [DEG N]
            ' FLAG                : FLAG - 1 or 2
            '
            ' GSW_ADJUST_BALTIC   : ABSOLUTE SALINITY                  [G/KG]
            '                         WHEN FLAG = 1
            '                     : PRACTICAL SALINITY                 [PSU]
            '                         WHEN FLAG = 2


            ' SIA Version 3.0:
            ' code translated to VB by R. Feistel, 22 Feb 2011,
            ' from:
            ' F90 code modified 1st February 2011, by Paul Barker.
            ' This version of the GSW software is comparible to the
            ' GSW Oceanographic Toolbox version 3.0


            Dim n2&, n3&
            Dim xb_left#(3), xb_right#(2), yb_left#(3), yb_right#(2), xx_left#, xx_right#

            'data xb_left/12.6d0, 7.d0, 26.d0/, yb_left/50.d0, 59.d0, 69.d0/
            xb_left(1) = 12.6
            xb_left(2) = 7.0#
            xb_left(3) = 26.0#
            yb_left(1) = 50.0#
            yb_left(2) = 59.0#
            yb_left(3) = 69.0#

            'data xb_right/45.d0, 26.d0/, yb_right/50.d0, 69.d0/
            xb_right(1) = 45.0#
            xb_right(2) = 26.0#
            yb_right(1) = 50.0#
            yb_right(2) = 69.0#

            n2 = 2
            n3 = 3

            If (flag = 1) Then
                gsw_adjust_baltic = sa
            ElseIf (flag = 2) Then
                gsw_adjust_baltic = sp
            End If

            If (xb_left(2) < lons And lons < xb_right(1) And yb_left(1) < lats And lats < yb_left(3)) Then
                xx_left = xinterp1(yb_left, xb_left, n3, lats)
                xx_right = xinterp1(yb_right, xb_right, n2, lats)
                If (xx_left <= lons And lons <= xx_right) Then
                    If (flag = 1) Then
                        'SIA Versions 1.0 and 2.0:
                        'gsw_adjust_baltic = (35.16504 / 35#) * sp + 0.124 * (1# - sp / 35#)

                        'SIA Version 3.0 of 11 Jan 2011:
                        gsw_adjust_baltic = ((35.16504 - 0.087) / 35.0#) * sp + 0.087
                    ElseIf (flag = 2) Then
                        'SIA Versions 1.0 and 2.0:
                        'gsw_adjust_baltic = (35# / 35.04104) * (sa - 0.124)

                        'SIA Version 3.0 of 11 Jan 2011:
                        gsw_adjust_baltic = (35.0# / (35.16504 - 0.087)) * (sa - 0.087)
                    End If
                End If
            End If

        End Function


        Private Sub add_barrier(ByRef saar_add_barrier() As Double, _
                            ByRef saar() As Double, _
                            ByVal lon0 As Double, _
                            ByVal lat0 As Double, _
                            ByVal lons As Double, _
                            ByVal lats As Double, _
                            ByVal dlon As Double, _
                            ByVal dlat As Double)



            ' SIA Version 3.0:
            ' code translated to VB by R. Feistel, 22 Feb 2011,
            ' from:
            ' F90 code modified 1st February 2011, by Paul Barker.
            ' This version of the GSW software is comparible to the
            ' GSW Oceanographic Toolbox version 3.0


            ' ADD A BARRIER THROUGH CENTRAL AMERICA (PANAMA) AND THEN AVERAGE
            ' OVER THE APPROPRIATE SIDE OF THE BARRIER
            '
            ' SAAR                : ABSOLUTE SALINITY ANOMALY RATIO          [UNITLESS]
            ' LON0                : LONGITUDES OF DATA                       [DEG E]
            ' LAT0                : LATITUDES OF DATA                        [DEG N]
            ' LONS                : LONGITUDES OF REGULAR GRID               [DEG E]
            ' LATS                : LATITUDES OF REGULAR GRID                [DEG N]
            ' DLON                : LONGITUDE DIFFERENCE OF REGULAR GRID     [DEG LONGITUDE]
            ' DLAT                : LATITUDES DIFFERENCE OF REGULAR GRID     [DEG LATITUDE]
            '
            ' RESULT              : ABSOLUTE SALINITY ANOMALY RATIO OF DATA  [UNITLESS]

            'integer n4, n6
            'parameter(n4=4, n6=6)
            Const n4 = 4
            Const n6 = 6

            Dim k&, nmean&, above_line&(n4), above_line0&
            Dim lons_pan#(n6), lats_pan#(n6), R#, lats_line#
            Dim saar_mean#

            'On Error GoTo Errortrap

            For k = 1 To n6
                'data lons_pan/260.0000, 272.5900, 276.5000, 278.6500, 280.7300, 292.000/
                lons_pan(k) = Choose(k, 260.0#, 272.59, 276.5, 278.65, 280.73, 292.0#)
                'data  lats_pan/ 19.5500,  13.9700,   9.6000,   8.1000,   9.3300,   3.400/
                lats_pan(k) = Choose(k, 19.55, 13.97, 9.6, 8.1, 9.33, 3.4)
            Next k

            Call indx(lons_pan, n6, lon0, k)                         '   the long0/lat0 point
            R = (lon0 - lons_pan(k)) / (lons_pan(k + 1) - lons_pan(k))
            lats_line = lats_pan(k) + R * (lats_pan(k + 1) - lats_pan(k))
            If (lats_line <= lat0) Then
                above_line0 = 1
            Else
                above_line0 = 0
            End If

            ''print *, 'above_line = ', above_line0

            Call indx(lons_pan, n6, lons, k)                                  '  the 1 and 4 long/lat points
            R = (lons - lons_pan(k)) / (lons_pan(k + 1) - lons_pan(k))
            lats_line = lats_pan(k) + R * (lats_pan(k + 1) - lats_pan(k))
            If (lats_line <= lats) Then
                above_line(1) = 1
            Else
                above_line(1) = 0
            End If
            If (lats_line <= lats + dlat) Then
                above_line(4) = 1
            Else
                above_line(4) = 0
            End If

            Call indx(lons_pan, n6, lons + dlon, k)                         '  the 2 and 3 long/lat points
            R = (lons + dlon - lons_pan(k)) / (lons_pan(k + 1) - lons_pan(k))
            lats_line = lats_pan(k) + R * (lats_pan(k + 1) - lats_pan(k))
            If (lats_line <= lats) Then
                above_line(2) = 1
            Else
                above_line(2) = 0
            End If
            If (lats_line <= lats + dlat) Then
                above_line(3) = 1
            Else
                above_line(3) = 0
            End If

            nmean = 0
            saar_mean = 0.0#
            For k = 1 To n4
                saar_add_barrier(k) = saar(k)
                If ((Abs(saar(k)) <= 100.0#) And above_line0 = above_line(k)) Then
                    nmean = nmean + 1
                    saar_mean = saar_mean + saar(k)
                End If
            Next k

            If (nmean = 0) Then
                saar_mean = 0
            Else
                saar_mean = saar_mean / nmean
            End If

            For k = 1 To n4
                If ((Abs(saar(k)) >= 10000000000.0#) Or above_line0 <> above_line(k)) Then
                    saar_add_barrier(k) = saar_mean
                End If
            Next k

Errortrap:
        End Sub


        Private Sub saar_add_mean(ByRef saar() As Double, _
                                ByVal lon0 As Double, _
                                ByVal lat0 As Double)



            ' SIA Version 3.0:
            ' code translated to VB by R. Feistel, 22 Feb 2011,
            ' from:
            ' F90 code modified 1st February 2011, by Paul Barker.
            ' This version of the GSW software is comparible to the
            ' GSW Oceanographic Toolbox version 3.0


            ' REPLACE NANS WITH NAMEAN
            '
            ' SAAR          : ABSOLUTE SALINITY ANOMALY RATIO          [UNITLESS]
            '                 OF THE 4 ADJACENT NEIGHBOURS
            '
            ' RESULT        : NANMEAN OF THE 4 ADJACENT NEIGHBOURS     [UNITLESS]


            Dim k&, nmean&
            Dim saar_mean#

            nmean = 0
            saar_mean = 0.0#

            For k = 1 To 4
                If (Abs(saar(k)) <= 100.0#) Then
                    nmean = nmean + 1
                    saar_mean = saar_mean + saar(k)
                End If
            Next k

            If (nmean = 0) Then
                saar_mean = 0
            Else
                saar_mean = saar_mean / nmean
            End If

            For k = 1 To 4
                If (Abs(saar(k)) >= 100.0#) Then
                    saar(k) = saar_mean
                End If
            Next k

        End Sub


        Private Sub indx(ByRef x() As Double, _
                        ByVal n As Long, _
                        ByVal z As Double, _
                        ByRef k As Long)


            'DESCRIPTION: FIND THE INDEX OF A REAL NUMBER IN A
            '   MONOTONICALLY INCREASING REAL ARRAY
            '
            'INPUT:           X   ARRAY OF INCREASING VALUES
            '                 N   LENGTH OF ARRAY
            '           Z   REAL NUMBER
            '
            'OUTPUT:    K   INDEX K - IF X(K) <= Z < X(K+1), OR
            '       N-1         - IF Z = X(N)
            '
            'CREATED:   JUNE 1993
            '

            Dim ku&, kl&, km&

            If (x(1) < z And z < x(n)) Then
                kl = 1
                ku = n
                Do While (ku - kl > 1)
                    km = (ku + kl) / 2
                    If (z > x(km)) Then
                        kl = km
                    Else
                        ku = km
                    End If
                Loop
                k = kl
                If (z = x(k + 1)) Then k = k + 1
            Else
                If (z = x(1)) Then
                    k = 1
                ElseIf (z = x(n)) Then
                    k = n - 1
                Else
                    Throw New Exception("ERROR in indx.f : out of range" + Chr(13) + _
                    "z=" + Str(z) + ", n=" + Str(n) + ", x(1)=" + Str(x(1)) + ", x(n)=" + Str(x(n)))
                    'print *, z,n,x
                    'pause
                End If
            End If

        End Sub


        Private Function xinterp1(ByRef x() As Double, _
                                ByRef y() As Double, _
                                ByVal n As Long, _
                                ByVal x0 As Double) As Double


            ' LINEARLY INTERPOLATE A REAL ARRAY
            '
            ' X                   : X ARRAY (MONOTONIC)
            ' Y                   : Y ARRAY
            ' N                   : LENGTH OF X AND Y ARRAYS
            ' X0                  : X VALUE TO BE INTERPOLATED
            '
            ' RESULT              : INTERPOLATED VALUE

            Dim k&
            Dim R#

            indx(x, n, x0, k)
            R = (x0 - x(k)) / (x(k + 1) - x(k))
            xinterp1 = y(k) + R * (y(k + 1) - y(k))

        End Function


        Private Function max(ByVal a As Double, ByVal b As Double) As Double


            max = IIf(a > b, a, b)

        End Function

        Private Function sum(ByRef a() As Double) As Double

            Dim S As Double, i As Long

            For i = LBound(a) To UBound(a)
                S = S + a(i)
            Next i
            sum = S

        End Function


        Private Function Read_gsw_data(ByVal nx As Long, _
                                    ByVal ny As Long, _
                                    ByVal nz As Long, _
                                    ByRef lon() As Double, _
                                    ByRef lat() As Double, _
                                    ByRef pres() As Double, _
                                    ByRef ndpt(,) As Double, _
                                    ByRef dels(,,) As Double) As Double

            Dim i&, j&, k&

            'SIA version 3.0:
            'f = f + "gsw_data_v3_0.dat"

            'SIA version 1.0 and 2.0 (SIA 2.0 is identical with SIA 1.1)
            'f = f + "gsw_data.dat"

            Dim pathsep As Char = System.IO.Path.DirectorySeparatorChar
            Dim filecontents As String() = IO.File.ReadAllLines(My.Application.Info.DirectoryPath & pathsep & "data" & pathsep & "gsw_data_v3_0.dat")

            j = 0

            For i = 1 To nx
                lon(i) = Double.Parse(filecontents(j), System.Globalization.CultureInfo.InvariantCulture)
                j += 1
            Next i

            For i = 1 To ny
                lat(i) = Double.Parse(filecontents(j), System.Globalization.CultureInfo.InvariantCulture)
                j += 1
            Next i

            For i = 1 To nz
                pres(i) = Double.Parse(filecontents(j), System.Globalization.CultureInfo.InvariantCulture)
                j += 1
            Next i

            For i = 1 To nx
                For j = 1 To ny
                    ndpt(j, i) = Double.Parse(filecontents(j), System.Globalization.CultureInfo.InvariantCulture)
                    j += 1
                Next j
            Next i

            For i = 1 To nx
                For j = 1 To ny
                    For k = 1 To nz
                        dels(k, j, i) = Double.Parse(filecontents(j), System.Globalization.CultureInfo.InvariantCulture)
                        j += 1
                    Next k
                Next j
            Next i

        End Function

#End Region

#Region "Convert_5"

        '#########################################################################

        'This module requires the library module
        '     Constants_0_Mdl, file Constants_0.bas
        '     Convert_0_Mdl,   file Convert_0.bas

        '#########################################################################


        'This module implements conversion formulas between different scales and units

        'Implementation in VB6 by Rainer Feistel
        'for publication in Ocean Science, as described in the papers

        'Feistel, R., Wright, D.G., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part I: Background and Equations.
        'Ocean Science, 2009

        'Wright, D.G., Feistel, R., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part II: The Library Routines,
        'Ocean Science, 2009


        'Private Const ErrorReturn = 9.99999999E+98


        Public Function cnv_pressure(ByVal unit_out As String, _
                                    ByVal p_in As Double, _
                                    ByVal unit_in As String, _
                        Optional ByVal lat As Double = 45.0#) As Double

            'NOTE: UNIT_IN AND UNIT_OUT ARE CASE SENSITIVE. mPa IS NOT THE SAME AS MPA.
            '      ALL UNITS MUST BE IN CAPITAL LETTERS

            'check values:
            'cnv_pressure("MPa",4000,"dbar") = 40.101325

            'cnv_pressure("DBAR", 10331325, "PA") = 1023#
            'cnv_pressure("PA", 10331325, "DBAR") = 10331325#
            'cnv_pressure("TORR", 10331325, "PA") = 77491.3101406
            'cnv_pressure("PA", 77491.3101406, "TORR") = 10331325#
            'cnv_pressure("KGF", 10331325, "PA") = 105350.19604
            'cnv_pressure("PA", 105350.19604, "KGF") = 10331325#
            'cnv_pressure("ATM", 10331325, "PA") = 105350.19604
            'cnv_pressure("PA", 105350.19604, "ATM") = 10331325#
            'cnv_pressure("LBF/IN2", 10331325, "PA") = 1498.42272437
            'cnv_pressure("PA", 1498.42272437, "LBF/IN2") = 10331325#
            'cnv_pressure("PSI", 10331325, "PA") = 1498.42272437
            'cnv_pressure("PA", 1498.42272437, "PSI") = 10331325#
            'cnv_pressure("MPA", 4000, "DBAR") = 40.101325
            'cnv_pressure("M", 1023#, "DBAR") = 1011.94563591
            'cnv_pressure("DBAR", 1011.94563591, "M") = 1023

            Dim p_si As Double

            If unit_in = unit_out Then
                cnv_pressure = p_in
                Exit Function
            End If

            cnv_pressure = ErrorReturn

            'convert input to SI
            Select Case unit_in
                Case ("PA") : p_si = p_in
                Case ("HPA") : p_si = p_in * 100.0#
                Case ("MPA") : p_si = p_in * 1000000.0#
                Case ("KPA") : p_si = p_in * 1000.0#
                Case ("DBAR") : p_si = p_in * 10000.0# + 101325.0#    'SEA PRESSURE ASSUMED
                    'Case ("DB"):      p_si = p_in * 10000# + 101325#    'SEA PRESSURE ASSUMED
                Case ("MBAR") : p_si = p_in * 100.0#
                    'Case ("MB"):      p_si = p_in * 100#
                Case ("BAR") : p_si = p_in * 100000.0#
                Case ("KBAR") : p_si = p_in * 100000000.0#
                Case ("TORR") : p_si = p_in * 101325.0# / 760.0#
                Case ("MMHG") : p_si = p_in * 101325.0# / 760.0#
                Case ("KGF") : p_si = p_in * 98.0665   'ATM = KG/CM2 GAUGE
                Case ("ATM") : p_si = p_in * 98.0665   'ATM = KG/CM2 GAUGE
                Case ("LBF/IN2") : p_si = p_in * 6894.8    'POUND PER SQUARE INCH
                Case ("PSI") : p_si = p_in * 6894.8    'POUND PER SQUARE INCH
                Case ("M") : p_si = 101325.0# + 10000.0# * sw_pres(p_in, lat)
                Case Else : Exit Function
            End Select

            'convert SI to output
            Select Case unit_out
                Case ("PA") : cnv_pressure = p_si
                Case ("HPA") : cnv_pressure = p_si / 100.0#
                Case ("MPA") : cnv_pressure = p_si / 1000000.0#
                Case ("KPA") : cnv_pressure = p_si / 1000.0#
                Case ("DBAR") : cnv_pressure = (p_si - 101325.0#) / 10000.0#   'SEA PRESSURE ASSUMED
                    'Case ("DB"):      cnv_pressure = (p_si - 101325#) / 10000#   'SEA PRESSURE ASSUMED
                Case ("MBAR") : cnv_pressure = p_si / 100.0#
                    'Case ("MB"):      cnv_pressure = p_si / 100#
                Case ("BAR") : cnv_pressure = p_si / 100000.0#
                Case ("KBAR") : cnv_pressure = p_si / 100000000.0#
                Case ("TORR") : cnv_pressure = p_si / 101325.0# * 760.0#
                Case ("MMHG") : cnv_pressure = p_si / 101325.0# * 760.0#
                Case ("KGF") : cnv_pressure = p_si / 98.0665   'ATM = KG/CM2 GAUGE
                Case ("ATM") : cnv_pressure = p_si / 98.0665   'ATM = KG/CM2 GAUGE
                Case ("LBF/IN2") : cnv_pressure = p_si / 6894.8    'POUND PER SQUARE INCH
                Case ("PSI") : cnv_pressure = p_si / 6894.8    'POUND PER SQUARE INCH
                Case ("M") : cnv_pressure = sw_dpth((p_si - 101325.0#) / 10000.0#, lat)
                Case Else : Exit Function
            End Select

        End Function


        Public Function cnv_temperature(ByVal unit_out As String, _
                                    ByVal t_in As Double, _
                                    ByVal unit_in As String) As Double


            'NOTE: UNIT_IN AND UNIT_OUT ARE CASE SENSITIVE. DegC IS NOT THE SAME AS DEGC.
            '      ALL UNITS MUST BE IN CAPITAL LETTERS

            Dim t_si As Double, t_c As Double

            'CHECK VALUE:
            'cnv_temperature("DEGF(T48)",300,"K(T48)") = 80.33
            'cnv_temperature("DEGF(T68)",300,"K(T48)") = 80.3139948157844
            'cnv_temperature("DEGF(T90)",300,"K(T48)") = 80.3018730496216
            'cnv_temperature("DEGC(T48)", 300, "K(T48)") = 26.85
            'cnv_temperature("DEGC(T68)", 300, "K(T48)") = 26.841108231|2
            'cnv_temperature("DEGC(T90)", 300, "K(T48)") = 26.834373916|7
            'cnv_temperature("K(T68)", 300, "K(T48)") = 299.991108231
            'cnv_temperature("K(T90)", 300, "K(T48)") = 299.98437391|7
            'cnv_temperature("DEGF(T68)",299.991108231,"K(T68)") = 80.3139916337851
            'cnv_temperature("DEGF(T90)",299.991108231,"K(T68)") = 80.3018698684571
            'cnv_temperature("DEGC(T68)", 299.991108231, "K(T68)") = 26.8411064632
            'cnv_temperature("DEGC(T90)", 299.991108231, "K(T68)") = 26.8343721491
            'cnv_temperature("K(T90)", 299.991108231, "K(T68)") = 299.984372149
            'cnv_temperature("DEGF(T90)",299.984372149,"K(T90)") = 80.3018698682001
            'cnv_temperature("DEGC(T90)", 299.984373917, "K(T90)") = 26.834373917

            If unit_in = unit_out Then
                cnv_temperature = t_in
                Exit Function
            End If

            cnv_temperature = ErrorReturn

            'CONVERT INPUT TO T90
            Select Case (unit_in)
                Case ("DEGF(T48)") : t_c = (t_in - 32.0#) / 1.8
                    t_si = t_si_from_t48_k(t_k_from_t_c(t_c))
                Case ("DEGF(T68)") : t_c = (t_in - 32.0#) / 1.8
                    t_si = t_si_from_t68_k(t_k_from_t_c(t_c))
                Case ("DEGF(T90)") : t_c = (t_in - 32.0#) / 1.8
                    t_si = t_k_from_t_c(t_c)
                Case ("DEGC(T48)") : t_si = t_si_from_t48_k(t_k_from_t_c(t_in))
                Case ("DEGC(T68)") : t_si = t_si_from_t68_k(t_k_from_t_c(t_in))
                Case ("DEGC(T90)") : t_si = t_k_from_t_c(t_in)
                Case "C", "°C" : t_si = t_k_from_t_c(t_in)
                Case ("K(T48)") : t_si = t_si_from_t48_k(t_in)
                Case ("K(T68)") : t_si = t_si_from_t68_k(t_in)
                Case ("K(T90)") : t_si = t_in
                Case ("K") : t_si = t_in
                Case Else : Exit Function
            End Select

            'CONVERT T90 TO OUTPUT
            Select Case (unit_out)
                Case ("DEGF(T48)")
                    If (unit_in = "K(T48)") Then
                        cnv_temperature = 32.0# + 1.8 * t_c_from_t_k(t_in)
                    ElseIf (unit_in = "DEGC(T48)") Then
                        cnv_temperature = 32.0# + 1.8 * t_in
                    Else
                        Exit Function  't48_f is not a valid output except for t48_k or t48_c input
                    End If
                Case ("DEGF(T68)") : cnv_temperature = 32.0# + 1.8 * t_c_from_t_k(t68_k_from_t_si(t_si))
                Case ("DEGF(T90)") : cnv_temperature = 32.0# + 1.8 * t_c_from_t_k(t_si)
                Case ("DEGC(T48)")
                    If (UCase(unit_in) = "K(T48)") Then
                        cnv_temperature = t_c_from_t_k(t_in)
                    Else
                        Exit Function  't48_c is not a valid output except for t48_k input
                    End If
                Case ("DEGC(T68)") : cnv_temperature = t_c_from_t_k(t68_k_from_t_si(t_si))
                Case ("DEGC(T90)") : cnv_temperature = t_c_from_t_k(t_si)
                Case "C", "°C" : cnv_temperature = t_c_from_t_k(t_si)
                Case ("K(T68)") : cnv_temperature = t68_k_from_t_si(t_si)
                Case ("K(T90)") : cnv_temperature = t_si
                Case ("K") : cnv_temperature = t_si
                Case Else : Exit Function
            End Select

        End Function


        Public Function cnv_salinity(ByVal unit_out As String, _
                                    ByVal s_in As Double, _
                                    ByVal unit_in As String, _
                        Optional ByVal t_si As Double = 298.15, _
                        Optional ByVal p_si As Double = ErrorReturn, _
                        Optional ByVal lon As Double = ErrorReturn, _
                        Optional ByVal lat As Double = ErrorReturn) As Double


            'NOTE: UNIT_IN AND UNIT_OUT ARE CASE SENSITIVE. Cl IS NOT THE SAME AS CL.

            'CHECK VALUES (version 3.0):
            'CNV_SALINITY("CL",0.0357,"KG/KG(ABS)",,101325+1023E4,201,-21)  = 19.6659461766504
            'CNV_SALINITY("KN",0.0357,"KG/KG(ABS)",,101325+1023E4,201,-21)  = 35.527032848854
            'CNV_SALINITY("CND",0.0357,"KG/KG(ABS)",273.15+25.5,101325+1023E4,201,-21) = 1.27556269127774
            'CNV_SALINITY("PSU",0.0357,"KG/KG(ABS)",,101325+1023E4,201,-21) = 35.5275150654278
            'CNV_SALINITY("KG/KG(REF)",0.0357d0,"KG/KG(ABS)",,101325+1023E4,201,-21) = 3.56950425250392E-02
            'CNV_SALINITY("KG/KG(ABS)",0.0356951724471,"KG/KG(REF)",,101325+1023E4,201,-21) = 0.035700129940105
            'CNV_SALINITY("CL", 35.5276443777339, "PSU") = 19.6660177563499
            'CNV_SALINITY("KN", 35.5276443777339, "PSU") = 35.5271620502116
            'CNV_SALINITY("CND",35.52764437773386,"PSU",273.15+25.5,101325+1023E4) = 1.27556680821579
            'CNV_SALINITY("KG/KG(REF)", 35.5276443777339, "PSU") = 3.56951724471082E-02
            'CNV_SALINITY("KG/KG(ABS)",35.52764437773386d0,"PSU",,101325+1023E4,201,-21) = 3.57001299401131E-02
            'CNV_SALINITY("PSU", 35.5271620502, "KN") = 35.5276443777223
            'CNV_SALINITY("KG/KG(ABS)",0.355271620502E+02,"KN",,101325+1023E4,201,-21) = 3.57001299401016E-02
            'CNV_SALINITY("PSU",0.127556680822E+01,"CND",273.15+25.5,101325+1023E4) = 35.5276443778661
            'CNV_SALINITY("KG/KG(ABS)",0.127556680822E+01,"CND",273.15+25.5,101325+1023E4,201,-21) = 3.57001299402461E-02


            ' CHECK VALUES (obsolete Fortran versions 1.0, 2.0):
            'CNV_SALINITY("CL",0.0357,"KG/KG(ABS)",,101325+1023E4,201,-21)  = 0.196660177563E+02
            'CNV_SALINITY("KN",0.0357,"KG/KG(ABS)",,101325+1023E4,201,-21)  = 0.355271620502E+02
            'CNV_SALINITY("CND",0.0357,"KG/KG(ABS)",273.15+25.5,101325+1023E4,201,-21)  = 0.127556680822E+01
            'CNV_SALINITY("PSU",0.0357,"KG/KG(ABS)",,101325+1023E4,201,-21)  = 0.355276443777E+02
            'CNV_SALINITY("KG/KG(REF)",0.0357d0,"KG/KG(ABS)",,101325+1023E4,201,-21)  = 0.356951724471E-01
            'CNV_SALINITY("KG/KG(ABS)",0.0356951724471,"KG/KG(REF)",,101325+1023E4,201,-21)  = 0.0357
            'CNV_SALINITY("CL", 35.5276443777339, "PSU") = 19.6660177563
            'CNV_SALINITY("KN", 35.5276443777339, "PSU") = 35.5271620502
            'CNV_SALINITY("CND",35.52764437773386,"PSU",273.15+25.5,101325+1023E4) = 1.27556680821579
            'CNV_SALINITY("KG/KG(REF)", 35.5276443777339, "PSU") = 0.0356951724471
            'CNV_SALINITY("KG/KG(ABS)",35.52764437773386d0,"PSU",,101325+1023E4,201,-21)  = 0.0357
            'CNV_SALINITY("PSU", 35.5271620502, "KN") = 35.5276443777
            'CNV_SALINITY("KG/KG(ABS)",0.355271620502E+02,"KN",,101325+1023E4,201,-21)  = 0.357000000000E-01
            'CNV_SALINITY("PSU",0.127556680822E+01,"CND",273.15+25.5,101325+1023E4) = 35.5276443778661
            'CNV_SALINITY("KG/KG(ABS)",0.127556680822E+01,"CND",273.15+25.5,101325+1023E4,201,-21)  = 0.0357

            Const ups = SO_salinity_si / 35  'SR = SP * ups
            Const ucl = 1.80655 * ups        'SR = Cl * ucl
            Const c35 = 4.2914               'C(35,15,0) = 42.914 mS/cm = 4.2914 S/m,  Culkin & Smith

            Dim s_si As Double, psal As Double, t68_c As Double
            Dim p_dbar As Double, c_ratio As Double, p_loc As Double

            If unit_in = unit_out Then
                cnv_salinity = s_in
                Exit Function
            End If

            cnv_salinity = ErrorReturn

            p_loc = p_si
            If p_si = ErrorReturn Or _
                lon = ErrorReturn Or _
                lat = ErrorReturn Then
                p_loc = 101325
                lon = -20
                lat = 50
            End If
            If p_si = ErrorReturn Then p_si = 101325

            'CONVERT INPUT TO REFERENCE SALINITY IN KG/KG
            Select Case unit_in

                'Chlorinity
                Case "CL" : s_si = s_in * ucl

                    'Knudsen Salinity
                Case "KN" : s_si = (s_in - 0.03) / 1.805 * ucl

                    'Conductivity
                Case "MS/CM", "MMHO/CM" 'conductivity in mS/cm = mmho/cm
                    'Temperature and pressure is needed to convert from CONDUCTIVITY
                    'If missing, proceeding with t90 = 25°C and atmospheric pressure
                    t68_c = cnv_temperature("DEGC(T68)", t_si, "K(T90)")
                    p_dbar = cnv_pressure("DBAR", p_si, "PA")
                    c_ratio = 0.1 * s_in / c35
                    s_si = sal78_from_cnd(c_ratio, t68_c, p_dbar) * ups

                Case "S/M"    'conductivity in S/m
                    t68_c = cnv_temperature("DEGC(T68)", t_si, "K(T90)")
                    p_dbar = cnv_pressure("DBAR", p_si, "PA")
                    c_ratio = s_in / c35
                    s_si = sal78_from_cnd(c_ratio, t68_c, p_dbar) * ups

                Case "CND"    'conductivity ratio
                    t68_c = cnv_temperature("DEGC(T68)", t_si, "K(T90)")
                    p_dbar = cnv_pressure("DBAR", p_si, "PA")
                    c_ratio = s_in
                    s_si = sal78_from_cnd(c_ratio, t68_c, p_dbar) * ups

                    'PRACTICAL SALINITY
                Case "PSU", "PSS", "ONE" : s_si = s_in * ups

                    'Reference SALINITY
                Case "KG/KG(REF)" : s_si = s_in

                Case "G/KG(REF)" : s_si = 0.001 * s_in

                    'ABSOLUTE SALINITY
                Case "KG/KG(ABS)"
                    'Position and pressure are needed to convert from ABSOLUTE SALINITY
                    'in order to determine the ABSOLUTE SALINITY anomaly
                    'If missing, proceeding value from surface North Atlantic (near zero)
                    'Pressure is needed to convert from ABSOLUTE SALINITY
                    'If missing, proceeding with atmospheric pressure
                    s_si = psal_from_asal(s_in, lon, lat, p_loc) * ups

                Case "G/KG(ABS)"
                    s_si = psal_from_asal(0.001 * s_in, lon, lat, p_loc) * ups

                Case Else : Exit Function

            End Select

            'CONVERT SI REFERENCE SALINITY TO OUTPUT
            Select Case unit_out

                'Chlorinity
                Case "CL" : cnv_salinity = s_si / ucl

                    'Knudsen Salinity
                Case "KN" : cnv_salinity = 0.03 + 1.805 * s_si / ucl

                    'Conductivity
                Case "MS/CM", "MMHO/CM" 'conductivity in mS/cm = mmho/cm
                    'Temperature and pressure is needed to convert to CONDUCTIVITY
                    'If missing, proceeding with t90 = 25°C and atmospheric pressure
                    t68_c = cnv_temperature("DEGC(T68)", t_si, "K(T90)")
                    p_dbar = cnv_pressure("DBAR", p_si, "PA")
                    psal = s_si / ups
                    cnv_salinity = 10 * c35 * cnd_from_sal78(psal, t68_c, p_dbar)

                Case "S/M"  'conductivity in S/m
                    t68_c = cnv_temperature("DEGC(T68)", t_si, "K(T90)")
                    p_dbar = cnv_pressure("DBAR", p_si, "PA")
                    psal = s_si / ups
                    cnv_salinity = c35 * cnd_from_sal78(psal, t68_c, p_dbar)

                Case "CND"    'conductivity ratio
                    t68_c = cnv_temperature("DEGC(T68)", t_si, "K(T90)")
                    p_dbar = cnv_pressure("DBAR", p_si, "PA")
                    psal = s_si / ups
                    cnv_salinity = cnd_from_sal78(psal, t68_c, p_dbar)

                    'PRACTICAL SALINITY
                Case "PSU", "PSS", "ONE" : cnv_salinity = s_si / ups

                    'Reference SALINITY
                Case "KG/KG(REF)" : cnv_salinity = s_si

                Case "G/KG(REF)" : cnv_salinity = 1000.0# * s_si

                    'ABSOLUTE SALINITY
                Case "KG/KG(ABS)"
                    'Position and pressure are needed to convert to ABSOLUTE SALINITY
                    'in order to determine the ABSOLUTE SALINITY anomaly
                    'If missing, proceeding value from surface North Atlantic (near zero)
                    'Pressure is needed to convert from ABSOLUTE SALINITY
                    'If missing, proceeding with atmospheric pressure
                    psal = s_si / ups
                    cnv_salinity = asal_from_psal(psal, lon, lat, p_loc)

                Case "G/KG(ABS)"
                    psal = s_si / ups
                    cnv_salinity = 1000.0# * asal_from_psal(psal, lon, lat, p_loc)

                Case Else : Exit Function

            End Select

        End Function


        Private Function present(ByVal x As Double) As Boolean

            present = (x <> ErrorReturn)

        End Function


        Private Function t_k_from_t_c(ByVal t_c As Double) As Double

            'this function computes temperature in K from temperature in °C (ITS-90, IPTS-68, IPTS-48)

            t_k_from_t_c = 273.15 + t_c

        End Function


        Private Function t_c_from_t_k(ByVal T_K As Double) As Double

            'this function computes temperature in °C from temperature in K (ITS-90, IPTS-68, IPTS-48)

            t_c_from_t_k = T_K - 273.15

        End Function


        Private Function t68_k_from_t48_k(ByVal t48_k)

            'this function returns IPTS-68 temperature in K computed from IPTS-48 temperature in K.

            'VB code adapted from:

            'C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            'C
            'C                          RUHR-UNIVERSITAET BOCHUM
            'C                        Lehrstuhl fuer Thermodynamik
            'C                          Prof. Dr.-Ing. W. Wagner
            'C                              D - 44780 BOCHUM
            'C                             Tel.: 0234/700-3033
            'C                             FAX : 0234/7094 -163
            'C
            'C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

            'c
            'C PROGRAMM ZUR TEMPERATURUMRECHNUNG: IPTS-48 IN IPTS-68
            'c
            'C DER WERT 't48_k' IN DER IPTS-48 (IN KELVIN) WIRD VOM RUFENDEN PROGRAMM UEBENOMMEN
            'C UND IN t68_k_from_t48_k (IN KELVIN) WIEDER UEBERGEBEN.
            'c

            Const A48A4 = 0.003984517
            Const B48 = -0.000000585502
            Const C48C4 = -0.00000000000435716
            Const A48A68 = -0.00000019539
            Const B48B68 = 0.0000000019539
            Const A68 = 0.003984712
            Const B68 = -0.000000587456

            Dim t48 As Double, t68 As Double, T68ALT As Double, TSTRI As Double
            Dim HG1 As Double, HG2 As Double, HG3 As Double, FKT As Double
            Dim i As Integer, k As Integer

            Dim a(21)

            a(1) = 273.15
            a(2) = 250.846209678803
            a(3) = 135.099869965
            a(4) = 52.78567590085
            a(5) = 27.6768548854105
            a(6) = 39.1053205376684
            a(7) = 65.5613230578069
            a(8) = 80.8035868559867
            a(9) = 70.5242118234052
            a(10) = 44.7847589638966
            a(11) = 21.2525653556078
            a(12) = 7.67976358170846
            a(13) = 2.1368945938285
            a(14) = 0.459843348928069
            a(15) = 0.0763614629231648
            a(16) = 0.00969328620373121
            a(17) = 0.000923069154007008
            a(18) = 0.0000638116590952654
            a(19) = 0.00000302293237874619
            a(20) = 0.000000087755139130376
            a(21) = 0.00000000117702613125477

            If (t48_k < 90.0#) Then
                'MsgBox "ACHTUNG: Keine Umrechnung T48T68 fuer t48_k < 90 K '"
                t68_k_from_t48_k = t48_k
                Exit Function
            End If


            If (t48_k > 273.15) Then GoTo 10

            t48 = t48_k - 273.15
            HG1 = 1.0# + A48A4 * t48 + B48 * t48 * t48 + C48C4 * t48 * t48 * t48 * (t48 - 100.0#)
            HG1 = Log(HG1)
            t68 = 0.0#
            For k = 1 To 21
                i = 22 - k
                t68 = t68 * HG1 + a(i)
            Next k

            t68_k_from_t48_k = t68
            Exit Function

10:
            t48 = t48_k - 273.15
            HG1 = A68 + 2.0# * B68 * t48
            HG2 = A48A68 * t48 + B48B68 * t48 * t48
            t68 = t48
            T68ALT = t48
            TSTRI = t48

11:         FKT = 0.045 * TSTRI / 100.0# * (TSTRI / 100.0# - 1.0#) * (TSTRI / 419.58 - 1.0#) * (TSTRI / 630.74 - 1.0#)
            HG3 = B68 * (t68 - t48 - FKT) * (t68 - t48 - FKT)
            t68 = t48 + FKT + HG2 / HG1 - HG3 / HG1
            If (Abs(t68 - T68ALT) < 0.000000000001) Then GoTo 12 'NOTE 1D-5 REPLACED BY 1D-12
            T68ALT = t68
            TSTRI = t68
            GoTo 11

12:
99:         t68_k_from_t48_k = t68 + 273.15

        End Function


        Private Function t_si_from_t48_k(ByVal t48_k As Double) As Double

            t_si_from_t48_k = t_si_from_t68_k(t68_k_from_t48_k(t48_k))

        End Function


        Private Function t_si_from_t68_k(ByVal t68_k As Double) As Double

            'this function returns ITS-90 temperature in K computed from IPTS-68 temperature in K.

            'VB code adapted from:

            'C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            'c
            'C                          RUHR-UNIVERSITAET BOCHUM
            'C                        Lehrstuhl fuer Thermodynamik
            'C                          Prof. Dr.-Ing. W. Wagner
            'C                              D - 44780 BOCHUM
            'C                             Tel.: 0234/700-3033
            'C                             FAX : 0234/7094 -163
            'c
            'C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            'c
            'C   REGION 1:  13.8 K  < t68_k <   83.8 K
            'c
            'C   REGION 2:  83.8 K  < t68_k <  903.75 K
            'c
            'C   REGION 3: 903.89 K < T68 < 1337.58 K
            'c
            'C   REGION 4:     1337.58 < T68
            'c
            'C   INPUT:  t68_k      TEMPERATURE IN K (IPTS-68)
            'c
            'C   OUTPUT: t68_k_from_t68_k   TEMPERATURE IN K (ITS-90)
            'c
            'c From:
            'c
            'C  Supplementary Information for the ITS-90, Bureau International des Poids et
            'C  Mesures, Pav Breteuil, 92312 Sevres Cedex, France, 1990.
            'c
            'C  R.L.Rusby: The Conversion of Thermal Reference Values to the
            'C             ITS-90. J. Chem. Thermodynamics, 1991, 23, 1153 - 1161.
            'c
            'C  Rusby et al.: International Report. Revised Values for (t90-t68) from
            'C                630C to 1064C. Metrologia, 1994, 31, 149 - 153.
            'c
            'C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

            Dim DELTAT As Double, Tn As Double
            Dim j As Integer

            Dim a(13), b(9), c(6) As Double

            'c
            a(0) = -0.005903
            a(1) = 0.008174
            a(2) = -0.061924
            a(3) = -0.193388
            a(4) = 1.490793
            a(5) = 1.252347
            a(6) = -9.835868
            a(7) = 1.411912
            a(8) = 25.277595
            a(9) = -19.183815
            a(10) = -18.437089
            a(11) = 27.000895
            a(12) = -8.716324
            'c
            b(1) = -0.148759
            b(2) = -0.267408
            b(3) = 1.08076
            b(4) = 1.269056
            b(5) = -4.089591
            b(6) = -1.871251
            b(7) = 7.438081
            b(8) = -3.536296
            'c
            c(0) = 78.687209
            c(1) = -0.47135991
            c(2) = 0.0010954715
            c(3) = -0.0000012357884
            c(4) = 0.00000000067736583
            c(5) = -0.00000000000014458081
            'c

            DELTAT = 0.0#
            'c
            If (t68_k < 13.8) Then GoTo 999
            'c
            'C     ^* REGION 4 ^*
            'c
            If (t68_k >= 1337.58) Then
                DELTAT = -0.25 * (t68_k / 1337.33) ^ 2
                GoTo 1000
                'c
                'C     ^* REGION 3 ^*
                'c
                'C DIE GLEICHUNG IST IN ABHAENGIGKEIT VON T90 FORMULIERT, FUER DIE
                'C BERECHNUNG VON DELTAT WIRD HIER T68=T90 GESETZT /AK 16.7.96
                'c
            ElseIf (t68_k >= 903.89) Then
                Tn = t68_k - 273.15
                For j = 1 To 5
                    DELTAT = DELTAT + c(j) * Tn ^ j
                Next j

                DELTAT = DELTAT + c(0)
                GoTo 1000
                'c
                'C     ^* REGION 2 ^*
                'c
            ElseIf (t68_k >= 83.8) Then
                Tn = (t68_k - 273.15) / 630.0#
                For j = 1 To 8
                    DELTAT = DELTAT + b(j) * Tn ^ j
                Next j
                GoTo 1000
                'c
                'C     ^* REGION 1 ^*
                'c
            ElseIf (t68_k >= 13.8) Then
                Tn = (t68_k - 40.0#) / 40.0#
                For j = 1 To 12
                    DELTAT = DELTAT + a(j) * Tn ^ j
                Next j

                DELTAT = DELTAT + a(0)
                GoTo 1000
            End If
            'c
999:        'MsgBox "Temperature out of range of validity, no conversion."
            t_si_from_t68_k = t68_k
            Exit Function

            'c
1000:       t_si_from_t68_k = t68_k + DELTAT

        End Function


        Private Function t68_k_from_t_si(ByVal t_si As Double) As Double

            'this function returns IPTS-68 temperature in K computed from ITS-90 temperature in K.

            'VB code adapted from:

            'C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            'C
            'C                          RUHR-UNIVERSITAET BOCHUM
            'C                        Lehrstuhl fuer Thermodynamik
            'C                          Prof. Dr.-Ing. W. Wagner
            'C                              D - 44780 BOCHUM
            'C                             Tel.: 0234/700-3033
            'C                             FAX : 0234/7094 -163
            'C
            'C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            'C
            'C   REGION 1:  13.8 K  < t_si <   83.8 K
            'C
            'C   REGION 2:  83.8 K  < t_si <  903.75 K
            'C
            'C   REGION 3: 903.765 K < t_si < 1337.33 K
            'C
            'C   REGION 4:     1337.33 < t_si
            'C
            'C   INPUT:  t_si      TEMPERATURE IN K (ITS-90)
            'C
            'C   OUTPUT: t68_k_from_t_si   TEMPERATURE IN K (IPTS-68)
            'C
            'C From:
            'C
            'C  Supplementary Information for the ITS-90, Bureau International des Poids et
            'C  Mesures, Pav Breteuil, 92312 Sevres Cedex, France, 1990.
            'C
            'C  R.L.Rusby: The Conversion of Thermal Reference Values to the
            'C             ITS-90. J. Chem. Thermodynamics, 1991, 23, 1153 - 1161.
            'C
            'C  Rusby et al.: International Report. Revised Values for (t90-t68) from
            'C                630C to 1064C. Metrologia, 1994, 31, 149 - 153.
            'C
            'C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

            Dim j As Integer
            Dim DELTAT As Double, Tn As Double

            Dim a(13), b(9), c(6) As Double

            a(0) = -0.005903
            a(1) = 0.008174
            a(2) = -0.061924
            a(3) = -0.193388
            a(4) = 1.490793
            a(5) = 1.252347
            a(6) = -9.835868
            a(7) = 1.411912
            a(8) = 25.277595
            a(9) = -19.183815
            a(10) = -18.437089
            a(11) = 27.000895
            a(12) = -8.716324

            b(1) = -0.148759
            b(2) = -0.267408
            b(3) = 1.08076
            b(4) = 1.269056
            b(5) = -4.089591
            b(6) = -1.871251
            b(7) = 7.438081
            b(8) = -3.536296

            c(0) = 78.687209
            c(1) = -0.47135991
            c(2) = 0.0010954715
            c(3) = -0.0000012357884
            c(4) = 0.00000000067736583
            c(5) = -0.00000000000014458081

            DELTAT = 0
            If (t_si < 13.8) Then GoTo 999

            'C
            'C     ^* REGION 4 ^*
            'C
            If (t_si >= 1337.33) Then
                DELTAT = -0.25 * (t_si / 1337.33) ^ 2
                GoTo 1000

                'C
                'C     ^* REGION 3 ^*
                'C
            ElseIf (t_si >= 903.765) Then
                Tn = t_si - 273.15

                For j = 1 To 5
                    DELTAT = DELTAT + c(j) * Tn ^ j
                Next j

                DELTAT = DELTAT + c(0)
                GoTo 1000
                'C
                'C     ^* REGION 2 ^*
                'C
            ElseIf (t_si >= 83.8) Then
                Tn = (t_si - 273.15) / 630.0#

                For j = 1 To 8
                    DELTAT = DELTAT + b(j) * Tn ^ j
                Next j
                GoTo 1000
                'C
                'C     ^* REGION 1 ^*
                'C
            ElseIf (t_si >= 13.8) Then
                Tn = (t_si - 40.0#) / 40.0#
                For j = 1 To 12
                    DELTAT = DELTAT + a(j) * Tn ^ j
                Next j

                DELTAT = DELTAT + a(0)
                GoTo 1000
            End If

999:        'MsgBox "Temperature out of range of validity, no conversion."
            t68_k_from_t_si = t_si
            Exit Function

1000:       t68_k_from_t_si = t_si - DELTAT

        End Function


        Private Function p_si_from_psea_pa(ByVal psea_pa As Double) As Double

            'this function computes absolute pressure in Pa from sea pressure in Pa

            p_si_from_psea_pa = psea_pa + 101325.0#

        End Function


        Private Function psea_pa_from_p_si(ByVal p_si As Double) As Double

            'this function computes sea pressure in Pa from absolute pressure in Pa

            psea_pa_from_p_si = p_si - 101325.0#

        End Function


        Private Function psea_pa_from_psea_bar(ByVal psea_bar As Double) As Double

            'this function computes sea pressure in Pa from absolute pressure in Pa

            psea_pa_from_psea_bar = psea_bar * 100000.0#

        End Function


        Private Function psea_bar_from_psea_pa(ByVal psea_pa As Double) As Double

            'this function computes sea pressure in Pa from absolute pressure in Pa

            psea_bar_from_psea_pa = psea_pa * 0.00001

        End Function


        Function sal78_from_cnd(ByVal cnd As Double, _
                                ByVal t68_c As Double, _
                                ByVal p_dbar As Double) As Double


            'THE CONDUCTIVITY RATIO (CND)=1.0000000 FOR SALINITY=35 PSS-78
            'TEMPERATURE=15.0 DEG. CELSIUS AND ATMOSPHERIC PRESSURE.
            'FUNCTION TO CONVERT CONDUCTIVITY RATIO TO SALINITY

            'REFERENCES: ALSO LOCATED IN UNESCO REPORT NO. 37 1981
            'PRACTICAL SALINITY SCALE 1978: E.L. LEWIS IEEE OCEAN ENG. JAN. 1980

            '----------------------------------------------------------
            'UNITS:
            '  PRESSURE      p_dbar   DECIBARS
            '  TEMPERATURE   t68_c    DEG CELSIUS IPTS-68
            '  CONDUCTIVITY  CND      RATIO
            '  SALINITY      SAL78    PSS-78
            '----------------------------------------------------------
            'CHECKVALUES:
            '  SAL78=40.00000 for CND=1.8880911555787682, T=40 DEG C, P=10000 DECIBARS
            '----------------------------------------------------------
            'SAL78 RATIO: RETURNS ZERO FOR CONDUCTIVITY RATIO: < 0.0005
            '----------------------------------------------------------
            'ORIGINAL FORTRAN CODE IS FOUND IN:
            '  UNESCO TECHNICAL PAPERS IN MARINE SCIENCE 44 (1983) -
            '  'ALGORITHMS FOR COMPUTATION OF FUNDAMENTAL PROPERTIES OF SEAWATER'
            '----------------------------------------------------------
            'TRANSLATED TO OBJECT PASCAL BY:
            '  DR. JAN SCHULZ, 19 MAY 2008, WWW.CODE10.INFO
            '----------------------------------------------------------
            'TRANSLATED BACK TO FORTRAN BY:
            '  D. WRIGHT, BIO, 23 SEPT., 2009
            '----------------------------------------------------------

            'real*8 sal78_from_cnd, cnd, t68_c, p_dbar
            Dim dt#, res#, RT#

            sal78_from_cnd = 0.0#

            ' ZERO SALINITY/CONDUCTIVITY TRAP
            If (cnd <= 0.0005) Then Exit Function

            dt = t68_c - 15.0#

            ' CONVERT CONDUCTIVITY TO SALINITY
            res = cnd
            RT = res / (rt35(t68_c) * (1.0# + c(p_dbar) / (b(t68_c) + a(t68_c) * res)))
            RT = Sqrt(Abs(RT))
            sal78_from_cnd = sal(RT, dt)

        End Function


        Function cnd_from_sal78(ByVal sal78 As Double, _
                                ByVal t68_c As Double, _
                                ByVal p_dbar As Double) As Double


            'THE CONDUCTIVITY RATIO (CND)=1.0000000 FOR SALINITY=35 PSS-78
            'TEMPERATURE=15.0 DEG. CELSIUS AND ATMOSPHERIC PRESSURE.
            'Function to convert SALINITY TO CONDUCTIVITY RATIO

            'REFERENCES: ALSO LOCATED IN UNESCO REPORT NO. 37 1981
            'PRACTICAL SALINITY SCALE 1978: E.L. LEWIS IEEE OCEAN ENG. JAN. 1980

            '----------------------------------------------------------
            'UNITS:
            '  PRESSURE      p_dbar      DECIBARS
            '  TEMPERATURE   t68_c      DEG CELSIUS IPTS-68
            '  CONDUCTIVITY  CND    RATIO
            '  SALINITY      SAL78  PSS-78
            '----------------------------------------------------------
            'CHECKVALUES:
            '  CND=1.8880911555787682 for SAL78=40.0000, T=40 DEG C, P=10000 DECIBARS
            '----------------------------------------------------------
            'CND: RETURNS ZERO FOR SALINITY: < 0.02
            '----------------------------------------------------------
            'ORIGINAL FORTRAN CODE IS FOUND IN:
            '  UNESCO TECHNICAL PAPERS IN MARINE SCIENCE 44 (1983) -
            '  'ALGORITHMS FOR COMPUTATION OF FUNDAMENTAL PROPERTIES OF SEAWATER'
            '----------------------------------------------------------
            'TRANSLATED TO OBJECT PASCAL BY:
            '  DR. JAN SCHULZ, 19 MAY 2008, WWW.CODE10.INFO
            '----------------------------------------------------------
            'TRANSLATED BACK TO FORTRAN BY:
            '  D. WRIGHT, BIO, 23 SEPT., 2009
            '----------------------------------------------------------

            Dim n&
            Dim cnd#
            Dim dt#, RT#, si#, dels#, rtt#, at#, bt#, cp#, res#

            cnd = 0.0#

            'ZERO SALINITY/CONDUCTIVITY TRAP
            If (sal78 <= 0.2) Then Exit Function

            dt = t68_c - 15.0#

            'INVERT SALINITY TO CONDUCTIVITY BY THE
            'NEWTON-RAPHSON ITERATIVE METHOD

            'FIRST APPROXIMATION
            RT = Sqrt(sal78 / 35.0#)
            si = sal(RT, dt)

            'ITERATION LOOP BEGINS HERE WITH A MAXIMUM OF 20 CYCLES
            For n = 1 To 20
                RT = RT + (sal78 - si) / dsal(RT, dt)
                si = sal(RT, dt)
                dels = Abs(si - sal78)
                If (n > 1 And dels < 1.0E-20) Then Exit For
            Next n


            'COMPUTE CONDUCTIVITY RATIO
            rtt = rt35(t68_c) * RT * RT
            at = a(t68_c)
            bt = b(t68_c)
            cp = c(p_dbar)
            cp = rtt * (cp + bt)
            bt = bt - rtt * at

            'SOLVE QUADRATIC EQUATION FOR R: R=RT35*RT*(1+C/AR+B)
            res = Sqrt(Abs(bt * bt + 4.0# * at * cp)) - bt

            'RETURN CONDUCTIVITY
            cnd_from_sal78 = 0.5 * res / at

        End Function


        Private Function sal(ByVal xr As Double, _
                                ByVal xt As Double) As Double


            'PRACTICAL SALINITY SCALE 1978 DEFINITION WITH TEMPERATURE
            'CORRECTION;XT :=T-15.0; XR:=SQRT(RT)

            'real*8 sal, xr, xt

            sal = ((((2.7081 * xr - 7.0261) * xr + 14.0941) * xr + 25.3851) * xr _
                - 0.1692) * xr + 0.008 _
                + (xt / (1.0# + 0.0162 * xt)) * (((((-0.0144 * xr _
                + 0.0636) * xr - 0.0375) * xr - 0.0066) * xr _
                - 0.0056) * xr + 0.0005)

        End Function


        Private Function dsal(ByVal xr As Double, _
                                ByVal xt As Double) As Double


            'FUNCTION FOR DERIVATIVE OF SAL(XR,XT) WITH XR

            'real*8 dsal, xr, xt

            dsal = ((((13.5405 * xr - 28.1044) * xr + 42.2823) * xr + 50.7702) _
                    * xr - 0.1692) + (xt / (1.0# + 0.0162 * xt)) * ((((-0.072 _
                    * xr + 0.2544) * xr - 0.1125) * xr - 0.0132) * xr - 0.0056)

        End Function


        Private Function rt35(ByVal xt As Double) As Double


            'FUNCTION RT35: C(35,T,0)/C(35,15,0) VARIATION WITH TEMPERATURE

            'real*8 rt35, xt

            rt35 = (((0.0000000010031 * xt - 0.00000069698) * xt + 0.0001104259) * xt _
                    + 0.0200564) * xt + 0.6766097

        End Function


        Private Function c(ByVal xp As Double) As Double


            'C(XP) POLYNOMIAL CORRESPONDS TO A1-A3 CONSTANTS: LEWIS 1980

            'real*8 c, xp

            c = ((0.000000000000003989 * xp - 0.000000000637) * xp + 0.0000207) * xp

        End Function


        Private Function b(ByVal xt As Double) As Double


            'real*8 b, xt

            b = (0.0004464 * xt + 0.03426) * xt + 1.0#

        End Function


        Private Function a(ByVal xt As Double) As Double


            'A(XT) POLYNOMIAL CORRESPONDS TO B3 AND B4 CONSTANTS: LEWIS 1980

            'integer n, iter
            'real*8 a, xt
            'real*8 dt, res, rt, si, dels, rtt, at, bt, cp

            a = -0.003107 * xt + 0.4215

        End Function


        Function sw_dpth(ByVal p As Double, _
                            Optional ByVal lat As Double = 45.0#) As Double


            ' SW_DPTH    DEPTH FROM PRESSURE

            ' SW_DPTH   IS A MODIFICATION OF THE MATLAB ROUTINE WRITTEN BY PHIL MORGAN
            '           OF CSIRO FOR THE SEAWATER LIBRARY DEVELOPED BY HIM AROUND 1992.
            '
            ' USAGE:  DPTH = SW_DPTH(P,LAT)
            '
            ' DESCRIPTION:
            '    CALCULATES DEPTH IN METRES FROM PRESSURE IN DBARS.
            '    BASED ON EQN 25, P26.  UNESCO 1983.
            '
            ' INPUT:
            '   P   = PRESSURE    [DB]
            '   LAT = LATITUDE IN DECIMAL DEGRESS NORTH [-90..+90]
            '
            ' OUTPUT:
            '  DPTH = DEPTH [METRES]
            '
            ' DISCLAIMER:
            '   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
            '
            ' REFERENCES:
            '    UNESCO 1983. ALGORITHMS FOR COMPUTATION OF FUNDAMENTAL PROPERTIES OF
            '    SEAWATER, 1983. _UNESCO TECH. PAP. IN MAR. SCI._, NO. 44, 53 PP.


            'real*8 p, sw_dpth
            Dim deg2rad#, c1#, x#
            'real*8, optional :: lat

            'If (Not present(lat)) Then lat = 45#

            sw_dpth = ErrorReturn

            ' CHECK INPUTS
            If (p < 0.0# Or Abs(lat) > 90.0#) Then Exit Function

            ' The following commented code is the original
            ' It is not an exact inverse of sw_press
            'deg2rad = 3.1415926535 / 180#
            'c1 = 9.72659
            'c2 = -0.000022512
            'c3 = 0.0000000002279
            'c4 = -1.82E-15
            'gam_dash = 0.000002184
            'x   = sin(lat*deg2rad)^2  ' CONVERT TO RADIANS
            'sw_dpth = (((c4 * P + c3) * P + c2) * P + c1) * P
            'sw_dpth = sw_dpth / (9.780318 * (1# + (0.0052788 + 0.0000236 * x) * x) + gam_dash * 0.5 * P)

            ' The following code is simply the analytical inverse of swpres
            deg2rad = 3.1415926535 / 180.0#
            x = Sin(Abs(lat) * deg2rad)      ' convert to radians
            c1 = 0.00592 + x ^ 2 * 0.00525
            sw_dpth = ((1.0# - c1) ^ 2 - (1.0# - c1 - 0.00000442 * p) ^ 2) / 0.00000884

        End Function

        Function sw_pres(ByVal depth As Double, _
                            Optional ByVal lat As Double = 45.0#) As Double

            ' SW_PRES    PRESSURE FROM DEPTH
            ' SW_PRES   IS A MODIFICATION OF THE MATLAB ROUTINE WRITTEN BY PHIL MORGAN
            '           OF CSIRO FOR THE SEAWATER LIBRARY DEVELOPED BY HIM AROUND 1992.
            '
            ' USAGE:  PRES = SW_PRES(DEPTH,LAT)
            '
            ' DESCRIPTION:
            '    CALCULATES PRESSURE IN DBARS FROM DEPTH IN METERS.
            '
            ' INPUT:
            '   DEPTH = DEPTH [METRES]
            '   LAT   = LATITUDE IN DECIMAL DEGRESS NORTH [-90..+90]
            '
            ' OUTPUT:
            '  PRES   = PRESSURE    [DB]
            '
            ' DISCLAIMER:
            '   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
            '
            ' REFERENCES:
            '    SAUNDERS, P.M. 1981
            '    "PRACTICAL CONVERSION OF PRESSURE TO DEPTH"
            '    JOURNAL OF PHYSICAL OCEANOGRAPHY, 11, 573-574
            '
            ' CHECK VALUE:
            '    P=7500.00 DB for LAT=30 DEG, DEPTH=7321.45 METERS


            'real*8 depth, sw_pres
            Dim deg2rad#, c1#, x#
            'real*8, optional :: lat

            'If (Not present(lat)) Then lat = 45#

            sw_pres = ErrorReturn

            ' CHECK INPUTS
            If (depth < 0.0# Or Abs(lat) > 90.0#) Then Exit Function

            deg2rad = 3.1415926535 / 180.0#
            x = Sin(Abs(lat) * deg2rad)      ' convert to radians
            c1 = 0.00592 + x ^ 2 * 0.00525
            sw_pres = ((1.0# - c1) - Sqrt((1.0# - c1) ^ 2 - 0.00000884 * depth)) / 0.00000442

        End Function

#End Region

#Region "Maths_0"

        '#########################################################################

        'This module requires the library module
        '     Constants_0_Mdl, file Constants_0.bas

        '#########################################################################


        'This module implements mathematical functions

        'Implementation in VB6 by Rainer Feistel
        'for publication in Ocean Science 2008, as described in the papers

        'Feistel, R., Wright, D.G., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part I: Background and Equations.
        'Ocean Science, 2009

        'Wright, D.G., Feistel, R., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part II: The Library Routines,
        'Ocean Science, 2009


        'Private Const ErrorReturn = 9.99999999E+98

        'Private Const PI = 3.14159265358979


        Public Function arccos(ByVal x As Double) As Double
            'Returns real function arccos(x)

            Dim a As Double

            a = arcsin(x)
            If a = ErrorReturn Then
                arccos = a
            Else
                arccos = 0.5 * PI - a
            End If

        End Function


        Public Function arcsin(ByVal x As Double) As Double

            'Returns real function arcsin(x) between -PI/2 and +PI/2

            Select Case Abs(x)

                'For invalid arguments > 1, accept a certain rounding tolerance
                Case Is > 1.00000001 : arcsin = ErrorReturn

                    'compute regular values from intrinsic arctan
                Case Is < 1 : arcsin = Atan(x / Sqrt(1.0# - x * x))

                    'values -PI/2 and +PI/2
                Case Else : arcsin = 0.5 * PI * Sign(x)

            End Select

        End Function


        Public Function get_CubicRoots(ByVal R As Double, _
                                        ByVal S As Double, _
                                        ByVal t As Double, _
                                        ByRef x1 As Double, _
                                        ByRef x2 As Double, _
                                        ByRef x3 As Double) As Long

            'input: r,s,t are the coefficients of the polynomial
            '       x^3 + r * x^2 + s * x + t = 0
            'output: x1, x2, x3 are the roots of the polynomial
            'returns:
            '         get_CubicRoots = 1:  x1 real solution, x2 real part, x3 imag part of the complex pair
            '         get_CubicRoots = 3:  x1, x2, x3 real solutions

            'requires the function arccos(x)

            Dim p#, q#
            Dim a#, phi#, u#, v#

            p# = S - R ^ 2 / 3.0#
            q# = 2.0# * R ^ 3 / 27.0# - R * S / 3.0# + t

            a = (q / 2.0#) ^ 2 + (p / 3.0#) ^ 3
            If a >= 0 Then
                u = -q / 2 + Sqrt(a)
                If u >= 0 Then
                    u = u ^ (1.0# / 3.0#)
                Else
                    u = -(-u) ^ (1.0# / 3.0#)
                End If
                v = -q / 2 - Sqrt(a)
                If v >= 0 Then
                    v = v ^ (1.0# / 3.0#)
                Else
                    v = -(-v) ^ (1.0# / 3.0#)
                End If
                If a = 0 Then
                    '2 equal + 1 real solution
                    get_CubicRoots = 3
                    x1 = u + v - R / 3.0#
                    x2 = -(u + v) / 2 - R / 3.0#
                    x3 = x2
                Else
                    get_CubicRoots = 1
                    '2 complex solutions + 1 real solution
                    x1 = u + v - R / 3.0#
                    x2 = -(u + v) / 2 - R / 3.0#    'real part
                    x3 = (u - v) * 0.5 * Sqrt(3)   'imag part
                End If
                Exit Function
            End If

            '3 real solutions
            get_CubicRoots = 3
            a = Sqrt(-p ^ 3 / 27.0#)
            phi = arccos(-q / (2.0# * a))

            If phi = ErrorReturn Then
                get_CubicRoots = 0
                Exit Function
            End If

            a = 2.0# * a ^ (1.0# / 3.0#)
            x1 = a * Cos(phi / 3.0#) - R / 3.0#
            x2 = a * Cos((phi + 2.0# * PI) / 3.0#) - R / 3.0#
            x3 = a * Cos((phi + 4.0# * PI) / 3.0#) - R / 3.0#

        End Function


        Public Function matrix_solve(ByRef a(,) As Double, _
                                        ByRef b() As Double, _
                                        ByRef x() As Double, _
                                        ByVal n As Long) As Long

            'solves a system of linear equation by matrix inversion

            Dim notok As Long, i As Long, j As Long

            notok = matrix_invert(a, n)
            If notok <> 0 Then  'singular
                matrix_solve = notok
                Exit Function
            End If

            For i = 1 To n
                x(i) = 0
                For j = 1 To n
                    x(i) = x(i) + a(i, j) * b(j)
                Next j
            Next i

            matrix_solve = 0

        End Function


        Private Function matrix_invert(ByRef a(,) As Double, ByVal n As Long) As Long

            'inverts a matrix in place by Gauss elimination without pivoting

            Dim i As Long, j As Long, k As Long

            For i = 1 To n
                If a(i, i) = 0 Then  'matrix singular
                    matrix_invert = i
                    Exit Function
                End If
                a(i, i) = 1.0# / a(i, i)
                For j = 1 To n
                    If j <> i Then
                        a(j, i) = -a(j, i) * a(i, i)
                        For k = 1 To n
                            If k <> i Then a(j, k) = a(j, k) + a(j, i) * a(i, k)
                        Next k
                    End If
                Next j
                For k = 1 To n
                    If k <> i Then a(i, k) = a(i, i) * a(i, k)
                Next k
            Next i

            matrix_invert = 0  'no error

        End Function


        Public Function EFormat(ByVal x As Double, _
                                Optional ByVal digits As Integer = 0, _
                                Optional ByVal Esymbol As String = "E") As String

            'converts a number x into a Fortran E format string
            'example: EFormat(273.15) = 0.27315E+3
            '         EFormat(-1) = -0.1E+1

            Dim i%, ex&, xe$, xx#, vz%, ee$, xa#, inc_ee%

            If x = 0 Then
                EFormat$ = " 0.0"
                Exit Function
            End If

            xe$ = Str(x)
            i = InStr(xe, "E")
            If i <> 0 Then
                ex& = Val(Mid(xe, i + 1)) + 1
                xx# = 0.1 * Val(Left(xe, i - 1))
                xe$ = Str(xx) + "E" + IIf(ex > 0, "+", "") + Trim(Str(ex))
                xe = Left(xe, 1) + "0" + Mid(xe, 2)
            Else
                vz% = Sign(x)
                xa# = Abs(x)
                xe$ = Trim(Str(xa))
                i = InStr(xe, ".")
                If i = 0 Then
                    ex& = Len(xe)
                    Do While Right(xe, 1) = "0"
                        xe = Left(xe, Len(xe) - 1)
                    Loop
                    xe = IIf(vz < 0, "-", " ") + "0." + xe + "E+" + Trim(Str(ex))
                Else
                    If Left(xe, 2) <> ".0" Then
                        ex& = i - 1
                        xe = IIf(vz < 0, "-", " ") + "0." + Left(xe, i - 1) + Mid(xe, i + 1) + "E+" + Trim(Str(ex))
                    Else
                        Do While Left(xe, 2) = ".0"
                            xe = "." + Mid(xe, 3)
                            ex = ex - 1
                        Loop
                        xe = IIf(vz < 0, "-", " ") + "0." + Left(xe, i - 1) + Mid(xe, i + 1) + _
                                "E" + IIf(Sign(ex) < 0, "", "+") + Trim(Str(ex))
                    End If
                End If
            End If

            If Right(xe, 3) = "E+0" Then xe = Left(xe, Len(xe) - 3)

            If digits > 0 Then
                vz% = 1
                If Left(xe, 1) = "-" Then
                    vz = -1
                    xe = Mid(xe, 2)
                End If
                i = InStr(xe, "E")
                If i > 0 Then
                    ee$ = Mid(xe, i)
                    xe = Left(xe, i - 1)
                Else
                    ee$ = ""
                End If
                If digits < 16 Then
                    xe = Trim(Str(Round(Val(xe), digits)))
                    If xe = "1" Then
                        xe = ".1"
                        inc_ee = 1
                    Else
                        inc_ee = 0
                    End If
                End If
                If Len(xe) < digits + 1 Then
                    xe = xe + Format(digits + 1 - Len(xe), "0")
                End If
                If Left(xe, 1) = "." Then xe = "0" + xe
                If inc_ee = 1 Then
                    If Len(ee) > 2 Then 'increment exponent by 1
                        ee = Trim(Str(Val(Mid(ee, 2)) + 1))
                        If ee = "0" Then
                            ee = ""
                        Else
                            Select Case Left(ee, 1)
                                Case "+", "-"
                                Case Else : ee = "+" + ee
                            End Select
                            ee = "E" + ee
                        End If
                    ElseIf ee = "" Then
                        ee = "E+1"
                    End If
                End If
                xe = xe + ee$
                If vz < 0 Then xe = "-" + xe Else xe = " " + xe
            End If

            If Esymbol$ <> "E" Then
                i = InStr(xe, "E")
                If i <> 0 Then Mid(xe, i, 1) = Esymbol$
            End If

            EFormat$ = xe

        End Function


        Function TwoCols(ByVal x1 As Double, ByVal x2 As Double) As String

            Const wid = 24

            Dim col1 As String, col2 As String

            col1 = EFormat(x1)
            col2 = EFormat(x2)

            If col1 = col2 Then
                TwoCols = Left(col1 + "," + Space(wid), wid) + col2
            Else
                TwoCols = Left(col1 + "," + Space(wid), wid) + _
                            Left(col2 + "," + Space(wid), wid) + _
                            "Diff = " + EFormat(x1 - x2)
            End If

        End Function


#End Region

#Region "Flu_1"

        '#########################################################################

        'This module requires the library module
        '     Constants_0_Mdl, file Constants_0.bas
        '     Maths_0_Mdl,     file Maths_0.bas

        '#########################################################################


        'This module implements the Helmholtz potential of fluid water and its
        'first and second partial derivatives with respect to temperature and
        'density as defined in IAPWS-95:

        'Release on the IAPWS Formulation 1995 for the Thermodynamic Properties of
        'Ordinary Water Substance for General and Scientific Use
        'The International Association for the Properties of Water and Steam
        'Fredericia, Denmark, September 1996,
        'revised Doorwerth, The Netherlands, September 2009

        'with an extension to 50 - 130 K of the ideal-gas part, as described in

        'Feistel, R., Kretzschmar, H.-J., Span, R., Hagen, E., Wright, D.G., Herrmann, S.:
        'Thermodynamic Properties of Sea Air.
        'Ocean Science Discussion 6(2009)2193–2325.

        'Implementation in VB6 by Rainer Feistel
        'for publication in Ocean Science, as described in the papers

        'Feistel, R., Wright, D.G., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part I: Background and Equations.
        'Ocean Science, 2009

        'Wright, D.G., Feistel, R., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part II: The Library Routines,
        'Ocean Science, 2009


        'Private Const ErrorReturn = 9.99999999E+98

        'Arrays defined in Table 1 of IAPWS-95
        'Numerical values of the coefficients and parameters of the ideal-gas
        'part of the dimensionless Helmholtz function and energy, Eq.(5)

        Private n0(8) As Double
        Private gam0(8) As Double

        'Arrays defined in Table 2
        'Numerical values of the coefficients and parameters of the
        'residual part of the dimensionless Helmholtz function and energy, Eq.(6)

        Private ai(56) As Double
        Private bi(56) As Double
        Private ci(56) As Double
        Private di(56) As Double
        Private ni(56) As Double
        Private ti(56) As Double

        Private aa(56) As Double
        Private bb(56) As Double
        Private cc(56) As Double
        Private dd(56) As Double

        Private alf(56) As Double
        Private bet(56) As Double
        Private gam(56) As Double
        Private eps(56) As Double

        'cp extension below 130 K
        Private ee As Double

        'Critical Point
        Private Const rhoc = CP_density_si   'kg/m3
        Private Const Tc = CP_temperature_si  'K


        Public Function flu_f_si(ByVal drv_t As Integer, _
                                    ByVal drv_d As Integer, _
                                    ByVal t_si As Double, _
                                    ByVal d_si As Double) As Double

            'return value:
            'flu_f_si:          derivative of the Helmholtz function in the basic SI unit

            'input parameters:
            'drv_t:             order of the partial temperature derivative
            'drv_d:             order of the partial density derivative
            't_si:              absolute temperature ITS-90 in K
            'd_si:              density in kg/m^3

            'Check values:
            'flu_f_si(0,0,300,1000) = -5351.74115204056
            'flu_f_si(1,0,300,1000) = -390.904170767491
            'flu_f_si(0,1,300,1000) = 7.83300135596|477
            'flu_f_si(2,0,300,1000) = -13.6840204925353
            'flu_f_si(1,1,300,1000) = 0.639359046588391
            'flu_f_si(0,2,300,1000) = 2.24824656167368

            Dim del As Double, tau As Double
            Dim RT As Double
            Dim f As Double, ft As Double, fd As Double, fdt As Double, ftt As Double

            'Const R = Gas_constant_H2O_si  'specific gas constant of H2O in J/(kg K)
            Const R = Gas_constant_H2O_IAPWS95 'value used in the IAPWS-95 Release

            flu_f_si = ErrorReturn

            'exclude illegal calls:
            If check_limits = 1 Then
                If t_si < flu_tmin Or t_si > flu_tmax Then Exit Function
                If d_si <= flu_dmin Or d_si > flu_dmax Then Exit Function
            Else
                If t_si <= 0 Then Exit Function
                If d_si <= 0 Then Exit Function
            End If

            'reduce input values:
            tau = Tc / t_si
            del = d_si / rhoc

            RT = R * t_si

            InitIAPWS95()

            'expressions from Table 4 of the IAPWS Advisory Note #3, revised, Berlin 2008:
            Select Case drv_t

                Case 0
                    Select Case drv_d

                        Case 0 : flu_f_si = RT * (phi0_(del, tau) + phir_(del, tau))

                        Case 1 : flu_f_si = RT / rhoc * (phi0_d(del, tau) + phir_d(del, tau))

                        Case 2 : flu_f_si = RT / rhoc ^ 2 * (phi0_dd(del, tau) + phir_dd(del, tau))

                        Case Else : Exit Function
                    End Select

                Case 1
                    Select Case drv_d

                        Case 0 : f = phi0_(del, tau) + phir_(del, tau)
                            ft = phi0_t(del, tau) + phir_t(del, tau)
                            flu_f_si = R * (f - tau * ft)

                        Case 1 : fd = phi0_d(del, tau) + phir_d(del, tau)
                            fdt = phi0_dt(del, tau) + phir_dt(del, tau)
                            flu_f_si = R / rhoc * (fd - tau * fdt)

                        Case Else : Exit Function
                    End Select

                Case 2
                    Select Case drv_d

                        Case 0 : ftt = phi0_tt(del, tau) + phir_tt(del, tau)
                            flu_f_si = R / t_si * tau ^ 2 * ftt

                        Case Else : Exit Function
                    End Select

                Case Else : Exit Function
            End Select

        End Function


        Private Function delta_(ByVal i As Integer, ByVal del As Double, ByVal tau As Double) As Double

            'Function delta as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            'away from and at the critical density
            delta_ = theta_(i, del, tau) ^ 2 + bb(i) * Abs(del - 1.0#) ^ (2.0# * ai(i))


        End Function


        Private Function delta_d(ByVal i As Integer, ByVal del As Double, ByVal tau As Double) As Double

            'Function delta_d as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            If del = 1 Then  'at the critical density
                delta_d = 0
            Else
                delta_d = (del - 1.0#) * (aa(i) * theta_(i, del, tau) * 2.0# / bet(i) * Abs(del - 1.0#) ^ (1.0# / bet(i) - 2.0#) _
                            + 2.0# * bb(i) * ai(i) * Abs(del - 1.0#) ^ (2.0# * ai(i) - 2.0#))
            End If

        End Function


        Private Function delta_dd(ByVal i As Integer, ByVal del As Double, ByVal tau As Double) As Double

            'Function delta_dd as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            If del = 1 Then  'at the critical density
                delta_dd = 0
            Else
                delta_dd = delta_d(i, del, tau) / (del - 1) + _
                            4 * bb(i) * ai(i) * (ai(i) - 1) * Abs(del - 1) ^ (2 * ai(i) - 2) + _
                            2 * (aa(i) / bet(i)) ^ 2 * Abs(del - 1) ^ (2 / bet(i) - 2) + _
                            2 * theta_(i, del, tau) * aa(i) / bet(i) * (1 / bet(i) - 2) * Abs(del - 1) ^ (1 / bet(i) - 2)
            End If

        End Function


        Private Function deltab_d(ByVal i As Integer, ByVal del As Double, ByVal tau As Double) As Double

            'Function delta^bi_d as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            Dim delta As Double

            delta = delta_(i, del, tau)

            If delta = 0 Then
                deltab_d = 0
            Else
                deltab_d = bi(i) * delta ^ (bi(i) - 1.0#) * delta_d(i, del, tau)
            End If

        End Function


        Private Function deltab_dt(ByVal i As Integer, ByVal del As Double, ByVal tau As Double) As Double

            'Function delta^bi_dt as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            Dim delta As Double

            delta = delta_(i, del, tau)

            If delta = 0 Then
                deltab_dt = 0
            Else
                deltab_dt = -2 * bi(i) * delta ^ (bi(i) - 2) * _
                                (aa(i) / bet(i) * delta * (del - 1.0#) * Abs(del - 1.0#) ^ (1.0# / bet(i) - 2.0#) + _
                                theta_(i, del, tau) * (bi(i) - 1) * delta_d(i, del, tau))
            End If

        End Function


        Private Function deltab_t(ByVal i As Integer, ByVal del As Double, ByVal tau As Double) As Double

            'Function delta^bi_t as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            Dim delta As Double

            delta = delta_(i, del, tau)
            If delta = 0 Then
                deltab_t = 0
            Else
                deltab_t = -2 * theta_(i, del, tau) * bi(i) * delta ^ (bi(i) - 1)
            End If

        End Function


        Private Function deltab_tt(ByVal i As Integer, ByVal del As Double, ByVal tau As Double) As Double

            'Function delta^bi_tt as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            Dim delta As Double

            delta = delta_(i, del, tau)
            If delta = 0 Then
                deltab_tt = 0
            Else
                deltab_tt = 2 * bi(i) * delta ^ (bi(i) - 2) * (delta + 2.0# * theta_(i, del, tau) ^ 2 * (bi(i) - 1))
            End If
        End Function


        Private Function deltab_dd(ByVal i As Integer, ByVal del As Double, ByVal tau As Double) As Double

            'Function delta^bi_dd as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            Dim delta As Double

            delta = delta_(i, del, tau)
            If delta = 0 Then
                deltab_dd = 0
            Else
                deltab_dd = bi(i) * (delta ^ (bi(i) - 1) * delta_dd(i, del, tau) + _
                            (bi(i) - 1) * delta ^ (bi(i) - 2) * delta_d(i, del, tau) ^ 2)
            End If
        End Function


        Private Function theta_(ByVal i As Integer, ByVal del As Double, ByVal tau As Double) As Double

            'Function theta as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            If del = 1 Then  'at the critical density
                theta_ = 1.0# - tau
            Else
                theta_ = 1.0# - tau + aa(i) * Abs(del - 1.0#) ^ (1.0# / bet(i))
            End If

        End Function


        Private Function psi_(ByVal i As Integer, ByVal del As Double, ByVal tau As Double) As Double

            'Function psi as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            psi_ = Exp(-cc(i) * (del - 1.0#) ^ 2 - dd(i) * (tau - 1.0#) ^ 2)

        End Function


        Private Function psi_t(ByVal i As Integer, ByVal del As Double, ByVal tau As Double) As Double

            'Function psi_t as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            psi_t = -2 * dd(i) * (tau - 1.0#) * psi_(i, del, tau)

        End Function


        Private Function psi_tt(ByVal i As Integer, ByVal del As Double, ByVal tau As Double) As Double

            'Function psi_tt as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            psi_tt = 2 * dd(i) * (2 * dd(i) * (tau - 1.0#) ^ 2 - 1) * psi_(i, del, tau)

        End Function


        Private Function psi_d(ByVal i As Integer, ByVal del As Double, ByVal tau As Double) As Double

            'Function psi_d as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            psi_d = -2.0# * cc(i) * (del - 1.0#) * psi_(i, del, tau)

        End Function


        Private Function psi_dt(ByVal i As Integer, ByVal del As Double, ByVal tau As Double) As Double

            'Function psi_dt as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            psi_dt = 4.0# * cc(i) * dd(i) * (del - 1.0#) * (tau - 1.0#) * psi_(i, del, tau)

        End Function


        Private Function psi_dd(ByVal i As Integer, ByVal del As Double, ByVal tau As Double) As Double

            'Function psi_dd as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            psi_dd = 2.0# * cc(i) * (2 * cc(i) * (del - 1.0#) ^ 2 - 1.0#) * psi_(i, del, tau)

        End Function


        Private Sub InitIAPWS95()

            Dim i As Integer

            If di(1) = 1 Then Exit Sub

            'Coefficients defined in Table 1 of IAPWS-95
            'Numerical values of the coefficients and parameters of the ideal-gas
            'part of the dimensionless Helmholtz free energy, Eq.(5)
            '-------------------------------------------------------------------------------

            'adjustment to the IAPWS-95 reference state conditions at the triple point
            'original values in the IAPWS-95 Release
            'i = 1: n0(i) = -8.32044648201
            'i = 2: n0(i) = 6.6832105268

            'improved values obtained from a quadruple precision implementation
            i = 1 : n0(i) = -8.32044648374969
            i = 2 : n0(i) = 6.68321052759323

            i = 3 : n0(i) = 3.00632
            i = 4 : n0(i) = 0.012436 : gam0(i) = 1.28728967
            i = 5 : n0(i) = 0.97315 : gam0(i) = 3.53734222
            i = 6 : n0(i) = 1.2795 : gam0(i) = 7.74073708
            i = 7 : n0(i) = 0.96956 : gam0(i) = 9.24437796
            i = 8 : n0(i) = 0.24873 : gam0(i) = 27.5075105
            '-------------------------------------------------------------------------------

            'Coefficients defined in Table 2 of IAPWS-95
            'Numerical values of the coefficients and parameters of the
            'residual part of the dimensionless Helmholtz free energy, Eq.(6)
            '-------------------------------------------------------------------------------
            i = 1 : di(i) = 1 : ti(i) = -0.5 : ni(i) = 0.012533547935523
            i = 2 : di(i) = 1 : ti(i) = 0.875 : ni(i) = 7.8957634722828
            i = 3 : di(i) = 1 : ti(i) = 1 : ni(i) = -8.7803203303561
            i = 4 : di(i) = 2 : ti(i) = 0.5 : ni(i) = 0.31802509345418
            i = 5 : di(i) = 2 : ti(i) = 0.75 : ni(i) = -0.26145533859358
            i = 6 : di(i) = 3 : ti(i) = 0.375 : ni(i) = -0.0078199751687981
            i = 7 : di(i) = 4 : ti(i) = 1 : ni(i) = 0.0088089493102134
            i = 8 : ci(i) = 1 : di(i) = 1 : ti(i) = 4 : ni(i) = -0.66856572307965
            i = 9 : ci(i) = 1 : di(i) = 1 : ti(i) = 6 : ni(i) = 0.20433810950965
            i = 10 : ci(i) = 1 : di(i) = 1 : ti(i) = 12 : ni(i) = -0.000066212605039687
            i = 11 : ci(i) = 1 : di(i) = 2 : ti(i) = 1 : ni(i) = -0.19232721156002
            i = 12 : ci(i) = 1 : di(i) = 2 : ti(i) = 5 : ni(i) = -0.25709043003438
            i = 13 : ci(i) = 1 : di(i) = 3 : ti(i) = 4 : ni(i) = 0.16074868486251
            i = 14 : ci(i) = 1 : di(i) = 4 : ti(i) = 2 : ni(i) = -0.040092828925807
            i = 15 : ci(i) = 1 : di(i) = 4 : ti(i) = 13 : ni(i) = 0.00000039343422603254
            i = 16 : ci(i) = 1 : di(i) = 5 : ti(i) = 9 : ni(i) = -0.0000075941377088144
            i = 17 : ci(i) = 1 : di(i) = 7 : ti(i) = 3 : ni(i) = 0.00056250979351888
            i = 18 : ci(i) = 1 : di(i) = 9 : ti(i) = 4 : ni(i) = -0.000015608652257135
            i = 19 : ci(i) = 1 : di(i) = 10 : ti(i) = 11 : ni(i) = 0.0000000011537996422951
            i = 20 : ci(i) = 1 : di(i) = 11 : ti(i) = 4 : ni(i) = 0.00000036582165144204
            i = 21 : ci(i) = 1 : di(i) = 13 : ti(i) = 13 : ni(i) = -0.0000000000013251180074668
            i = 22 : ci(i) = 1 : di(i) = 15 : ti(i) = 1 : ni(i) = -0.00000000062639586912454
            i = 23 : ci(i) = 2 : di(i) = 1 : ti(i) = 7 : ni(i) = -0.10793600908932
            i = 24 : ci(i) = 2 : di(i) = 2 : ti(i) = 1 : ni(i) = 0.017611491008752
            i = 25 : ci(i) = 2 : di(i) = 2 : ti(i) = 9 : ni(i) = 0.22132295167546
            i = 26 : ci(i) = 2 : di(i) = 2 : ti(i) = 10 : ni(i) = -0.40247669763528
            i = 27 : ci(i) = 2 : di(i) = 3 : ti(i) = 10 : ni(i) = 0.58083399985759
            i = 28 : ci(i) = 2 : di(i) = 4 : ti(i) = 3 : ni(i) = 0.0049969146990806
            i = 29 : ci(i) = 2 : di(i) = 4 : ti(i) = 7 : ni(i) = -0.031358700712549
            i = 30 : ci(i) = 2 : di(i) = 4 : ti(i) = 10 : ni(i) = -0.74315929710341
            i = 31 : ci(i) = 2 : di(i) = 5 : ti(i) = 10 : ni(i) = 0.4780732991548
            i = 32 : ci(i) = 2 : di(i) = 6 : ti(i) = 6 : ni(i) = 0.020527940895948
            i = 33 : ci(i) = 2 : di(i) = 6 : ti(i) = 10 : ni(i) = -0.13636435110343
            i = 34 : ci(i) = 2 : di(i) = 7 : ti(i) = 10 : ni(i) = 0.014180634400617
            i = 35 : ci(i) = 2 : di(i) = 9 : ti(i) = 1 : ni(i) = 0.0083326504880713
            i = 36 : ci(i) = 2 : di(i) = 9 : ti(i) = 2 : ni(i) = -0.029052336009585
            i = 37 : ci(i) = 2 : di(i) = 9 : ti(i) = 3 : ni(i) = 0.038615085574206
            i = 38 : ci(i) = 2 : di(i) = 9 : ti(i) = 4 : ni(i) = -0.020393486513704
            i = 39 : ci(i) = 2 : di(i) = 9 : ti(i) = 8 : ni(i) = -0.0016554050063734
            i = 40 : ci(i) = 2 : di(i) = 10 : ti(i) = 6 : ni(i) = 0.0019955571979541
            i = 41 : ci(i) = 2 : di(i) = 10 : ti(i) = 9 : ni(i) = 0.00015870308324157
            i = 42 : ci(i) = 2 : di(i) = 12 : ti(i) = 8 : ni(i) = -0.00001638856834253
            i = 43 : ci(i) = 3 : di(i) = 3 : ti(i) = 16 : ni(i) = 0.043613615723811
            i = 44 : ci(i) = 3 : di(i) = 4 : ti(i) = 22 : ni(i) = 0.034994005463765
            i = 45 : ci(i) = 3 : di(i) = 4 : ti(i) = 23 : ni(i) = -0.076788197844621
            i = 46 : ci(i) = 3 : di(i) = 5 : ti(i) = 23 : ni(i) = 0.022446277332006
            i = 47 : ci(i) = 4 : di(i) = 14 : ti(i) = 10 : ni(i) = -0.000062689710414685
            i = 48 : ci(i) = 6 : di(i) = 3 : ti(i) = 50 : ni(i) = -0.00000000055711118565645
            i = 49 : ci(i) = 6 : di(i) = 6 : ti(i) = 44 : ni(i) = -0.19905718354408
            i = 50 : ci(i) = 6 : di(i) = 6 : ti(i) = 46 : ni(i) = 0.31777497330738
            i = 51 : ci(i) = 6 : di(i) = 6 : ti(i) = 50 : ni(i) = -0.11841182425981
            '------------------------------------------------------------------------------------------------------------------------------
            i = 52 : di(i) = 3 : ti(i) = 0 : ni(i) = -31.306260323435 : alf(i) = 20 : bet(i) = 150 : gam(i) = 1.21 : eps(i) = 1
            i = 53 : di(i) = 3 : ti(i) = 1 : ni(i) = 31.546140237781 : alf(i) = 20 : bet(i) = 150 : gam(i) = 1.21 : eps(i) = 1
            i = 54 : di(i) = 3 : ti(i) = 4 : ni(i) = -2521.3154341695 : alf(i) = 20 : bet(i) = 250 : gam(i) = 1.25 : eps(i) = 1
            '------------------------------------------------------------------------------------------------------------------------------
            i = 55 : ai(i) = 3.5 : bi(i) = 0.85 : bb(i) = 0.2 : ni(i) = -0.14874640856724 : cc(i) = 28 : dd(i) = 700 : aa(i) = 0.32 : bet(i) = 0.3
            i = 56 : ai(i) = 3.5 : bi(i) = 0.95 : bb(i) = 0.2 : ni(i) = 0.31806110878444 : cc(i) = 32 : dd(i) = 800 : aa(i) = 0.32 : bet(i) = 0.3
            '------------------------------------------------------------------------------------------------------------------------------

            'cp extension below 130 K
            ee = 0.278296458178592

        End Sub


        Private Function phi0_(ByVal del As Double, ByVal tau As Double) As Double

            'Function phi0 as defined in row #1 of Table 4 of IAPWS-95:
            'The ideal-gas part phi0 of the dimensionless Helmholtz free energy and its derivatives

            Const e = Tc / 130.0#

            Dim phi As Double
            Dim i As Integer

            phi = Log(del) + n0(1) + n0(2) * tau + n0(3) * Log(tau)
            For i = 4 To 8
                phi = phi + n0(i) * Log(1.0# - Exp(-gam0(i) * tau))
            Next i

            If tau > e Then  'extension below 130 K
                phi = phi + ee * (-0.5 / tau - 3.0# / e ^ 2 * (tau + e) * Log(tau / e) _
                                    + 4.5 * tau / e ^ 2 + 0.5 * tau ^ 2 / e ^ 3 - 4.5 / e)
            End If

            phi0_ = phi

        End Function


        Private Function phi0_t(ByVal del As Double, ByVal tau As Double) As Double

            'Function phi0_t as defined in row #4 of Table 4 of IAPWS-95:
            'The ideal-gas part phi0 of the dimensionless Helmholtz free energy and its derivatives

            Const e = Tc / 130.0#

            Dim phi As Double
            Dim i As Integer

            phi = n0(2) + n0(3) / tau
            For i = 4 To 8
                phi = phi + n0(i) * gam0(i) * (1.0# / (1.0# - Exp(-gam0(i) * tau)) - 1.0#)
            Next i

            If tau > e Then  'extension below 130 K
                phi = phi + ee * (0.5 / tau ^ 2 - 3.0# / e ^ 2 * Log(tau / e) _
                                    - 3.0# / (e * tau) + tau / e ^ 3 + 1.5 / e ^ 2)
            End If

            phi0_t = phi

        End Function


        Private Function phi0_tt(ByVal del As Double, ByVal tau As Double) As Double

            'Function phi0_tt as defined in row #5 of Table 4 of IAPWS-95:
            'The ideal-gas part phi0 of the dimensionless Helmholtz free energy and its derivatives

            Const e = Tc / 130.0#

            Dim phi As Double
            Dim i As Integer

            phi = -n0(3) / tau ^ 2
            For i = 4 To 8
                phi = phi - n0(i) * gam0(i) ^ 2 * Exp(-gam0(i) * tau) / (1.0# - Exp(-gam0(i) * tau)) ^ 2
            Next i

            If tau > e Then  'extension below 130 K
                phi = phi + ee * (-1.0# / tau + 1.0# / e) ^ 3
            End If

            phi0_tt = phi

        End Function


        Private Function phi0_d(ByVal del As Double, ByVal tau As Double) As Double

            'Function phi0_d as defined in row #2 of Table 4 of IAPWS-95:
            'The ideal-gas part phi0 of the dimensionless Helmholtz free energy and its derivatives

            phi0_d = 1.0# / del

        End Function


        Private Function phi0_dd(ByVal del As Double, ByVal tau As Double) As Double

            'Function phi0_dd as defined in row #3 of Table 4 of IAPWS-95:
            'The ideal-gas part phi0 of the dimensionless Helmholtz free energy and its derivatives

            phi0_dd = -1.0# / del ^ 2

        End Function


        Private Function phi0_dt(ByVal del As Double, ByVal tau As Double) As Double

            'Function phi0_dt as defined in row #6 of Table 4 of IAPWS-95:
            'The ideal-gas part phi0 of the dimensionless Helmholtz free energy and its derivatives

            phi0_dt = 0

        End Function


        Private Function phir_(ByVal del As Double, ByVal tau As Double) As Double

            'Function phir as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            Dim phi As Double
            Dim i As Integer

            phi = 0

            For i = 1 To 7
                phi = phi + ni(i) * del ^ di(i) * tau ^ ti(i)
            Next i

            For i = 8 To 51
                phi = phi + ni(i) * del ^ di(i) * tau ^ ti(i) * Exp(-del ^ ci(i))
            Next i

            For i = 52 To 54
                phi = phi + ni(i) * del ^ di(i) * tau ^ ti(i) * _
                            Exp(-alf(i) * (del - eps(i)) ^ 2 - bet(i) * (tau - gam(i)) ^ 2)
            Next i

            For i = 55 To 56
                phi = phi + ni(i) * delta_(i, del, tau) ^ bi(i) * del * psi_(i, del, tau)
            Next i

            phir_ = phi

        End Function


        Private Function phir_d(ByVal del As Double, ByVal tau As Double) As Double

            'Function phir_d as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            Dim phi As Double, psi As Double
            Dim i As Integer

            phi = 0

            For i = 1 To 7
                phi = phi + ni(i) * di(i) * del ^ (di(i) - 1) * tau ^ ti(i)
            Next i

            For i = 8 To 51
                phi = phi + ni(i) * Exp(-del ^ ci(i)) * (del ^ (di(i) - 1) * tau ^ ti(i) * (di(i) - ci(i) * del ^ ci(i)))
            Next i

            For i = 52 To 54
                phi = phi + ni(i) * del ^ di(i) * tau ^ ti(i) * _
                            Exp(-alf(i) * (del - eps(i)) ^ 2 - bet(i) * (tau - gam(i)) ^ 2) * _
                            (di(i) / del - 2.0# * alf(i) * (del - eps(i)))
            Next i

            For i = 55 To 56
                psi = psi_(i, del, tau)
                phi = phi + ni(i) * (delta_(i, del, tau) ^ bi(i) * _
                            (psi + del * psi_d(i, del, tau)) + deltab_d(i, del, tau) * del * psi)
            Next i

            phir_d = phi

        End Function


        Private Function phir_dd(ByVal del As Double, ByVal tau As Double) As Double

            'Function phir_dd as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            Dim phi As Double, psi As Double, psid As Double, tmp As Double
            Dim i As Integer

            phi = 0

            For i = 1 To 7
                phi = phi + ni(i) * di(i) * (di(i) - 1) * del ^ (di(i) - 2) * tau ^ ti(i)
            Next i

            For i = 8 To 51
                tmp = (del ^ (di(i) - 2) * tau ^ ti(i) * _
                        ((di(i) - ci(i) * del ^ ci(i)) * (di(i) - 1 - ci(i) * del ^ ci(i)) - ci(i) ^ 2 * del ^ ci(i)))
                phi = phi + ni(i) * Exp(-del ^ ci(i)) * tmp
            Next i

            For i = 52 To 54
                phi = phi + ni(i) * tau ^ ti(i) * _
                            Exp(-alf(i) * (del - eps(i)) ^ 2 - bet(i) * (tau - gam(i)) ^ 2) * _
                            (-2 * alf(i) * del ^ di(i) + 4 * alf(i) ^ 2 * del ^ di(i) * (del - eps(i)) ^ 2 _
                            - 4 * di(i) * alf(i) * del ^ (di(i) - 1) * (del - eps(i)) + _
                            di(i) * (di(i) - 1) * del ^ (di(i) - 2))
            Next i

            For i = 55 To 56
                psi = psi_(i, del, tau)
                psid = psi_d(i, del, tau)
                phi = phi + ni(i) * (delta_(i, del, tau) ^ bi(i) * (2 * psid + del * psi_dd(i, del, tau)) + _
                            2 * deltab_d(i, del, tau) * (psi + del * psid) + deltab_dd(i, del, tau) * del * psi)
            Next i

            phir_dd = phi

        End Function


        Private Function phir_t(ByVal del As Double, ByVal tau As Double) As Double

            'Function phir_t as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            Const fastmode = False
            'flu_entropy_si(647, 358) = 4320.923066755  correct

            'Const fastmode = True
            'flu_entropy_si(647, 358) = 4320.92306292204  less correct

            If fastmode Then GoTo JackettCode

            Dim phi As Double
            Dim i As Integer

            phi = 0

            For i = 1 To 7
                phi = phi + ni(i) * ti(i) * del ^ di(i) * tau ^ (ti(i) - 1)
            Next i

            For i = 8 To 51
                phi = phi + ni(i) * ti(i) * del ^ di(i) * tau ^ (ti(i) - 1) * Exp(-del ^ ci(i))
            Next i

            For i = 52 To 54
                phi = phi + ni(i) * del ^ di(i) * tau ^ ti(i) * _
                            Exp(-alf(i) * (del - eps(i)) ^ 2 - bet(i) * (tau - gam(i)) ^ 2) * _
                            (ti(i) / tau - 2 * bet(i) * (tau - gam(i)))
            Next i

            For i = 55 To 56
                phi = phi + ni(i) * del * (deltab_t(i, del, tau) * psi_(i, del, tau) + _
                                            delta_(i, del, tau) ^ bi(i) * psi_t(i, del, tau))
            Next i

            phir_t = phi

            Exit Function



            'fast version adopted from David Jackett's Fortran code


JackettCode:

            '!    First derivative (with respect to tau) of the residual part of the
            '!    dimensionless Helmholtz free energy of fluid water, as in
            '!
            '!    The International Association for the Properties of Water and Steam
            '!    Fredericia, Denmark, September 1996
            '!
            '!    delta               : dimensionless density
            '!    tau                 : dimensionless temperature
            '!
            '!    DphirDtau           : first derivative of phir (wrt tau)
            '!
            '!
            '!    David Jackett       : 10/04/08

            Dim delta#, delta2#, delta3#, delta4#, delta6#, delta13#, delta14#

            Dim delta1_2#, delta_tau#, delta_tau2#, delta_tau3#, delta_tau4#, delta2_tau#

            Dim tau2#, tau3#, tau4#, tau5#, tau6#, tau7#

            Dim tau8#, tau9#, tau10#, tau11#, tau13#, tau12#, tau15#, tau16#, tau21#, tau22#, tau23#, tau43#, tau44#, tau49#, tau50#

            Dim taup375#, taup5#, taup75#, taup875#

            Dim tau1_2#, tau2_2#, tau3_2#, tau_1#

            Dim cd1#, cd2#, cd3#, cd4#, cd6#, cdt1#, cdt2#, cdt3#, cdt4#

            Dim edt1#, edt2#, edt3#, edt4#, the_rest#

            delta = del
            delta2 = delta * delta : delta3 = delta * delta2 : delta4 = delta * delta3 : delta6 = delta3 * delta3
            delta13 = delta3 * delta4 * delta6 : delta14 = delta * delta13

            delta1_2 = (-1.0# + delta) * (-1.0# + delta)

            tau2 = tau * tau : tau3 = tau * tau2 : tau4 = tau * tau3 : tau5 = tau * tau4 : tau6 = tau * tau5
            tau7 = tau * tau6 : tau8 = tau * tau7 : tau9 = tau * tau8 : tau10 = tau * tau9 : tau11 = tau2 * tau9
            tau12 = tau * tau11 : tau13 = tau4 * tau9 : tau15 = tau4 * tau11
            tau16 = tau8 * tau8 : tau21 = tau5 * tau16 : tau22 = tau9 * tau13 : tau23 = tau * tau22
            tau43 = tau21 * tau22 : tau44 = tau22 * tau22 : tau49 = tau5 * tau44 : tau50 = tau44 * tau6

            taup375 = tau ^ 0.375 : taup5 = Sqrt(tau) : taup75 = taup375 * taup375 : taup875 = taup375 * taup5

            tau1_2 = (-1.21 + tau) * (-1.21 + tau) : tau2_2 = (-1.25 + tau) * (-1.25 + tau)
            tau3_2 = (-1.0# + tau) * (-1.0# + tau)


            cd1 = delta * (0.0000000126917960652461 * tau10 + _
                                    delta * (0.00000146328660576816 * tau3 + _
                                    delta2 * (-0.00000000062639586912454 * delta2 - 0.0000000000172265340970684 * tau12)))
            cd1 = delta * (-0.0000683472393793296 * tau8 + _
                                delta2 * (0.00168752938055664 * tau2 + _
                                delta2 * (-0.00006243460902854 * tau3 + cd1)))
            cd1 = delta * (0.64299473945004 * tau3 + delta * _
                            (tau * (-0.080185657851614 + 0.00000511464493842302 * tau11) + cd1))
            cd1 = delta * (tau3 * (-2.6742628923186 + tau2 * (1.2260286570579 - 0.000794551260476244 * tau6)) + _
                    delta * (-0.19232721156002 - 1.2854521501719 * tau4 + cd1))


            cd2 = delta * (-0.00013110854674024 * delta2 * tau7 + _
                                    tau5 * (0.0119733431877246 + 0.00142832774917413 * tau3))
            cd2 = cd2 + tau * (-0.05810467201917 + _
                                    tau * (tau * (-0.081573946054816 - 0.0132432400509872 * tau4) + 0.115845256722618))
            cd2 = delta * (tau5 * (0.123167645375688 - 1.3636435110343 * tau4) + _
                                delta * (0.14180634400617 * tau9 + _
                                delta2 * (0.0083326504880713 + cd2)))
            cd2 = (tau2 * (0.0149907440972418 + tau4 * (-0.219510904987843 - 7.4315929710341 * tau3)) + _
                            delta * (4.780732991548 * tau9 + cd2))
            cd2 = delta * (-0.75555206362524 * tau6 + delta * (0.017611491008752 + _
                        (1.99190656507914 - 4.0247669763528 * tau) * tau8 + _
                        delta * (5.8083399985759 * tau9 + delta * cd2)))


            cd3 = delta3 * (0.697817851580976 * tau15 + delta * _
                        ((0.76986812020283 - 1.76612855042628 * tau) * tau21 + 0.516264378636138 * delta * tau22))


            cd4 = -0.00062689710414685 * delta14 * tau9


            cd6 = delta3 * (-0.0000000278555592828225 * tau49 + _
                        delta3 * tau43 * (-8.75851607593952 + tau2 * (14.6176487721395 - 5.9205912129905 * tau4)))



            cdt1 = delta3 * (-11332.6263571691 + (20843.127003345 - 9463.8420713343 * tau) * tau)

            edt1 = Exp(-20.0# * delta1_2 - 150.0# * tau1_2)


            cdt2 = delta3 * tau3 * (-10085.261736678 + tau * (-1575822.14635594 + 1260657.71708475 * tau))

            edt2 = Exp(-20.0# * delta1_2 - 250.0# * tau2_2)


            delta_tau = (1.0# + 0.32 * delta1_2 ^ 1.66666666666667 - tau) : delta_tau2 = delta_tau * delta_tau : delta2_tau = delta * delta_tau

            delta_tau3 = (0.2 * delta1_2 ^ 3.5 + delta_tau2) : delta_tau4 = delta * tau_1

            If delta_tau3 = 0 Then
                cdt3 = 0
            Else
                cdt3 = delta_tau3 ^ 0.85 * ((0.252868894564308 * delta2_tau) / delta_tau3 + 208.244971994136 * delta_tau4)
            End If

            edt3 = Exp(-28.0# * delta1_2 - 700.0# * tau3_2)

            If delta_tau3 = 0 Then
                cdt4 = 0
            Else
                cdt4 = delta_tau3 ^ 0.95 * ((-0.604316106690436 * delta2_tau) / delta_tau3 - 508.897774055104 * delta_tau4)
            End If

            edt4 = Exp(-32.0# * delta1_2 - 800.0# * tau3_2)


            the_rest = -8.7803203303561 * delta + 0.0088089493102134 * delta4 - (0.0062667739677615 * delta * taup5) / tau2 + _
                                (-0.00293249068829929 * delta3 * taup375 + delta2 * (0.15901254672709 * taup5 - _
                                        0.196091503945185 * taup75) + 6.90879303824745 * delta * taup875) / tau


            phir_t = cd1 * Exp(-delta) + cd2 * Exp(-delta2) + cd3 * Exp(-delta3) + cd4 * Exp(-delta4) + cd6 * Exp(-delta6) + _
                            cdt1 * edt1 + cdt2 * edt2 + cdt3 * edt3 + cdt4 * edt4 + the_rest



        End Function


        Private Function phir_dt(ByVal del As Double, ByVal tau As Double) As Double

            'Function phir_dt as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            Dim phi As Double, psi As Double, psit As Double
            Dim i As Integer

            phi = 0

            For i = 1 To 7
                phi = phi + ni(i) * di(i) * ti(i) * del ^ (di(i) - 1) * tau ^ (ti(i) - 1)
            Next i

            For i = 8 To 51
                phi = phi + ni(i) * ti(i) * Exp(-del ^ ci(i)) * del ^ (di(i) - 1) * tau ^ (ti(i) - 1) * _
                            (di(i) - ci(i) * del ^ ci(i))
            Next i

            For i = 52 To 54
                phi = phi + ni(i) * del ^ di(i) * tau ^ ti(i) * _
                            Exp(-alf(i) * (del - eps(i)) ^ 2 - bet(i) * (tau - gam(i)) ^ 2) * _
                            (di(i) / del - 2.0# * alf(i) * (del - eps(i))) * _
                            (ti(i) / tau - 2 * bet(i) * (tau - gam(i)))
            Next i

            For i = 55 To 56
                psi = psi_(i, del, tau)
                psit = psi_t(i, del, tau)
                phi = phi + ni(i) * (delta_(i, del, tau) ^ bi(i) * (psit + del * psi_dt(i, del, tau)) + _
                            del * deltab_d(i, del, tau) * psit + _
                            deltab_t(i, del, tau) * (psi + del * psi_d(i, del, tau)) + _
                            deltab_dt(i, del, tau) * del * psi)
            Next i

            phir_dt = phi

        End Function


        Private Function phir_tt(ByVal del As Double, ByVal tau As Double) As Double

            'Function phir_tt as defined in Table 5 of IAPWS-95:
            'The residual part phir of the dimensionless Helmholtz free energy and its derivatives

            Dim phi As Double
            Dim i As Integer

            phi = 0

            For i = 1 To 7
                phi = phi + ni(i) * ti(i) * (ti(i) - 1) * del ^ di(i) * tau ^ (ti(i) - 2)
            Next i

            For i = 8 To 51
                phi = phi + ni(i) * ti(i) * (ti(i) - 1) * del ^ di(i) * tau ^ (ti(i) - 2) * Exp(-del ^ ci(i))
            Next i

            For i = 52 To 54
                phi = phi + ni(i) * del ^ di(i) * tau ^ ti(i) * _
                            Exp(-alf(i) * (del - eps(i)) ^ 2 - bet(i) * (tau - gam(i)) ^ 2) * _
                            ((ti(i) / tau - 2 * bet(i) * (tau - gam(i))) ^ 2 - ti(i) / tau ^ 2 - 2 * bet(i))
            Next i

            For i = 55 To 56
                phi = phi + ni(i) * del * (deltab_tt(i, del, tau) * psi_(i, del, tau) + _
                                        2 * deltab_t(i, del, tau) * psi_t(i, del, tau) + _
                                            delta_(i, del, tau) ^ bi(i) * psi_tt(i, del, tau))
            Next i

            phir_tt = phi

        End Function


        Public Function chk_IAPWS95_Table6() As String

            'Function values as given in Table 6 of IAPWS-95, revision of Doorwerth 2009:
            'Values for the ideal-gas part phi0, Eq. (5), and for the residual part phir , Eq. (6), of the
            'dimensionless Helmholtz free energy, together with the corresponding derivatives
            'for T = 500 K and rho = 838.025 kg m–3

            'phi0_ =     0.204 797 733E1  phir_ =   – 0.342 693 206E1
            'phi0_d =    0.384 236 747    phir_d =  – 0.364 366 650
            'phi0_dd = – 0.147 637 878    phir_dd =   0.856 063 701
            'phi0_t =    0.904 611 106E1  phir_t =  – 0.581 403 435E1
            'phi0_tt = – 0.193 249 185E1  phir_tt = – 0.223 440 737E1
            'phi0_dt =   0                phir_dt = – 0.112 176 915E1

            Dim CRLF As String, TB As String
            Dim txt As String
            Dim tau As Double, del As Double

            CRLF = Chr(13) + Chr(10)
            TB = Chr(9)

            txt = "Implementation of IAPWS-95 in Visual Basic" + CRLF
            txt = txt + "for Publication in Ocean Science, 2009" + CRLF
            txt = txt + "R. Feistel, IOW, Version " + Version + CRLF
            txt = txt + "Compiled on " + CStr(Now) + CRLF + CRLF

            txt = txt + "Function values as given in Table 6 of IAPWS-95:" + CRLF
            txt = txt + "Values for the ideal-gas part, phi0, Eq.(5)," + CRLF
            txt = txt + "for T = 500 K and rho = 838.025 kg/m^3" + CRLF + CRLF

            del = 838.025 / rhoc
            tau = Tc / 500.0#

            InitIAPWS95()

            txt = txt + "Call:    " + TB + " Table Value" + CRLF
            txt = txt + "phi0_    " + TB + EFormat(Val(" 0.204797733E1"), 9) + CRLF
            txt = txt + "this code" + TB + EFormat(phi0_(del, tau), 9) + CRLF + CRLF
            txt = txt + "phi0_d   " + TB + EFormat(Val(" 0.384236747"), 9) + CRLF
            txt = txt + "this code" + TB + EFormat(phi0_d(del, tau), 9) + CRLF + CRLF
            txt = txt + "phi0_dd  " + TB + EFormat(Val("-0.147637878"), 9) + CRLF
            txt = txt + "this code" + TB + EFormat(phi0_dd(del, tau), 9) + CRLF + CRLF
            txt = txt + "phi0_t   " + TB + EFormat(Val(" 0.904611106E1"), 9) + CRLF
            txt = txt + "this code" + TB + EFormat(phi0_t(del, tau), 9) + CRLF + CRLF
            txt = txt + "phi0_tt  " + TB + EFormat(Val("-0.193249185E1"), 9) + CRLF
            txt = txt + "this code" + TB + EFormat(phi0_tt(del, tau), 9) + CRLF + CRLF
            txt = txt + "phi0_dt  " + TB + EFormat(Val(" 0.0"), 9) + CRLF
            txt = txt + "this code" + TB + EFormat(phi0_dt(del, tau), 9) + CRLF + CRLF

            txt = txt + "Values for the residual part, phir, Eq.(5)," + CRLF + CRLF

            txt = txt + "Call:    " + TB + " Table Value" + CRLF
            txt = txt + "phir_    " + TB + EFormat(Val("-0.342693206E1"), 9) + CRLF
            txt = txt + "this code" + TB + EFormat(phir_(del, tau), 9) + CRLF + CRLF
            txt = txt + "phir_d   " + TB + EFormat(Val("-0.364366650"), 9) + CRLF
            txt = txt + "this code" + TB + EFormat(phir_d(del, tau), 9) + CRLF + CRLF
            txt = txt + "phir_dd  " + TB + EFormat(Val(" 0.856063701"), 9) + CRLF
            txt = txt + "this code" + TB + EFormat(phir_dd(del, tau), 9) + CRLF + CRLF
            txt = txt + "phir_t   " + TB + EFormat(Val("-0.581403435E1"), 9) + CRLF
            txt = txt + "this code" + TB + EFormat(phir_t(del, tau), 9) + CRLF + CRLF
            txt = txt + "phir_tt  " + TB + EFormat(Val("-0.223440737E1"), 9) + CRLF
            txt = txt + "this code" + TB + EFormat(phir_tt(del, tau), 9) + CRLF + CRLF
            txt = txt + "phir_dt  " + TB + EFormat(Val("-0.112176915E1"), 9) + CRLF
            txt = txt + "this code" + TB + EFormat(phir_dt(del, tau), 9) + CRLF + CRLF

            chk_IAPWS95_Table6 = txt

        End Function


        Public Function chk_IAPWS95_Table7() As String

            'Function values as given in Table 7 of IAPWS-95, revision of Doorwerth 2009:
            'Thermodynamic property values in the single-phase region for selected values of T and rho

            'T   rho             p                cv              w               s
            'K   kg m–3          MPa              kJ kg–1 K–1     m s–1           kJ kg–1 K–1
            '300 0.996 556 0E3   0.992 418 352E–1 0.413 018 112E1 0.150 151 914E4 0.393 062 643
            '    0.100 530 8E4   0.200 022 515E2  0.406 798 347E1 0.153 492 501E4 0.387 405 401
            '    0.118 820 2E4   0.700 004 704E3  0.346 135 580E1 0.244 357 992E4 0.132 609 616
            '500 0.435 000 0     0.999 679 423E–1 0.150 817 541E1 0.548 314 253E3 0.794 488 271E1
            '    0.453 200 0E1   0.999 938 125    0.166 991 025E1 0.535 739 001E3 0.682 502 725E1
            '    0.838 025 0E3   0.100 003 858E2  0.322 106 219E1 0.127 128 441E4 0.256 690 919E1
            '    0.108 456 4E4   0.700 000 405E3  0.307 437 693E1 0.241 200 877E4 0.203 237 509E1
            '647 0.358 000 0E3   0.220 384 756E2  0.618 315 728E1 0.252 145 078E3 0.432 092 307E1
            '900 0.241 000 0     0.100 062 559    0.175 890 657E1 0.724 027 147E3 0.916 653 194E1
            '    0.526 150 0E2   0.200 000 690E2  0.193 510 526E1 0.698 445 674E3 0.659 070 225E1
            '    0.870 769 0E3   0.700 000 006E3  0.266 422 350E1 0.201 933 608E4 0.417 223 802E1

            Dim CRLF As String
            Dim txt As String
            Dim d As Double, t As Double

            Dim i As Integer, row As String
            Dim ft As Double, fd As Double, ftt As Double, ftd As Double, fdd As Double
            Dim p As Double, cv As Double, c As Double, eta As Double

            CRLF = Chr(13) + Chr(10)

            txt = "Implementation of IAPWS-95 in Visual Basic" + CRLF
            txt = txt + "for Publication in Ocean Science, 2009" + CRLF
            txt = txt + "R. Feistel, IOW, Version " + Version + CRLF
            txt = txt + "Compiled on " + CStr(Now) + CRLF + CRLF

            txt = txt + "Function values as given in Table 7 of IAPWS-95:" + CRLF
            txt = txt + "Thermodynamic property values in the single-phase region for selected values of T and rho" + CRLF + CRLF

            txt = txt + "T   rho           P               cv              c               eta" + CRLF
            txt = txt + "K   kg/m^3        MPa             kJ/(kg K)       m/s             kJ/(kg K)" + CRLF

            For i = 1 To 11
                Select Case i
                    Case 1 : row = "300 0.9965560E+3  0.992418352E-1  0.413018112E+1  0.150151914E+4  0.393062643"
                    Case 2 : row = "    0.1005308E+4  0.200022515E+2  0.406798347E+1  0.153492501E+4  0.387405401"
                    Case 3 : row = "    0.1188202E+4  0.700004704E+3  0.346135580E+1  0.244357992E+4  0.132609616"
                    Case 4 : row = "500 0.4350000     0.999679423E-1  0.150817541E+1  0.548314253E+3  0.794488271E+1"
                    Case 5 : row = "    0.4532000E+1  0.999938125     0.166991025E+1  0.535739001E+3  0.682502725E+1"
                    Case 6 : row = "    0.8380250E+3  0.100003858E+2  0.322106219E+1  0.127128441E+4  0.256690918E+1"
                    Case 7 : row = "    0.1084564E+4  0.700000405E+3  0.307437693E+1  0.241200877E+4  0.203237509E+1"
                    Case 8 : row = "647 0.3580000E+3  0.220384756E+2  0.618315728E+1  0.252145078E+3  0.432092307E+1"
                    Case 9 : row = "900 0.2410000     0.100062559     0.175890657E+1  0.724027147E+3  0.916653194E+1"
                    Case 10 : row = "    0.5261500E+2  0.200000690E+2  0.193510526E+1  0.698445674E+3  0.659070225E+1"
                    Case Else : row = "    0.8707690E+3  0.700000006E+3  0.266422350E+1  0.201933608E+4  0.417223802E+1"
                End Select

                txt = txt + row + CRLF
                txt = txt + "this code:       "

                'get temperature and density input
                If Left(row, 3) <> "   " Then t = Val(Left(row, 3))
                d = Val(Mid(row, 5, 12))

                'compute pressure
                fd = flu_f_si(0, 1, t, d)
                p = d ^ 2 * fd
                txt = txt + Left(EFormat(0.000001 * p, 9) + Space(16), 16)

                'compute cv
                ftt = flu_f_si(2, 0, t, d)
                cv = -t * ftt
                txt = txt + Left(EFormat(0.001 * cv, 9) + Space(16), 16)

                'compute sound speed
                fdd = flu_f_si(0, 2, t, d)
                ftd = flu_f_si(1, 1, t, d)
                c = Sqrt(d ^ 2 * (ftt * fdd - ftd ^ 2) / ftt + 2 * d * fd)
                txt = txt + Left(EFormat(c, 9) + Space(16), 16)

                'compute entropy
                ft = flu_f_si(1, 0, t, d)
                eta = -ft
                txt = txt + EFormat(0.001 * eta, 9) + CRLF + CRLF

            Next i

            chk_IAPWS95_Table7 = txt

        End Function

#End Region

#Region "Flu_3a"

        '#########################################################################

        'This module requires the library modules:
        '     Constants_0_Mdl,  file Constants_0.bas
        '     Flu_1_Mdl,        file Flu_1.bas
        '     Maths_0_Mdl,      file Maths_0.bas
        '     Convert_0_Mdl,    file Convert_0.bas

        '#########################################################################


        'This module implements the Gibbs functions of liquid water and vapour
        'depending on temperature and pressure, as well as their partial derivatives,
        'computed numerically from the Helmholtz function of fluid water, IAPWS-95.

        'Implementation in VB6 by Rainer Feistel
        'for publication in Ocean Science, as described in the papers

        'Feistel, R., Wright, D.G., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part I: Background and Equations.
        'Ocean Science, 2009

        'Wright, D.G., Feistel, R., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part II: The Library Routines,
        'Ocean Science, 2009


        'Private Const ErrorReturn = 9.99999999E+98

        'Control parameters of the density iteration
        Private ctrl_initialized As Integer
        Private ctrl_mode_liquid As Integer
        Private ctrl_mode_vapour As Integer

        Private ctrl_loop_maximum As Long

        Private ctrl_density_liquid As Double
        Private ctrl_density_vapour As Double

        Private ctrl_eps_exit_liquid As Double
        Private ctrl_eps_exit_vapour As Double

        Private ctrl_method_liquid As Long
        Private ctrl_method_vapour As Long

        Private ctrl_density2_liquid As Double
        Private ctrl_density2_vapour As Double

        'Coefficients of IF97
        'Coefficients of region 1
        Private i1i(34) As Integer
        Private j1i(34) As Integer
        Private n1i(34) As Double

        'Coefficients of region 2, ideal part
        Private j0i(9) As Integer
        Private n0i(9) As Double

        'Coefficients of region 2, residual part
        Private iri(43) As Integer
        Private jri(43) As Integer
        Private nri(43) As Double

        'Coefficients of the F03 Gibbs function
        Private Const maxt = 7, maxp = 6
        Private gc(maxt, maxp) As Double


        Public Function liq_g_si(ByVal drv_t As Integer, _
                                ByVal drv_p As Integer, _
                                ByVal t_si As Double, _
                                ByVal p_si As Double) As Double

            'this implements the Gibbs function of liquid water computed from IAPWS-95
            'and its first and second derivatives with respect to the absolute temperature t_si
            'and the absolute pressure p_si

            'note: the accuracy of this functions depends on the iteration settings of this module

            'Output: liq_g_si = specific Gibbs energy in J/kg or its derivative

            'Input:  drv_t = order of temperature derivative, 0 <= drv_t <= 2
            '        drv_p = order of pressure derivative, 0 <= drv_p + drv_t <= 2
            '        t_si = absolute temperature, in K
            '        p_si = absolute pressure, in Pa

            'Check values with default settings:
            'liq_g_si( 0, 0, 300, 1E5) =-5265.0504557718
            'liq_g_si( 1, 0, 300, 1E5) =-393.062433814569
            'liq_g_si( 0, 1, 300, 1E5) = 1.00345555938138E-03
            'liq_g_si( 2, 0, 300, 1E5) =-13.9354650734086
            'liq_g_si( 1, 1, 300, 1E5) = 2.75753316815429E-07
            'liq_g_si( 0, 2, 300, 1E5) =-4.52072086722098E-13

            Dim g As Double, d As Double

            liq_g_si = ErrorReturn

            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            d = liq_density_si(t_si, p_si)  'numerical inverse function of IAPWS-95

            If d = ErrorReturn Then Exit Function
            If d <= 0 Then Exit Function

            g = flu_t_p_derivative_si(drv_t, drv_p, t_si, d)
            If g = ErrorReturn Then Exit Function

            liq_g_si = g

        End Function


        Public Function vap_g_si(ByVal drv_t As Integer, _
                                ByVal drv_p As Integer, _
                                ByVal t_si As Double, _
                                ByVal p_si As Double) As Double

            'this implements the Gibbs function of water vapour computed from IAPWS-95
            'and its first and second derivatives with respect to the absolute temperature t_si
            'and the absolute pressure p_si

            'note: the accuracy of this functions depends on the iteration settings of this module

            'Output: liq_g_si = specific Gibbs energy in J/kg or its derivative

            'Input:  drv_t = order of temperature derivative, 0 <= drv_t <= 2
            '        drv_p = order of pressure derivative, 0 <= drv_p + drv_t <= 2
            '        t_si = absolute temperature, in K
            '        p_si = absolute pressure, in Pa

            'Check values with default settings:
            'vap_g_si( 0, 0, 300, 1E3) =-180090.341338025
            'vap_g_si( 1, 0, 300, 1E3) =-9103.67940087138
            'vap_g_si( 0, 1, 300, 1E3) = 138.38847806943
            'vap_g_si( 2, 0, 300, 1E3) =-6.24707163426883
            'vap_g_si( 1, 1, 300, 1E3) = 0.462704658817776
            'vap_g_si( 0, 2, 300, 1E3) =-0.138455798863587

            Dim g As Double, d As Double

            vap_g_si = ErrorReturn

            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            d = vap_density_si(t_si, p_si)  'numerical inverse function of IAPWS-95

            If d = ErrorReturn Then Exit Function
            If d <= 0 Then Exit Function

            g = flu_t_p_derivative_si(drv_t, drv_p, t_si, d)
            If g = ErrorReturn Then Exit Function

            vap_g_si = g

        End Function


        Public Function liq_density_si(ByVal t_si As Double, ByVal p_si As Double) As Double

            'this function returns the density of liquid water as a function of temperature and pressure

            'Output: liq_density_si = density in kg/m3

            'Input:  t_si = absolute temperature, in K
            '        p_si = absolute pressure, in Pa

            'Check value with default settings: liq_density_si(300, 1E5) = 996.556340388894

            Dim d As Double, d2 As Double, eps As Double
            Dim maxit As Long

            liq_density_si = ErrorReturn

            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            'avoid numerical problems at the very critical point
            If t_si = CP_temperature_si And p_si = CP_pressure_si Then
                liq_density_si = CP_density_si
                Exit Function
            End If

            init_it_ctrl_density()

            'consider the control settings for the iteration
            Select Case ctrl_mode_liquid
                Case 1 : d = ctrl_density_liquid
                Case 0
                    If t_si <= 623.15 And p_si <= 100000000.0# Then
                        'IF-97 at subcritical temperature, liquid
                        d = aux_liq_density_if97_si(t_si, p_si)
                    ElseIf t_si <= CP_temperature_si And p_si <= 16529000.0# Then
                        'IF-97 at subcritical temperature, superheated liquid
                        d = aux_liq_density_if97_si(t_si, p_si)
                    ElseIf t_si <= 1073.15 And p_si <= 16529000.0# Then
                        'IF-97 at subcritical pressure, fluid
                        d = aux_vap_density_if97_si(t_si, p_si)
                    ElseIf t_si <= 650 And p_si <= 35000000 Then
                        'cubic EOF in the critical region
                        d = aux_liq_density_critical_si(t_si, p_si)
                    ElseIf t_si <= 650 Then
                        'dense fluid
                        d = 1000
                    Else
                        'ideal gas anywhere else
                        d = aux_density_ideal_si(t_si, p_si)
                    End If
                Case -1 : d = aux_liq_density_if97_si(t_si, p_si)
                Case -2 : d = aux_density_EOS80_si(t_si, p_si)
                Case -3 : d = aux_liq_density_f03_si(t_si, p_si)
                Case Else : Exit Function
            End Select

            If d = ErrorReturn Then Exit Function
            If d <= 0 Then Exit Function

            Select Case ctrl_loop_maximum
                Case 0 : maxit = 100
                Case -1 : liq_density_si = d
                    Exit Function
                Case Is > 0 : maxit = ctrl_loop_maximum
                Case Else : Exit Function
            End Select

            eps = ctrl_eps_exit_liquid
            If eps = 0 Then Exit Function

            If ctrl_method_liquid > 1 Then
                'specify the second point for Secant or Brent method
                Select Case ctrl_density2_liquid
                    Case 0 : d2 = DensityIteration_Newton(t_si, p_si, d, 1, 1.0E+99)
                        If d2 = ErrorReturn Then Exit Function
                        If ctrl_method_liquid = 2 Then 'Brent
                            d2 = d + 2 * (d2 - d)
                        End If
                    Case -1 : d2 = DensityIteration_Newton(t_si, p_si, d, 1, 1.0E+99)
                        If d2 = ErrorReturn Then Exit Function
                    Case -2 : d2 = DensityIteration_Newton(t_si, p_si, d, 1, 1.0E+99)
                        If d2 = ErrorReturn Then Exit Function
                        d2 = d + 0.5 * (d2 - d)
                    Case -3 : d2 = DensityIteration_Newton(t_si, p_si, d, 1, 1.0E+99)
                        If d2 = ErrorReturn Then Exit Function
                        d2 = d + 2 * (d2 - d)
                    Case Is > 0 : d2 = ctrl_density2_liquid
                    Case Else : Exit Function
                End Select
            End If

            'run the iteration
            Select Case ctrl_method_liquid
                Case 0 : d = DensityIteration_Newton(t_si, p_si, d, maxit, eps)
                Case 1 : d = DensityIteration_Newton(t_si, p_si, d, maxit, eps)
                Case 2 : d = DensityIteration_Brent(t_si, p_si, d, d2, maxit, eps)
                Case 3 : d = DensityIteration_Secant(t_si, p_si, d, d2, maxit, eps)
                Case Else : Exit Function
            End Select

            If d = ErrorReturn Then Exit Function
            If d <= 0 Then Exit Function

            'avoid accidental vapour density
            If t_si < CP_temperature_si And p_si < CP_pressure_si Then
                If d < CP_density_si Then Exit Function
            End If


            liq_density_si = d

        End Function


        Public Function vap_density_si(ByVal t_si As Double, ByVal p_si As Double) As Double

            'this function returns the density of water vapour as a function of temperature and pressure

            'Output: vap_density_si = density in kg/m3

            'Input:  t_si = absolute temperature, in K
            '        p_si = absolute pressure, in Pa

            'Check value with default settings: vap_density_si(300, 1E3) = 7.2260351002509E-03


            Dim d As Double, d2 As Double, eps As Double
            Dim maxit As Long

            vap_density_si = ErrorReturn

            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            'avoid numerical problems at the very critical point
            If t_si = CP_temperature_si And p_si = CP_pressure_si Then
                vap_density_si = CP_density_si
                Exit Function
            End If

            init_it_ctrl_density()

            'consider the control settings for the iteration
            Select Case ctrl_mode_liquid
                Case 1 : d = ctrl_density_vapour
                Case 0
                    If t_si <= 273.15 Then
                        d = aux_density_ideal_si(t_si, p_si)
                    ElseIf t_si <= 623.15 And p_si <= CP_pressure_si Then
                        'IF-97 at subcritical pressure, vapour
                        d = aux_vap_density_if97_si(t_si, p_si)
                    ElseIf t_si <= 1073.15 And p_si <= 16529000 Then
                        'IF-97 at subcritical pressure, fluid
                        d = aux_vap_density_if97_si(t_si, p_si)
                    ElseIf t_si <= 623.15 And p_si <= 100000000.0# Then
                        'IF-97 at subcritical temperature, liquid
                        d = aux_liq_density_if97_si(t_si, p_si)
                    ElseIf t_si <= 650 And p_si <= CP_pressure_si Then
                        'cubic EOF in the critical region, vapour
                        d = aux_vap_density_critical_si(t_si, p_si)
                    ElseIf t_si <= 650 And p_si <= 35000000 Then
                        'cubic EOF in the critical region, liquid since p > pc
                        d = aux_liq_density_critical_si(t_si, p_si)
                    ElseIf t_si <= 650 Then
                        'dense fluid
                        d = 1000
                    Else
                        'ideal gas anywhere else
                        d = aux_density_ideal_si(t_si, p_si)
                    End If
                Case -1 : d = aux_vap_density_if97_si(t_si, p_si)
                Case -2 : d = aux_density_ideal_si(t_si, p_si)
                Case Else : Exit Function
            End Select

            If d = ErrorReturn Then Exit Function
            If d <= 0 Then Exit Function

            Select Case ctrl_loop_maximum
                Case 0 : maxit = 100
                Case -1 : vap_density_si = d
                    Exit Function
                Case Is > 0 : maxit = ctrl_loop_maximum
                Case Else : Exit Function
            End Select

            eps = ctrl_eps_exit_vapour
            If eps = 0 Then Exit Function

            If ctrl_method_vapour > 1 Then
                'specify the second point for Secant or Brent method
                Select Case ctrl_density2_vapour
                    Case 0 : d2 = DensityIteration_Newton(t_si, p_si, d, 1, 1.0E+99)
                        If d2 = ErrorReturn Then Exit Function
                        If ctrl_method_vapour = 2 Then 'Brent
                            d2 = d + 2 * (d2 - d)
                        End If
                    Case -1 : d2 = DensityIteration_Newton(t_si, p_si, d, 1, 1.0E+99)
                        If d2 = ErrorReturn Then Exit Function
                    Case -2 : d2 = DensityIteration_Newton(t_si, p_si, d, 1, 1.0E+99)
                        If d2 = ErrorReturn Then Exit Function
                        d2 = d + 0.5 * (d2 - d)
                    Case -3 : d2 = DensityIteration_Newton(t_si, p_si, d, 1, 1.0E+99)
                        If d2 = ErrorReturn Then Exit Function
                        d2 = d + 2 * (d2 - d)
                    Case Is > 0 : d2 = ctrl_density2_vapour
                    Case Else : Exit Function
                End Select
            End If

            'run the iteration
            Select Case ctrl_method_vapour
                Case 0 : d = DensityIteration_Newton(t_si, p_si, d, maxit, eps)
                Case 1 : d = DensityIteration_Newton(t_si, p_si, d, maxit, eps)
                Case 2 : d = DensityIteration_Brent(t_si, p_si, d, d2, maxit, eps)
                Case 3 : d = DensityIteration_Secant(t_si, p_si, d, d2, maxit, eps)
                Case Else : Exit Function
            End Select


            If d = ErrorReturn Then Exit Function
            If d <= 0 Then Exit Function

            'avoid accidental liquid density
            If t_si < CP_temperature_si And p_si < CP_pressure_si Then
                If d > CP_density_si Then Exit Function
            End If

            vap_density_si = d

        End Function


        Private Function DensityIteration_Newton(ByVal t As Double, _
                                                ByVal p As Double, _
                                                ByVal d As Double, _
                                                ByVal maxit As Integer, _
                                                ByVal eps As Double) As Double

            'The function returns the density as a function of temperature and pressure,
            'computed from the Helmholtz function by Newton iteration
            'http://en.wikipedia.org/wiki/Newton%27s_method

            'output: DensityIteration_Newton: density in kg/m3
            '
            '        The value ErrorReturn is returned if
            '        - the maximum number of iterations is exceeded without meeting the exit criterion
            '        - the function call to flu_f_si has returned an error
            '        - density has taken a zero or negative value during the iteration

            'input:  t: absolute temperature in K
            '        p: absolute pressure in Pa
            '        d: initial guess for density in kg/m3
            '    maxit: maximum number of iteration steps to be done
            '      eps: required accuracy of density
            '           eps > 0: absolute density uncertainty in kg/m3
            '           eps < 0: relative density uncertainty

            Dim dd As Double, fd As Double, fdd As Double
            Dim it As Long

            DensityIteration_Newton = ErrorReturn

            If p <= 0 Then Exit Function

            If check_limits = 1 Then
                If t < flu_tmin Or t > flu_tmax Then Exit Function
            Else
                If t <= 0 Then Exit Function
            End If

            check_limits = check_limits - 1

            For it = 1 To maxit

                fd = flu_f_si(0, 1, t, d)
                If fd = ErrorReturn Then Exit For

                fdd = flu_f_si(0, 2, t, d)
                If fdd = ErrorReturn Then Exit For

                dd = d * (2 * fd + d * fdd)
                If dd = 0 Then Exit For

                dd = (p - d ^ 2 * fd) / dd
                d = d + dd

                If d <= 0 Then Exit For

                If eps > 0 Then                'absolute limit
                    If Abs(dd) < eps Then
                        DensityIteration_Newton = d
                        Exit For
                    End If
                Else                           'relative limit
                    If Abs(dd) < -eps * d Then
                        DensityIteration_Newton = d
                        Exit For
                    End If
                End If

            Next it

            check_limits = check_limits + 1

            If check_limits = 1 Then
                If d <= flu_dmin Or d > flu_dmax Then
                    DensityIteration_Newton = ErrorReturn
                End If
            End If

        End Function


        Private Function DensityIteration_Brent(ByVal t As Double, _
                                            ByVal p As Double, _
                                            ByVal d1 As Double, _
                                            ByVal d2 As Double, _
                                            ByVal maxit As Integer, _
                                            ByVal eps As Double) As Double

            'The function returns the density as a function of temperature and pressure,
            'computed from the Helmholtz function by Brent iteration
            'http://en.wikipedia.org/wiki/Brent's_method

            'output: DensityIteration_Brent: density in kg/m^3
            '
            '        The value ErrorReturn is returned if
            '        - the maximum number of iterations is exceeded without meeting the exit criterion
            '        - the function call to flu_f_si has returned an error
            '        - density has taken a zero or negative value during the iteration

            'input:  t: absolute temperature in K
            '        p: absolute pressure in Pa
            '       d1: initial guess for density in kg/m^3
            '       d2: counterpoint density
            '    maxit: maximum number of iteration steps to be done
            '      eps: required accuracy of density
            '           eps > 0: absolute density uncertainty in kg/m^3
            '           eps < 0: relative density uncertainty

            Dim a As Double, b As Double, c As Double, d As Double, S As Double
            Dim fa As Double, fb As Double, fc As Double, fs As Double
            Dim mflag As Boolean
            Dim it As Long

            DensityIteration_Brent = ErrorReturn

            If p <= 0 Then Exit Function

            If check_limits = 1 Then
                If t < flu_tmin Or t > flu_tmax Then Exit Function
            Else
                If t <= 0 Then Exit Function
            End If

            check_limits = check_limits - 1

            a = d1
            fa = a ^ 2 * flu_f_si(0, 1, t, a) - p
            b = d2
            fb = b ^ 2 * flu_f_si(0, 1, t, b) - p
            If fa * fb > 0 Then GoTo ExitFunction

            If Abs(fa) < Abs(fb) Then
                Swap(a, b)
                Swap(fa, fb)
            End If

            c = a
            fc = fa
            mflag = True

            For it = 1 To maxit

                If fb = 0 Then
                    DensityIteration_Brent = b
                    Exit For
                End If
                If eps > 0 Then                'absolute limit
                    If Abs(a - b) < eps Then
                        DensityIteration_Brent = b
                        Exit For
                    End If
                Else                           'relative limit
                    If Abs(a - b) < -eps * b Then
                        DensityIteration_Brent = b
                        Exit For
                    End If
                End If

                If fa = fb Then Exit For

                If fa <> fc And fb <> fc Then
                    S = a * fb * fc / ((fa - fb) * (fa - fc)) + _
                        b * fa * fc / ((fb - fa) * (fb - fc)) + _
                        c * fa * fb / ((fc - fa) * (fc - fb))
                Else
                    S = b - (b - a) * fb / (fb - fa)
                End If

                If ((3 * a + b) / 4 - S) * (b - S) > 0 Or _
                    (mflag = True And Abs(S - b) >= 0.5 * Abs(b - c)) Or _
                    (mflag = False And Abs(S - b) >= 0.5 * (c - d)) Then
                    S = 0.5 * (a + b)
                Else
                    mflag = False
                End If

                fs = S ^ 2 * flu_f_si(0, 1, t, S) - p
                d = c
                c = b
                fc = fb

                If fa * fs < 0 Then
                    b = S
                    fb = fs
                Else
                    a = S
                    fa = fs
                End If

                If Abs(fa) < Abs(fb) Then
                    Swap(a, b)
                    Swap(fa, fb)
                End If

            Next it

ExitFunction:
            check_limits = check_limits + 1

            If check_limits = 1 Then
                If d <= flu_dmin Or d > flu_dmax Then
                    DensityIteration_Brent = ErrorReturn
                End If
            End If

        End Function


        Private Function DensityIteration_Secant(ByVal t As Double, _
                                                ByVal p As Double, _
                                                ByVal d As Double, _
                                                ByVal d2 As Double, _
                                                ByVal maxit As Integer, _
                                                ByVal eps As Double) As Double

            'The function returns the density as a function of temperature and pressure,
            'computed from the Helmholtz function by secant iteration
            'http://en.wikipedia.org/wiki/Secant_method

            'output: DensityIteration_Secant: density in kg/m^3
            '
            '        The value ErrorReturn is returned if
            '        - the maximum number of iterations is exceeded without meeting the exit criterion
            '        - the function call to flu_f_si has returned an error
            '        - density has taken a zero or negative value during the iteration

            'input:  t: absolute temperature in K
            '        p: absolute pressure in Pa
            '        d: initial guess for density in kg/m^3
            '       d2: counterpoint density
            '    maxit: maximum number of iteration steps to be done
            '      eps: required accuracy of density
            '           eps > 0: absolute density uncertainty in kg/m^3
            '           eps < 0: relative density uncertainty

            Dim dd As Double, d1 As Double, p1 As Double, p2 As Double
            Dim it As Long

            DensityIteration_Secant = ErrorReturn

            If p <= 0 Then Exit Function

            If check_limits = 1 Then
                If t < flu_tmin Or t > flu_tmax Then Exit Function
            Else
                If t <= 0 Then Exit Function
            End If

            check_limits = check_limits - 1

            p2 = d2 ^ 2 * flu_f_si(0, 1, t, d2)
            If p2 = ErrorReturn Then GoTo ExitFunction

            For it = 1 To maxit
                d1 = d2
                p1 = p2
                d2 = d
                p2 = d ^ 2 * flu_f_si(0, 1, t, d)

                If p2 = ErrorReturn Then Exit For
                If p2 = p1 Then Exit For

                dd = -(d2 - d1) * (p2 - p) / (p2 - p1)
                d = d + dd

                If d <= 0 Then Exit For

                If eps > 0 Then                'absolute limit
                    If Abs(dd) < eps Then
                        DensityIteration_Secant = d
                        Exit For
                    End If
                Else                           'relative limit
                    If Abs(dd) < -eps * d Then
                        DensityIteration_Secant = d
                        Exit For
                    End If
                End If

            Next it

ExitFunction:
            check_limits = check_limits + 1

            If check_limits = 1 Then
                If d <= flu_dmin Or d > flu_dmax Then
                    DensityIteration_Secant = ErrorReturn
                End If
            End If

        End Function


        Private Function flu_t_p_derivative_si(ByVal drv_t As Integer, _
                                            ByVal drv_p As Integer, _
                                            ByVal t_si As Double, _
                                            ByVal d_si As Double) As Double

            'this function computes t-p derivatives of g from t-d derivatives of f

            Dim g As Double

            Dim f As Double, ft As Double, fd As Double
            Dim ftt As Double, ftd As Double, fdd As Double

            flu_t_p_derivative_si = ErrorReturn

            If t_si <= 0 Then Exit Function
            If d_si <= 0 Then Exit Function

            Select Case drv_t

                Case 0
                    Select Case drv_p

                        Case 0 : f = flu_f_si(0, 0, t_si, d_si)              'g
                            If f = ErrorReturn Then Exit Function
                            fd = flu_f_si(0, 1, t_si, d_si)
                            If fd = ErrorReturn Then Exit Function
                            g = f + d_si * fd

                        Case 1 : g = 1.0# / d_si                               'g_p

                        Case 2 : fd = flu_f_si(0, 1, t_si, d_si)             'g_pp
                            If fd = ErrorReturn Then Exit Function
                            fdd = flu_f_si(0, 2, t_si, d_si)
                            If fdd = ErrorReturn Then Exit Function
                            g = d_si ^ 3 * (2.0# * fd + d_si * fdd)
                            If g = 0 Then Exit Function
                            g = -1.0# / g

                        Case Else : Exit Function
                    End Select

                Case 1
                    Select Case drv_p

                        Case 0 : ft = flu_f_si(1, 0, t_si, d_si)             'g_t
                            If ft = ErrorReturn Then Exit Function
                            g = ft

                        Case 1 : fd = flu_f_si(0, 1, t_si, d_si)             'g_tp
                            If fd = ErrorReturn Then Exit Function
                            ftd = flu_f_si(1, 1, t_si, d_si)
                            If ftd = ErrorReturn Then Exit Function
                            fdd = flu_f_si(0, 2, t_si, d_si)
                            If fdd = ErrorReturn Then Exit Function
                            g = d_si * (2.0# * fd + d_si * fdd)
                            If g = 0 Then Exit Function
                            g = ftd / g

                        Case Else : Exit Function
                    End Select

                Case 2
                    Select Case drv_p

                        Case 0 : fd = flu_f_si(0, 1, t_si, d_si)             'g_tt
                            If fd = ErrorReturn Then Exit Function
                            ftt = flu_f_si(2, 0, t_si, d_si)
                            If ftt = ErrorReturn Then Exit Function
                            ftd = flu_f_si(1, 1, t_si, d_si)
                            If ftd = ErrorReturn Then Exit Function
                            fdd = flu_f_si(0, 2, t_si, d_si)
                            If fdd = ErrorReturn Then Exit Function
                            g = 2.0# * fd + d_si * fdd
                            If g = 0 Then Exit Function
                            g = ftt - d_si * ftd ^ 2 / g

                        Case Else : Exit Function
                    End Select

                Case Else : Exit Function
            End Select

            flu_t_p_derivative_si = g

        End Function


        Public Sub set_it_ctrl_density(ByVal key As String, ByVal value As Double)

            'this sub sets control parameters for the iteration used to compute
            'IAPWS-95 density from pressure

            'key              value
            'it_steps         0           set iteration number to default (100)
            'it_steps         n > 0       set iteration number to n
            'it_steps        -1           do not iterate, use initial value

            'init_liq_dens    0           use default liquid density (IF97) to start in the subcritical region (T <= 623.16 K or P <=16.529 MPa)
            '                             use default cubic EOS to start in the critical region (623.16 K < T < 650 K and 16.529 MPa < P < 35 MPa)
            '                             use default 1000 kg/m³ to start in the remaining supercritical region
            'init_liq_dens   -1           use IF97 liquid density to start
            'init_liq_dens   -2           use EOS80 liquid density to start
            'init_liq_dens   -3           use F03 liquid density to start
            'init_liq_dens    d > 0       use value d as liquid density to start

            'init_vap_dens    0           use default vapour density (IF97) to start in the subcritical region (T <= 623.16 K or P <=16.529 MPa)
            '                             use default cubic EOS to start in the critical region (623.16 K < T < 650 K and 16.529 MPa < P < 35 MPa)
            '                             use default 1000 kg/m³ to start in the remaining supercritical region
            'init_vap_dens   -1           use IF97 vapour density to start
            'init_vap_dens   -2           use ideal-gas vapour density to start
            'init_vap_dens    d > 0       use value d as vapour density to start

            'tol_liq_dens     0           use default exit accuracy for liquid density (0.1 ppm)
            'tol_liq_dens     eps         use eps as exit accuracy for liquid density (eps < 0 means relative error)

            'tol_vap_dens     0           use default exit accuracy for vapour density (0.1 ppm)
            'tol_vap_dens     eps         use eps as exit accuracy for vapour density (eps < 0 means relative error)

            'method_liq       0           use default iteration method (now: Newton method) for liquid
            'method_liq       1           use Newton method for liquid
            'method_liq       2           use Brent method for liquid
            'method_liq       3           use secant method for liquid

            'dens2_liq        0           use default counterpoint for Brent/Secant method for liquid
            '                         Brent: 2 * Newton step, Secant: 1 * Newton step
            'dens2_liq       -1           use Newton step as the first counterpoint for liquid
            'dens2_liq       -2           use 0.5 * Newton step as the first counterpoint for liquid
            'dens2_liq       -3           use 2 * Newton step as the first counterpoint for liquid
            'dens2_liq        d > 0       use d as the first counterpoint density for liquid

            'method_vap       0           use default iteration method (now: Newton method) for vapour
            'method_vap       1           use Newton method for vapour
            'method_vap       2           use Brent method for vapour
            'method_vap       3           use secant method for vapour

            'dens2_vap        0           use default counterpoint for Brent/Secant method for vapour
            '                         Brent: 2 * Newton step, Secant: 1 * Newton step
            'dens2_vap       -1           use Newton step as the first counterpoint for vapour
            'dens2_vap       -2           use 0.5 * Newton step as the first counterpoint for vapour
            'dens2_vap       -3           use 2 * Newton step as the first counterpoint for vapour
            'dens2_vap        d > 0       use d as the first counterpoint density for vapour

            init_it_ctrl_density()

            Select Case LCase(Trim(key))

                Case "it_steps" 'iteration steps
                    Select Case value
                        Case 0 : ctrl_loop_maximum = 100  'default = 100
                        Case Is < 0 : ctrl_loop_maximum = -1
                        Case Else : ctrl_loop_maximum = value
                    End Select

                Case "init_liq_dens" 'start liquid density
                    Select Case CLng(value)
                        Case 0 : ctrl_mode_liquid = 0    'default = IF97
                        Case Is < -3 'Exit Sub
                        Case Is < 0 : ctrl_mode_liquid = value
                        Case Else : ctrl_mode_liquid = 1
                            ctrl_density_liquid = value
                    End Select

                Case "init_vap_dens" 'start vapour density
                    Select Case CLng(value)
                        Case 0 : ctrl_mode_vapour = 0    'default = IF97
                        Case Is < -2 'Exit Sub
                        Case Is < 0 : ctrl_mode_vapour = value
                        Case Else : ctrl_mode_vapour = 1
                            ctrl_density_vapour = value
                    End Select

                Case "tol_liq_dens" 'required liquid density tolerance
                    Select Case value
                        Case 0 : ctrl_eps_exit_liquid = -0.0000001   'default = 0.1 ppm relative
                        Case Else : ctrl_eps_exit_liquid = value
                    End Select

                Case "tol_vap_dens" 'required vapour density tolerance
                    Select Case value
                        Case 0 : ctrl_eps_exit_vapour = -0.0000001   'default = 0.1 ppm relative
                        Case Else : ctrl_eps_exit_vapour = value
                    End Select

                Case "method_liq"
                    Select Case value
                        Case 0 To 3 : ctrl_method_liquid = value
                    End Select

                Case "dens2_liq"
                    Select Case value
                        Case Is >= -3 : ctrl_density2_liquid = value
                    End Select

                Case "method_vap"
                    Select Case value
                        Case 0 To 3 : ctrl_method_vapour = value
                    End Select

                Case "dens2_vap"
                    Select Case value
                        Case Is >= -3 : ctrl_density2_vapour = value
                    End Select

            End Select

        End Sub


        Public Function get_it_ctrl_density(ByVal key As String) As Double

            'this function returns control parameters as set for the Newton iteration
            'used to compute IAPWS-95 density from pressure

            init_it_ctrl_density()

            Select Case LCase(Trim(key))

                Case "it_steps" 'iteration steps
                    get_it_ctrl_density = ctrl_loop_maximum

                Case "init_liq_dens" 'start liquid density
                    If ctrl_mode_liquid = 1 Then
                        get_it_ctrl_density = ctrl_density_liquid
                    Else
                        get_it_ctrl_density = ctrl_mode_liquid
                    End If

                Case "init_vap_dens" 'start vapour density
                    If ctrl_mode_vapour = 1 Then
                        get_it_ctrl_density = ctrl_density_vapour
                    Else
                        get_it_ctrl_density = ctrl_mode_vapour
                    End If

                Case "tol_liq_dens" 'required liquid density tolerance
                    get_it_ctrl_density = ctrl_eps_exit_liquid

                Case "tol_vap_dens" 'required vapour density tolerance
                    get_it_ctrl_density = ctrl_eps_exit_vapour

                Case "method_liq" 'selected iteration method for liquid
                    get_it_ctrl_density = ctrl_method_liquid

                Case "dens2_liq" 'counterpoint value for liquid (irrelevant for Newton)
                    get_it_ctrl_density = ctrl_density2_liquid

                Case "method_vap" 'selected iteration method for vapour
                    get_it_ctrl_density = ctrl_method_vapour

                Case "dens2_vap" 'counterpoint value for vapour (irrelevant for Newton)
                    get_it_ctrl_density = ctrl_density2_vapour

                Case Else
                    get_it_ctrl_density = ErrorReturn

            End Select

        End Function


        Private Function aux_density_EOS80_si(ByVal t_si As Double, _
                                            ByVal p_si As Double) As Double

            'This function returns the density of liquid water computed from the
            'International Equation of State of Seawater 1980, EOS-80, as a function of temperature
            'and pressure

            'output:  aux_density_EOS80_si: density in kg/m^3

            'input:   t_si: absolute temperature in K
            '         p_si: absolute pressure in Pa

            Dim AW As Double, BW As Double, KW As Double, RW As Double
            Dim t As Double, p As Double

            aux_density_EOS80_si = ErrorReturn

            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            'R.C.Millard Jr.: International Oceanographic Tables Vol.4
            'UNESCO technical Papers in Marine Science 40
            'UNESCO 1987

            t = cnv_temperature("DEGC(T68)", t_si, "K(T90)")  't68 in °C
            p = 0.1 * cnv_pressure("DBAR", p_si, "PA")        'sea pressure in bar

            BW = 0.0000850935 + t * (-0.00000612293 + t * 0.000000052787)
            AW = 3.239908 + t * (0.00143713 + t * (0.000116092 - t * 0.000000577905))
            KW = 0.01360477 - t * 0.00005155288
            KW = 19652.21 + t * (148.4206 + t * (-2.327105 + t * KW))
            KW = KW + p * (AW + p * BW)
            If KW = 0 Then Exit Function

            KW = 1.0# - p / KW
            If KW = 0 Then Exit Function

            RW = 0.0001001685 + t * (-0.000001120083 + t * 0.000000006536332)
            RW = 999.842594 + t * (0.06793952 + t * (-0.00909529 + t * RW))

            aux_density_EOS80_si = RW / KW

        End Function


        Private Function aux_density_ideal_si(ByVal t_si As Double, _
                                            ByVal p_si As Double) As Double

            'This function returns the density of ideal-gas vapour as a function of temperature
            'and pressure

            'output:  aux_density_ideal_si: density in kg/m^3

            'input:   t_si: absolute temperature in K
            '         p_si: absolute pressure in Pa


            Const R = Gas_constant_H2O_si         'specific gas constant of water in J/(kg K)

            aux_density_ideal_si = ErrorReturn

            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            aux_density_ideal_si = p_si / (R * t_si)

        End Function


        Private Sub init_it_ctrl_density()

            If ctrl_initialized = -1 Then Exit Sub

            ctrl_initialized = -1

            'Set default values and modes for density iteration
            ctrl_mode_liquid = 0
            ctrl_mode_vapour = 0
            ctrl_loop_maximum = 100
            ctrl_density_liquid = 1000
            ctrl_density_vapour = 1
            ctrl_eps_exit_liquid = -0.0000001 'relative, 0.1 ppm
            ctrl_eps_exit_vapour = -0.0000001 'relative, 0.1 ppm

            'Set default values for alternative iteration methods
            ctrl_method_liquid = 0 'default = Newton
            ctrl_method_vapour = 0 'default = Newton

            'Set default counterpoint values for alternative iteration methods
            ctrl_density2_liquid = 0  'default = .5 * Newton step
            ctrl_density2_vapour = 0  'default = .5 * Newton step

        End Sub


        Private Sub aux_density_critical_si(ByVal t_si As Double, _
                                        ByVal p_si As Double, _
                                        ByRef d_liq As Double, _
                                        ByRef d_vap As Double)

            'returns approximate liquid and/or vapour density from a cubic equation of state at given T, P
            'usable in the range 620 K < T < 650 K, 10 MPa < P < 35 MPa
            'supercritical range:  return valid d_liq = d_vap
            '2-phase region:       return valid d_liq <> d_vap          (including the metastable region)
            'liquid region:        return valid d_liq,  invalid d_vap
            'vapour region:        return valid d_vap,  invalid d_liq

            'the critical properties of IAPWS-95
            Const dc = CP_density_si
            Const pc = CP_pressure_si
            Const Tc = CP_temperature_si

            Const a10 = -7.60041479494879
            Const a20 = 118.661872386874
            Const a11 = -17.463827264079
            Const a21 = 186.040087842884
            Const a12 = 0.69701967809328
            Const a22 = 25.5059905941023
            Const a03 = -0.602044738250314
            Const a13 = 30.8633119943879
            Const a23 = 14.4873846518829

            Dim pr As Double, tr As Double
            Dim R As Double, S As Double, t As Double
            Dim d1 As Double, d2 As Double, d3 As Double
            Dim p1 As Double, p2 As Double
            Dim a0 As Double, a1 As Double, a2 As Double, a3 As Double

            d_liq = ErrorReturn
            d_vap = ErrorReturn
            If p_si <= 0 Then Exit Sub
            If t_si <= 0 Then Exit Sub

            'reduced P and T
            pr = pc / p_si - 1.0#
            tr = t_si / Tc - 1.0#

            'cubic eq. pr = a0 + a1*dr + a2*dr^2 + a3*dr^3
            'with reduced density dr = d/dc - 1
            a0 = (a10 + a20 * tr) * tr
            a1 = (a11 + a21 * tr) * tr
            a2 = (a12 + a22 * tr) * tr
            a3 = a03 + (a13 + a23 * tr) * tr  'a3 < 0 holds for T < 659.6 K

            If a3 >= 0 Then Exit Sub

            If tr < 0 Then

                'get the pressure range of 2-phase solutions at the given temperature.
                'solutions of dP/dV = 0, i.e. the spinodal curve:
                R = a2 * a2 - 3.0# * a1 * a3
                If R < 0 Then Exit Sub
                R = Sqrt(R)
                d1 = -(a2 + R) / (3.0# * a3)
                d2 = -(a2 - R) / (3.0# * a3)
                'the pressure range is
                p1 = a0 + d1 * (a1 + d1 * (a2 + d1 * a3)) 'highest pressure for subcooled vapour (lowest reduced pressure)
                p2 = a0 + d2 * (a1 + d2 * (a2 + d2 * a3)) 'lowest pressure for superheated liquid (highest reduced pressure)

            Else

                'one fluid state
                p1 = pr
                p2 = pr

            End If

            'Coefficients of the cubic eq.   dr^3 + r * dr^2 + s * dr + t = 0
            R = a2 / a3
            S = a1 / a3
            t = (a0 - pr) / a3

            Select Case get_CubicRoots(R, S, t, d1, d2, d3)

                Case 1
                    If d1 <= -1 Then Exit Sub
                    If pr >= p1 Then
                        'get the vapour density
                        d_vap = (d1 + 1) * dc
                    End If
                    If pr <= p2 Then
                        'get the liquid density
                        d_liq = (d1 + 1) * dc
                    End If

                Case 3 : Sort3up(d1, d2, d3)           '3 solutions, min is vapour, max is liquid
                    If d1 <= -1 Then Exit Sub
                    If d3 <= -1 Then Exit Sub
                    d_vap = (d1 + 1) * dc
                    d_liq = (d3 + 1) * dc

            End Select

        End Sub


        Private Function aux_liq_density_critical_si(ByVal t_si As Double, _
                                                    ByVal p_si As Double) As Double

            'returns the approximate liquid density from the cubic equation

            Dim d_liq As Double, d_vap As Double

            aux_density_critical_si(t_si, p_si, d_liq, d_vap)

            aux_liq_density_critical_si = d_liq

        End Function


        Private Function aux_vap_density_critical_si(ByVal t_si As Double, _
                                                    ByVal p_si As Double) As Double

            'returns the approximate vapour density from the cubic equation

            Dim d_liq As Double, d_vap As Double

            aux_density_critical_si(t_si, p_si, d_liq, d_vap)

            aux_vap_density_critical_si = d_vap

        End Function


        Private Sub Swap(ByRef a As Double, ByRef b As Double)
            Dim c As Double
            c = a
            a = b
            b = c
        End Sub


        Private Sub Sort3up(ByRef d1 As Double, _
                        ByRef d2 As Double, _
                        ByRef d3 As Double)

            'sort d1, d2, d3 increasing

            If d2 > d3 Then Swap(d2, d3)
            If d1 > d2 Then Swap(d1, d2)
            If d2 > d3 Then Swap(d2, d3)

        End Sub


        Private Function aux_liq_density_f03_si(ByVal t_si As Double, _
                                            ByVal p_si As Double) As Double

            'This function returns the density of liquid water computed from the
            'Gibbs function 2003 of Seawater as a function of temperature
            'and pressure, published in

            'R. Feistel:
            'A new extended Gibbs thermodynamic potential of seawater.
            'Progress in Oceanography, 58/1 (2003) 43-115

            'output:  aux_liq_density_f03_si: density in kg/m^3

            'input:   t_si: absolute temperature in K
            '         p_si: absolute pressure in Pa

            Dim d As Double

            aux_liq_density_f03_si = ErrorReturn

            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            d = aux_liq_g_f03_si(0, 1, t_si, p_si)

            If d = ErrorReturn Then Exit Function
            If d <= 0 Then Exit Function

            aux_liq_density_f03_si = 1.0# / d

        End Function


        Private Function aux_liq_g_f03_si(ByVal drv_t As Integer, _
                                        ByVal drv_p As Integer, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'this function implements the Gibbs function of pure water as defined in
            'R. Feistel:
            'A new extended Gibbs thermodynamic potential of seawater.
            'Progress in Oceanography, 58 (2003) 43-115

            Const T0 = Celsius_temperature_si     'in K
            Const tu = 40.0#                        'in K
            Const P0 = Sealevel_pressure_si       'in Pa
            Const pu = 100000000.0# 'in Pa

            Dim y As Double, z As Double, g As Double

            aux_liq_g_f03_si = ErrorReturn

            If drv_t < 0 Then Exit Function
            If drv_p < 0 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            InitF03()

            y = (t_si - T0) / tu
            z = (p_si - P0) / pu

            g = polyf03_gyz(drv_t, drv_p, y, z)
            If g = ErrorReturn Then Exit Function

            aux_liq_g_f03_si = g / (tu ^ drv_t * pu ^ drv_p)

        End Function


        Private Sub InitF03()

            If gc(2, 0) = -12357.785933039 Then Exit Sub

            gc(2, 0) = -12357.785933039
            gc(3, 0) = 736.741204151612
            gc(4, 0) = -148.185936433658
            gc(5, 0) = 58.0259125842571
            gc(6, 0) = -18.9843846514172
            gc(7, 0) = 3.05081646487967
            gc(0, 1) = 100015.695367145
            gc(1, 1) = -270.983805184062
            gc(2, 1) = 1455.0364540468
            gc(3, 1) = -672.50778314507
            gc(4, 1) = 397.968445406972
            gc(5, 1) = -194.618310617595
            gc(6, 1) = 63.5113936641785
            gc(7, 1) = -9.63108119393062
            gc(0, 2) = -2544.5765420363
            gc(1, 2) = 776.153611613101
            gc(2, 2) = -756.558385769359
            gc(3, 2) = 499.360390819152
            gc(4, 2) = -301.815380621876
            gc(5, 2) = 120.520654902025
            gc(6, 2) = -22.2897317140459
            gc(0, 3) = 284.517778446287
            gc(1, 3) = -196.51255088122
            gc(2, 3) = 273.479662323528
            gc(3, 3) = -239.545330654412
            gc(4, 3) = 152.196371733841
            gc(5, 3) = -55.2723052340152
            gc(6, 3) = 8.17060541818112
            gc(0, 4) = -33.3146754253611
            gc(1, 4) = 28.9796526294175
            gc(2, 4) = -55.5604063817218
            gc(3, 4) = 48.8012518593872
            gc(4, 4) = -26.3748377232802
            gc(5, 4) = 6.48190668077221
            gc(0, 5) = 4.20263108803084
            gc(1, 5) = -2.13290083518327
            gc(2, 5) = 4.34420671917197
            gc(3, 5) = -1.66307106208905
            gc(0, 6) = -0.546428511471039

            'IAPWS-95 reference state condition
            'energy = 0 and entropy = 0 at the triple point:
            'gc(0, 0) = 101.342743139672
            'gc(1, 0) = 5.90578348518236

            'quadruple precision values (D.G.Wright 21 July 2008)
            'gc(0, 0) = 1.013427431396741480431228220832E2
            'gc(1, 0) = 5.905783479094018366702121889468E0
            gc(0, 0) = 101.342743139674
            gc(1, 0) = 5.90578347909402

            'RF: dynamical adjustment:
            '
            'g = polyf03_gyz(0, 0, yt, zt)
            'gt = polyf03_gyz(1, 0, yt, zt) / tu
            'gp = polyf03_gyz(0, 1, yt, zt) / pu
            '
            'gc(0, 0) = gc(0, 0) - g + pt * gp
            'gc(1, 0) = gc(1, 0) - gt * tu

        End Sub


        Private Function polyf03_gyz(ByVal drv_y As Integer, _
                                    ByVal drv_z As Integer, _
                                    ByVal y As Double, _
                                    ByVal z As Double) As Double

            'returns the value of the polynomial derivative
            '(d/dy)^drv_y (d/dz)^drv_z sum(j,k) gc(j,k)*y^j*z^k

            Dim g As Double
            Dim yj As Double, zk As Double

            Dim j As Integer, jmax As Integer
            Dim k As Integer, kmax As Integer

            Dim c As Double, L As Integer

            g = 0
            If y = 0 Then jmax = drv_y Else jmax = maxt
            If z = 0 Then kmax = drv_z Else kmax = maxp

            yj = 1.0#
            For j = drv_y To jmax   'loop over powers of y

                zk = 1.0#
                For k = drv_z To kmax    'loop over powers of z

                    If gc(j, k) <> 0 Then
                        c = gc(j, k) * yj * zk

                        For L = 1 To drv_y            'factors from y-derivatives
                            c = c * CDbl(j - L + 1)
                        Next L

                        For L = 1 To drv_z            'factors from z-derivatives
                            c = c * CDbl(k - L + 1)
                        Next L

                        g = g + c
                    End If

                    If k < kmax Then zk = zk * z
                Next k

                If j < jmax Then yj = yj * y
            Next j

            polyf03_gyz = g

        End Function


        Private Function aux_liq_g_if97_si(ByVal drv_t As Integer, _
                                        ByVal drv_p As Integer, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'This function returns the Gibbs function g(t,p) and its 1st and 2nd derivatives
            'with respect to temperature and pressure, as defined for the region 1 (liquid) in IAPWS-IF97
            '
            'output: aux_liq_g_if97_si: specific Gibbs energy in J/kg or its t-p derivative
            '
            'input:  drv_t: order of the temperature derivative (0-2)
            '        drv_p: order of the pressure derivative (0-2)
            '        t_si:  absolute temperature in K
            '        p_si:  absolute pressure in Pa

            Const tu = 1386.0#
            Const pu# = 16530000.0#
            Const R = 461.526         'J kg-1 K-1  specific gas constant
            'note this deviates from Gas_Constant_H2O_si = 461.51805

            Dim g As Double, gt As Double, gtt As Double
            Dim gp As Double, gtp As Double, gpp As Double
            Dim PI As Double, tau As Double, RT As Double

            aux_liq_g_if97_si = ErrorReturn

            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            InitIF97_1()

            PI = p_si / pu
            tau = tu / t_si
            RT = R * t_si

            Select Case drv_t

                Case 0
                    Select Case drv_p

                        Case 0 : g = gamma_1(0, 0, tau, PI)
                            If g = ErrorReturn Then Exit Function
                            aux_liq_g_if97_si = RT * g

                        Case 1 : gp = gamma_1(0, 1, tau, PI)
                            If gp = ErrorReturn Then Exit Function
                            aux_liq_g_if97_si = RT * gp / pu

                        Case 2 : gpp = gamma_1(0, 2, tau, PI)
                            If gpp = ErrorReturn Then Exit Function
                            aux_liq_g_if97_si = RT * gpp / pu ^ 2

                        Case Else : Exit Function
                    End Select

                Case 1
                    Select Case drv_p

                        Case 0 : g = gamma_1(0, 0, tau, PI)
                            If g = ErrorReturn Then Exit Function
                            gt = gamma_1(1, 0, tau, PI)
                            If gt = ErrorReturn Then Exit Function
                            aux_liq_g_if97_si = R * (g - tau * gt)

                        Case 1 : gp = gamma_1(0, 1, tau, PI)
                            If gp = ErrorReturn Then Exit Function
                            gtp = gamma_1(1, 1, tau, PI)
                            If gtp = ErrorReturn Then Exit Function
                            aux_liq_g_if97_si = R * (gp - tau * gtp) / pu

                        Case Else : Exit Function
                    End Select

                Case 2
                    Select Case drv_p

                        Case 0 : gtt = gamma_1(2, 0, tau, PI)
                            If gtt = ErrorReturn Then Exit Function
                            aux_liq_g_if97_si = R * tau ^ 2 * gtt / t_si

                        Case Else : Exit Function
                    End Select

                Case Else : Exit Function
            End Select

        End Function


        Private Function aux_vap_g_if97_si(ByVal drv_t As Integer, _
                                        ByVal drv_p As Integer, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'This function returns the Gibbs function g(t,p) and its 1st and 2nd derivatives
            'with respect to temperature and pressure, as defined for the region 2 (vapour) in IAPWS-IF97
            '
            'output: aux_vap_g_if97_si: specific Gibbs energy in J/kg or its t-p derivative
            '
            'input:  drv_t: order of the temperature derivative (0-2)
            '        drv_p: order of the pressure derivative (0-2)
            '        t_si:  absolute temperature in K
            '        p_si:  absolute pressure in Pa

            Const tu = 540.0#
            Const pu = 1000000.0#
            Const R = 461.526         'J kg-1 K-1  specific gas constant
            'note this deviates from Gas_Constant_H2O_si = 461.51805

            Dim g As Double, gt As Double, gtt As Double
            Dim gp As Double, gtp As Double, gpp As Double
            Dim PI As Double, tau As Double, RT As Double

            aux_vap_g_if97_si = ErrorReturn

            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            InitIF97_2()

            PI = p_si / pu
            tau = tu / t_si
            RT = R * t_si

            Select Case drv_t

                Case 0
                    Select Case drv_p

                        Case 0 : g = gamma_2(0, 0, tau, PI)
                            If g = ErrorReturn Then Exit Function
                            aux_vap_g_if97_si = RT * g

                        Case 1 : gp = gamma_2(0, 1, tau, PI)
                            If gp = ErrorReturn Then Exit Function
                            aux_vap_g_if97_si = RT * gp / pu

                        Case 2 : gpp = gamma_2(0, 2, tau, PI)
                            If gpp = ErrorReturn Then Exit Function
                            aux_vap_g_if97_si = RT * gpp / pu ^ 2

                        Case Else : Exit Function
                    End Select

                Case 1
                    Select Case drv_p

                        Case 0 : g = gamma_2(0, 0, tau, PI)
                            If g = ErrorReturn Then Exit Function
                            gt = gamma_2(1, 0, tau, PI)
                            If gt = ErrorReturn Then Exit Function
                            aux_vap_g_if97_si = R * (g - tau * gt)

                        Case 1 : gp = gamma_2(0, 1, tau, PI)
                            If gp = ErrorReturn Then Exit Function
                            gtp = gamma_2(1, 1, tau, PI)
                            If gtp = ErrorReturn Then Exit Function
                            aux_vap_g_if97_si = R * (gp - tau * gtp) / pu

                        Case Else : Exit Function
                    End Select

                Case 2
                    Select Case drv_p

                        Case 0 : gtt = gamma_2(2, 0, tau, PI)
                            If gtt = ErrorReturn Then Exit Function
                            aux_vap_g_if97_si = R * tau ^ 2 * gtt / t_si

                        Case Else : Exit Function
                    End Select

                Case Else : Exit Function
            End Select

        End Function


        Private Function aux_liq_density_if97_si(ByVal t_si As Double, _
                                                ByVal p_si As Double) As Double

            'This function returns the density of liquid water as a function of temperature
            'and pressure, in the region 1 (liquid) in IAPWS-IF97

            'output:  aux_liq_density_if97_si: density in kg/m^3

            'input:   t_si: absolute temperature in K
            '         p_si: absolute pressure in Pa

            Dim d As Double

            aux_liq_density_if97_si = ErrorReturn

            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            d = aux_liq_g_if97_si(0, 1, t_si, p_si)

            If d = ErrorReturn Then Exit Function
            If d <= 0 Then Exit Function

            aux_liq_density_if97_si = 1.0# / d

        End Function


        Private Function aux_vap_density_if97_si(ByVal t_si As Double, _
                                                ByVal p_si As Double) As Double

            'This function returns the density of water vapour as a function of temperature
            'and pressure, in the region 2 (vapour) in IAPWS-IF97

            'output:  aux_vap_density_if97_si: density in kg/m^3

            'input:   t_si: absolute temperature in K
            '         p_si: absolute pressure in Pa

            Dim d As Double

            aux_vap_density_if97_si = ErrorReturn

            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            d = aux_vap_g_if97_si(0, 1, t_si, p_si)

            If d = ErrorReturn Then Exit Function
            If d <= 0 Then Exit Function

            aux_vap_density_if97_si = 1.0# / d

        End Function


        Private Sub InitIF97_1()

            Dim i As Integer

            If n1i(1) = 0.14632971213167 Then Exit Sub

            'Table 2. Numerical values of the coefficients and exponents of the dimensionless Gibbs free energy
            'for region 1, Eq. (7)
            'i      Ii          Ji           ni                             i       Ii           Ji            ni
            i = 1 : i1i(i) = 0 : j1i(i) = -2 : n1i(i) = 0.14632971213167 : i = 18 : i1i(i) = 2 : j1i(i) = 3 : n1i(i) = -0.0000044141845330846
            i = 2 : i1i(i) = 0 : j1i(i) = -1 : n1i(i) = -0.84548187169114 : i = 19 : i1i(i) = 2 : j1i(i) = 17 : n1i(i) = -0.00000000000000072694996297594
            i = 3 : i1i(i) = 0 : j1i(i) = 0 : n1i(i) = -3.756360367204 : i = 20 : i1i(i) = 3 : j1i(i) = -4 : n1i(i) = -0.000031679644845054
            i = 4 : i1i(i) = 0 : j1i(i) = 1 : n1i(i) = 3.3855169168385 : i = 21 : i1i(i) = 3 : j1i(i) = 0 : n1i(i) = -0.0000028270797985312
            i = 5 : i1i(i) = 0 : j1i(i) = 2 : n1i(i) = -0.95791963387872 : i = 22 : i1i(i) = 3 : j1i(i) = 6 : n1i(i) = -0.00000000085205128120103
            i = 6 : i1i(i) = 0 : j1i(i) = 3 : n1i(i) = 0.15772038513228 : i = 23 : i1i(i) = 4 : j1i(i) = -5 : n1i(i) = -0.0000022425281908
            i = 7 : i1i(i) = 0 : j1i(i) = 4 : n1i(i) = -0.016616417199501 : i = 24 : i1i(i) = 4 : j1i(i) = -2 : n1i(i) = -0.00000065171222895601
            i = 8 : i1i(i) = 0 : j1i(i) = 5 : n1i(i) = 0.00081214629983568 : i = 25 : i1i(i) = 4 : j1i(i) = 10 : n1i(i) = -0.00000000000014341729937924
            i = 9 : i1i(i) = 1 : j1i(i) = -9 : n1i(i) = 0.00028319080123804 : i = 26 : i1i(i) = 5 : j1i(i) = -8 : n1i(i) = -0.00000040516996860117
            i = 10 : i1i(i) = 1 : j1i(i) = -7 : n1i(i) = -0.00060706301565874 : i = 27 : i1i(i) = 8 : j1i(i) = -11 : n1i(i) = -0.0000000012734301741641
            i = 11 : i1i(i) = 1 : j1i(i) = -1 : n1i(i) = -0.018990068218419 : i = 28 : i1i(i) = 8 : j1i(i) = -6 : n1i(i) = -0.00000000017424871230634
            i = 12 : i1i(i) = 1 : j1i(i) = 0 : n1i(i) = -0.032529748770505 : i = 29 : i1i(i) = 21 : j1i(i) = -29 : n1i(i) = -6.8762131295531E-19
            i = 13 : i1i(i) = 1 : j1i(i) = 1 : n1i(i) = -0.021841717175414 : i = 30 : i1i(i) = 23 : j1i(i) = -31 : n1i(i) = 1.4478307828521E-20
            i = 14 : i1i(i) = 1 : j1i(i) = 3 : n1i(i) = -0.00005283835796993 : i = 31 : i1i(i) = 29 : j1i(i) = -38 : n1i(i) = 2.6335781662795E-23
            i = 15 : i1i(i) = 2 : j1i(i) = -3 : n1i(i) = -0.00047184321073267 : i = 32 : i1i(i) = 30 : j1i(i) = -39 : n1i(i) = -1.1947622640071E-23
            i = 16 : i1i(i) = 2 : j1i(i) = 0 : n1i(i) = -0.00030001780793026 : i = 33 : i1i(i) = 31 : j1i(i) = -40 : n1i(i) = 1.8228094581404E-24
            i = 17 : i1i(i) = 2 : j1i(i) = 1 : n1i(i) = 0.000047661393906987 : i = 34 : i1i(i) = 32 : j1i(i) = -41 : n1i(i) = -9.3537087292458E-26

        End Sub


        Private Sub InitIF97_2()

            Dim i As Integer

            If n0i(i) = -9.6927686500217 Then Exit Sub

            'Table 10. Numerical values of the coefficients and exponents of the ideal-gas part gamma_0 of the
            'dimensionless Gibbs free energy for region 2, Eq. (16)
            'i     Ji0          ni0
            i = 1 : j0i(i) = 0 : n0i(i) = -9.6927686500217
            i = 2 : j0i(i) = 1 : n0i(i) = 10.086655968018
            i = 3 : j0i(i) = -5 : n0i(i) = -0.005608791128302
            i = 4 : j0i(i) = -4 : n0i(i) = 0.071452738081455
            i = 5 : j0i(i) = -3 : n0i(i) = -0.40710498223928
            i = 6 : j0i(i) = -2 : n0i(i) = 1.4240819171444
            i = 7 : j0i(i) = -1 : n0i(i) = -4.383951131945
            i = 8 : j0i(i) = 2 : n0i(i) = -0.28408632460772
            i = 9 : j0i(i) = 3 : n0i(i) = 0.021268463753307
            'If Eq. (16) is incorporated into Eq. (18), instead of the-values for n0i(1) and n0i(1)
            'given above, the following values
            'n0i(1) = -0.96937268393049E1
            'n0i(2) = 0.10087275970006E2
            'should be used


            'Table 11. Numerical values of the coefficients and exponents of the
            'residual part gamma_r of the dimensionless Gibbs free energy for
            'region 2, Eq. (17)
            'i      Ii           Ji           ni
            i = 1 : iri(i) = 1 : jri(i) = 0 : nri(i) = -0.0017731742473213
            i = 2 : iri(i) = 1 : jri(i) = 1 : nri(i) = -0.017834862292358
            i = 3 : iri(i) = 1 : jri(i) = 2 : nri(i) = -0.045996013696365
            i = 4 : iri(i) = 1 : jri(i) = 3 : nri(i) = -0.057581259083432
            i = 5 : iri(i) = 1 : jri(i) = 6 : nri(i) = -0.05032527872793
            i = 6 : iri(i) = 2 : jri(i) = 1 : nri(i) = -0.000033032641670203
            i = 7 : iri(i) = 2 : jri(i) = 2 : nri(i) = -0.00018948987516315
            i = 8 : iri(i) = 2 : jri(i) = 4 : nri(i) = -0.0039392777243355
            i = 9 : iri(i) = 2 : jri(i) = 7 : nri(i) = -0.043797295650573
            i = 10 : iri(i) = 2 : jri(i) = 36 : nri(i) = -0.000026674547914087
            i = 11 : iri(i) = 3 : jri(i) = 0 : nri(i) = 0.000000020481737692309
            i = 12 : iri(i) = 3 : jri(i) = 1 : nri(i) = 0.00000043870667284435
            i = 13 : iri(i) = 3 : jri(i) = 3 : nri(i) = -0.00003227767723857
            i = 14 : iri(i) = 3 : jri(i) = 6 : nri(i) = -0.0015033924542148
            i = 15 : iri(i) = 3 : jri(i) = 35 : nri(i) = -0.040668253562649
            i = 16 : iri(i) = 4 : jri(i) = 1 : nri(i) = -0.00000000078847309559367
            i = 17 : iri(i) = 4 : jri(i) = 2 : nri(i) = 0.000000012790717852285
            i = 18 : iri(i) = 4 : jri(i) = 3 : nri(i) = 0.00000048225372718507
            i = 19 : iri(i) = 5 : jri(i) = 7 : nri(i) = 0.0000022922076337661
            i = 20 : iri(i) = 6 : jri(i) = 3 : nri(i) = -0.000000000016714766451061
            i = 21 : iri(i) = 6 : jri(i) = 16 : nri(i) = -0.0021171472321355
            i = 22 : iri(i) = 6 : jri(i) = 35 : nri(i) = -23.895741934104
            i = 23 : iri(i) = 7 : jri(i) = 0 : nri(i) = -5.905956432427E-18
            i = 24 : iri(i) = 7 : jri(i) = 11 : nri(i) = -0.0000012621808899101
            i = 25 : iri(i) = 7 : jri(i) = 25 : nri(i) = -0.038946842435739
            i = 26 : iri(i) = 8 : jri(i) = 8 : nri(i) = 0.000000000011256211360459
            i = 27 : iri(i) = 8 : jri(i) = 36 : nri(i) = -8.2311340897998
            i = 28 : iri(i) = 9 : jri(i) = 13 : nri(i) = 0.000000019809712802088
            i = 29 : iri(i) = 10 : jri(i) = 4 : nri(i) = 1.0406965210174E-19
            i = 30 : iri(i) = 10 : jri(i) = 10 : nri(i) = -0.00000000000010234747095929
            i = 31 : iri(i) = 10 : jri(i) = 14 : nri(i) = -0.0000000010018179379511
            i = 32 : iri(i) = 16 : jri(i) = 29 : nri(i) = -0.000000000080882908646985
            i = 33 : iri(i) = 16 : jri(i) = 50 : nri(i) = 0.10693031879409
            i = 34 : iri(i) = 18 : jri(i) = 57 : nri(i) = -0.33662250574171
            i = 35 : iri(i) = 20 : jri(i) = 20 : nri(i) = 8.9185845355421E-25
            i = 36 : iri(i) = 20 : jri(i) = 35 : nri(i) = 0.00000000000030629316876232
            i = 37 : iri(i) = 20 : jri(i) = 48 : nri(i) = -0.0000042002467698208
            i = 38 : iri(i) = 21 : jri(i) = 21 : nri(i) = -5.9056029685639E-26
            i = 39 : iri(i) = 22 : jri(i) = 53 : nri(i) = 0.0000037826947613457
            i = 40 : iri(i) = 23 : jri(i) = 39 : nri(i) = -0.0000000000000012768608934681
            i = 41 : iri(i) = 24 : jri(i) = 26 : nri(i) = 7.3087610595061E-29
            i = 42 : iri(i) = 24 : jri(i) = 40 : nri(i) = 5.5414715350778E-17
            i = 43 : iri(i) = 24 : jri(i) = 58 : nri(i) = -0.0000009436970724121

        End Sub


        Private Function gamma_0(ByVal drv_t As Integer, _
                                ByVal drv_p As Integer, _
                                ByVal tau As Double, _
                                ByVal PI As Double) As Double

            'this function implements the derivatives of gamma_0 as given in Table 13 of IF-97

            Dim g As Double, pwrt As Double
            Dim i As Integer, k As Integer

            gamma_0 = ErrorReturn

            If PI <= 0 Then Exit Function
            If tau <= 0 Then Exit Function
            If drv_t < 0 Then Exit Function
            If drv_p < 0 Then Exit Function

            g = 0
            If drv_t = 0 Then
                If drv_p = 0 Then
                    g = Log(PI)
                Else
                    g = 1.0# / PI
                    For k = 2 To drv_p
                        g = (1 - k) * g / PI
                    Next k
                End If
            End If

            If drv_p = 0 Then
                For i = 1 To 9
                    pwrt = tau ^ (j0i(i) - drv_t)
                    For k = 0 To drv_t - 1
                        pwrt = pwrt * (j0i(i) - k)
                    Next k
                    g = g + n0i(i) * pwrt
                Next i
            End If

            gamma_0 = g

        End Function


        Private Function gamma_1(ByVal drv_t As Integer, _
                                ByVal drv_p As Integer, _
                                ByVal tau As Double, _
                                ByVal PI As Double) As Double

            'this function implements the derivatives of gamma as given in Table 4 of IF-97

            Dim i As Integer, k As Integer
            Dim g As Double, pp As Double, Tt As Double
            Dim pwrt As Double, pwrp As Double

            gamma_1 = ErrorReturn

            If PI <= 0 Then Exit Function
            If tau <= 0 Then Exit Function
            If drv_t < 0 Then Exit Function
            If drv_p < 0 Then Exit Function

            pp = 7.1 - PI
            Tt = tau - 1.222

            g = 0
            For i = 1 To 34

                If Tt = 0 Then
                    Select Case j1i(i) - drv_t
                        Case 0 : pwrt = 1
                        Case Is > 0 : pwrt = 0
                        Case Else : Exit Function
                    End Select
                Else
                    pwrt = Tt ^ (j1i(i) - drv_t)
                End If
                For k = 0 To drv_t - 1
                    pwrt = pwrt * (j1i(i) - k)
                Next k

                If pp = 0 Then
                    Select Case i1i(i) - drv_p
                        Case 0 : pwrp = 1
                        Case Is > 0 : pwrp = 0
                        Case Else : Exit Function
                    End Select
                Else
                    pwrp = pp ^ (i1i(i) - drv_p)
                End If
                For k = 0 To drv_p - 1
                    pwrp = -pwrp * (i1i(i) - k)
                Next k

                g = g + n1i(i) * pwrp * pwrt

            Next i

            gamma_1 = g

        End Function


        Private Function gamma_2(ByVal drv_t As Integer, _
                                ByVal drv_p As Integer, _
                                ByVal tau As Double, _
                                ByVal PI As Double) As Double

            'this function implements the derivatives of gamma as given in Eq. 15 of IF-97

            Dim g0 As Double, gr As Double

            InitIF97_2()

            gamma_2 = ErrorReturn

            g0 = gamma_0(drv_t, drv_p, tau, PI)
            If g0 = ErrorReturn Then Exit Function

            gr = gamma_r(drv_t, drv_p, tau, PI)
            If gr = ErrorReturn Then Exit Function

            gamma_2 = g0 + gr

        End Function


        Private Function gamma_r(ByVal drv_t As Integer, _
                                ByVal drv_p As Integer, _
                                ByVal tau As Double, _
                                ByVal PI As Double) As Double

            'this function implements the derivatives of gamma_r as given in Table 14 of IF-97

            Dim i As Integer, k As Integer
            Dim g As Double, Tt As Double
            Dim pwrt As Double, pwrp As Double

            gamma_r = ErrorReturn

            If PI <= 0 Then Exit Function
            If tau <= 0 Then Exit Function
            If drv_t < 0 Then Exit Function
            If drv_p < 0 Then Exit Function

            Tt = tau - 0.5

            g = 0
            For i = 1 To 43

                If Tt = 0 Then
                    Select Case jri(i) - drv_t
                        Case 0 : pwrt = 1
                        Case Is > 0 : pwrt = 0
                        Case Else : Exit Function
                    End Select
                Else
                    pwrt = Tt ^ (jri(i) - drv_t)
                End If
                For k = 0 To drv_t - 1
                    pwrt = pwrt * (jri(i) - k)
                Next k

                pwrp = PI ^ (iri(i) - drv_p)
                For k = 0 To drv_p - 1
                    pwrp = pwrp * (iri(i) - k)
                Next k

                g = g + nri(i) * pwrp * pwrt

            Next i

            gamma_r = g

        End Function


#End Region

#Region "Sal_1"

        '#########################################################################

        'This module requires the library module
        '     Constants_0_Mdl, file Constants_0.bas

        '#########################################################################


        'This module implements the saline part of the Gibbs potential of seawater
        'in the form of 7 coefficients of its power expansion in salinity,
        'and their first and second partial derivatives with respect to temperature
        'and pressure, as defined in IAPWS-08:

        'Release on the IAPWS Formulation 2008 for the Thermodynamic Properties of Seawater
        'The International Association for the Properties of Water and Steam
        'Berlin, Germany, September 2008

        'Implementation in VB6 by Rainer Feistel
        'for publication in Ocean Science 2009, as described in the papers

        'Feistel, R., Wright, D.G., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part I: Background and Equations.
        'Ocean Science, 2009

        'Wright, D.G., Feistel, R., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part II: The Library Routines,
        'Ocean Science, 2009

        'If IsExtension2010 = True is specified, the Gibbs function is optionally extended
        '(although not endorsed by IAPWS) as described in
        'R. Feistel
        'Extended Equation of State for Seawater at Elevated Temperature and Salinity
        'Reference updated: Desalination, 250, 14-18, 2010


        'Private Const ErrorReturn = 9.99999999E+98

        Private Const maxs = 7, maxt2 = 6, maxp2 = 5   'Max. powers of polynomials

        Private gi(maxs, maxt, maxp) As Double       'Array of coeffs of the Gibbs function terms


        Public Function sal_g_term_si(ByVal term As Integer, _
                                    ByVal drv_t As Integer, _
                                    ByVal drv_p As Integer, _
                                    ByVal t_si As Double, _
                                    ByVal p_si As Double) As Double

            'returns the T-P derivative of the i-th term  (d/dT)^drv_t (d/dP)^drv_p gi(T, P)
            'of the salinity expansion of the saline Gibbs function gS,
            '
            ' gS(S,T,P) = g1(T,P)*S*ln(S) + sum(i>1) gi(T,P)*S^(i/2)
            '
            ' S = absolute salinity in kg/kg, i.e. the mass fraction of salt in seawater
            ' t_si = T = absolute temperature in K, ITS-90
            ' p_si = P = absolute pressure in Pa
            ' gS = specific Gibbs energy in J/kg, relative to pure water

            'check values with default settings:

            'sal_g_term_si(1,0,0,300,1E6)= 79427.9694845756
            'sal_g_term_si(1,1,0,300,1E6)= 264.759898281919
            'sal_g_term_si(1,0,1,300,1E6)= 0
            'sal_g_term_si(1,2,0,300,1E6)= 0
            'sal_g_term_si(1,1,1,300,1E6)= 0
            'sal_g_term_si(1,0,2,300,1E6)= 0

            'sal_g_term_si(2,0,0,300,1E6)= 301223.934546274
            'sal_g_term_si(2,1,0,300,1E6)= 1558.10730392553
            'sal_g_term_si(2,0,1,300,1E6)= -7.60101106780255E-04
            'sal_g_term_si(2,2,0,300,1E6)= 19.0397747693888
            'sal_g_term_si(2,1,1,300,1E6)= 9.35496617544689E-07
            'sal_g_term_si(2,0,2,300,1E6)= 1.28077803425625E-12

            'sal_g_term_si(3,0,0,300,1E6)= -345570.781497815
            'sal_g_term_si(3,1,0,300,1E6)= -1749.19911586842
            'sal_g_term_si(3,0,1,300,1E6)= 2.01608449880727E-04
            'sal_g_term_si(3,2,0,300,1E6)= -9.3208882361981
            'sal_g_term_si(3,1,1,300,1E6)= 2.17412289836757E-08
            'sal_g_term_si(3,0,2,300,1E6)= -4.78063827317764E-13

            'sal_g_term_si(4,0,0,300,1E6)= 1468073.64558789
            'sal_g_term_si(4,1,0,300,1E6)= 7741.24404962481
            'sal_g_term_si(4,0,1,300,1E6)= -4.33952339915708E-04
            'sal_g_term_si(4,2,0,300,1E6)= -6.13689642369677
            'sal_g_term_si(4,1,1,300,1E6)= -3.50876195079922E-06
            'sal_g_term_si(4,0,2,300,1E6)= -6.06204383304926E-13

            'sal_g_term_si(5,0,0,300,1E6)= -3776969.31546023
            'sal_g_term_si(5,1,0,300,1E6)= -15135.6522247701
            'sal_g_term_si(5,0,1,300,1E6)= 1.11272425476394E-03
            'sal_g_term_si(5,2,0,300,1E6)= 0
            'sal_g_term_si(5,1,1,300,1E6)= 0
            'sal_g_term_si(5,0,2,300,1E6)= 0

            'sal_g_term_si(6,0,0,300,1E6)= 6151235.69234474
            'sal_g_term_si(6,1,0,300,1E6)= 14157.050993291
            'sal_g_term_si(6,0,1,300,1E6)= 0
            'sal_g_term_si(6,2,0,300,1E6)= 0
            'sal_g_term_si(6,1,1,300,1E6)= 0
            'sal_g_term_si(6,0,2,300,1E6)= 0

            'sal_g_term_si(7,0,0,300,1E6)= -3734033.38866189
            'sal_g_term_si(7,1,0,300,1E6)= 0
            'sal_g_term_si(7,0,1,300,1E6)= 0
            'sal_g_term_si(7,2,0,300,1E6)= 0
            'sal_g_term_si(7,1,1,300,1E6)= 0
            'sal_g_term_si(7,0,2,300,1E6)= 0


            'check values with Public Const IsExtension2010 = True:

            'sal_g_term_si(1,0,0,300,1E6)= 79427.9694845756
            'sal_g_term_si(1,1,0,300,1E6)= 264.759898281919
            'sal_g_term_si(1,0,1,300,1E6)= 0
            'sal_g_term_si(1,2,0,300,1E6)= 0
            'sal_g_term_si(1,1,1,300,1E6)= 0
            'sal_g_term_si(1,0,2,300,1E6)= 0

            'sal_g_term_si(2,0,0,300,1E6)= 301223.934546274
            'sal_g_term_si(2,1,0,300,1E6)= 1558.10730392553
            'sal_g_term_si(2,0,1,300,1E6)= -7.60101106780255E-04
            'sal_g_term_si(2,2,0,300,1E6)= 19.0397747693888
            'sal_g_term_si(2,1,1,300,1E6)= 9.35496617544689E-07
            'sal_g_term_si(2,0,2,300,1E6)= 1.28077803425625E-12

            'sal_g_term_si(3,0,0,300,1E6)= -345570.781497815
            'sal_g_term_si(3,1,0,300,1E6)= -1749.19911586842
            'sal_g_term_si(3,0,1,300,1E6)= 2.01608449880727E-04
            'sal_g_term_si(3,2,0,300,1E6)= -9.3208882361981
            'sal_g_term_si(3,1,1,300,1E6)= 2.17412289836757E-08
            'sal_g_term_si(3,0,2,300,1E6)= -4.78063827317764E-13

            'sal_g_term_si(4,0,0,300,1E6)= 1468073.64558789
            'sal_g_term_si(4,1,0,300,1E6)= 7741.24404962481
            'sal_g_term_si(4,0,1,300,1E6)= -4.33952339915708E-04
            'sal_g_term_si(4,2,0,300,1E6)= -6.13689642369677
            'sal_g_term_si(4,1,1,300,1E6)= -3.50876195079922E-06
            'sal_g_term_si(4,0,2,300,1E6)= -6.06204383304926E-13

            'sal_g_term_si(5,0,0,300,1E6)= -3776969.31546023
            'sal_g_term_si(5,1,0,300,1E6)= -15135.6522247701
            'sal_g_term_si(5,0,1,300,1E6)= 1.11272425476394E-03
            'sal_g_term_si(5,2,0,300,1E6)= 0
            'sal_g_term_si(5,1,1,300,1E6)= 0
            'sal_g_term_si(5,0,2,300,1E6)= 0

            'sal_g_term_si(6,0,0,300,1E6)= 6151235.69234474
            'sal_g_term_si(6,1,0,300,1E6)= 14157.050993291
            'sal_g_term_si(6,0,1,300,1E6)= 0
            'sal_g_term_si(6,2,0,300,1E6)= 0
            'sal_g_term_si(6,1,1,300,1E6)= 0
            'sal_g_term_si(6,0,2,300,1E6)= 0

            'sal_g_term_si(7,0,0,300,1E6)= -3734033.38866189
            'sal_g_term_si(7,1,0,300,1E6)= 0
            'sal_g_term_si(7,0,1,300,1E6)= 0
            'sal_g_term_si(7,2,0,300,1E6)= 0
            'sal_g_term_si(7,1,1,300,1E6)= 0
            'sal_g_term_si(7,0,2,300,1E6)= 0

            sal_g_term_si = ErrorReturn

            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            If drv_t < 0 Then Exit Function
            If drv_p < 0 Then Exit Function

            If term < 1 Or term > maxs Then
                sal_g_term_si = 0
                Exit Function
            End If

            InitIAPWS08()

            If check_limits = 1 Then
                'SAL_LIMITS
                If t_si < sal_tmin Or t_si > sal_tmax Or _
                    p_si < sal_pmin Or p_si > sal_pmax Then Exit Function
            End If

            sal_g_term_si = poly_gtp(term, drv_t, drv_p, t_si, p_si)

        End Function


        Private Sub InitIAPWS08()

            Const ups = SO_salinity_si / 35.0#
            Const su = 40.0# * ups
            Const gi100 = 5812.81456626732 * 0.5 / su

            If gi(1, 0, 0) = gi100 Then Exit Sub

            Dim gc(maxs, maxt2, maxp2) As Double       'Local array of coeffs of the Gibbs function
            Dim i As Integer, j As Integer, k As Integer
            Dim ln_su As Double, sqr_su As Double

            'Coefficients from the IAPWS Release 2008

            'Logarithmic terms from Reference Composition 2008, computed 01 Aug 2007
            'Deep-Sea Research I 55(2008)50-72.
            gc(1, 0, 0) = 5812.81456626732
            gc(1, 1, 0) = 851.226734946706

            'Seawater Reference State defined by WG127, computed 03 March 2008
            gc(2, 0, 0) = 1416.27648484197  'computed from a quadruple-precision implementation
            gc(2, 1, 0) = 168.072408311545

            'Thermal and colligative properties at 101325 Pa, computed 01 Aug 2007
            gc(3, 0, 0) = -2432.14662381794
            gc(4, 0, 0) = 2025.80115603697
            gc(5, 0, 0) = -1091.66841042967
            gc(6, 0, 0) = 374.60123787784
            gc(7, 0, 0) = -48.5891069025409
            gc(3, 1, 0) = -493.407510141682
            gc(4, 1, 0) = 543.835333000098
            gc(5, 1, 0) = -196.028306689776
            gc(6, 1, 0) = 36.7571622995805
            gc(2, 2, 0) = 880.031352997204
            gc(3, 2, 0) = -43.0664675978042
            gc(4, 2, 0) = -68.5572509204491
            gc(2, 3, 0) = -225.267649263401
            gc(3, 3, 0) = -10.0227370861875
            gc(4, 3, 0) = 49.3667694856254
            gc(2, 4, 0) = 91.4260447751259
            gc(3, 4, 0) = 0.875600661808945
            gc(4, 4, 0) = -17.1397577419788
            gc(2, 5, 0) = -21.6603240875311
            gc(4, 5, 0) = 2.49697009569508
            gc(2, 6, 0) = 2.13016970847183

            'coefficients of the pressure part of the 2003 Gibbs function
            'Progress in Oceanography 58(2003)43-114
            gc(2, 0, 1) = -3310.49154044839
            gc(3, 0, 1) = 199.459603073901
            gc(4, 0, 1) = -54.7919133532887
            gc(5, 0, 1) = 36.0284195611086
            gc(2, 1, 1) = 729.116529735046
            gc(3, 1, 1) = -175.292041186547
            gc(4, 1, 1) = -22.6683558512829
            gc(2, 2, 1) = -860.764303783977
            gc(3, 2, 1) = 383.058066002476
            gc(2, 3, 1) = 694.244814133268
            gc(3, 3, 1) = -460.319931801257
            gc(2, 4, 1) = -297.728741987187
            gc(3, 4, 1) = 234.565187611355

            gc(2, 0, 2) = 384.794152978599
            gc(3, 0, 2) = -52.2940909281335
            gc(4, 0, 2) = -4.08193978912261
            gc(2, 1, 2) = -343.956902961561
            gc(3, 1, 2) = 83.1923927801819
            gc(2, 2, 2) = 337.409530269367
            gc(3, 2, 2) = -54.1917262517112
            gc(2, 3, 2) = -204.889641964903
            gc(2, 4, 2) = 74.726141138756

            gc(2, 0, 3) = -96.5324320107458
            gc(3, 0, 3) = 68.0444942726459
            gc(4, 0, 3) = -30.1755111971161
            gc(2, 1, 3) = 124.687671116248
            gc(3, 1, 3) = -29.483064349429
            gc(2, 2, 3) = -178.314556207638
            gc(3, 2, 3) = 25.6398487389914
            gc(2, 3, 3) = 113.561697840594
            gc(2, 4, 3) = -36.4872919001588

            gc(2, 0, 4) = 15.8408172766824
            gc(3, 0, 4) = -3.41251932441282
            gc(2, 1, 4) = -31.656964386073
            gc(2, 2, 4) = 44.2040358308
            gc(2, 3, 4) = -11.1282734326413

            gc(2, 0, 5) = -2.62480156590992
            gc(2, 1, 5) = 7.04658803315449
            gc(2, 2, 5) = -7.92001547211682

            If IsExtension2010 Then
                'R. Feistel
                'Extended Equation of State for Seawater at Elevated Temperature and Salinity
                'Desalination, 250, 14-18, 2010
                '(note that the coefficients given in that paper are incorrect and
                ' must be multiplied by 100, as given below)
                '
                'to cover the measurements of
                'F. Millero and F. Huang
                'Ocean Sci. Discuss. 6 (2009) 153-169.
                'http://www.ocean-sci-discuss.net/6/153/2009/

                gc(2, 1, 1) = gc(2, 1, 1) - 34.7600838235511
                gc(3, 1, 1) = gc(3, 1, 1) + 93.5178208024272
                gc(4, 1, 1) = gc(4, 1, 1) - 60.3483495593212
                gc(2, 2, 1) = gc(2, 2, 1) + 22.8081199116236
                gc(4, 2, 1) = gc(4, 2, 1) - 24.2869748847311
                gc(2, 3, 1) = gc(2, 3, 1) - 56.0725782144008
                gc(3, 3, 1) = gc(3, 3, 1) - 14.3523729429211
                gc(4, 3, 1) = gc(4, 3, 1) + 92.6367388049097
                gc(4, 4, 1) = gc(4, 4, 1) - 41.6658900599273
                gc(2, 5, 1) = gc(2, 5, 1) + 64.5288813326254
                gc(3, 5, 1) = gc(3, 5, 1) - 40.3505133068118
                gc(2, 6, 1) = gc(2, 6, 1) - 4.32746069361075
                gc(4, 6, 1) = gc(4, 6, 1) + 2.05954716712622

            End If

            'Modified coefficients as required for computing each of the terms g1 to g7
            'of the salinity expansion of gS, stored into the global array gi()

            ln_su = Log(su)
            sqr_su = Sqrt(su)

            For j = 0 To maxt2
                For k = 0 To maxp2
                    gi(1, j, k) = gc(1, j, k) * 0.5 / su
                    gi(2, j, k) = (gc(2, j, k) - 0.5 * gc(1, j, k) * ln_su) / su
                    For i = 3 To maxs
                        gi(i, j, k) = gc(i, j, k) / sqr_su ^ i
                    Next i
                Next k
            Next j

        End Sub


        Private Function poly_gyz(ByVal term As Integer, _
                                ByVal drv_y As Integer, _
                                ByVal drv_z As Integer, _
                                ByVal y As Double, _
                                ByVal z As Double) As Double

            'returns the value of the polynomial derivative
            '(d/dy)^drv_y (d/dz)^drv_z sum(j,k) gi(term,j,k)*y^j*z^k

            Dim g As Double
            Dim yj As Double, zk As Double

            Dim j As Integer, jmax As Integer
            Dim k As Integer, kmax As Integer

            Dim c As Double, L As Integer

            g = 0
            If y = 0 Then jmax = drv_y Else jmax = maxt2
            If z = 0 Then kmax = drv_z Else kmax = maxp2

            yj = 1.0#
            For j = drv_y To jmax   'loop over powers of y

                zk = 1.0#
                For k = drv_z To kmax    'loop over powers of z

                    If gi(term, j, k) <> 0 Then
                        c = gi(term, j, k) * yj * zk

                        For L = 1 To drv_y            'factors from y-derivatives
                            c = c * CDbl(j - L + 1)
                        Next L

                        For L = 1 To drv_z            'factors from z-derivatives
                            c = c * CDbl(k - L + 1)
                        Next L

                        g = g + c
                    End If

                    If k < kmax Then zk = zk * z
                Next k

                If j < jmax Then yj = yj * y
            Next j

            poly_gyz = g

        End Function


        Private Function poly_gtp(ByVal term As Integer, _
                                ByVal drv_t As Integer, _
                                ByVal drv_p As Integer, _
                                ByVal t_si As Double, _
                                ByVal p_si As Double) As Double

            'returns the T-P derivative of the i-th term  (d/dT)^drv_t (d/dP)^drv_p gi(T, P)
            'of the salinity expansion of the saline Gibbs function gS,
            '
            ' gS(S,T,P) = g1(T,P)*S*ln(S) + sum(i>1) gi(T,P)*S^(i/2)
            '
            ' S = absolute salinity in kg/kg, i.e. the mass fraction of salt in seawater
            ' T = t_si = absolute temperature in K, ITS-90
            ' P = p_si = absolute pressure in Pa

            Const tu = 40.0#
            Const pu = 100000000.0#

            Dim y As Double, z As Double

            'reduced input values of T, P
            y = (t_si - Celsius_temperature_si) / tu
            z = (p_si - Sealevel_pressure_si) / pu

            poly_gtp = poly_gyz(term, drv_t, drv_p, y, z) / (tu ^ drv_t * pu ^ drv_p)

        End Function

#End Region

#Region "Sal_2"

        '#########################################################################

        'This module requires the library modules
        '     Constants_0_Mdl, file Constants_0.bas
        '     Sal_1_Mdl,     file Sal_1.bas

        '#########################################################################


        'This module implements the saline part of the Gibbs function of seawater,
        'sal_g_si(drv_s, drv_t, drv_p, sa_si, t_si, p_si) as well as its 1st and 2n
        'partial derivatives with respect to salinity, temperature and pressure,
        'as well as thermodynamic properties of sea salt in seawater as functions
        'of absolute salinity in kg/kg, absolute temperature in K and absolute pressure in Pa,
        'computed from the salinity expansion terms of the Gibbs potential,
        'as defined in IAPWS-08:

        'Release on the IAPWS Formulation 2008 for the Thermodynamic Properties of Seawater
        'The International Association for the Properties of Water and Steam
        'Berlin, Germany, September 2008

        'Implementation in VB6 by Rainer Feistel
        'for publication in Ocean Science, as described in the papers

        'Feistel, R., Wright, D.G., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part I: Background and Equations.
        'Ocean Science, 2009

        'Wright, D.G., Feistel, R., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part II: The Library Routines,
        'Ocean Science, 2009


        Public Function sal_g_si(ByVal drv_s As Integer, _
                                ByVal drv_t As Integer, _
                                ByVal drv_p As Integer, _
                                ByVal sa_si As Double, _
                                ByVal t_si As Double, _
                                ByVal p_si As Double) As Double

            'returns the S-T-P derivative (d/dS)^drv_s (d/dT)^drv_t (d/dP)^drv_p gS(S,T,P)
            'of the saline Gibbs function gS,
            '
            ' gS(S,T,P) = g1(T,P)*S*ln(S) + sum(i>1) gi(T,P)*S^(i/2)
            '
            ' sa_si = S = absolute salinity in kg/kg, i.e. the mass fraction of salt in seawater
            ' t_si  = T = absolute temperature in K, ITS-90
            ' p_si  = P = absolute pressure in Pa
            ' sal_g_si = gS = specific Gibbs energy in J/kg, relative to pure water

            'check values:
            'sal_g_si(0,0,0,0.035,300,1E6)= 127.03364030915
            'sal_g_si(1,0,0,0.035,300,1E6)= 77949.2100394602
            'sal_g_si(0,1,0,0.035,300,1E6)= 18.636040707255
            'sal_g_si(0,0,1,0.035,300,1E6)= -2.55600080318783E-05
            'sal_g_si(2,0,0,0.035,300,1E6)= 2248200.54659641
            'sal_g_si(1,1,0,0.035,300,1E6)= 790.563810558295
            'sal_g_si(1,0,1,0.035,300,1E6)= -7.15686520587585E-04
            'sal_g_si(0,2,0,0.035,300,1E6)= 0.597842170749115
            'sal_g_si(0,1,1,0.035,300,1E6)= 2.85865076268776E-08
            'sal_g_si(0,0,2,0.035,300,1E6)= 4.09543164904626E-14

            Dim g As Double, sqr_s As Double
            Dim gi(maxs) As Double
            Dim i As Integer

            sal_g_si = ErrorReturn

            If drv_s < 0 Or drv_s > 3 Then Exit Function
            If drv_t < 0 Then Exit Function
            If drv_p < 0 Then Exit Function

            If check_limits <> 1 Then
                If sa_si < 0 Or sa_si >= 1 Then Exit Function
                If t_si <= 0 Then Exit Function
                If p_si <= 0 Then Exit Function
            Else
                'SAL_LIMITS
                'If t_si < sal_tmin Or t_si > sal_tmax Or _
                '    sa_si < sal_smin Or sa_si > sal_smax Or _
                '    p_si < sal_pmin Or p_si > sal_pmax Then Exit Function
            End If

            'compute the limit S = 0:
            If sa_si = 0 Then

                If drv_s = 0 Then         'if no S-derivative taken, each term of gS vanishes
                    sal_g_si = 0
                    Exit Function
                End If

                If (drv_p > 0 Or drv_t > 1) And drv_s = 1 Then          'if the log term is gone, the 1st S-derivative
                    sal_g_si = sal_g_term_si(2, drv_t, drv_p, t_si, p_si) 'returns the linear S term
                    Exit Function
                End If

                Exit Function            'all other cases have a singularity at S = 0
            End If


            'compute the cases S > 0:
            For i = 1 To maxs
                gi(i) = sal_g_term_si(i, drv_t, drv_p, t_si, p_si)  'get all coefficients of the salinity expansion
                If gi(i) = ErrorReturn Then Exit Function
            Next i

            g = 0
            If gi(1) <> 0 Then    'take the required S-derivative of the log term
                Select Case drv_s
                    Case 0 : g = gi(1) * sa_si * Log(sa_si)
                    Case 1 : g = gi(1) * (Log(sa_si) + 1)
                    Case 2 : g = gi(1) / sa_si
                    Case 3 : g = -gi(1) / sa_si ^ 2
                End Select
            End If

            sqr_s = Sqrt(sa_si)

            For i = 2 To maxs    'add the S-derivatives of each root(S) term
                If gi(i) <> 0 Then
                    Select Case drv_s
                        Case 0 : g = g + gi(i) * sqr_s ^ i
                        Case 1 : g = g + gi(i) * (i / 2) * sqr_s ^ (i - 2)
                        Case 2 : g = g + gi(i) * (i / 2) * (i / 2 - 1) * sqr_s ^ (i - 4)
                        Case 3 : g = g + gi(i) * (i / 2) * (i / 2 - 1) * (i / 2 - 2) * sqr_s ^ (i - 6)
                    End Select
                End If
            Next i

            sal_g_si = g

        End Function


        Public Function sal_act_coeff_si(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns the mean activity coefficient, ln(gamma), of seawater

            'sa_si:   absolute salinity in kg/kg
            't_si:    absolute temperature in K
            'p_si:    absolute pressure in Pa

            'check value: sal_act_coeff_si(0.035,300,1E6) = -0.527003008913238

            Dim lng As Double, sqr_s As Double
            Dim gi(maxs) As Double
            Dim i As Integer

            sal_act_coeff_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            If sa_si = 0 Then
                sal_act_coeff_si = 0
                Exit Function
            End If

            'compute the cases S > 0:
            For i = 1 To maxs
                If i <> 2 Then
                    gi(i) = sal_g_term_si(i, 0, 0, t_si, p_si)  'get all coefficients of the salinity expansion
                    If gi(i) = ErrorReturn Then Exit Function
                End If
            Next i

            sqr_s = Sqrt(sa_si)
            lng = 0

            For i = 3 To maxs
                lng = lng + gi(i) * (sa_si * (1.0# - 0.5 * i) + 0.5 * i) * sqr_s ^ (i - 2)
            Next i

            sal_act_coeff_si = lng / gi(1) + Log(1.0# - sa_si) - sa_si

        End Function


        Public Function sal_act_potential_si(ByVal sa_si As Double, _
                                            ByVal t_si As Double, _
                                            ByVal p_si As Double) As Double

            'returns the activity potential, psi, of seawater

            'sa_si:   absolute salinity in kg/kg
            't_si:    absolute temperature in K
            'p_si:    absolute pressure in Pa

            'check value: sal_act_potential_si(0.035,300,1E6) = -0.429940465498403

            Dim psi As Double, sqr_s As Double
            Dim gi(maxs) As Double
            Dim i As Integer

            sal_act_potential_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            If sa_si = 0 Then
                sal_act_potential_si = 0
                Exit Function
            End If

            'compute the cases S > 0:
            For i = 1 To maxs
                If i <> 2 Then
                    gi(i) = sal_g_term_si(i, 0, 0, t_si, p_si)  'get all coefficients of the salinity expansion
                    If gi(i) = ErrorReturn Then Exit Function
                End If
            Next i

            sqr_s = Sqrt(sa_si)
            psi = 0

            For i = 3 To maxs
                psi = psi + gi(i) * sqr_s ^ (i - 2)
            Next i

            sal_act_potential_si = psi / gi(1) + Log(1.0# - sa_si)

        End Function


        Public Function sal_activity_w_si(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns the activity of water in seawater, a_w
            'for seawater with Reference Composition, see Millero et al. Deep-Sea Research I, 55(2008) 50-72

            'sa_si:   absolute salinity in kg/kg
            't_si:    absolute temperature in K
            'p_si:    absolute pressure in Pa

            'check value: sal_activity_w_si(0.035,300,1E6) = 0.981388410188134

            Dim phi As Double

            If sa_si = 0 Then
                sal_activity_w_si = 1.0#
                Exit Function
            End If

            phi = sal_osm_coeff_si(sa_si, t_si, p_si)

            If phi = ErrorReturn Then
                sal_activity_w_si = ErrorReturn
                Exit Function
            End If

            sal_activity_w_si = Exp(-phi * sa_si / (1.0# - sa_si) * Molar_mass_H2O_si / Molar_mass_seasalt_si)

        End Function


        Public Function sal_dilution_si(ByVal sa_si As Double, _
                                    ByVal t_si As Double, _
                                    ByVal p_si As Double) As Double

            'returns the dilution coefficient of seawater, D = S * (d2g/dS2)_T_P, in J/kg

            'sa_si:   absolute salinity in kg/kg
            't_si:    absolute temperature in K
            'p_si:    absolute pressure in Pa

            'check value: sal_dilution_si(0.035,300,1E6) = 78687.0191308744

            Dim d As Double, sqr_s As Double
            Dim gi(maxs) As Double
            Dim i As Integer

            sal_dilution_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            If sa_si = 0 Then
                sal_dilution_si = sal_g_term_si(1, 0, 0, t_si, p_si)
            End If

            For i = 1 To maxs
                If i <> 2 Then
                    gi(i) = sal_g_term_si(i, 0, 0, t_si, p_si)  'get all coefficients of the salinity expansion
                    If gi(i) = ErrorReturn Then Exit Function
                End If
            Next i

            sqr_s = Sqrt(sa_si)
            d = gi(1)

            For i = 3 To maxs
                d = d + gi(i) * 0.25 * i * (i - 2) * sqr_s ^ (i - 2)
            Next i

            sal_dilution_si = d

        End Function


        Public Function sal_mixenthalpy_si(ByVal sa1_si As Double, _
                                        ByVal sa2_si As Double, _
                                        ByVal w1 As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns the mixing enthalpy of seawater, h_mix, in J/kg

            'sa1_si:  salinity of component 1, in kg/kg
            'sa2_si:  salinity of component 2, in kg/kg
            'w1:      mass fraction of component 1, w1 = m1 / (m1 + m2)
            't_si:    absolute temperature in K
            'p_si:    absolute pressure in Pa

            'check value: sal_mixenthalpy_si(0.01,0.035,0.6,300,1E6) = 16.1539911284051

            Dim h As Double, h1 As Double, h2 As Double
            Dim S As Double, s1 As Double, s2 As Double
            Dim s12 As Double, w2 As Double

            sal_mixenthalpy_si = ErrorReturn

            If sa1_si < 0 Or sa1_si >= 1 Then Exit Function
            If sa2_si < 0 Or sa2_si >= 1 Then Exit Function
            If w1 < 0 Or w1 > 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            w2 = 1 - w1
            s12 = w1 * sa1_si + w2 * sa2_si
            h = sal_g_si(0, 0, 0, s12, t_si, p_si)
            If h = ErrorReturn Then Exit Function
            S = -sal_g_si(0, 1, 0, s12, t_si, p_si)
            If S = ErrorReturn Then Exit Function
            h = h + t_si * S

            h1 = sal_g_si(0, 0, 0, sa1_si, t_si, p_si)
            If h1 = ErrorReturn Then Exit Function
            s1 = -sal_g_si(0, 1, 0, sa1_si, t_si, p_si)
            If s1 = ErrorReturn Then Exit Function
            h1 = h1 + t_si * s1

            h2 = sal_g_si(0, 0, 0, sa2_si, t_si, p_si)
            If h2 = ErrorReturn Then Exit Function
            s2 = -sal_g_si(0, 1, 0, sa2_si, t_si, p_si)
            If s2 = ErrorReturn Then Exit Function
            h2 = h2 + t_si * s2

            sal_mixenthalpy_si = h - w1 * h1 - w2 * h2

        End Function


        Public Function sal_mixentropy_si(ByVal sa1_si As Double, _
                                        ByVal sa2_si As Double, _
                                        ByVal w1 As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns the mixing entropy of seawater, eta_mix, in J/(kg K)

            'sa1_si:  salinity of component 1, in kg/kg
            'sa2_si:  salinity of component 2, in kg/kg
            'w1:      mass fraction of component 1, w1 = m1 / (m1 + m2)
            't_si:    absolute temperature in K
            'p_si:    absolute pressure in Pa

            'check value: sal_mixentropy_si(0.01,0.035,0.6,300,1E6) = 0.96682942261|6511

            Dim S As Double, s1 As Double, s2 As Double
            Dim s12 As Double, w2 As Double

            sal_mixentropy_si = ErrorReturn

            If sa1_si < 0 Or sa1_si >= 1 Then Exit Function
            If sa2_si < 0 Or sa2_si >= 1 Then Exit Function
            If w1 < 0 Or w1 > 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            w2 = 1 - w1
            s12 = w1 * sa1_si + w2 * sa2_si
            S = -sal_g_si(0, 1, 0, s12, t_si, p_si)
            If S = ErrorReturn Then Exit Function

            s1 = -sal_g_si(0, 1, 0, sa1_si, t_si, p_si)
            If s1 = ErrorReturn Then Exit Function

            s2 = -sal_g_si(0, 1, 0, sa2_si, t_si, p_si)
            If s2 = ErrorReturn Then Exit Function

            sal_mixentropy_si = S - w1 * s1 - w2 * s2

        End Function


        Public Function sal_mixvolume_si(ByVal sa1_si As Double, _
                                        ByVal sa2_si As Double, _
                                        ByVal w1 As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns the mixing volume of seawater, v_mix, in m^3/kg

            'sa1_si:  salinity of component 1, in kg/kg
            'sa2_si:  salinity of component 2, in kg/kg
            'w1:      mass fraction of component 1, w1 = m1 / (m1 + m2)
            't_si:    absolute temperature in K
            'p_si:    absolute pressure in Pa

            'check value: sal_mixvolume_si(0.01,0.035,0.6,300,1E6) = -5.94174956891818E-08

            Dim v As Double, v1 As Double, v2 As Double
            Dim s12 As Double, w2 As Double

            sal_mixvolume_si = ErrorReturn

            If sa1_si < 0 Or sa1_si >= 1 Then Exit Function
            If sa2_si < 0 Or sa2_si >= 1 Then Exit Function
            If w1 < 0 Or w1 > 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            w2 = 1 - w1
            s12 = w1 * sa1_si + w2 * sa2_si
            v = sal_g_si(0, 0, 1, s12, t_si, p_si)
            If v = ErrorReturn Then Exit Function

            v1 = sal_g_si(0, 0, 1, sa1_si, t_si, p_si)
            If v1 = ErrorReturn Then Exit Function

            v2 = sal_g_si(0, 0, 1, sa2_si, t_si, p_si)
            If v2 = ErrorReturn Then Exit Function

            sal_mixvolume_si = v - w1 * v1 - w2 * v2

        End Function


        Public Function sal_osm_coeff_si(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns the osmotic coefficient of seawater, phi

            'sa_si:   absolute salinity in kg/kg
            't_si:    absolute temperature in K
            'p_si:    absolute pressure in Pa

            'check value: sal_osm_coeff_si(0.035,300,1E6) = 0.902937456585165

            Dim phi As Double, sqr_s As Double
            Dim gi(maxs) As Double
            Dim i As Integer

            sal_osm_coeff_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            If sa_si = 0 Then
                sal_osm_coeff_si = 1.0#
                Exit Function
            End If

            'compute the cases S > 0:
            For i = 1 To maxs
                If i <> 2 Then
                    gi(i) = sal_g_term_si(i, 0, 0, t_si, p_si)  'get all coefficients of the salinity expansion
                    If gi(i) = ErrorReturn Then Exit Function
                End If
            Next i

            sqr_s = Sqrt(sa_si)
            phi = 0

            For i = 3 To maxs
                phi = phi + gi(i) * (0.5 * i - 1.0#) * sqr_s ^ (i - 2)
            Next i

            sal_osm_coeff_si = phi * (1.0# - sa_si) / gi(1) + 1.0# - sa_si

        End Function


        Public Function sal_chempot_h2o_si(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns the saline part of chemical potential of water in seawater, µ_WS, in J/kg

            'sa_si:   absolute salinity in kg/kg
            't_si:    absolute temperature in K
            'p_si:    absolute pressure in Pa

            'check value: sal_chempot_h2o_si(0.035,300,1E6) = -2601.18871107196

            Dim g As Double, sqr_s As Double
            Dim gi(maxs) As Double
            Dim i As Integer

            sal_chempot_h2o_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            For i = 1 To maxs
                If i <> 2 Then
                    gi(i) = sal_g_term_si(i, 0, 0, t_si, p_si)  'get all coefficients of the salinity expansion
                    If gi(i) = ErrorReturn Then Exit Function
                End If
            Next i

            sqr_s = Sqrt(sa_si)
            g = gi(1)

            For i = 3 To maxs
                g = g + gi(i) * (0.5 * i - 1.0#) * sqr_s ^ (i - 2)
            Next i

            sal_chempot_h2o_si = -g * sa_si

        End Function


        Public Function sal_chempot_rel_si(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns the relative chemical potential of seawater, µ, in J/kg

            'sa_si:   absolute salinity in kg/kg
            't_si:    absolute temperature in K
            'p_si:    absolute pressure in Pa

            'check value: sal_chempot_rel_si(0.035,300,1E6) = 77949.2100394602

            sal_chempot_rel_si = ErrorReturn

            If sa_si <= 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            sal_chempot_rel_si = sal_g_si(1, 0, 0, sa_si, t_si, p_si)

        End Function


        Public Function sal_chem_coeff_si(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns the chemical coefficient of seawater, DS, in J/kg

            'sa_si:   absolute salinity in kg/kg
            't_si:    absolute temperature in K
            'p_si:    absolute pressure in Pa

            'check value: sal_chem_coeff_si(0.035,300,1E6) = 2754.04566958061

            Dim d As Double

            sal_chem_coeff_si = ErrorReturn

            d = sal_dilution_si(sa_si, t_si, p_si)
            If d = ErrorReturn Then Exit Function

            sal_chem_coeff_si = sa_si * d

        End Function


        Public Function sal_saltenthalpy_si(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns the specific enthalpy of seasalt in seawater, h_S, in J/kg

            'sa_si:   absolute salinity in kg/kg
            't_si:    absolute temperature in K
            'p_si:    absolute pressure in Pa

            'check value: sal_saltenthalpy_si(0.035,300,1E6) = -156107.95919621

            Dim h As Double, sqr_s As Double
            Dim gi(maxs) As Double
            Dim gti(maxs) As Double
            Dim i As Integer

            sal_saltenthalpy_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            For i = 2 To maxs
                gi(i) = sal_g_term_si(i, 0, 0, t_si, p_si)  'get all coefficients of the salinity expansion
                If gi(i) = ErrorReturn Then Exit Function
                gti(i) = sal_g_term_si(i, 1, 0, t_si, p_si)  'get all t-derivatives of the salinity expansion
                If gti(i) = ErrorReturn Then Exit Function
            Next i

            sqr_s = Sqrt(sa_si)
            h = 0

            For i = 2 To maxs
                h = h + (gi(i) - t_si * gti(i)) * sqr_s ^ (i - 2)
            Next i

            sal_saltenthalpy_si = h

        End Function


        Public Function sal_saltentropy_si(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns the specific entropy of seasalt in seawater, eta_S, in J/(kg K)

            'sa_si:   absolute salinity in kg/kg
            't_si:    absolute temperature in K
            'p_si:    absolute pressure in Pa

            'check value: sal_saltentropy_si(0.035,300,1E6) = -532.458305921571

            Dim eta As Double, sqr_s As Double
            Dim gi(maxs) As Double
            Dim i As Integer

            sal_saltentropy_si = ErrorReturn

            If sa_si <= 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            For i = 1 To maxs
                gi(i) = sal_g_term_si(i, 1, 0, t_si, p_si)  'get all coefficients of the salinity expansion
                If gi(i) = ErrorReturn Then Exit Function
            Next i

            sqr_s = Sqrt(sa_si)
            eta = -gi(1) * Log(sa_si)

            For i = 2 To maxs
                eta = eta - gi(i) * sqr_s ^ (i - 2)
            Next i

            sal_saltentropy_si = eta

        End Function


        Public Function sal_saltvolume_si(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns the specific volume of seasalt in seawater, v_S, in m^3/kg

            'sa_si:   absolute salinity in kg/kg
            't_si:    absolute temperature in K
            'p_si:    absolute pressure in Pa

            'check value: sal_saltvolume_si(0.035,300,1E6) = -7.30285943767952E-04

            Dim v As Double, sqr_s As Double
            Dim gi(maxs) As Double
            Dim i As Integer

            sal_saltvolume_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            If sa_si = 0 Then
                sal_saltvolume_si = sal_g_term_si(2, 0, 1, t_si, p_si)
                Exit Function
            End If

            For i = 2 To maxs
                gi(i) = sal_g_term_si(i, 0, 1, t_si, p_si)  'get all coefficients of the salinity expansion
                If gi(i) = ErrorReturn Then Exit Function
            Next i

            sqr_s = Sqrt(sa_si)
            v = gi(2)

            For i = 3 To maxs
                v = v + gi(i) * sqr_s ^ (i - 2)
            Next i

            sal_saltvolume_si = v

        End Function


        Public Function sal_molality_si(ByVal sa_si As Double) As Double

            'function returns the molality of seawater in mol/kg as a function of absolute salinity in kg/kg
            'for seawater with Reference Composition, see Millero et al. Deep-Sea Research I, 55(2008) 50-72

            'check value: sal_molality_si(0.035) = 1.15493681892608

            sal_molality_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function

            sal_molality_si = sa_si / ((1 - sa_si) * Molar_mass_seasalt_si)

        End Function




#End Region

#Region "Sea_3a"

        '#########################################################################

        'This module requires the library modules:
        '     Constants_0_Mdl, file Constants_0.bas
        '     Maths_0_Mdl,     file Maths_0.bas
        '     Sal_1_Mdl,       file Sal_1.bas
        '     Sal_2_Mdl,       file Sal_2.bas
        '     Flu_3a_Mdl,      file Flu_3a.bas

        '#########################################################################


        'This module implements the Gibbs function of seawater depending on temperature
        'and pressure, as well as their partial derivatives,
        'as defined in IAPWS-08:

        'Release on the IAPWS Formulation 2008 for the Thermodynamic Properties of Seawater
        'The International Association for the Properties of Water and Steam
        'Berlin, Germany, September 2008

        'Implementation in VB6 by Rainer Feistel
        'for publication in Ocean Science, as described in the papers

        'Feistel, R., Wright, D.G., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part I: Background and Equations.
        'Ocean Science, 2009

        'Wright, D.G., Feistel, R., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part II: The Library Routines,
        'Ocean Science, 2009

        Public Function sea_g_si(ByVal drv_s As Integer, _
                                ByVal drv_t As Integer, _
                                ByVal drv_p As Integer, _
                                ByVal sa_si As Double, _
                                ByVal t_si As Double, _
                                ByVal p_si As Double) As Double

            'this function implements the Gibbs function of seawater, IAPWS-08

            'returns sea_g_si as the S-T-P derivative

            '(d/dS)^drv_s (d/dT)^drv_t (d/dP)^drv_p g(S,T,P)

            'of the specific Gibbs energy of seawater, g(S,T,P), in J/kg

            'sa_si     absolute salinity in kg/kg
            't_si      absolute temperature in K
            'p_si      absolute pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a

            'Check values with default settings:
            'sea_g_si( 0, 0, 0, 0.035, 300, 1E5) =-5114.9961985|6615
            'sea_g_si( 1, 0, 0, 0.035, 300, 1E5) = 78593.7757371468
            'sea_g_si( 0, 1, 0, 0.035, 300, 1E5) =-374.452240540063
            'sea_g_si( 0, 0, 1, 0.035, 300, 1E5) = 9.7785861518227E-04
            'sea_g_si( 2, 0, 0, 0.035, 300, 1E5) = 2247550.41118143
            'sea_g_si( 1, 1, 0, 0.035, 300, 1E5) = 789.934255688123
            'sea_g_si( 1, 0, 1, 0.035, 300, 1E5) =-7.16682401264827E-04
            'sea_g_si( 0, 2, 0, 0.035, 300, 1E5) =-13.3358324654699
            'sea_g_si( 0, 1, 1, 0.035, 300, 1E5) = 3.04605539768293E-07
            'sea_g_si( 0, 0, 2, 0.035, 300, 1E5) =-4.10945807960411E-13

            Dim gw As Double, gs As Double

            sea_g_si = ErrorReturn

            'If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            If drv_s < 0 Or drv_s > 3 Then Exit Function
            If drv_t < 0 Then Exit Function
            If drv_p < 0 Then Exit Function

            'water part
            gw = 0
            If drv_s = 0 Then
                gw = liq_g_si(drv_t, drv_p, t_si, p_si)
                If gw = ErrorReturn Then Exit Function
            End If

            'saline part
            gs = sal_g_si(drv_s, drv_t, drv_p, sa_si, t_si, p_si)
            If gs = ErrorReturn Then Exit Function

            sea_g_si = gw + gs

        End Function


        Public Function sea_chempot_h2o_si(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns   µ_W(S,T,P) = g - S*(dg/dS)_T_P  chem. potential of water in seawater in J/kg,
            'sa_si     absolute salinity in kg/kg
            't_si      absolute temperature in K
            'p_si      absolute pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a

            'Check value with default settings: sea_chempot_h2o_si(0.035, 300, 1E5) = -7865.7783493|6628

            Dim g As Double, mu_s As Double

            sea_chempot_h2o_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            g = liq_g_si(0, 0, t_si, p_si)
            If g = ErrorReturn Then Exit Function

            mu_s = sal_chempot_h2o_si(sa_si, t_si, p_si)
            If mu_s = ErrorReturn Then Exit Function

            sea_chempot_h2o_si = g + mu_s

        End Function


        Public Function sea_chempot_rel_si(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns   µ(S,T,P) = (dg/dS)_T_P  relative chem. potential of seawater in J/kg,
            'sa_si     absolute salinity in kg/kg
            't_si      absolute temperature in K
            'p_si      absolute pressure in Pa

            'Check value with default settings:
            'sea_chempot_rel_si(0.035, 300, 1E5) = 78593.7757371468

            sea_chempot_rel_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            sea_chempot_rel_si = sal_chempot_rel_si(sa_si, t_si, p_si)

        End Function


        Public Function sea_g_contraction_t_si(ByVal sa_si As Double, _
                                            ByVal t_si As Double, _
                                            ByVal p_si As Double) As Double

            'returns   beta(S,T,P) = (1/D)*(dD/dS)_T_P isothermal haline contraction of seawater in kg/kg,
            'sa_si     absolute salinity in kg/kg
            't_si      absolute temperature in K
            'p_si      absolute pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a

            'Check value with default settings: sea_g_contraction_t_si(0.035, 300, 1E5) = 0.732910044599075


            Dim gp As Double, gsp As Double

            sea_g_contraction_t_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            gp = sea_g_si(0, 0, 1, sa_si, t_si, p_si)
            If gp = ErrorReturn Then Exit Function
            If gp <= 0 Then Exit Function

            gsp = sea_g_si(1, 0, 1, sa_si, t_si, p_si)
            If gsp = ErrorReturn Then Exit Function

            sea_g_contraction_t_si = -gsp / gp

        End Function


        Public Function sea_cp_si(ByVal sa_si As Double, _
                                ByVal t_si As Double, _
                                ByVal p_si As Double) As Double

            'returns   cp(S,T,P) = T * (deta/dT)_P  isobaric heat capacity of seawater in J/(kg K),
            'sa_si     absolute salinity in kg/kg
            't_si      absolute temperature in K
            'p_si      absolute pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a

            'Check value with default settings: sea_cp_si(0.035, 300, 1E5) = 4000.74973964098

            Dim gtt As Double

            sea_cp_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            gtt = sea_g_si(0, 2, 0, sa_si, t_si, p_si)
            If gtt = ErrorReturn Then Exit Function

            sea_cp_si = -t_si * gtt

        End Function


        Public Function sea_density_si(ByVal sa_si As Double, _
                                    ByVal t_si As Double, _
                                    ByVal p_si As Double) As Double

            'returns   d(S,T,P) = 1/(dg/dP)_S_T  density of seawater in kg/m^3
            'sa_si     absolute salinity in kg/kg
            't_si      absolute temperature in K
            'p_si      absolute pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a

            'Check value with default settings: sea_density_si(0.035, 300, 1E5) = 1022.6427261303

            Dim gp As Double

            sea_density_si = ErrorReturn

            'If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            gp = sea_g_si(0, 0, 1, sa_si, t_si, p_si)
            If gp = ErrorReturn Then Exit Function
            If gp <= 0 Then Exit Function

            sea_density_si = 1.0# / gp

        End Function


        Public Function sea_enthalpy_si(ByVal sa_si As Double, _
                                    ByVal t_si As Double, _
                                    ByVal p_si As Double) As Double

            'returns   h(S,T,P) = g - T*(dg/dT)_S_P enthalpy of seawater in J/kg,
            'sa_si     absolute salinity in kg/kg
            't_si      absolute temperature in K,
            'p_si      absolute pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a

            'Check value with default settings: sea_enthalpy_si(0.035, 300, 1E5) = 107220.675963453

            Dim g As Double, gt As Double

            sea_enthalpy_si = ErrorReturn

            'If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            g = sea_g_si(0, 0, 0, sa_si, t_si, p_si)
            If g = ErrorReturn Then Exit Function

            gt = sea_g_si(0, 1, 0, sa_si, t_si, p_si)
            If gt = ErrorReturn Then Exit Function

            sea_enthalpy_si = g - t_si * gt

        End Function


        Public Function sea_entropy_si(ByVal sa_si As Double, _
                                    ByVal t_si As Double, _
                                    ByVal p_si As Double) As Double

            'returns   eta(S,T,P) = -(dg/dT)_S_P  specific entropy of seawater in J/(kg K),
            'sa_si     absolute salinity in kg/kg
            't_si      absolute temperature in K
            'p_si      absolute pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a

            'Check value with default settings: sea_entropy_si(0.035, 300, 1E5) = 374.452240540063

            Dim gt As Double

            sea_entropy_si = ErrorReturn

            'If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            gt = sea_g_si(0, 1, 0, sa_si, t_si, p_si)
            If gt = ErrorReturn Then Exit Function

            sea_entropy_si = -gt

        End Function


        Public Function sea_g_expansion_t_si(ByVal sa_si As Double, _
                                            ByVal t_si As Double, _
                                            ByVal p_si As Double) As Double

            'returns   alpha(S,T,P) = -(1/D) * (dD/dT)_S_P  thermal expansion of seawater in 1/K,
            'sa_si     absolute salinity in kg/kg
            't_si      absolute temperature in K
            'p_si      absolute pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a

            'Check value with default settings: sea_g_expansion_t_si(0.035, 300, 1E5) = 3.11502639583039E-04

            Dim gtp As Double, gp As Double

            sea_g_expansion_t_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            gtp = sea_g_si(0, 1, 1, sa_si, t_si, p_si)
            If gtp = ErrorReturn Then Exit Function

            gp = sea_g_si(0, 0, 1, sa_si, t_si, p_si)
            If gp = ErrorReturn Then Exit Function
            If gp <= 0 Then Exit Function

            sea_g_expansion_t_si = gtp / gp

        End Function


        Public Function sea_gibbs_energy_si(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns   g(S,T,P) = g   Gibbs energy of seawater in J/kg,
            'sa_si     absolute salinity in kg/kg
            't_si      absolute temperature in K,
            'p_si      absolute pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a

            'Check value with default settings: sea_gibbs_energy_si(0.035, 300, 1E5) = -5114.9961985|6615

            sea_gibbs_energy_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            sea_gibbs_energy_si = sea_g_si(0, 0, 0, sa_si, t_si, p_si)

        End Function


        Public Function sea_internal_energy_si(ByVal sa_si As Double, _
                                            ByVal t_si As Double, _
                                            ByVal p_si As Double) As Double

            'returns   u(S,T,P) = g - T*(dg/dT)_S_P - P*(dg/dP)_S_T internal energy of seawater in J/kg,
            'sa_si     absolute salinity in kg/kg
            't_si      absolute temperature in K
            'p_si      absolute pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a

            'Check value with default settings: sea_internal_energy_si(0.035, 300, 1E5) = 107122.890101934

            Dim g As Double, gt As Double, gp As Double

            sea_internal_energy_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            g = sea_g_si(0, 0, 0, sa_si, t_si, p_si)
            If g = ErrorReturn Then Exit Function

            gt = sea_g_si(0, 1, 0, sa_si, t_si, p_si)
            If gt = ErrorReturn Then Exit Function

            gp = sea_g_si(0, 0, 1, sa_si, t_si, p_si)
            If gp = ErrorReturn Then Exit Function
            If gp <= 0 Then Exit Function

            sea_internal_energy_si = g - t_si * gt - p_si * gp

        End Function


        Public Function sea_kappa_s_si(ByVal sa_si As Double, _
                                    ByVal t_si As Double, _
                                    ByVal p_si As Double) As Double

            'returns   kappa_s(S,T,P) = (1/D) * (dD/dP)_S_eta  isentropic compressibility of seawater in 1/Pa,
            'sa_si     absolute salinity in kg/kg
            't_si      absolute temperature in K
            'p_si      absolute pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a

            'Check value with default settings: sea_kappa_s_si(0.035, 300, 1E5) = 4.13135667732431E-10

            Dim gp As Double
            Dim gtp As Double, gtt As Double, gpp As Double

            sea_kappa_s_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            gp = sea_g_si(0, 0, 1, sa_si, t_si, p_si)
            If gp = ErrorReturn Then Exit Function
            If gp <= 0 Then Exit Function

            gtp = sea_g_si(0, 1, 1, sa_si, t_si, p_si)
            If gtp = ErrorReturn Then Exit Function

            gtt = sea_g_si(0, 2, 0, sa_si, t_si, p_si)
            If gtt = ErrorReturn Then Exit Function
            If gtt = 0 Then Exit Function

            gpp = sea_g_si(0, 0, 2, sa_si, t_si, p_si)
            If gpp = ErrorReturn Then Exit Function

            sea_kappa_s_si = (gtp * gtp - gtt * gpp) / (gp * gtt)

        End Function


        Public Function sea_kappa_t_si(ByVal sa_si As Double, _
                                    ByVal t_si As Double, _
                                    ByVal p_si As Double) As Double

            'returns   kappa_t(S,T,P) = (1/D) * (dD/dP)_S_T  isothermal compressibility of seawater in 1/K,
            'sa_si     absolute salinity in kg/kg
            't_si      absolute temperature in K
            'p_si      absolute pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a

            'Check value with default settings: sea_kappa_t_si(0.035, 300, 1E5) = 4.20250741344455E-10

            Dim gp As Double, gpp As Double

            sea_kappa_t_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            gp = sea_g_si(0, 0, 1, sa_si, t_si, p_si)
            If gp = ErrorReturn Then Exit Function
            If gp <= 0 Then Exit Function

            gpp = sea_g_si(0, 0, 2, sa_si, t_si, p_si)
            If gpp = ErrorReturn Then Exit Function

            sea_kappa_t_si = -gpp / gp

        End Function


        Public Function sea_lapserate_si(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns   gamma(S,T,P) = (dT/dP)_S_eta  adiabatic lapse rate of seawater in K/Pa,
            'sa_si     absolute salinity in kg/kg
            't_si      absolute temperature in K
            'p_si      absolute pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a

            'Check value with default settings: sea_lapserate_si(0.035, 300, 1E5) = 2.28411342566727E-08

            Dim gtp As Double, gtt As Double

            sea_lapserate_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            gtp = sea_g_si(0, 1, 1, sa_si, t_si, p_si)
            If gtp = ErrorReturn Then Exit Function

            gtt = sea_g_si(0, 2, 0, sa_si, t_si, p_si)
            If gtt = ErrorReturn Then Exit Function
            If gtt = 0 Then Exit Function

            sea_lapserate_si = -gtp / gtt

        End Function


        Public Function sea_osm_coeff_si(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns the osmotic coefficient of seawater, phi

            'sa_si:   absolute salinity in kg/kg
            't_si:    absolute temperature in K
            'p_si:    absolute pressure in Pa

            'Check value with default settings: sea_osm_coeff_si(0.035, 300, 1E5) = 0.902777495349254

            sea_osm_coeff_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            sea_osm_coeff_si = sal_osm_coeff_si(sa_si, t_si, p_si)

        End Function


        Public Function sea_soundspeed_si(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns   c(S,T,P) = sqr[ (dP/dD)_S_eta ] sound speed of seawater in m/s,
            'sa_si     absolute salinity in kg/kg
            't_si      absolute temperature in K
            'p_si      absolute pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a

            'Check value with default settings: sea_soundspeed_si(0.035, 300, 1E5) = 1538.47940766397

            Dim gp As Double, c As Double
            Dim gtp As Double, gtt As Double, gpp As Double

            sea_soundspeed_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function

            gp = sea_g_si(0, 0, 1, sa_si, t_si, p_si)
            If gp = ErrorReturn Then Exit Function
            If gp <= 0 Then Exit Function

            gtp = sea_g_si(0, 1, 1, sa_si, t_si, p_si)
            If gtp = ErrorReturn Then Exit Function

            gtt = sea_g_si(0, 2, 0, sa_si, t_si, p_si)
            If gtt = ErrorReturn Then Exit Function
            If gtt = 0 Then Exit Function

            gpp = sea_g_si(0, 0, 2, sa_si, t_si, p_si)
            If gpp = ErrorReturn Then Exit Function

            c = gtp * gtp - gtt * gpp
            If c = 0 Then Exit Function
            c = gtt / c
            If c < 0 Then Exit Function

            sea_soundspeed_si = gp * Sqrt(c)

        End Function


        Public Function sea_temp_maxdensity_si(ByVal sa_si As Double, _
                                            ByVal p_si As Double) As Double

            'returns   t_md(S,P) temperature of maximum density in K, by Newton iteration of d2g/dtdp = 0
            'input:
            'sa_si     absolute salinity in kg/kg
            'p_si      absolute pressure in Pa

            'Check value with default settings:
            'sea_temp_maxdensity_si(0.01, 1E5) = 274.950121503097

            Const h = 0.000001   'finite difference in t to estimate d3g/dt2dp
            Const T0 = 300.0#      'initial temperature value
            Const eps = 0.000001 'temperature tolerance
            Const maxit = 20&    'max iteration steps

            Dim g_tp As Double, g_ttp As Double, t As Double, dt As Double
            Dim iter As Long

            sea_temp_maxdensity_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function

            t = T0
            For iter = 1 To maxit

                g_tp = sea_g_si(0, 1, 1, sa_si, t, p_si)
                If g_tp = ErrorReturn Then Exit Function

                g_ttp = sea_g_si(0, 1, 1, sa_si, t + h, p_si)
                If g_ttp = ErrorReturn Then Exit Function
                g_ttp = (g_ttp - g_tp) / h                    'finite difference approximation for 3rd derivative
                If g_ttp = 0 Then Exit Function

                'improve estimate
                dt = -g_tp / g_ttp
                t = t + dt
                If t <= 0 Then Exit Function

                If Abs(dt) < eps Then
                    sea_temp_maxdensity_si = t
                    Exit Function
                End If

            Next iter

        End Function


        Public Function chk_IAPWS08_Table8a() As String

            'TABLE 8  Numerical check values for the water part computed from
            'gW and its derivatives, Table 4, for the saline part computed from
            'gS and its derivatives, Eq. (4), and for the seawater property
            'computed from the Gibbs function g = gW + gS and its derivatives, Eq. (3).
            'The numerical functions evaluated here at given points (S, T, p) are defined in Tables 3-6.

            'a)  Properties at S = Sn = 0.035 165 04 kg kg-1,  T = T0 = 273.15 K, p = p0 = 101325 Pa

            'Quantity         Water part       Saline part      Seawater        Unit
            'g                0.101342742E3   -0.101342742E3    0.0             J kg-1
            '(dg/dS)_T,p      0.0              0.639974067E5    0.639974067E5   J kg-1
            '(dg/dT)_S,p      0.147643376     -0.147643376      0.0             J kg-1 K-1
            '(dg/dp)-S,T      0.100015694E-2  -0.274957224E-4   0.972661217E-3  m3 kg-1
            '(d2g/dSdp)-T     0.0             -0.759615412E-3  -0.759615412E-3  m3 kg-1
            '(d2g/dT 2)_S,p  -0.154473542E2    0.852861151     -0.145944931E2   J kg-1 K-2
            '(d2g/dTdp)_S    -0.677700318E-7   0.119286787E-6   0.515167556E-7  m3 kg-1 K-1
            '(d2g/dp2)_S,T   -0.508928895E-12  0.581535172E-13 -0.450775377E-12 m3 kg-1 Pa-1
            'h                0.610139535E2   -0.610139535E2    0.0             J kg-1
            'f                0.18399E-2      -0.985567377E2   -0.985548978E2   J kg-1
            'u               -0.403269484E2   -0.582279494E2   -0.985548978E2   J kg-1
            's               -0.147643376      0.147643376      0.0             J kg-1 K-1
            'rho              0.99984386E3     -                0.102810720E4   kg m - 3
            'cp               0.421944481E4   -0.232959023E3    0.398648579E4   J kg-1 K-1
            'w                0.140238253E4    -                0.144900246E4   m s-1
            'µW               0.101342742E3   -0.235181411E4   -0.225047137E4   J kg-1


            Dim CRLF As String
            Dim txt As String, row As String
            Dim S As Double, t As Double, p As Double, q(3) As Double
            Dim i As Integer, j As Integer

            CRLF = Chr(13) + Chr(10)

            txt = " Implementation of IAPWS-08 in Visual Basic" + CRLF
            txt = txt + " for Publication in Ocean Science, 2009" + CRLF
            txt = txt + " R. Feistel, IOW, Version " + Version + CRLF
            txt = txt + " Compiled on " + CStr(Now) + CRLF + CRLF

            txt = txt + " Function values as given in Table 8a of IAPWS-08:" + CRLF
            txt = txt + " Properties at S = 0.03516504 kg kg-1,  T = 273.15 K, P = 101325 Pa" + CRLF + CRLF

            txt = txt + "Quantity         Water part        Saline part       Seawater        Unit" + CRLF

            t = 273.15
            p = 101325

            For i = 1 To 16
                Select Case i
                    Case 1 : row = "g                0.101342742E+3   -0.101342742E+3    0.0             J kg-1"
                    Case 2 : row = "(dg/dS)_T,P      0.0               0.639974067E+5    0.639974067E+5  J kg-1"
                    Case 3 : row = "(dg/dT)_S,P      0.147643376      -0.147643376       0.0             J kg-1 K-1"
                    Case 4 : row = "(dg/dP)_S,T      0.100015694E-2   -0.274957224E-4    0.972661217E-3  m3 kg-1"
                    Case 5 : row = "(d2g/dSdP)_T     0.0              -0.759615412E-3   -0.759615412E-3  m3 kg-1"
                    Case 6 : row = "(d2g/dT2)_S,P   -0.154473542E+2    0.852861151      -0.145944931E+2  J kg-1 K-2"
                    Case 7 : row = "(d2g/dTdP)_S    -0.677700318E-7    0.119286787E-6    0.515167556E-7  m3 kg-1 K-1"
                    Case 8 : row = "(d2g/dP2)_S,T   -0.508928895E-12   0.581535172E-13  -0.450775377E-12 m3 kg-1 Pa-1"
                    Case 9 : row = "h                0.610139535E+2   -0.610139535E+2    0.0             J kg-1"
                    Case 10 : row = "f                0.18399xxxxE-2   -0.985567377E+2   -0.985548978E+2  J kg-1"
                    Case 11 : row = "u               -0.403269484E+2   -0.582279494E+2   -0.985548978E+2  J kg-1"
                    Case 12 : row = "s               -0.147643376       0.147643376       0.0             J kg-1 K-1"
                    Case 13 : row = "rho              0.999843086E+3    -                 0.102810720E+4  kg m - 3"
                    Case 14 : row = "cp               0.421944481E+4   -0.232959023E+3    0.398648579E+4  J kg-1 K-1"
                    Case 15 : row = "w                0.140238253E+4    -                 0.144900246E+4  m s-1"
                    Case Else : row = "µW               0.101342742E+3   -0.235181411E+4   -0.225047137E+4  J kg-1"
                End Select

                txt = txt + row + CRLF
                txt = txt + "this code:      "

                For j = 1 To 3 Step 2
                    S = IIf(j = 3, 0.03516504, 0)
                    q(j) = 0
                    Select Case i
                        Case 1 : q(j) = sea_g_si(0, 0, 0, S, t, p)
                        Case 2 : If S > 0 Then q(j) = sea_g_si(1, 0, 0, S, t, p) 'no s-derivative of pure water
                        Case 3 : q(j) = sea_g_si(0, 1, 0, S, t, p)
                        Case 4 : q(j) = sea_g_si(0, 0, 1, S, t, p)
                        Case 5 : If S > 0 Then q(j) = sea_g_si(1, 0, 1, S, t, p) 'no s-derivative of pure water
                        Case 6 : q(j) = sea_g_si(0, 2, 0, S, t, p)
                        Case 7 : q(j) = sea_g_si(0, 1, 1, S, t, p)
                        Case 8 : q(j) = sea_g_si(0, 0, 2, S, t, p)

                        Case 9 : q(j) = sea_enthalpy_si(S, t, p)
                        Case 10 : q(j) = sea_g_si(0, 0, 0, S, t, p) - p * sea_g_si(0, 0, 1, S, t, p)
                        Case 11 : q(j) = sea_g_si(0, 0, 0, S, t, p) - t * sea_g_si(0, 1, 0, S, t, p) - p * sea_g_si(0, 0, 1, S, t, p)
                        Case 12 : q(j) = sea_entropy_si(S, t, p)
                        Case 13 : q(j) = sea_density_si(S, t, p)
                        Case 14 : q(j) = sea_cp_si(S, t, p)
                        Case 15 : q(j) = sea_soundspeed_si(S, t, p)
                        Case 16 : q(j) = sea_chempot_h2o_si(S, t, p)
                    End Select

                Next j
                txt = txt + Left(EFormat(q(1), 9) + Space(18), 18)
                Select Case i
                    Case 13, 15 : txt = txt + Left(" -" + Space(18), 18)  'saline part only if linear in g
                    Case Else : txt = txt + Left(EFormat(q(3) - q(1), 9) + Space(18), 18)
                End Select
                txt = txt + EFormat(q(3), 9)
                txt = txt + CRLF + CRLF

            Next i

            chk_IAPWS08_Table8a = txt

        End Function


        Public Function chk_IAPWS08_Table8b() As String

            'TABLE 8  Numerical check values for the water part computed from
            'gW and its derivatives, Table 4, for the saline part computed from
            'gS and its derivatives, Eq. (4), and for the seawater property
            'computed from the Gibbs function g = gW + gS and its derivatives, Eq. (3).
            'The numerical functions evaluated here at given points (S, T, p) are defined in Tables 3-6.

            'b)  Properties at S = 0.1 kg kg-1, T = 353 K, p = p0 = 101325 Pa.
            'This point is located in the regions (C) and (F) of Fig. 1 with restricted validity

            'Quantity  Water part  Saline part Property of seawater  Unit
            'g               -0.446114969E+5   0.150871740E+5  -0.295243229E+5  J kg-1
            '(dg/dS)T,P       0.0              0.251957276E+6   0.251957276E+6  J kg-1
            '(dg/dT)S,P      -0.107375993E+4   0.156230907E+3  -0.917529024E+3  J kg-1 K-1
            '(dg/dP)S,T       0.102892956E-2  -0.579227286E-4   0.971006828E-3  m3 kg-1
            '(d2g/dSdP)T      0.0             -0.305957802E-3  -0.305957802E-3  m3 kg-1
            '(d2g/dT2)S,P   - 0.118885000E+2   0.127922649E+1  -0.106092735E+2  J kg-1 K-2
            '(d2g/dTdP)S      0.659051552E-6   0.803061596E-6   0.146211315E-5  m3 kg-1 K-1
            '(d2g/dP2)S,T    -0.474672819E-12  0.213086154E-12 -0.261586665E-12 m3 kg-1 Pa-1
            'h                0.334425759E+6  -0.400623363E+5   0.294363423E+6  J kg-1
            'f               -0.447157532E+5   0.150930430E+5  -0.296227102E+5  J kg-1
            'u                0.334321503E+6  -0.400564673E+5   0.294265035E+6  J kg-1
            's                0.107375993E+4  -0.156230907E+3   0.917529024E+3  J kg-1 K-1
            'rho              0.971883832E+3   -                0.102985888E+4  kg m - 3
            'cP               0.419664050E+4  -0.451566952E+3   0.374507355E+4  J kg-1 K-1
            'w                0.155446297E+4   -                0.396127835E+4  m s-1
            'µW              -0.446114969E+5  -0.101085536E+5  -0.547200505E+5  J kg-1

            Dim CRLF As String
            Dim txt As String, row As String
            Dim S As Double, t As Double, p As Double, q(3) As Double
            Dim i As Integer, j As Integer

            CRLF = Chr(13) + Chr(10)

            txt = " Implementation of IAPWS-08 in Visual Basic" + CRLF
            txt = txt + " for Publication in Ocean Science, 2009" + CRLF
            txt = txt + " R. Feistel, IOW, Version " + Version + CRLF
            txt = txt + " Compiled on " + CStr(Now) + CRLF + CRLF

            txt = txt + " Function values as given in Table 8b of IAPWS-08:" + CRLF
            txt = txt + " Properties at S = 0.03516504 kg kg-1,  T = 273.15 K, P = 101325 Pa" + CRLF + CRLF

            txt = txt + "Quantity         Water part        Saline part       Seawater        Unit" + CRLF

            t = 353
            p = 101325

            For i = 1 To 16
                Select Case i
                    Case 1 : row = "g               -0.446114969E+5    0.150871740E+5   -0.295243229E+5  J kg-1"
                    Case 2 : row = "(dg/dS)T,P       0.0               0.251957276E+6    0.251957276E+6  J kg-1"
                    Case 3 : row = "(dg/dT)S,P      -0.107375993E+4    0.156230907E+3   -0.917529024E+3  J kg-1 K-1"
                    Case 4 : row = "(dg/dP)S,T       0.102892956E-2   -0.579227286E-4    0.971006828E-3  m3 kg-1"
                    Case 5 : row = "(d2g/dSdP)T      0.0              -0.305957802E-3   -0.305957802E-3  m3 kg-1"
                    Case 6 : row = "(d2g/dT2)S,P   - 0.118885000E+2    0.127922649E+1   -0.106092735E+2  J kg-1 K-2"
                    Case 7 : row = "(d2g/dTdP)S      0.659051552E-6    0.803061596E-6    0.146211315E-5  m3 kg-1 K-1"
                    Case 8 : row = "(d2g/dP2)S,T    -0.474672819E-12   0.213086154E-12  -0.261586665E-12 m3 kg-1 Pa-1"
                    Case 9 : row = "h                0.334425759E+6   -0.400623363E+5    0.294363423E+6  J kg-1"
                    Case 10 : row = "f               -0.447157532E+5    0.150930430E+5   -0.296227102E+5  J kg-1"
                    Case 11 : row = "u                0.334321503E+6   -0.400564673E+5    0.294265035E+6  J kg-1"
                    Case 12 : row = "s                0.107375993E+4   -0.156230907E+3    0.917529024E+3  J kg-1 K-1"
                    Case 13 : row = "rho              0.971883832E+3    -                 0.102985888E+4  kg m - 3"
                    Case 14 : row = "cP               0.419664050E+4   -0.451566952E+3    0.374507355E+4  J kg-1 K-1"
                    Case 15 : row = "w                0.155446297E+4    -                 0.396127835E+4  m s-1"
                    Case Else : row = "µW              -0.446114969E+5   -0.101085536E+5   -0.547200505E+5  J kg-1"
                End Select

                txt = txt + row + CRLF
                txt = txt + "this code:      "

                For j = 1 To 3 Step 2
                    S = IIf(j = 3, 0.1, 0)
                    q(j) = 0
                    Select Case i
                        Case 1 : q(j) = sea_g_si(0, 0, 0, S, t, p)
                        Case 2 : If S > 0 Then q(j) = sea_g_si(1, 0, 0, S, t, p) 'no s-derivative of pure water
                        Case 3 : q(j) = sea_g_si(0, 1, 0, S, t, p)
                        Case 4 : q(j) = sea_g_si(0, 0, 1, S, t, p)
                        Case 5 : If S > 0 Then q(j) = sea_g_si(1, 0, 1, S, t, p) 'no s-derivative of pure water
                        Case 6 : q(j) = sea_g_si(0, 2, 0, S, t, p)
                        Case 7 : q(j) = sea_g_si(0, 1, 1, S, t, p)
                        Case 8 : q(j) = sea_g_si(0, 0, 2, S, t, p)

                        Case 9 : q(j) = sea_enthalpy_si(S, t, p)
                        Case 10 : q(j) = sea_g_si(0, 0, 0, S, t, p) - p * sea_g_si(0, 0, 1, S, t, p)
                        Case 11 : q(j) = sea_g_si(0, 0, 0, S, t, p) - t * sea_g_si(0, 1, 0, S, t, p) - p * sea_g_si(0, 0, 1, S, t, p)
                        Case 12 : q(j) = sea_entropy_si(S, t, p)
                        Case 13 : q(j) = sea_density_si(S, t, p)
                        Case 14 : q(j) = sea_cp_si(S, t, p)
                        Case 15 : q(j) = sea_soundspeed_si(S, t, p)
                        Case 16 : q(j) = sea_chempot_h2o_si(S, t, p)
                    End Select

                Next j
                txt = txt + Left(EFormat(q(1), 9) + Space(18), 18)
                Select Case i
                    Case 13, 15 : txt = txt + Left(" -" + Space(18), 18)  'saline part only if linear in g
                    Case Else : txt = txt + Left(EFormat(q(3) - q(1), 9) + Space(18), 18)
                End Select
                txt = txt + EFormat(q(3), 9)
                txt = txt + CRLF + CRLF

            Next i

            chk_IAPWS08_Table8b = txt

        End Function


        Public Function chk_IAPWS08_Table8c() As String

            'TABLE 8  Numerical check values for the water part computed from
            'gW and its derivatives, Table 4, for the saline part computed from
            'gS and its derivatives, Eq. (4), and for the seawater property
            'computed from the Gibbs function g = gW + gS and its derivatives, Eq. (3).
            'The numerical functions evaluated here at given points (S, T, p) are defined in Tables 3-6.

            'c)  Properties at S = Sn = 0.035 165 04 kg kg-1, T = T0 = 273.15 K, p = 1E8 Pa
            '
            'Quantity  Water part  Saline part Property of seawater  Unit
            'g                0.977303862E+5  -0.260093051E+4   0.951294557E+5  J kg-1
            '(dg/dS)_T,P      0.0             -0.545861581E+4  -0.545861581E+4  J kg-1
            '(dg/dT)_S,P      0.851466502E-    0.754045685E-    0.160551219E+2  J kg-1 K-1
            '(dg/dP)_S,T      0.956683329E-3  -0.229123842E-4   0.933770945E-3  m3 kg-1
            '(d2g/dSdP)_T     0.0             -0.640757619E-3  -0.640757619E-3  m3 kg-1
            '(d2g/dT2)_S,P   -0.142969873E+2   0.488076974     -0.138089104E+2  J kg-1 K-2
            '(d2g/dTdP)_S     0.199079571E-6   0.466284412E-7   0.245708012E-6  m3 kg-1 K-1
            '(d2g/dP2)_S,T   -0.371530889E-12  0.357345736E-13 -0.335796316E-12 m3 kg-1 Pa-1
            'h                0.954046055E+5  -0.466060630E+4   0.907439992E+5  J kg-1
            'f                0.206205330E+4  -0.309692089E+3   0.175236121E+4  J kg-1
            'u               -0.263727446E+3  -0.236936788E+4  -0.263309532E+4  J kg-1
            's               -0.851466502E+1  -0.754045685E+1  -0.160551219E+2  J kg-1 K-1
            'rho              0.104527796E+4   -                0.107092645E+4  kg m-3
            'cP               0.390522209E+4  -0.133318225E+3   0.377190387E+4  J kg-1 K-1
            'w                0.157542240E+4   -                0.162198998E+4  m s-1
            'µW               0.977303862E+5  -0.240897806E+4   0.953214082E+5  J kg-1

            Dim CRLF As String
            Dim txt As String, row As String
            Dim S As Double, t As Double, p As Double, q(3) As Double
            Dim i As Integer, j As Integer

            CRLF = Chr(13) + Chr(10)

            txt = " Implementation of IAPWS-08 in Visual Basic" + CRLF
            txt = txt + " for Publication in Ocean Science, 2009" + CRLF
            txt = txt + " R. Feistel, IOW, Version " + Version + CRLF
            txt = txt + " Compiled on " + CStr(Now) + CRLF + CRLF

            txt = txt + " Function values as given in Table 8c of IAPWS-08:" + CRLF
            txt = txt + " Properties at S = 0.03516504 kg kg-1,  T = 273.15 K, P = 100 MPa" + CRLF + CRLF

            txt = txt + "Quantity         Water part        Saline part       Seawater        Unit" + CRLF

            t = 273.15
            p = 100000000.0#

            For i = 1 To 16
                Select Case i
                    Case 1 : row = "g                0.977303862E+5   -0.260093051E+4    0.951294557E+5  J kg-1"
                    Case 2 : row = "(dg/dS)_T,P      0.0              -0.545861581E+4   -0.545861581E+4  J kg-1"
                    Case 3 : row = "(dg/dT)_S,P      0.851466502E+1    0.754045685E+1    0.160551219E+2  J kg-1 K-1"
                    Case 4 : row = "(dg/dP)_S,T      0.956683329E-3   -0.229123842E-4    0.933770945E-3  m3 kg-1"
                    Case 5 : row = "(d2g/dSdP)_T     0.0              -0.640757619E-3   -0.640757619E-3  m3 kg-1"
                    Case 6 : row = "(d2g/dT2)_S,P   -0.142969873E+2    0.488076974      -0.138089104E+2  J kg-1 K-2"
                    Case 7 : row = "(d2g/dTdP)_S     0.199079571E-6    0.466284412E-7    0.245708012E-6  m3 kg-1 K-1"
                    Case 8 : row = "(d2g/dP2)_S,T   -0.371530889E-12   0.357345736E-13  -0.335796316E-12 m3 kg-1 Pa-1"
                    Case 9 : row = "h                0.954046055E+5   -0.466060630E+4    0.907439992E+5  J kg-1"
                    Case 10 : row = "f                0.206205330E+4   -0.309692089E+3    0.175236121E+4  J kg-1"
                    Case 11 : row = "u               -0.263727446E+3   -0.236936788E+4   -0.263309532E+4  J kg-1"
                    Case 12 : row = "s               -0.851466502E+1   -0.754045685E+1   -0.160551219E+2  J kg-1 K-1"
                    Case 13 : row = "rho              0.104527796E+4    -                 0.107092645E+4  kg m-3"
                    Case 14 : row = "cP               0.390522209E+4   -0.133318225E+3    0.377190387E+4  J kg-1 K-1"
                    Case 15 : row = "w                0.157542240E+4    -                 0.162198998E+4  m s-1"
                    Case Else : row = "µW               0.977303862E+5   -0.240897806E+4    0.953214082E+5  J kg-1"
                End Select

                txt = txt + row + CRLF
                txt = txt + "this code:      "

                For j = 1 To 3 Step 2
                    S = IIf(j = 3, 0.03516504, 0)
                    q(j) = 0
                    Select Case i
                        Case 1 : q(j) = sea_g_si(0, 0, 0, S, t, p)
                        Case 2 : If S > 0 Then q(j) = sea_g_si(1, 0, 0, S, t, p) 'no s-derivative of pure water
                        Case 3 : q(j) = sea_g_si(0, 1, 0, S, t, p)
                        Case 4 : q(j) = sea_g_si(0, 0, 1, S, t, p)
                        Case 5 : If S > 0 Then q(j) = sea_g_si(1, 0, 1, S, t, p) 'no s-derivative of pure water
                        Case 6 : q(j) = sea_g_si(0, 2, 0, S, t, p)
                        Case 7 : q(j) = sea_g_si(0, 1, 1, S, t, p)
                        Case 8 : q(j) = sea_g_si(0, 0, 2, S, t, p)

                        Case 9 : q(j) = sea_enthalpy_si(S, t, p)
                        Case 10 : q(j) = sea_g_si(0, 0, 0, S, t, p) - p * sea_g_si(0, 0, 1, S, t, p)
                        Case 11 : q(j) = sea_g_si(0, 0, 0, S, t, p) - t * sea_g_si(0, 1, 0, S, t, p) - p * sea_g_si(0, 0, 1, S, t, p)
                        Case 12 : q(j) = sea_entropy_si(S, t, p)
                        Case 13 : q(j) = sea_density_si(S, t, p)
                        Case 14 : q(j) = sea_cp_si(S, t, p)
                        Case 15 : q(j) = sea_soundspeed_si(S, t, p)
                        Case 16 : q(j) = sea_chempot_h2o_si(S, t, p)
                    End Select

                Next j
                txt = txt + Left(EFormat(q(1), 9) + Space(18), 18)
                Select Case i
                    Case 13, 15 : txt = txt + Left(" -" + Space(18), 18)  'saline part only if linear in g
                    Case Else : txt = txt + Left(EFormat(q(3) - q(1), 9) + Space(18), 18)
                End Select
                txt = txt + EFormat(q(3), 9)
                txt = txt + CRLF + CRLF

            Next i

            chk_IAPWS08_Table8c = txt

        End Function


        Public Function Sea_3a_example_call(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As String

            Dim CRLF As String, TB As String
            Dim txt As String

            CRLF = Chr(13) + Chr(10)
            TB = Chr(9)

            txt = " Implementation of thermodynamic properties of seawater in Visual Basic" + CRLF
            txt = txt + " for Publication in Ocean Science, 2009" + CRLF
            txt = txt + " R. Feistel, IOW, Version " + Version + CRLF
            txt = txt + " Compiled on " + CStr(Now) + CRLF + CRLF

            If p_si <= 0 Then
                txt = txt + "incorrect: negative pressure"
                Sea_3a_example_call = txt
                Exit Function
            End If

            If t_si <= 0 Then
                txt = txt + "incorrect: negative temperature"
                Sea_3a_example_call = txt
                Exit Function
            End If

            If sa_si < 0 Then
                txt = txt + "incorrect: negative salinity"
                Sea_3a_example_call = txt
                Exit Function
            End If

            If sa_si >= 1 Then
                txt = txt + "incorrect: salinity >= 1000 g/kg"
                Sea_3a_example_call = txt
                Exit Function
            End If


            If p_si > sal_pmax Then
                txt = txt + "Warning: pressure >" + Str(sal_pmax) + " Pa is outside validity" + CRLF + CRLF
            End If

            If p_si < sal_pmin Then
                txt = txt + "Warning: pressure <" + Str(sal_pmin) + " Pa is outside validity" + CRLF + CRLF
            End If

            If t_si > sal_tmax Then
                txt = txt + "Warning: temperature >" + Str(sal_tmax) + " K is outside validity" + CRLF + CRLF
            End If

            If t_si < sal_tmin Then
                txt = txt + "Warning: temperature <" + Str(sal_tmin) + " K is outside validity" + CRLF + CRLF
            End If

            If sa_si > sal_smax Then
                txt = txt + "Warning: salinity >" + Str(sal_smax) + " K is outside validity" + CRLF + CRLF
            End If

            If sa_si < sal_smin Then
                txt = txt + "Warning: salinity >" + Str(sal_smin) + " K is outside validity" + CRLF + CRLF
            End If

            txt = txt + " Absolute salinity:          " + TB + Str(sa_si) + TB + "kg/kg" + CRLF
            txt = txt + " Absolute temperature:       " + TB + Str(t_si) + TB + "K" + CRLF
            txt = txt + " Absolute pressure:          " + TB + Str(p_si) + TB + "Pa" + CRLF + CRLF
            txt = txt + " Isobaric heat capacity:     " + TB + Str(sea_cp_si(sa_si, t_si, p_si)) + TB + "J/(kg K)" + CRLF
            txt = txt + " Density:                    " + TB + Str(sea_density_si(sa_si, t_si, p_si)) + TB + "kg/m3" + CRLF
            txt = txt + " Enthalpy:                   " + TB + Str(sea_enthalpy_si(sa_si, t_si, p_si)) + TB + "J/kg" + CRLF
            txt = txt + " Entropy:                    " + TB + Str(sea_entropy_si(sa_si, t_si, p_si)) + TB + "J/(kg K)" + CRLF
            txt = txt + " Thermal expansion:          " + TB + Str(sea_g_expansion_t_si(sa_si, t_si, p_si)) + TB + "1/K" + CRLF
            txt = txt + " Haline contraction:         " + TB + Str(sea_g_contraction_t_si(sa_si, t_si, p_si)) + TB + "kg/kg" + CRLF
            txt = txt + " Gibbs energy:               " + TB + Str(sea_gibbs_energy_si(sa_si, t_si, p_si)) + TB + "J/kg" + CRLF
            txt = txt + " Internal energy:            " + TB + Str(sea_internal_energy_si(sa_si, t_si, p_si)) + TB + "J/kg" + CRLF
            txt = txt + " Adiabatic compressibility:  " + TB + Str(sea_kappa_s_si(sa_si, t_si, p_si)) + TB + "1/Pa" + CRLF
            txt = txt + " Isothermal compressibility: " + TB + Str(sea_kappa_t_si(sa_si, t_si, p_si)) + TB + "1/Pa" + CRLF
            txt = txt + " Adiabatic lapse rate:       " + TB + Str(sea_lapserate_si(sa_si, t_si, p_si)) + TB + "K/Pa" + CRLF
            txt = txt + " Osmotic coefficient:        " + TB + Str(sea_osm_coeff_si(sa_si, t_si, p_si)) + TB + "1" + CRLF
            txt = txt + " Sound speed:                " + TB + Str(sea_soundspeed_si(sa_si, t_si, p_si)) + TB + "m/s" + CRLF + CRLF

            txt = txt + "Note: Absolute energy and entropy are defined by IAPWS-95 and IAPWS-08" + CRLF + CRLF

            Sea_3a_example_call = txt

        End Function




#End Region

#Region "Sea_3b"

        '#########################################################################

        'This module requires the library modules:
        '     Constants_0_Mdl, file Constants_0.bas
        '     Sal_2_Mdl,     file Sal_2.bas
        '     Flu_3a_Mdl,    file Flu_3a.bas
        '     Sea_3a_Mdl,    file Sea_3a.bas

        '#########################################################################


        'This module implements the enthalpy of seawater depending on salinity, entropy
        'and pressure, as well as its partial derivatives, as defined in:

        'Feistel, R.
        'A Gibbs Function for Seawater Thermodynamics
        'for -6 °C to 80 °C and Salinity up to 120 g kg-1
        'Deep-Sea Research I, 55(2008)1639-1671

        'Implementation in VB6 by Rainer Feistel
        'for publication in Ocean Science, as described in the papers

        'Feistel, R., Wright, D.G., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part I: Background and Equations.
        'Ocean Science, 2009

        'Wright, D.G., Feistel, R., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part II: The Library Routines,
        'Ocean Science, 2009


        'Private Const ErrorReturn = 9.99999999E+98

        'Control parameters of the temperature iteration
        'Private ctrl_initialized As Integer

        Private ctrl_mode_pottemp As Integer
        'Private ctrl_loop_maximum As Long
        Private ctrl_init_pottemp As Double
        Private ctrl_eps_exit_pottemp As Double


        Public Function sea_h_si(ByVal drv_s As Long, _
                                ByVal drv_eta As Long, _
                                ByVal drv_p As Long, _
                                ByVal sa_si As Double, _
                                ByVal eta_si As Double, _
                                ByVal p_si As Double) As Double

            'this function implements enthalpy as a thermodynamic potential, depending
            'on salinity, entropy and pressure

            'returns sea_h_si as the S-eta-P derivative

            '(d/dS)^drv_s (d/deta)^drv_eta (d/dP)^drv_p h(S,eta,P)

            'of the specific enthalpy of seawater, h(S,eta,P), in J/kg

            'sa_si     absolute salinity in kg/kg
            'eta_si    specific entropy in J/(kg K)
            'p_si      absolute pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a
            '      and on the iteration settings for temperature by set_it_ctrl_pottemp of this module

            'Check values with default settings:
            'sea_h_si( 0, 0, 0, 0.035, 500, 1E5) = 145481.970750042
            'sea_h_si( 1, 0, 0, 0.035, 500, 1E5) = 86860.798048569
            'sea_h_si( 0, 1, 0, 0.035, 500, 1E5) = 309.557955853377
            'sea_h_si( 0, 0, 1, 0.035, 500, 1E5) = 9.81092930969333E-04
            'sea_h_si( 2, 0, 0, 0.035, 500, 1E5) = 2393730.16716348
            'sea_h_si( 1, 1, 0, 0.035, 500, 1E5) = 72.5298236487807
            'sea_h_si( 1, 0, 1, 0.035, 500, 1E5) =-6.84629317366511E-04
            'sea_h_si( 0, 2, 0, 0.035, 500, 1E5) = 7.72873234085026E-02
            'sea_h_si( 0, 1, 1, 0.035, 500, 1E5) = 2.86305358701992E-08
            'sea_h_si( 0, 0, 2, 0.035, 500, 1E5) =-3.96880481108341E-13

            Dim t As Double

            sea_h_si = ErrorReturn

            If drv_s < 0 Or drv_s > 2 Then Exit Function
            If drv_eta < 0 Or drv_eta > 2 Then Exit Function
            If drv_p < 0 Or drv_p > 2 Then Exit Function

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si < 0 Then Exit Function

            'compute temperature from entropy.
            'if p_si = in-situ pressure, this is in-situ temperature
            'if p_si = reference pressure, this is potential temperature
            t = sea_temperature_si(sa_si, eta_si, p_si)
            If t = ErrorReturn Then Exit Function

            sea_h_si = sea_s_eta_p_derivatives_si(drv_s, drv_eta, drv_p, sa_si, t, p_si)

        End Function


        Public Function sea_pottemp_si(ByVal sa_si As Double, _
                                    ByVal t_si As Double, _
                                    ByVal p_si As Double, _
                                    ByVal pr_si As Double) As Double

            'this function computes potential temperature of seawater

            'returns   273.15 K + theta(S,T,P,Pr) potential temperature of seawater in K,
            'sa_si     absolute salinity in kg/kg
            't_si      absolute in-situ temperature in K
            'p_si      absolute in-situ pressure in Pa
            'pr_si     absolute reference pressure in Pa

            'Check value with default settings: sea_pottemp_si(0.035,300,1e7,1e5) = 299.771869405324

            Dim S As Double

            sea_pottemp_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si < 0 Then Exit Function
            If p_si < 0 Then Exit Function
            If pr_si < 0 Then Exit Function

            If p_si = pr_si Then
                sea_pottemp_si = t_si
                Exit Function
            End If

            S = sea_entropy_si(sa_si, t_si, p_si)  'specific entropy in-situ
            If S = ErrorReturn Then Exit Function

            sea_pottemp_si = sea_temperature_si(sa_si, S, pr_si)

        End Function


        Public Function sea_potdensity_si(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double, _
                                        ByVal pr_si As Double) As Double

            'this function computes potential density of seawater

            'returns   rho_theta(S,T,P,Pr) potential density of seawater in kg/m³,
            'sa_si     absolute salinity in kg/kg
            't_si      absolute in-situ temperature in K
            'p_si      absolute in-situ pressure in Pa
            'pr_si     absolute reference pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a
            '      and on the iteration settings for temperature by set_it_ctrl_pottemp of this module

            'Check value with default settings: sea_potdensity_si(0.035,300,1e7,1e5) = 1022.71520130298

            Dim S As Double, v As Double

            sea_potdensity_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si < 0 Then Exit Function
            If p_si < 0 Then Exit Function
            If pr_si < 0 Then Exit Function

            S = sea_entropy_si(sa_si, t_si, p_si)  'specific entropy in-situ
            If S = ErrorReturn Then Exit Function

            v = sea_h_si(0, 0, 1, sa_si, S, pr_si) 'specific volume at reference pressure
            If v = ErrorReturn Then Exit Function
            If v <= 0 Then Exit Function

            sea_potdensity_si = 1 / v

        End Function


        Public Function sea_potenthalpy_si(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double, _
                                        ByVal pr_si As Double) As Double

            'this function computes potential enthalpy of seawater

            'returns   h_theta(S,T,P,Pr) potential enthalpy of seawater in J/kg,
            'sa_si     absolute salinity in kg/kg
            't_si      absolute in-situ temperature in K
            'p_si      absolute in-situ pressure in Pa
            'pr_si     absolute reference pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a
            '      and on the iteration settings for temperature by set_it_ctrl_pottemp of this module

            'Check value with default settings: sea_potenthalpy_si(0.035,300,1e7,1e5) = 106307.996083199

            Dim S As Double

            sea_potenthalpy_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If t_si < 0 Then Exit Function
            If p_si < 0 Then Exit Function
            If pr_si < 0 Then Exit Function

            S = sea_entropy_si(sa_si, t_si, p_si)  'specific entropy in-situ
            If S = ErrorReturn Then Exit Function

            sea_potenthalpy_si = sea_h_si(0, 0, 0, sa_si, S, pr_si)

        End Function


        Public Function sea_h_expansion_t_si(ByVal sa_si As Double, _
                                            ByVal eta_si As Double, _
                                            ByVal p_si As Double) As Double

            'this function computes thermal expansion coefficient of seawater as function of entropy

            'returns   sea_h_expansion_t_si(S,eta,P) = (1/v)*(dv/dt)_s_p thermal expansion coefficient in 1/K,
            '                                          at constant salinity and pressure
            'sa_si     absolute salinity in kg/kg
            'eta_si    entropy in J/(kg K)
            'p_si      absolute in-situ pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a
            '      and on the iteration settings for temperature by set_it_ctrl_pottemp of this module

            'Check value with default settings: sea_h_expansion_t_si(0.035, 500, 1E5) = 3.77581809091213E-04

            Dim hp As Double, hep As Double, hee As Double

            sea_h_expansion_t_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function

            hp = sea_h_si(0, 0, 1, sa_si, eta_si, p_si)
            If hp = ErrorReturn Or hp = 0 Then Exit Function
            hep = sea_h_si(0, 1, 1, sa_si, eta_si, p_si)
            If hep = ErrorReturn Then Exit Function
            hee = sea_h_si(0, 2, 0, sa_si, eta_si, p_si)
            If hee = ErrorReturn Or hee = 0 Then Exit Function

            sea_h_expansion_t_si = hep / (hp * hee)

        End Function


        Public Function sea_h_expansion_theta_si(ByVal sa_si As Double, _
                                                ByVal eta_si As Double, _
                                                ByVal p_si As Double, _
                                                ByVal pref_si As Double) As Double

            'this function computes thermal expansion coefficient wrt potential temperature of seawater
            'as function of entropy

            'returns   sea_h_expansion_theta_si(S,eta,P) = (1/v)*(dv/dtheta)_s_p thermal expansion coefficient in 1/K,
            '                                          at constant salinity and pressure
            'sa_si     absolute salinity in kg/kg
            'eta_si    entropy in J/(kg K)
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a
            '      and on the iteration settings for temperature by set_it_ctrl_pottemp of this module

            'Check value with default settings: sea_h_expansion_theta_si(0.035, 500, 1E7, 1E5) = 3.84755380181319E-04

            Dim hp As Double, hep As Double, hee As Double

            sea_h_expansion_theta_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function
            If pref_si <= 0 Then Exit Function

            hp = sea_h_si(0, 0, 1, sa_si, eta_si, p_si)
            If hp = ErrorReturn Or hp = 0 Then Exit Function
            hep = sea_h_si(0, 1, 1, sa_si, eta_si, p_si)
            If hep = ErrorReturn Then Exit Function
            hee = sea_h_si(0, 2, 0, sa_si, eta_si, pref_si)
            If hee = ErrorReturn Or hee = 0 Then Exit Function

            sea_h_expansion_theta_si = hep / (hp * hee)

        End Function


        Public Function sea_h_expansion_h_si(ByVal sa_si As Double, _
                                            ByVal eta_si As Double, _
                                            ByVal p_si As Double, _
                                            ByVal pref_si As Double) As Double

            'this function computes the thermal expansion coefficient wrt potential enthapy of seawater
            'as function of entropy

            'returns   sea_h_expansion_h_si(S,eta,P) = (1/v)*(dv/dhpot)_s_p thermal expansion coefficient in (kg K) / J,
            '                                          at constant salinity and pressure. hpot is potential enthalpy.
            'sa_si     absolute salinity in kg/kg
            'eta_si    entropy in J/(kg K)
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a
            '      and on the iteration settings for temperature by set_it_ctrl_pottemp of this module

            'Check value with default settings: sea_h_expansion_h_si(0.035, 500, 1E7, 1E5) = 9.60618615640423E-08

            Dim hp As Double, hep As Double, he As Double

            sea_h_expansion_h_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function
            If pref_si <= 0 Then Exit Function

            hp = sea_h_si(0, 0, 1, sa_si, eta_si, p_si)
            If hp = ErrorReturn Or hp = 0 Then Exit Function
            hep = sea_h_si(0, 1, 1, sa_si, eta_si, p_si)
            If hep = ErrorReturn Then Exit Function
            he = sea_h_si(0, 1, 0, sa_si, eta_si, pref_si)
            If he = ErrorReturn Or he = 0 Then Exit Function

            sea_h_expansion_h_si = hep / (hp * he)

        End Function


        Public Function sea_h_contraction_t_si(ByVal sa_si As Double, _
                                            ByVal eta_si As Double, _
                                            ByVal p_si As Double) As Double

            'this function computes the adiabatic haline contraction coefficient of seawater

            'returns   sea_h_contraction_t_si(S,eta,P) = - (1/v)*(dv/ds)_t_p haline contraction coefficient in kg/kg,
            '                                                (kg seawater / kg salt)
            '                                               at constant temperature and pressure
            'sa_si     absolute salinity in kg/kg
            'eta_si    entropy in J/(kg K)
            'p_si      absolute in-situ pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a
            '      and on the iteration settings for temperature by set_it_ctrl_pottemp of this module

            'Check value with default settings: sea_h_contraction_t_si(0.035, 500, 1E5) = 0.725209049048546

            Dim hp As Double, hep As Double, hse As Double, hee As Double, hsp As Double

            sea_h_contraction_t_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function

            hp = sea_h_si(0, 0, 1, sa_si, eta_si, p_si)
            If hp = ErrorReturn Or hp = 0 Then Exit Function
            hse = sea_h_si(1, 1, 0, sa_si, eta_si, p_si)
            If hse = ErrorReturn Then Exit Function
            hep = sea_h_si(0, 1, 1, sa_si, eta_si, p_si)
            If hep = ErrorReturn Then Exit Function
            hsp = sea_h_si(1, 0, 1, sa_si, eta_si, p_si)
            If hsp = ErrorReturn Then Exit Function
            hee = sea_h_si(0, 2, 0, sa_si, eta_si, p_si)
            If hee = ErrorReturn Or hee = 0 Then Exit Function

            sea_h_contraction_t_si = (hse * hep - hsp * hee) / (hp * hee)

        End Function


        Public Function sea_h_contraction_theta_si(ByVal sa_si As Double, _
                                                ByVal eta_si As Double, _
                                                ByVal p_si As Double, _
                                                ByVal pref_si As Double) As Double

            'this function computes the haline contraction coefficient of seawater at constant potential temperature

            'returns   sea_h_contraction_theta_si(S,eta,P) = - (1/v)*(dv/ds)_theta_p haline contraction coefficient in kg/kg,
            '                                                (kg seawater / kg salt)
            '                                                at constant potential temperature and pressure
            'sa_si     absolute salinity in kg/kg
            'eta_si    entropy in J/(kg K)
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a
            '      and on the iteration settings for temperature by set_it_ctrl_pottemp of this module

            'Check value with default settings: sea_h_contraction_theta_si(0.035, 500, 1E7, 1E5) = 0.717342103504727

            Dim hp As Double, hep As Double, hse As Double, hee As Double, hsp As Double

            sea_h_contraction_theta_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function
            If pref_si <= 0 Then Exit Function

            hp = sea_h_si(0, 0, 1, sa_si, eta_si, p_si)
            If hp = ErrorReturn Or hp = 0 Then Exit Function
            hse = sea_h_si(1, 1, 0, sa_si, eta_si, pref_si)
            If hse = ErrorReturn Then Exit Function
            hep = sea_h_si(0, 1, 1, sa_si, eta_si, p_si)
            If hep = ErrorReturn Then Exit Function
            hsp = sea_h_si(1, 0, 1, sa_si, eta_si, p_si)
            If hsp = ErrorReturn Then Exit Function
            hee = sea_h_si(0, 2, 0, sa_si, eta_si, pref_si)
            If hee = ErrorReturn Or hee = 0 Then Exit Function

            sea_h_contraction_theta_si = (hse * hep - hsp * hee) / (hp * hee)

        End Function


        Public Function sea_h_contraction_h_si(ByVal sa_si As Double, _
                                            ByVal eta_si As Double, _
                                            ByVal p_si As Double, _
                                            ByVal pref_si As Double) As Double

            'this function computes the haline contraction coefficient of seawater at constant potential enthalpy

            'returns   sea_h_contraction_h_si(S,eta,P) = - (1/v)*(dv/ds)_h_p haline contraction coefficient in kg/kg,
            '                                                (kg seawater / kg salt)
            '                                               at constant potential enthalpy and pressure
            'sa_si     absolute salinity in kg/kg
            'eta_si    entropy in J/(kg K)
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a
            '      and on the iteration settings for temperature by set_it_ctrl_pottemp of this module

            'Check value with default settings: sea_h_contraction_h_si(0.035, 500, 1E7, 1E5) = 0.69777987358974

            Dim hp As Double, hep As Double, hs As Double, he As Double, hsp As Double

            sea_h_contraction_h_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function
            If pref_si <= 0 Then Exit Function

            hp = sea_h_si(0, 0, 1, sa_si, eta_si, p_si)
            If hp = ErrorReturn Or hp = 0 Then Exit Function
            hs = sea_h_si(1, 0, 0, sa_si, eta_si, pref_si)
            If hs = ErrorReturn Then Exit Function
            hep = sea_h_si(0, 1, 1, sa_si, eta_si, p_si)
            If hep = ErrorReturn Then Exit Function
            hsp = sea_h_si(1, 0, 1, sa_si, eta_si, p_si)
            If hsp = ErrorReturn Then Exit Function
            he = sea_h_si(0, 1, 0, sa_si, eta_si, pref_si)
            If he = ErrorReturn Or he = 0 Then Exit Function

            sea_h_contraction_h_si = (hs * hep - hsp * he) / (hp * he)

        End Function


        Public Function sea_temperature_si(ByVal sa_si As Double, _
                                        ByVal eta_si As Double, _
                                        ByVal p_si As Double) As Double

            'returns   t(S,eta,P) (potential) temperature of seawater in K,
            'sa_si     absolute salinity in kg/kg
            'eta_si    entropy in J/(kg K),
            'p_si      absolute (reference) pressure in Pa

            'this is the inverse function to sea_entropy_si(sa_si, t_si, p_si) in Sea_3a

            'note: the accuracy of this function depends on the iteration settings for
            '      density computed in Flu_3a and on those made in this module

            'Check value with default settings: sea_temperature_si(0.035, 500, 1E5) = 309.557955853377

            Dim t As Double, eps As Double, maxit As Long

            sea_temperature_si = ErrorReturn

            init_it_ctrl_pottemp()

            Select Case ctrl_mode_pottemp
                Case 0 : t = 273.15 + eta_si / 4000.0#
                Case Else : t = ctrl_init_pottemp
            End Select

            Select Case ctrl_loop_maximum
                Case 0 : maxit = 100
                Case -1 : sea_temperature_si = t
                    Exit Function
                Case Is > 0 : maxit = ctrl_loop_maximum
                Case Else : Exit Function
            End Select

            eps = ctrl_eps_exit_pottemp
            If eps <= 0 Then Exit Function

            'run the iteration
            sea_temperature_si = pottemp_iteration(sa_si, eta_si, p_si, t, maxit, eps)

        End Function


        Private Function sea_s_eta_p_derivatives_si(ByVal drv_s As Integer, _
                                                ByVal drv_eta As Integer, _
                                                ByVal drv_p As Integer, _
                                                ByVal sa_si As Double, _
                                                ByVal t_si As Double, _
                                                ByVal p_si As Double) As Double

            'this function computes seawater s-eta-p derivatives of h from s-t-p derivatives of g

            Dim g As Double, gt As Double, gp As Double, gs As Double
            Dim gss As Double, gst As Double, gsp As Double
            Dim gtt As Double, gpp As Double, gtp As Double

            Dim h As Double

            'in one case we do not at all need to compute the Gibbs function:
            If drv_s = 0 And drv_eta = 1 And drv_p = 0 Then
                sea_s_eta_p_derivatives_si = t_si
                Exit Function
            End If

            sea_s_eta_p_derivatives_si = ErrorReturn

            Select Case drv_s

                Case 0
                    Select Case drv_eta

                        Case 0
                            Select Case drv_p

                                Case 0 : g = sea_g_si(0, 0, 0, sa_si, t_si, p_si)
                                    If g = ErrorReturn Then Exit Function
                                    gt = sea_g_si(0, 1, 0, sa_si, t_si, p_si)
                                    If gt = ErrorReturn Then Exit Function
                                    h = g - t_si * gt                          'h

                                Case 1 : gp = sea_g_si(0, 0, 1, sa_si, t_si, p_si)
                                    If gp = ErrorReturn Then Exit Function
                                    h = gp                                      'dh/dp

                                Case 2 : gtt = sea_g_si(0, 2, 0, sa_si, t_si, p_si)
                                    If gtt = ErrorReturn Then Exit Function
                                    If gtt = 0 Then Exit Function
                                    gtp = sea_g_si(0, 1, 1, sa_si, t_si, p_si)
                                    If gtp = ErrorReturn Then Exit Function
                                    gpp = sea_g_si(0, 0, 2, sa_si, t_si, p_si)
                                    If gpp = ErrorReturn Then Exit Function
                                    h = (gtt * gpp - gtp ^ 2) / gtt              'd2h/dp2

                                Case Else : Exit Function
                            End Select

                        Case 1
                            Select Case drv_p
                                Case 0 : h = t_si                                     'dh/deta, has already been handled initially

                                Case 1 : gtt = sea_g_si(0, 2, 0, sa_si, t_si, p_si)
                                    If gtt = ErrorReturn Then Exit Function
                                    If gtt = 0 Then Exit Function
                                    gtp = sea_g_si(0, 1, 1, sa_si, t_si, p_si)
                                    If gtp = ErrorReturn Then Exit Function
                                    h = -gtp / gtt                               'd2h/deta dp

                                Case Else : Exit Function
                            End Select

                        Case 2
                            Select Case drv_p
                                Case 0 : gtt = sea_g_si(0, 2, 0, sa_si, t_si, p_si)
                                    If gtt = ErrorReturn Then Exit Function
                                    If gtt = 0 Then Exit Function
                                    h = -1.0# / gtt                                'd2h/deta2

                                Case Else : Exit Function
                            End Select

                        Case Else : Exit Function
                    End Select

                Case 1
                    Select Case drv_eta

                        Case 0
                            Select Case drv_p

                                Case 0 : gs = sea_g_si(1, 0, 0, sa_si, t_si, p_si)
                                    If gs = ErrorReturn Then Exit Function
                                    h = gs                                        'dh/ds

                                Case 1 : gtt = sea_g_si(0, 2, 0, sa_si, t_si, p_si)
                                    If gtt = ErrorReturn Then Exit Function
                                    If gtt = 0 Then Exit Function
                                    gst = sea_g_si(1, 1, 0, sa_si, t_si, p_si)
                                    If gst = ErrorReturn Then Exit Function
                                    gsp = sea_g_si(1, 0, 1, sa_si, t_si, p_si)
                                    If gsp = ErrorReturn Then Exit Function
                                    gtp = sea_g_si(0, 1, 1, sa_si, t_si, p_si)
                                    If gtp = ErrorReturn Then Exit Function
                                    h = (gtt * gsp - gst * gtp) / gtt             'd2h/dsdp

                                Case Else : Exit Function
                            End Select

                        Case 1
                            Select Case drv_p

                                Case 0 : gtt = sea_g_si(0, 2, 0, sa_si, t_si, p_si)
                                    If gtt = ErrorReturn Then Exit Function
                                    If gtt = 0 Then Exit Function
                                    gst = sea_g_si(1, 1, 0, sa_si, t_si, p_si)
                                    If gst = ErrorReturn Then Exit Function
                                    h = -gst / gtt                                'd2h/dsdeta

                                Case Else : Exit Function
                            End Select

                        Case Else : Exit Function
                    End Select


                Case 2
                    Select Case drv_eta

                        Case 0
                            Select Case drv_p

                                Case 0 : gtt = sea_g_si(0, 2, 0, sa_si, t_si, p_si)
                                    If gtt = ErrorReturn Then Exit Function
                                    If gtt = 0 Then Exit Function
                                    gst = sea_g_si(1, 1, 0, sa_si, t_si, p_si)
                                    If gst = ErrorReturn Then Exit Function
                                    gss = sea_g_si(2, 0, 0, sa_si, t_si, p_si)
                                    If gss = ErrorReturn Then Exit Function
                                    h = (gtt * gss - gst ^ 2) / gtt               'd2h/ds2

                                Case Else : Exit Function
                            End Select

                        Case Else : Exit Function
                    End Select

                Case Else : Exit Function
            End Select

            sea_s_eta_p_derivatives_si = h

        End Function


        Private Sub init_it_ctrl_pottemp()

            If ctrl_initialized = -1 Then Exit Sub

            ctrl_initialized = -1

            'Set default values and modes for temperature iteration
            ctrl_loop_maximum = 100
            ctrl_mode_pottemp = 0           'default: theta = t0 + 273*eta/4000
            ctrl_init_pottemp = 273.15
            ctrl_eps_exit_pottemp = 0.0001  'default = 0.1 mK

        End Sub


        Private Function pottemp_iteration(ByVal sa_si As Double, _
                                        ByVal eta_si As Double, _
                                        ByVal p_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal maxit As Long, _
                                        ByVal eps As Double) As Double

            'returns   theta =  potential temperature of seawater in K,
            '          i.e. the temperature that solves eta_si = sea_entropy_si(sa_si, t_si, p_si)

            'sa_si     absolute salinity in kg/kg
            'eta_si    entropy in J/(kg K)
            'p_si      absolute (reference) pressure in Pa
            't_si      absolute (potential) temperature in K, initial value
            'maxit     max. number of iterations
            'eps       required tolerance in K

            Dim i As Long
            Dim S As Double, theta As Double, cp As Double, dt As Double

            pottemp_iteration = ErrorReturn

            If eps <= 0 Then Exit Function
            If maxit <= 0 Then Exit Function

            If check_limits <> 1 Then
                If sa_si < 0 Or sa_si > 1 Or _
                    t_si <= 0 Or p_si <= 0 Then Exit Function
            Else
                'FLU_LIMITS
                If t_si < flu_tmin Or t_si > flu_tmax Then Exit Function
                'SAL_LIMITS
                If t_si < sal_tmin Or t_si > sal_tmax Or _
                    sa_si > sal_smax Or _
                    p_si < sal_pmin Or p_si > sal_pmax Then Exit Function
            End If

            check_limits = check_limits - 1

            theta = t_si
            For i = 1 To maxit

                'get entropy and its first derivative for Newton iteration
                S = sea_entropy_si(sa_si, theta, p_si)
                cp = sea_cp_si(sa_si, theta, p_si)

                If S = ErrorReturn Then Exit For
                If cp = ErrorReturn Then Exit For
                If cp <= 0 Then Exit For

                'next temperature improvement step
                dt = theta * (eta_si - S) / cp
                theta = theta + dt
                If theta <= 0 Then Exit For

                If Abs(dt) < eps Then
                    pottemp_iteration = theta
                    Exit For
                End If

            Next i

            check_limits = check_limits + 1

        End Function


        Public Sub set_it_ctrl_pottemp(ByVal key As String, ByVal value As Double)

            'this sub sets control parameters for the Newton iteration used to compute
            'potential temperature from reference pressure

            'key             value
            'it_steps        0           set iteration number to default (100)
            'it_steps        n > 0       set iteration number to n
            'it_steps       -1           do not iterate, use initial value

            'init_theta      0           use default potential temperature theta = t0 + 273*eta/4000 to start
            'init_theta      t > 0       use value t as potential temperature to start

            'tol_theta       0           use default exit accuracy for potential temperature (0.1 mK)
            'tol_theta       eps > 0     use eps as exit accuracy for potential temperature

            init_it_ctrl_pottemp()

            Select Case LCase(Trim(key))

                Case "it_steps" 'iteration steps
                    Select Case value
                        Case 0 : ctrl_loop_maximum = 100     'default = 100
                        Case Is < 0 : ctrl_loop_maximum = -1
                        Case Else : ctrl_loop_maximum = value
                    End Select

                Case "init_theta" 'start theta
                    Select Case CLng(value)
                        Case 0 : ctrl_mode_pottemp = 0        'default: theta = t0 + eta/4000
                        Case Is > 0 : ctrl_mode_pottemp = 1
                            ctrl_init_pottemp = value
                    End Select

                Case "tol_theta" 'required theta tolerance
                    Select Case value
                        Case 0 : ctrl_eps_exit_pottemp = 0.0001 'default = 0.1 mK
                        Case Is > 0 : ctrl_eps_exit_pottemp = value
                    End Select

            End Select

        End Sub

#End Region

#Region "Sea_3c"

        '#########################################################################

        'This module requires the library modules:
        '     Constants_0_Mdl, file Constants_0.bas
        '     Sea_3a_Mdl,      file Sea_3a.bas
        '     Sea_3b_Mdl,      file Sea_3b.bas

        '#########################################################################


        'This module implements the entropy of seawater depending on salinity, enthalpy
        'and pressure, as defined in:

        'Implementation in VB6 by Rainer Feistel
        'for publication in Ocean Science 2009, as

        'Feistel, R., Wright, D.G., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part I: Background and Equations.
        'Ocean Science, 2009

        'Wright, D.G., Feistel, R., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part II: The Library Routines,
        'Ocean Science, 2009


        'Private Const ErrorReturn = 9.99999999E+98

        'Control parameters of the entropy iteration
        'Private ctrl_initialized As Integer

        Private ctrl_mode_entropy As Integer
        'Private ctrl_loop_maximum As Long
        Private ctrl_init_entropy As Double
        Private ctrl_eps_exit_entropy As Double


        Public Function sea_eta_entropy_si(ByVal sa_si As Double, _
                                        ByVal h_si As Double, _
                                        ByVal p_si As Double) As Double

            'this function computes specific entropy of seawater from enthalpy
            '(the thermodynamic potential in terms of salinity, enthalpy and pressure is entropy)

            'returns   entropy of seawater in J/(kg K),

            'either from
            'sa_si     absolute salinity in kg/kg
            'h_si      in-situ enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa

            'or from
            'sa_si     absolute salinity in kg/kg
            'h_si      potential enthalpy in J/kg,
            'p_si      absolute reference pressure in Pa

            'note: the accuracy of this function depends on the iteration settings for
            '      entropy computed in Sea_3b and on those made in this module

            'Check value with default settings: sea_eta_entropy_si(0.035, 1E5, 1E5) = 350.310622663165

            sea_eta_entropy_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function


            Dim eta As Double, eps As Double, maxit As Long

            sea_eta_entropy_si = ErrorReturn

            init_it_ctrl_entropy()

            Select Case ctrl_mode_entropy
                Case 0 : eta = h_si / 273.15
                Case Else : eta = ctrl_init_entropy
            End Select

            Select Case ctrl_loop_maximum
                Case 0 : maxit = 100
                Case -1 : sea_eta_entropy_si = eta
                    Exit Function
                Case Is > 0 : maxit = ctrl_loop_maximum
                Case Else : Exit Function
            End Select

            eps = ctrl_eps_exit_entropy
            If eps <= 0 Then Exit Function

            sea_eta_entropy_si = entropy_iteration(sa_si, h_si, p_si, eta, maxit, eps)

        End Function


        Public Function sea_eta_temperature_si(ByVal sa_si As Double, _
                                            ByVal h_si As Double, _
                                            ByVal p_si As Double, _
                                            ByVal pref_si As Double, _
                                            ByVal key As String) As Double

            'this function computes in-situ temperature of seawater from either in-situ enthalpy or potential enthalpy

            'returns   temperature of seawater in K,

            'either from
            'sa_si     absolute salinity in kg/kg
            'h_si      in-situ enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   not required
            'key       "h"

            'or from
            'sa_si     absolute salinity in kg/kg
            'h_si      potential enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "hpot"

            'note: the accuracy of this function depends on the iteration settings for
            '      entropy computed in Sea_3b and on those made in this module

            'Check values with default settings:
            'sea_eta_temperature_si(0.035,1E5,1E7,1E5,"h") = 295.985682129013
            'sea_eta_temperature_si(0.035,1E5,1E7,1E5,"hpot") = 298.413424847945

            sea_eta_temperature_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function
            If key <> "h" And key <> "hpot" Then Exit Function
            If key = "hpot" And pref_si <= 0 Then Exit Function

            Dim eta As Double

            If key = "hpot" Then
                eta = sea_eta_entropy_si(sa_si, h_si, pref_si)  'entropy from pot. enthalpy
            Else
                eta = sea_eta_entropy_si(sa_si, h_si, p_si)     'entropy from in-situ enthalpy
            End If
            If eta = ErrorReturn Then Exit Function

            sea_eta_temperature_si = sea_h_si(0, 1, 0, sa_si, eta, p_si)

        End Function


        Public Function sea_eta_pottemp_si(ByVal sa_si As Double, _
                                        ByVal h_si As Double, _
                                        ByVal p_si As Double, _
                                        ByVal pref_si As Double, _
                                        ByVal key As String) As Double

            'this function computes  potential temperature of seawater from either in-situ enthalpy or potential enthalpy

            'returns   absolute potential temperature of seawater in K,

            'either from
            'sa_si     absolute salinity in kg/kg
            'h_si      in-situ enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "h"

            'or from
            'sa_si     absolute salinity in kg/kg
            'h_si      potential enthalpy in J/kg,
            'p_si      not required
            'pref_si   absolute reference pressure in Pa
            'key       "hpot"

            'note: the accuracy of this function depends on the iteration settings for
            '      entropy computed in Sea_3b and on those made in this module

            'Check values with default settings:
            'sea_eta_pottemp_si(0.035,1E5,1E7,1E5,"h") = 295.782199115252
            'sea_eta_pottemp_si(0.035,1E5,1E7,1E5,"hpot") = 298.194955181952

            sea_eta_pottemp_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function
            If key <> "h" And key <> "hpot" Then Exit Function
            If key = "hpot" And pref_si <= 0 Then Exit Function

            Dim eta As Double

            If key = "hpot" Then
                eta = sea_eta_entropy_si(sa_si, h_si, pref_si)  'entropy from pot. enthalpy
            Else
                eta = sea_eta_entropy_si(sa_si, h_si, p_si)     'entropy from in-situ enthalpy
            End If
            If eta = ErrorReturn Then Exit Function

            sea_eta_pottemp_si = sea_h_si(0, 1, 0, sa_si, eta, pref_si)

        End Function


        Public Function sea_eta_density_si(ByVal sa_si As Double, _
                                        ByVal h_si As Double, _
                                        ByVal p_si As Double, _
                                        ByVal pref_si As Double, _
                                        ByVal key As String) As Double

            'this function computes in-situ density of seawater from either in-situ enthalpy or potential enthalpy

            'returns   density of seawater in kg/m3,

            'either from
            'sa_si     absolute salinity in kg/kg
            'h_si      in-situ enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   not required
            'key       "h"

            'or from
            'sa_si     absolute salinity in kg/kg
            'h_si      potential enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "hpot"

            'note: the accuracy of this function depends on the iteration settings for
            '      entropy computed in Sea_3b and on those made in this module

            'Check values with default settings:
            'sea_eta_density_si(0.035,1E5,1E7,1E5,"h") = 1028.10986556355
            'sea_eta_density_si(0.035,1E5,1E7,1E5,"hpot") = 1027.36529797843

            sea_eta_density_si = ErrorReturn

            'If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function
            If key <> "h" And key <> "hpot" Then Exit Function
            If key = "hpot" And pref_si <= 0 Then Exit Function

            Dim eta As Double, v As Double

            If key = "hpot" Then
                eta = sea_eta_entropy_si(sa_si, h_si, pref_si)  'entropy from pot. enthalpy
            Else
                eta = sea_eta_entropy_si(sa_si, h_si, p_si)     'entropy from in-situ enthalpy
            End If
            If eta = ErrorReturn Then Exit Function

            v = sea_h_si(0, 0, 1, sa_si, eta, p_si)      'specific volume
            If v = ErrorReturn Or v <= 0 Then Exit Function

            sea_eta_density_si = 1 / v

        End Function


        Public Function sea_eta_potdensity_si(ByVal sa_si As Double, _
                                            ByVal h_si As Double, _
                                            ByVal p_si As Double, _
                                            ByVal pref_si As Double, _
                                            ByVal key As String) As Double

            'this function computes potential density of seawater from either in-situ enthalpy or potential enthalpy

            'returns   potential density of seawater in kg/m3,

            'either from
            'sa_si     absolute salinity in kg/kg
            'h_si      in-situ enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "h"

            'or from
            'sa_si     absolute salinity in kg/kg
            'h_si      potential enthalpy in J/kg,
            'p_si      not required
            'pref_si   absolute reference pressure in Pa
            'key       "hpot"

            'note: the accuracy of this function depends on the iteration settings for
            '      entropy computed in Sea_3b and on those made in this module

            'Check values with default settings:
            'sea_eta_potdensity_si(0.035,1E5,1E7,1E5,"h") = 1023.91737474051
            'sea_eta_potdensity_si(0.035,1E5,1E7,1E5,"hpot") = 1023.20527737089

            sea_eta_potdensity_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function
            If pref_si <= 0 Then Exit Function

            Dim eta As Double, v As Double

            If key = "hpot" Then
                eta = sea_eta_entropy_si(sa_si, h_si, pref_si)  'entropy from pot. enthalpy
            Else
                eta = sea_eta_entropy_si(sa_si, h_si, p_si)     'entropy from in-situ enthalpy
            End If
            If eta = ErrorReturn Then Exit Function

            v = sea_h_si(0, 0, 1, sa_si, eta, pref_si)      'pot. specific volume
            If v = ErrorReturn Or v <= 0 Then Exit Function

            sea_eta_potdensity_si = 1 / v

        End Function


        Public Function sea_eta_contraction_t_si(ByVal sa_si As Double, _
                                                ByVal x_si As Double, _
                                                ByVal p_si As Double, _
                                                ByVal pref_si As Double, _
                                                ByVal key As String) As Double

            'this function computes the haline contraction coefficient of seawater at constant temperature
            'from either in-situ enthalpy (key="h") or potential enthalpy (key = "hpot")
            'or from in-situ temperature (key="t") or potential temperature (key = "tpot")

            'returns   haline contraction -(1/v)*(dv/ds)_t_p of seawater in kg/kg,

            'either from
            'sa_si     absolute salinity in kg/kg
            'x_si      in-situ enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   not required
            'key       "h"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      potential enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "hpot"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      in-situ temperature in K,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   not required
            'key       "t"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      potential temperature in K,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "tpot"

            'note: the accuracy of this function depends on the iteration settings for
            '      entropy computed in Sea_3b and on those made in this module

            'Check values with default settings:
            'sea_eta_contraction_t_si(0.035,1E5,1E7,1E5,"h") = 0.728755239643685
            'sea_eta_contraction_t_si(0.035,1E5,1E7,1E5,"hpot") = 0.72634931742843
            'sea_eta_contraction_t_si(0.035,300,1E7,1E5,"t") = 0.724913833446307
            'sea_eta_contraction_t_si(0.035,300,1E7,1E5,"tpot") = 0.724714253917745

            Dim eta As Double

            sea_eta_contraction_t_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function

            eta = sea_eta_entropy_x_si(sa_si, x_si, p_si, pref_si, key)  'entropy
            If eta = ErrorReturn Then Exit Function

            sea_eta_contraction_t_si = sea_h_contraction_t_si(sa_si, eta, p_si)

        End Function


        Public Function sea_eta_contraction_theta_si(ByVal sa_si As Double, _
                                                    ByVal x_si As Double, _
                                                    ByVal p_si As Double, _
                                                    ByVal pref_si As Double, _
                                                    ByVal key As String) As Double

            'this function computes the haline contraction coefficient of seawater at constant potential temperature
            'from either in-situ enthalpy (key="h") or potential enthalpy (key = "hpot")
            'or from in-situ temperature (key="t") or potential temperature (key = "tpot")

            'returns   haline contraction - (1/v)*(dv/ds)_t_p of seawater in kg/kg,

            'either from
            'sa_si     absolute salinity in kg/kg
            'x_si      in-situ enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "h"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      potential enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "hpot"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      in-situ temperature in K,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "t"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      potential temperature in K,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "tpot"

            'note: the accuracy of this function depends on the iteration settings for
            '      entropy computed in Sea_3b and on those made in this module

            'Check values with default settings:
            'sea_eta_contraction_theta_si(0.035,1E5,1E7,1E5,"h") = 0.728499505688196
            'sea_eta_contraction_theta_si(0.035,1E5,1E7,1E5,"hpot") = 0.72609973270266
            'sea_eta_contraction_theta_si(0.035,300,1E7,1E5,"t") = 0.724667977117403
            'sea_eta_contraction_theta_si(0.035,300,1E7,1E5,"tpot") = 0.724468894946259

            Dim eta As Double

            sea_eta_contraction_theta_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function
            If pref_si <= 0 Then Exit Function

            eta = sea_eta_entropy_x_si(sa_si, x_si, p_si, pref_si, key)  'entropy
            If eta = ErrorReturn Then Exit Function

            sea_eta_contraction_theta_si = sea_h_contraction_theta_si(sa_si, eta, p_si, pref_si)

        End Function


        Public Function sea_eta_contraction_h_si(ByVal sa_si As Double, _
                                                ByVal x_si As Double, _
                                                ByVal p_si As Double, _
                                                ByVal pref_si As Double, _
                                                ByVal key As String) As Double

            'this function computes the haline contraction coefficient of seawater at constant potential enthalpy
            'from either in-situ enthalpy (key="h") or potential enthalpy (key = "hpot")
            'or from in-situ temperature (key="t") or potential temperature (key = "tpot")

            'returns   haline contraction -(1/v)*(dv/ds)_h_p of seawater in kg/kg,

            'either from
            'sa_si     absolute salinity in kg/kg
            'x_si      in-situ enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "h"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      potential enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "hpot"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      in-situ temperature in K,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "t"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      potential temperature in K,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "tpot"

            'note: the accuracy of this function depends on the iteration settings for
            '      entropy computed in Sea_3b and on those made in this module

            'Check values with default settings:
            'sea_eta_contraction_h_si(0.035,1E5,1E7,1E5,"h") = 0.718452125956615
            'sea_eta_contraction_h_si(0.035,1E5,1E7,1E5,"hpot") = 0.714531922616011
            'sea_eta_contraction_h_si(0.035,300,1E7,1E5,"t") = 0.712069013012519
            'sea_eta_contraction_h_si(0.035,300,1E7,1E5,"tpot") = 0.711718411190048

            Dim eta As Double

            sea_eta_contraction_h_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function
            If pref_si <= 0 Then Exit Function

            eta = sea_eta_entropy_x_si(sa_si, x_si, p_si, pref_si, key)  'entropy
            If eta = ErrorReturn Then Exit Function

            sea_eta_contraction_h_si = sea_h_contraction_h_si(sa_si, eta, p_si, pref_si)

        End Function


        Public Function sea_eta_expansion_t_si(ByVal sa_si As Double, _
                                            ByVal x_si As Double, _
                                            ByVal p_si As Double, _
                                            ByVal pref_si As Double, _
                                            ByVal key As String) As Double

            'this function computes the thermal expansion coefficient of seawater wrt temperature
            'from either in-situ enthalpy (key="h") or potential enthalpy (key = "hpot")
            'or from in-situ temperature (key="t") or potential temperature (key = "tpot")

            'returns   thermal expansion  (1/v)*(dv/dt)_s_p of seawater in 1/K
            '          at constant salinity and pressure.

            'either from
            'sa_si     absolute salinity in kg/kg
            'x_si      in-situ enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   not used
            'key       "h"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      potential enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "hpot"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      in-situ temperature in K,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   not used
            'key       "t"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      potential temperature in K,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "tpot"

            'note: the accuracy of this function depends on the iteration settings for
            '      entropy computed in Sea_3b and on those made in this module

            'Check values with default settings:
            'sea_eta_expansion_t_si(0.035,1E5,1E7,1E5,"h") = 2.89480851145219E-04
            'sea_eta_expansion_t_si(0.035,1E5,1E7,1E5,"hpot") = 3.07242256460933E-04
            'sea_eta_expansion_t_si(0.035,300,1E7,1E5,"t") = 3.18513471409967E-04
            'sea_eta_expansion_t_si(0.035,300,1E7,1E5,"tpot") = 3.20122324739611E-04

            Dim eta As Double

            sea_eta_expansion_t_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function

            eta = sea_eta_entropy_x_si(sa_si, x_si, p_si, pref_si, key)  'entropy
            If eta = ErrorReturn Then Exit Function

            sea_eta_expansion_t_si = sea_h_expansion_t_si(sa_si, eta, p_si)

        End Function


        Public Function sea_eta_expansion_theta_si(ByVal sa_si As Double, _
                                                ByVal x_si As Double, _
                                                ByVal p_si As Double, _
                                                ByVal pref_si As Double, _
                                                ByVal key As String) As Double

            'this function computes the thermal expansion coefficient of seawater wrt potential temperature
            'from either in-situ enthalpy (key="h") or potential enthalpy (key = "hpot")
            'or from in-situ temperature (key="t") or potential temperature (key = "tpot")

            'returns   thermal expansion  (1/v)*(dv/dtheta)_s_p of seawater in 1/K
            '          at constant salinity and pressure. theta is potential temperature

            'either from
            'sa_si     absolute salinity in kg/kg
            'x_si      in-situ enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "h"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      potential enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "hpot"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      in-situ temperature in K,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "t"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      potential temperature in K,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "tpot"

            'note: the accuracy of this function depends on the iteration settings for
            '      entropy computed in Sea_3b and on those made in this module

            'Check values with default settings:
            'sea_eta_expansion_theta_si(0.035,1E5,1E7,1E5,"h") = 2.91293979901893E-04
            'sea_eta_expansion_theta_si(0.035,1E5,1E7,1E5,"hpot") = 3.0913484855363E-04
            'sea_eta_expansion_theta_si(0.035,300,1E7,1E5,"t") = 3.20454167782952E-04
            'sea_eta_expansion_theta_si(0.035,300,1E7,1E5,"tpot") = 3.2206971083886E-04

            Dim eta As Double
            sea_eta_expansion_theta_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function
            If pref_si <= 0 Then Exit Function

            eta = sea_eta_entropy_x_si(sa_si, x_si, p_si, pref_si, key)  'entropy
            If eta = ErrorReturn Then Exit Function

            sea_eta_expansion_theta_si = sea_h_expansion_theta_si(sa_si, eta, p_si, pref_si)

        End Function


        Public Function sea_eta_expansion_h_si(ByVal sa_si As Double, _
                                            ByVal x_si As Double, _
                                            ByVal p_si As Double, _
                                            ByVal pref_si As Double, _
                                            ByVal key As String) As Double

            'this function computes the thermal expansion coefficient of seawater wrt potential enthalpy
            'from either in-situ enthalpy (key="h") or potential enthalpy (key = "hpot")
            'or from in-situ temperature (key="t") or potential temperature (key = "tpot")

            'returns   thermal expansion  (1/v)*(dv/dhpot)_s_p of seawater in kg / J
            '          at constant salinity and pressure. hpot is potential enthalpy.

            'either from
            'sa_si     absolute salinity in kg/kg
            'x_si      in-situ enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "h"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      potential enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "hpot"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      in-situ temperature in K,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "t"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      potential temperature in K,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "tpot"

            'note: the accuracy of this function depends on the iteration settings for
            '      entropy computed in Sea_3b and on those made in this module

            'Check values with default settings:
            'sea_eta_expansion_h_si(0.035,1E5,1E7,1E5,"h") = 7.28514646020664E-08
            'sea_eta_expansion_h_si(0.035,1E5,1E7,1E5,"hpot") = 7.7287677224533E-08
            'sea_eta_expansion_h_si(0.035,300,1E7,1E5,"t") = 8.01009066332972E-08
            'sea_eta_expansion_h_si(0.035,300,1E7,1E5,"tpot") = 8.05023387610723E-08

            Dim eta As Double

            sea_eta_expansion_h_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function
            If pref_si <= 0 Then Exit Function

            eta = sea_eta_entropy_x_si(sa_si, x_si, p_si, pref_si, key)  'entropy
            If eta = ErrorReturn Then Exit Function

            sea_eta_expansion_h_si = sea_h_expansion_h_si(sa_si, eta, p_si, pref_si)

        End Function


        Public Sub set_it_ctrl_entropy(ByVal key As String, ByVal value As Double)

            'this sub sets control parameters for the Newton iteration used to compute
            'entropy from enthalpy

            'key             value
            'it_steps        0           set iteration number to default (100)
            'it_steps        n > 0       set iteration number to n
            'it_steps       -1           do not iterate, use initial value

            'mode_eta        0           use default entropy eta = h/273.15 to start
            'mode_eta        1           use value set by init_eta as entropy to start

            'init_eta        eta         use value eta as entropy to start if mode_eta = 1

            'tol_eta         0           use default exit accuracy for entropy (1E-4 J/(kg K))
            'tol_eta         eps > 0     use eps as exit accuracy for entropy

            init_it_ctrl_entropy()

            Select Case LCase(Trim(key))

                Case "it_steps" 'iteration steps
                    Select Case value
                        Case 0 : ctrl_loop_maximum = 100     'default = 100
                        Case Is < 0 : ctrl_loop_maximum = -1
                        Case Else : ctrl_loop_maximum = value
                    End Select

                Case "mode_eta" 'start eta
                    Select Case CLng(value)
                        Case 0 : ctrl_mode_entropy = 0        'default: eta = h/t0
                        Case 1 : ctrl_mode_entropy = 1        'default: eta = init_eta
                    End Select

                Case "init_eta" 'start eta if ctrl_mode_entropy = 1
                    ctrl_init_entropy = value

                Case "tol_eta" 'required eta tolerance
                    Select Case value
                        Case 0 : ctrl_eps_exit_entropy = 0.0001 'default = 1E-4 J/(kg K)
                        Case Is > 0 : ctrl_eps_exit_entropy = value
                    End Select

            End Select

        End Sub


        Private Sub init_it_ctrl_entropy()

            If ctrl_initialized = -1 Then Exit Sub

            ctrl_initialized = -1

            'Set default values and modes for entropy iteration
            ctrl_loop_maximum = 100
            ctrl_mode_entropy = 0           'default: eta = h/t0
            ctrl_eps_exit_entropy = 0.0001  'default = 1E-4 J/kgK
            ctrl_init_entropy = 0

        End Sub


        Private Function entropy_iteration(ByVal sa_si As Double, _
                                        ByVal h_si As Double, _
                                        ByVal p_si As Double, _
                                        ByVal eta_si As Double, _
                                        ByVal maxit As Long, _
                                        ByVal eps As Double) As Double

            'returns   eta =  entropy of seawater in J/(kg K),
            '          i.e. the entropy that solves h_si = sea_enthalpy_si(sa_si, eta_si, p_si)
            '          for eta_si at given h_si

            'sa_si     absolute salinity in kg/kg
            'h_si      enthalpy in J/kg
            'p_si      absolute pressure in Pa
            'eta_si    entropy in J/(kg K), initial value
            'maxit     max. number of iterations
            'eps       required tolerance in J/(kg K)

            Dim i As Long
            Dim h As Double, eta As Double, t As Double, de As Double

            entropy_iteration = ErrorReturn

            If eps <= 0 Then Exit Function
            If maxit <= 0 Then Exit Function

            If check_limits <> 1 Then
                If sa_si < 0 Or sa_si >= 1 Or _
                    p_si <= 0 Then Exit Function
            Else
                'SAL_LIMITS
                If sa_si < sal_smin Or sa_si > sal_smax Or _
                    p_si < sal_pmin Or p_si > sal_pmax Then Exit Function
            End If

            check_limits = check_limits - 1

            eta = eta_si
            For i = 1 To maxit

                'get enthalpy and its first derivative for Newton iteration
                h = sea_h_si(0, 0, 0, sa_si, eta, p_si)
                If h = ErrorReturn Then Exit For
                t = sea_h_si(0, 1, 0, sa_si, eta, p_si)
                If t = ErrorReturn Then Exit For
                If t <= 0 Then Exit For

                'next entropy improvement step
                de = (h_si - h) / t
                eta = eta + de

                If Abs(de) < eps Then
                    entropy_iteration = eta
                    Exit For
                End If

            Next i

            check_limits = check_limits + 1

        End Function


        Private Function sea_eta_entropy_x_si(ByVal sa_si As Double, _
                                            ByVal x_si As Double, _
                                            ByVal p_si As Double, _
                                            ByVal pref_si As Double, _
                                            ByVal key As String) As Double

            'this function computes the entropy of seawater
            'from either in-situ enthalpy (key="h") or potential enthalpy (key = "hpot")
            'or from in-situ temperature (key="t") or potential temperature (key = "tpot")

            'returns   entropy of seawater in J/(kg K),

            'either from
            'sa_si     absolute salinity in kg/kg
            'x_si      in-situ enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   not required
            'key       "h"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      potential enthalpy in J/kg,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "hpot"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      in-situ temperature in K,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   not required
            'key       "t"

            'or from
            'sa_si     absolute salinity in kg/kg
            'x_si      potential temperature in K,
            'p_si      absolute in-situ pressure in Pa
            'pref_si   absolute reference pressure in Pa
            'key       "tpot"

            sea_eta_entropy_x_si = ErrorReturn

            If sa_si < 0 Or sa_si >= 1 Then Exit Function
            If p_si <= 0 Then Exit Function

            Select Case key
                Case "hpot"
                    If pref_si <= 0 Then Exit Function
                    sea_eta_entropy_x_si = sea_eta_entropy_si(sa_si, x_si, pref_si)  'entropy from pot. enthalpy

                Case "h"
                    sea_eta_entropy_x_si = sea_eta_entropy_si(sa_si, x_si, p_si)     'entropy from in-situ enthalpy

                Case "tpot"
                    If pref_si <= 0 Then Exit Function
                    If x_si <= 0 Then Exit Function
                    sea_eta_entropy_x_si = sea_entropy_si(sa_si, x_si, pref_si)  'entropy from pot. temperature

                Case "t"
                    If x_si <= 0 Then Exit Function
                    sea_eta_entropy_x_si = sea_entropy_si(sa_si, x_si, p_si)     'entropy from in-situ temperature

            End Select

        End Function


#End Region

#Region "Sea_3d"

        '#########################################################################

        'This module requires the library modules:
        '     Constants_0_Mdl, file Constants_0.bas
        '     Sal_2_Mdl,     file Sal_2.bas
        '     Flu_3a_Mdl,    file Flu_3a.bas

        '#########################################################################


        'This module computes the properties of seawater as functions of temperature,
        'pressure and density, in particular, the absolute salinity

        'Implementation in VB6 by Rainer Feistel
        'for publication in Ocean Science, as described in the papers

        'Feistel, R., Wright, D.G., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part I: Background and Equations.
        'Ocean Science, 2009

        'Wright, D.G., Feistel, R., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part II: The Library Routines,
        'Ocean Science, 2009


        'Private Const ErrorReturn = 9.99999999E+98

        'Control parameters of the salinity iteration
        'Private ctrl_initialized As Integer

        Private ctrl_mode_salinity As Integer
        'Private ctrl_loop_maximum As Long
        Private ctrl_init_salinity As Double
        Private ctrl_eps_exit_salinity As Double

        'Private Const Version = "19 Aug 2009"  'corrected 21 Jul 2015
        'Private Const Version = "21 Jul 2015"

        Public Function sea_sa_si(ByVal t_si As Double, _
                                ByVal p_si As Double, _
                                ByVal d_si As Double) As Double

            'This function computes the absolute salinity sa_si in kg/kg from
            'the absolute temperature t_si in K, the absolute pressure p_si in Pa
            'and the seawater density d_si in kg/m3

            'check value with default settings:
            'sea_sa_si(273.15, 101325, 1028) = 3.50315257708621E-02

            Dim S As Double, maxit As Integer, eps As Double

            sea_sa_si = ErrorReturn

            If t_si <= 0 Then Exit Function
            If p_si <= 0 Then Exit Function
            If d_si <= 0 Then Exit Function

            init_it_ctrl_salinity()

            Select Case ctrl_mode_salinity
                Case 0 : S = SO_salinity_si
                Case Else : S = ctrl_init_salinity
            End Select

            Select Case ctrl_loop_maximum
                Case 0 : maxit = 100
                Case -1 : sea_sa_si = S
                    Exit Function
                Case Is > 0 : maxit = ctrl_loop_maximum
                Case Else : Exit Function
            End Select

            eps = ctrl_eps_exit_salinity
            If eps = 0 Then Exit Function

            'run the iteration
            sea_sa_si = salinity_iteration(t_si, p_si, d_si, S, maxit, eps)

        End Function


        Private Function salinity_iteration(ByVal t_si As Double, _
                                        ByVal p_si As Double, _
                                        ByVal d_si As Double, _
                                        ByVal S As Double, _
                                        ByVal maxit As Integer, _
                                        ByVal eps As Double) As Double

            Dim ds As Double, dv As Double
            Dim gs_p As Double, gs_sp As Double
            Dim it As Integer

            'this function calculates the absolute salinity s from given values
            'of the absolute temperature, t_si, in K, absolute pressure, p_si,
            'in Pa and the seawater density, d_si, in kg/m3

            'output: salinity_iteration is the result for the absolute salinity in kg/kg

            'input: t_si:  absolute temperature in K
            '       p_si:  absolute pressure in Pa
            '       d_si:  seawater density in kg/m3
            '       s:     initial salinity estimate in kg/kg
            '       maxit: maxiumum number of iteration steps
            '       eps:   salinity tolerance, eps < 0 means relative error

            salinity_iteration = ErrorReturn

            If eps <= 0 Then Exit Function
            If maxit <= 0 Then Exit Function

            If check_limits = 1 Then
                'FLU_LIMITS
                If t_si < flu_tmin Or t_si > flu_tmax Or _
                    d_si < flu_dmin Or d_si > flu_dmax Then Exit Function
                'SAL_LIMITS
                If t_si < sal_tmin Or t_si > sal_tmax Or _
                    p_si < sal_pmin Or p_si > sal_pmax Then Exit Function
            Else
                If t_si <= 0 Or _
                    p_si <= 0 Or _
                    d_si <= 0 Then Exit Function
            End If

            check_limits = check_limits - 1

            'pure water density
            dv = liq_density_si(t_si, p_si)
            If dv = ErrorReturn Or dv = 0 Then Exit Function

            'saline part of the given specific volume
            dv = 1 / d_si - 1 / dv

            For it = 1 To maxit
                'derivatives for Newton iteration
                gs_p = sal_g_si(0, 0, 1, S, t_si, p_si)
                If gs_p = ErrorReturn Then Exit For
                gs_sp = sal_g_si(1, 0, 1, S, t_si, p_si)
                If gs_sp = ErrorReturn Then Exit For
                If gs_sp = 0 Then Exit For

                'compute salinity correction
                ds = (dv - gs_p) / gs_sp
                'update salinity estimate
                S = S + ds
                If S < 0 Or S >= 1 Then Exit For

                'check iteration tolerance
                If (eps > 0 And Abs(ds) < eps) Or _
                    (eps < 0 And Abs(ds) < eps * S) Then
                    salinity_iteration = S
                    Exit For
                End If

            Next it

            check_limits = check_limits + 1

            If check_limits = 1 Then
                If S < sal_smin Or S > sal_smax Then
                    salinity_iteration = ErrorReturn
                End If
            End If

        End Function


        Private Sub init_it_ctrl_salinity()

            If ctrl_initialized = -1 Then Exit Sub

            ctrl_initialized = -1

            'Set default values and modes for salinity iteration
            ctrl_loop_maximum = 100
            ctrl_mode_salinity = 0           'default: sa = SO_salinity_si = 36.16504 g/kg
            ctrl_init_salinity = SO_salinity_si
            ctrl_eps_exit_salinity = 0.0000001  'default = 0.0001 g/kg

        End Sub


        Public Sub set_it_ctrl_salinity(ByVal key As String, ByVal value As Double)

            'this sub sets control parameters for the Newton iteration used to compute
            'potential temperature from reference pressure

            'key             value
            'it_steps        0           set iteration number to default (100)
            'it_steps        n > 0       set iteration number to n
            'it_steps       -1           do not iterate, use initial value

            'init_sa         0           use default sa = 35.16504 g/kg to start
            'init_sa         s > 0       use value s as salinity to start

            'tol_sa          0           use default exit accuracy for salinity (0.1 mg/kg)
            'tol_sa          eps > 0     use eps as exit accuracy for salinity

            init_it_ctrl_salinity()

            Select Case LCase(Trim(key))

                Case "it_steps" 'iteration steps
                    Select Case value
                        Case 0 : ctrl_loop_maximum = 100     'default = 100
                        Case Is < 0 : ctrl_loop_maximum = -1
                        Case Else : ctrl_loop_maximum = value
                    End Select

                Case "init_sa" 'start salinity
                    '   Select Case CLng(value)
                    Select Case value                            'line corrected 21 Jul 2015
                        Case 0 : ctrl_mode_salinity = 0        'default: sa = 35.16504 g/kg
                        Case Is > 0 : ctrl_mode_salinity = 1
                            ctrl_init_salinity = value
                    End Select

                Case "tol_sa" 'required salinity tolerance
                    Select Case value
                        Case 0 : ctrl_eps_exit_salinity = 0.0000001 'default = 0.1 mg/kg
                        Case Is > 0 : ctrl_eps_exit_salinity = value
                    End Select

            End Select

        End Sub


#End Region

#Region "Sea_5a"

        '#########################################################################

        'This module requires the library modules:
        '     Constants_0_Mdl, file Constants_0.bas
        '     Sea_3a_Mdl,      file Sea_3a.bas
        '     Sea_3b_Mdl,      file Sea_3b.bas
        '     Sea_3c_Mdl,      file Sea_3c.bas

        '#########################################################################


        'This module implements seawater properties related to potential temperature
        'and conservative temperature. VB Code adapted from D.R. Jackett and D.G.Wright.

        'Implementation in VB6 by Rainer Feistel
        'for publication in Ocean Science, as described in the papers

        'Feistel, R., Wright, D.G., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part I: Background and Equations.
        'Ocean Science, 2009

        'Wright, D.G., Feistel, R., Jackett, D.R., Miyagawa, K., Reissmann, J.H.,
        'Wagner, W., Overhoff, U., Guder, C., Feistel, A., Marion, G.M.:
        'Numerical implementation and oceanographic application of the thermodynamic
        'potentials of water, vapour, ice, seawater and air. Part II: The Library Routines,
        'Ocean Science, 2009


        'Private Const ErrorReturn = 9.99999999E+98

        Private Const CP0 = 3991.86795711963       'J/(kg K), in concrete 02/12/08
        Private Const p_ref = 101325.0#              'Pa, reference pressure


        Public Function sea_ctmp_from_ptmp0_si(ByVal sa_si As Double, _
                                            ByVal tpot_si As Double) As Double


            '   conservative temperature from potential temperature of seawater
            '
            '   sa_si         : Absolute Salinity                        [kg/kg]
            '   pt0           : potential temperature with               [K]
            '                   reference pressure of 101325 Pa
            '
            '   result        : conservative temperature                 [K]

            'Checkvalue: sea_ctmp_from_ptmp0_si(0.035, 300) =   300.010069445349

            Dim ct As Double

            sea_ctmp_from_ptmp0_si = ErrorReturn

            ct = sea_enthalpy_si(sa_si, tpot_si, p_ref)
            If ct = ErrorReturn Then Exit Function

            sea_ctmp_from_ptmp0_si = 273.15 + ct / CP0

        End Function


        Public Function sea_alpha_ct_si(ByVal sa_si As Double, _
                                    ByVal t_si As Double, _
                                    ByVal p_si As Double) As Double


            ' THERMAL EXPANSION COEFFICIENT OF SEAWATER WRT CONSERVATIVE TEMPERATURE
            '
            ' SA_SI                  : ABSOLUTE SALINITY                  [KG/KG]
            ' T_SI                   : IN SITU TEMPERATURE                [K]
            ' P_SI                   : ABSOLUTE PRESSURE                  [PA]
            '
            ' SEA_ALPHA_CT_SI        : THERMAL EXPANSION COEFFICIENT      [1/K]
            '                          WRT CONSERVATIVE TEMPERATURE


            'check value with default settings:
            'sea_alpha_ct_si(0.035, 300, 1E8) = 3.91772847589188E-04

            Dim ah As Double

            sea_alpha_ct_si = ErrorReturn

            ah = sea_eta_expansion_h_si(sa_si, t_si, p_si, p_ref, "t")
            If ah = ErrorReturn Then Exit Function

            sea_alpha_ct_si = CP0 * ah

        End Function


        Public Function sea_alpha_pt0_si(ByVal sa_si As Double, _
                                        ByVal t_si As Double, _
                                        ByVal p_si As Double) As Double


            ' THERMAL EXPANSION COEFFICIENT OF SEAWATER WRT POTENTIAL TEMPERATURE
            '
            ' SA_SI                  : ABSOLUTE SALINITY                  [KG/KG]
            ' T_SI                   : IN SITU TEMPERATURE                [K]
            ' P_SI                   : ABSOLUTE PRESSURE                  [PA]
            '
            ' SEA_ALPHA_PT0_SI       : THERMAL EXPANSION COEFFICIENT      [1/K]
            '                          WRT POTENTIAL TEMPERATURE

            'check value with default settings:
            'sea_alpha_pt0_si(0.035, 300, 1e8) = 3.92515634063559E-04

            sea_alpha_pt0_si = sea_eta_expansion_theta_si(sa_si, t_si, p_si, p_ref, "t")

        End Function


        Public Function sea_alpha_t_si(ByVal sa_si As Double, _
                                    ByVal t_si As Double, _
                                    ByVal p_si As Double) As Double


            ' THERMAL EXPANSION COEFFICIENT OF SEAWATER WRT IN SITU TEMPERATURE
            '
            ' SA_SI                  : ABSOLUTE SALINITY                  [KG/KG]
            ' T_SI                   : IN SITU TEMPERATURE                [K]
            ' P_SI                   : ABSOLUTE PRESSURE                  [PA]
            '
            ' SEA_ALPHA_T_SI         : THERMAL EXPANSION COEFFICIENT      [1/K]
            '                          WRT IN SITU TEMPERATURE

            'check value with default settings:
            'sea_alpha_t_si(0.035, 300, 1e8) = 3.73608885177539E-04

            sea_alpha_t_si = sea_g_expansion_t_si(sa_si, t_si, p_si)

        End Function


        Public Function sea_beta_ct_si(ByVal sa_si As Double, _
                                    ByVal t_si As Double, _
                                    ByVal p_si As Double) As Double


            ' HALINE CONTRACTION COEFFICIENT OF SEAWATER WRT CONSTANT CONSERVATIVE TEMPERATURE
            '
            ' SA_SI                  : ABSOLUTE SALINITY                  [KG/KG]
            ' T_SI                   : IN SITU TEMPERATURE                [K]
            ' P_SI                   : ABSOLUTE PRESSURE                  [PA]
            '
            ' RESULT                 : HALINE CONTRACTION COEFFICIENT     [KG/KG]
            '                          WRT CONSTANT CONSERVATIVE TEMPERATURE

            'check value with default settings:
            'sea_beta_ct_si(0.035, 300, 1e8) = 0.649596383653744

            sea_beta_ct_si = sea_eta_contraction_h_si(sa_si, t_si, p_si, p_ref, "t")

        End Function


        Public Function sea_beta_pt0_si(ByVal sa_si As Double, _
                                    ByVal t_si As Double, _
                                    ByVal p_si As Double) As Double


            ' HALINE CONTRACTION COEFFICIENT OF SEAWATER WRT CONSTANT POTENTIAL TEMPERATURE
            '
            ' SA_SI                  : ABSOLUTE SALINITY                  [KG/KG]
            ' T_SI                   : IN SITU TEMPERATURE                [K]
            ' P_SI                   : ABSOLUTE PRESSURE                  [PA]
            '
            ' RESULT                 : HALINE CONTRACTION COEFFICIENT     [KG/KG]
            '                          WRT CONSTANT POTENTIAL TEMPERATURE

            'check value with default settings:
            'sea_beta_pt0_si(0.035, 300, 1e8) = 0.663973579411448

            sea_beta_pt0_si = sea_eta_contraction_theta_si(sa_si, t_si, p_si, p_ref, "t")

        End Function


        Public Function sea_beta_t_si(ByVal sa_si As Double, _
                                    ByVal t_si As Double, _
                                    ByVal p_si As Double) As Double


            ' HALINE CONTRACTION COEFFICIENT OF SEAWATER WRT IN SITU TEMPERATURE
            '
            ' SA_SI                  : ABSOLUTE SALINITY                  [KG/KG]
            ' T_SI                   : IN SITU TEMPERATURE                [K]
            ' P_SI                   : ABSOLUTE PRESSURE                  [PA]
            '
            ' RESULT                 : HALINE CONTRACTION COEFFICIENT     [KG/KG]
            '                          WRT CONSTANT IN SITU TEMPERATURE

            'check value with default settings:
            'sea_beta_t_si(0.035, 300, 1e8) = 0.666238827368374

            sea_beta_t_si = sea_g_contraction_t_si(sa_si, t_si, p_si)

        End Function


        Public Function sea_ptmp0_from_ctmp_si(ByVal sa_si As Double, _
                                            ByVal ct_si As Double) As Double


            ' potential temperature of seawater from conservative temperature
            '
            ' sa_si               : Absolute Salinity                  [kg/kg]
            ' ct_si               : conservative temperature           [K]
            '
            ' result              : potential temperature with         [K]
            '                       reference pressure of  101325 Pa

            'Checkvalue: sea_ptmp0_from_ctmp_si(0.035, 0.300010069445E+03) =   299.999999999654

            Dim nloops&, n&
            Dim s1#, ct1#, P0#
            Dim a0#, a1#, a2#, a3#, a4#, a5#, b0#, b1#, b2#, b3#
            Dim a5ct#, b3ct#, ct_factor#, th0_num#, rec_th0_den#
            Dim th0#, ct0#, dth_dct#, theta#, dct#, dct_dth#, factor#

            sea_ptmp0_from_ctmp_si = ErrorReturn

            s1 = sa_si * 35.0# / 35.16504 : ct1 = ct_si - 273.15 : P0 = 101325.0#

            a0 = -0.0144601364634479 : b0 = 1.0#
            a1 = -0.00330530899585292 : b1 = 0.00065060971156358
            a2 = 0.000106241592912898 : b2 = 0.0038302894868509
            a3 = 0.947756667379449 : b3 = 0.00000124781176036803
            a4 = 0.00216659194773661
            a5 = 0.0038288429550399

            a5ct = a5 * ct1 : b3ct = b3 * ct1

            ct_factor = (a3 + a4 * s1 + a5ct)

            th0_num = a0 + s1 * (a1 + a2 * s1) + ct1 * ct_factor

            rec_th0_den = 1.0# / (b0 + b1 * s1 + ct1 * (b2 + b3ct))

            th0 = th0_num * rec_th0_den

            ct0 = sea_ctmp_from_ptmp0_si(sa_si, 273.15 + th0)

            dth_dct = (ct_factor + a5ct - (b2 + b3ct + b3ct) * th0) * rec_th0_den

            theta = th0 - (ct0 - ct_si) * dth_dct


            nloops = 1                  ' default

            '    NOTE: nloops = 1 gives theta with a maximum error of &

            n = 0

            Do While n <= nloops
                factor = sea_ctmp_from_ptmp0_si(sa_si, 273.15 + theta)
                If factor = ErrorReturn Then Exit Function
                dct = factor - ct_si

                factor = sea_g_si(0, 2, 0, sa_si, 273.15 + theta, P0)
                If factor = ErrorReturn Then Exit Function
                dct_dth = -(theta + 273.15) * factor / CP0

                theta = theta - dct / dct_dth
                n = n + 1
            Loop

            sea_ptmp0_from_ctmp_si = theta + 273.15

        End Function


        Public Function sea_cabb_ct_si#(ByVal sa_si#, ByVal t_si#, ByVal p_si#)


            ' cabbeling coefficient of seawater wrt conservative temperature
            '
            ' sa_si                  : Absolute Salinity                  [kg/kg]
            ' t_si                   : in situ temperature                [K]
            ' p_si                   : sea (gauge) pressure               [Pa]
            '
            ' result                 : cabbeling coefficient              [1/(K**2)]
            '                          wrt conservative temperature

            'check value:
            'sea_cabb_ct_si(0.035, 300, 1E5) = 8.612525|67438267E-06  'DEFAULT TOLERANCE
            'sea_cabb_ct_si(0.035, 300, 1E5) = 8.612525|64750277E-06  'TOLERANCE RESET TO -1D-12

            Dim th#, dct#, th_l#, th_u#, t_l#, t_u#, alpha#, beta#, alpha_ct#, ratio#
            Dim ct#, ct_l#, ct_u#, dsa#, sa_l#, sa_u#, alpha_sa#, beta_sa#, factor1#, factor2#

            sea_cabb_ct_si = ErrorReturn

            th = sea_pottemp_si(sa_si, t_si, p_si, p_ref) ' NOTE: ct = const <==> theta = const
            'here, p_ref is commonly specified at the module level
            If th = ErrorReturn Then Exit Function
            ct = sea_ctmp_from_ptmp0_si(sa_si, th)
            If ct = ErrorReturn Then Exit Function

            'ct derivative: The modification made here is to ensure centered differences
            dct = 0.001
            ct_l = ct - dct
            ct_u = ct + dct

            th_l = sea_ptmp0_from_ctmp_si(sa_si, ct_l)
            If th_l = ErrorReturn Then Exit Function
            th_u = sea_ptmp0_from_ctmp_si(sa_si, ct_u)
            If th_u = ErrorReturn Then Exit Function

            t_l = sea_pottemp_si(sa_si, th_l, p_ref, p_si)
            If t_l = ErrorReturn Then Exit Function
            t_u = sea_pottemp_si(sa_si, th_u, p_ref, p_si)
            If t_u = ErrorReturn Then Exit Function

            alpha = sea_alpha_ct_si(sa_si, t_si, p_si)
            If alpha = ErrorReturn Then Exit Function
            beta = sea_beta_ct_si(sa_si, t_si, p_si)
            If beta = ErrorReturn Or beta = 0 Then Exit Function
            ratio = alpha / beta

            factor1 = sea_alpha_ct_si(sa_si, t_u, p_si)
            If (factor1 = ErrorReturn) Then Exit Function
            factor2 = sea_alpha_ct_si(sa_si, t_l, p_si)
            If (factor2 = ErrorReturn) Then Exit Function

            'calculate d(alpha_ct)/dct
            If (ct_u = ct_l) Then Exit Function
            alpha_ct = (factor1 - factor2) / (ct_u - ct_l)

            'sa derivatives

            'dsa = Min(sa_si, 1E-5):
            dsa = IIf(sa_si < 0.00001, sa_si, 0.00001)

            If (sa_si > dsa) Then
                sa_l = sa_si - dsa
                sa_u = sa_si + dsa
            ElseIf (sa_si >= 0.0#) Then
                sa_l = 0.0#
                sa_u = dsa
            Else
                Exit Function
            End If

            If (sa_u = sa_l) Then Exit Function

            factor1 = sea_alpha_ct_si(sa_u, t_si, p_si)
            If (factor1 = ErrorReturn) Then Exit Function
            factor2 = sea_alpha_ct_si(sa_l, t_si, p_si)
            If (factor2 = ErrorReturn) Then Exit Function
            alpha_sa = (factor1 - factor2) / (sa_u - sa_l)

            factor1 = sea_beta_ct_si(sa_u, t_si, p_si)
            If (factor1 = ErrorReturn) Then Exit Function
            factor2 = sea_beta_ct_si(sa_l, t_si, p_si)
            If (factor2 = ErrorReturn) Then Exit Function
            beta_sa = (factor1 - factor2) / (sa_u - sa_l)

            'cabbeling coefficient

            sea_cabb_ct_si = alpha_ct + ratio * (2.0# * alpha_sa - ratio * beta_sa)

        End Function


        Public Function sea_cabb_pt0_si#(ByVal sa_si#, ByVal t_si#, ByVal p_si#)


            ' cabbeling coefficient of seawater wrt potential temperature
            '
            ' sa_si                  : Absolute Salinity                  [kg/kg]
            ' t_si                   : in situ temperature                [K]
            ' p_si                   : sea (gauge) pressure               [Pa]
            '
            ' result                 : cabbeling coefficient              [1/(K**2)]
            '                          wrt potential temperature

            'check value:
            'sea_cabb_pt0_si(0.035, 300, 1E5) = 8.3387453|7690444E-06  'DEFAULT TOLERANCES
            'sea_cabb_pt0_si(0.035, 300, 1E5) = 8.3387453|0126243E-06  'TOLERANCE RESET TO -1D-12

            Dim th#, dth#, th_l#, th_u#, t_l#, t_u#, alpha#, beta#, alpha_pt0#, ratio#
            Dim dsa#, sa_l#, sa_u#, alpha_sa#, beta_sa#, factor1#, factor2#

            sea_cabb_pt0_si = ErrorReturn

            th = sea_pottemp_si(sa_si, t_si, p_si, p_ref)
            If (th = ErrorReturn) Then Exit Function

            'th derivative
            dth = 0.001
            th_l = th - dth
            th_u = th + dth

            t_l = sea_pottemp_si(sa_si, th_l, p_ref, p_si)
            If t_l = ErrorReturn Then Exit Function
            t_u = sea_pottemp_si(sa_si, th_u, p_ref, p_si)
            If t_u = ErrorReturn Or th_u = th_l Then Exit Function

            alpha = sea_alpha_pt0_si(sa_si, t_si, p_si)
            If alpha = ErrorReturn Then Exit Function
            beta = sea_beta_pt0_si(sa_si, t_si, p_si)
            If beta = ErrorReturn Or beta = 0 Then Exit Function
            ratio = alpha / beta

            factor1 = sea_alpha_pt0_si(sa_si, t_u, p_si)
            If (factor1 = ErrorReturn) Then Exit Function
            factor2 = sea_alpha_pt0_si(sa_si, t_l, p_si)
            If (factor2 = ErrorReturn) Then Exit Function
            alpha_pt0 = (factor1 - factor2) / (th_u - th_l)

            'sa derivatives
            'dsa = Min(sa_si, 1E-5):
            dsa = IIf(sa_si < 0.00001, sa_si, 0.00001)
            If (sa_si >= dsa) Then
                sa_l = sa_si - dsa
                sa_u = sa_si + dsa
            ElseIf (sa_si >= 0.0#) Then
                sa_l = 0.0#
                sa_u = dsa
            Else
                Exit Function
            End If
            If sa_u = sa_l Then Exit Function

            factor1 = sea_alpha_pt0_si(sa_u, t_si, p_si)
            If (factor1 = ErrorReturn) Then Exit Function
            factor2 = sea_alpha_pt0_si(sa_l, t_si, p_si)
            If (factor2 = ErrorReturn) Then Exit Function

            alpha_sa = (factor1 - factor2) / (sa_u - sa_l)

            beta_sa = (sea_beta_pt0_si(sa_u, t_si, p_si) - sea_beta_pt0_si(sa_l, t_si, p_si)) / (sa_u - sa_l)

            'cabbeling coefficient
            sea_cabb_pt0_si = alpha_pt0 + ratio * (2.0# * alpha_sa - ratio * beta_sa)

        End Function


        Public Function sea_thrmb_ct_si#(ByVal sa_si#, ByVal t_si#, ByVal p_si#)


            ' thermobaric coefficient of seawater wrt conservative temperature
            '
            ' sa_si                  : Absolute Salinity                  [kg/kg]
            ' t_si                   : in situ temperature                [K]
            ' p_si                   : sea (gauge) pressure               [Pa]
            '
            ' result                 : thermobaric coefficient            [1/(K Pa)]
            '                          wrt conservative temperature

            'check value:
            'sea_thrmb_ct_si(0.035, 300, 1E5) = 1.4810927|1668362E-12  'DEFAULT TOLERANCES
            'sea_thrmb_ct_si(0.035, 300, 1E5) = 1.4810927|5172403E-12  'TOLERANCE RESET TO -1D-12

            Dim theta#, dp#, p_l#, p_u#, t_l#, t_u#, alpha#, beta#
            Dim alpha_l#, alpha_u#, beta_l#, beta_u#

            sea_thrmb_ct_si = ErrorReturn

            dp = 1000.0#

            theta = sea_pottemp_si(sa_si, t_si, p_si, p_ref)
            'here, p_ref is commonly specified at the module level
            If theta = ErrorReturn Then Exit Function

            If (p_si >= dp) Then
                p_l = p_si - dp
                p_u = p_si + dp
            Else
                p_l = 0.0#
                p_u = dp
            End If
            If p_u = p_l Then Exit Function

            t_l = sea_pottemp_si(sa_si, theta, p_ref, p_l) ' ct = const ==> theta = const
            If t_l = ErrorReturn Then Exit Function
            t_u = sea_pottemp_si(sa_si, theta, p_ref, p_u)
            If t_u = ErrorReturn Then Exit Function

            alpha = sea_alpha_ct_si(sa_si, t_si, p_si)
            If alpha = ErrorReturn Then Exit Function

            beta = sea_beta_ct_si(sa_si, t_si, p_si)
            If beta = ErrorReturn Then Exit Function

            alpha_u = sea_alpha_ct_si(sa_si, t_u, p_u)
            If (alpha_u = ErrorReturn) Then Exit Function
            beta_u = sea_beta_ct_si(sa_si, t_u, p_u)
            If (beta_u = ErrorReturn Or beta_u = 0) Then Exit Function

            alpha_l = sea_alpha_ct_si(sa_si, t_l, p_l)
            If (alpha_l = ErrorReturn) Then Exit Function
            beta_l = sea_beta_ct_si(sa_si, t_l, p_l)
            If (beta_l = ErrorReturn Or beta_l = 0) Then Exit Function

            sea_thrmb_ct_si = beta * (alpha_u / beta_u - alpha_l / beta_l) / (p_u - p_l)

        End Function


        Public Function sea_thrmb_pt0_si#(ByVal sa_si#, ByVal t_si#, ByVal p_si#)


            ' thermobaric coefficient of seawater wrt potential temperature
            '
            ' sa_si                  : Absolute Salinity                  [kg/kg]
            ' t_si                   : in situ temperature                [K]
            ' p_si                   : sea (gauge) pressure               [Pa]
            '
            ' result                 : thermobaric coefficient            [1/(K Pa)]
            '                          wrt potential temperature

            'check value:
            'sea_thrmb_pt0_si(0.035, 300, 1E5) = 1.4594101|0702991E-12  'DEFAULT TOLERANCES
            'sea_thrmb_pt0_si(0.035, 300, 1E5) = 1.4594101|3482853E-12  'TOLERANCE RESET TO -1D-12

            Dim theta#, dp#, p_l#, p_u#, t_l#, t_u#, beta#
            Dim alpha_l#, alpha_u#, beta_l#, beta_u#

            sea_thrmb_pt0_si = ErrorReturn

            dp = 1000.0#

            theta = sea_pottemp_si(sa_si, t_si, p_si, p_ref)
            'here, p_ref is commonly specified at the module level
            If theta = ErrorReturn Then Exit Function

            If (p_si >= dp) Then
                p_l = p_si - dp
                p_u = p_si + dp
            Else
                p_l = 0.0#
                p_u = dp
            End If
            If p_u = p_l Then Exit Function

            t_l = sea_pottemp_si(sa_si, theta, p_ref, p_l)
            If (t_l = ErrorReturn) Then Exit Function

            t_u = sea_pottemp_si(sa_si, theta, p_ref, p_u)
            If (t_u = ErrorReturn) Then Exit Function

            beta = sea_beta_pt0_si(sa_si, t_si, p_si)
            If (beta = ErrorReturn) Then Exit Function

            alpha_u = sea_alpha_pt0_si(sa_si, t_u, p_u)
            If (alpha_u = ErrorReturn) Then Exit Function
            beta_u = sea_beta_pt0_si(sa_si, t_u, p_u)
            If (beta_u = ErrorReturn Or beta_u = 0) Then Exit Function

            alpha_l = sea_alpha_pt0_si(sa_si, t_l, p_l)
            If (alpha_l = ErrorReturn) Then Exit Function
            beta_l = sea_beta_pt0_si(sa_si, t_l, p_l)
            If (beta_l = ErrorReturn Or beta_l = 0) Then Exit Function

            sea_thrmb_pt0_si = beta * (alpha_u / beta_u - alpha_l / beta_l) / (p_u - p_l)

        End Function

#End Region

#Region "Extras"

        Public Function sea_viscosity(S As Double, T As Double) As Double

            '% SW_Viscosity    Dynamic viscosity of seawater
            '%=========================================================================
            '% USAGE:  mu = SW_Viscosity(T,uT,S,uS)
            '%
            '% DESCRIPTION:
            '%   Dynamic viscosity of seawater at atmospheric pressure (0.1 MPa) using
            '%   Eq. (22) given in [1] which best fit the data of [2], [3] and [4].
            '%   The pure water viscosity equation is a best fit to the data of [5].
            '%   Values at temperature higher than the normal boiling temperature
            '%   are calculated at the saturation pressure.
            '%
            '% INPUT:
            '%   T  = temperature
            '%   uT = temperature unit
            '%        'C'  : [degree Celsius] (ITS-90)
            '%        'K'  : [Kelvin]
            '%        'F'  : [degree Fahrenheit]
            '%        'R'  : [Rankine]
            '%   S  = salinity
            '%   uS = salinity unit
            '%        'ppt': [g/kg]  (reference-composition salinity)
            '%        'ppm': [mg/kg] (in parts per million)
            '%        'w'  : [kg/kg] (mass fraction)
            '%        '%'  : [kg/kg] (in parts per hundred)
            '%
            '%   Note: T and S must have the same dimensions
            '%
            '% OUTPUT:
            '%   mu = dynamic viscosity [kg/m-s]
            '%
            '%   Note: mu will have the same dimensions as T and S
            '%
            '% VALIDITY: 0 < T < 180 C and 0 < S < 150 g/kg;
            '%
            '% ACCURACY: 1.5%
            '%
            '% REVISION HISTORY:
            '%   2009-12-18: Mostafa H. Sharqawy (mhamed@mit.edu), MIT
            '%               - Initial version
            '%   2012-06-06: Karan H. Mistry (mistry@mit.edu), MIT
            '%               - Allow T,S input in various units
            '%               - Allow T,S to be matrices of any size
            '%
            '% DISCLAIMER:
            '%   This software is provided "as is" without warranty of any kind.
            '%   See the file sw_copy.m for conditions of use and licence.
            '%
            '% REFERENCES:
            '%   [1] M. H. Sharqawy, J. H. Lienhard V, and S. M. Zubair, Desalination
            '%       and Water Treatment, 16, 354-380, 2010. (http://web.mit.edu/seawater/)
            '%   [2] B. M. Fabuss, A. Korosi, and D. F. Othmer, J., Chem. Eng. Data 14(2), 192, 1969.
            '%   [3] J. D. Isdale, C. M. Spence, and J. S. Tudhope, Desalination, 10(4), 319 - 328, 1972
            '%   [4] F. J. Millero, The Sea, Vol. 5, 3 – 80, John Wiley, New York, 1974
            '%   [5] IAPWS release on the viscosity of ordinary water substance 2008
            '%=========================================================================

            T = T - 273.15

            Dim a As Double() = New Double() {0,
                0.15700386464,
                64.99262005,
                -91.296496657,
                0.000042844324477,
                1.540913604,
                0.019981117208,
                -0.000095203865864,
                7.9739318223,
                -0.075614568881,
                0.00047237011074}

            Dim mu_w, A2, B, mu As Double

            mu_w = a(4) + 1 / (a(1) * (T + a(2)) ^ 2 + a(3))

            A2 = a(5) + a(6) * T + a(7) * T ^ 2
            B = a(8) + a(9) * T + a(10) * T ^ 2
            mu = mu_w * (1 + A2 * S + B * S ^ 2)

            Return mu

        End Function

        Public Function sea_thermalcond(S As Double, T As Double) As Double

            '% SW_Conductivity    Thermal conductivity of seawater
            '%=========================================================================
            '% USAGE:  k = SW_Conductivity(T,uT,S,uS)
            '%
            '% DESCRIPTION:
            '%   Thermal conductivity of seawater at 0.1 MPa given by [1]
            '%   Values at temperature higher than the normal boiling temperature
            '%   are calculated at the saturation pressure.
            '%
            '% INPUT:
            '%   T  = temperature
            '%   uT = temperature unit
            '%        'C'  : [degree Celsius] (ITS-90)
            '%        'K'  : [Kelvin]
            '%        'F'  : [degree Fahrenheit]
            '%        'R'  : [Rankine]
            '%   S  = salinity
            '%   uS = salinity unit
            '%        'ppt': [g/kg] (reference-composition salinity)
            '%        'ppm': [mg/kg] (in parts per million)
            '%        'w'  : [kg/kg] (mass fraction)
            '%        '%'  : [kg/kg] (in parts per hundred)
            '%
            '%   Note: T and S must have the same dimensions
            '%
            '% OUTPUT:
            '%   k = thermal conductivity [W/m K]
            '%
            '%   Note: k will have the same dimensions as T and S
            '%
            '% VALIDITY: 0 < T < 180 C; 0 < S < 160 g/kg
            '%
            '% ACCURACY: 3.0%
            '%
            '% REVISION HISTORY:
            '%   2009-12-18: Mostafa H. Sharqawy (mhamed@mit.edu), MIT
            '%               - Initial version
            '%   2012-06-06: Karan H. Mistry (mistry@mit.edu), MIT
            '%               - Allow T,S input in various units
            '%               - Allow T,S to be matrices of any size
            '%
            '% DISCLAIMER:
            '%   This software is provided "as is" without warranty of any kind.
            '%   See the file sw_copy.m for conditions of use and licence.
            '%
            '% REFERENCES:
            '%  [1] D. T. Jamieson, and J. S. Tudhope, Desalination, 8, 393-401, 1970.
            '%=========================================================================

            T = T - 273.15

            S = S * 1000

            T = 1.00024 * T      '%convert from T_90 to T_68
            S = S / 1.00472    '%convert from S to S_P

            Return 10 ^ (Log10(240 + 0.0002 * S) + 0.434 * (2.3 - (343.5 + 0.037 * S) / (T + 273.15)) * (1 - (T + 273.15) / (647.3 + 0.03 * S)) ^ (1 / 3) - 3)

        End Function

        Public Function sea_vaporpressure(S As Double, T As Double) As Double

            '% SW_Psat    Saturation (vapor) pressure of seawater
            '%=========================================================================
            '% USAGE:  Pv = SW_Psat(T,uT,S,uS)
            '%
            '% DESCRIPTION:
            '%   Vapor pressure of seawater given by [1] based on Raoult's law.
            '%   The pure water vapor pressure is given by [2]
            '%
            '% INPUT:
            '%   T  = temperature
            '%   uT = temperature unit
            '%        'C'  : [degree Celsius] (ITS-90)
            '%        'K'  : [Kelvin]
            '%        'F'  : [degree Fahrenheit]
            '%        'R'  : [Rankine]
            '%   S  = salinity
            '%   uS = salinity unit
            '%        'ppt': [g/kg]  (reference-composition salinity)
            '%        'ppm': [mg/kg] (in parts per million)
            '%        'w'  : [kg/kg] (mass fraction)
            '%        '%'  : [kg/kg] (in parts per hundred)
            '%
            '%   Note: T and S must have the same dimensions
            '%
            '% OUTPUT:
            '%   Pv = vapor pressure [N/m^2]
            '%
            '%   Note: Pv will have the same dimensions as T and S
            '%
            '% VALIDITY: 0 < T < 200 C; 0 < S < 240 g/kg
            '%
            '% ACCURACY: 0.1%
            '%
            '% REVISION HISTORY:
            '%   2009-12-18: Mostafa H. Sharqawy (mhamed@mit.edu), MIT
            '%               - Initial version
            '%   2012-06-06: Karan H. Mistry (mistry@mit.edu), MIT
            '%               - Allow T,S input in various units
            '%               - Allow T,S to be matrices of any size
            '%
            '% DISCLAIMER:
            '%   This software is provided "as is" without warranty of any kind.
            '%   See the file sw_copy.m for conditions of use and licence.
            '%
            '% REFERENCES:
            '%   [1] M. H. Sharqawy, J. H. Lienhard V, and S. M. Zubair, Desalination
            '%       and Water Treatment, 16, 354-380, 2010. (http://web.mit.edu/seawater/)
            '%   [2]	ASHRAE handbook: Fundamentals, ASHRAE; 2005.
            '%=========================================================================

            S = S * 1000

            Dim a As Double() = New Double() {0,
                    -5800.2206,
                    1.3914993,
                    -0.048640239,
                    0.000041764768,
                    -0.000000014452093,
                    6.5459673}

            Dim Pv_w As Double = Exp((a(1) / T) + a(2) + a(3) * T + a(4) * T ^ 2 + a(5) * T ^ 3 + a(6) * Log(T))
            Dim Pv As Double = Pv_w / (1 + 0.57357 * (S / (1000 - S)))

            Return Pv

        End Function

        Public Function sea_surfacetension(S As Double, T As Double) As Double

            '% SW_SurfaceTension    Surface tension of seawater
            '%=========================================================================
            '% USAGE:  sigma = SW_SurfaceTension(T,uT,S,uS)
            '%
            '% DESCRIPTION:
            '%   Surface tension of seawater at atmospheric pressure (0.1 MPa) using Eq. (28)
            '%   given by [1] which best fit the data of [2] and [3]. The pure water
            '%   surface tension is given by [4]. Values at temperature higher than
            '%   the normal boiling temperature are calculated at the saturation pressure.
            '%
            '% INPUT:
            '%   T  = temperature
            '%   uT = temperature unit
            '%        'C'  : [degree Celsius] (ITS-90)
            '%        'K'  : [Kelvin]
            '%        'F'  : [degree Fahrenheit]
            '%        'R'  : [Rankine]
            '%   S  = salinity
            '%   uS = salinity unit
            '%        'ppt': [g/kg]  (reference-composition salinity)
            '%        'ppm': [mg/kg] (in parts per million)
            '%        'w'  : [kg/kg] (mass fraction)
            '%        '%'  : [kg/kg] (in parts per hundred)
            '%
            '%   Note: T and S must have the same dimensions
            '%
            '% OUTPUT:
            '%   sigma = surface tension [N/m]
            '%
            '%   Note: sigma will have the same dimensions as T and S
            '%
            '% VALIDITY: 0 < T < 40 C, 0 < S < 40 g/kg
            '%
            '% ACCURACY: 0.18%
            '%
            '% REVISION HISTORY:
            '%   2009-12-18: Mostafa H. Sharqawy (mhamed@mit.edu), MIT
            '%               - Initial version
            '%   2012-06-06: Karan H. Mistry (mistry@mit.edu), MIT
            '%               - Allow T,S input in various units
            '%               - Allow T,S to be matrices of any size
            '%
            '% DISCLAIMER:
            '%   This software is provided "as is" without warranty of any kind.
            '%   See the file sw_copy.m for conditions of use and licence.
            '%
            '% REFERENCES:
            '%   [1] M. H. Sharqawy, J. H. Lienhard V, and S. M. Zubair, Desalination
            '%       and Water Treatment, 16, 354-380, 2010. (http://web.mit.edu/seawater/)
            '%   [2] O. Krummel, Wiss. Meeresunters. der Kieler Komm., Bd. 5, Heft 2, 1900.
            '%   [3] C. Guohua, S. Jingzeng, G. Ling, and Z. Lijun, Study on the  surface tension of seawater,
            '%       Oceanologia et Limnologia Sinica, 25(3), 306-311, 1994.
            '%   [4]	IAPWS, Release on the Surface Tension of Ordinary Water Substance, 1994.
            '%=========================================================================


            T = T - 273.15

            S = S * 1000

            Dim sigma_w As Double = 0.2358 * ((1 - ((T + 273.15) / 647.096)) ^ 1.256) * (1 - 0.625 * (1 - ((T + 273.15) / 647.096)))

            Dim a As Double() = New Double() {0,
                    0.00022637334337,
                    0.0094579521377,
                    0.033104954843}

            Return sigma_w * (1 + (a(1) * T + a(2)) * Log(1 + a(3) * S))

        End Function

#End Region

    End Class

    End Namespace