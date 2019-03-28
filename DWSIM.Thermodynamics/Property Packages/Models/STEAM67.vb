Namespace PropertyPackages.Auxiliary

    <System.Serializable()> Public Class STEAM67

        Declare Function steam67 Lib "steam67.dll" (ByRef temperature As Double, ByRef pressure As Double, ByRef quality As Double, ByRef weight As Double, ByRef enthalpy As Double, ByRef entropy As Double, ByRef saturation_temperature As Double, ByRef saturation_pressure As Double, ByRef degrees_superheat As Double, ByRef degrees_subcooling As Double, ByRef viscosity As Double, ByRef critical_velocity As Double, ByVal action As Integer) As Integer

        Function flash67(ByRef temperature As Double, ByRef pressure As Double, ByRef quality As Double, ByRef enthalpy As Double, ByRef entropy As Double) As Object
            'Function returns array of 12 doubles

            'Dim temperature As Double      '      1
            'Dim pressure As Double         '      2
            'Dim quality As Double          '      3
            Dim weight As Double '                 4
            'Dim enthalpy As Double          '     5
            'Dim entropy As Double           '     6
            Dim saturation_temperature As Double ' 7
            Dim saturation_pressure As Double '    8
            Dim degrees_superheat As Double '      9
            Dim degrees_subcooling As Double '    10
            Dim viscosity As Double '             11
            Dim critical_velocity As Double '     12
            Dim action As Integer
            Dim iret As Integer

            action = 2 'viscosity=1, crit_vel=2

            Try
                iret = steam67(temperature, pressure, quality, weight, enthalpy, entropy, saturation_temperature, saturation_pressure, degrees_superheat, degrees_subcooling, viscosity, critical_velocity, action)
            Catch ex As Exception
                Return New Object() {temperature, pressure, quality, weight, enthalpy, entropy, saturation_temperature, saturation_pressure, degrees_superheat, degrees_subcooling, viscosity, critical_velocity}
            End Try
            'Function returns array of 12 doubles
            '(a) Highlight 12 vert cells, (b) enter "=transpose(flash67(t,p,q,h,s))", (c) press CTRL-SHIFT-ENTER
            flash67 = New Object() {temperature, pressure, quality, weight, enthalpy, entropy, saturation_temperature, saturation_pressure, degrees_superheat, degrees_subcooling, viscosity, critical_velocity}

        End Function

        'Flash67	

        'Input	

        'Temperature, F	
        'Pressure, psia	
        'Quality, 0=L, 100=V	
        'Spec volume	
        'Enthalpy
        'Entropy

        'Output	

        'Temperature, F	
        'Pressure, psia	
        'Quality, 0=L, 100=V	
        'Spec volume, ft3/lb	
        'Enthalpy, btu/lb
        'Entropy, btu/lb/f	
        'Tsat, F	
        'Psat, psia	
        'Deg superheat, F	
        'Deg subcool, F
        'Viscosity, cP 
        'Crit velocity	
        'Density, lb/ft3	
        'SG	


    End Class

End Namespace
