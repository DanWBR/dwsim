Public Class WeatherData

    Implements Interfaces.IWeatherData, ICustomXMLSerialization

    Public Property CurrentCondition As Interfaces.WeatherCondition = WeatherCondition.Sunny Implements IWeatherData.CurrentCondition

    Public Property WindSpeed_km_h As Double = 5 Implements IWeatherData.WindSpeed_km_h

    Public Property RelativeHumidity_pct As Double = 30 Implements IWeatherData.RelativeHumidity_pct

    Public Property Temperature_C As Double = 30 Implements IWeatherData.Temperature_C

    Public Property SolarIrradiation_kWh_m2 As Double = 1.0 Implements IWeatherData.SolarIrradiation_kWh_m2

    Public Property Latitude As Double Implements IWeatherData.Latitude

    Public Property Longitude As Double Implements IWeatherData.Longitude

    Public Property AtmosphericPressure_Pa As Double = 101325 Implements IWeatherData.AtmosphericPressure_Pa

    Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData

        Return XMLSerializer.XMLSerializer.Serialize(Me)

    End Function

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData

        Return XMLSerializer.XMLSerializer.Deserialize(Me, data)

    End Function

End Class
