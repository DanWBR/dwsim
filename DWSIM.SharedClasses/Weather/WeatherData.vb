Public Class WeatherData

    Implements Interfaces.IWeatherData

    Public Property CurrentConditon As Interfaces.WeatherCondition = WeatherCondition.Sunny Implements IWeatherData.CurrentConditon

    Public Property WindSpeed_km_h As Double = 5 Implements IWeatherData.WindSpeed_km_h

    Public Property RelativeHumidity_pct As Double = 30 Implements IWeatherData.RelativeHumidity_pct

    Public Property Temperature_C As Double = 30 Implements IWeatherData.Temperature_C

    Public Property SolarIrradiation_kWh_m2 As Double = 4 Implements IWeatherData.SolarIrradiation_kWh_m2

    Public Property Latitude As Double Implements IWeatherData.Latitude

    Public Property Longitude As Double Implements IWeatherData.Longitude

End Class
