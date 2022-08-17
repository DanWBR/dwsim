Public Interface IWeatherProvider

    Function GetCurrentWeather(latitude As Double, longitude As Double) As IWeatherData


End Interface

Public Interface IWeatherData

    Property CurrentCondition As WeatherCondition

    Property WindSpeed_km_h As Double

    Property RelativeHumidity_pct As Double

    Property Temperature_C As Double

    Property SolarIrradiation_kWh_m2 As Double

    Property AtmosphericPressure_Pa As Double

    Property Latitude As Double

    Property Longitude As Double

End Interface

Public Enum WeatherCondition

    Sunny = 0
    Cloudy = 1
    Rainy = 2
    Night = 3

End Enum