Imports System.Net.Http
Imports Newtonsoft.Json

Public Class WeatherProvider

    Implements IWeatherProvider

    Sub New()

    End Sub

    Public Function GetCurrentWeather(latitude As Double, longitude As Double) As IWeatherData Implements IWeatherProvider.GetCurrentWeather

        Dim getdata = GetWeather(latitude, longitude)
        getdata.Wait()

        Dim result = getdata.Result.results(0)

        Dim formattedresult As New WeatherData

        With formattedresult
            .AtmosphericPressure_Pa = result.pressure.value / 1000.0 * 101325
            Select Case result.phrase.ToLower()
                Case "cloudy"
                    .CurrentCondition = WeatherCondition.Cloudy
                Case "clear", "sunny"
                    .CurrentCondition = WeatherCondition.Sunny
                Case "rainy", "light rain", "rain"
                    .CurrentCondition = WeatherCondition.Rainy
            End Select
            .Latitude = latitude
            .Longitude = longitude
            .RelativeHumidity_pct = result.relativeHumidity
            .Temperature_C = result.temperature.value
            .WindSpeed_km_h = result.wind.speed.value
        End With

        Return formattedresult

    End Function

    Private Async Function GetWeather(latitude As Double, longitude As Double) As Task(Of WeatherResult)

        Using client = New HttpClient()

            Using request = New HttpRequestMessage()

                request.Method = HttpMethod.Get
                request.RequestUri = New Uri(String.Format("https://dwsimwebserver.azurewebsites.net/getweather?latitude={0}&longitude={1}", latitude, longitude))

                Dim response As HttpResponseMessage = Await client.SendAsync(request).ConfigureAwait(False)
                Dim result As String = Await response.Content.ReadAsStringAsync()
                Return JsonConvert.DeserializeObject(Of WeatherResult)(result)

            End Using

        End Using

    End Function

End Class

Public Class Temperature
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class RealFeelTemperature
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class RealFeelTemperatureShade
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class DewPoint
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class Direction
    Public Property degrees As Double
    Public Property localizedDescription As String
End Class

Public Class Speed
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class Wind
    Public Property direction As Direction
    Public Property speed As Speed
End Class

Public Class WindGust
    Public Property speed As Speed
End Class

Public Class Visibility
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class Ceiling
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class Pressure
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class PressureTendency
    Public Property localizedDescription As String
    Public Property code As String
End Class

Public Class Past24HourTemperatureDeparture
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class ApparentTemperature
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class WindChillTemperature
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class WetBulbTemperature
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class PastHour
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class Past3Hours
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class Past6Hours
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class Past9Hours
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class Past12Hours
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class Past18Hours
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class Past24Hours
    Public Property value As Double
    Public Property unit As String
    Public Property unitType As Integer
End Class

Public Class PrecipitationSummary
    Public Property pastHour As PastHour
    Public Property past3Hours As Past3Hours
    Public Property past6Hours As Past6Hours
    Public Property past9Hours As Past9Hours
    Public Property past12Hours As Past12Hours
    Public Property past18Hours As Past18Hours
    Public Property past24Hours As Past24Hours
End Class

Public Class TemperatureSummary
    Public Property past6Hours As Past6Hours
    Public Property past12Hours As Past12Hours
    Public Property past24Hours As Past24Hours
End Class

Public Class Result
    Public Property dateTime As DateTime
    Public Property phrase As String
    Public Property iconCode As Integer
    Public Property hasPrecipitation As Boolean
    Public Property isDayTime As Boolean
    Public Property temperature As Temperature
    Public Property realFeelTemperature As RealFeelTemperature
    Public Property realFeelTemperatureShade As RealFeelTemperatureShade
    Public Property relativeHumidity As Integer
    Public Property dewPoint As DewPoint
    Public Property wind As Wind
    Public Property windGust As WindGust
    Public Property uvIndex As Integer
    Public Property uvIndexPhrase As String
    Public Property visibility As Visibility
    Public Property obstructionsToVisibility As String
    Public Property cloudCover As Integer
    Public Property ceiling As Ceiling
    Public Property pressure As Pressure
    Public Property pressureTendency As PressureTendency
    Public Property past24HourTemperatureDeparture As Past24HourTemperatureDeparture
    Public Property apparentTemperature As ApparentTemperature
    Public Property windChillTemperature As WindChillTemperature
    Public Property wetBulbTemperature As WetBulbTemperature
    Public Property precipitationSummary As PrecipitationSummary
    Public Property temperatureSummary As TemperatureSummary
End Class

Public Class WeatherResult
    Public Property results As Result()

End Class
