using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.AutomaticTranslation
{
    public class TranslationRoutine
    {

        private static readonly string subscriptionKey = "";
        private static readonly string endpoint = "https://api.cognitive.microsofttranslator.com/";

        private static readonly string location = "eastus";

        public static async Task<List<TranslationResult>> TranslateStrings(List<TranslationItem> items, string language)
        {
            // Input and output languages are defined as parameters.
            string route = "/translate?api-version=3.0&to=" + language;
            var requestBody = JsonConvert.SerializeObject(items);

            using (var client = new HttpClient())
            using (var request = new HttpRequestMessage())
            {
                // Build the request.
                request.Method = HttpMethod.Post;
                request.RequestUri = new Uri(endpoint + route);
                request.Content = new StringContent(requestBody, Encoding.UTF8, "application/json");
                request.Headers.Add("Ocp-Apim-Subscription-Key", subscriptionKey);
                request.Headers.Add("Ocp-Apim-Subscription-Region", location);

                // Send the request and get response.
                HttpResponseMessage response = await client.SendAsync(request).ConfigureAwait(false);
                // Read response as a string.
                string result = await response.Content.ReadAsStringAsync();

                return JsonConvert.DeserializeObject<List<TranslationResult>>(result);

            }
        }

        public static async Task<string> TranslateString(string text, string language)
        {
            // Input and output languages are defined as parameters.
            string route = "/translate?api-version=3.0&to=" + language;
            object[] body = new object[] { new { Text = text } };
            var requestBody = JsonConvert.SerializeObject(body);

            using (var client = new HttpClient())
            using (var request = new HttpRequestMessage())
            {
                // Build the request.
                request.Method = HttpMethod.Post;
                request.RequestUri = new Uri(endpoint + route);
                request.Content = new StringContent(requestBody, Encoding.UTF8, "application/json");
                request.Headers.Add("Ocp-Apim-Subscription-Key", subscriptionKey);
                request.Headers.Add("Ocp-Apim-Subscription-Region", location);

                // Send the request and get response.
                HttpResponseMessage response = await client.SendAsync(request).ConfigureAwait(false);
                // Read response as a string.
                string result = await response.Content.ReadAsStringAsync();

                try
                {
                    return JsonConvert.DeserializeObject<List<TranslationResult>>(result)[0].translations[0].text;
                }
                catch
                {
                    return text;
                }

            }
        }
    }
}
