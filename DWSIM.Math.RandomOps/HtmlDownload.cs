/// ------------------------------------------------------
/// RandomOps - (Pseudo) Random Number Generator For C#
/// Copyright (C) 2003-2010 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// RandomOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Net;
using System.IO;

namespace RandomOps
{
    /// <summary>
    /// Methods for downloading a web-page from the internet.
    /// </summary>
    static class HtmlDownload
    {
        /// <summary>
        /// Download webpage from given url-address to a StreamReader object.
        /// </summary>
        /// <param name="url">Internet address of web-page to download.</param>
        /// <returns>StreamReader-object with web-page contents.</returns>
        public static StreamReader HtmlToStream(string url)
        {
            WebRequest webRequest = WebRequest.Create(url);
            WebResponse webResponse = webRequest.GetResponse();
            StreamReader streamReader = new StreamReader(webResponse.GetResponseStream());

            return streamReader;
        }

        /// <summary>
        /// Download webpage from given url-address to a string.
        /// </summary>
        /// <param name="url">Internet address of web-page to download.</param>
        /// <returns>String with web-page contents.</returns>
        public static string HtmlToString(string url)
        {
            return HtmlToStream(url).ReadToEnd();
        }
    }
}
