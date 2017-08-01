/// ------------------------------------------------------
/// RandomOps - (Pseudo) Random Number Generator For C#
/// Copyright (C) 2003-2010 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// RandomOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Collections.Generic;

namespace RandomOps
{
    /// <summary>
    /// Random Number Generator (RNG) that downloads random bytes from the internet
    /// website: www.random.org. Not thread-safe.
    /// These random bytes are generated from atmospheric noise picked up by radios.
    /// Only a certain number of bits are allowed to be downloaded from this website
    /// to your IP address each day, so this RNG is used with a Fallback-PRNG.
    /// </summary>
    public class RandomDotOrg : ByteStream
    {
        #region Constructor.
        /// <summary>
        /// Constructs the RNG-object.
        /// </summary>
        /// <param name="bufferSize">Number of random bytes the buffer holds.</param>
        /// <param name="randFallback">Fallback RNG to be used when buffer is empty.</param>
        /// <param name="numFallback">Use fallback RNG for this many bytes before trying to fill buffer again.</param>
        public RandomDotOrg(int bufferSize, Random randFallback, int numFallback)
            : base(bufferSize, randFallback, numFallback)
        {
        }
        #endregion

        #region Download random bytes from internet.
        /// <summary>
        /// Do the actual retrieval of random bytes from the www.random.org website.
        /// </summary>
        /// <param name="length">Number of bytes to retrieve (max 10000).</param>
        /// <returns>List of random bytes.</returns>
        /// <exception cref="WebException">
        /// Thrown if daily download quota is depleted,
        /// the HTTP response code is 503 for 'Server Unavailable'.
        /// </exception>
        public static List<byte> RetrieveBytes(int length)
        {
            List<byte> bytes = new List<byte>(length);

            string url = @"http://www.random.org/integers/?num=" + length + @"&min=0&max=255&col=1&base=10&format=plain&rnd=new";
            string data = HtmlDownload.HtmlToString(url);

            string[] dataArray = data.Split('\n');

            foreach (string s in dataArray)
            {
                if (s != null && s != "")
                {
                    byte b;

                    if (System.Byte.TryParse(s, out b))
                    {
                        bytes.Add(b);
                    }
                }
            }

            return bytes;
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the RNG.
        /// </summary>
        public override string Name
        {
            get { return "Random.Org Sync"; }
        }

        /// <summary>
        /// The www.random.org website only allows 10000 elements
        /// to be retrieved at once.
        /// </summary>
        protected override int MaxRetrieveLength
        {
            get { return 10000; }
        }

        /// <summary>
        /// Called from ByteStream to request the retrieval of random bytes.
        /// </summary>
        /// <param name="length">The number of bytes requested.</param>
        protected sealed override void DoFillBuffer(int length)
        {
            // Retrieve the bytes.
            IEnumerable<byte> bytes = RetrieveBytes(length);

            // Add the bytes to the ByteStream-buffer.
            base.AddBuffer(bytes);
        }
        #endregion
    }
}
