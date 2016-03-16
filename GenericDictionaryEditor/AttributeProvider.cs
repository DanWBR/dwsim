using System.ComponentModel;
using System;

namespace Wexman.Design
{
    /// <summary>
    /// Provides attributes for the Keys or Values in the dictionary.
    /// </summary>
    public abstract class AttributeProvider
    {
        /// <summary>
        /// Returns a collection of attributes for the specified type. 
        /// </summary>
        /// <param name="type">The type of the key or value to provide attributes for.</param>
        /// <returns>An <see cref="AttributeCollection"/> with the attributes for the component.</returns>
        public abstract AttributeCollection GetAttributes(Type type);
    }
}
