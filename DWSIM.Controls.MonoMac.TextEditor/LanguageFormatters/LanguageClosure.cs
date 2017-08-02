using System;
using System.Collections.Generic;
using MonoMac.Foundation;
using MonoMac.AppKit;

namespace MonoMac.AppKit.TextKit.Formatter
{
	/// <summary>
	/// The <see cref="AppKit.TextKit.Formatter.LanguageClosure"/> class holds information about a closure such as 
	/// (), [], "" or '' that can be used to define a section of text in a <see cref="AppKit.TextKit.Formatter.LanguageDescriptor"/>.
	/// The Language Closure is used by auto complete in a <see cref="AppKit.TextKit.Formatter.SourceTextView"/>.
	/// </summary>
	public class LanguageClosure : NSObject
	{
		#region Computed Properties
		/// <summary>
		/// Gets or sets the starting character for this closure.
		/// </summary>
		/// <value>The starting character.</value>
		public char StartingCharacter { get; set; }

		/// <summary>
		/// Gets or sets the ending character for this closure.
		/// </summary>
		/// <value>The ending character.</value>
		public char EndingCharacter { get; set; }
		#endregion

		#region Constructors
		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.LanguageClosure"/> class.
		/// </summary>
		public LanguageClosure ()
		{
		}

		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.LanguageClosure"/> class.
		/// </summary>
		/// <param name="character">The character that both starts and ends this closure.</param>
		public LanguageClosure (char character)
		{
			// Initialize
			this.StartingCharacter = character;
			this.EndingCharacter = character;
		}

		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.LanguageClosure"/> class.
		/// </summary>
		/// <param name="startingCharacter">The character that starts the closure.</param>
		/// <param name="endingCharacter">The character that ends the closure.</param>
		public LanguageClosure (char startingCharacter, char endingCharacter)
		{
			// Initialize
			this.StartingCharacter = startingCharacter;
			this.EndingCharacter = endingCharacter;
		}
		#endregion
	}
}

