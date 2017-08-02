using System;

namespace MonoMac.AppKit.TextKit.Formatter
{
	/// <summary>
	/// Defines the type of a <see cref="AppKit.TextKit.Formatter.FormatDescriptor"/>.
	/// </summary>
	public enum FormatDescriptorType
	{
		/// <summary>
		/// Defines a format that starts with a given character sequence and runs to
		/// the end of the line.
		/// </summary>
		Prefix,

		/// <summary>
		/// Defines a format that is enclosed between a starting and ending character
		/// sequence.
		/// </summary>
		Enclosure
	}
}

