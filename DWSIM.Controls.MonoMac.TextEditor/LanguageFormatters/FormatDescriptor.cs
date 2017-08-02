using System;
using System.Collections.Generic;
using MonoMac.Foundation;
using MonoMac.AppKit;

namespace MonoMac.AppKit.TextKit.Formatter
{
	/// <summary>
	/// The <see cref="AppKit.TextKit.Formatter.FormatDescriptor"/> defines a language formmating instruction
	/// that can be used by a <see cref="AppKit.TextKit.Formatter.LanguageDescriptor"/> to define syntax
	/// highlighting to a <see cref="AppKit.TextKit.Formatter.LanguageFormatter"/>.
	/// </summary>
	public class FormatDescriptor : NSObject
	{
		#region Computed Properties
		/// <summary>
		/// Gets or sets the <c>FormatDescriptorType</c> for this format descriptor.
		/// </summary>
		/// <value>The <c>FormatDescriptorType</c>.</value>
		public FormatDescriptorType Type { get; set; } = FormatDescriptorType.Prefix;

		/// <summary>
		/// Gets or sets the forground color that text matching this format will be set to.
		/// </summary>
		/// <value>The <c>NSColor</c>.</value>
		public NSColor Color { get; set;} = NSColor.Gray;

		/// <summary>
		/// Gets or sets the character sequence that this format starts with.
		/// </summary>
		/// <value>The starting <c>string</c> sequence.</value>
		public string StartsWith { get; set; } = "";

		/// <summary>
		/// Gets or sets the character sequence that text matching this format ends with.
		/// </summary>
		/// <value>The ending <c>string</c> sequence.</value>
		/// <remarks>This value will be an empty string ("") if the <c>Type</c> is a <c>Prefix</c> format.</remarks>
		public string EndsWith { get; set; } = "";

		/// <summary>
		/// Gets or sets the index of the last matching character within either the <c>StartsWith</c> or
		/// <c>EndsWith</c> based on the state of the <c>Active</c> property.
		/// </summary>
		/// <value>The index of the char.</value>
		/// <remarks>This value should ONLY be changed by the <see cref="AppKit.TextKit.Formatter.LanguageFormatter"/>.</remarks>
		public int CharIndex { get; set; } = 0;

		/// <summary>
		/// Gets or sets if this format has been "activated" (if the matching <c>StartsWith</c> character sequence
		/// has been found).
		/// </summary>
		/// <value><c>true</c> if the matching <c>StartsWith</c> character sequence
		/// has been found; otherwise, <c>false</c>.</value>
		/// <remarks>This value should ONLY be changed by the <see cref="AppKit.TextKit.Formatter.LanguageFormatter"/>.</remarks>
		public bool Active { get; set; } = false;

		/// <summary>
		/// Gets a value indicating whether this <see cref="AppKit.TextKit.Formatter.FormatDescriptor"/> is "triggered"
		/// (all of the <c>StartsWith</c> or <c>EndsWith</c> characters have been found based on the <c>Active</c>
		/// property).
		/// </summary>
		/// <value><c>true</c> if triggered; otherwise, <c>false</c>.</value>
		public bool Triggered {
			get {
				if (Active) {
					return (CharIndex > (EndsWith.Length - 1));
				} else {
					return (CharIndex > (StartsWith.Length - 1));
				}
			}
		}
		#endregion

		#region Constructors
		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.FormatDescriptor"/> class.
		/// </summary>
		/// <param name="startsWith">The starting character sequence for this format.</param>
		/// <param name="color">The <c>NSColor</c> that text in this sequence will be set too.</param>
		/// <remarks>The <c>type</c> will automatically be set to <c>Prefix</c>.</remarks>
		public FormatDescriptor (string startsWith, NSColor color)
		{

			// Initilize
			this.Type = FormatDescriptorType.Prefix;
			this.StartsWith = startsWith;
			this.Color = color;
		}

		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.FormatDescriptor"/> class.
		/// </summary>
		/// <param name="startsWith">The starting character sequence for this format.</param>
		/// <param name="endsWith">The ending character sequence for this format.</param>
		/// <param name="color">The <c>NSColor</c> that text in this sequence will be set too.</param>
		/// <remarks>The <c>type</c> will automatically be set to <c>Enclosure</c>.</remarks>
		public FormatDescriptor (string startsWith, string endsWith, NSColor color)
		{

			// Initilize
			this.Type = FormatDescriptorType.Enclosure;
			this.StartsWith = startsWith;
			this.EndsWith = endsWith;
			this.Color = color;
		}
		#endregion

		#region Public Methods
		/// <summary>
		/// Tests to see if the passed in character matches the character at <c>CharIndex</c> of
		/// either the <c>StartsWith</c> or <c>EndsWith</c> character sequence based on the state
		/// of the <c>Active</c> property.
		/// </summary>
		/// <returns><c>true</c>, if character was matched, <c>false</c> otherwise.</returns>
		/// <param name="c">The character being tested.</param>
		public bool MatchesCharacter(char c) {
			var matches = false;

			// Is this format currently active?
			if (Active) {
				matches = (c == EndsWith [CharIndex]);
			} else {
				matches = (c == StartsWith [CharIndex]);
			}

			// Increment
			if (matches) {
				++CharIndex;
			}

			return matches;
		}
		#endregion
	}
}

