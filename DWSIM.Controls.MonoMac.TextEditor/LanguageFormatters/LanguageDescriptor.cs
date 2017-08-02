using System;
using System.Collections.Generic;
using MonoMac.Foundation;
using MonoMac.AppKit;

using nfloat = System.Double;
using nint = System.Int64;
using nuint = System.UInt64;

namespace MonoMac.AppKit.TextKit.Formatter
{
	/// <summary>
	/// The <see cref="AppKit.TextKit.Formatter.LanguageDescriptor"/> class defines how a 
	/// <see cref="AppKit.TextKit.Formatter.LanguageFormatter"/> should syntax highlight text for a given
	/// language such as C#, JavaScript or Visual Basic.
	/// </summary>
	[Register("LanguageDescriptor")]
 	public class LanguageDescriptor : NSObject
	{
		#region Computed Properties
		/// <summary>
		/// Gets the language identifier.
		/// </summary>
		/// <value>The language identifier.</value>
		public virtual string LanguageIdentifier {
			get { return "Default"; }
		}

		/// <summary>
		/// Gets or sets the language separators that can be used to define a "word" in the
		/// given language.
		/// </summary>
		/// <value>The language separators.</value>
		public char[] LanguageSeparators = new char[]{'.'};

		/// <summary>
		/// Gets the escape character for the given language.
		/// </summary>
		/// <value>The escape character.</value>
		public virtual char EscapeCharacter {
			get { return '\\';}
		}

		/// <summary>
		/// Gets or sets the collection of <see cref="AppKit.TextKit.Formatter.KeywordDescriptor"/> used to define
		/// the keywords for this language.
		/// </summary>
		/// <value>The keywords.</value>
		public Dictionary<string, KeywordDescriptor> Keywords  = new Dictionary<string, KeywordDescriptor>();

		/// <summary>
		/// Gets or sets the collection of <see cref="AppKit.TextKit.Formatter.FormatDescriptor"/> formats used to
		/// syntax highlight this language.
		/// </summary>
		/// <value>The formats.</value>
		public List<FormatDescriptor> Formats = new List<FormatDescriptor>();

		/// <summary>
		/// Gets or sets the collection of <see cref="AppKit.TextKit.Formatter.LanguageClosure"/> used to auto
		/// complete to closure of text such as (), [], "" or ''.
		/// </summary>
		/// <value>The closures.</value>
		public List<LanguageClosure> Closures = new List<LanguageClosure>();

		/// <summary>
		/// Gets or sets the formatting commands that can be added to the user interface for the 
		/// user to select and apply to a selection of text in the editor.
		/// </summary>
		/// <value>The <see cref="AppKit.TextKit.Formatter.LanguageFormatCommand"/> items.</value>
		public List<LanguageFormatCommand> FormattingCommands = new List<LanguageFormatCommand>();

		/// <summary>
		/// Gets or sets the color of generic keywords.
		/// </summary>
		/// <value>The <c>NSColor</c> of the keyword.</value>
		[Export("KeywordColor")]
		public NSColor KeywordColor {
			get { return LoadColor("KeywordColor", NSColor.FromDeviceRgba(0.06f, 0.52f, 0.50f, 1.0f)); }
			set {
				WillChangeValue ("KeywordColor");
				SaveColor ("KeywordColor", value, true);
				DidChangeValue ("KeywordColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of generic keyword type.
		/// </summary>
		/// <value>The <c>NSColor</c> of the type.</value>
		[Export("TypeColor")]
		public NSColor TypeColor {
			get { return LoadColor ("TypeColor", NSColor.FromDeviceRgba (0.06f, 0.52f, 0.50f, 1.0f)); }
			set {
				WillChangeValue ("TypeColor");
				SaveColor ("TypeColor", value, true);
				DidChangeValue ("TypeColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a value type keyword.
		/// </summary>
		/// <value>The <c>NSColor</c> of the value type.</value>
		[Export("ValueTypeColor")]
		public NSColor ValueTypeColor {
			get { return LoadColor ("ValueTypeColor", NSColor.Blue); }
			set {
				WillChangeValue ("ValueTypeColor");
				SaveColor ("ValueTypeColor", value, true);
				DidChangeValue ("ValueTypeColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a reference type keyword.
		/// </summary>
		/// <value>The <c>NSColor</c> of the reference type.</value>
		[Export("ReferenceTypeColor")]
		public NSColor ReferenceTypeColor {
			get { return LoadColor ("ReferenceTypeColor", NSColor.FromDeviceRgba (0f, 0.56f, 0.80f, 1.0f)); }
			set {
				WillChangeValue ("ReferenceTypeColor");
				SaveColor ("ReferenceTypeColor", value, true);
				DidChangeValue ("ReferenceTypeColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a access modifier keyword.
		/// </summary>
		/// <value>The <c>NSColor</c> of the access modifier.</value>
		[Export("AccessModifierColor")]
		public NSColor AccessModifierColor {
			get { return LoadColor ("AccessModifierColor", NSColor.FromDeviceRgba (0.06f, 0.52f, 0.50f, 1.0f)); }
			set {
				WillChangeValue ("AccessModifierColor");
				SaveColor ("AccessModifierColor", value, true);
				DidChangeValue ("AccessModifierColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a generic modifier keyword.
		/// </summary>
		/// <value>The <c>NSColor</c> of the modifier.</value>
		[Export("ModifierColor")]
		public NSColor ModifierColor {
			get { return LoadColor ("ModifierColor", NSColor.FromDeviceRgba (0.06f, 0.52f, 0.50f, 1.0f)); }
			set {
				WillChangeValue ("ModifierColor");
				SaveColor ("ModifierColor", value, true);
				DidChangeValue ("ModifierColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a selection statement keyword.
		/// </summary>
		/// <value>The <c>NSColor</c> of the selection statement.</value>
		[Export("SelectionStatementColor")]
		public NSColor SelectionStatementColor {
			get { return LoadColor ("SelectionStatementColor", NSColor.FromDeviceRgba(0.50f, 0.25f, 0f, 1.0f)); }
			set {
				WillChangeValue ("SelectionStatementColor");
				SaveColor ("SelectionStatementColor", value, true);
				DidChangeValue ("SelectionStatementColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a iteration statement keyword.
		/// </summary>
		/// <value>The <c>NSColor</c> of the iteration statement.</value>
		[Export("IterationStatementColor")]
		public NSColor IterationStatementColor {
			get { return LoadColor ("IterationStatementColor", NSColor.FromDeviceRgba(0.50f, 0f, 0f, 1.0f)); }
			set {
				WillChangeValue ("IterationStatementColor");
				SaveColor ("IterationStatementColor", value, true);
				DidChangeValue ("IterationStatementColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a jump statement keyword.
		/// </summary>
		/// <value>The <c>NSColor</c> of the jump statement.</value>
		[Export("JumpStatementColor")]
		public NSColor JumpStatementColor {
			get { return LoadColor ("JumpStatementColor", NSColor.FromDeviceRgba(0.50f, 0.50f, 0.0f, 1.0f)); }
			set {
				WillChangeValue ("JumpStatementColor");
				SaveColor ("JumpStatementColor", value, true);
				DidChangeValue ("JumpStatementColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a exception handling keyword.
		/// </summary>
		/// <value>The <c>NSColor</c> of the exception handling.</value>
		[Export("ExceptionHandlingColor")]
		public NSColor ExceptionHandlingColor {
			get { return LoadColor ("ExceptionHandlingColor", NSColor.FromDeviceRgba(1f, 0f, 0f, 1.0f)); }
			set {
				WillChangeValue ("ExceptionHandlingColor");
				SaveColor ("ExceptionHandlingColor", value, true);
				DidChangeValue ("ExceptionHandlingColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a generic statement keyword.
		/// </summary>
		/// <value>The <c>NSColor</c> of the statement.</value>
		[Export("StatementColor")]
		public NSColor StatementColor {
			get { return LoadColor ("StatementColor", NSColor.FromDeviceRgba(1f, 0f, 0.50f, 1.0f)); }
			set {
				WillChangeValue ("StatementColor");
				SaveColor ("StatementColor", value, true);
				DidChangeValue ("StatementColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a method parameter keyword.
		/// </summary>
		/// <value>The <c>NSColor</c> of the method parameter.</value>
		[Export("MethodParameterColor")]
		public NSColor MethodParameterColor {
			get { return LoadColor ("MethodParameterColor", NSColor.FromDeviceRgba(0.06f, 0.52f, 0.50f, 1.0f)); }
			set {
				WillChangeValue ("MethodParameterColor");
				SaveColor ("MethodParameterColor", value, true);
				DidChangeValue ("MethodParameterColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a namespace keyword.
		/// </summary>
		/// <value>The <c>NSColor</c> of the namespace.</value>
		[Export("NamespaceColor")]
		public NSColor NamespaceColor {
			get { return LoadColor ("NamespaceColor", NSColor.FromDeviceRgba(0.06f, 0.52f, 0.50f, 1.0f)); }
			set {
				WillChangeValue ("NamespaceColor");
				SaveColor ("NamespaceColor", value, true);
				DidChangeValue ("NamespaceColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a operator keyword.
		/// </summary>
		/// <value>The <c>NSColor</c> of the operator keyword.</value>
		[Export("OperatorKeywordColor")]
		public NSColor OperatorKeywordColor {
			get { return LoadColor ("OperatorKeywordColor", NSColor.FromDeviceRgba(0.80f, 0.40f, 1f, 1.0f)); }
			set {
				WillChangeValue ("OperatorKeywordColor");
				SaveColor ("OperatorKeywordColor", value, true);
				DidChangeValue ("OperatorKeywordColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a conversion keyword.
		/// </summary>
		/// <value>The <c>NSColor</c> of the conversion keyword.</value>
		[Export("ConversionKeywordColor")]
		public NSColor ConversionKeywordColor {
			get { return LoadColor ("ConversionKeywordColor", NSColor.Purple); }
			set {
				WillChangeValue ("ConversionKeywordColor");
				SaveColor ("ConversionKeywordColor", value, true);
				DidChangeValue ("ConversionKeywordColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a access keyword.
		/// </summary>
		/// <value>The <c>NSColor</c> of the access keyword.</value>
		[Export("AccessKeywordColor")]
		public NSColor AccessKeywordColor {
			get { return LoadColor ("AccessKeywordColor", NSColor.Purple); }
			set {
				WillChangeValue ("AccessKeywordColor");
				SaveColor ("AccessKeywordColor", value, true);
				DidChangeValue ("AccessKeywordColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a literal keyword.
		/// </summary>
		/// <value>The <c>NSColor</c> of the literal keyword.</value>
		[Export("LiteralKeywordColor")]
		public NSColor LiteralKeywordColor {
			get { return LoadColor ("LiteralKeywordColor", NSColor.Purple); }
			set {
				WillChangeValue ("LiteralKeywordColor");
				SaveColor ("LiteralKeywordColor", value, true);
				DidChangeValue ("LiteralKeywordColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a contextual keyword.
		/// </summary>
		/// <value>The <c>NSColor</c> of the contextual keyword.</value>
		[Export("ContextualKeywordColor")]
		public NSColor ContextualKeywordColor {
			get { return LoadColor ("ContextualKeywordColor", NSColor.FromDeviceRgba(0f, 0.50f, 0.25f, 1.0f)); }
			set {
				WillChangeValue ("ContextualKeywordColor");
				SaveColor ("ContextualKeywordColor", value, true);
				DidChangeValue ("ContextualKeywordColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a query keyword.
		/// </summary>
		/// <value>The <c>NSColor</c> of the query keyword.</value>
		[Export("QueryKeywordColor")]
		public NSColor QueryKeywordColor {
			get { return LoadColor ("QueryKeywordColor", NSColor.Orange); }
			set {
				WillChangeValue ("QueryKeywordColor");
				SaveColor ("QueryKeywordColor", value, true);
				DidChangeValue ("QueryKeywordColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a preprocessor directive keyword.
		/// </summary>
		/// <value>The <c>NSColor</c> of the preprocessor directive.</value>
		[Export("PreprocessorDirectiveColor")]
		public NSColor PreprocessorDirectiveColor {
			get { return LoadColor ("PreprocessorDirectiveColor", NSColor.FromDeviceRgba(0.69f, 0.03f, 0.61f, 1.0f)); }
			set {
				WillChangeValue ("PreprocessorDirectiveColor");
				SaveColor ("PreprocessorDirectiveColor", value, true);
				DidChangeValue ("PreprocessorDirectiveColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a comment.
		/// </summary>
		/// <value>The <c>NSColor</c> of the comment.</value>
		[Export("CommentColor")]
		public NSColor CommentColor {
			get { return LoadColor ("CommentColor", NSColor.Gray); }
			set {
				WillChangeValue ("CommentColor");
				SaveColor ("CommentColor", value, true);
				DidChangeValue ("CommentColor");
			}
		}

		/// <summary>
		/// Gets or sets the color of a string literal.
		/// </summary>
		/// <value>The <c>NSColor</c> of the string literal.</value>
		[Export("StringLiteralColor")]
		public NSColor StringLiteralColor {
			get { return LoadColor ("StringLiteralColor", NSColor.Orange); }
			set {
				WillChangeValue ("StringLiteralColor");
				SaveColor ("StringLiteralColor", value, true);
				DidChangeValue ("StringLiteralColor");
			}
		}
		#endregion

		#region Constructors
		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.LanguageDescriptor"/> class.
		/// </summary>
		public LanguageDescriptor ()
		{
		}
		#endregion

		#region Public Methods
		/// <summary>
		/// Define this instance.
		/// </summary>
		public virtual void Define() {

			// Initialize
			Keywords.Clear();
			Formats.Clear ();
			Closures.Clear ();
			FormattingCommands.Clear ();

			// Define the default set of closures
			Closures.Add(new LanguageClosure('(',')'));
			Closures.Add(new LanguageClosure('[',']'));
			Closures.Add(new LanguageClosure('<','>'));
			Closures.Add(new LanguageClosure('{','}'));
			Closures.Add(new LanguageClosure('"'));
		}

		/// <summary>
		/// Formats the passed in string of text for previewing.
		/// </summary>
		/// <returns>The string formatted for preview.</returns>
		/// <param name="text">Text.</param>
		public virtual string FormatForPreview(string text) {
			return text;
		}

		/// <summary>
		/// Resets all of the <see cref="AppKit.TextKit.Formatter.FormatDescriptor"/> for this language to
		/// their default states of unmacthed and inactive.
		/// </summary>
		/// <remarks>This should only be called ba a <see cref="AppKit.TextKit.Formatter.LanguageFormatter"/>.</remarks>
		public virtual void ClearFormats() {

			// Clear the process state of all formats
			foreach (FormatDescriptor format in Formats) {
				format.CharIndex = 0;
				format.Active = false;
			}

		}

		/// <summary>
		/// Converts the given color into a web style hex string in the form #RRBBGG or optionally #RRBBGGAA.
		/// </summary>
		/// <returns>The web hex string representing the given color.</returns>
		/// <param name="color">The <c>NSColor</c> to convert.</param>
		/// <param name="withAlpha">If set to <c>true</c> with the alpha (transparency) of the color will be
		/// included.</param>
		public string NSColorToHexString(NSColor color, bool withAlpha) {
			//Break color into pieces
			float red=0, green=0, blue=0, alpha=0;
			color.GetRgba (out red, out green, out blue, out alpha);

			// Adjust to byte
			alpha *= 255;
			red *= 255;
			green *= 255;
			blue *= 255;

			//With the alpha value?
			if (withAlpha) {
				return String.Format ("#{0:X2}{1:X2}{2:X2}{3:X2}", (int)alpha, (int)red, (int)green, (int)blue);
			} else {
				return String.Format ("#{0:X2}{1:X2}{2:X2}", (int)red, (int)green, (int)blue);
			}
		}

		/// <summary>
		/// Converts a web formatted hex string in the form #RRGGBB or #RRGGBBAA into a
		/// color.
		/// </summary>
		/// <returns>The <c>NSColor</c> represented by the hex string.</returns>
		/// <param name="hexValue">The web formatted hex string in the form #RRGGBB or #RRGGBBAA.</param>
		public NSColor NSColorFromHexString (string hexValue)
		{
			var colorString = hexValue.Replace ("#", "");
			float red, green, blue, alpha;

			// Convert color based on length
			switch (colorString.Length) {
			case 3 : // #RGB
				red = Convert.ToInt32(string.Format("{0}{0}", colorString.Substring(0, 1)), 16) / 255f;
				green = Convert.ToInt32(string.Format("{0}{0}", colorString.Substring(1, 1)), 16) / 255f;
				blue = Convert.ToInt32(string.Format("{0}{0}", colorString.Substring(2, 1)), 16) / 255f;
				return NSColor.FromDeviceRgba(red, green, blue, 1.0f);
			case 6 : // #RRGGBB
				red = Convert.ToInt32(colorString.Substring(0, 2), 16) / 255f;
				green = Convert.ToInt32(colorString.Substring(2, 2), 16) / 255f;
				blue = Convert.ToInt32(colorString.Substring(4, 2), 16) / 255f;
				return NSColor.FromDeviceRgba(red, green, blue, 1.0f);
			case 8 : // #AARRGGBB
				alpha = Convert.ToInt32(colorString.Substring(0, 2), 16) / 255f;
				red = Convert.ToInt32(colorString.Substring(2, 2), 16) / 255f;
				green = Convert.ToInt32(colorString.Substring(4, 2), 16) / 255f;
				blue = Convert.ToInt32(colorString.Substring(6, 2), 16) / 255f;
				return NSColor.FromDeviceRgba(red, green, blue, alpha);
			default :
				throw new ArgumentOutOfRangeException(string.Format("Invalid color value '{0}'. It should be a hex value of the form #RBG, #RRGGBB or #AARRGGBB", hexValue));
			}
		}

		/// <summary>
		/// Loads the requested color from system-wide user defaults.
		/// </summary>
		/// <returns>The <c>NSColor</c> for the given key or the default value if the key
		/// cannot be found in the user defaults.</returns>
		/// <param name="key">The user default key for the color.</param>
		/// <param name="defaultValue">The default <c>NSColor</c> value.</param>
		public NSColor LoadColor(string key, NSColor defaultValue) {

			// Attempt to read color, add the language ID to make unique
			var hex = NSUserDefaults.StandardUserDefaults.StringForKey(LanguageIdentifier + key);

			// Take action based on value
			if (hex == null) {
				return defaultValue;
			} else {
				return NSColorFromHexString (hex);
			}
		}

		/// <summary>
		/// Saves the given color to the systwm-wide user defaults with the give keyword.
		/// </summary>
		/// <param name="color">The <c>NSColor</c> to save to the user defaults.</param>
		/// <param name="key">The user default key to assign the color to.</param>
		/// <param name="sync">If set to <c>true</c> sync changes to preferences.</param>
		public void SaveColor(string key, NSColor color, bool sync) {
			// Save to default, add the language ID to make unique
			NSUserDefaults.StandardUserDefaults.SetString(NSColorToHexString(color,true), LanguageIdentifier + key);
			if (sync) NSUserDefaults.StandardUserDefaults.Synchronize ();
		}
		#endregion
	}
}

