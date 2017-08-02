using System;

namespace MonoMac.AppKit.TextKit.Formatter
{
	/// <summary>
	/// Defines the type of a <see cref="AppKit.TextKit.Formatter.KeywordDescriptor"/>.
	/// </summary>
	public enum KeywordType
	{
		/// <summary>
		/// A generic keyword that doesn't fall under one of the other types.
		/// </summary>
		Keyword,

		/// <summary>
		/// The generic variable type keyword such as <c>var</c>.
		/// </summary>
		Type,

		/// <summary>
		/// A variable type keyword such as <c>string</c> or <c>int</c>.
		/// </summary>
		ValueType,

		/// <summary>
		/// A reference variable type such as <c>object</c>.
		/// </summary>
		ReferenceType,

		/// <summary>
		/// An access modifier keyword such as <c>public</c> or <c>private</c>.
		/// </summary>
		AccessModifier,

		/// <summary>
		/// A geeneric modifier type of keyword.
		/// </summary>
		Modifier,

		/// <summary>
		/// A selection statement keyword such as <c>if</c>.
		/// </summary>
		SelectionStatement,

		/// <summary>
		/// An iteration statement keyword such as <c>for</c>.
		/// </summary>
		IterationStatement,

		/// <summary>
		/// A jump statement keyword such as <c>break</c>.
		/// </summary>
		JumpStatement,

		/// <summary>
		/// A exception handling statement keyword such as <c>try</c> or <c>catch</c>.
		/// </summary>
		ExceptionHandlingStatement,

		/// <summary>
		/// A generic statement keyword.
		/// </summary>
		Statement,

		/// <summary>
		/// A method parameters ketword such as <c>out</c>.
		/// </summary>
		MethodParameters,

		/// <summary>
		/// A namespace keyword.
		/// </summary>
		NamespaceKeyword,

		/// <summary>
		/// An operator keyword such as <c>sizeof</c>.
		/// </summary>
		OperatorKeyword,

		/// <summary>
		/// A conversion keyword such as <c>explicit</c>.
		/// </summary>
		ConversionKeyword,

		/// <summary>
		/// An access keyword such as <c>this</c>.
		/// </summary>
		AccessKeywords,

		/// <summary>
		/// A literal keyword such as <c>null</c>.
		/// </summary>
		LiteralKeywords,

		/// <summary>
		/// A contextual keyword such as <c>get</c> or <c>set</c>.
		/// </summary>
		ContextualKeywords,

		/// <summary>
		/// A query keywords such as <c>select</c>.
		/// </summary>
		QueryKeywords,

		/// <summary>
		/// A preprocessor directive keyword like <c>#if</c>.
		/// </summary>
		PreprocessorDirective
	}
}

