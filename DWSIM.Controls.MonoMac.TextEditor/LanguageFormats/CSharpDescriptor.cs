using System;
using System.Collections.Generic;
using MonoMac.Foundation;
using MonoMac.AppKit;
using MonoMac.AppKit.TextKit.Formatter;

namespace MonoMac.AppKit.TextKit.Formatter
{
	/// <summary>
	/// Defines how C# should be colorized using a <see cref="AppKit.TextKit.Formatter.LanguageFormatter"/>.
	/// </summary>
	public class CSharpDescriptor : LanguageDescriptor
	{
		#region Computed Properties
		/// <summary>
		/// Gets the language identifier.
		/// </summary>
		/// <value>The language identifier.</value>
		public override string LanguageIdentifier {
			get { return "CSharp"; }
		}

		/// <summary>
		/// Gets or sets the language separators for C#
		/// </summary>
		/// <value>The language separators.</value>
		public override char[] LanguageSeparators { get; set; } = new char[]{'=','+','-','*','/','%','&','<','>',';',':','^','!','~','?','|',',','"','\'','(',')','[',']','{','}'};
		#endregion

		#region Constructors
		/// <summary>
		/// Initializes a new instance of the <see cref="AppKit.TextKit.Formatter.CSharpDescriptor"/> class.
		/// </summary>
		public CSharpDescriptor ()
		{
		}
		#endregion

		#region Override Methods
		/// <summary>
		/// Define this instance.
		/// </summary>
		public override void Define ()
		{
			// Call base class
			base.Define();

			// Value Types
			// Keywords.Add("", new KeywordDescriptor(KeywordType.ValueType, ValueTypeColor, ""));
			Keywords.Add("bool", new KeywordDescriptor(KeywordType.ValueType, ValueTypeColor, "It is used to declare variables to store the Boolean values, true and false."));
			Keywords.Add("byte", new KeywordDescriptor(KeywordType.ValueType, ValueTypeColor, "The byte keyword denotes an integral type that stores values between 0 to 255."));
			Keywords.Add("char", new KeywordDescriptor(KeywordType.ValueType, ValueTypeColor, "The char keyword is used to declare an instance of the System.Char structure that the .NET Framework uses to represent a Unicode character."));
			Keywords.Add("decimal", new KeywordDescriptor(KeywordType.ValueType, ValueTypeColor, "The decimal keyword denotes an integral type that stores values between (-7.9 x 10^28 to 7.9 x 10^28) / (10^(0 to 28))."));
			Keywords.Add("double", new KeywordDescriptor(KeywordType.ValueType, ValueTypeColor, "The double keyword signifies a simple type that stores 64-bit floating-point values."));
			Keywords.Add("enum", new KeywordDescriptor(KeywordType.ValueType, ValueTypeColor, "The enum keyword is used to declare an enumeration, a distinct type that consists of a set of named constants called the enumerator list."));
			Keywords.Add("float", new KeywordDescriptor(KeywordType.ValueType, ValueTypeColor, "The float keyword signifies a simple type that stores 32-bit floating-point values. "));
			Keywords.Add("int", new KeywordDescriptor(KeywordType.ValueType, ValueTypeColor, "The int keyword denotes an integral type that stores values between -2,147,483,648 to 2,147,483,647."));
			Keywords.Add("long", new KeywordDescriptor(KeywordType.ValueType, ValueTypeColor, "The long keyword denotes an integral type that stores values between –9,223,372,036,854,775,808 to 9,223,372,036,854,775,807."));
			Keywords.Add("sbyte", new KeywordDescriptor(KeywordType.ValueType, ValueTypeColor, "The sbyte keyword indicates an integral type that stores values between -128 to 127."));
			Keywords.Add("short", new KeywordDescriptor(KeywordType.ValueType, ValueTypeColor, "The short keyword denotes an integral data type that stores values between -32,768 to 32,767."));
			Keywords.Add("struct", new KeywordDescriptor(KeywordType.ValueType, ValueTypeColor, "A struct type is a value type that is typically used to encapsulate small groups of related variables, such as the coordinates of a rectangle or the characteristics of an item in an inventory."));
			Keywords.Add("uint", new KeywordDescriptor(KeywordType.ValueType, ValueTypeColor, "The uint keyword signifies an integral type that stores values between 0 to 4,294,967,295."));
			Keywords.Add("ulong", new KeywordDescriptor(KeywordType.ValueType, ValueTypeColor, "The ulong keyword denotes an integral type that stores values between 0 to 18,446,744,073,709,551,615."));
			Keywords.Add("ushort", new KeywordDescriptor(KeywordType.ValueType, ValueTypeColor, "The ushort keyword indicates an integral data type that stores values between 0 to 65,535."));

			// Reference Types
			// Keywords.Add("", new KeywordDescriptor(KeywordType.ReferenceType, ReferenceTypeColor, ""));
			Keywords.Add("class", new KeywordDescriptor(KeywordType.ReferenceType, ReferenceTypeColor, "Classes are declared using the keyword class."));
			Keywords.Add("delegate", new KeywordDescriptor(KeywordType.ReferenceType, ReferenceTypeColor, "The declaration of a delegate type is similar to a method signature. It has a return value and any number of parameters of any type."));
			Keywords.Add("dynamic", new KeywordDescriptor(KeywordType.ReferenceType, ReferenceTypeColor, "The dynamic type enables the operations in which it occurs to bypass compile-time type checking. Instead, these operations are resolved at run time."));
			Keywords.Add("interface", new KeywordDescriptor(KeywordType.ReferenceType, ReferenceTypeColor, "An interface contains only the signatures of methods, properties, events or indexers. "));
			Keywords.Add("object", new KeywordDescriptor(KeywordType.ReferenceType, ReferenceTypeColor, "In the unified type system of C#, all types, predefined and user-defined, reference types and value types, inherit directly or indirectly from Object. You can assign values of any type to variables of type object. "));
			Keywords.Add("string", new KeywordDescriptor(KeywordType.ReferenceType, ReferenceTypeColor, "The string type represents a sequence of zero or more Unicode characters. string is an alias for String in the .NET Framework."));

			// Generic Types
			// Keywords.Add("", new KeywordDescriptor(KeywordType.Type, TypeColor, ""));
			Keywords.Add("void", new KeywordDescriptor(KeywordType.Type, TypeColor, "When used as the return type for a method, void specifies that the method doesn't return a value."));
			Keywords.Add("var", new KeywordDescriptor(KeywordType.Type, TypeColor, "An implicitly typed local variable is strongly typed just as if you had declared the type yourself, but the compiler determines the type."));

			// Access modifiers
			// Keywords.Add("", new KeywordDescriptor(KeywordType.AccessModifier, AccessModifierColor, ""));
			Keywords.Add("public", new KeywordDescriptor(KeywordType.AccessModifier, AccessModifierColor, "The public keyword is an access modifier for types and type members. There are no restrictions on accessing public members."));
			Keywords.Add("private", new KeywordDescriptor(KeywordType.AccessModifier, AccessModifierColor, "Private members are accessible only within the body of the class or the struct in which they are declared."));
			Keywords.Add("internal", new KeywordDescriptor(KeywordType.AccessModifier, AccessModifierColor, "Internal types or members are accessible only within files in the same assembly."));
			Keywords.Add("protected", new KeywordDescriptor(KeywordType.AccessModifier, AccessModifierColor, "A protected member is accessible within its class and by derived class instances."));

			// Modifiers
			// Keywords.Add("", new KeywordDescriptor(KeywordType.Modifier, ModifierColor, ""));
			Keywords.Add("abstract", new KeywordDescriptor(KeywordType.Modifier, ModifierColor, "The abstract modifier indicates that the thing being modified has a missing or incomplete implementation."));
			Keywords.Add("async", new KeywordDescriptor(KeywordType.Modifier, ModifierColor, "Use the async modifier to specify that a method, lambda expression, or anonymous method is asynchronous. "));
			Keywords.Add("const", new KeywordDescriptor(KeywordType.Modifier, ModifierColor, "You use the const keyword to declare a constant field or a constant local. Constant fields and locals aren't variables and may not be modified. "));
			Keywords.Add("event", new KeywordDescriptor(KeywordType.Modifier, ModifierColor, "The event keyword is used to declare an event in a publisher class."));
			Keywords.Add("extern", new KeywordDescriptor(KeywordType.Modifier, ModifierColor, "The extern modifier is used to declare a method that is implemented externally. "));
			Keywords.Add("in", new KeywordDescriptor(KeywordType.Modifier, ModifierColor, "For generic type parameters, the in keyword specifies that the type parameter is contravariant. You can use the in keyword in generic interfaces and delegates."));
			Keywords.Add("override", new KeywordDescriptor(KeywordType.Modifier, ModifierColor, "The override modifier is required to extend or modify the abstract or virtual implementation of an inherited method, property, indexer, or event."));
			Keywords.Add("readonly", new KeywordDescriptor(KeywordType.Modifier, ModifierColor, "When a field declaration includes a readonly modifier, assignments to the fields introduced by the declaration can only occur as part of the declaration or in a constructor in the same class."));
			Keywords.Add("sealed", new KeywordDescriptor(KeywordType.Modifier, ModifierColor, "When applied to a class, the sealed modifier prevents other classes from inheriting from it."));
			Keywords.Add("static", new KeywordDescriptor(KeywordType.Modifier, ModifierColor, "Use the static modifier to declare a static member, which belongs to the type itself rather than to a specific object. "));
			Keywords.Add("unsafe", new KeywordDescriptor(KeywordType.Modifier, ModifierColor, "The unsafe keyword denotes an unsafe context, which is required for any operation involving pointers."));
			Keywords.Add("virtual", new KeywordDescriptor(KeywordType.Modifier, ModifierColor, "The virtual keyword is used to modify a method, property, indexer, or event declaration and allow for it to be overridden in a derived class. "));
			Keywords.Add("volatile", new KeywordDescriptor(KeywordType.Modifier, ModifierColor, "The volatile keyword indicates that a field might be modified by multiple threads that are executing at the same time. "));

			// Selection Statements
			// Keywords.Add("", new KeywordDescriptor(KeywordType.SelectionStatement, SelectionStatementColor, ""));
			Keywords.Add("if", new KeywordDescriptor(KeywordType.SelectionStatement, SelectionStatementColor, "An if statement identifies which statement to run based on the value of a Boolean expression."));
			Keywords.Add("else", new KeywordDescriptor(KeywordType.SelectionStatement, SelectionStatementColor, "In an if-else statement, if condition evaluates to true, the then-statement runs. If condition is false, the else-statement runs. "));
			Keywords.Add("switch", new KeywordDescriptor(KeywordType.SelectionStatement, SelectionStatementColor, "The switch statement is a control statement that selects a switch section to execute from a list of candidates."));
			Keywords.Add("case", new KeywordDescriptor(KeywordType.SelectionStatement, SelectionStatementColor, "Each case label specifies a constant value."));
			Keywords.Add("default", new KeywordDescriptor(KeywordType.SelectionStatement, SelectionStatementColor, "f no case label contains a matching value, control is transferred to the default section, if there is one."));

			// Iteration Statements
			// Keywords.Add("", new KeywordDescriptor(KeywordType.IterationStatement, IterationStatementColor, ""));
			Keywords.Add("do", new KeywordDescriptor(KeywordType.IterationStatement, IterationStatementColor, "The do statement executes a statement or a block of statements repeatedly until a specified expression evaluates to false. "));
			Keywords.Add("for", new KeywordDescriptor(KeywordType.IterationStatement, IterationStatementColor, "By using a for loop, you can run a statement or a block of statements repeatedly until a specified expression evaluates to false."));
			Keywords.Add("foreach", new KeywordDescriptor(KeywordType.IterationStatement, IterationStatementColor, "The foreach statement repeats a group of embedded statements for each element in an array or an object collection that implements the System.Collections.IEnumerable or System.Collections.Generic.IEnumerable<T> interface. "));
			Keywords.Add("while", new KeywordDescriptor(KeywordType.IterationStatement, IterationStatementColor, "The while statement executes a statement or a block of statements until a specified expression evaluates to false."));

			// Jump Statements
			// Keywords.Add("", new KeywordDescriptor(KeywordType.JumpStatement, JumpStatementColor, ""));
			Keywords.Add("break", new KeywordDescriptor(KeywordType.JumpStatement, JumpStatementColor, "The break statement terminates the closest enclosing loop or switch statement in which it appears. Control is passed to the statement that follows the terminated statement, if any."));
			Keywords.Add("continue", new KeywordDescriptor(KeywordType.JumpStatement, JumpStatementColor, "The continue statement passes control to the next iteration of the enclosing while, do, for, or foreach statement in which it appears."));
			Keywords.Add("goto", new KeywordDescriptor(KeywordType.JumpStatement, JumpStatementColor, "The goto statement transfers the program control directly to a labeled statement."));
			Keywords.Add("return", new KeywordDescriptor(KeywordType.JumpStatement, JumpStatementColor, "The return statement terminates execution of the method in which it appears and returns control to the calling method. It can also return an optional value."));

			// Exception Handling Statements
			// Keywords.Add("", new KeywordDescriptor(KeywordType.ExceptionHandlingStatement, ExceptionHandlingColor, ""));
			Keywords.Add("throw", new KeywordDescriptor(KeywordType.ExceptionHandlingStatement, ExceptionHandlingColor, "The throw statement is used to signal the occurrence of an anomalous situation (exception) during the program execution."));
			Keywords.Add("try", new KeywordDescriptor(KeywordType.ExceptionHandlingStatement, ExceptionHandlingColor, "The try-catch statement consists of a try block followed by one or more catch clauses, which specify handlers for different exceptions."));
			Keywords.Add("catch", new KeywordDescriptor(KeywordType.ExceptionHandlingStatement, ExceptionHandlingColor, "The try-catch statement consists of a try block followed by one or more catch clauses, which specify handlers for different exceptions."));
			Keywords.Add("finally", new KeywordDescriptor(KeywordType.ExceptionHandlingStatement, ExceptionHandlingColor, "A common usage of catch and finally together is to obtain and use resources in a try block, deal with exceptional circumstances in a catch block, and release the resources in the finally block."));

			// Statements
			// Keywords.Add("", new KeywordDescriptor(KeywordType.Statement, StatementColor, ""));
			Keywords.Add("checked", new KeywordDescriptor(KeywordType.Statement, StatementColor, "The checked keyword is used to explicitly enable overflow checking for integral-type arithmetic operations and conversions."));
			Keywords.Add("unchecked", new KeywordDescriptor(KeywordType.Statement, StatementColor, "The unchecked keyword is used to suppress overflow-checking for integral-type arithmetic operations and conversions."));
			Keywords.Add("fixed", new KeywordDescriptor(KeywordType.Statement, StatementColor, "The fixed statement prevents the garbage collector from relocating a movable variable. The fixed statement is only permitted in an unsafe context."));
			Keywords.Add("lock", new KeywordDescriptor(KeywordType.Statement, StatementColor, "The lock keyword marks a statement block as a critical section by obtaining the mutual-exclusion lock for a given object, executing a statement, and then releasing the lock."));

			// Method Parameters
			// Keywords.Add("", new KeywordDescriptor(KeywordType.MethodParameters, MethodParameterColor, ""));
			Keywords.Add("params", new KeywordDescriptor(KeywordType.MethodParameters, MethodParameterColor, "By using the params keyword, you can specify a method parameter that takes a variable number of arguments."));
			Keywords.Add("ref", new KeywordDescriptor(KeywordType.MethodParameters, MethodParameterColor, "The ref keyword causes an argument to be passed by reference, not by value."));
			Keywords.Add("out", new KeywordDescriptor(KeywordType.MethodParameters, MethodParameterColor, "The out keyword causes arguments to be passed by reference. This is like the ref keyword, except that ref requires that the variable be initialized before it is passed. "));

			// Namespace Keywords
			// Keywords.Add("", new KeywordDescriptor(KeywordType.NamespaceKeyword, NamespaceColor, ""));
			Keywords.Add("namespace", new KeywordDescriptor(KeywordType.NamespaceKeyword, NamespaceColor, "The namespace keyword is used to declare a scope that contains a set of related objects."));
			Keywords.Add("using", new KeywordDescriptor(KeywordType.MethodParameters, NamespaceColor, "Allows the use of types in a namespace so that you do not have to qualify the use of a type in that namespace."));

			// Operator Keywords
			// Keywords.Add("", new KeywordDescriptor(KeywordType.OperatorKeyword, OperatorKeywordColor, ""));
			Keywords.Add("as", new KeywordDescriptor(KeywordType.OperatorKeyword, OperatorKeywordColor, "You can use the as operator to perform certain types of conversions between compatible reference types or nullable types."));
			Keywords.Add("await", new KeywordDescriptor(KeywordType.OperatorKeyword, OperatorKeywordColor, "The await operator is applied to a task in an asynchronous method to suspend the execution of the method until the awaited task completes."));
			Keywords.Add("is", new KeywordDescriptor(KeywordType.OperatorKeyword, OperatorKeywordColor, "Checks if an object is compatible with a given type."));
			Keywords.Add("new", new KeywordDescriptor(KeywordType.OperatorKeyword, OperatorKeywordColor, "Used to create objects and invoke constructors."));
			Keywords.Add("sizeof", new KeywordDescriptor(KeywordType.OperatorKeyword, OperatorKeywordColor, "Used to obtain the size in bytes for an unmanaged type."));
			Keywords.Add("typeof", new KeywordDescriptor(KeywordType.OperatorKeyword, OperatorKeywordColor, "Used to obtain the System.Type object for a type."));
			Keywords.Add("true", new KeywordDescriptor(KeywordType.OperatorKeyword, OperatorKeywordColor, "Represents the boolean value true."));
			Keywords.Add("false", new KeywordDescriptor(KeywordType.OperatorKeyword, OperatorKeywordColor, "Represents the boolean value false."));
			Keywords.Add("stackalloc", new KeywordDescriptor(KeywordType.OperatorKeyword, OperatorKeywordColor, "The stackalloc keyword is used in an unsafe code context to allocate a block of memory on the stack."));
			Keywords.Add("nameof", new KeywordDescriptor(KeywordType.OperatorKeyword, OperatorKeywordColor, "Used to obtain the simple (unqualified) string name of a variable, type, or member. "));

			// Conversion Keywords
			// Keywords.Add("", new KeywordDescriptor(KeywordType.ConversionKeyword, ConversionKeywordColor, ""));
			Keywords.Add("explicit", new KeywordDescriptor(KeywordType.ConversionKeyword, ConversionKeywordColor, "The explicit keyword declares a user-defined type conversion operator that must be invoked with a cast. "));
			Keywords.Add("implicit", new KeywordDescriptor(KeywordType.ConversionKeyword, ConversionKeywordColor, "The implicit keyword is used to declare an implicit user-defined type conversion operator. "));
			Keywords.Add("operator", new KeywordDescriptor(KeywordType.ConversionKeyword, ConversionKeywordColor, "Use the operator keyword to overload a built-in operator or to provide a user-defined conversion in a class or struct declaration."));

			// Access Keywords
			// Keywords.Add("", new KeywordDescriptor(KeywordType.AccessKeywords, AccessKeywordColor, ""));
			Keywords.Add("base", new KeywordDescriptor(KeywordType.AccessKeywords, AccessKeywordColor, "Accesses the members of the base class."));
			Keywords.Add("this", new KeywordDescriptor(KeywordType.AccessKeywords, AccessKeywordColor, "Refers to the current instance of the class."));

			// Literal Keywords
			// Keywords.Add("", new KeywordDescriptor(KeywordType.LiteralKeywords, LiteralKeywordColor, ""));
			Keywords.Add("null", new KeywordDescriptor(KeywordType.LiteralKeywords, LiteralKeywordColor, "The null keyword is a literal that represents a null reference, one that does not refer to any object. null is the default value of reference-type variables."));

			// Contextual Keywords
			// Keywords.Add("", new KeywordDescriptor(KeywordType.ContextualKeywords, ContextualKeywordColor, ""));
			Keywords.Add("add", new KeywordDescriptor(KeywordType.ContextualKeywords, ContextualKeywordColor, "The add contextual keyword is used to define a custom event accessor that is invoked when client code subscribes to your event."));
			Keywords.Add("get", new KeywordDescriptor(KeywordType.ContextualKeywords, ContextualKeywordColor, "The get keyword defines an accessor method in a property or indexer that retrieves the value of the property or the indexer element."));
			Keywords.Add("global", new KeywordDescriptor(KeywordType.ContextualKeywords, ContextualKeywordColor, "The global contextual keyword, when it comes before the :: operator, refers to the global namespace, which is the default namespace for any C# program and is otherwise unnamed."));
			Keywords.Add("partial", new KeywordDescriptor(KeywordType.ContextualKeywords, ContextualKeywordColor, "Partial type definitions allow for the definition of a class, struct, interface or method to be split into multiple files or definitions."));
			Keywords.Add("remove", new KeywordDescriptor(KeywordType.ContextualKeywords, ContextualKeywordColor, "The remove contextual keyword is used to define a custom event accessor that is invoked when client code unsubscribes from your event."));
			Keywords.Add("set", new KeywordDescriptor(KeywordType.ContextualKeywords, ContextualKeywordColor, "The set keyword defines an accessor method in a property or indexer that assigns the value of the property or the indexer element."));
			Keywords.Add("where", new KeywordDescriptor(KeywordType.ContextualKeywords, ContextualKeywordColor, "In a generic type definition, the where clause is used to specify constraints on the types that can be used as arguments for a type parameter defined in a generic declaration."));
			Keywords.Add("value", new KeywordDescriptor(KeywordType.ContextualKeywords, ContextualKeywordColor, "The contextual keyword value is used in the set accessor in ordinary property declarations. It is similar to an input parameter on a method. "));
			Keywords.Add("yield", new KeywordDescriptor(KeywordType.ContextualKeywords, ContextualKeywordColor, "When you use the yield keyword in a statement, you indicate that the method, operator, or get accessor in which it appears is an iterator. "));

			// Query Keywords (LINQ)
			// Keywords.Add("", new KeywordDescriptor(KeywordType.QueryKeywords, QueryKeywordColor, ""));
			Keywords.Add("from", new KeywordDescriptor(KeywordType.QueryKeywords, QueryKeywordColor, "A query expression must begin with a from clause. Additionally, a query expression can contain sub-queries, which also begin with a from clause. "));
			Keywords.Add("select", new KeywordDescriptor(KeywordType.QueryKeywords, QueryKeywordColor, "In a query expression, the select clause specifies the type of values that will be produced when the query is executed."));
			Keywords.Add("group", new KeywordDescriptor(KeywordType.QueryKeywords, QueryKeywordColor, "The group clause returns a sequence of IGrouping<TKey,TElement> objects that contain zero or more items that match the key value for the group."));
			Keywords.Add("into", new KeywordDescriptor(KeywordType.QueryKeywords, QueryKeywordColor, "The into contextual keyword can be used to create a temporary identifier to store the results of a group, join or select clause into a new identifier."));
			Keywords.Add("orderby", new KeywordDescriptor(KeywordType.QueryKeywords, QueryKeywordColor, "In a query expression, the orderby clause causes the returned sequence or subsequence (group) to be sorted in either ascending or descending order."));
			Keywords.Add("join", new KeywordDescriptor(KeywordType.QueryKeywords, QueryKeywordColor, "The join clause is useful for associating elements from different source sequences that have no direct relationship in the object model."));
			Keywords.Add("let", new KeywordDescriptor(KeywordType.QueryKeywords, QueryKeywordColor, "In a query expression, it is sometimes useful to store the result of a sub-expression in order to use it in subsequent clauses. You can do this with the let keyword."));
			Keywords.Add("ascending", new KeywordDescriptor(KeywordType.QueryKeywords, QueryKeywordColor, "The ascending contextual keyword is used in the orderby clause in query expressions to specify that the sort order is from smallest to largest."));
			Keywords.Add("descending", new KeywordDescriptor(KeywordType.QueryKeywords, QueryKeywordColor, "The descending contextual keyword is used in the orderby clause in query expressions to specify that the sort order is from largest to smallest."));
			Keywords.Add("on", new KeywordDescriptor(KeywordType.QueryKeywords, QueryKeywordColor, "The on contextual keyword is used in the join clause of a query expression to specify the join condition."));
			Keywords.Add("equals", new KeywordDescriptor(KeywordType.QueryKeywords, QueryKeywordColor, "The equals contextual keyword is used in a join clause in a query expression to compare the elements of two sequences."));
			Keywords.Add("by", new KeywordDescriptor(KeywordType.QueryKeywords, QueryKeywordColor, "The by contextual keyword is used in the group clause in a query expression to specify how the returned items should be grouped."));

			// Preprocessor Directive
			// Keywords.Add("", new KeywordDescriptor(KeywordType.PreprocessorDirective, PreprocessorDirectiveColor, ""));
			Keywords.Add("#if", new KeywordDescriptor(KeywordType.PreprocessorDirective, PreprocessorDirectiveColor, "When the C# compiler encounters an #if directive, followed eventually by an #endif directive, it will compile the code between the directives only if the specified symbol is defined."));
			Keywords.Add("#else", new KeywordDescriptor(KeywordType.PreprocessorDirective, PreprocessorDirectiveColor, "#else lets you create a compound conditional directive, so that, if none of the expressions in the preceding #if or (optional) #elif directives to true, the compiler will evaluate all code between #else and the subsequent #endif."));
			Keywords.Add("#elif", new KeywordDescriptor(KeywordType.PreprocessorDirective, PreprocessorDirectiveColor, "#elif lets you create a compound conditional directive. The #elif expression will be evaluated if neither the preceding #if (C# Reference) nor any preceding, optional, #elif directive expressions evaluate to true."));
			Keywords.Add("#endif", new KeywordDescriptor(KeywordType.PreprocessorDirective, PreprocessorDirectiveColor, "#endif specifies the end of a conditional directive, which began with the #if directive."));
			Keywords.Add("#define", new KeywordDescriptor(KeywordType.PreprocessorDirective, PreprocessorDirectiveColor, "You use #define to define a symbol."));
			Keywords.Add("#undef", new KeywordDescriptor(KeywordType.PreprocessorDirective, PreprocessorDirectiveColor, "#undef lets you undefine a symbol, such that, by using the symbol as the expression in a #if directive, the expression will evaluate to false."));
			Keywords.Add("#warning", new KeywordDescriptor(KeywordType.PreprocessorDirective, PreprocessorDirectiveColor, "#warning lets you generate a level one warning from a specific location in your code."));
			Keywords.Add("#error", new KeywordDescriptor(KeywordType.PreprocessorDirective, PreprocessorDirectiveColor, "#error lets you generate an error from a specific location in your code."));
			Keywords.Add("#line", new KeywordDescriptor(KeywordType.PreprocessorDirective, PreprocessorDirectiveColor, "#line lets you modify the compiler's line number and (optionally) the file name output for errors and warnings. This example shows how to report two warnings associated with line numbers."));
			Keywords.Add("#region", new KeywordDescriptor(KeywordType.PreprocessorDirective, PreprocessorDirectiveColor, "#region lets you specify a block of code that you can expand or collapse when using the outlining feature."));
			Keywords.Add("#endregion", new KeywordDescriptor(KeywordType.PreprocessorDirective, PreprocessorDirectiveColor, "#endregion marks the end of a #region block."));
			Keywords.Add("#pragma", new KeywordDescriptor(KeywordType.PreprocessorDirective, PreprocessorDirectiveColor, "#pragma gives the compiler special instructions for the compilation of the file in which it appears."));

			// Define formats
			Formats.Add(new FormatDescriptor ("//", CommentColor));
			Formats.Add (new FormatDescriptor ("/*", "*/", CommentColor));
			Formats.Add(new FormatDescriptor ("\"","\"", StringLiteralColor));
			Formats.Add (new FormatDescriptor ("'", "'", StringLiteralColor));

			// Define formatting commands
			FormattingCommands.Add(new LanguageFormatCommand("Comment","//"));
			FormattingCommands.Add(new LanguageFormatCommand("Block Comment","/*","*/"));
			FormattingCommands.Add (new LanguageFormatCommand ());

			var Directives = new LanguageFormatCommand ("Preprocessor Directives");
			Directives.SubCommands.Add(new LanguageFormatCommand("If","#if \n","\n#endif\n"));
			Directives.SubCommands.Add(new LanguageFormatCommand("If..Else","#if \n","\n#else\n\n#endif\n"));
			Directives.SubCommands.Add(new LanguageFormatCommand("Define","#define "));
			Directives.SubCommands.Add(new LanguageFormatCommand("Undefine","#undef"));
			Directives.SubCommands.Add(new LanguageFormatCommand("Warning","#warning"));
			Directives.SubCommands.Add(new LanguageFormatCommand("Error","#error"));
			Directives.SubCommands.Add(new LanguageFormatCommand("Line","#line"));
			Directives.SubCommands.Add(new LanguageFormatCommand("Region","#region \n","\n#endregion\n"));
			Directives.SubCommands.Add(new LanguageFormatCommand("Pragma","#pragma"));
			FormattingCommands.Add (Directives);
		}

		/// <summary>
		/// Formats the passed in string of text for previewing.
		/// </summary>
		/// <returns>The string formatted for preview.</returns>
		/// <param name="text">Text.</param>
		public override string FormatForPreview (string text)
		{
			return "<pre><code>" + text + "</code></pre>";
		}
		#endregion
	}
}

