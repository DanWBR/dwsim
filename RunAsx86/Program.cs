using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Xml;

namespace RunAsx86
{
	class Program
	{
		[STAThread]
		static int Main(string[] args)
		{
			try
			{
				if (args.Length == 0)
				{
					Console.WriteLine("Please, go to application directory, provide path and arguments");
					return -1;
				}
				var folder = Path.GetDirectoryName(args[0]);
				var target = Prepare(
					string.IsNullOrEmpty(folder) ? Path.Combine(Environment.CurrentDirectory, args[0]) : args[0]);

				Console.WriteLine("Executing: " + target);
				var folders = (new[] {folder}).Concat(GetAdditionalDllPathes(folder, target)).ToArray();
				AppDomain.CurrentDomain.AssemblyResolve += (o,e) => LoadFromSameFolder(o,e,folders);
				var assembly = Assembly.LoadFile(target);

				var type = GetType(assembly);
				if (type == null)
				{
					Console.WriteLine("Can't find method Main");
					return -1;
				}
				var m = GetMethod(type);
				try
				{
					m.Invoke(type, new object[] {args.Skip(1).ToArray()});
				}
				catch (TargetParameterCountException)
				{
					m.Invoke(type, new object[0]);
				}
				Console.WriteLine("Done");
				return 1;
			}
			catch (Exception ex)
			{
				Console.WriteLine(ex);
				return -1;
			}
		}

		private static IEnumerable<string> GetAdditionalDllPathes(string folder, string target)
		{
			var config = target + ".config";
			if (!File.Exists(config)) return new string[0];
			try
			{
				var doc = new XmlDocument();
				doc.Load(config);
				var manager = new XmlNamespaceManager(doc.NameTable);
				manager.AddNamespace("bindings", "urn:schemas-microsoft-com:asm.v1");
				var nodes = doc.SelectSingleNode("/configuration/runtime/bindings:assemblyBinding", manager);
				if(nodes == null)return new string[0];
				return nodes
					.ChildNodes
					.Cast<XmlNode>()
					.Where(x => x.Name == "probing")
					.Where(x => x.Attributes != null)
					.Where(x => x.Attributes["privatePath"]!=null)
					.Select(x => Path.Combine(folder, x.Attributes["privatePath"].Value))
					.ToArray();
			}
			catch (Exception ex)
			{
				Console.WriteLine("Error loading config file for exe: {0}", ex);
				return new string[0];
			}
		}

		private static Type GetType(Assembly assembly)
		{
			return assembly.GetTypes().FirstOrDefault(type => GetMethod(type) != null);
		}

		private static string Prepare(string path)
		{
			if (File.Exists(path)) return path;
			if (File.Exists(path + ".exe")) return path + ".exe";
			throw new ArgumentException(string.Format("Nothing to execute: '{0}'", path));
		}

		static Assembly LoadFromSameFolder(object sender, ResolveEventArgs args, params string[] folders)
		{
			string assemblyPath;
			foreach (var folder in folders.Where(folder => !string.IsNullOrEmpty(folder)))
			{
				assemblyPath = Path.Combine(folder, new AssemblyName(args.Name).Name + ".dll");
				if (File.Exists(assemblyPath))
					return Assembly.LoadFrom(assemblyPath);
			}
			assemblyPath = Path.Combine(Environment.CurrentDirectory, new AssemblyName(args.Name).Name + ".dll");
			return File.Exists(assemblyPath) ? 
				Assembly.LoadFrom(assemblyPath) : 
				null;
		}

		private static MethodInfo GetMethod(Type t)
		{
			return t.GetMethod("Main", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static);
		}
	}
}
