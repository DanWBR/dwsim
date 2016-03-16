/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

using System;
using System.IO;
using System.IO.Compression;
using System.Reflection;
using System.Runtime.InteropServices;

namespace Yeppp
{

	internal interface INativeLibraryLoader
	{
		IntPtr LoadLibrary(string path);
		bool UnloadLibrary(IntPtr library);
		IntPtr FindFunction(IntPtr library, string function);
	}

	internal sealed class WindowsLibraryLoader : INativeLibraryLoader
	{

		IntPtr INativeLibraryLoader.LoadLibrary(string path)
		{
			return WindowsLibraryLoader.LoadLibraryW(path);
		}

		bool INativeLibraryLoader.UnloadLibrary(IntPtr library)
		{
			return WindowsLibraryLoader.FreeLibrary(library);
		}

		IntPtr INativeLibraryLoader.FindFunction(IntPtr library, string function)
		{
			return WindowsLibraryLoader.GetProcAddress(library, function);
		}

		[DllImport("kernel32", SetLastError=true, CharSet=CharSet.Unicode, ExactSpelling=true)]
		private static extern IntPtr LoadLibraryW(string path);

		[DllImport("kernel32", SetLastError=true)]
		[return: MarshalAs(UnmanagedType.Bool)]
		private static extern bool FreeLibrary(IntPtr library);

		[DllImport("kernel32", SetLastError=true, CharSet=CharSet.Ansi, ExactSpelling=true)]
		private static extern IntPtr GetProcAddress(IntPtr library, string function);

	}

	internal abstract class UnixLibraryLoader : INativeLibraryLoader
	{

		IntPtr INativeLibraryLoader.LoadLibrary(string path)
		{
			int flags = GetDLOpenFlags();
			return UnixLibraryLoader.dlopen(path, flags);
		}

		bool INativeLibraryLoader.UnloadLibrary(IntPtr library)
		{
			return UnixLibraryLoader.dlclose(library) == 0;
		}

		IntPtr INativeLibraryLoader.FindFunction(IntPtr library, string function)
		{
			return UnixLibraryLoader.dlsym(library, function);
		}

		protected abstract int GetDLOpenFlags();

		[DllImport("dl", CallingConvention=CallingConvention.Cdecl)]
		private static extern IntPtr dlopen([MarshalAs(UnmanagedType.LPStr)] string path, int flags);

		[DllImport("dl", CallingConvention=CallingConvention.Cdecl)]
		private static extern IntPtr dlsym(IntPtr library, [MarshalAs(UnmanagedType.LPStr)] string function);

		[DllImport("dl", CallingConvention=CallingConvention.Cdecl)]
		private static extern int dlclose(IntPtr library);

	}

	internal sealed class AndroidLibraryLoader : UnixLibraryLoader
	{

		protected override int GetDLOpenFlags()
		{
			return RTLD_NOW | RTLD_LOCAL;
		}

		private const int RTLD_NOW   = 0x00000000;
		private const int RTLD_LOCAL = 0x00000000;

	}

	internal sealed class LinuxLibraryLoader : UnixLibraryLoader
	{

		protected override int GetDLOpenFlags()
		{
			return RTLD_NOW | RTLD_LOCAL;
		}

		private const int RTLD_NOW   = 0x00000002;
		private const int RTLD_LOCAL = 0x00000000;

	}

	internal sealed class OSXLibraryLoader : UnixLibraryLoader
	{

		protected override int GetDLOpenFlags()
		{
			return RTLD_NOW | RTLD_LOCAL;
		}

		private const int RTLD_NOW   = 0x00000002;
		private const int RTLD_LOCAL = 0x00000004;

	}

	internal struct Loader
	{

		public static NativeLibrary LoadNativeLibrary()
		{
			ABI abi = Library.GetProcessABI();
			if (abi.Equals(ABI.Unknown))
				return null;

			INativeLibraryLoader loader = GetNativeLibraryLoader(abi);
			if (loader == null)
				return null;

			string resource = GetNativeLibraryResource(abi);
			if (resource == null)
				return null;

			string path = ExtractResource(resource);
			if (path == null)
				return null;

			return new NativeLibrary(path, loader);
		}

		private static INativeLibraryLoader GetNativeLibraryLoader(ABI abi)
		{
			if (abi.IsWindows())
				return new WindowsLibraryLoader();
			else if (abi.IsLinux())
				return new LinuxLibraryLoader();
			else if (abi.IsOSX())
				return new OSXLibraryLoader();
			else
				return null;
		}

		private static string GetNativeLibraryResource(ABI abi)
		{
			if (abi.Equals(ABI.Windows_X86))
				return "windows/x86/yeppp.dll";
			else if (abi.Equals(ABI.Windows_X86_64))
				return "windows/x86_64/yeppp.dll";
			else if (abi.Equals(ABI.OSX_X86))
				return "osx/x86/libyeppp.dylib";
			else if (abi.Equals(ABI.OSX_X86_64))
				return "osx/x86_64/libyeppp.dylib";
			else if (abi.Equals(ABI.Linux_X86))
				return "linux/x86/libyeppp.so";
			else if (abi.Equals(ABI.Linux_X86_64))
				return "linux/x86_64/libyeppp.so";
			else if (abi.Equals(ABI.Linux_ARMEL))
				return "linux/armel/libyeppp.so";
			else if (abi.Equals(ABI.Linux_ARMHF))
				return "linux/armhf/libyeppp.so";
			else
				return null;
		}

		private static string ExtractResource(string resource)
		{
			string path = null;
			try
			{
				path = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName());

				Assembly assembly = Assembly.GetExecutingAssembly();
				using (Stream resourceStream = assembly.GetManifestResourceStream(resource))
				{
					using (DeflateStream deflateStream = new DeflateStream(resourceStream, CompressionMode.Decompress))
					{
						using (FileStream fileStream = new FileStream(path, FileMode.CreateNew, FileAccess.Write, FileShare.None))
						{
							byte[] buffer = new byte[1048576];
							int bytesRead;
							do
							{
								bytesRead = deflateStream.Read(buffer, 0, buffer.Length);
								if (bytesRead != 0)
									fileStream.Write(buffer, 0, bytesRead);
							} while (bytesRead != 0);
						}
					}
				}
				return path;
			}
			catch
			{
				File.Delete(path);
				return null;
			}
		}

	}

}
