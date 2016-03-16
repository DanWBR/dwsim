/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

using System;

namespace Yeppp
{

	internal class NativeLibrary : IDisposable
	{

		private string path;
		private IntPtr handle;
		private readonly INativeLibraryLoader loader;

		public NativeLibrary(string path, INativeLibraryLoader loader)
		{
			this.path = path;
			this.loader = loader;
			this.handle = loader.LoadLibrary(path);
			if (this.handle == IntPtr.Zero)
			{
				throw new DllNotFoundException(path);
			}
		}

		~NativeLibrary()
		{
			Dispose(false);
		}

		public Delegate GetFunction(string name, Type type)
		{
			IntPtr function = loader.FindFunction(this.handle, name);
			if (function == IntPtr.Zero)
				throw new EntryPointNotFoundException(name);
			return System.Runtime.InteropServices.Marshal.GetDelegateForFunctionPointer(function, type);
		}

		public void Dispose()
		{
			Dispose(true);
			GC.SuppressFinalize(this);
		}

		private void Dispose(bool disposing)
		{
			if (this.handle != IntPtr.Zero)
			{
				this.loader.UnloadLibrary(this.handle);
				this.handle = IntPtr.Zero;
			}
			if (this.path != null)
			{
				try
				{
					System.IO.File.Delete(path);
				}
				catch
				{
				}
				this.path = null;
			}
		}

	}

}
