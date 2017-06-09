/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

using System;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;

namespace Yeppp
{

	internal class Process
	{

		private enum OperatingSystem
		{
			Unknown,
			Windows,
			Linux,
			OSX
		}

		private static bool IsLinux()
		{
			try 
			{
				/* Linux provides the result of `uname -s` in procfs. Other OSes (e.g. OS X) may not have this file. */
				using (StreamReader reader = new StreamReader("/proc/sys/kernel/ostype"))
				{
					string ostype = reader.ReadLine();
					return ostype.Equals("Linux", StringComparison.InvariantCultureIgnoreCase);
				}
			}
			catch
			{
				return false;
			}
		}

		[DllImport("c")]
		private static extern int sysctl(int[] name, uint namelen, IntPtr oldDataBuffer, ref UIntPtr oldDataBufferSize, IntPtr newDataBuffer, UIntPtr newDataBufferSize);

		private const int CTL_KERN    = 1;
		private const int KERN_OSTYPE = 1;

		private static bool IsOSX()
		{
			int[] mib = { CTL_KERN, KERN_OSTYPE, 0 };
			UIntPtr bufferSize = UIntPtr.Zero;
			IntPtr buffer = IntPtr.Zero;
			try
			{
				if (sysctl(mib, 2, IntPtr.Zero, ref bufferSize, IntPtr.Zero, UIntPtr.Zero) != 0)
					return false;

				buffer = Marshal.AllocHGlobal(checked((int)bufferSize));
				if (sysctl(mib, 2, buffer, ref bufferSize, IntPtr.Zero, UIntPtr.Zero) != 0)
					return false;

				string ostype = Marshal.PtrToStringAnsi(buffer, checked((int)bufferSize));
				return ostype.Equals("Darwin", StringComparison.InvariantCultureIgnoreCase);
			}
			catch
			{
				return false;
			}
			finally
			{
				Marshal.FreeHGlobal(buffer);
			}
		}

		private static OperatingSystem DetectOperatingSystem()
		{
			uint platform = unchecked((uint)Environment.OSVersion.Platform);
			switch (platform)
			{
				case 2: /* PlatformID.Win32NT */
					return OperatingSystem.Windows;
				case 6: /* PlatformID.MacOSX */
					return OperatingSystem.OSX;
				case 4: /* PlatformID.Unix */
				case 128: /* Old Mono versions */
					if (IsLinux())
						return OperatingSystem.Linux;
					else if (IsOSX())
						return OperatingSystem.OSX;
					else
						return OperatingSystem.Unknown;
				default:
					return OperatingSystem.Unknown;
			}
		}

		[DllImport("kernel32")]
		private static extern void GetSystemInfo([MarshalAs(UnmanagedType.Struct)] out SystemInfo systemInfo);

		[StructLayout(LayoutKind.Sequential)]
		private struct SystemInfo
		{
			public ushort processorArchitecture;
			public ushort reserved;
			public uint pageSize;
			public IntPtr minimumApplicationAddress;
			public IntPtr maximumApplicationAddress;
			public UIntPtr activeProcessorMask;
			public uint numberOfProcessors;
			public uint processorType;
			public uint allocationGranularity;
			public ushort processorLevel;
			public ushort processorRevision;
		}

		private const ushort PROCESSOR_ARCHITECTURE_INTEL         = 0;
		private const ushort PROCESSOR_ARCHITECTURE_MIPS          = 1;
		private const ushort PROCESSOR_ARCHITECTURE_SHX           = 4;
		private const ushort PROCESSOR_ARCHITECTURE_ARM           = 5;
		private const ushort PROCESSOR_ARCHITECTURE_IA64          = 6;
		private const ushort PROCESSOR_ARCHITECTURE_AMD64         = 9;
		private const ushort PROCESSOR_ARCHITECTURE_IA32_ON_WIN64 = 10;

		private static ABI DetectWindowsABI()
		{
			try
			{
				SystemInfo systemInfo;
				GetSystemInfo(out systemInfo);
				switch (systemInfo.processorArchitecture)
				{
					case PROCESSOR_ARCHITECTURE_INTEL:
					case PROCESSOR_ARCHITECTURE_IA32_ON_WIN64:
						return ABI.Windows_X86;
					case PROCESSOR_ARCHITECTURE_ARM:
						return ABI.Windows_ARM;
					case PROCESSOR_ARCHITECTURE_IA64:
						return ABI.Windows_IA64;
					case PROCESSOR_ARCHITECTURE_AMD64:
						return ABI.Windows_X86_64;
					default:
						return ABI.Unknown;
				}
			}
			catch
			{
				return ABI.Unknown;
			}
		}

		[DllImport ("c")]
		private static extern int sysctlbyname([MarshalAs(UnmanagedType.LPStr)] string name, out int oldData, ref UIntPtr oldDataBufferSize, IntPtr newDataBuffer, UIntPtr newDataBufferSize);

		private const int CPU_TYPE_X86       = 0x00000007;
		private const int CPU_TYPE_X86_64    = 0x01000007;
		private const int CPU_TYPE_POWERPC   = 0x00000012;
		private const int CPU_TYPE_POWERPC64 = 0x01000012;

		private static ABI DetectOSXABI()
		{
			try
			{
				UIntPtr bufferSize = unchecked((UIntPtr)sizeof(int));
				int cpuType = 0;
				if (sysctlbyname("sysctl.proc_cputype", out cpuType, ref bufferSize, IntPtr.Zero, UIntPtr.Zero) != 0)
					return ABI.Unknown;

				switch (cpuType)
				{
					case CPU_TYPE_X86:
						return ABI.OSX_X86;
					case CPU_TYPE_X86_64:
						return ABI.OSX_X86_64;
					case CPU_TYPE_POWERPC:
						return ABI.OSX_PPC;
					case CPU_TYPE_POWERPC64:
						return ABI.OSX_PPC64;
					default:
						return ABI.Unknown;
				}
			}
			catch
			{
				return ABI.Unknown;
			}
		}

		/* Constants for ELF headers */
		private const int EI_MAG0 = 0;
		private const int EI_MAG1 = 1;
		private const int EI_MAG2 = 2;
		private const int EI_MAG3 = 3;
		private const int EI_CLASS = 4;
		private const int EI_DATA = 5;

		private const byte ELFCLASS32 = 1;
		private const byte ELFCLASS64 = 2;

		private const byte ELFDATA2LSB = 1;
		private const byte ELFDATA2MSB = 2;

		private const int EM_386    = 3;
		private const int EM_X86_64 = 62;
		private const int EM_IA64   = 50;
		private const int EM_K1OM   = 181;
		private const int EM_PPC    = 20;
		private const int EM_PPC64  = 21;
		private const int EM_ARM    = 40;
		private const int EM_ARM64  = 183;

		private const uint EF_ARM_ABIMASK        = 0xFF000000;
		private const uint EF_ARM_ABI_FLOAT_SOFT = 0x00000200;
		private const uint EF_ARM_ABI_FLOAT_HARD = 0x00000400;

		private const uint SHT_ARM_ATTRIBUTES = 0x70000003;

		/* ARM EABI build attributes */
		private const uint Tag_File = 1;
		private const uint Tag_CPU_raw_name = 4;
		private const uint Tag_CPU_name = 5;
		private const uint Tag_ABI_VFP_args = 28;
		private const uint Tag_compatibility = 32;

		private static ABI DetectLinuxABI()
		{
			try {
				using(FileStream fileStream = new FileStream("/proc/self/exe", FileMode.Open, FileAccess.Read, FileShare.Read))
				{
					byte[] identification = new byte[16];
					int bytesRead = fileStream.Read(identification, 0, identification.Length);
					if (bytesRead != identification.Length)
						return ABI.Unknown;

					if ((identification[EI_MAG0] == 0x7F) && (identification[EI_MAG1] == 'E') && (identification[EI_MAG2] == 'L') && (identification[EI_MAG3] == 'F'))
					{
						/* Detected ELF signature  */
						byte elfEndianess = identification[EI_DATA];
						if ((elfEndianess == ELFDATA2LSB) || (elfEndianess == ELFDATA2MSB))
						{
							/* Little endian headers and ABI */
							byte elfClass = identification[EI_CLASS];
							byte[] header = null;
							switch (elfClass)
							{
								case ELFCLASS32:
									/* ELF-32 file format */
									header = new byte[36];
									break;
								case ELFCLASS64:
									/* ELF-64 file format */
									header = new byte[48];
									break;
								default:
									return ABI.Unknown;
							}

							bytesRead = fileStream.Read(header, 0, header.Length);
							if (bytesRead != header.Length)
								return ABI.Unknown;

							ushort machine = (elfEndianess == ELFDATA2LSB) ?
								unchecked((ushort)(header[2] | (header[3] << 8))):
								unchecked((ushort)(header[3] | (header[2] << 8)));
							switch (machine)
							{
								case EM_386:
									if ((elfEndianess == ELFDATA2LSB) && (elfClass == ELFCLASS64))
										return ABI.Linux_X86;
									else
										return ABI.Unknown;
								case EM_X86_64:
									if (elfEndianess == ELFDATA2LSB)
										return (elfClass == ELFCLASS64) ? ABI.Linux_X86_64 : ABI.Linux_X32;
									else
										return ABI.Unknown;
								case EM_IA64:
									if ((elfEndianess == ELFDATA2LSB) && (elfClass == ELFCLASS64))
										return ABI.Linux_IA64;
									else
										return ABI.Unknown;
								case EM_ARM:
									if ((elfEndianess == ELFDATA2LSB) && (elfClass == ELFCLASS32))
										return DetectLinuxArmABI(fileStream, header);
									else
										return ABI.Unknown;
								case EM_ARM64:
									if ((elfEndianess == ELFDATA2LSB) && (elfClass == ELFCLASS64))
										return ABI.Linux_ARM64;
									else
										return ABI.Unknown;
								case EM_PPC:
									if ((elfEndianess == ELFDATA2MSB) && (elfClass == ELFCLASS32))
										return ABI.Linux_PPC;
									else
										return ABI.Unknown;
								case EM_PPC64:
									if ((elfEndianess == ELFDATA2MSB) && (elfClass == ELFCLASS64))
										return ABI.Linux_PPC64;
									else
										return ABI.Unknown;
							}
						}
					}
				}
				return ABI.Unknown;
			} catch {
				return ABI.Unknown;
			}
		}

		private static bool IsTagNTBS(uint tag)
		{
			switch (tag)
			{
				case Tag_CPU_raw_name:
				case Tag_CPU_name:
				case Tag_compatibility:
					return true;
				default:
					if (tag < 32)
						return false;
					else
						return (tag % 2) == 1;
			}
		}

		private static ABI DetectLinuxArmABI(FileStream fileStream, byte[] header)
		{
			int flags = header[20] | (header[21] << 8) | (header[22] << 16) | (header[23] << 24);
			/* Check that ELF header conforms to ARM EABI */
			if ((flags & EF_ARM_ABIMASK) == 0)
				return ABI.Linux_ARM;

			uint fpFlags = unchecked((uint)flags) & (EF_ARM_ABI_FLOAT_SOFT | EF_ARM_ABI_FLOAT_HARD);
			switch (fpFlags)
			{
				case EF_ARM_ABI_FLOAT_SOFT:
					/* Soft-float ARM EABI (armel) */
					return ABI.Linux_ARMEL;
				case EF_ARM_ABI_FLOAT_HARD:
					/* Hard-float ARM EABI (armhf) */
					return ABI.Linux_ARMHF;
				default:
					/* ARM EABI version (armel or armhf) is not specified here: need to parse sections */
					int sectionHeadersOffset = header[16] | (header[17] << 8) | (header[18] << 16) | (header[19] << 24);
					int sectionHeaderSize = header[30] | (header[31] << 8);
					int sectionCount = header[32] | (header[33] << 8);

					/* Check the header size */
					if (sectionHeaderSize != 40)
						return ABI.Unknown;

					/* Skip the null section */
					fileStream.Seek(sectionHeadersOffset + sectionHeaderSize, SeekOrigin.Begin);
					for (int sectionIndex = 1; sectionIndex < sectionCount; sectionIndex++)
					{
						/* Read section header */
						byte[] sectionHeader = new byte[sectionHeaderSize];
						int bytesRead = fileStream.Read(sectionHeader, 0, sectionHeader.Length);
						if (bytesRead == sectionHeader.Length)
						{
							int sectionType = sectionHeader[4] | (sectionHeader[5] << 8) | (sectionHeader[6] << 16) | (sectionHeader[7] << 24);
							if (sectionType == SHT_ARM_ATTRIBUTES)
							{
								/* Found .ARM.attributes section. Now read it into memory. */
								int sectionOffset = sectionHeader[16] | (sectionHeader[17] << 8) | (sectionHeader[18] << 16) | (sectionHeader[19] << 24);
								fileStream.Seek(sectionOffset, SeekOrigin.Begin);
								int sectionSize = sectionHeader[20] | (sectionHeader[21] << 8) | (sectionHeader[22] << 16) | (sectionHeader[23] << 24);
								byte[] section = new byte[sectionSize];
								bytesRead = fileStream.Read(section, 0, section.Length);
								if (bytesRead != section.Length)
									return ABI.Unknown;

								/* Verify that it has known format version */
								byte formatVersion = section[0];
								if (formatVersion != 'A')
									return ABI.Unknown;

								/* Iterate build attribute sections. We look for "aeabi" attributes section. */
								int attributesSectionOffset = 1;
								while (attributesSectionOffset < sectionSize)
								{
									int attributesSectionLength = section[attributesSectionOffset] |
										(section[attributesSectionOffset+1] << 8) |
										(section[attributesSectionOffset+2] << 16) |
										(section[attributesSectionOffset+3] << 24);
									if (attributesSectionLength > 10)
									{
										/* Check if attributes section name if "aeabi" */
										if ((section[attributesSectionOffset+4] == 'a') &&
											(section[attributesSectionOffset+5] == 'e') &&
											(section[attributesSectionOffset+6] == 'a') &&
											(section[attributesSectionOffset+7] == 'b') &&
											(section[attributesSectionOffset+8] == 'i') &&
											(section[attributesSectionOffset+9] == 0))
										{

											/* Iterate build attribute subsections. */
											int attributesSubsectionOffset = attributesSectionOffset + 10;
											while (attributesSubsectionOffset < attributesSectionOffset + attributesSectionLength) {
												int attributesSubsectionLength = section[attributesSubsectionOffset+1] |
													(section[attributesSubsectionOffset+2] << 8) |
													(section[attributesSubsectionOffset+3] << 16) |
													(section[attributesSubsectionOffset+4] << 24);
												/* We look for subsection of attributes for the whole file. */
												int attributesSubsectionTag = section[attributesSubsectionOffset];
												if (attributesSubsectionTag == Tag_File)
												{
													/* Now read tag: value pairs */
													int tagOffset = attributesSubsectionOffset + 5;
													while (tagOffset < attributesSubsectionOffset + attributesSubsectionLength)
													{
														/* Read ULEB128-encoded integer */
														sbyte tagByte = unchecked((sbyte)section[tagOffset++]);
														uint tag = unchecked((uint)(tagByte & 0x7F));
														while (tagByte < 0)
														{
															tagByte = unchecked((sbyte)section[tagOffset++]);
															tag = (tag << 7) | unchecked((uint)(tagByte & 0x7F));
														}
														if (IsTagNTBS(tag))
														{
															/* Null-terminated string. Skip. */
															while (section[tagOffset++] != 0);
														}
														else
														{
															/* ULEB128-encoded integer. Parse. */
															sbyte valueByte = unchecked((sbyte)section[tagOffset++]);
															uint value = unchecked((uint)(valueByte & 0x7F));
															while (valueByte < 0)
															{
																valueByte = unchecked((sbyte)section[tagOffset++]);
																value = (value << 7) | unchecked((uint)(valueByte & 0x7F));
															}
															if (tag == Tag_ABI_VFP_args)
															{
																switch (value)
																{
																	case 0:
																		/* The user intended FP parameter/result passing to conform to AAPCS, base variant. */
																		return ABI.Linux_ARMEL;
																	case 1:
																		/* The user intended FP parameter/result passing to conform to AAPCS, VFP variant. */
																		return ABI.Linux_ARMHF;
																	case 2:
																		/* The user intended FP parameter/result passing to conform to tool chain-specific conventions. */
																		return ABI.Unknown;
																	case 3:
																		/* Code is compatible with both the base and VFP variants; the user did not permit non-variadic functions to pass FP parameters/result. */
																		return ABI.Linux_ARMEL;
																	default:
																		return ABI.Unknown;
																}
															}
														}
													}
												}
												attributesSubsectionOffset += attributesSubsectionLength;
											}
										}
									}
									attributesSectionOffset += attributesSectionLength;
								}
								/* If no Tag_ABI_VFP_args is present, assume default value (soft-float). */
								return ABI.Linux_ARMEL;
							}
						}
					}
					/* EABI attributed section not found: unknown EABI variant */
					return ABI.Unknown;
			}
		}

		public static ABI DetectABI()
		{
			OperatingSystem operatingSystem = DetectOperatingSystem();
			switch (operatingSystem) {
				case OperatingSystem.Windows:
					return DetectWindowsABI();
				case OperatingSystem.OSX:
					/* https://www.opensource.apple.com/source/top/top-73/libtop.c */
					/* https://stackoverflow.com/questions/1350181/determine-a-processs-architecture */
					return DetectOSXABI();
				case OperatingSystem.Linux:
					return DetectLinuxABI();
				default:
					return ABI.Unknown;
			}
		}

	}

}
