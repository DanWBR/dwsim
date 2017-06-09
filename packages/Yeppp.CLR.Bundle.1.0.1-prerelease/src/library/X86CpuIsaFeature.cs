/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

namespace Yeppp
{

	/// <summary>x86-specific ISA extensions.</summary>
	/// <seealso cref="Library.IsSupported(CpuIsaFeature)" />
	public sealed class X86CpuIsaFeature : CpuIsaFeature
	{

		/// <summary>x87 FPU integrated on chip.</summary>
		public static readonly X86CpuIsaFeature FPU        = new X86CpuIsaFeature(0);
		/// <summary>x87 CPUID instruction.</summary>
		public static readonly X86CpuIsaFeature Cpuid      = new X86CpuIsaFeature(1);
		/// <summary>RDTSC instruction.</summary>
		public static readonly X86CpuIsaFeature Rdtsc      = new X86CpuIsaFeature(2);
		/// <summary>CMOV, FCMOV, and FCOMI/FUCOMI instructions.</summary>
		public static readonly X86CpuIsaFeature CMOV       = new X86CpuIsaFeature(3);
		/// <summary>SYSENTER and SYSEXIT instructions.</summary>
		public static readonly X86CpuIsaFeature SYSENTER   = new X86CpuIsaFeature(4);
		/// <summary>SYSCALL and SYSRET instructions.</summary>
		public static readonly X86CpuIsaFeature SYSCALL    = new X86CpuIsaFeature(5);
		/// <summary>RDMSR and WRMSR instructions.</summary>
		public static readonly X86CpuIsaFeature MSR        = new X86CpuIsaFeature(6);
		/// <summary>CLFLUSH instruction.</summary>
		public static readonly X86CpuIsaFeature Clflush    = new X86CpuIsaFeature(7);
		/// <summary>MONITOR and MWAIT instructions.</summary>
		public static readonly X86CpuIsaFeature MONITOR    = new X86CpuIsaFeature(8);
		/// <summary>FXSAVE and FXRSTOR instructions.</summary>
		public static readonly X86CpuIsaFeature FXSAVE     = new X86CpuIsaFeature(9);
		/// <summary>XSAVE, XRSTOR, XGETBV, and XSETBV instructions.</summary>
		public static readonly X86CpuIsaFeature XSAVE      = new X86CpuIsaFeature(10);
		/// <summary>CMPXCHG8B instruction.</summary>
		public static readonly X86CpuIsaFeature Cmpxchg8b  = new X86CpuIsaFeature(11);
		/// <summary>CMPXCHG16B instruction.</summary>
		public static readonly X86CpuIsaFeature Cmpxchg16b = new X86CpuIsaFeature(12);
		/// <summary>Support for 64-bit mode.</summary>
		public static readonly X86CpuIsaFeature X64        = new X86CpuIsaFeature(13);
		/// <summary>Support for LAHF and SAHF instructions in 64-bit mode.</summary>
		public static readonly X86CpuIsaFeature LahfSahf64 = new X86CpuIsaFeature(14);
		/// <summary>RDFSBASE, RDGSBASE, WRFSBASE, and WRGSBASE instructions.</summary>
		public static readonly X86CpuIsaFeature FsGsBase   = new X86CpuIsaFeature(15);
		/// <summary>MOVBE instruction.</summary>
		public static readonly X86CpuIsaFeature Movbe      = new X86CpuIsaFeature(16);
		/// <summary>POPCNT instruction.</summary>
		public static readonly X86CpuIsaFeature Popcnt     = new X86CpuIsaFeature(17);
		/// <summary>LZCNT instruction.</summary>
		public static readonly X86CpuIsaFeature Lzcnt      = new X86CpuIsaFeature(18);
		/// <summary>BMI instruction set.</summary>
		public static readonly X86CpuIsaFeature BMI        = new X86CpuIsaFeature(19);
		/// <summary>BMI 2 instruction set.</summary>
		public static readonly X86CpuIsaFeature BMI2       = new X86CpuIsaFeature(20);
		/// <summary>TBM instruction set.</summary>
		public static readonly X86CpuIsaFeature TBM        = new X86CpuIsaFeature(21);
		/// <summary>RDRAND instruction.</summary>
		public static readonly X86CpuIsaFeature Rdrand     = new X86CpuIsaFeature(22);
		/// <summary>Padlock Advanced Cryptography Engine on chip.</summary>
		public static readonly X86CpuIsaFeature ACE        = new X86CpuIsaFeature(23);
		/// <summary>Padlock Advanced Cryptography Engine 2 on chip.</summary>
		public static readonly X86CpuIsaFeature ACE2       = new X86CpuIsaFeature(24);
		/// <summary>Padlock Random Number Generator on chip.</summary>
		public static readonly X86CpuIsaFeature RNG        = new X86CpuIsaFeature(25);
		/// <summary>Padlock Hash Engine on chip.</summary>
		public static readonly X86CpuIsaFeature PHE        = new X86CpuIsaFeature(26);
		/// <summary>Padlock Montgomery Multiplier on chip.</summary>
		public static readonly X86CpuIsaFeature PMM        = new X86CpuIsaFeature(27);
		/// <summary>AES instruction set.</summary>
		public static readonly X86CpuIsaFeature AES        = new X86CpuIsaFeature(28);
		/// <summary>PCLMULQDQ instruction.</summary>
		public static readonly X86CpuIsaFeature Pclmulqdq  = new X86CpuIsaFeature(29);
		/// <summary>RDTSCP instruction.</summary>
		public static readonly X86CpuIsaFeature Rdtscp     = new X86CpuIsaFeature(30);
		/// <summary>Lightweight Profiling extension.</summary>
		public static readonly X86CpuIsaFeature LPW        = new X86CpuIsaFeature(31);
		/// <summary>Hardware Lock Elision extension.</summary>
		public static readonly X86CpuIsaFeature HLE        = new X86CpuIsaFeature(32);
		/// <summary>Restricted Transactional Memory extension.</summary>
		public static readonly X86CpuIsaFeature RTM        = new X86CpuIsaFeature(33);
		/// <summary>XTEST instruction.</summary>
		public static readonly X86CpuIsaFeature Xtest      = new X86CpuIsaFeature(34);
		/// <summary>RDSEED instruction.</summary>
		public static readonly X86CpuIsaFeature Rdseed     = new X86CpuIsaFeature(35);
		/// <summary>ADCX and ADOX instructions.</summary>
		public static readonly X86CpuIsaFeature ADX        = new X86CpuIsaFeature(36);
		/// <summary>SHA instruction set.</summary>
		public static readonly X86CpuIsaFeature SHA        = new X86CpuIsaFeature(37);
		/// <summary>Memory Protection Extension.</summary>
		public static readonly X86CpuIsaFeature MPX        = new X86CpuIsaFeature(38);

		internal X86CpuIsaFeature(uint id) : base(id, CpuArchitecture.X86.Id)
		{
		}

	}

}
