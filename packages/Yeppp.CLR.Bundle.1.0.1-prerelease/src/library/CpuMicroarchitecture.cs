/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

namespace Yeppp
{

	/// <summary>Type of processor microarchitecture.</summary>
	/// <remarks>Low-level instruction performance characteristics, such as latency and throughput, are constant within microarchitecture.</remarks>
	/// <remarks>Processors of the same microarchitecture can differ in supported instruction sets and other extensions.</remarks>
	/// <seealso cref="Library.GetCpuMicroarchitecture" />
	public struct CpuMicroarchitecture
	{

		/// <summary>Microarchitecture is unknown, or the library failed to get information about the microarchitecture from OS</summary>
		public static readonly CpuMicroarchitecture Unknown           = new CpuMicroarchitecture(0);

		/// <summary>Pentium and Pentium MMX microarchitecture.</summary>
		public static readonly CpuMicroarchitecture P5                = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x0001);
		/// <summary>Pentium Pro, Pentium II, and Pentium III.</summary>
		public static readonly CpuMicroarchitecture P6                = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x0002);
		/// <summary>Pentium 4 with Willamette, Northwood, or Foster cores.</summary>
		public static readonly CpuMicroarchitecture Willamette        = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x0003);
		/// <summary>Pentium 4 with Prescott and later cores.</summary>
		public static readonly CpuMicroarchitecture Prescott          = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x0004);
		/// <summary>Pentium M.</summary>
		public static readonly CpuMicroarchitecture Dothan            = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x0005);
		/// <summary>Intel Core microarchitecture.</summary>
		public static readonly CpuMicroarchitecture Yonah             = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x0006);
		/// <summary>Intel Core 2 microarchitecture on 65 nm process.</summary>
		public static readonly CpuMicroarchitecture Conroe            = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x0007);
		/// <summary>Intel Core 2 microarchitecture on 45 nm process.</summary>
		public static readonly CpuMicroarchitecture Penryn            = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x0008);
		/// <summary>Intel Atom on 45 nm process.</summary>
		public static readonly CpuMicroarchitecture Bonnell           = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x0009);
		/// <summary>Intel Nehalem and Westmere microarchitectures (Core i3/i5/i7 1st gen).</summary>
		public static readonly CpuMicroarchitecture Nehalem           = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x000A);
		/// <summary>Intel Sandy Bridge microarchitecture (Core i3/i5/i7 2nd gen).</summary>
		public static readonly CpuMicroarchitecture SandyBridge       = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x000B);
		/// <summary>Intel Atom on 32 nm process.</summary>
		public static readonly CpuMicroarchitecture Saltwell          = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x000C);
		/// <summary>Intel Ivy Bridge microarchitecture (Core i3/i5/i7 3rd gen).</summary>
		public static readonly CpuMicroarchitecture IvyBridge         = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x000D);
		/// <summary>Intel Haswell microarchitecture (Core i3/i5/i7 4th gen).</summary>
		public static readonly CpuMicroarchitecture Haswell           = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x000E);
		/// <summary> Intel Silvermont microarchitecture (22 nm out-of-order Atom).</summary>
		public static readonly CpuMicroarchitecture Silvermont        = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x000F);

		/// <summary>Intel Knights Ferry HPC boards.</summary>
		public static readonly CpuMicroarchitecture KnightsFerry      = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x0100);
		/// <summary>Intel Knights Corner HPC boards (aka Xeon Phi).</summary>
		public static readonly CpuMicroarchitecture KnightsCorner     = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x0101);

		/// <summary>AMD K5.</summary>
		public static readonly CpuMicroarchitecture K5                = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.AMD.Id      << 16) + 0x0001);
		/// <summary>AMD K6 and alike.</summary>
		public static readonly CpuMicroarchitecture K6                = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.AMD.Id      << 16) + 0x0002);
		/// <summary>AMD Athlon and Duron.</summary>
		public static readonly CpuMicroarchitecture K7                = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.AMD.Id      << 16) + 0x0003);
		/// <summary>AMD Geode GX and LX.</summary>
		public static readonly CpuMicroarchitecture Geode             = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.AMD.Id      << 16) + 0x0004);
		/// <summary>AMD Athlon 64, Opteron 64.</summary>
		public static readonly CpuMicroarchitecture K8                = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.AMD.Id      << 16) + 0x0005);
		/// <summary>AMD K10 (Barcelona, Istambul, Magny-Cours).</summary>
		public static readonly CpuMicroarchitecture K10               = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.AMD.Id      << 16) + 0x0006);
		/// <summary>AMD Bobcat mobile microarchitecture.</summary>
		public static readonly CpuMicroarchitecture Bobcat            = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.AMD.Id      << 16) + 0x0007);
		/// <summary>AMD Bulldozer microarchitecture (1st gen K15).</summary>
		public static readonly CpuMicroarchitecture Bulldozer         = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.AMD.Id      << 16) + 0x0008);
		/// <summary>AMD Piledriver microarchitecture (2nd gen K15).</summary>
		public static readonly CpuMicroarchitecture Piledriver        = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.AMD.Id      << 16) + 0x0009);
		/// <summary>AMD Jaguar mobile microarchitecture.</summary>
		public static readonly CpuMicroarchitecture Jaguar            = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.AMD.Id      << 16) + 0x000A);
		/// <summary>AMD Steamroller microarchitecture (3rd gen K15).</summary>
		public static readonly CpuMicroarchitecture Steamroller       = new CpuMicroarchitecture((CpuArchitecture.X86.Id << 24) + (CpuVendor.AMD.Id      << 16) + 0x000B);

		/// <summary>DEC/Intel StrongARM processors.</summary>
		public static readonly CpuMicroarchitecture StrongARM         = new CpuMicroarchitecture((CpuArchitecture.ARM.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x0001);
		/// <summary>Intel/Marvell XScale processors.</summary>
		public static readonly CpuMicroarchitecture XScale            = new CpuMicroarchitecture((CpuArchitecture.ARM.Id << 24) + (CpuVendor.Intel.Id    << 16) + 0x0002);

		/// <summary>ARM7 series.</summary>
		public static readonly CpuMicroarchitecture ARM7              = new CpuMicroarchitecture((CpuArchitecture.ARM.Id << 24) + (CpuVendor.ARM.Id      << 16) + 0x0001);
		/// <summary>ARM9 series.</summary>
		public static readonly CpuMicroarchitecture ARM9              = new CpuMicroarchitecture((CpuArchitecture.ARM.Id << 24) + (CpuVendor.ARM.Id      << 16) + 0x0002);
		/// <summary>ARM 1136, ARM 1156, ARM 1176, or ARM 11MPCore.</summary>
		public static readonly CpuMicroarchitecture ARM11             = new CpuMicroarchitecture((CpuArchitecture.ARM.Id << 24) + (CpuVendor.ARM.Id      << 16) + 0x0003);
		/// <summary>ARM Cortex-A5.</summary>
		public static readonly CpuMicroarchitecture CortexA5          = new CpuMicroarchitecture((CpuArchitecture.ARM.Id << 24) + (CpuVendor.ARM.Id      << 16) + 0x0004);
		/// <summary>ARM Cortex-A7.</summary>
		public static readonly CpuMicroarchitecture CortexA7          = new CpuMicroarchitecture((CpuArchitecture.ARM.Id << 24) + (CpuVendor.ARM.Id      << 16) + 0x0005);
		/// <summary>ARM Cortex-A8.</summary>
		public static readonly CpuMicroarchitecture CortexA8          = new CpuMicroarchitecture((CpuArchitecture.ARM.Id << 24) + (CpuVendor.ARM.Id      << 16) + 0x0006);
		/// <summary>ARM Cortex-A9.</summary>
		public static readonly CpuMicroarchitecture CortexA9          = new CpuMicroarchitecture((CpuArchitecture.ARM.Id << 24) + (CpuVendor.ARM.Id      << 16) + 0x0007);
		/// <summary>ARM Cortex-A15.</summary>
		public static readonly CpuMicroarchitecture CortexA15         = new CpuMicroarchitecture((CpuArchitecture.ARM.Id << 24) + (CpuVendor.ARM.Id      << 16) + 0x0008);

		/// <summary>Qualcomm Scorpion.</summary>
		public static readonly CpuMicroarchitecture Scorpion          = new CpuMicroarchitecture((CpuArchitecture.ARM.Id << 24) + (CpuVendor.Qualcomm.Id << 16) + 0x0001);
		/// <summary>Qualcomm Krait.</summary>
		public static readonly CpuMicroarchitecture Krait             = new CpuMicroarchitecture((CpuArchitecture.ARM.Id << 24) + (CpuVendor.Qualcomm.Id << 16) + 0x0002);

		/// <summary>Marvell Sheeva PJ1.</summary>
		public static readonly CpuMicroarchitecture PJ1               = new CpuMicroarchitecture((CpuArchitecture.ARM.Id << 24) + (CpuVendor.Marvell.Id  << 16) + 0x0001);
		/// <summary>Marvell Sheeva PJ4.</summary>
		public static readonly CpuMicroarchitecture PJ4               = new CpuMicroarchitecture((CpuArchitecture.ARM.Id << 24) + (CpuVendor.Marvell.Id  << 16) + 0x0002);

		/// <summary>Apple A6 and A6X processors.</summary>
		public static readonly CpuMicroarchitecture Swift             = new CpuMicroarchitecture((CpuArchitecture.ARM.Id << 24) + (CpuVendor.Apple.Id    << 16) + 0x0001);
		/// <summary>Apple A7 processor.</summary>
		public static readonly CpuMicroarchitecture Cyclone           = new CpuMicroarchitecture((CpuArchitecture.ARM.Id << 24) + (CpuVendor.Apple.Id    << 16) + 0x0002);

		/// <summary>Intel Itanium.</summary>
		public static readonly CpuMicroarchitecture Itanium           = new CpuMicroarchitecture((CpuArchitecture.IA64.Id << 24) + (CpuVendor.Intel.Id   << 16) + 0x0001);
		/// <summary>Intel Itanium 2.</summary>
		public static readonly CpuMicroarchitecture Itanium2          = new CpuMicroarchitecture((CpuArchitecture.IA64.Id << 24) + (CpuVendor.Intel.Id   << 16) + 0x0002);

		/// <summary>MIPS 24K.</summary>
		public static readonly CpuMicroarchitecture MIPS24K           = new CpuMicroarchitecture((CpuArchitecture.MIPS.Id << 24) + (CpuVendor.MIPS.Id    << 16) + 0x0001);
		/// <summary>MIPS 34K.</summary>
		public static readonly CpuMicroarchitecture MIPS34K           = new CpuMicroarchitecture((CpuArchitecture.MIPS.Id << 24) + (CpuVendor.MIPS.Id    << 16) + 0x0002);
		/// <summary>MIPS 74K.</summary>
		public static readonly CpuMicroarchitecture MIPS74K           = new CpuMicroarchitecture((CpuArchitecture.MIPS.Id << 24) + (CpuVendor.MIPS.Id    << 16) + 0x0003);

		/// <summary>Ingenic XBurst.</summary>
		public static readonly CpuMicroarchitecture XBurst            = new CpuMicroarchitecture((CpuArchitecture.MIPS.Id << 24) + (CpuVendor.Ingenic.Id << 16) + 0x0001);
		/// <summary>Ingenic XBurst 2.</summary>
		public static readonly CpuMicroarchitecture XBurst2           = new CpuMicroarchitecture((CpuArchitecture.MIPS.Id << 24) + (CpuVendor.Ingenic.Id << 16) + 0x0002);

		/// <summary>IBM PowerPC 970 (PowerPC G5).</summary>
		public static readonly CpuMicroarchitecture PowerPC970        = new CpuMicroarchitecture((CpuArchitecture.PowerPC.Id << 24) + (CpuVendor.IBM.Id << 16) + 0x0021);

		/// <summary>IBM POWER 4.</summary>
		public static readonly CpuMicroarchitecture POWER4            = new CpuMicroarchitecture((CpuArchitecture.PowerPC.Id << 24) + (CpuVendor.IBM.Id << 16) + 0x011F);
		/// <summary>IBM POWER 5.</summary>
		public static readonly CpuMicroarchitecture POWER5            = new CpuMicroarchitecture((CpuArchitecture.PowerPC.Id << 24) + (CpuVendor.IBM.Id << 16) + 0x0120);
		/// <summary>IBM POWER 6.</summary>
		public static readonly CpuMicroarchitecture POWER6            = new CpuMicroarchitecture((CpuArchitecture.PowerPC.Id << 24) + (CpuVendor.IBM.Id << 16) + 0x0121);
		/// <summary>IBM POWER 7.</summary>
		public static readonly CpuMicroarchitecture POWER7            = new CpuMicroarchitecture((CpuArchitecture.PowerPC.Id << 24) + (CpuVendor.IBM.Id << 16) + 0x0122);
		/// <summary>IBM POWER 8.</summary>
		public static readonly CpuMicroarchitecture POWER8            = new CpuMicroarchitecture((CpuArchitecture.PowerPC.Id << 24) + (CpuVendor.IBM.Id << 16) + 0x0123);

		/// <summary>IBM PowerPC 440 (Blue Gene/L processor).</summary>
		public static readonly CpuMicroarchitecture PowerPC440        = new CpuMicroarchitecture((CpuArchitecture.PowerPC.Id << 24) + (CpuVendor.IBM.Id << 16) + 0x0200);
		/// <summary>IBM PowerPC 450 (Blue Gene/P processor).</summary>
		public static readonly CpuMicroarchitecture PowerPC450        = new CpuMicroarchitecture((CpuArchitecture.PowerPC.Id << 24) + (CpuVendor.IBM.Id << 16) + 0x0201);
		/// <summary>IBM PowerPC A2 (Blue Gene/Q processor).</summary>
		public static readonly CpuMicroarchitecture PowerPCA2         = new CpuMicroarchitecture((CpuArchitecture.PowerPC.Id << 24) + (CpuVendor.IBM.Id << 16) + 0x0202);

		/// <summary>P.A.Semi PWRficient.</summary>
		public static readonly CpuMicroarchitecture PWRficient        = new CpuMicroarchitecture((CpuArchitecture.PowerPC.Id << 24) + (CpuVendor.PASemi.Id << 16) + 0x0001);

		private readonly uint id;

		internal CpuMicroarchitecture(uint id)
		{
			this.id = id;
		}

		internal uint Id
		{
			get
			{
				return this.id;
			}
		}

		/// <summary>Compares for equality with another <see cref="CpuMicroarchitecture" /> object.</summary>
		/// <remarks>Comparison is performed by value.</remarks>
		public bool Equals(CpuMicroarchitecture other)
		{
			return this.id == other.id;
		}

		/// <summary>Compares for equality with another object.</summary>
		/// <remarks>Comparison is performed by value.</remarks>
		public override bool Equals(System.Object other)
		{
			if (other == null || GetType() != other.GetType())
				return false;

			return this.Equals((CpuMicroarchitecture)other);
		}

		/// <summary>Provides a hash for the object.</summary>
		/// <remarks>Non-equal <see cref="CpuMicroarchitecture" /> objects are guaranteed to have different hashes.</remarks>
		public override int GetHashCode()
		{
			return unchecked((int)this.id);
		}


		/// <summary>Provides a string ID for the object.</summary>
		/// <remarks>The string ID starts with a Latin letter and contains only Latin letters, digits, and underscore symbol.</remarks>
		/// <seealso cref="Description" />
		public override string ToString()
		{
			return Library.GetString(Enumeration.CpuMicroarchitecture, this.id, StringType.ID);
		}

		/// <summary>Provides a description for the object.</summary>
		/// <remarks>The description can contain spaces and non-ASCII characters.</remarks>
		/// <seealso cref="ToString()" />
		public string Description
		{
			get
			{
				return Library.GetString(Enumeration.CpuMicroarchitecture, this.id, StringType.Description);
			}
		}

	}

}
