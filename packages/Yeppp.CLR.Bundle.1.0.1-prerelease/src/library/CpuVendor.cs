/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

namespace Yeppp
{

	/// <summary>The company which designed the processor microarchitecture.</summary>
	/// <seealso cref="Library.GetCpuVendor" />
	public struct CpuVendor
	{

		/// <summary>Processor vendor is not known to the library, or the library failed to get vendor information from the OS.</summary>
		public static readonly CpuVendor Unknown            = new CpuVendor(0);

		/* x86/x86-64 CPUs */

		/// <summary>Intel Corporation. Vendor of x86, x86-64, IA64, and ARM processor microarchitectures.</summary>
		/// <remarks>Sold its ARM design subsidiary in 2006. The last ARM processor design was released in 2004.</remarks>
		public static readonly CpuVendor Intel              = new CpuVendor(1);
		/// <summary>Advanced Micro Devices, Inc. Vendor of x86 and x86-64 processor microarchitectures.</summary>
		public static readonly CpuVendor AMD                = new CpuVendor(2);
		/// <summary>VIA Technologies, Inc. Vendor of x86 and x86-64 processor microarchitectures.</summary>
		/// <remarks>Processors are designed by Centaur Technology, a subsidiary of VIA Technologies.</remarks>
		public static readonly CpuVendor VIA                = new CpuVendor(3);
		/// <summary>Transmeta Corporation. Vendor of x86 processor microarchitectures.</summary>
		/// <remarks>Now defunct. The last processor design was released in 2004.</remarks>
		/// <remarks>Transmeta processors implemented VLIW ISA and used binary translation to execute x86 code.</remarks>
		public static readonly CpuVendor Transmeta          = new CpuVendor(4);
		/// <summary>Cyrix Corporation. Vendor of x86 processor microarchitectures.</summary>
		/// <remarks>Now defunct. The last processor design was released in 1996.</remarks>
		public static readonly CpuVendor Cyrix              = new CpuVendor(5);
		/// <summary>Rise Technology. Vendor of x86 processor microarchitectures.</summary>
		/// <remarks>Now defunct. The last processor design was released in 1999.</remarks>
		public static readonly CpuVendor Rise               = new CpuVendor(6);
		/// <summary>National Semiconductor. Vendor of x86 processor microarchitectures.</summary>
		/// <remarks>Sold its x86 design subsidiary in 1999. The last processor design was released in 1998.</remarks>
		public static readonly CpuVendor NSC                = new CpuVendor(7);
		/// <summary>Silicon Integrated Systems. Vendor of x86 processor microarchitectures.</summary>
		/// <remarks>Sold its x86 design subsidiary in 2001. The last processor design was released in 2001.</remarks>
		public static readonly CpuVendor SiS                = new CpuVendor(8);
		/// <summary>NexGen. Vendor of x86 processor microarchitectures.</summary>
		/// <remarks>Now defunct. The last processor design was released in 1994.</remarks>
		/// <remarks>NexGen designed the first x86 microarchitecture which decomposed x86 instructions into simple microoperations.</remarks>
		public static readonly CpuVendor NexGen             = new CpuVendor(9);
		/// <summary>United Microelectronics Corporation. Vendor of x86 processor microarchitectures.</summary>
		/// <remarks>Ceased x86 in the early 1990s. The last processor design was released in 1991.</remarks>
		/// <remarks>Designed U5C and U5D processors. Both are 486 level.</remarks>
		public static readonly CpuVendor UMC                = new CpuVendor(10);
		/// <summary>RDC Semiconductor Co., Ltd. Vendor of x86 processor microarchitectures.</summary>
		/// <remarks>Designes embedded x86 CPUs.</remarks>
		public static readonly CpuVendor RDC                = new CpuVendor(11);
		/// <summary>DM&amp;P Electronics Inc. Vendor of x86 processor microarchitectures.</summary>
		/// <remarks>Mostly embedded x86 designs.</remarks>
		public static readonly CpuVendor DMP                = new CpuVendor(12);
		
		/* ARM CPUs */
		
		/// <summary>ARM Holdings plc. Vendor of ARM processor microarchitectures.</summary>
		public static readonly CpuVendor ARM                = new CpuVendor(20);
		/// <summary>Marvell Technology Group Ltd. Vendor of ARM processor microarchitectures.</summary>
		public static readonly CpuVendor Marvell            = new CpuVendor(21);
		/// <summary>Qualcomm Incorporated. Vendor of ARM processor microarchitectures.</summary>
		public static readonly CpuVendor Qualcomm           = new CpuVendor(22);
		/// <summary>Digital Equipment Corporation. Vendor of ARM processor microarchitecture.</summary>
		/// <remarks>Sold its ARM designs in 1997. The last processor design was released in 1997.</remarks>
		public static readonly CpuVendor DEC                = new CpuVendor(23);
		/// <summary>Texas Instruments Inc. Vendor of ARM processor microarchitectures.</summary>
		public static readonly CpuVendor TI                 = new CpuVendor(24);
		/// <summary>Apple Inc. Vendor of ARM processor microarchitectures.</summary>
		public static readonly CpuVendor Apple              = new CpuVendor(25);

		/* MIPS CPUs */

		/// <summary>Ingenic Semiconductor. Vendor of MIPS processor microarchitectures.</summary>
		public static readonly CpuVendor Ingenic            = new CpuVendor(40);
		/// <summary>Institute of Computing Technology of the Chinese Academy of Sciences. Vendor of MIPS processor microarchitectures.</summary>
		public static readonly CpuVendor ICT                = new CpuVendor(41);
		/// <summary>MIPS Technologies, Inc. Vendor of MIPS processor microarchitectures.</summary>
		public static readonly CpuVendor MIPS               = new CpuVendor(42);

		/* PowerPC CPUs */

		/// <summary>International Business Machines Corporation. Vendor of PowerPC processor microarchitectures.</summary>
		public static readonly CpuVendor IBM                = new CpuVendor(50);
		/// <summary>Motorola, Inc. Vendor of PowerPC and ARM processor microarchitectures.</summary>
		public static readonly CpuVendor Motorola           = new CpuVendor(51);
		/// <summary>P. A. Semi. Vendor of PowerPC processor microarchitectures.</summary>
		/// <remarks>Now defunct. The last processor design was released in 2007.</remarks>
		public static readonly CpuVendor PASemi             = new CpuVendor(52);

		/* SPARC CPUs */

		/// <summary>Sun Microsystems, Inc. Vendor of SPARC processor microarchitectures.</summary>
		/// <remarks>Now defunct. The last processor design was released in 2008.</remarks>
		public static readonly CpuVendor Sun                = new CpuVendor(60);
		/// <summary>Oracle Corporation. Vendor of SPARC processor microarchitectures.</summary>
		public static readonly CpuVendor Oracle             = new CpuVendor(61);
		/// <summary>Fujitsu Limited. Vendor of SPARC processor microarchitectures.</summary>
		public static readonly CpuVendor Fujitsu            = new CpuVendor(62);
		/// <summary>Moscow Center of SPARC Technologies CJSC. Vendor of SPARC processor microarchitectures.</summary>
		public static readonly CpuVendor MCST               = new CpuVendor(63);

		private readonly uint id;

		internal CpuVendor(uint id)
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

		/// <summary>Compares for equality with another <see cref="CpuVendor" /> object.</summary>
		/// <remarks>Comparison is performed by value.</remarks>
		public bool Equals(CpuVendor other)
		{
			return this.id == other.id;
		}

		/// <summary>Compares for equality with another object.</summary>
		/// <remarks>Comparison is performed by value.</remarks>
		public override bool Equals(System.Object other)
		{
			if (other == null || GetType() != other.GetType())
				return false;

			return this.Equals((CpuVendor)other);
		}

		/// <summary>Provides a hash for the object.</summary>
		/// <remarks>Non-equal <see cref="CpuVendor" /> objects are guaranteed to have different hashes.</remarks>
		public override int GetHashCode()
		{
			return unchecked((int)this.id);
		}

		/// <summary>Provides a string ID for the object.</summary>
		/// <remarks>The string ID starts with a Latin letter and contains only Latin letters, digits, and underscore symbol.</remarks>
		/// <seealso cref="Description" />
		public override string ToString()
		{
			return Library.GetString(Enumeration.CpuVendor, this.id, StringType.ID);
		}

		/// <summary>Provides a description for the object.</summary>
		/// <remarks>The description can contain spaces and non-ASCII characters.</remarks>
		/// <seealso cref="ToString()" />
		public string Description
		{
			get
			{
				return Library.GetString(Enumeration.CpuVendor, this.id, StringType.Description);
			}
		}

	}

}
