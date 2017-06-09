/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

namespace Yeppp
{

	/// <summary>Non-ISA processor or system features.</summary>
	/// <seealso cref="CpuArchitecture.CpuSystemFeatures" />
	/// <seealso cref="Library.IsSupported(CpuSystemFeature)" />
	/// <seealso cref="X86CpuSystemFeature" />
	/// <seealso cref="ArmCpuSystemFeature" />
	public class CpuSystemFeature {

		/// <summary>The processor has a built-in cycle counter, and the operating system provides a way to access it.</summary>
		public static readonly CpuSystemFeature CycleCounter      = new CpuSystemFeature(0);
		/// <summary>The processor has a 64-bit cycle counter, or the operating system provides an abstraction of a 64-bit cycle counter.</summary>
		public static readonly CpuSystemFeature CycleCounter64Bit = new CpuSystemFeature(1);
		/// <summary>The processor and the operating system allows to use 64-bit pointers.</summary>
		public static readonly CpuSystemFeature AddressSpace64Bit = new CpuSystemFeature(2);
		/// <summary>The processor and the operating system allows to do 64-bit arithmetical operations on general-purpose registers.</summary>
		public static readonly CpuSystemFeature GPRegisters64Bit  = new CpuSystemFeature(3);
		/// <summary>The processor and the operating system allows misaligned memory reads and writes.</summary>
		public static readonly CpuSystemFeature MisalignedAccess  = new CpuSystemFeature(4);
		/// <summary>The processor or the operating system support at most one hardware thread.</summary>
		public static readonly CpuSystemFeature SingleThreaded    = new CpuSystemFeature(5);

		private readonly uint architectureId;
		private readonly uint id;

		internal CpuSystemFeature(uint id, uint architectureId) {
			this.id = id;
			this.architectureId = architectureId;
		}

		internal CpuSystemFeature(uint id) {
			this.id = id;
			this.architectureId = CpuArchitecture.Unknown.Id;
		}

		internal uint Id
		{
			get
			{
				return this.id;
			}
		}

		internal uint ArchitectureId
		{
			get
			{
				return this.architectureId;
			}
		}

		/// <summary>Compares for equality with another <see cref="CpuSystemFeature" /> object.</summary>
		/// <remarks>Comparison is performed by value.</remarks>
		public bool Equals(CpuSystemFeature other)
		{
			if (other == null)
				return false;

			return ((this.id ^ other.id) | (this.architectureId ^ other.architectureId)) == 0;
		}

		/// <summary>Compares for equality with another object.</summary>
		/// <remarks>Comparison is performed by value.</remarks>
		public override bool Equals(System.Object other)
		{
			if (other == null || GetType() != other.GetType())
				return false;

			return this.Equals((CpuSystemFeature)other);
		}

		/// <summary>Provides a hash for the object.</summary>
		/// <remarks>Non-equal <see cref="CpuSystemFeature" /> objects are guaranteed to have different hashes.</remarks>
		public override int GetHashCode()
		{
			return unchecked((int)(this.id ^ (this.architectureId << 16)));
		}

		/// <summary>Provides a string ID for the object.</summary>
		/// <remarks>The string ID starts with a Latin letter and contains only Latin letters, digits, and underscore symbol.</remarks>
		/// <seealso cref="Description" />
		public override string ToString()
		{
			Enumeration enumeration = unchecked((Enumeration)(0x300 + this.architectureId));
			return Library.GetString(enumeration, this.id, StringType.ID);
		}

		/// <summary>Provides a description for the object.</summary>
		/// <remarks>The description can contain spaces and non-ASCII characters.</remarks>
		/// <seealso cref="ToString()" />
		public string Description
		{
			get
			{
				Enumeration enumeration = unchecked((Enumeration)(0x300 + this.architectureId));
				return Library.GetString(enumeration, this.id, StringType.Description);
			}
		}

		internal static bool IsDefined(uint id, uint architectureId)
		{
			Enumeration enumeration = unchecked((Enumeration)(0x300 + architectureId));
			return Library.IsDefined(enumeration, id);
		}

	}

}
