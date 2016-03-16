/*
 *                      Yeppp! library implementation
 *
 * This file is part of Yeppp! library and licensed under the New BSD license.
 * See LICENSE.txt for the full text of the license.
 */

namespace Yeppp
{

	/// <summary>ISA extensions.</summary>
	/// <seealso cref="CpuArchitecture.CpuIsaFeatures" />
	/// <seealso cref="Library.IsSupported(CpuIsaFeature)" />
	/// <seealso cref="X86CpuIsaFeature" />
	/// <seealso cref="ArmCpuIsaFeature" />
	/// <seealso cref="MipsCpuIsaFeature" />
	/// <seealso cref="IA64CpuIsaFeature" />
	/// <seealso cref="PowerPCCpuIsaFeature" />
	public class CpuIsaFeature
	{

		private readonly uint architectureId;
		private readonly uint id;

		internal CpuIsaFeature(uint id, uint architectureId)
		{
			this.id = id;
			this.architectureId = architectureId;
		}

		internal CpuIsaFeature(uint id) {
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

		/// <summary>Compares for equality with another <see cref="CpuIsaFeature" /> object.</summary>
		/// <remarks>Comparison is performed by value.</remarks>
		public bool Equals(CpuIsaFeature other)
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

			return this.Equals((CpuIsaFeature)other);
		}

		/// <summary>Provides a hash for the object.</summary>
		/// <remarks>Non-equal <see cref="CpuIsaFeature" /> objects are guaranteed to have different hashes.</remarks>
		public override int GetHashCode()
		{
			return unchecked((int)(this.id ^ (this.architectureId << 16)));
		}

		/// <summary>Provides a string ID for the object.</summary>
		/// <remarks>The string ID starts with a Latin letter and contains only Latin letters, digits, and underscore symbol.</remarks>
		/// <seealso cref="Description" />
		public override string ToString()
		{
			Enumeration enumeration = unchecked((Enumeration)(0x100 + this.architectureId));
			return Library.GetString(enumeration, this.id, StringType.ID);
		}

		/// <summary>Provides a description for the object.</summary>
		/// <remarks>The description can contain spaces and non-ASCII characters.</remarks>
		/// <seealso cref="ToString()" />
		public string Description
		{
			get
			{
				Enumeration enumeration = unchecked((Enumeration)(0x100 + this.architectureId));
				return Library.GetString(enumeration, this.id, StringType.Description);
			}
		}

		internal static bool IsDefined(uint id, uint architectureId)
		{
			Enumeration enumeration = unchecked((Enumeration)(0x100 + architectureId));
			return Library.IsDefined(enumeration, id);
		}

	}

}
