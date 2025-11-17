namespace DWSIM.OTS.ControlGateway.Models;

public class OpcUaMapping
{
    public string Version { get; set; } = "1.0";
    public NamespaceConfig Namespace { get; set; } = new();
    public List<FolderDefinition> Folders { get; set; } = new();
    public List<TagCategory> TagCategories { get; set; } = new();
    public List<ExplicitMapping> ExplicitMappings { get; set; } = new();
    public AlarmConfiguration? Alarms { get; set; }
    public List<string> NodeProperties { get; set; } = new();
    public UpdateRatesConfig UpdateRates { get; set; } = new();
    public SecurityConfig Security { get; set; } = new();
}

public class NamespaceConfig
{
    public string Uri { get; set; } = string.Empty;
    public int Index { get; set; } = 2;
}

public class FolderDefinition
{
    public string Name { get; set; } = string.Empty;
    public string NodeId { get; set; } = string.Empty;
    public List<FolderDefinition>? Children { get; set; }
}

public class TagCategory
{
    public string Category { get; set; } = string.Empty;
    public string Pattern { get; set; } = string.Empty;
    public string OpcuaFolder { get; set; } = string.Empty;
    public List<PropertyDefinition> Properties { get; set; } = new();
}

public class PropertyDefinition
{
    public string Name { get; set; } = string.Empty;
    public string DataType { get; set; } = string.Empty;
    public bool Writable { get; set; }
    public string? Units { get; set; }
    public string? Description { get; set; }
    public EngineeringUnits? EngineeringUnits { get; set; }
    public List<string>? EnumStrings { get; set; }
}

public class EngineeringUnits
{
    public double Low { get; set; }
    public double High { get; set; }
}

public class ExplicitMapping
{
    public string DwsimTag { get; set; } = string.Empty;
    public string OpcuaNodeId { get; set; } = string.Empty;
    public string BrowseName { get; set; } = string.Empty;
    public string DisplayName { get; set; } = string.Empty;
    public string DataType { get; set; } = string.Empty;
    public bool Writable { get; set; }
    public string? Units { get; set; }
    public string? Description { get; set; }
    public EngineeringUnits? EngineeringUnits { get; set; }
    public List<string>? EnumStrings { get; set; }
}

public class AlarmConfiguration
{
    public Dictionary<string, int> SeverityLevels { get; set; } = new();
    public List<AlarmDefinition>? Definitions { get; set; }
}

public class AlarmDefinition
{
    public string Name { get; set; } = string.Empty;
    public string Tag { get; set; } = string.Empty;
    public string Condition { get; set; } = string.Empty;
    public string Severity { get; set; } = string.Empty;
    public string Message { get; set; } = string.Empty;
}

public class UpdateRatesConfig
{
    public int DefaultMs { get; set; } = 1000;
    public int FastMs { get; set; } = 100;
    public int SlowMs { get; set; } = 5000;
}

public class SecurityConfig
{
    public string Mode { get; set; } = "simulation_only";
    public bool AllowAnonymous { get; set; } = true;
    public bool RequireEncryption { get; set; } = false;
}
