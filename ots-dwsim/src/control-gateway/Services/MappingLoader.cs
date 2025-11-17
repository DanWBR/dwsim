using DWSIM.OTS.ControlGateway.Models;
using Microsoft.Extensions.Logging;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

namespace DWSIM.OTS.ControlGateway.Services;

public class MappingLoader : IMappingLoader
{
    private readonly ILogger<MappingLoader> _logger;
    private readonly IDeserializer _yamlDeserializer;

    public MappingLoader(ILogger<MappingLoader> logger)
    {
        _logger = logger;

        _yamlDeserializer = new DeserializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance)
            .IgnoreUnmatchedProperties()
            .Build();
    }

    public async Task<OpcUaMapping> LoadMappingAsync(string filePath)
    {
        try
        {
            _logger.LogInformation("Loading OPC UA mapping from {FilePath}", filePath);

            if (!File.Exists(filePath))
            {
                throw new FileNotFoundException($"Mapping file not found: {filePath}");
            }

            var yamlContent = await File.ReadAllTextAsync(filePath);
            var mapping = _yamlDeserializer.Deserialize<OpcUaMapping>(yamlContent);

            if (mapping == null)
            {
                throw new InvalidOperationException("Failed to deserialize mapping file");
            }

            _logger.LogInformation("Successfully loaded OPC UA mapping version {Version}", mapping.Version);
            _logger.LogDebug("Mapping contains {FolderCount} folders, {CategoryCount} tag categories, {ExplicitCount} explicit mappings",
                mapping.Folders.Count, mapping.TagCategories.Count, mapping.ExplicitMappings.Count);

            return mapping;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error loading OPC UA mapping from {FilePath}", filePath);
            throw;
        }
    }

    public OpcUaMapping ApplySessionContext(OpcUaMapping template, string sessionId)
    {
        _logger.LogDebug("Applying session context for session {SessionId}", sessionId);

        // Deep clone the mapping and replace {session_id} placeholders
        var mapping = CloneMapping(template);

        // Replace session_id in namespace URI
        if (mapping.Namespace.Uri.Contains("{session_id}"))
        {
            mapping.Namespace.Uri = mapping.Namespace.Uri.Replace("{session_id}", sessionId);
        }

        // Replace in folder NodeIds
        ReplaceSessionIdInFolders(mapping.Folders, sessionId);

        // Replace in explicit mappings
        foreach (var explicitMapping in mapping.ExplicitMappings)
        {
            if (explicitMapping.OpcuaNodeId.Contains("{session_id}"))
            {
                explicitMapping.OpcuaNodeId = explicitMapping.OpcuaNodeId.Replace("{session_id}", sessionId);
            }
        }

        _logger.LogDebug("Session context applied for session {SessionId}", sessionId);

        return mapping;
    }

    private void ReplaceSessionIdInFolders(List<FolderDefinition> folders, string sessionId)
    {
        foreach (var folder in folders)
        {
            if (folder.NodeId.Contains("{session_id}"))
            {
                folder.NodeId = folder.NodeId.Replace("{session_id}", sessionId);
            }

            if (folder.Children != null && folder.Children.Count > 0)
            {
                ReplaceSessionIdInFolders(folder.Children, sessionId);
            }
        }
    }

    private OpcUaMapping CloneMapping(OpcUaMapping source)
    {
        // Simple clone implementation - in production use a proper deep clone library
        var serializer = new SerializerBuilder().Build();
        var yaml = serializer.Serialize(source);
        return _yamlDeserializer.Deserialize<OpcUaMapping>(yaml);
    }
}
