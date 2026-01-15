package com.smojol.api.query.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.smojol.api.query.jcl.ParameterGroup;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Représentation d'un dataset utilisé par les programmes COBOL et JCL
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Dataset {

    @JsonProperty("name")
    private String name;

    @JsonProperty("type")
    private String type;

    @JsonProperty("organization")
    private String organization;

    @JsonProperty("record_format")
    private String recordFormat;

    @JsonProperty("lrecl")
    private int lrecl;

    @JsonProperty("blksize")
    private int blksize;

    @JsonProperty("access_method")
    private String accessMethod;

    @JsonProperty("used_by_cbl")
    private List<String> usedByCobol;

    @JsonProperty("used_by_jcl")
    private List<String> usedByJcl;

    @JsonProperty("volume_serial")
    private String volumeSerial;

    @JsonProperty("unit")
    private String unit;

    @JsonProperty("retention")
    private String retention;

    @JsonProperty("allocation")
    private Map<String, Object> allocation;

    @JsonProperty("description")
    private String description;

    @JsonProperty("last_modified")
    private long lastModified;
    
    // Groupes de paramètres génériques
    @JsonProperty("parameter_groups")
    private Map<String, ParameterGroup> parameterGroups;

    /**
     * Ajoute un groupe de paramètres générique
     */
    public void addParameterGroup(String groupName, ParameterGroup group) {
        if (parameterGroups == null) {
            parameterGroups = new HashMap<>();
        }
        parameterGroups.put(groupName, group);
    }
    
    /**
     * Récupère un groupe de paramètres
     */
    public ParameterGroup getParameterGroup(String groupName) {
        return parameterGroups != null ? parameterGroups.get(groupName) : new ParameterGroup(new HashMap<>());
    }
    
    /**
     * Vérifie si un groupe existe
     */
    public boolean hasParameterGroup(String groupName) {
        return parameterGroups != null && parameterGroups.containsKey(groupName);
    }
    
    /**
     * Retourne tous les noms de groupes
     */
    public Set<String> getParameterGroupNames() {
        return parameterGroups != null ? parameterGroups.keySet() : new HashSet<>();
    }

    /**
     * Vérifie si le dataset est valide
     */
    public boolean isValid() {
        return name != null && !name.isEmpty();
    }

    /**
     * Retourne le nombre de programmes utilisant ce dataset
     */
    public int getUsedByCobolCount() {
        return usedByCobol != null ? usedByCobol.size() : 0;
    }

    /**
     * Retourne le nombre de JCLs utilisant ce dataset
     */
    public int getUsedByJclCount() {
        return usedByJcl != null ? usedByJcl.size() : 0;
    }

    /**
     * Retourne le nombre total d'utilisateurs (COBOL + JCL)
     */
    public int getTotalUserCount() {
        return getUsedByCobolCount() + getUsedByJclCount();
    }

    /**
     * Vérifie si ce dataset est utilisé par un programme
     */
    public boolean isUsedByCobol(String program) {
        return usedByCobol != null && usedByCobol.contains(program);
    }

    /**
     * Vérifie si ce dataset est utilisé par un JCL
     */
    public boolean isUsedByJcl(String jcl) {
        return usedByJcl != null && usedByJcl.contains(jcl);
    }

    /**
     * Retourne une description lisible du format du dataset
     */
    public String getFormatDescription() {
        if (recordFormat == null) {
            return "Unknown";
        }
        return String.format("%s LRECL %d", recordFormat, lrecl);
    }

    /**
     * Retourne l'organisation du dataset
     */
    public DatasetOrganization getOrganizationType() {
        if (organization == null) {
            return DatasetOrganization.UNKNOWN;
        }
        try {
            return DatasetOrganization.valueOf(organization.toUpperCase());
        } catch (IllegalArgumentException e) {
            return DatasetOrganization.UNKNOWN;
        }
    }

    /**
     * Types d'organisation de datasets
     */
    public enum DatasetOrganization {
        SEQUENTIAL,      // Sequential (PS)
        PARTITIONED,     // Partitioned (PDS)
        DIRECT,          // Direct
        INDEXED,         // Indexed Sequential (ISAM)
        RELATIVE,        // Relative
        UNKNOWN          // Unknown
    }

    /**
     * Retourne une représentation résumée
     */
    public String getSummary() {
        return String.format("%s (%s, %s, Used by %d COBOL + %d JCL)",
                name, organization, getFormatDescription(),
                getUsedByCobolCount(), getUsedByJclCount());
    }
}
