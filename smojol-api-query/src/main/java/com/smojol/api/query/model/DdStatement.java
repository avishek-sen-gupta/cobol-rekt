package com.smojol.api.query.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.smojol.api.query.jcl.JclParameterMap;
import com.smojol.api.query.jcl.ParameterGroup;
import lombok.*;

import java.util.*;

/**
 * Représentation d'un DD Statement dans un JCL
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DdStatement {
    
    @JsonProperty("name")
    private String name;
    
    @JsonProperty("parameters")
    private Map<String, Object> parameters;
    
    @JsonProperty("line_number")
    private int lineNumber;
    
    // Paramètres parsés de manière générique
    private transient JclParameterMap parsedParameters;
    
    /**
     * Parse les paramètres de manière complètement générique
     */
    public void parseParameters() {
        if (parameters == null) return;
        this.parsedParameters = new JclParameterMap(parameters);
    }
    
    /**
     * Accès au wrapper JclParameterMap
     */
    public JclParameterMap getParametersMap() {
        if (parsedParameters == null) {
            parseParameters();
        }
        return parsedParameters;
    }
    
    // ========== Méthodes helper génériques (optionnelles) ==========
    
    /**
     * Récupère n'importe quel paramètre string
     */
    public String getParameter(String key) {
        return getParametersMap().getString(key);
    }
    
    /**
     * Récupère n'importe quel paramètre integer
     */
    public Integer getParameterAsInt(String key) {
        return getParametersMap().getInteger(key);
    }
    
    /**
     * Parse n'importe quel groupe key=value
     */
    public ParameterGroup getParameterGroup(String key) {
        return getParametersMap().parseKeyValuePairs(key);
    }
    
    /**
     * Parse n'importe quelle liste
     */
    public List<String> getParameterList(String key) {
        return getParametersMap().parseCommaList(key);
    }
    
    /**
     * Vérifie si un paramètre existe
     */
    public boolean hasParameter(String key) {
        return getParametersMap().has(key);
    }
    
    /**
     * Retourne tous les noms de paramètres
     */
    public Set<String> getParameterNames() {
        return getParametersMap().keys();
    }
}
