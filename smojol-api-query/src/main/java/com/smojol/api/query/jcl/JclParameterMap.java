package com.smojol.api.query.jcl;

import java.util.*;

/**
 * Wrapper générique autour de Map<String, Object>
 * Fournit des méthodes utilitaires pour parser n'importe quel paramètre JCL
 */
public class JclParameterMap {
    private final Map<String, Object> parameters;
    
    public JclParameterMap(Map<String, Object> parameters) {
        this.parameters = parameters != null ? parameters : new HashMap<>();
    }
    
    /**
     * Récupère une valeur string avec gestion des parenthèses
     */
    public String getString(String key) {
        return getString(key, null);
    }
    
    public String getString(String key, String defaultValue) {
        Object value = parameters.get(key);
        if (value == null) return defaultValue;
        
        String str = value.toString().trim();
        
        // Enlever parenthèses externes si présentes
        if (str.startsWith("(") && str.endsWith(")")) {
            str = str.substring(1, str.length() - 1).trim();
        }
        
        return str;
    }
    
    /**
     * Récupère un Integer
     */
    public Integer getInteger(String key) {
        return getInteger(key, null);
    }
    
    public Integer getInteger(String key, Integer defaultValue) {
        String str = getString(key);
        if (str == null || str.isEmpty()) return defaultValue;
        
        try {
            return Integer.parseInt(str);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }
    
    /**
     * Récupère un Boolean
     */
    public Boolean getBoolean(String key) {
        return getBoolean(key, null);
    }
    
    public Boolean getBoolean(String key, Boolean defaultValue) {
        String str = getString(key);
        if (str == null || str.isEmpty()) return defaultValue;
        return Boolean.parseBoolean(str);
    }
    
    /**
     * Parse une structure "key=value,key=value" et retourne ParameterGroup
     */
    public ParameterGroup parseKeyValuePairs(String key) {
        String value = getString(key);
        if (value == null || value.isEmpty()) {
            return new ParameterGroup(new HashMap<>());
        }
        
        Map<String, Object> result = new HashMap<>();
        
        // Split par virgules
        String[] parts = value.split(",");
        for (String part : parts) {
            part = part.trim();
            
            int equalsIndex = part.indexOf('=');
            if (equalsIndex == -1) {
                // Pas de '=', c'est une valeur standalone
                result.put(part, part);
                continue;
            }
            
            String k = part.substring(0, equalsIndex).trim();
            String v = part.substring(equalsIndex + 1).trim();
            
            result.put(k, v);
        }
        
        return new ParameterGroup(result);
    }
    
    /**
     * Parse une liste séparée par virgules
     */
    public List<String> parseCommaList(String key) {
        String value = getString(key);
        if (value == null || value.isEmpty()) {
            return new ArrayList<>();
        }
        
        List<String> result = new ArrayList<>();
        String[] parts = value.split(",");
        for (String part : parts) {
            result.add(part.trim());
        }
        
        return result;
    }
    
    /**
     * Parse avec respect des parenthèses imbriquées
     */
    public List<String> parseTokens(String key, char delimiter) {
        String value = getString(key);
        if (value == null || value.isEmpty()) {
            return new ArrayList<>();
        }
        
        return tokenize(value, delimiter);
    }
    
    /**
     * Tokenize en respectant les parenthèses
     */
    private List<String> tokenize(String input, char delimiter) {
        List<String> tokens = new ArrayList<>();
        if (input == null || input.isEmpty()) {
            return tokens;
        }
        
        StringBuilder currentToken = new StringBuilder();
        int parenDepth = 0;
        
        for (char c : input.toCharArray()) {
            if (c == '(') {
                parenDepth++;
                currentToken.append(c);
            } else if (c == ')') {
                parenDepth--;
                currentToken.append(c);
            } else if (c == delimiter && parenDepth == 0) {
                if (currentToken.length() > 0) {
                    tokens.add(currentToken.toString().trim());
                    currentToken = new StringBuilder();
                }
            } else {
                currentToken.append(c);
            }
        }
        
        if (currentToken.length() > 0) {
            tokens.add(currentToken.toString().trim());
        }
        
        return tokens;
    }
    
    /**
     * Vérifie si une clé existe
     */
    public boolean has(String key) {
        return parameters.containsKey(key);
    }
    
    /**
     * Retourne toutes les clés
     */
    public Set<String> keys() {
        return parameters.keySet();
    }
    
    /**
     * Retourne la Map brute
     */
    public Map<String, Object> toMap() {
        return new HashMap<>(parameters);
    }
    
    /**
     * Retourne un sous-groupe de paramètres parsés
     */
    public ParameterGroup getGroup(String key) {
        return parseKeyValuePairs(key);
    }
}
