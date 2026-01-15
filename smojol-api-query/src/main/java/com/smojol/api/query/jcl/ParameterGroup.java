package com.smojol.api.query.jcl;

import java.util.*;

/**
 * Groupe de paramètres complètement générique
 * Peut représenter DCB, DISP, SPACE ou n'importe quel autre groupe
 */
public class ParameterGroup {
    private final Map<String, Object> values;
    
    public ParameterGroup(Map<String, Object> values) {
        this.values = values != null ? values : new HashMap<>();
    }
    
    /**
     * Accès générique par clé
     */
    public Object get(String key) {
        return values.get(key);
    }
    
    /**
     * Accès typé String
     */
    public String getString(String key) {
        return getString(key, null);
    }
    
    public String getString(String key, String defaultValue) {
        Object value = values.get(key);
        return value != null ? value.toString() : defaultValue;
    }
    
    /**
     * Accès typé Integer
     */
    public Integer getInteger(String key) {
        return getInteger(key, null);
    }
    
    public Integer getInteger(String key, Integer defaultValue) {
        Object value = values.get(key);
        if (value == null) return defaultValue;
        
        if (value instanceof Integer) {
            return (Integer) value;
        }
        
        try {
            return Integer.parseInt(value.toString());
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }
    
    /**
     * Accès typé Boolean
     */
    public Boolean getBoolean(String key) {
        return getBoolean(key, null);
    }
    
    public Boolean getBoolean(String key, Boolean defaultValue) {
        Object value = values.get(key);
        if (value == null) return defaultValue;
        
        if (value instanceof Boolean) {
            return (Boolean) value;
        }
        
        return Boolean.parseBoolean(value.toString());
    }
    
    /**
     * Vérifie si une clé existe
     */
    public boolean has(String key) {
        return values.containsKey(key);
    }
    
    /**
     * Retourne toutes les clés
     */
    public Set<String> keys() {
        return values.keySet();
    }
    
    /**
     * Retourne toutes les valeurs
     */
    public Collection<Object> values() {
        return values.values();
    }
    
    /**
     * Retourne la Map complète
     */
    public Map<String, Object> toMap() {
        return new HashMap<>(values);
    }
    
    /**
     * Retourne le nombre d'éléments
     */
    public int size() {
        return values.size();
    }
    
    /**
     * Vérifie si vide
     */
    public boolean isEmpty() {
        return values.isEmpty();
    }
    
    /**
     * Itérateur sur les entrées
     */
    public Set<Map.Entry<String, Object>> entries() {
        return values.entrySet();
    }
    
    @Override
    public String toString() {
        return values.toString();
    }
    
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ParameterGroup that = (ParameterGroup) o;
        return Objects.equals(values, that.values);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(values);
    }
}
