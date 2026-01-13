/**
 * COBOL Explorer Configuration
 * 
 * Modify these settings to customize the application behavior.
 * URL parameters will override these defaults.
 * 
 * Usage examples:
 *   - Default: Opens with dataPath set below
 *   - Override via URL: index.html?dataPath=/custom/path/to/analysis
 */
const CONFIG = {
    // Path to the directory containing the analysis JSON files
    // Absolute path from server root (use /out when serving from project root)
    dataPath: '/out',

    // Required JSON files in the dataPath directory:
    files: {
        projectAnalysis: 'project-analysis.json',
        jclAnalysis: 'jcl-analysis.json',
        copybookAnalysis: 'copybook-analysis-complete.json'
    },

    // Application settings
    app: {
        title: 'COBOL Explorer',
        version: '1.0.0'
    }
};

// Allow URL parameter override for dataPath
(function() {
    const urlParams = new URLSearchParams(window.location.search);
    const dataPathParam = urlParams.get('dataPath');
    if (dataPathParam) {
        CONFIG.dataPath = dataPathParam;
        console.log(`[Config] Using dataPath from URL: ${dataPathParam}`);
    } else {
        console.log(`[Config] Using default dataPath: ${CONFIG.dataPath}`);
    }
})();
