package org.smojol.common.logging;

import java.io.IOException;
import java.io.InputStream;
import java.util.logging.LogManager;

public class LoggingConfig {
    public static void setupLogging() {
        String configFile = System.getProperty("java.util.logging.config.file");
        if (configFile != null) {
            System.out.println("Using custom logging configuration: " + configFile);
            return;
        }
        try (InputStream defaultConfig = LoggingConfig.class.getClassLoader().getResourceAsStream("logging.properties")) {
            if (defaultConfig != null) {
                LogManager.getLogManager().readConfiguration(defaultConfig);
                System.out.println("Loaded default logging configuration.");
            } else System.err.println("Default logging configuration not found.");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
