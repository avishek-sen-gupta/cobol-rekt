const { Eta } = require('eta');
const fs = require('fs');
const path = require('path');

// Configuration
const config = {
    apiUrl: process.env.API_URL || 'http://localhost:8080/api',
    outputFile: path.join(__dirname, 'index-generated.html')
};

// Configure ETA instance
const eta = new Eta({
    views: path.join(__dirname, 'templates'),
    cache: false,
    autoEscape: false // We handle escaping in JavaScript
});

console.log('🔨 Building COBOL Explorer UI with ETA...\n');
console.log('Configuration:');
console.log(`  - API URL: ${config.apiUrl}`);
console.log(`  - Output: ${config.outputFile}\n`);

try {
    // Render the index template
    const html = eta.render('index', {
        apiUrl: config.apiUrl
    });

    // Write to file
    fs.writeFileSync(config.outputFile, html, 'utf8');

    console.log('✅ Build successful!');
    console.log(`\n📄 Generated: ${path.basename(config.outputFile)}`);
    console.log(`📊 Size: ${(html.length / 1024).toFixed(2)} KB`);
    console.log(`\n🚀 To view the app:`);
    console.log(`   1. Start the Java API: cd ../smojol-rest-api && mvn spring-boot:run`);
    console.log(`   2. Serve the UI: python -m http.server 8080`);
    console.log(`   3. Open: http://localhost:8080/index-generated.html`);
} catch (error) {
    console.error('❌ Build failed:', error.message);
    console.error(error.stack);
    process.exit(1);
}
