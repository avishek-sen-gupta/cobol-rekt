set -e
source ./scripts/build-app.sh
mvn clean verify -Dmaven.test.skip
