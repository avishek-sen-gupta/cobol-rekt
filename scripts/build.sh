set -e
pushd .
cd ./smojol-app/cobol-lekt
npm run lint
npm run build
popd
rm -rf ./smojol-api/src/main/resources/static/dist
cp -r ./smojol-app/cobol-lekt/dist ./smojol-api/src/main/resources/static/dist
mvn clean verify -Dmaven.test.skip
