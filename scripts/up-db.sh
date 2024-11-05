#~/code/flyway/flyway -configFiles=db/migration.toml migrate
set -e
liquibase --defaultsFile=db/config/liquibase.properties update
