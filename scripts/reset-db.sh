#~/code/flyway/flyway -configFiles=db/migration.toml clean
set -e
liquibase --defaultsFile=db/config/liquibase.properties rollbackCount 6
