# qdb

A database management tool, currently only handles migrations.

## Commands & parameters

```bash
$ qdb --help
qdb

Usage: qdb [--version] [--help] [-v|--verbose] [-h|--host HOST] [-P|--port PORT]
           (-u|--user USER) (-d|--database DATABASE) (-p|--password PASSWORD)
           (-m|--migrations MIGRATIONS_PATH) COMMAND
  Tool for managing databases: migrations, etc.

Available options:
  --version                Show version
  --help                   Show this help text
  -v,--verbose             Verbose output?
  -h,--host HOST           Host to connect to
  -P,--port PORT           Port to connect to
  -u,--user USER           User to connect as
  -d,--database DATABASE   Database to use
  -p,--password PASSWORD   Password to use
  -m,--migrations MIGRATIONS_PATH
                           Migrations directory

Available commands:
  migrate                  Apply all unapplied migrations
  rollback                 Roll back an amount of migrations
  add-migration            Add a migration in the migrations directory
  list-migrations          List all migrations in the database
  update-migrations        Update the migrations in the database to match your
                           migrations directory
  remove-migration         Remove a migration from the database by filename
```
