# qdb

A database management tool, currently only handles migrations.

## Commands & parameters

```bash
$ qdb --help
qdb

Usage: qdb [--version] [--help] [-v|--verbose] [-h|--host HOST] [-P|--port PORT]
           (-u|--user USER) (-p|--password PASSWORD) (-d|--database DATABASE)
           (-m|--migrations MIGRATIONS_PATH) COMMAND
  Tool for managing databases: migrations, etc.

Available options:
  --version                Show version
  --help                   Show this help text
  -v,--verbose             Verbose output?
  -h,--host HOST           Host to connect to
  -P,--port PORT           Port to connect to
  -u,--user USER           User to connect as
  -p,--password PASSWORD   Password to use
  -d,--database DATABASE   Database to use
  -m,--migrations MIGRATIONS_PATH
                           Migrations directory

Available commands:
  migrate                  Apply all unapplied migrations
  rollback                 Roll back an amount of migrations
  add-migration            Add a migration in the migrations directory
```
