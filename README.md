# qdb
A database management tool, currently only handles migrations. 

Works for RDS databases.

## Installation
1. Make sure you have the Haskell tool [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/). This is used for building the entire project and once you've installed it, it will manage having the correct version of the compiler and all the Haskell libraries that are used as well.
2. Clone the project `git clone https://github.com/quanterall/qdb`
3. Enter the qdb project directory `cd qdb`
4. Run `stack install`

## Commands & parameters
```bash
$ qdb --help
qdb

Usage: qdb [--version] [--help] [-v|--verbose] [-c|--config PATH] COMMAND
  Tool for managing databases: migrations, etc.

Available options:
  --version                Show version
  --help                   Show this help text
  -v,--verbose             Verbose output?
  -c,--config PATH         Path to the configuration file

Available commands:
  migrate                  Apply all unapplied migrations
  rollback                 Roll back an amount of migrations
  add-migration            Add a migration in the migrations directory
  list-migrations          List all migrations in the database
  update-migrations        Update the migrations in the database to match your
                           migrations directory
  remove-migration         Remove a migration from the database by filename
```

### Providing arguments through the cli
```bash
$ qdb migrate --host <host> --port <port> --u <user> -p <password> -d <database>
```

If it's an RDS database and has a AWS Secret, you can only specify the `arn` value of the secret.

```bash
$ qdb migrate --secret-arn <arn-value>
```

### Providing arguments through config file
`qdb` accepts path for `yaml` configuration file that could be used to specify all connection arguments.
An example config file might look like so:
```yaml
migrationsPath: migrations/
secretArn: arn:aws:secretsmanager:<region>:<account_id>:secret:<secret-id>
```

By default `qdb` looks for a configuration file named `.qdb.yaml` in the current directory. 

Otherwise it could be specified via cli: `qdb --config ".my_qdb_config.yaml`