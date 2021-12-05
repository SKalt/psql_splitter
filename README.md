# psql_splitter

a [`nom`](https://github.com/Geal/nom) parser combinator that matches a psql statement.

Postgres has a dialect of SQL that I'm going to call pgsql.
Postgres also has a client, `psql`, which recognizes meta-commands and inline-copy syntax that normal pgsql does not.
This parser recognizes psql meta-commands and pgsql tokens, allowing the `statement` combinator to know when a statement has ended.
The `statement` combinator also attaches color commentary on the end of a line to the previous statement, so `select 1; -- take a number` is matched as one statement.

## Status

This is a hack with some tests, a README, and a license.

This parser doesn't understand all the tokens in pgsql and psql, which has its pros:

- `psql_splitter::statement` is more likely to split statements out of invalid psql
- a simpler parser is easier to maintain

but also its cons:

- a simpler parser is more likely to be missing something about the enormous pgsql language

## Scope/features

- the splitter should match invalid psql, always splitting the input into one or more statements
- the splitter should treat psql meta-commands as statement terminators, e.g `select 1 as foo \gset` and `\d my_table` would both be complete statements.
- the splitter should ignore statement-terminators inside pgsql strings and parentheses and exotic psql such as `\if ... \endif` and `COPY table_name FROM STDIN; ... \.`
- the splitter should somewhat-reliably split psql and pgsql statements as defined by postgres, but not neccessarily other sql-like dialects derived from postgres's psql and pgsql.
- the splitter should be correct. Speed, a small binary, and memory efficiency aren't goals, but they're nice to have.

## Contributing

If you find a bug, please submit an issue with a reproduction case.
PRs for tooling would also be welcome.

## License

While this library is offered under the attached BSD 3-clause at ./LICENSE, `./src/*.sql` are excerpts from the postgres codebase, and so are covered by [the postgres license](https://www.postgresql.org/about/licence/).

## Prior art

[`libpg_query`'s `split_with_scanner`](https://github.com/pganalyze/libpg_query)
[`codeschool/sqlite-parser`'s test-suite-extraction tools](https://github.com/codeschool/sqlite-parser/blob/master/test/misc/test-grammar.pegjs)
