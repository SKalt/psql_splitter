use nom::branch::alt;
use nom::bytes::complete::{is_a, tag_no_case, take_till, take_till1, take_until, take_until1};
use nom::character::complete::{alpha1, anychar, line_ending};
use nom::combinator::{eof, not};
use nom::multi::many0;
use nom::sequence::tuple;
use nom::{
    bytes::complete::{is_not, tag},
    combinator::{opt, recognize},
    sequence::delimited,
    IResult,
};

fn single_quote_string(input: &str) -> IResult<&str, &str> {
    return recognize(delimited(tag("'"), opt(is_not("'")), tag("'")))(input);
}

#[test]
fn test_single_quote_string() {
    // assert_eq!(single_quote_string("abc'def'"), Ok(("abc'def'")));
    assert_eq!(single_quote_string("'abc'def'"), Ok(("def'", "'abc'")));
    assert_eq!(single_quote_string("'a''b'"), Ok(("'b'", "'a'")));
    assert_eq!(single_quote_string("''"), Ok(("", "''")));
}

fn double_quote_string(input: &str) -> IResult<&str, &str> {
    return recognize(delimited(tag("\""), opt(is_not("\"")), tag("\"")))(input);
}

#[test]
fn test_double_quote_string() {
    // assert_eq!(double_quote_string("abc'def'"), Ok(("abc'def'")));
    assert_eq!(
        double_quote_string("\"abc\"def\""),
        Ok(("def\"", "\"abc\""))
    );
    assert_eq!(double_quote_string("\"a\"\"b\""), Ok(("\"b\"", "\"a\"")));
    assert_eq!(double_quote_string("\"\""), Ok(("", "\"\"")));
}

fn dollar_delim(input: &str) -> IResult<&str, &str> {
    return recognize(delimited(tag("$"), many0(is_not("$")), tag("$")))(input);
}

#[test]
fn test_dollar_delim() {
    assert_eq!(dollar_delim("$A$"), Ok(("", "$A$")));
    assert_eq!(dollar_delim("$$abc$$"), Ok(("abc$$", "$$")));
}

fn dollar_quote_string(input: &str) -> IResult<&str, &str> {
    let (rest, delim) = dollar_delim(input)?;
    let (rest, middle) = take_until(delim)(rest)?;
    let (rest, _) = tag(delim)(rest)?;
    return Ok((rest, &input[..delim.len() * 2 + middle.len()]));
}

#[test]
fn test_dollar_quote() {
    let junk = "$a$ $junk$a$";
    assert_eq!(dollar_quote_string(junk), Ok(("", "$a$ $junk$a$")));
    assert_eq!(dollar_quote_string("$$$$"), Ok(("", "$$$$")));
    assert_eq!(dollar_quote_string("$a$b$b$a$b$"), Ok(("b$", "$a$b$b$a$")));
}

fn c_style_comment(input: &str) -> IResult<&str, &str> {
    let (mut rest, _) = tag("/*")(input)?;
    let mut mid_size = 0usize;
    loop {
        let (r, m) = take_till(|c| match c {
            '/' | '*' => return true,
            _ => return false,
        })(rest)?;
        rest = r;
        mid_size += m.len();
        if let Ok((r, m)) = c_style_comment(rest) {
            rest = r;
            mid_size += m.len()
        } else if let Ok((r, u)) = tag::<&str, &str, nom::error::Error<&str>>("*/")(rest) {
            rest = r;
            mid_size += u.len();
            break;
        } else {
            // eat an asterisk or single slash
            let (r, u) = recognize(anychar)(rest)?;
            rest = r;
            mid_size += u.len();
        }
    }
    return Ok((rest, &input[..2 + mid_size]));
}

#[test]
fn test_nested_c_style_comment() {
    assert_eq!(c_style_comment("/* abc */"), Ok(("", "/* abc */")));
    assert_eq!(c_style_comment("/* /**/ */"), Ok(("", "/* /**/ */")));
    assert_eq!(
        c_style_comment("/* /**/ /**/ */"),
        Ok(("", "/* /**/ /**/ */"))
    );
}

fn parenthetical(input: &str) -> IResult<&str, &str> {
    let (mut rest, _) = tag("(")(input)?;
    let mut mid_size = 0usize;
    let mut parser = alt((
        parenthetical,
        line_comment,
        c_style_comment,
        single_quote_string,
        double_quote_string,
        dollar_quote_string,
        take_till1(|c| match c {
            '(' | ')' | '/' | '\\' | '$' | '"' | '-' => return true,
            _ => return false,
        }),
        // need to take until tag("/*"), tag("'"), tag("\""), dollar_delim
        // match
    ));
    loop {
        match parser(rest) {
            Ok((r, m)) => {
                rest = r;
                mid_size += m.len();
            }
            Err(_) => break,
        }
    }
    let (rest, _) = tag(")")(rest)?;
    return Ok((rest, &input[..2 + mid_size]));
}

#[test]
fn test_parenthetical() {
    assert_eq!(parenthetical("(input)"), Ok(("", "(input)")));
    assert_eq!(parenthetical("( /*)*/)"), Ok(("", "( /*)*/)")));
    let multiline = "(
        -- ) $$ ) $$
    )";
    assert_eq!(parenthetical(multiline), Ok(("", multiline)));
    assert_eq!(parenthetical("(())"), Ok(("", "(())")))
}

fn line_comment(input: &str) -> IResult<&str, &str> {
    let (mut rest, _) = tag("--")(input)?;
    loop {
        if let Ok((r, _)) = line_ending::<&str, nom::error::Error<&str>>(rest) {
            rest = r;
            break;
        } else if eof::<&str, nom::error::Error<&str>>(rest).is_ok() {
            break;
        } else {
            let (r, _) = recognize(anychar)(rest)?;
            rest = r;
        }
    }

    // let (rest, _) = alt((line_ending, eof))(rest)?;
    return Ok((rest, &input[..input.len() - rest.len()]));
}

#[test]
fn test_line_comment() {
    assert_eq!(line_comment("-- foo"), Ok(("", "-- foo")));
    assert_eq!(line_comment("-- bar\n"), Ok(("", "-- bar\n")));
    assert_eq!(line_comment("--baz\n"), Ok(("", "--baz\n")));
    assert_eq!(line_comment("--\n"), Ok(("", "--\n")));
    assert_eq!(line_comment("-- quux\n--..."), Ok(("--...", "-- quux\n")));
}

fn psql_meta_cmd(input: &str) -> IResult<&str, &str> {
    let (rest, _) = tag("\\")(input)?;
    let (rest, a) = alpha1(rest)?;
    let (rest, line) = take_until("\n")(rest)?;
    let (rest, eol) = line_ending(rest)?;
    return Ok((rest, &input[..1 + a.len() + line.len() + eol.len()]));
}

fn string_or_comment(input: &str) -> IResult<&str, &str> {
    alt((
        c_style_comment,
        line_comment,
        single_quote_string,
        double_quote_string,
        dollar_quote_string,
    ))(input)
}

fn psql_if(input: &str) -> IResult<&str, &str> {
    // TODO: watch out for strings, comments
    let (mut rest, start) = tag(r"\if")(input)?;
    let mut mid_size = 0usize;
    loop {
        if let Ok((r, mid)) = recognize(tuple((take_until(r"\if"), psql_if)))(rest) {
            rest = r;
            mid_size += mid.len();
        } else if let Ok((r, mid)) =
            take_until::<&str, &str, nom::error::Error<&str>>(r"\endif")(rest)
        {
            rest = r;
            mid_size += mid.len();
            break;
        } else {
            break; // let tag raise the error
        }
    }
    let (rest, end) = tag(r"\endif")(rest)?;
    return Ok((rest, &input[..start.len() + mid_size + end.len()]));
}

#[test]
fn test_psql_if() {
    let ez = r"\if true select 1 \endif";
    assert_eq!(psql_if(ez), Ok(("", ez)));
    let multiline = r"\if false
        select 1;
    \elsif false
        select 2;
    \else
        select 3;
    \endif";
    assert_eq!(psql_if(multiline), Ok(("", multiline)));
    let nested = r"\if :var
        \if false
            ...
        \else
            select 1;
        \endif
    \endif";
    assert_eq!(psql_if(nested), Ok(("", nested)));
    let inline_nested = r"\if on \if :var \if :yep 1 \endif \endif \endif";
    assert_eq!(psql_if(inline_nested), Ok(("", inline_nested)));
    assert!(psql_if(r"\if").is_err());
}

fn inline_stdin_delim(input: &str) -> IResult<&str, &str> {
    let (rest, _) = line_ending(input)?;
    let (rest, _) = tag("\\.")(rest)?;
    let (rest, _) = alt((line_ending, eof))(rest)?;
    return Ok((rest, &input[..input.len() - rest.len()]));
}

fn inline_stdin(input: &str) -> IResult<&str, &str> {
    let mut rest = input;
    loop {
        let (r, _) = take_till(|c| c == '\r' || c == '\n')(rest)?;
        match inline_stdin_delim(r) {
            Ok((rest, _)) => return Ok((rest, &input[..input.len() - rest.len()])),
            Err(e) => {
                if r.len() == 0 {
                    return Err(e);
                } else {
                    rest = &r[1..];
                }
            }
        }
    }
}

fn sql_copy_from_stdin(input: &str) -> IResult<&str, &str> {
    let (mut rest, _) = tag_no_case("copy")(input)?;
    loop {
        if rest.len() == 0
            || tag_no_case::<&str, &str, nom::error::Error<&str>>("from")(rest).is_ok()
        {
            break;
        } else {
            rest = &rest[1..];
        }
    }
    let (rest, _) = tag_no_case("from")(rest)?;
    let (rest, _) = is_a(" \t\n\r")(rest)?;
    let (mut rest, _) = tag_no_case("stdin")(rest)?;
    let mut parser = alt((
        is_a(" \t\r\n"),
        parenthetical,
        string_or_comment,
        take_till1(|c| match c {
            '\'' | '(' | '\"' | '/' | '-' | '$' | ';' => true,
            _ => false,
        }),
    ));
    loop {
        while let Ok((r, _)) = parser(rest) {
            rest = r;
        }
        if rest.len() == 0 {
            // fail almost informatively
            return tag(";")(rest);
        }
        if let Ok((r, _)) = tag::<&str, &str, nom::error::Error<&str>>(";")(rest) {
            rest = r;
            break;
        } else {
            rest = &rest[1..];
        }
    }
    let (r, inline) = recognize(opt(inline_stdin))(rest)?;

    // heuristic: check if something like "copy from stdin" could be part of the inline stdin
    if inline.len() > 0
        && (inline.contains("copy") || inline.contains("COPY"))
        && (inline.contains("stdin") || inline.contains("STDIN"))
    {
        // many0 and many1(statement) are inexplicably failing me
        let mut statements = vec![];
        let mut inline_input = inline;
        while let Ok((remaining_inline, matched_inline)) = statement(inline_input) {
            statements.push(matched_inline);
            inline_input = remaining_inline;
        }

        // let (m, statements) = many1(statement)(inline)?;
        if statements.len() == 0 {
            rest = r;
        } else {
            // strip leading whitespace & comments
            let (last, _) = many0(alt((is_a(" \t\r\n"), line_comment, c_style_comment)))(
                *statements.last().unwrap(),
            )?;
            let is_err = sql_copy_from_stdin(last).is_err();
            if is_err && psql_copy_from_stdin(last).is_err() {
                rest = r;
            }
        }
        if inline_input.len() > 0 {
            assert!(statement(inline_input).is_err());
        }
        assert_eq!(
            inline_input.len(),
            0usize,
            "not consumed: >>>{}<<<",
            inline_input
        );
    } else {
        rest = r;
    }
    let result = &input[..input.len() - rest.len()];
    return Ok((rest, result));
}

fn psql_copy_from_stdin(input: &str) -> IResult<&str, &str> {
    let (mut rest, _) = tag_no_case(r"\copy ")(input)?;
    loop {
        if rest.len() == 0
            || tag_no_case::<&str, &str, nom::error::Error<&str>>("from")(rest).is_ok()
        {
            break;
        } else {
            rest = &rest[1..];
        }
    }
    let (rest, _) = is_a(" \t\n\r")(rest)?;
    let (rest, _) = tag_no_case("stdin")(rest)?;
    let (rest, _) = take_until1("\n")(rest)?;
    let (rest, _) = inline_stdin(rest)?;
    return Ok((rest, &input[..input.len() - rest.len()]));
}

#[test]
fn test_sql_copy_from_stdin() {
    let spongebob_case = "CoPy my_table FrOm StdIn;\n\\.\n";
    assert_eq!(
        sql_copy_from_stdin(spongebob_case),
        Ok(("", spongebob_case))
    );
    let with_opt = "copy my_table from stdin with quote ';';\na;b;c\n\\.\n";
    assert_eq!(sql_copy_from_stdin(with_opt), Ok(("", with_opt)));
}

pub fn statement(input: &str) -> IResult<&str, &str> {
    let (mut rest, _) = not(eof)(input)?;
    let mut not_statement = alt((
        is_a(" \t\r\n"),
        psql_if, // complete statement?
        string_or_comment,
        // is_not("\\c\'\"$/-;"),
        // TODO: replace with is_not()?
        take_till1(|c| match c {
            '\\' | 'c' | 'C' | '\'' | '"' | '$' | '/' | '-' | ';' => return true,
            _ => return false,
        }),
    ));
    let mut statement_terminator = alt((
        psql_meta_cmd,
        psql_copy_from_stdin,
        sql_copy_from_stdin,
        tag(";"),
    ));
    'outer: loop {
        'inner: loop {
            if let Ok((r, _)) = not_statement(rest) {
                rest = r;
            } else {
                break 'inner;
            }
        }
        if let Ok((r, _)) = statement_terminator(rest) {
            rest = r;
            break 'outer;
        } else if rest == "" {
            break;
        } else {
            let (r, _) = recognize(anychar)(rest)?;
            rest = r;
        }
    }
    // consume any trailing line-comments and newlines
    let (mut rest, _) = opt(is_a(" \t"))(rest)?;
    if let Ok((r, _)) = line_comment(rest) {
        rest = r;
    }
    loop {
        if let Ok((r, _)) = line_ending::<&str, nom::error::Error<&str>>(rest) {
            rest = r
        } else {
            break;
        }
    }
    return Ok((rest, &input[..input.len() - rest.len()]));
}

#[test]
fn test_statement() {
    let with_start_space = "
    select 1;";
    assert_eq!(statement(with_start_space), Ok(("", with_start_space)));
    let simple_inline = "select 1; select 2;";
    assert_eq!(statement(simple_inline), Ok(("select 2;", "select 1; ")));
    let with_comment = "select 1; -- get a number";
    assert_eq!(statement(with_comment), Ok(("", with_comment)));
    assert_eq!(statement(";"), Ok(("", ";")));
    let what = "COPY instead_of_insert_tbl_view_2 FROM stdin;
test1
\\.
";
    assert_eq!(statement(what), Ok(("", what)));
    // assert!(statement(
    //     "\n    copy bar from stdin;
    // arbitrary, text\n\\."
    // )
    // .is_ok());
    let bad = "
    copy foo from stdin;
    copy bar from stdin;
    arbitrary, text\n\\.";
    assert_eq!(
        statement(bad),
        Ok((
            "    copy bar from stdin;
    arbitrary, text\n\\.",
            "\n    copy foo from stdin;\n",
        ))
    );
    let worse = "

-- too many columns in column list: should fail
COPY x (a, b, c, d, e, d, c) from stdin;

-- missing data: should fail
COPY x from stdin;

\\.\n";
    assert_eq!(
        statement(worse),
        Ok((
            "-- missing data: should fail
COPY x from stdin;

\\.\n",
            "\n\n-- too many columns in column list: should fail
COPY x (a, b, c, d, e, d, c) from stdin;\n\n",
        ))
    );
    let maybs = r#"
CREATE TRIGGER trg_x_before BEFORE INSERT ON x
FOR EACH ROW EXECUTE PROCEDURE fn_x_before();

COPY x (a, b, c, d, e) from stdin;
9999	\N	\\N	\NN	\N
10000	21	31	41	51
\.

COPY x (b, d) from stdin;
1	test_1
\.

COPY x (b, d) from stdin;
2	test_2
3	test_3
4	test_4
5	test_5
\.

COPY x (a, b, c, d, e) from stdin;
10001	22	32	42	52
10002	23	33	43	53
10003	24	34	44	54
10004	25	35	45	55
10005	26	36	46	56
\.

-- non-existent column in column list: should fail
COPY x (xyz) from stdin;

-- too many columns in column list: should fail
COPY x (a, b, c, d, e, d, c) from stdin;

-- missing data: should fail
COPY x from stdin;

\.
COPY x from stdin;
2000	230	23	23
\.
COPY x from stdin;
2001	231	\N	\N
\.

-- extra data: should fail
COPY x from stdin;
2002	232	40	50	60	70	80
\.

-- various COPY options: delimiters, oids, NULL string, encoding
COPY x (b, c, d, e) from stdin delimiter ',' null 'x';
x,45,80,90
x,\x,\\x,\\\x
x,\,,\\\,,\\
\.

COPY x from stdin WITH DELIMITER AS ';' NULL AS '';
3000;;c;;
\.

COPY x from stdin WITH DELIMITER AS ':' NULL AS E'\\X' ENCODING 'sql_ascii';
4000:\X:C:\X:\X
4001:1:empty::
4002:2:null:\X:\X
4003:3:Backslash:\\:\\
4004:4:BackslashX:\\X:\\X
4005:5:N:\N:\N
4006:6:BackslashN:\\N:\\N
4007:7:XX:\XX:\XX
4008:8:Delimiter:\::\:
\.

COPY x TO stdout WHERE a = 1;
COPY x from stdin WHERE a = 50004;
50003	24	34	44	54
50004	25	35	45	55
50005	26	36	46	56
\.

COPY x from stdin WHERE a > 60003;
60001	22	32	42	52
60002	23	33	43	53
60003	24	34	44	54
60004	25	35	45	55
60005	26	36	46	56
\.

COPY x from stdin WHERE f > 60003;

COPY x from stdin WHERE a = max(x.b);

COPY x from stdin WHERE a IN (SELECT 1 FROM x);

COPY x from stdin WHERE a IN (generate_series(1,5));

COPY x from stdin WHERE a = row_number() over(b);


-- check results of copy in
SELECT * FROM x;

-- check copy out
COPY x TO stdout;
COPY x (c, e) TO stdout;
COPY x (b, e) TO stdout WITH NULL 'I''m null';

CREATE TEMP TABLE y (
    col1 text,
    col2 text
);

INSERT INTO y VALUES ('Jackson, Sam', E'\\h');
INSERT INTO y VALUES ('It is "perfect".',E'\t');
INSERT INTO y VALUES ('', NULL);

COPY y TO stdout WITH CSV;
COPY y TO stdout WITH CSV QUOTE '''' DELIMITER '|';
COPY y TO stdout WITH CSV FORCE QUOTE col2 ESCAPE E'\\' ENCODING 'sql_ascii';
COPY y TO stdout WITH CSV FORCE QUOTE *;

-- Repeat above tests with new 9.0 option syntax

COPY y TO stdout (FORMAT CSV);
COPY y TO stdout (FORMAT CSV, QUOTE '''', DELIMITER '|');
COPY y TO stdout (FORMAT CSV, FORCE_QUOTE (col2), ESCAPE E'\\');
COPY y TO stdout (FORMAT CSV, FORCE_QUOTE *);

\copy y TO stdout (FORMAT CSV)
\copy y TO stdout (FORMAT CSV, QUOTE '''', DELIMITER '|')
\copy y TO stdout (FORMAT CSV, FORCE_QUOTE (col2), ESCAPE E'\\')
\copy y TO stdout (FORMAT CSV, FORCE_QUOTE *)

--test that we read consecutive LFs properly

CREATE TEMP TABLE testnl (a int, b text, c int);

COPY testnl FROM stdin CSV;
1,"a field with two LFs

inside",2
\.

-- test end of copy marker
CREATE TEMP TABLE testeoc (a text);

COPY testeoc FROM stdin CSV;
a\.
\.b
c\.d
"\."
\.

COPY testeoc TO stdout CSV;

-- test handling of nonstandard null marker that violates escaping rules

CREATE TEMP TABLE testnull(a int, b text);
INSERT INTO testnull VALUES (1, E'\\0'), (NULL, NULL);

COPY testnull TO stdout WITH NULL AS E'\\0';

COPY testnull FROM stdin WITH NULL AS E'\\0';
42	\\0
\0	\0
\.

SELECT * FROM testnull;

BEGIN;
CREATE TABLE vistest (LIKE testeoc);
COPY vistest FROM stdin CSV;
a0
b
\.
COMMIT;
SELECT * FROM vistest;
BEGIN;
TRUNCATE vistest;
COPY vistest FROM stdin CSV;
a1
b
\.
SELECT * FROM vistest;
SAVEPOINT s1;
TRUNCATE vistest;
COPY vistest FROM stdin CSV;
d1
e
\.
SELECT * FROM vistest;
COMMIT;
SELECT * FROM vistest;

BEGIN;
TRUNCATE vistest;
COPY vistest FROM stdin CSV FREEZE;
a2
b
\.
SELECT * FROM vistest;
SAVEPOINT s1;
TRUNCATE vistest;
COPY vistest FROM stdin CSV FREEZE;
d2
e
\.
SELECT * FROM vistest;
COMMIT;
SELECT * FROM vistest;

BEGIN;
TRUNCATE vistest;
COPY vistest FROM stdin CSV FREEZE;
x
y
\.
SELECT * FROM vistest;
COMMIT;
TRUNCATE vistest;
COPY vistest FROM stdin CSV FREEZE;
p
g
\.
BEGIN;
TRUNCATE vistest;
SAVEPOINT s1;
COPY vistest FROM stdin CSV FREEZE;
m
k
\.
COMMIT;
BEGIN;
INSERT INTO vistest VALUES ('z');
SAVEPOINT s1;
TRUNCATE vistest;
ROLLBACK TO SAVEPOINT s1;
COPY vistest FROM stdin CSV FREEZE;
d3
e
\.
COMMIT;
CREATE FUNCTION truncate_in_subxact() RETURNS VOID AS
$$
BEGIN
    TRUNCATE vistest;
EXCEPTION
    WHEN OTHERS THEN
    INSERT INTO vistest VALUES ('subxact failure');
END;
$$ language plpgsql;
BEGIN;
INSERT INTO vistest VALUES ('z');
SELECT truncate_in_subxact();
COPY vistest FROM stdin CSV FREEZE;
d4
e
\.
SELECT * FROM vistest;
COMMIT;
SELECT * FROM vistest;
-- Test FORCE_NOT_NULL and FORCE_NULL options
CREATE TEMP TABLE forcetest (
    a INT NOT NULL,
    b TEXT NOT NULL,
    c TEXT,
    d TEXT,
    e TEXT
);
\pset null NULL
-- should succeed with no effect ("b" remains an empty string, "c" remains NULL)
BEGIN;
COPY forcetest (a, b, c) FROM STDIN WITH (FORMAT csv, FORCE_NOT_NULL(b), FORCE_NULL(c));
1,,""
\.
COMMIT;
SELECT b, c FROM forcetest WHERE a = 1;
-- should succeed, FORCE_NULL and FORCE_NOT_NULL can be both specified
BEGIN;
COPY forcetest (a, b, c, d) FROM STDIN WITH (FORMAT csv, FORCE_NOT_NULL(c,d), FORCE_NULL(c,d));
2,'a',,""
\.
COMMIT;
SELECT c, d FROM forcetest WHERE a = 2;
-- should fail with not-null constraint violation
BEGIN;
COPY forcetest (a, b, c) FROM STDIN WITH (FORMAT csv, FORCE_NULL(b), FORCE_NOT_NULL(c));
3,,""
\.
ROLLBACK;
-- should fail with "not referenced by COPY" error
BEGIN;
COPY forcetest (d, e) FROM STDIN WITH (FORMAT csv, FORCE_NOT_NULL(b));
ROLLBACK;
-- should fail with "not referenced by COPY" error
BEGIN;
COPY forcetest (d, e) FROM STDIN WITH (FORMAT csv, FORCE_NULL(b));
ROLLBACK;
\pset null ''

-- test case with whole-row Var in a check constraint
create table check_con_tbl (f1 int);
create function check_con_function(check_con_tbl) returns bool as $$
begin
    raise notice 'input = %', row_to_json($1);
    return $1.f1 > 0;
end $$ language plpgsql immutable;
alter table check_con_tbl add check (check_con_function(check_con_tbl.*));
\d+ check_con_tbl
copy check_con_tbl from stdin;
1
\N
\.
copy check_con_tbl from stdin;
0
\.
select * from check_con_tbl;

-- test with RLS enabled.
CREATE ROLE regress_rls_copy_user;
CREATE ROLE regress_rls_copy_user_colperms;
CREATE TABLE rls_t1 (a int, b int, c int);

COPY rls_t1 (a, b, c) from stdin;
1	4	1
2	3	2
3	2	3
4	1	4
\.

CREATE POLICY p1 ON rls_t1 FOR SELECT USING (a % 2 = 0);
ALTER TABLE rls_t1 ENABLE ROW LEVEL SECURITY;
ALTER TABLE rls_t1 FORCE ROW LEVEL SECURITY;

GRANT SELECT ON TABLE rls_t1 TO regress_rls_copy_user;
GRANT SELECT (a, b) ON TABLE rls_t1 TO regress_rls_copy_user_colperms;

-- all columns
COPY rls_t1 TO stdout;
COPY rls_t1 (a, b, c) TO stdout;

-- subset of columns
COPY rls_t1 (a) TO stdout;
COPY rls_t1 (a, b) TO stdout;

-- column reordering
COPY rls_t1 (b, a) TO stdout;

SET SESSION AUTHORIZATION regress_rls_copy_user;

-- all columns
COPY rls_t1 TO stdout;
COPY rls_t1 (a, b, c) TO stdout;

-- subset of columns
COPY rls_t1 (a) TO stdout;
COPY rls_t1 (a, b) TO stdout;

-- column reordering
COPY rls_t1 (b, a) TO stdout;

RESET SESSION AUTHORIZATION;

SET SESSION AUTHORIZATION regress_rls_copy_user_colperms;

-- attempt all columns (should fail)
COPY rls_t1 TO stdout;
COPY rls_t1 (a, b, c) TO stdout;

-- try to copy column with no privileges (should fail)
COPY rls_t1 (c) TO stdout;

-- subset of columns (should succeed)
COPY rls_t1 (a) TO stdout;
COPY rls_t1 (a, b) TO stdout;

RESET SESSION AUTHORIZATION;

-- test with INSTEAD OF INSERT trigger on a view
CREATE TABLE instead_of_insert_tbl(id serial, name text);
CREATE VIEW instead_of_insert_tbl_view AS SELECT ''::text AS str;

COPY instead_of_insert_tbl_view FROM stdin; -- fail
test1
\.

CREATE FUNCTION fun_instead_of_insert_tbl() RETURNS trigger AS $$
BEGIN
    INSERT INTO instead_of_insert_tbl (name) VALUES (NEW.str);
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER trig_instead_of_insert_tbl_view
    INSTEAD OF INSERT ON instead_of_insert_tbl_view
    FOR EACH ROW EXECUTE PROCEDURE fun_instead_of_insert_tbl();

COPY instead_of_insert_tbl_view FROM stdin;
test1
\.

SELECT * FROM instead_of_insert_tbl;

-- Test of COPY optimization with view using INSTEAD OF INSERT
-- trigger when relation is created in the same transaction as
-- when COPY is executed.
BEGIN;
CREATE VIEW instead_of_insert_tbl_view_2 as select ''::text as str;
CREATE TRIGGER trig_instead_of_insert_tbl_view_2
    INSTEAD OF INSERT ON instead_of_insert_tbl_view_2
    FOR EACH ROW EXECUTE PROCEDURE fun_instead_of_insert_tbl();

COPY instead_of_insert_tbl_view_2 FROM stdin;
test1
\.

SELECT * FROM instead_of_insert_tbl;
COMMIT;

-- clean up
DROP TABLE forcetest;
DROP TABLE vistest;
DROP FUNCTION truncate_in_subxact();
DROP TABLE x, y;
DROP TABLE rls_t1 CASCADE;
DROP ROLE regress_rls_copy_user;
DROP ROLE regress_rls_copy_user_colperms;
DROP FUNCTION fn_x_before();
DROP FUNCTION fn_x_after();
DROP TABLE instead_of_insert_tbl;
DROP VIEW instead_of_insert_tbl_view;
DROP VIEW instead_of_insert_tbl_view_2;
DROP FUNCTION fun_instead_of_insert_tbl();

"#;
    assert!(many0(statement)(maybs).is_ok());
}
