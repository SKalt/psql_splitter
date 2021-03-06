use nom::branch::alt;
use nom::bytes::complete::{is_a, tag_no_case, take_till, take_till1, take_until, take_until1};
use nom::character::complete::{alpha1, anychar, line_ending};
use nom::combinator::{eof, not};
use nom::multi::many0;
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
    let (mut rest, _) = alpha1(rest)?;
    loop {
        if let Ok((r, _)) = line_ending::<&str, nom::error::Error<&str>>(rest) {
            rest = r;
            break;
        } else if rest == "" {
            break;
        } else {
            rest = &rest[1..];
        }
    }
    return Ok((rest, &input[..input.len() - rest.len()]));
}

pub fn comment(input: &str) -> IResult<&str, &str> {
    return alt((c_style_comment, line_comment))(input);
}

pub fn comment_or_whitespace(input: &str) -> IResult<&str, &str> {
    return alt((is_a(" \t\r\n"), comment))(input);
}

fn string_or_comment(input: &str) -> IResult<&str, &str> {
    alt((
        comment,
        single_quote_string,
        double_quote_string,
        dollar_quote_string,
    ))(input)
}

fn psql_if(input: &str) -> IResult<&str, &str> {
    let (mut rest, _) = tag(r"\if")(input)?;
    loop {
        if rest.len() == 0 {
            // \endif not matched
            let _ = tag(r"\elif")(rest)?; // let the tag raise the error
        } else if let Ok((r, _)) = alt((string_or_comment, is_a(" \t\r\n")))(rest) {
            // consume whitespace, strings, comments
            rest = r
        } else if let Ok((r, _)) = psql_if(rest) {
            // handle nested \if..\endif clauses
            rest = r;
        } else if let Ok((r, _)) = tag::<&str, &str, nom::error::Error<&str>>(r"\endif")(rest) {
            // match a closing \endif tag and break
            rest = r;
            break;
        } else {
            // consume a character and try again
            rest = &rest[1..];
        }
    }
    let result = &input[..input.len() - rest.len()];
    assert!(result.starts_with(r"\if") && result.ends_with(r"\endif"));
    // `statement` will handle eating the rest of the line if appropriate
    return Ok((rest, result));
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
    let large_nested = include_str!("./large_nested_if.sql");
    let first_line = large_nested.find("\n").unwrap();
    let large_nested = &large_nested[first_line + 1..]; // skip the line-comment
    assert_eq!(
        psql_if(large_nested),
        Ok(("\n", &large_nested[..large_nested.len() - 1]))
    )
    // assert!(psql_if(&large_nested[first_line + 1..]).is_ok());
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
    let (rest, _) = tag_no_case("copy")(input)?;
    let (mut rest, _) = is_a(" \t\r\n")(rest)?;
    // TODO: watch out for statements, strings, comments
    let mut copy_delim = alt((
        tag_no_case::<&str, &str, nom::error::Error<&str>>("to"),
        tag_no_case::<&str, &str, nom::error::Error<&str>>("from"),
    ));
    loop {
        if let Ok((r, m)) = copy_delim(rest) {
            tag_no_case::<&str, &str, nom::error::Error<&str>>("from")(m)?;
            // fail if the next match is "to"; this is a "copy to stdout" statement or similar.
            rest = r;
            break;
        }
        if rest.len() == 0 {
            break;
        } else {
            rest = &rest[1..];
        }
    }
    // let (rest, _) = tag_no_case("from")(rest)?;
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

    // heuristic: check if another statement could be part of the inline stdin
    if inline.len() > 0 && (inline.contains(";")) {
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
    // strip leading whitespace and comments
    while let Ok((r, _)) = comment_or_whitespace(rest) {
        rest = r
    }

    let mut complete_statement = alt((
        psql_if,
        psql_copy_from_stdin,
        sql_copy_from_stdin,
        psql_meta_cmd,
    ));
    let mut statement_part = alt((
        is_a(" \t\r\n"),
        psql_if,
        string_or_comment,
        is_not(r#"'"$\/-;"#),
    ));
    let mut statement_terminator = alt((psql_meta_cmd, tag(";")));

    if let Ok((r, _)) = complete_statement(rest) {
        rest = r;
    } else {
        'outer: loop {
            'inner: loop {
                if let Ok((r, _)) = statement_part(rest) {
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
    //     let weird = include_str!("./weird.sql");
    //     let expected = "GRANT SELECT ON TABLE rls_t1 TO regress_rls_copy_user;\n";
    //     assert_eq!(statement(weird), Ok((&weird[expected.len()..], expected)));
    //     let with_start_space = "
    //     select 1;";
    //     assert_eq!(statement(with_start_space), Ok(("", with_start_space)));
    //     let simple_inline = "select 1; select 2;";
    //     assert_eq!(statement(simple_inline), Ok(("select 2;", "select 1; ")));
    //     let with_comment = "select 1; -- get a number";
    //     assert_eq!(statement(with_comment), Ok(("", with_comment)));
    //     assert_eq!(statement(";"), Ok(("", ";")));
    //     let what = "COPY instead_of_insert_tbl_view_2 FROM stdin;
    // test1
    // \\.
    // ";
    //     assert_eq!(statement(what), Ok(("", what)));
    //     // assert!(statement(
    //     //     "\n    copy bar from stdin;
    //     // arbitrary, text\n\\."
    //     // )
    //     // .is_ok());
    //     let bad = "
    //     copy foo from stdin;
    //     copy bar from stdin;
    //     arbitrary, text\n\\.";
    //     assert_eq!(
    //         statement(bad),
    //         Ok((
    //             "    copy bar from stdin;
    //     arbitrary, text\n\\.",
    //             "\n    copy foo from stdin;\n",
    //         ))
    //     );
    //     let worse = "

    // -- too many columns in column list: should fail
    // COPY x (a, b, c, d, e, d, c) from stdin;

    // -- missing data: should fail
    // COPY x from stdin;

    // \\.\n";
    //     assert_eq!(
    //         statement(worse),
    //         Ok((
    //             "-- missing data: should fail
    // COPY x from stdin;

    // \\.\n",
    //             "\n\n-- too many columns in column list: should fail
    // COPY x (a, b, c, d, e, d, c) from stdin;\n\n",
    //         ))
    //     );
    //     let run_on = "
    //     copy foo to stdout;
    //     select 1;
    //     copy bar from stdin;\n\\.\n";
    //     let expected = "
    //     copy foo to stdout;\n";
    //     assert_eq!(statement(run_on), Ok((&run_on[expected.len()..], expected)));
    //     let psql_if_example = include_str!("./if_basics.sql");
    //     let expected_remainder =
    //         "-- at this point query buffer should still have last valid line\n\\g\n";
    //     assert_eq!(
    //         statement(psql_if_example),
    //         Ok((
    //             expected_remainder,
    //             &psql_if_example[..psql_if_example.len() - expected_remainder.len()]
    //         ))
    //     );
    //     let inline_if = include_str!("./inline_if.sql");
    //     assert_eq!(statement(inline_if), Ok(("", inline_if)));
    let large_nested = include_str!("./large_nested_if.sql");
    assert_eq!(statement(large_nested), Ok(("", large_nested)));
}

/// doesn't speak `:variable`s yet
/// note the argument must be a complete statement
pub fn is_psql(statement: &str) -> bool {
    let mut rest = statement;
    let mut complete_psql_statement = alt((
        psql_if,
        psql_copy_from_stdin,
        sql_copy_from_stdin,
        psql_meta_cmd,
    ));
    let mut statement_part = alt((is_a(" \t\r\n"), string_or_comment, is_not(r#"'"$\/-;"#)));

    if let Ok(_) = complete_psql_statement(rest) {
        return true;
    }

    loop {
        if rest == "" {
            return false;
        } else if rest.starts_with(";") {
            return false;
        } else if let Ok(_) = psql_meta_cmd(rest) {
            return true;
        } else if let Ok((r, _)) = statement_part(rest) {
            rest = r;
        } else {
            rest = &rest[1..];
        }
    }
}

#[test]
fn test_is_psql() {
    assert!(is_psql(r"select 1 as foo \gset"));
    assert!(!is_psql("select 1;"));
    assert!(is_psql("copy foo from stdin;\n\\.\n"));
}

#[test]
fn statement_integration_test_psql() {
    let psql = include_str!("./regress_psql.sql");
    let mut rest = psql;
    let mut statements: Vec<&str> = vec![];
    while let Ok((r, m)) = statement(rest) {
        statements.push(m);
        rest = r;
    }
    assert_eq!(rest, "");
    assert_eq!(psql, statements.join("").as_str());
}
