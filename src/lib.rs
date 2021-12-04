use nom::multi::many1;
use nom::branch::alt;
use nom::bytes::complete::{
    is_a, tag_no_case, take_till, take_till1, take_until, take_until1, take_while,
};
use nom::character::complete::{alpha1, anychar, line_ending};
use nom::character::is_space;
use nom::combinator::eof;
use nom::error::Error;
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
    // println!("{} / {}", start, &rest);
    // let (rest, middle) = not(tag(start))(rest)?;
    // let (rest, end) = tag(start)(rest)?;
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

fn psql_if(input: &str) -> IResult<&str, &str> {
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

// COPY ... FROM STDIN
// \copy ... FROM stdin / pstdin
// just use regexes

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
    // recognize(tuple((line_ending, tag("."), alt((line_ending, eof)))))(input)?
    // let (rest, inline) = take_until("\n\\.\n")(input)?;
    // let (rest, delim) = tag("\n\\.\n")(rest)?;
    // return Ok((rest, &input[..inline.len() + delim.len()]));
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
        c_style_comment,
        line_comment,
        single_quote_string,
        double_quote_string,
        dollar_quote_string,
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
        let (m, statements) = many1(statement)(inline)?;
        assert_eq!(m.len(), 0usize);
        if statements.len() == 0 {
            rest = r;
        } else {
            let last = *statements.last().unwrap();
            if sql_copy_from_stdin(last).is_err() && psql_copy_from_stdin(last).is_err() {
                rest = r;
            }
        }
        // if sql_copy_from_stdin(inline).is_err() && psql_copy_from_stdin(inline).is_err() {
        //     rest = r;
        // }
    } else {
        rest = r;
    }

    return Ok((rest, &input[..input.len() - rest.len()]));
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
    let mut rest = input;
    let mut parser = alt((
        is_a(" \t\r\n"),
        psql_copy_from_stdin,
        sql_copy_from_stdin,
        psql_if,
        single_quote_string,
        double_quote_string,
        dollar_quote_string,
        c_style_comment,
        line_comment,
        // is_not("\\c\'\"$/-;"),
        // TODO: replace with is_not()
        take_till1(|c| match c {
            '\\' | 'c' | '\'' | '"' | '$' | '/' | '-' | ';' => return true,
            _ => return false,
        }),
    ));
    'outer: loop {
        'inner: loop {
            if let Ok((r, _)) = parser(rest) {
                rest = r;
            } else {
                break 'inner;
            }
        }
        if let Ok((r, _)) = alt((psql_meta_cmd, tag(";")))(rest) {
            rest = r;
            break 'outer;
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
}
