open Grain_tests.TestFramework;
open Grain_tests.Runner;
open Grain_parsing;
open Grain_parsing.Ast_helper;

describe("parsing", ({test, testSkip}) => {
  let test_or_skip =
    Sys.backend_type == Other("js_of_ocaml") ? testSkip : test;
  let assertParse = makeParseRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertFileRun = makeFileRunner(test_or_skip);

  // operators
  assertFileRun(
    "custom_operator",
    "customOperator",
    "Ok(3)\nErr(\"Division by zero!\")\n",
  );
  let a =
    Exp.ident(
      Location.mknoloc(Identifier.IdentName(Location.mknoloc("a"))),
    );
  let b =
    Exp.ident(
      Location.mknoloc(Identifier.IdentName(Location.mknoloc("b"))),
    );
  let c =
    Exp.ident(
      Location.mknoloc(Identifier.IdentName(Location.mknoloc("c"))),
    );
  let testOp = op =>
    assertParse(
      op,
      "a " ++ op ++ " b",
      {
        statements: [
          Top.expr(
            Exp.apply(
              Exp.ident(
                Location.mknoloc(
                  Identifier.IdentName(Location.mknoloc(op)),
                ),
              ),
              [a, b],
            ),
          ),
        ],
        comments: [],
        prog_loc: Location.dummy_loc,
      },
    );

  let ops = [
    // Smoketest of operators which should all work
    "||+",
    "||^",
    "&&*&^%",
    "|*",
    "^^^",
    "&-",
    "==!",
    "==$",
    "==*==",
    "!==^",
    "<<<<<",
    "<%>",
    "<=>",
    ">>>>",
    ">>>>>>>>",
    "><><><",
    "+==",
    "+!",
    "++!",
    "+-+",
    "**//**",
    "**",
    // verify that common fan favorites work
    "??",
    "???",
    "+.",
    "-.",
    "*.",
    "/.",
    ">>=",
    "|>",
    ">:",
    "%%",
    "===",
    "!==",
    "==?",
    "&?",
    "++",
    "--",
    "^*^",
    "^-^",
  ];
  List.iter(testOp, ops);

  // verify precedence is maintained
  assertParse(
    "custom_op_precedence_1",
    "a +++ b *** c",
    {
      statements: [
        Top.expr(
          Exp.apply(
            Exp.ident(
              Location.mknoloc(
                Identifier.IdentName(Location.mknoloc("+++")),
              ),
            ),
            [
              a,
              Exp.apply(
                Exp.ident(
                  Location.mknoloc(
                    Identifier.IdentName(Location.mknoloc("***")),
                  ),
                ),
                [b, c],
              ),
            ],
          ),
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "custom_op_precedence_2",
    "a &&-- b &-- c",
    {
      statements: [
        Top.expr(
          Exp.apply(
            Exp.ident(
              Location.mknoloc(
                Identifier.IdentName(Location.mknoloc("&&--")),
              ),
            ),
            [
              a,
              Exp.apply(
                Exp.ident(
                  Location.mknoloc(
                    Identifier.IdentName(Location.mknoloc("&--")),
                  ),
                ),
                [b, c],
              ),
            ],
          ),
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "custom_op_precedence_3",
    "a ||-- b |-- c",
    {
      statements: [
        Top.expr(
          Exp.apply(
            Exp.ident(
              Location.mknoloc(
                Identifier.IdentName(Location.mknoloc("||--")),
              ),
            ),
            [
              a,
              Exp.apply(
                Exp.ident(
                  Location.mknoloc(
                    Identifier.IdentName(Location.mknoloc("|--")),
                  ),
                ),
                [b, c],
              ),
            ],
          ),
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "regression_issue_1473",
    "a << b >> c",
    {
      statements: [
        Top.expr(
          Exp.apply(
            Exp.ident(
              Location.mknoloc(
                Identifier.IdentName(Location.mknoloc(">>")),
              ),
            ),
            [
              Exp.apply(
                Exp.ident(
                  Location.mknoloc(
                    Identifier.IdentName(Location.mknoloc("<<")),
                  ),
                ),
                [a, b],
              ),
              c,
            ],
          ),
        ),
      ],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );

  // Whitespace tests

  // Reason does not support OCaml's Unicode escapes, which is why these are
  // UTF-8 byte sequences instead of pretty Unicode escapes

  assertParse(
    "whitespace_1",
    // In order,
    // HORIZONTAL TABULATION
    // VERTICAL TABULATION
    // SPACE
    // LEFT-TO-RIGHT MARK
    // RIGHT-TO-LEFT MARK
    // LINE FEED
    // FORM FEED
    // CARRIAGE RETURN
    // NEXT LINE
    // LINE SEPARATOR
    // PARAGRAPH SEPARATOR
    "
    \x09
    \x0b
    \x20
    \xe2\x80\x8e
    \xe2\x80\x8f
    \x0a
    \x0c
    \x0d
    \xc2\x85
    \xe2\x80\xa8
    \xe2\x80\xa9
    ",
    {statements: [], comments: [], prog_loc: Location.dummy_loc},
  );

  assertCompileError(
    "invalid_whitespace_nbsp",
    "\xc2\xa0",
    "Grain lexer doesn't recognize this token",
  );
  assertCompileError(
    "invalid_whitespace_emspace",
    "\xe2\x80\x83",
    "Grain lexer doesn't recognize this token",
  );
  assertCompileError(
    "invalid_whitespace_hairspace",
    "\xe2\x80\x8a",
    "Grain lexer doesn't recognize this token",
  );
  assertCompileError(
    "invalid_whitespace_ideographicspace",
    "\xe3\x80\x80",
    "Grain lexer doesn't recognize this token",
  );

  assertParse(
    "end_of_statement_linefeed",
    "a\x0ab",
    {
      statements: [Top.expr(a), Top.expr(b)],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "end_of_statement_formfeed",
    "a\x0cb",
    {
      statements: [Top.expr(a), Top.expr(b)],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "end_of_statement_carriagereturn",
    "a\x0db",
    {
      statements: [Top.expr(a), Top.expr(b)],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "end_of_statement_crlf",
    "a\x0d\x0ab",
    {
      statements: [Top.expr(a), Top.expr(b)],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "end_of_statement_nextline",
    "a\xc2\x85b",
    {
      statements: [Top.expr(a), Top.expr(b)],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "end_of_statement_lineseparator",
    "a\xe2\x80\xa8b",
    {
      statements: [Top.expr(a), Top.expr(b)],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
  assertParse(
    "end_of_statement_paragraphseparator",
    "a\xe2\x80\xa9b",
    {
      statements: [Top.expr(a), Top.expr(b)],
      comments: [],
      prog_loc: Location.dummy_loc,
    },
  );
});
