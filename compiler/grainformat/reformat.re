open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;

module Doc = Res_doc;

let allComments: ref(list(Parsetree.comment)) = ref([]);

let getCommentLoc = (comment: Parsetree.comment) =>
  switch (comment) {
  | Line(cmt) => cmt.cmt_loc
  | Block(cmt) => cmt.cmt_loc
  | Doc(cmt) => cmt.cmt_loc
  | Shebang(cmt) => cmt.cmt_loc
  };

let removeComment =
    (comment: Parsetree.comment, comments: list(Parsetree.comment)) => {
  let removeLoc = getCommentLoc(comment);
  List.filter(
    c => {
      let loc = getCommentLoc(c);

      if (loc == removeLoc) {
        false;
      } else {
        true;
      };
    },
    comments,
  );
};

let get_raw_pos_info = (pos: Lexing.position) => (
  pos.pos_fname,
  pos.pos_lnum,
  pos.pos_cnum - pos.pos_bol,
  pos.pos_bol,
);

let makeStartLoc = (loc: Grain_parsing.Location.t) => {
  let mergedLoc: Grain_parsing.Location.t = {
    loc_start: loc.loc_start,
    loc_end: loc.loc_start,
    loc_ghost: loc.loc_ghost,
  };
  mergedLoc;
};

let makeEndLoc = (loc: Grain_parsing.Location.t) => {
  let mergedLoc: Grain_parsing.Location.t = {
    loc_start: loc.loc_start,
    loc_end: loc.loc_start,
    loc_ghost: loc.loc_ghost,
  };
  mergedLoc;
};

let print_loc = (msg: string, loc: Grain_parsing.Location.t) => {
  let (file, line, startchar, _) = get_raw_pos_info(loc.loc_start);
  let (_, endline, endchar, _) = get_raw_pos_info(loc.loc_end);
  /*let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in*/

  if (startchar >= 0) {
    if (line == endline) {
      print_endline(
        msg
        ++ " "
        ++ string_of_int(line)
        ++ ":"
        ++ string_of_int(startchar)
        ++ ","
        ++ string_of_int(endchar),
      );
    } else {
      print_endline(
        msg
        ++ " "
        ++ string_of_int(line)
        ++ ":"
        ++ string_of_int(startchar)
        ++ " - "
        ++ string_of_int(endline)
        ++ ":"
        ++ string_of_int(endchar),
      );
    };
  };
};

let print_comment = (comment: Parsetree.comment) => {
  let endloc =
    switch (comment) {
    | Line(cmt) => cmt.cmt_loc.loc_end
    | Block(cmt) => cmt.cmt_loc.loc_end
    | Doc(cmt) => cmt.cmt_loc.loc_end
    | Shebang(cmt) => cmt.cmt_loc.loc_end
    };

  let startloc =
    switch (comment) {
    | Line(cmt) => cmt.cmt_loc.loc_start
    | Block(cmt) => cmt.cmt_loc.loc_start
    | Doc(cmt) => cmt.cmt_loc.loc_start
    | Shebang(cmt) => cmt.cmt_loc.loc_start
    };

  let (_file, stmtstartline, startchar, _sbol) = get_raw_pos_info(startloc);
  let (_file, stmtendline, endchar, _sbol) = get_raw_pos_info(endloc);

  print_int(stmtstartline);
  print_string(":");
  print_int(startchar);

  print_string(",");
  print_int(stmtendline);
  print_string(":");
  print_int(endchar);
  print_string(" -  ");

  switch (comment) {
  | Line(cmt) => print_endline(cmt.cmt_source)
  | Block(cmt) => print_endline(cmt.cmt_source)
  | Doc(cmt) => print_endline(cmt.cmt_source)
  | Shebang(cmt) => print_endline(cmt.cmt_source)
  };
};

type stmtList =
  | BlankLine
  | BlockComment(string)
  | Statement(Parsetree.toplevel_stmt);

type sugaredListItem =
  | Regular(Grain_parsing.Parsetree.expression)
  | Spread(Grain_parsing.Parsetree.expression);

type sugaredPatternItem =
  | RegularPattern(Grain_parsing.Parsetree.pattern)
  | SpreadPattern(Grain_parsing.Parsetree.pattern);

let addParens = doc =>
  Doc.group(
    Doc.concat([
      Doc.lparen,
      Doc.indent(Doc.concat([Doc.softLine, doc])),
      Doc.softLine,
      Doc.rparen,
    ]),
  );

let addBraces = doc =>
  Doc.group(
    Doc.concat([
      Doc.lbrace,
      Doc.indent(Doc.concat([Doc.softLine, doc])),
      Doc.softLine,
      Doc.rbrace,
    ]),
  );

let infixop = (op: string) => {
  switch (op) {
  | "+"
  | "-"
  | "*"
  | "/"
  | "%"
  | "is"
  | "isnt"
  | "=="
  | "++"
  | "!="
  | "^"
  | "<"
  | "<<"
  | ">"
  | ">>"
  | ">>>"
  | "<="
  | ">="
  | "&"
  | "&&"
  | "|"
  | "||" => true
  | _ => false
  };
};

let commentToDoc = (comment: Parsetree.comment) =>
  switch (comment) {
  | Line(cmt) => Doc.text(cmt.cmt_source)
  | Block(cmt) =>
    //  print_endline(cmt.cmt_source);
    Doc.concat([Doc.text(cmt.cmt_source)])
  | Doc(cmt) => Doc.concat([Doc.text(cmt.cmt_source)])
  | Shebang(cmt) => Doc.text(cmt.cmt_source)
  };

let getCommentEndLine = (comment: Parsetree.comment) => {
  let endloc =
    switch (comment) {
    | Line(cmt) => cmt.cmt_loc.loc_end
    | Block(cmt) => cmt.cmt_loc.loc_end
    | Doc(cmt) => cmt.cmt_loc.loc_end
    | Shebang(cmt) => cmt.cmt_loc.loc_end
    };

  let (_, endline, _, _) = get_raw_pos_info(endloc);
  endline;
};

let get_comments_between =
    (
      loc1: Grain_parsing.Location.t,
      loc2: Grain_parsing.Location.t,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let (_, endline1, endchar1, _) = get_raw_pos_info(loc1.loc_end);
  let (_, startline2, startchar2, _) = get_raw_pos_info(loc2.loc_start);

  print_loc("get comment after", loc1);
  print_loc("get comment before", loc2);

  List.filter(
    (c: Grain_parsing.Parsetree.comment) => {
      switch (c) {
      | Block(cmt)
      | Line(cmt) =>
        let (_efile, bstartline, bstartchar, _ebol) =
          get_raw_pos_info(cmt.cmt_loc.loc_start);
        let (_efile, bendline, bendchar, _ebol) =
          get_raw_pos_info(cmt.cmt_loc.loc_end);

        print_loc("comparing to ", cmt.cmt_loc);
        let after =
          if (bstartline > endline1) {
            true;
          } else if (bstartline == endline1) {
            if (bstartchar >= endchar1) {
              true;
            } else {
              false;
            };
          } else {
            false;
          };
        let before =
          if (bendline > startline2) {
            false;
          } else if (bendline == startline2) {
            if (bendchar <= startchar2) {
              true;
            } else {
              false;
            };
          } else {
            true;
          };

        if (after && before) {
          print_endline("Match!");
        } else {
          print_endline("No Match!");
        };

        after && before;

      | _ => false
      }
    },
    comments,
  );
};

let get_comments_before =
    (
      loc: Grain_parsing.Location.t,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  let (_, line, startchar, _) = get_raw_pos_info(loc.loc_start);

  print_endline(
    "gcb, line "
    ++ string_of_int(line)
    ++ " char: "
    ++ string_of_int(startchar),
  );

  List.filter(
    (c: Grain_parsing.Parsetree.comment) => {
      switch (c) {
      | Block(cmt)
      | Line(cmt) =>
        let (_efile, bstartline, bstartchar, _ebol) =
          get_raw_pos_info(cmt.cmt_loc.loc_start);

        let (_efile, bendline, bendchar, _ebol) =
          get_raw_pos_info(cmt.cmt_loc.loc_end);

        print_endline(
          "gcb-block-starts, line "
          ++ string_of_int(bstartline)
          ++ " char: "
          ++ string_of_int(bstartchar),
        );
        print_endline(
          "gcb-block-ends, line "
          ++ string_of_int(bendline)
          ++ " char: "
          ++ string_of_int(bendchar),
        );

        if (bendline < line) {
          true;
        } else if (bendline == line) {
          bendchar < startchar;
        } else {
          false;
        };

      | Doc(_) =>
        //    print_endline("Doc");
        false
      | Shebang(_) =>
        // print_endline("Shebang");
        false
      }
    },
    comments,
  );
};

let find_comments_in_range =
    (
      range_start: int,
      range_end: int,
      comments: list(Grain_parsing.Parsetree.comment),
    ) => {
  List.find_all(
    (c: Grain_parsing.Parsetree.comment) => {
      switch (c) {
      | Line(cmt) =>
        let (_file, startline, _startchar, _sbol) =
          get_raw_pos_info(cmt.cmt_loc.loc_start);

        startline >= range_start && startline <= range_end;

      | Shebang(cmt) =>
        let (_file, startline, _startchar, _sbol) =
          get_raw_pos_info(cmt.cmt_loc.loc_start);

        startline >= range_start && startline <= range_end;

      | Block(cmt) =>
        let (_file, startline, _startchar, _sbol) =
          get_raw_pos_info(cmt.cmt_loc.loc_start);

        let (_efile, endline, _endchar, _ebol) =
          get_raw_pos_info(cmt.cmt_loc.loc_end);

        startline >= range_start && endline <= range_end;

      | Doc(cmt) =>
        let (_file, startline, _startchar, _sbol) =
          get_raw_pos_info(cmt.cmt_loc.loc_start);

        let (_efile, endline, _endchar, _ebol) =
          get_raw_pos_info(cmt.cmt_loc.loc_end);

        startline >= range_start && endline <= range_end;
      }
    },
    comments,
  );
};

let rec resugar_list_patterns = (patterns: list(Parsetree.pattern)) => {
  let processed_list = resugar_pattern_list_inner(patterns);
  let items =
    List.map(
      i =>
        switch (i) {
        | RegularPattern(e) => print_pattern(e)
        | SpreadPattern(e) =>
          Doc.concat([Doc.text("..."), print_pattern(e)])
        },
      processed_list,
    );
  Doc.group(
    Doc.concat([
      Doc.lbracket,
      Doc.join(Doc.concat([Doc.comma, Doc.line]), items),
      Doc.rbracket,
    ]),
  );
}

and resugar_pattern_list_inner = (patterns: list(Parsetree.pattern)) => {
  let arg1 = List.nth(patterns, 0);
  let arg2 = List.nth(patterns, 1);

  switch (arg2.ppat_desc) {
  | PPatConstruct(innerfunc, innerpatterns) =>
    let func =
      switch (innerfunc.txt) {
      | IdentName(name) => name
      | _ => ""
      };

    if (func == "[]") {
      [RegularPattern(arg1)];
    } else if (is_empty_pattern_array(arg2)) {
      [RegularPattern(arg1)];
    } else {
      [RegularPattern(arg1), SpreadPattern(arg2)];
    };
  | _ =>
    if (is_empty_pattern_array(arg2)) {
      [RegularPattern(arg1)];
    } else {
      [RegularPattern(arg1), SpreadPattern(arg2)];
    }
  };
}

and is_empty_array = (expr: Parsetree.expression) => {
  switch (expr.pexp_desc) {
  | PExpId(loc) =>
    let loc_txt =
      switch (loc.txt) {
      | IdentName(name) => name
      | _ => ""
      };

    if (loc_txt == "[]") {
      true;
    } else {
      false;
    };
  | _ => false
  };
}

and is_empty_pattern_array = (pat: Parsetree.pattern) => {
  let _ =
    switch (pat.ppat_desc) {
    | PPatAny => false
    | PPatConstant(c) => false
    | PPatVar({txt, _}) =>
      if (txt == "[]") {
        true;
      } else {
        false;
      }
    | PPatTuple(patterns) => false
    | PPatArray(patterns) => false
    | PPatConstraint(pattern, parsed_type) => false
    | PPatConstruct(location, patterns) => false
    | _ => false
    };

  false;
}

and resugar_list = (expressions: list(Parsetree.expression)) => {
  let processed_list = resugar_list_inner(expressions);

  let items =
    List.map(
      i =>
        switch (i) {
        | Regular(e) => print_expression(~expr=e, ~parentIsArrow=false)
        | Spread(e) =>
          Doc.concat([
            Doc.text("..."),
            print_expression(~expr=e, ~parentIsArrow=false),
          ])
        },
      processed_list,
    );

  Doc.group(
    Doc.concat([
      Doc.lbracket,
      Doc.indent(
        Doc.concat([
          Doc.softLine,
          Doc.join(Doc.concat([Doc.comma, Doc.line]), items),
        ]),
      ),
      Doc.softLine,
      Doc.rbracket,
    ]),
  );
}

and resugar_list_inner = (expressions: list(Parsetree.expression)) => {
  let arg1 = List.nth(expressions, 0);
  let arg2 = List.nth(expressions, 1);

  switch (arg2.pexp_desc) {
  | PExpApp(innerfunc, innerexpressions) =>
    // let funcName = print_expression(~expr=innerfunc, ~parentIsArrow=false);
    // let funcNameAsString = Doc.toString(~width=20, funcName);

    let funcName = getFunctionName(innerfunc);

    if (funcName == "[...]") {
      let inner = resugar_list_inner(innerexpressions);
      List.append([Regular(arg1)], inner);
    } else {
      [Regular(arg1), Spread(arg2)];
    };
  | _ =>
    if (is_empty_array(arg2)) {
      [Regular(arg1)];
    } else {
      [Regular(arg1), Spread(arg2)];
    }
  };
}

and print_record_pattern =
    (
      patternlocs:
        list(
          (
            Grain_parsing__Location.loc(Grain_parsing__Identifier.t),
            Grain_parsing__Parsetree.pattern,
          ),
        ),
      closedflag: Grain_parsing__Asttypes.closed_flag,
    ) => {
  let close =
    switch (closedflag) {
    | Open => Doc.concat([Doc.text(","), Doc.space, Doc.text("_")])
    | Closed => Doc.nil
    };
  Doc.concat([
    Doc.lbrace,
    Doc.space,
    Doc.join(
      Doc.concat([Doc.comma, Doc.space]),
      List.map(
        (
          patternloc: (
            Grain_parsing__Location.loc(Grain_parsing__Identifier.t),
            Grain_parsing__Parsetree.pattern,
          ),
        ) => {
          let (loc, pat) = patternloc;

          let printed_ident: Doc.t = print_ident(loc.txt);
          let printed_pat = print_pattern(pat);

          let pun =
            switch (printed_ident) {
            | Text(i) =>
              switch ((printed_pat: Doc.t)) {
              | Text(e) => i == e
              | _ => false
              }
            | _ => false
            };
          if (pun) {
            printed_ident;
          } else {
            Doc.concat([printed_ident, Doc.text(": "), printed_pat]);
          };
        },
        patternlocs,
      ),
    ),
    close,
    Doc.space,
    Doc.rbrace,
  ]);
}
and print_pattern = (pat: Parsetree.pattern) => {
  print_endline("Pattern loc");
  // print_loc("pattern:", pat.ppat_loc);

  // Comments.debug_pattern(pat);

  let locAfter = Walktree.get_location_after(pat.ppat_loc);

  let trailingComment =
    switch (locAfter) {
    | None =>
      print_endline("No loc after pattern");
      Doc.nil;
    | Some(loc) =>
      print_endline("We have an item after us");

      let comments = get_comments_between(pat.ppat_loc, loc, allComments^);

      if (List.length(comments) > 0) {
        print_endline("A pattern is followed by comments");

        List.iter(c => print_comment(c), comments);

        let commentDocs =
          Doc.join(Doc.space, List.map(c => commentToDoc(c), comments));

        // List.iter(
        //   c => allComments := removeComment(c, allComments^),
        //   comments,
        // );
        Doc.concat([Doc.space, commentDocs]);
      } else {
        Doc.nil;
      };
    };

  // Comments.debug_pattern(pat);

  let printed_pattern =
    switch (pat.ppat_desc) {
    | PPatAny => Doc.text("_")
    | PPatConstant(c) => Doc.concat([print_constant(c), trailingComment])
    | PPatVar({txt, _}) =>
      if (infixop(txt)) {
        Doc.concat([Doc.lparen, Doc.text(txt), Doc.rparen]);
      } else {
        Doc.concat([Doc.text(txt), trailingComment]);
      }
    | PPatTuple(patterns) =>
      addParens(
        Doc.join(Doc.comma, List.map(p => print_pattern(p), patterns)),
      )
    | PPatArray(patterns) =>
      Doc.group(
        Doc.concat([
          Doc.lbracket,
          Doc.join(Doc.comma, List.map(p => print_pattern(p), patterns)),
          Doc.rbracket,
        ]),
      )
    | PPatRecord(patternlocs, closedflag) =>
      print_record_pattern(patternlocs, closedflag)
    | PPatConstraint(pattern, parsed_type) =>
      Doc.concat([
        print_pattern(pattern),
        Doc.concat([Doc.text(":"), Doc.space]),
        print_type(parsed_type),
      ])
    | PPatConstruct(location, patterns) =>
      let func =
        switch (location.txt) {
        | IdentName(name) => name
        | _ => ""
        };
      if (func == "[...]") {
        resugar_list_patterns(patterns);
      } else {
        Doc.concat([
          print_ident(location.txt),
          if (List.length(patterns) > 0) {
            addParens(
              Doc.join(
                Doc.comma,
                List.map(pat => print_pattern(pat), patterns),
              ),
            );
          } else {
            Doc.nil;
          },
        ]);
      };

    | PPatOr(pattern1, pattern2) => Doc.text("PPatOr")
    | PPatAlias(pattern, loc) => Doc.text("PPatAlias")
    };

  Doc.concat([printed_pattern, Doc.nil]);
}
and print_constant = (c: Parsetree.constant) => {
  let print_c =
    switch (c) {
    | PConstNumber(PConstNumberInt(i)) => Printf.sprintf("%s", i)
    | PConstNumber(PConstNumberFloat(f)) => Printf.sprintf("%s", f)
    | PConstNumber(PConstNumberRational(n, d)) =>
      Printf.sprintf("%s/%s", n, d)
    | PConstBytes(b) => Printf.sprintf("%s", b)
    | PConstChar(c) => Printf.sprintf("'%s'", c)
    | PConstFloat64(f) => Printf.sprintf("%sd", f)
    | PConstFloat32(f) => Printf.sprintf("%sf", f)
    | PConstInt32(i) => Printf.sprintf("%sl", i)
    | PConstInt64(i) => Printf.sprintf("%sL", i)
    | PConstWasmI32(i) => Printf.sprintf("%sn", i)
    | PConstWasmI64(i) => Printf.sprintf("%sN", i)
    | PConstWasmF32(f) => Printf.sprintf("%sw", f)
    | PConstWasmF64(f) => Printf.sprintf("%sW", f)
    | PConstBool(true) => "true"
    | PConstBool(false) => "false"
    | PConstVoid => "void"
    | PConstString(s) => Printf.sprintf("\"%s\"", s)
    };
  Doc.text(print_c);
}
and print_ident = (ident: Identifier.t) => {
  switch (ident) {
  | IdentName(name) => Doc.text(name)
  | IdentExternal(externalIdent, second) =>
    Doc.concat([
      print_ident(externalIdent),
      Doc.text("."),
      Doc.text(second),
    ])
  };
}

and print_imported_ident = (ident: Identifier.t) => {
  switch (ident) {
  | IdentName(name) =>
    if (infixop(name)) {
      Doc.concat([Doc.lparen, Doc.text(name), Doc.rparen]);
    } else {
      Doc.text(name);
    }

  | IdentExternal(externalIdent, second) =>
    Doc.concat([
      print_ident(externalIdent),
      Doc.text("."),
      Doc.text(second),
    ])
  };
}
and print_record =
    (
      fields:
        list(
          (
            Grain_parsing__Location.loc(Grain_parsing__Identifier.t),
            Grain_parsing__Parsetree.expression,
          ),
        ),
    ) =>
  Doc.concat([
    Doc.lbrace,
    Doc.indent(
      Doc.concat([
        Doc.line,
        Doc.join(
          Doc.concat([Doc.comma, Doc.line]),
          List.map(
            (
              field: (
                Grain_parsing__Location.loc(Grain_parsing__Identifier.t),
                Grain_parsing__Parsetree.expression,
              ),
            ) => {
              let (locidentifier, expr) = field;
              let ident = locidentifier.txt;

              let printed_ident = print_ident(ident);
              let printed_expr =
                print_expression(~expr, ~parentIsArrow=false);

              let pun =
                switch (printed_ident) {
                | Text(i) =>
                  switch ((printed_expr: Doc.t)) {
                  | Text(e) => i == e
                  | _ => false
                  }
                | _ => false
                };

              if (!pun) {
                Doc.group(
                  Doc.concat([printed_ident, Doc.text(": "), printed_expr]),
                );
              } else {
                Doc.group(printed_ident);
              };
            },
            fields,
          ),
        ),
        Doc.ifBreaks(Doc.text(","), Doc.nil),
      ]),
    ),
    Doc.line,
    Doc.rbrace,
  ])

and print_type = (p: Grain_parsing__Parsetree.parsed_type) => {
  switch (p.ptyp_desc) {
  | PTyAny => Doc.text("AnyType")
  | PTyVar(name) => Doc.text(name)
  | PTyArrow(types, parsed_type) =>
    Doc.concat([
      Doc.group(
        if (List.length(types) == 1) {
          print_type(List.hd(types));
        } else {
          Doc.concat([
            Doc.lparen,
            Doc.join(
              Doc.concat([Doc.comma, Doc.space]),
              List.map(t => print_type(t), types),
            ),
            Doc.rparen,
          ]);
        },
      ),
      Doc.text(" -> "),
      print_type(parsed_type),
    ])

  | PTyTuple(parsed_types) =>
    addParens(
      Doc.join(Doc.comma, List.map(t => print_type(t), parsed_types)),
    )

  | PTyConstr(locidentifier, parsedtypes) =>
    let ident = locidentifier.txt;
    Doc.concat([
      print_ident(ident),
      Doc.concat(
        List.map(
          typ => {
            Doc.concat([Doc.text("<"), print_type(typ), Doc.text(">")])
          },
          parsedtypes,
        ),
      ),
    ]);
  | PTyPoly(locationstrings, parsed_type) => Doc.text("PTyPoly")
  };
}
and print_application =
    (func: Parsetree.expression, expressions: list(Parsetree.expression)) => {
  // let functionNameDoc = print_expression(~expr=func, ~parentIsArrow=false);
  // let functionName = Doc.toString(~width=100, functionNameDoc);

  let functionName = getFunctionName(func);

  if (infixop(functionName)) {
    let first = List.hd(expressions);
    let second = List.hd(List.tl(expressions)); // assumes an infix only has two expressions

    let firstBrackets =
      switch (first.pexp_desc) {
      | PExpApp(_) =>
        Doc.concat([
          Doc.lparen,
          print_expression(~expr=first, ~parentIsArrow=false),
          Doc.rparen,
        ])
      | _ => print_expression(~expr=first, ~parentIsArrow=false)
      };

    let secondBrackets =
      switch (second.pexp_desc) {
      | PExpApp(_) =>
        Doc.concat([
          Doc.lparen,
          print_expression(~expr=second, ~parentIsArrow=false),
          Doc.rparen,
        ])
      | _ => print_expression(~expr=second, ~parentIsArrow=false)
      };
    Doc.group(
      Doc.concat([
        firstBrackets,
        Doc.space,
        print_expression(~expr=func, ~parentIsArrow=false),
        Doc.line,
        secondBrackets,
      ]),
    );
  } else {
    //   let funcName = print_expression(~expr=func, ~parentIsArrow=false);

    let funcName = getFunctionName(func);

    // let funcNameAsString = Doc.toString(~width=20, funcName);
    if (funcName == "[...]") {
      resugar_list(expressions);
    } else if (funcName == "throw") {
      Doc.concat([
        print_expression(~expr=func, ~parentIsArrow=false),
        Doc.space,
        print_expression(~expr=List.hd(expressions), ~parentIsArrow=false),
      ]);
    } else if (funcName == "!") {
      Doc.concat([
        print_expression(~expr=func, ~parentIsArrow=false),
        print_expression(~expr=List.hd(expressions), ~parentIsArrow=false),
      ]);
    } else {
      Doc.concat([
        print_expression(~expr=func, ~parentIsArrow=false),
        Doc.lparen,
        Doc.join(
          Doc.concat([Doc.text(","), Doc.line]),
          List.map(
            e => print_expression(~expr=e, ~parentIsArrow=false),
            expressions,
          ),
        ),
        Doc.rparen,
      ]);
    };
  };
}

and getFunctionName = (expr: Parsetree.expression) => {
  switch (expr.pexp_desc) {
  | PExpConstant(x) =>
    print_endline("PExpConstant");
    Doc.toString(~width=1000, print_constant(x));
  | PExpId({txt: id}) =>
    print_endline("PExpId");
    Doc.toString(~width=1000, print_ident(id));
  | _ => ""
  };
}

and print_expression = (~expr: Parsetree.expression, ~parentIsArrow: bool) => {
  let expression_doc =
    switch (expr.pexp_desc) {
    | PExpConstant(x) =>
      print_loc("PExpConstant", expr.pexp_loc);

      print_constant(x);
    | PExpId({txt: id}) =>
      print_loc("PExpId", expr.pexp_loc);

      print_ident(id);
    | PExpLet(rec_flag, mut_flag, vbs) =>
      print_loc("PExpLet", expr.pexp_loc);

      value_bind_print(Asttypes.Nonexported, rec_flag, mut_flag, vbs);
    | PExpTuple(expressions) =>
      addParens(
        Doc.join(
          Doc.comma,
          List.map(
            e => print_expression(~expr=e, ~parentIsArrow=false),
            expressions,
          ),
        ),
      )

    | PExpArray(expressions) =>
      Doc.group(
        Doc.concat([
          Doc.lbracket,
          Doc.join(
            Doc.comma,
            List.map(
              e => print_expression(~expr=e, ~parentIsArrow=false),
              expressions,
            ),
          ),
          Doc.rbracket,
        ]),
      )
    | PExpArrayGet(expression1, expression2) =>
      Doc.concat([
        print_expression(~expr=expression1, ~parentIsArrow=false),
        Doc.lbracket,
        print_expression(~expr=expression2, ~parentIsArrow=false),
        Doc.rbracket,
      ])
    | PExpArraySet(expression1, expression2, expression3) =>
      Doc.concat([
        print_expression(~expr=expression1, ~parentIsArrow=false),
        Doc.lbracket,
        print_expression(~expr=expression2, ~parentIsArrow=false),
        Doc.rbracket,
        Doc.text(" = "),
        print_expression(~expr=expression3, ~parentIsArrow=false),
      ])

    | PExpRecord(record) => print_record(record)
    | PExpRecordGet(expression, {txt, _}) =>
      Doc.concat([
        print_expression(~expr=expression, ~parentIsArrow=false),
        Doc.dot,
        print_ident(txt),
      ])
    | PExpRecordSet(expression, {txt, _}, expression2) =>
      Doc.concat([
        print_expression(~expr=expression, ~parentIsArrow=false),
        Doc.dot,
        print_ident(txt),
        Doc.text(" = "),
        print_expression(~expr=expression2, ~parentIsArrow=false),
      ])
    | PExpMatch(expression, match_branches) =>
      print_loc("PExpMatch", expr.pexp_loc);

      Doc.concat([
        Doc.group(
          Doc.concat([
            Doc.text("match "),
            Doc.lparen,
            Doc.indent(
              Doc.concat([
                Doc.softLine,
                print_expression(~expr=expression, ~parentIsArrow=false),
              ]),
            ),
            Doc.softLine,
            Doc.rparen,
          ]),
        ),
        Doc.space,
        Doc.lbrace,
        Doc.indent(
          Doc.concat([
            Doc.hardLine,
            Doc.join(
              Doc.concat([Doc.comma, Doc.hardLine]),
              List.map(
                (branch: Parsetree.match_branch) =>
                  Doc.concat([
                    print_pattern(branch.pmb_pat),
                    switch (branch.pmb_guard) {
                    | None => Doc.nil
                    | Some(guard) =>
                      Doc.concat([
                        Doc.text(" when "),
                        print_expression(~expr=guard, ~parentIsArrow=false),
                      ])
                    },
                    Doc.text(" =>"),
                    Doc.ifBreaks(Doc.space, Doc.nil),
                    print_expression(
                      ~expr=branch.pmb_body,
                      ~parentIsArrow=true,
                    ),
                  ]),
                match_branches,
              ),
            ),
            Doc.ifBreaks(Doc.text(","), Doc.nil),
          ]),
        ),
        Doc.hardLine,
        Doc.rbrace,
      ]);

    | PExpPrim1(prim1, expression) =>
      print_endline("PExpPrim1");
      Doc.text("PExpPrim1");
    | PExpPrim2(prim2, expression, expression1) =>
      print_endline("PExpPrim2");
      Doc.text("PExpPrim2");
    | PExpPrimN(primn, expressions) =>
      print_endline("PExpPrimN");
      Doc.text("PExpPrimN");
    | PExpIf(condition, trueExpr, falseExpr) =>
      let trueClause =
        Doc.group(print_expression(~expr=trueExpr, ~parentIsArrow=false));

      let falseClause =
        switch (falseExpr.pexp_desc) {
        | PExpBlock(expressions) =>
          if (List.length(expressions) > 0) {
            Doc.concat([
              Doc.line,
              Doc.text("else "),
              Doc.group(
                print_expression(~expr=falseExpr, ~parentIsArrow=false),
              ),
            ]);
          } else {
            Doc.nil;
          }
        | _ =>
          Doc.concat([
            Doc.line,
            Doc.text("else "),
            Doc.group(
              print_expression(~expr=falseExpr, ~parentIsArrow=false),
            ),
          ])
        };

      if (parentIsArrow) {
        Doc.group(
          Doc.indent(
            Doc.concat([
              Doc.line,
              Doc.text("if "),
              Doc.group(
                Doc.concat([
                  Doc.lparen,
                  print_expression(~expr=condition, ~parentIsArrow=false),
                  Doc.rparen,
                  Doc.space,
                ]),
              ),
              trueClause,
              falseClause,
            ]),
          ),
        );
      } else {
        Doc.group(
          Doc.concat([
            Doc.line,
            Doc.text("if "),
            Doc.group(
              Doc.concat([
                Doc.lparen,
                print_expression(~expr=condition, ~parentIsArrow=false),
                Doc.rparen,
                Doc.space,
              ]),
            ),
            Doc.space,
            trueClause,
            falseClause,
          ]),
        );
      };
    | PExpWhile(expression, expression1) =>
      Doc.concat([
        Doc.text("while "),
        addParens(print_expression(~expr=expression, ~parentIsArrow=false)),
        Doc.space,
        Doc.group(print_expression(~expr=expression1, ~parentIsArrow=false)),
      ])

    | PExpFor(optexpression1, optexpression2, optexpression3, expression4) =>
      Doc.concat([
        Doc.text("for "),
        addParens(
          Doc.concat([
            switch (optexpression1) {
            | Some(expr) => print_expression(~expr, ~parentIsArrow=false)
            | None => Doc.nil
            },
            Doc.text(";"),
            switch (optexpression2) {
            | Some(expr) =>
              Doc.concat([
                Doc.space,
                print_expression(~expr, ~parentIsArrow=false),
              ])
            | None => Doc.nil
            },
            Doc.text(";"),
            switch (optexpression3) {
            | Some(expr) =>
              Doc.concat([
                Doc.space,
                print_expression(~expr, ~parentIsArrow=false),
              ])
            | None => Doc.nil
            },
          ]),
        ),
        Doc.space,
        print_expression(~expr=expression4, ~parentIsArrow=false),
      ])
    | PExpContinue =>
      Doc.group(Doc.concat([Doc.text("continue"), Doc.hardLine]))
    | PExpBreak => Doc.group(Doc.concat([Doc.text("break"), Doc.hardLine]))
    | PExpConstraint(expression, parsed_type) =>
      Doc.group(
        Doc.concat([
          print_expression(~expr=expression, ~parentIsArrow=false),
          Doc.text(": "),
          print_type(parsed_type),
        ]),
      )
    | PExpLambda(patterns, expression) =>
      print_loc("PExpLambda", expr.pexp_loc);

      Doc.group(
        Doc.concat([
          if (List.length(patterns) == 0) {
            Doc.text("()");
          } else if (List.length(patterns) > 1) {
            addParens(
              Doc.concat([
                Doc.join(
                  Doc.concat([Doc.text(","), Doc.line]),
                  List.map(p => print_pattern(p), patterns),
                ),
              ]),
            );
          } else {
            let pat = List.hd(patterns);

            switch (pat.ppat_desc) {
            | PPatConstraint(_) =>
              Doc.concat([Doc.lparen, print_pattern(pat), Doc.rparen])
            | _ => print_pattern(pat)
            };
          },
          Doc.space,
          Doc.text("=>"),
          Doc.space,
          print_expression(~expr=expression, ~parentIsArrow=false),
        ]),
      );

    | PExpApp(func, expressions) =>
      print_loc("PExpApp", expr.pexp_loc);

      print_application(func, expressions);
    | PExpBlock(expressions) =>
      print_loc("PExpBlock", expr.pexp_loc);

      // look for leading comments

      let leadingComments =
        if (List.length(expressions) > 0) {
          print_endline("looking for leading comments in a block");

          print_loc("block starts", makeStartLoc(expr.pexp_loc));
          print_loc("stmt starts", List.hd(expressions).pexp_loc);
          Doc.concat(
            List.map(
              c => commentToDoc(c),
              get_comments_between(
                makeStartLoc(expr.pexp_loc),
                List.hd(expressions).pexp_loc,
                allComments^,
              ),
            ),
          );
        } else {
          Doc.nil;
        };

      let block =
        Doc.join(
          Doc.hardLine,
          List.map(
            (e: Parsetree.expression) => {
              let printed_expression =
                print_expression(~expr=e, ~parentIsArrow=false);
              let locAfter = Walktree.get_location_after(e.pexp_loc);
              let trailingComment =
                switch (locAfter) {
                | None =>
                  print_endline("No loc after expression");
                  Doc.nil;
                | Some(loc) =>
                  print_endline("We have an item after expression");

                  let comments =
                    get_comments_between(e.pexp_loc, loc, allComments^);

                  if (List.length(comments) > 0) {
                    print_endline(
                      "A block expression  is followed by comments",
                    );

                    List.iter(c => print_comment(c), comments);

                    let commentDocs =
                      Doc.join(
                        Doc.space,
                        List.map(c => commentToDoc(c), comments),
                      );

                    List.iter(
                      c => allComments := removeComment(c, allComments^),
                      comments,
                    );
                    Doc.concat([Doc.space, commentDocs]);
                  } else {
                    Doc.nil;
                  };
                };

              Doc.group(Doc.concat([printed_expression, trailingComment]));
            },
            expressions,
          ),
        );

      Doc.breakableGroup(
        ~forceBreak=true,
        Doc.concat([
          Doc.lbrace,
          Doc.indent(Doc.concat([Doc.line, leadingComments, block])),
          Doc.line,
          Doc.rbrace,
        ]),
      );

    | PExpBoxAssign(expression, expression1) =>
      Doc.concat([
        print_expression(~expr=expression, ~parentIsArrow=false),
        Doc.text(" := "),
        print_expression(~expr=expression1, ~parentIsArrow=false),
      ])
    | PExpAssign(expression, expression1) =>
      print_loc("PExpAssign", expr.pexp_loc);

      switch (expression1.pexp_desc) {
      | PExpApp(func, expressions) =>
        let functionName = getFunctionName(func);

        let trimmedOperator = String.trim(functionName);

        let left = print_expression(~expr=expression, ~parentIsArrow=false);

        let first =
          print_expression(~expr=List.hd(expressions), ~parentIsArrow=false);

        if (left == first) {
          // +=, -=, *=, /=, and %=
          switch (trimmedOperator) {
          | "+"
          | "-"
          | "*"
          | "/"
          | "%" =>
            let sugaredOp = Doc.text(" " ++ trimmedOperator ++ "= ");

            Doc.concat([
              print_expression(~expr=expression, ~parentIsArrow=false),
              sugaredOp,
              print_expression(
                ~expr=List.hd(List.tl(expressions)),
                ~parentIsArrow=false,
              ),
            ]);
          | _ =>
            Doc.concat([
              print_expression(~expr=expression, ~parentIsArrow=false),
              Doc.text(" = "),
              print_expression(~expr=expression1, ~parentIsArrow=false),
            ])
          };
        } else {
          Doc.concat([
            print_expression(~expr=expression, ~parentIsArrow=false),
            Doc.text(" = "),
            print_expression(~expr=expression1, ~parentIsArrow=false),
          ]);
        };

      | _ =>
        Doc.concat([
          print_expression(~expr=expression, ~parentIsArrow=false),
          Doc.text(" = "),
          print_expression(~expr=expression1, ~parentIsArrow=false),
        ])
      };

    | /** Used for modules without body expressions */ PExpNull => Doc.nil
    };

  let commentDocs = Doc.nil;
  let commentsBeforeDocs = Doc.nil;

  Doc.concat([commentsBeforeDocs, expression_doc, commentDocs]);
}
and value_bind_print =
    (
      export_flag: Asttypes.export_flag,
      rec_flag,
      mut_flag,
      vbs: list(Parsetree.value_binding),
    ) => {
  let exported =
    switch (export_flag) {
    | Nonexported => Doc.nil
    | Exported => Doc.text("export ")
    };
  let recursive =
    switch (rec_flag) {
    | Nonrecursive => Doc.nil
    | Recursive => Doc.text("rec ")
    };
  let mutble =
    switch (mut_flag) {
    | Immutable => Doc.nil
    | Mutable => Doc.text("mut ")
    };
  let vb = List.hd(vbs); // FIX ME - multiple bindings ??
  print_loc("value binding:", vb.pvb_loc);
  Doc.group(
    Doc.concat([
      exported,
      Doc.text("let "),
      recursive,
      mutble,
      print_pattern(vb.pvb_pat),
      Doc.text(" = "),
      print_expression(~expr=vb.pvb_expr, ~parentIsArrow=false),
    ]),
  );
};
let rec print_data = (d: Grain_parsing__Parsetree.data_declaration) => {
  let nameloc = d.pdata_name;
  switch (d.pdata_kind) {
  | PDataVariant(constr_declarations) =>
    Doc.group(
      Doc.concat([
        Doc.text("enum"),
        Doc.space,
        Doc.text(nameloc.txt),
        if (List.length(d.pdata_params) > 0) {
          Doc.concat([
            Doc.text("<"),
            Doc.join(
              Doc.text(", "),
              List.map(t => print_type(t), d.pdata_params),
            ),
            Doc.text(">"),
            Doc.space,
          ]);
        } else {
          Doc.space;
        },
        Doc.lbrace,
        Doc.indent(
          Doc.concat([
            Doc.line,
            Doc.join(
              Doc.concat([Doc.comma, Doc.line]),
              List.map(
                (d: Grain_parsing__Parsetree.constructor_declaration) =>
                  Doc.group(
                    Doc.concat([
                      Doc.text(d.pcd_name.txt),
                      switch (d.pcd_args) {
                      | PConstrTuple(parsed_types) =>
                        if (List.length(parsed_types) > 0) {
                          Doc.concat([
                            Doc.text("("),
                            Doc.join(
                              Doc.concat([Doc.comma, Doc.line]),
                              List.map(t => print_type(t), parsed_types),
                            ),
                            Doc.text(")"),
                          ]);
                        } else {
                          Doc.nil;
                        }
                      | PConstrSingleton => Doc.nil
                      },
                    ]),
                  ),
                constr_declarations,
              ),
            ),
          ]),
        ),
        Doc.ifBreaks(Doc.text(","), Doc.nil),
        Doc.line,
        Doc.rbrace,
      ]),
    )

  | PDataRecord(label_declarations) =>
    Doc.group(
      Doc.concat([
        Doc.text("record"),
        Doc.space,
        Doc.text(nameloc.txt),
        Doc.space,
        if (List.length(d.pdata_params) > 0) {
          Doc.concat([
            Doc.text("<"),
            Doc.join(
              Doc.text(","),
              List.map(t => print_type(t), d.pdata_params),
            ),
            Doc.text(">"),
            Doc.space,
          ]);
        } else {
          Doc.nil;
        },
        Doc.concat([
          Doc.lbrace,
          Doc.indent(
            Doc.concat([
              Doc.line,
              Doc.join(
                Doc.concat([Doc.comma, Doc.line]),
                List.map(
                  (decl: Grain_parsing__Parsetree.label_declaration) => {
                    let isMutable =
                      switch (decl.pld_mutable) {
                      | Mutable => Doc.text("mut ")
                      | Immutable => Doc.nil
                      };
                    Doc.group(
                      Doc.concat([
                        isMutable,
                        print_ident(decl.pld_name.txt),
                        Doc.text(":"),
                        Doc.space,
                        print_type(decl.pld_type),
                      ]),
                    );
                  },
                  label_declarations,
                ),
              ),
            ]),
          ),
          Doc.line,
          Doc.rbrace,
        ]),
      ]),
    )
  };
};
let data_print =
    (
      datas:
        list(
          (
            Grain_parsing__Parsetree.export_flag,
            Grain_parsing__Parsetree.data_declaration,
          ),
        ),
    ) => {
  Doc.join(
    Doc.text(","),
    List.map(
      data => {
        let (expt, decl) = data;
        Doc.concat([
          switch ((expt: Asttypes.export_flag)) {
          | Nonexported => Doc.nil
          | Exported => Doc.text("export ")
          },
          print_data(decl),
        ]);
      },
      datas,
    ),
  );
};
let import_print = (imp: Parsetree.import_declaration) => {
  let vals =
    List.map(
      (v: Parsetree.import_value) => {
        switch (v) {
        | PImportModule(identloc) => print_ident(identloc.txt)
        | PImportAllExcept(identlocs) =>
          Doc.concat([
            Doc.text("*"),
            if (List.length(identlocs) > 0) {
              Doc.concat([
                Doc.text(" except "),
                addBraces(
                  Doc.join(
                    Doc.comma,
                    List.map(
                      (identloc: Location.loc(Grain_parsing__Identifier.t)) =>
                        print_ident(identloc.txt),
                      identlocs,
                    ),
                  ),
                ),
              ]);
            } else {
              Doc.nil;
            },
          ])
        | PImportValues(identlocsopts) =>
          Doc.concat([
            Doc.lbrace,
            Doc.indent(
              Doc.concat([
                Doc.line,
                if (List.length(identlocsopts) > 0) {
                  Doc.join(
                    Doc.concat([Doc.comma, Doc.line]),
                    List.map(
                      (
                        identlocopt: (
                          Grain_parsing.Parsetree.loc(
                            Grain_parsing.Identifier.t,
                          ),
                          option(
                            Grain_parsing.Parsetree.loc(
                              Grain_parsing.Identifier.t,
                            ),
                          ),
                        ),
                      ) => {
                        let (loc, optloc) = identlocopt;
                        switch (optloc) {
                        | None => print_imported_ident(loc.txt)
                        | Some(alias) =>
                          Doc.concat([
                            print_imported_ident(loc.txt),
                            Doc.text(" as "),
                            print_imported_ident(alias.txt),
                          ])
                        };
                      },
                      identlocsopts,
                    ),
                  );
                } else {
                  Doc.nil;
                },
              ]),
            ),
            Doc.line,
            Doc.rbrace,
          ])
        }
      },
      imp.pimp_val,
    );

  let path = imp.pimp_path.txt;

  Doc.group(
    Doc.concat([
      Doc.text("import "),
      Doc.join(Doc.concat([Doc.comma, Doc.space]), vals),
      Doc.text(" from "),
      Doc.doubleQuote,
      Doc.text(path),
      Doc.doubleQuote,
    ]),
  );
};

let print_export_desc = (desc: Parsetree.export_declaration_desc) => {
  let ident = desc.pex_name.txt;

  let fixedIdent =
    if (infixop(ident)) {
      Doc.concat([Doc.lparen, Doc.text(ident), Doc.rparen]);
    } else {
      Doc.text(ident);
    };
  Doc.concat([
    fixedIdent,
    switch (desc.pex_alias) {
    | Some(alias) => Doc.concat([Doc.text(" as "), Doc.text(alias.txt)])
    | None => Doc.nil
    },
  ]);
};

let print_export_declaration = (decl: Parsetree.export_declaration) => {
  switch (decl) {
  | ExportData(data) => print_export_desc(data)
  | ExportValue(value) => print_export_desc(value)
  };
};

let print_foreign_value_description = (vd: Parsetree.value_description) => {
  let ident = vd.pval_name.txt;

  let fixedIdent =
    if (infixop(ident) || ident == "!") {
      Doc.concat([Doc.lparen, Doc.text(ident), Doc.rparen]);
    } else {
      Doc.text(ident);
    };

  Doc.concat([
    fixedIdent,
    Doc.text(" : "),
    print_type(vd.pval_type),
    Doc.text(" from "),
    Doc.text("\""),
    Doc.text(vd.pval_mod.txt),
    Doc.text("\""),
  ]);
};

let print_primitive_value_description = (vd: Parsetree.value_description) => {
  let ident = vd.pval_name.txt;

  let fixedIdent =
    if (infixop(ident) || ident == "!") {
      Doc.concat([Doc.lparen, Doc.text(ident), Doc.rparen]);
    } else {
      Doc.text(ident);
    };

  Doc.concat([
    fixedIdent,
    Doc.text(" : "),
    print_type(vd.pval_type),
    Doc.text(" = "),
    Doc.text("\""),
    //   Doc.text(vd.pval_mod.txt),
    Doc.join(Doc.text(","), List.map(p => Doc.text(p), vd.pval_prim)),
    Doc.text("\""),
  ]);
};

let reformat_ast = (parsed_program: Parsetree.parsed_program) => {
  //  let walked_tree = Comments.walk_tree_add_comments(parsed_program);

  let _ = Walktree.walktree(parsed_program.statements);

  let toplevel_print = (data: Parsetree.toplevel_stmt) => {
    let attributes = data.ptop_attributes;

    print_loc("top level:", data.ptop_loc);

    let commentsbefore = []; //get_comments_before(data.ptop_loc, allComments^);

    let commentsBeforeDocs =
      if (List.length(commentsbefore) > 0) {
        print_endline("Following block has comments before:");
        List.iter(
          c => allComments := removeComment(c, allComments^),
          commentsbefore,
        );
        Doc.concat(List.map(c => commentToDoc(c), commentsbefore));
      } else {
        Doc.nil;
      };

    let withoutComments =
      switch (data.ptop_desc) {
      | PTopImport(import_declaration) => import_print(import_declaration)
      | PTopForeign(export_flag, value_description) =>
        let export =
          switch (export_flag) {
          | Nonexported => Doc.text("import ")
          | Exported => Doc.text("export ")
          };
        Doc.concat([
          export,
          Doc.text("foreign "),
          print_foreign_value_description(value_description),
        ]);
      | PTopPrimitive(export_flag, value_description) =>
        let export =
          switch (export_flag) {
          | Nonexported => Doc.nil
          | Exported => Doc.text("export ")
          };
        Doc.concat([
          export,
          Doc.text("primitive "),
          print_primitive_value_description(value_description),
        ]);
      | PTopData(data_declarations) => data_print(data_declarations)
      | PTopLet(export_flag, rec_flag, mut_flag, value_bindings) =>
        value_bind_print(export_flag, rec_flag, mut_flag, value_bindings)
      | PTopExpr(expression) =>
        print_expression(~expr=expression, ~parentIsArrow=false)
      | PTopException(export_flag, type_exception) =>
        let export =
          switch (export_flag) {
          | Nonexported => Doc.nil
          | Exported => Doc.text("export ")
          };
        let cstr = type_exception.ptyexn_constructor;

        let kind =
          switch (cstr.pext_kind) {
          | PExtDecl(sargs) =>
            switch (sargs) {
            | PConstrSingleton => Doc.nil
            | PConstrTuple(parsed_types) =>
              if (List.length(parsed_types) > 0) {
                Doc.concat([
                  Doc.text("("),
                  Doc.join(
                    Doc.comma,
                    List.map(t => print_type(t), parsed_types),
                  ),
                  Doc.text(")"),
                ]);
              } else {
                Doc.nil;
              }
            }

          | PExtRebind(lid) => print_ident(lid.txt)
          };

        Doc.concat([
          export,
          Doc.text("exception "),
          Doc.text(cstr.pext_name.txt),
          kind,
        ]);
      | PTopExport(export_declarations) =>
        Doc.group(
          Doc.concat([
            Doc.text("export "),
            Doc.join(
              Doc.concat([Doc.comma, Doc.line]),
              List.map(
                e => print_export_declaration(e),
                export_declarations,
              ),
            ),
          ]),
        )
      | PTopExportAll(export_excepts) =>
        Doc.concat([
          Doc.text("export * "),
          if (List.length(export_excepts) > 0) {
            Doc.concat([
              Doc.text("except "),
              Doc.join(
                Doc.comma,
                List.map(
                  (excpt: Parsetree.export_except) =>
                    switch (excpt) {
                    | ExportExceptData(data) => Doc.text(data.txt)
                    | ExportExceptValue(value) => Doc.text(value.txt)
                    },
                  export_excepts,
                ),
              ),
            ]);
          } else {
            Doc.nil;
          },
        ])
      };

    let attributeText =
      if (List.length(attributes) > 0) {
        Doc.concat([
          Doc.join(
            Doc.space,
            List.map(
              (a: Location.loc(string)) =>
                Doc.concat([Doc.text("@"), Doc.text(a.txt)]),
              attributes,
            ),
          ),
          Doc.hardLine,
        ]);
      } else {
        Doc.nil;
      };

    // let trailingComment =
    //   find_comments_in_range(stmtstartline, stmtendline, comments);

    let trailingCommentDocs = [];
    // List.map(
    //   c => Doc.concat([Doc.space, commentToDoc(c)]),
    //   trailingComment,
    // );

    Doc.concat([
      commentsBeforeDocs,
      attributeText,
      withoutComments,
      Doc.join(Doc.nil, trailingCommentDocs),
    ]);
  };

  let previousStatement: ref(option(Grain_parsing.Parsetree.toplevel_stmt)) =
    ref(None);

  allComments := parsed_program.comments;

  let pos: Stdlib__lexing.position = {
    pos_fname: "",
    pos_lnum: 0,
    pos_cnum: 0,
    pos_bol: 0,
  };

  let startLoc: Grain_parsing.Location.t = {
    loc_start: pos,
    loc_end: pos,
    loc_ghost: false,
  };

  let prevLoc: ref(Grain_parsing.Location.t) = ref(startLoc);

  let printedDoc =
    Doc.join(
      Doc.hardLine,
      List.map(
        (stmt: Grain_parsing.Parsetree.toplevel_stmt) => {
          // let's see if we had any comments above us
          // or in us

          let commentsAbove =
            get_comments_between(prevLoc^, stmt.ptop_loc, allComments^);

          let commentsAboveDocs =
            if (List.length(commentsAbove) > 0) {
              Doc.concat(
                List.map(
                  (c: Parsetree.comment) => commentToDoc(c),
                  commentsAbove,
                ),
              );
            } else {
              Doc.nil;
            };

          // we may also have blank lines, we want to reduce them to one

          // let lastCommentLine =
          //   if (List.length(commentsAbove) > 0) {
          //     Some(List.hd(List.rev(commentsAbove)));
          //   } else {
          //     None;
          //   };

          // let lastLine =
          //   switch (previousStatement^) {
          //   | None =>
          //     switch (lastCommentLine) {
          //     | None => 1
          //     | Some(cmt) => getCommentEndLine(cmt)
          //     }
          //   | Some(s) =>
          //     switch (lastCommentLine) {
          //     | None =>
          //       let (_, prevline, _, _) =
          //         get_raw_pos_info(s.ptop_loc.loc_end);

          //       prevline;

          //     | Some(cmt) => getCommentEndLine(cmt)
          //     }
          //   };

          // let blankLineAbove =
          //   if (stmtstartline - lastLine > 1) {
          //     Doc.hardLine;
          //   } else {
          //     Doc.nil;
          //   };

          let blankLineAbove = Doc.nil;

          previousStatement := Some(stmt);
          Doc.concat([
            commentsAboveDocs,
            blankLineAbove,
            Doc.group(toplevel_print(stmt)),
          ]);
        },
        parsed_program.statements,
      ),
    );

  // print any comments below

  let trailingComments = Doc.nil;
  // if (List.length(allComments^) > 0) {
  //   Doc.concat([
  //     Doc.hardLine,
  //     Doc.concat(List.map(c => commentToDoc(c), allComments^)),
  //   ]);
  // } else {
  //   Doc.nil;
  // };

  let finalDoc = Doc.concat([printedDoc, trailingComments]);

  // Use this to check the generated output
  //Doc.debug(finalDoc);
  //

  Doc.toString(~width=80, finalDoc) |> print_endline;
  //use this to see the AST in JSON
  // print_endline(
  //   Yojson.Basic.pretty_to_string(
  //     Yojson.Safe.to_basic(
  //       Grain_parsing__Parsetree.parsed_program_to_yojson(parsed_program),
  //     ),
  //   ),
  // );
};

let x = 3 + /* comment  */ 2;