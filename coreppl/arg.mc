-- Miking is licensed under the MIT license.
-- Copyright (C) David Broman. See file LICENSE.txt
--
-- A simple and extensible library for command line
-- argument parsing.

include "string.mc"
include "seq.mc"
include "char.mc"

type ArgResult = {
  strings : [String],
  options : a
}

type ParseOption = (String, String, String)
type ParseConfig = [([ParseOption], String, a -> String -> a)]

type ParseType
con ParseTypeInt : Unit -> ParseType
con ParseTypeIntMin : Int -> ParseType

type ParseResult
con ParseOK : ArgResult -> ParseResult
con ParseFailUnknownOption : String -> ParseResult
con ParseFailMissingOpArg : String -> ParseResult
con ParseFailConversion : (ParseType, String) -> ParseResult

-- argHelpOptions --



-- Creates a new string with new lines, and breaks between words.
-- Assumes that the string is currently at 'startPos', and
-- also adds 'indent' number of spaces before the next line.
let stringLineFormat = lam s. lam width. lam indent. lam startPos.
  recursive
    let next = lam s. lam acc. lam w. lam pos. lam space.
      let pos = addi (length w) pos in
      if leqi pos width then
        let pos = addi pos (length space) in
        let acc = concat acc w in
        let acc = if leqi pos width then concat acc space else acc in
          work s acc [] pos
      else
        let acc = concat acc (cons '\n' (make indent ' ')) in
        let w = concat w space in
        work s (concat acc w) [] (addi indent (length w))
    let work = lam s. lam acc. lam w. lam pos.
      match s with [c] ++ rest then
        if eqChar c ' ' then
          next rest acc w pos " "
        else
          work rest acc (snoc w c) pos
      else
        if eqi (length w) 0 then acc
        else next s acc w pos ""
  in
    work s [] [] startPos


type Options_argHelpOptions = {
  lineWidth : Int,
  indent : Int,
  spaceToText : Int
}

let argHelpOptions_defaults = {
  lineWidth = 80,
  indent = 2,
  spaceToText = 1
}

let argHelpOptions_general =
  lam options : Options_argHelp.
  lam opConfig : a.
  "test string options\n"

let argHelpOptions = argHelpOptions_general argHelpOptions_defaults


-- Utility functions that can should be moved to string.mc
-- TODO(davbr,2021-05-24): should be moved to string.mc
-- Returns true if the string is an integer
let stringIsInt = lam s.
  if eqi (length s) 0 then false else
  let s = if eqChar (get s 0) '-' then tail s else s in
    all isDigit s


-- argument value conversion --

let argToString = lam p.
    p.str

let argToInt = lam p.
  let v = string2int p.str in
  if stringIsInt p.str then string2int p.str
  else modref p.fail (Some (ParseTypeInt(), p.str)); 0

let argToIntMin = lam p. lam minVal.
  let v = argToInt p in
  if lti v minVal then
    modref p.fail (Some (ParseTypeIntMin(minVal), p.str)); v
  else
    v



-- argParse --

type Options_argParse = {
  args : [String]
}


let argParse_defaults = {
  args = tail argv
}




-- Main argument parsing function.
let argParse_general =
  lam options. lam argParseDefaults. lam argParseConfig.
  recursive
    -- Match one option
    let matchOption = lam str. lam confLst : ParseConfig.
     match confLst with [(opLst, _, f)] ++ rest then
       match find (lam x. match x with (s, _, _)
                          then isPrefix eqChar s str else never) opLst
       with Some (s, sep, _)
       then Some (s, sep, f)
       else matchOption str rest
     else None ()
    -- Handle parsing of options
    let handleOptionParsing = lam f. lam o. lam s.
      let failCode = ref (None ()) in
      let options = f {options = o, str = s, fail = failCode} in
      match deref failCode with Some (pType, str) then
        (Some (ParseFailConversion (pType, str)), options)
      else
        (None (), options)
    -- Main parsing loop
    let argMain = lam options. lam strings. lam args.
      match args with [s] ++ xs then
        match matchOption s argParseConfig with Some (op, sep, f) then
          if eqi (length sep) 0 then
            -- No value to the option
            if eqString s op then
              let parse = handleOptionParsing f options s in
              match parse with (Some ret, _) then
                ret
              else match parse with (None(), options) then
                argMain options strings xs
              else never
            else
              ParseFailUnknownOption s
          else
            -- TODO(davbr,2021-05-22): Add handling without space, e.g, "--foo=7"
            --                         and other separators than space
            if eqString s op then
              match xs with [s2] ++ xs then
                match matchOption s2 argParseConfig with Some _ then
                  ParseFailMissingOpArg s
                else
                  let parse = handleOptionParsing f options s2 in
                  match parse with (Some ret, _) then
                    ret
                  else match parse with (None(), options) then
                    argMain options strings xs
                  else never
              else
                 ParseFailMissingOpArg s
            else
              ParseFailUnknownOption s
        else
          -- Not an option, add to strings
          argMain options (snoc strings s) xs
      else
        ParseOK {strings = strings, options = options}
  in
    argMain argParseDefaults [] options.args



let argParse = argParse_general argParse_defaults



mexpr


let s1 = "This is a test that we can take a look at." in
let s2 = "This is a \n   test that we \n   can take a \n   look at." in
utest stringLineFormat s1 16 3 5 with s2 in
let s2 = "This is a test\n   that we can\n   take a look\n   at." in
utest stringLineFormat s1 14 3 0 with s2 in
let s2 = "This is a \n test that we\n can take a \n look at." in
utest stringLineFormat s1 13 1 0 with s2 in

type Options = {
  foo : Bool,
  len : Int,
  message : String
} in

let default = {
  foo = false,
  len = 7,
  message = ""
} in

let config = [
  ([("--foo", "", "")],
    "This is a boolean option. ",
    lam p. {p.options with foo = true}),
  ([("--len", "=", "<value>")],
    "A number argument followed by equality and then the integer value.",
    lam p. {p.options with len = argToIntMin p 1}),
  ([("-m", " ", "<msg>"),("--message", "=", "<msg>")],
    "A string argument, with both short and long form arguments, and different separators.",
    lam p. {p.options with message = argToString p})
] in

let testOptions = {argParse_defaults with args = ["file.mc", "--len", "12", "--foo", "-m", "mymsg", "f2"]} in
let argParseCustom = argParse_general testOptions in
let res = match argParseCustom default config with ParseOK r then r else error "Incorrect type" in
utest res.strings with ["file.mc", "f2"] using eqSeq eqString in
utest res.options.foo with true in
utest res.options.message with "mymsg" in
utest res.options.len with 12 in

let testOptions = {argParse_defaults with args = ["--len", "-2"]} in
utest (argParse_general testOptions) default config
with ParseFailConversion ((ParseTypeIntMin 1), "-2") in

let testOptions = {argParse_defaults with args = ["--messageNo", "msg"]} in
utest (argParse_general testOptions) default config
with ParseFailUnknownOption "--messageNo" in

let testOptions = {argParse_defaults with args = ["--message"]} in
utest (argParse_general testOptions) default config
with ParseFailMissingOpArg "--message" in

let testOptions = {argParse_defaults with args = ["--message", "--len", "78"]} in
utest (argParse_general testOptions) default config
with ParseFailMissingOpArg "--message" in


()

