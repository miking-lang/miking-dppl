-- Miking is licensed under the MIT license.
-- Copyright (C) David Broman. See file LICENSE.txt
--
-- A simple and extensible library for command line
-- argument parsing.

include "string.mc"
include "seq.mc"


type ArgResult = {
  strings : [String],
  options : a
}

type ParseConfig = [([String], [String], String, a -> String -> a)]


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

-- argument value conversion --

let argInt = lam x : String.
  string2int

-- Pretty printing of argument options --


-- Utility functions that can should be moved to string.mc




-- argParse --

type Options_argParse = {
  args : [String]
}


let argParse_defaults = {
  args = tail argv
}


let argParse_general : Options_argParse -> a -> ParseConfig -> Option ArgResult =
  lam options. lam argParseDefaults. lam argParseConfig.
  recursive
    let matchOption = lam str. lam confLst.
      match confLst with [(opLst, _, f)] ++ rest then
        match find (lam x. match x with (s, _, _)
                           then isPrefix eqChar s str else never) opLst
        with Some (s, sep, _)
        then Some (s, sep, f)
        else matchOption str rest
      else None ()
  in
  match matchOption "--maessage=2" argParseConfig with Some _ then
    print "TRUE\n"
  else
    print "FALSE\n";
  Some {strings = ["file.mc"], options = argParseDefaults}



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
    lam o:Options. lam. {o with foo = true}),
  ([("--len", "=", "<value>")],
    "A number argument followed by equality and then the integer value.",
    lam o:Options. lam s. {o with len = argInt s}),
  ([("-m", " ", "<msg>"),("--message", "=", "<msg>")],
    "A string argument, with both short and long form arguments, and different separators.",
    lam o:Options. lam s. {o with message = s})
] in

let testOptions = {argParse_defaults with args = ["file.mc", "--foo"]} in
let argParseCustom = argParse_general testOptions in
let res : ArgResult = match argParseCustom default config with Some r then r else error "Incorrect type" in
utest res.strings with ["file.mc"] using eqSeq eqString in

()
