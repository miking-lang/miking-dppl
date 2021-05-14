-- Miking is licensed under the MIT license.
-- Copyright (C) David Broman. See file LICENSE.txt
--
-- A simple and extensible library for command line
-- argument parsing.

include "string.mc"

type ArgResult = {
  strings : [String],
  options : a
}

type ParseConfig = [([String], [String], String, a -> String -> a)]


-- argHelpOptions --



-- Creates a new string with new lines, and breaks between words.
-- Assumes that the string is currently at indent position, and
-- also adds indent number of spaces before the next line.
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

-- argParse --

type Options_argParse = {
  args : [String]
}


let argParse_defaults = {
  args = tail argv
}


let argParse_general : Options_argParse -> a -> ParseConfig -> Option ArgResult =
  lam options. lam argParseDefaults. lam argParseConfig.
  Some {strings = ["Test"], options = argParseDefaults}

let argParse = argParse_general argParse_defaults


-- argument value conversion --

let argInt = lam x : String.
  string2int


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
  ([("--len", "=", "<int>")],
    "A number argument followed by equality and then the integer value.",
    lam o:Options. lam s. {o with len = argInt s})
  ([("-m", " ", "<msg>"),("--message", "=", "<msg>")],
    "A string argument, with both short and long form arguments, and different separators.",
    lam o:Options. lam s. {o with message = s})
] in

()
