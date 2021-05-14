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

utest 1 with 1 in

()
