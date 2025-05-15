/**
 * @file Tift grammar for tree-sitter
 * @author Asher Frost
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "tift",

  rules: {
    // TODO: add the actual grammar rules
    source_file: ($) => repeat($.theory),
    theory: ($) => seq(repeat1($.sentence), ";;;"),
    sentence: ($) => seq($.term, ";"),
    term: ($) => choice($.lamed, $.cons, $.atomic),
    cons: ($) => seq("(", repeat1($.term), ")"),
    lamed: ($) => seq("[", repeat1($.term), "]", "{", repeat1($.term), "}"),
    atomic: ($) => choice($.wildcard, $.identifier),
    wildcard: ($) => "_",
    identifier: ($) => /[^_\(\)\[\]\{\}\;]+/,
  },
});
