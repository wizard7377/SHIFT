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
    source_file: $ => "hello"
  }
});
