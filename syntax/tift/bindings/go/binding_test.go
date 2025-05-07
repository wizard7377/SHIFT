package tree_sitter_tift_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_tift "github.com/tree-sitter/tree-sitter-tift/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_tift.Language())
	if language == nil {
		t.Errorf("Error loading Tift grammar")
	}
}
