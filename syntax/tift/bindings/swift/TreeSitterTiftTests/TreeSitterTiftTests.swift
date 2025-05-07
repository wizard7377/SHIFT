import XCTest
import SwiftTreeSitter
import TreeSitterTift

final class TreeSitterTiftTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_tift())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Tift grammar")
    }
}
