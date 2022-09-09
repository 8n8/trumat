1. UTF8 char literal
    u24: ID
    u8-32: char
2. string literal
    u24: ID
    u16: length
    u24: ID of first byte
3. Most values are referenced by
    u8: table ID
    u24: row ID
4. Name
    u24: ID
    u24: ID of first byte of name string
    u16: length of name string
    u32: value reference of the named thing
5. Argument
    u24: ID
    u32: value reference of thing that has the argument
    u32: value reference of the argument
6. Parameter
    u24: ID
    u32: value reference of thing that has the parameter
7. Module member
    u24: module ID
    u32: value reference of module member
8. Custom type branch
    u24: ID
    u24: parent type ID
9. Type signature member
    u24: ID
    u24: ID of thing that has the type signature
10. Case branch
    u32: value reference of switched on value
    u32: value reference of pattern value
    u32: value reference of result
11. if else
    u32: value reference of switched on value
    u32: value reference of if result
    u32: value reference of else result
12. Module export
    u32: value reference of exported thing
13. Export all of a type from a module, like X(..)
    u24: ID of type
14. Export everything from a module
    u24: module ID
15. Module member import
    u24: module ID to import into
    u24: module to import
15. Whole module import
    u24: module ID to import into
    u32: value reference of thing to import
16. return value of a let in expression
    u24: ID of let in
    u32: value reference
17. list literal
    u24: ID
    u24: ID of first item
    u16: length
18. list item
    u32: value reference of item
19. wrapper
    u24: ID
    u32: value ID of thing wrapped
