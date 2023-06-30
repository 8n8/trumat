module Parser_ (Parser_(..), ModuleDeclarationWithTitle(..), Imports(..)) where


data Parser_
    = Start
    | ModuleDeclaration ParseModuleDeclarationWithTitle
    | SpaceAfterModuleDeclaration Space ModuleDeclarationWithTitle
    | Imports ParseImports ModuleDeclarationWithTitle
    | SpaceAfterImports Space ModuleDeclarationWithTitle Imports
    | TopLevelBinds 


newtype ModuleDeclarationWithTitle
    = ModuleDeclarationWithTitle Text


newtype Imports
    = Imports Text


data ParseImports
    = Init [Text]
    | ParsingImport ParseImport [Text]
    | ParsingSpaceAfterImport Space [Text]
