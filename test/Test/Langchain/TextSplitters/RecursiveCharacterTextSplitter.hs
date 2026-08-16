{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.TextSplitters.RecursiveCharacterTextSplitter (tests) where

import Data.Int (Int64)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.TextSplitters.RecursiveCharacterTextSplitter

tests :: TestTree
tests =
  testGroup
    "Langchain.TextSplitters.RecursiveCharacterTextSplitter Tests"
    [ testGroup
        "RecursiveCharacterTextSplitter Python parity"
        [ testCase "splitText keeps separators at start and end" $ do
            let splitTags = [",", "."]
                query = "Apple,banana,orange and tomato."
                startOps =
                  defaultRecursiveCharacterSplitterOps
                    { chunkSize = 10
                    , chunkOverlap = 0
                    , separators = splitTags
                    , keepSeparator = KeepSeparatorStart
                    }
                endOps = startOps {keepSeparator = KeepSeparatorEnd}

            splitText startOps query
              @?= ["Apple", ",banana", ",orange and tomato", "."]

            splitText endOps query
              @?= ["Apple,", "banana,", "orange and tomato."]
        , testCase "splitText iterative splitter keeps separators" $ do
            let requestedChunkSize = 5
                (adjustedChunkSize, output) =
                  testIterativeTextSplitter requestedChunkSize KeepSeparatorStart

            assertChunksWithinSize adjustedChunkSize output
            output
              @?= [ "....5"
                  , "X..3"
                  , "Y...4"
                  , "X....5"
                  , "Y..."
                  ]
        , testCase "splitText iterative splitter discards separators" $ do
            let requestedChunkSize = 5
                (adjustedChunkSize, output) =
                  testIterativeTextSplitter requestedChunkSize KeepSeparatorNone

            assertChunksWithinSize adjustedChunkSize output
            output
              @?= [ "....5"
                  , "..3"
                  , "...4"
                  , "....5"
                  , "..."
                  ]
        , testCase "splitText matches iterative text splitter behavior" $ do
            let text =
                  "Hi.\n\n\
                  \I'm Harrison.\n\n\
                  \How? Are? You?\n\
                  \Okay then f f f f.\n\
                  \This is a weird text to write, but gotta test the splittingggg some how.\n\n\
                  \Bye!\n\n\
                  \-H."
                ops =
                  defaultRecursiveCharacterSplitterOps
                    { chunkSize = 10
                    , chunkOverlap = 1
                    }

            splitText ops text
              @?= [ "Hi."
                  , "I'm"
                  , "Harrison."
                  , "How? Are?"
                  , "You?"
                  , "Okay then"
                  , "f f f f."
                  , "This is a"
                  , "weird"
                  , "text to"
                  , "write,"
                  , "but gotta"
                  , "test the"
                  , "splitting"
                  , "gggg"
                  , "some how."
                  , "Bye!"
                  , "-H."
                  ]
        , testCase "fromLanguage uses Python separators" $ do
            let ops =
                  fromLanguage
                    PYTHON
                    defaultRecursiveCharacterSplitterOps
                      { chunkSize = chunkSize16
                      , chunkOverlap = 0
                      }
                code =
                  "\n\
                  \def hello_world():\n\
                  \    print(\"Hello, World!\")\n\n\
                  \# Call the function\n\
                  \hello_world()\n\
                  \    "

            splitText ops code
              @?= [ "def"
                  , "hello_world():"
                  , "print(\"Hello,"
                  , "World!\")"
                  , "# Call the"
                  , "function"
                  , "hello_world()"
                  ]
        , testCase "fromLanguage uses Go separators" $ do
            let ops = fromLanguage GO defaultRecursiveCharacterSplitterOps {chunkSize = chunkSize16, chunkOverlap = 0}
                code =
                  "\n\
                  \package main\n\n\
                  \import \"fmt\"\n\n\
                  \func helloWorld() {\n\
                  \    fmt.Println(\"Hello, World!\")\n\
                  \}\n\n\
                  \func main() {\n\
                  \    helloWorld()\n\
                  \}\n\
                  \    "

            splitText ops code
              @?= [ "package main"
                  , "import \"fmt\""
                  , "func"
                  , "helloWorld() {"
                  , "fmt.Println(\"He"
                  , "llo,"
                  , "World!\")"
                  , "}"
                  , "func main() {"
                  , "helloWorld()"
                  , "}"
                  ]
        , testCase "fromLanguage uses Markdown separators" $ do
            let ops = fromLanguage MARKDOWN defaultRecursiveCharacterSplitterOps {chunkSize = chunkSize16, chunkOverlap = 0}
                code = "# Sample Document\n\n## Section\n\nThis is the content of the section."

            splitText ops code
              @?= [ "# Sample"
                  , "Document"
                  , "## Section"
                  , "This is the"
                  , "content of the"
                  , "section."
                  ]
        , testGroup
            "fromLanguage language splitters"
            (uncurry4 assertLanguageSplit <$> languageCases)
        , testCase "RST special characters" $ do
            let ops = fromLanguage RST defaultRecursiveCharacterSplitterOps {chunkSize = chunkSize16, chunkOverlap = 0}
            splitText ops "harry\n***\nbabylon is" @?= ["harry", "***\nbabylon is"]
        , testCase "Markdown special characters" $ do
            let ops = fromLanguage MARKDOWN defaultRecursiveCharacterSplitterOps {chunkSize = chunkSize16, chunkOverlap = 0}
            splitText ops "harry\n***\nbabylon is" @?= ["harry", "***\nbabylon is"]
        , testCase "CSharp separators do not contain Java implements keyword" $
            assertBool
              "C# uses ':' for interface implementation, not Java implements"
              ("\nimplements " `notElem` getSeparatorsForLanguage CSHARP)
        , testCase "Elixir separators do not contain while keyword" $
            assertBool
              "Elixir has no while loop"
              ("\nwhile " `notElem` getSeparatorsForLanguage ELIXIR)
        ]
    ]

chunkSize16 :: Int64
chunkSize16 = 16

assertLanguageSplit :: String -> Language -> Text -> [Text] -> TestTree
assertLanguageSplit name language code expected =
  testCase name $ do
    let ops = fromLanguage language defaultRecursiveCharacterSplitterOps {chunkSize = chunkSize16, chunkOverlap = 0}
    splitText ops code @?= expected

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

languageCases :: [(String, Language, Text, [Text])]
languageCases =
  [ ( "RST"
    , RST
    , "\nSample Document\n===============\n\nSection\n-------\n\nThis is the content of the section.\n\nLists\n-----\n\n- Item 1\n- Item 2\n- Item 3\n\nComment\n*******\nNot a comment\n\n.. This is a comment\n    "
    , [ "Sample Document"
      , "==============="
      , "Section"
      , "-------"
      , "This is the"
      , "content of the"
      , "section."
      , "Lists"
      , "-----"
      , "- Item 1"
      , "- Item 2"
      , "- Item 3"
      , "Comment"
      , "*******"
      , "Not a comment"
      , ".. This is a"
      , "comment"
      ]
    )
  , ( "PROTO"
    , PROTO
    , "\nsyntax = \"proto3\";\n\npackage example;\n\nmessage Person {\n    string name = 1;\n    int32 age = 2;\n    repeated string hobbies = 3;\n}\n    "
    , [ "syntax ="
      , "\"proto3\";"
      , "package"
      , "example;"
      , "message Person"
      , "{"
      , "string name"
      , "= 1;"
      , "int32 age ="
      , "2;"
      , "repeated"
      , "string hobbies"
      , "= 3;"
      , "}"
      ]
    )
  , ( "JavaScript"
    , JS
    , "\nfunction helloWorld() {\n  console.log(\"Hello, World!\");\n}\n\n// Call the function\nhelloWorld();\n    "
    , [ "function"
      , "helloWorld() {"
      , "console.log(\"He"
      , "llo,"
      , "World!\");"
      , "}"
      , "// Call the"
      , "function"
      , "helloWorld();"
      ]
    )
  , ( "COBOL"
    , COBOL
    , "\nIDENTIFICATION DIVISION.\nPROGRAM-ID. HelloWorld.\nDATA DIVISION.\nWORKING-STORAGE SECTION.\n01 GREETING           PIC X(12)   VALUE 'Hello, World!'.\nPROCEDURE DIVISION.\nDISPLAY GREETING.\nSTOP RUN.\n    "
    , [ "IDENTIFICATION"
      , "DIVISION."
      , "PROGRAM-ID."
      , "HelloWorld."
      , "DATA DIVISION."
      , "WORKING-STORAGE"
      , "SECTION."
      , "01 GREETING"
      , "PIC X(12)"
      , "VALUE 'Hello,"
      , "World!'."
      , "PROCEDURE"
      , "DIVISION."
      , "DISPLAY"
      , "GREETING."
      , "STOP RUN."
      ]
    )
  , ( "TypeScript"
    , TS
    , "\nfunction helloWorld(): void {\n  console.log(\"Hello, World!\");\n}\n\n// Call the function\nhelloWorld();\n    "
    , [ "function"
      , "helloWorld():"
      , "void {"
      , "console.log(\"He"
      , "llo,"
      , "World!\");"
      , "}"
      , "// Call the"
      , "function"
      , "helloWorld();"
      ]
    )
  , ( "Java"
    , JAVA
    , "\npublic class HelloWorld {\n    public static void main(String[] args) {\n        System.out.println(\"Hello, World!\");\n    }\n}\n    "
    , [ "public class"
      , "HelloWorld {"
      , "public"
      , "static void"
      , "main(String[]"
      , "args) {"
      , "System.out.prin"
      , "tln(\"Hello,"
      , "World!\");"
      , "}\n}"
      ]
    )
  , ( "Kotlin"
    , KOTLIN
    , "\nclass HelloWorld {\n    companion object {\n        @JvmStatic\n        fun main(args: Array<String>) {\n            println(\"Hello, World!\")\n        }\n    }\n}\n    "
    , [ "class"
      , "HelloWorld {"
      , "companion"
      , "object {"
      , "@JvmStatic"
      , "fun"
      , "main(args:"
      , "Array<String>)"
      , "{"
      , "println(\"Hello,"
      , "World!\")"
      , "}\n    }"
      , "}"
      ]
    )
  , ( "Rust"
    , RUST
    , "\nfn main() {\n    println!(\"Hello, World!\");\n}\n    "
    , ["fn main() {", "println!(\"Hello", ",", "World!\");", "}"]
    )
  , ( "LaTeX"
    , LATEX
    , "\nHi Harrison!\n\\chapter{1}\n"
    , ["Hi Harrison!", "\\chapter{1}"]
    )
  , ( "CSharp"
    , CSHARP
    , "\nusing System;\nclass Program\n{\n    static void Main()\n    {\n        int age = 30; // Change the age value as needed\n\n        // Categorize the age without any console output\n        if (age < 18)\n        {\n            // Age is under 18\n        }\n        else if (age >= 18 && age < 65)\n        {\n            // Age is an adult\n        }\n        else\n        {\n            // Age is a senior citizen\n        }\n    }\n}\n    "
    , [ "using System;"
      , "class Program\n{"
      , "static void"
      , "Main()"
      , "{"
      , "int age"
      , "= 30; // Change"
      , "the age value"
      , "as needed"
      , "//"
      , "Categorize the"
      , "age without any"
      , "console output"
      , "if (age"
      , "< 18)"
      , "{"
      , "//"
      , "Age is under 18"
      , "}"
      , "else if"
      , "(age >= 18 &&"
      , "age < 65)"
      , "{"
      , "//"
      , "Age is an adult"
      , "}"
      , "else"
      , "{"
      , "//"
      , "Age is a senior"
      , "citizen"
      , "}\n    }"
      , "}"
      ]
    )
  , ( "CPP"
    , CPP
    , "\n#include <iostream>\n\nint main() {\n    std::cout << \"Hello, World!\" << std::endl;\n    return 0;\n}\n    "
    , [ "#include"
      , "<iostream>"
      , "int main() {"
      , "std::cout"
      , "<< \"Hello,"
      , "World!\" <<"
      , "std::endl;"
      , "return 0;\n}"
      ]
    )
  , ( "Scala"
    , SCALA
    , "\nobject HelloWorld {\n  def main(args: Array[String]): Unit = {\n    println(\"Hello, World!\")\n  }\n}\n    "
    , [ "object"
      , "HelloWorld {"
      , "def"
      , "main(args:"
      , "Array[String]):"
      , "Unit = {"
      , "println(\"Hello,"
      , "World!\")"
      , "}\n}"
      ]
    )
  , ( "Ruby"
    , RUBY
    , "\ndef hello_world\n  puts \"Hello, World!\"\nend\n\nhello_world\n    "
    , [ "def hello_world"
      , "puts \"Hello,"
      , "World!\""
      , "end"
      , "hello_world"
      ]
    )
  , ( "PHP"
    , PHP
    , "\n<?php\nfunction hello_world() {\n    echo \"Hello, World!\";\n}\n\nhello_world();\n?>\n    "
    , [ "<?php"
      , "function"
      , "hello_world() {"
      , "echo"
      , "\"Hello,"
      , "World!\";"
      , "}"
      , "hello_world();"
      , "?>"
      ]
    )
  , ( "Swift"
    , SWIFT
    , "\nfunc helloWorld() {\n    print(\"Hello, World!\")\n}\n\nhelloWorld()\n    "
    , [ "func"
      , "helloWorld() {"
      , "print(\"Hello,"
      , "World!\")"
      , "}"
      , "helloWorld()"
      ]
    )
  , ( "R"
    , R
    , "\nlibrary(dplyr)\n\nmy_func <- function(x) {\n    return(x + 1)\n}\n\nif (TRUE) {\n    print(\"Hello\")\n}\n    "
    , [ "library(dplyr)"
      , "my_func <-"
      , "function(x) {"
      , "return(x +"
      , "1)"
      , "}"
      , "if (TRUE) {"
      , "print(\"Hello\")"
      , "}"
      ]
    )
  , ( "Markdown full"
    , MARKDOWN
    , "\n# Sample Document\n\n## Section\n\nThis is the content of the section.\n\n## Lists\n\n- Item 1\n- Item 2\n- Item 3\n\n### Horizontal lines\n\n***********\n____________\n-------------------\n\n#### Code blocks\n```\nThis is a code block\n\n# sample code\na = 1\nb = 2\n```\n    "
    , [ "# Sample"
      , "Document"
      , "## Section"
      , "This is the"
      , "content of the"
      , "section."
      , "## Lists"
      , "- Item 1"
      , "- Item 2"
      , "- Item 3"
      , "### Horizontal"
      , "lines"
      , "***********"
      , "____________"
      , "---------------"
      , "----"
      , "#### Code"
      , "blocks"
      , "```"
      , "This is a code"
      , "block"
      , "# sample code"
      , "a = 1\nb = 2"
      , "```"
      ]
    )
  , ( "HTML"
    , HTML
    , "\n<h1>Sample Document</h1>\n    <h2>Section</h2>\n        <p id=\"1234\">Reference content.</p>\n\n    <h2>Lists</h2>\n        <ul>\n            <li>Item 1</li>\n            <li>Item 2</li>\n            <li>Item 3</li>\n        </ul>\n\n        <h3>A block</h3>\n            <div class=\"amazing\">\n                <p>Some text</p>\n                <p>Some more text</p>\n            </div>\n    "
    , [ "<h1>Sample Document</h1>\n    <h2>Section</h2>"
      , "<p id=\"1234\">Reference content.</p>"
      , "<h2>Lists</h2>\n        <ul>"
      , "<li>Item 1</li>\n            <li>Item 2</li>"
      , "<li>Item 3</li>\n        </ul>"
      , "<h3>A block</h3>"
      , "<div class=\"amazing\">"
      , "<p>Some text</p>"
      , "<p>Some more text</p>\n            </div>"
      ]
    )
  , ( "Solidity"
    , SOL
    , "pragma solidity ^0.8.20;\n  contract HelloWorld {\n    function add(uint a, uint b) pure public returns(uint) {\n      return  a + b;\n    }\n  }\n  "
    , [ "pragma solidity"
      , "^0.8.20;"
      , "contract"
      , "HelloWorld {"
      , "function"
      , "add(uint a,"
      , "uint b) pure"
      , "public"
      , "returns(uint) {"
      , "return  a"
      , "+ b;"
      , "}\n  }"
      ]
    )
  , ( "Lua"
    , LUA
    , "\nlocal variable = 10\n\nfunction add(a, b)\n    return a + b\nend\n\nif variable > 5 then\n    for i=1, variable do\n        while i < variable do\n            repeat\n                print(i)\n                i = i + 1\n            until i >= variable\n        end\n    end\nend\n    "
    , [ "local variable"
      , "= 10"
      , "function add(a,"
      , "b)"
      , "return a +"
      , "b"
      , "end"
      , "if variable > 5"
      , "then"
      , "for i=1,"
      , "variable do"
      , "while i"
      , "< variable do"
      , "repeat"
      , "print(i)"
      , "i = i + 1"
      , "until i >="
      , "variable"
      , "end"
      , "end\nend"
      ]
    )
  , ( "Haskell"
    , HASKELL
    , "\n        main :: IO ()\n        main = do\n          putStrLn \"Hello, World!\"\n\n        -- Some sample functions\n        add :: Int -> Int -> Int\n        add x y = x + y\n    "
    , [ "main ::"
      , "IO ()"
      , "main = do"
      , "putStrLn"
      , "\"Hello, World!\""
      , "--"
      , "Some sample"
      , "functions"
      , "add :: Int ->"
      , "Int -> Int"
      , "add x y = x"
      , "+ y"
      ]
    )
  , ( "PowerShell short"
    , POWERSHELL
    , "\n# Check if a file exists\n$filePath = \"C:\\temp\\file.txt\"\nif (Test-Path $filePath) {\n    # File exists\n} else {\n    # File does not exist\n}\n    "
    , [ "# Check if a file exists\n$filePath = \"C:\\temp\\file.txt\""
      , "if (Test-Path $filePath) {\n    # File exists\n} else {"
      , "# File does not exist\n}"
      ]
    )
  , ( "PowerShell longer"
    , POWERSHELL
    , "\n# Get a list of all processes and export to CSV\n$processes = Get-Process\n$processes | Export-Csv -Path \"C:\\temp\\processes.csv\" -NoTypeInformation\n\n# Read the CSV file and display its content\n$csvContent = Import-Csv -Path \"C:\\temp\\processes.csv\"\n$csvContent | ForEach-Object {\n    $_.ProcessName\n}\n\n# End of script\n    "
    , [ "# Get a list of all processes and export to CSV"
      , "$processes = Get-Process"
      , "$processes | Export-Csv -Path \"C:\\temp\\processes.csv\""
      , "-NoTypeInformation"
      , "# Read the CSV file and display its content"
      , "$csvContent = Import-Csv -Path \"C:\\temp\\processes.csv\""
      , "$csvContent | ForEach-Object {\n    $_.ProcessName\n}"
      , "# End of script"
      ]
    )
  , ( "VisualBasic6"
    , VISUALBASIC6
    , "\nOption Explicit\n\nPublic Function SumTwoIntegers(ByVal a As Integer, ByVal b As Integer) As Integer\n    SumTwoIntegers = a + b\nEnd Function\n\nPublic Sub Main()\n    Dim i As Integer\n    Dim limit As Integer\n\n    i = 0\n    limit = 50\n\n    While i < limit\n        i = SumTwoIntegers(i, 1)\n\n        If i = limit \\ 2 Then\n            MsgBox \"Halfway there! i = \" & i\n        End If\n    Wend\n\n    MsgBox \"Done! Final value of i: \" & i\nEnd Sub\n"
    , [ "Option Explicit"
      , "Public Function"
      , "SumTwoIntegers("
      , "ByVal"
      , "a As Integer,"
      , "ByVal b As"
      , "Integer) As"
      , "Integer"
      , "SumTwoIntegers"
      , "= a + b"
      , "End Function"
      , "Public Sub"
      , "Main()"
      , "Dim i As"
      , "Integer"
      , "Dim limit"
      , "As Integer"
      , "i = 0"
      , "limit = 50"
      , "While i <"
      , "limit"
      , "i ="
      , "SumTwoIntegers("
      , "i,"
      , "1)"
      , "If i ="
      , "limit \\ 2 Then"
      , "MsgBox \"Halfway"
      , "there! i = \" &"
      , "i"
      , "End If"
      , "Wend"
      , "MsgBox"
      , "\"Done! Final"
      , "value of i: \" &"
      , "i"
      , "End Sub"
      ]
    )
  ]

testIterativeTextSplitter :: Int64 -> KeepSeparator -> (Int64, [Text])
testIterativeTextSplitter requestedChunkSize keepSep =
  let adjustedChunkSize =
        case keepSep of
          KeepSeparatorNone -> requestedChunkSize
          _ -> requestedChunkSize + 1
      ops =
        defaultRecursiveCharacterSplitterOps
          { chunkSize = adjustedChunkSize
          , chunkOverlap = 0
          , separators = ["X", "Y"]
          , keepSeparator = keepSep
          }
      text = "....5X..3Y...4X....5Y..."
      output = splitText ops text
   in (adjustedChunkSize, output)

assertChunksWithinSize :: Int64 -> [Text] -> Assertion
assertChunksWithinSize maxChunkSize =
  mapM_
    ( \chunk ->
        assertBool
          ("Chunk is larger than " <> show maxChunkSize)
          (T.length chunk <= maxChunkSize)
    )
