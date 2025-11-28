{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Monog.Converter.Html where

import Language.Monog.Ast.Content (Content(..))
import Language.Monog.Ast.Text (Writing(..))
import Language.Monog.Ast.Tree (Monog(..), Preferences(..), Section(..))

import Data.String.Interpolate.IsString (i)

{-
    This module provides functionality to convert Monog document content into HTML format.
    It includes functions to convert different types of content, such as paragraphs and chapters,
    into their corresponding HTML representations.
-}

toHtml :: Monog -> String
toHtml (Document (Preference title _) sections) =
    [i|<html>
    <head>
    <title>#{title}</title>
    </head>
    <body>
    <h1>#{title}</h1>
    #{concatMap sectionToHtml sections}
    </body>
    </html>|]

sectionToHtml :: Section -> String
sectionToHtml (Section secTitle contents) =
    [i|
    <section class="#{secTitle}">
    #{concatMap contentToHtml contents}
    </section>
    |]

contentToHtml :: Content -> String
contentToHtml (Paragraph writings) = [i|<p>#{concatMap writingToHtml writings}</p>|]
contentToHtml (Chapter level chTitle chContents) =
    blockToHtml "chapter" level chTitle chContents
contentToHtml (ArrowList level title contents) =
    arrowListHtml "arrow-list" level title contents

writingToHtml :: Writing -> String
writingToHtml (Plain text) = text
writingToHtml (Bold ws) = [i|<strong>#{concatMap writingToHtml ws}</strong>|]
writingToHtml (Italic ws) = [i|<em>#{concatMap writingToHtml ws}</em>|]
writingToHtml (Underline ws) = [i|<u>#{concatMap writingToHtml ws}</u>|]
writingToHtml (Strikethrough ws) = [i|<s>#{concatMap writingToHtml ws}</s>|]
writingToHtml (Monospaced ws) = [i|<code>#{concatMap writingToHtml ws}</code>|]
writingToHtml (Colored ws (r, g, b)) = [i|<span style="color: rgb(#{r}, #{g}, #{b});">#{concatMap writingToHtml ws}</span>|]
writingToHtml (Highlighted ws (r, g, b)) = [i|<span style="background-color: rgb(#{r}, #{g}, #{b});">#{concatMap writingToHtml ws}</span>|]

headingTagName :: Int -> String
headingTagName level = "h" ++ show (min 6 (max 2 (level + 1)))

blockToHtml :: String -> Int -> String -> [Content] -> String
blockToHtml className level title contents =
    let headingTag = headingTagName level
    in [i|
        <div class="#{className} level-#{level}">
            <#{headingTag}>#{title}</#{headingTag}>
            #{concatMap contentToHtml contents}
        </div>
    |]

arrowListHtml :: String -> Int -> String -> [Content] -> String
arrowListHtml className level title contents =
    let headingTag = headingTagName level
    in [i|
        <details class="#{className} level-#{level}">
            <summary>#{title}</summary>
            #{concatMap contentToHtml contents}
        </details>
    |]