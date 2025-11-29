{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Binah.Converter.Html where

import Language.Binah.Ast.Content (Content(..))
import Language.Binah.Ast.Text (Writing(..))
import Language.Binah.Ast.Tree (Binah(..), Preferences(..), Section(..))

import Data.String.Interpolate.IsString (i)
import Control.Monad.State (State, get, put, evalState, modify, gets)
import Data.List (intercalate)
import Data.Char (ord)

{-
    This module provides functionality to convert Binah document content into HTML format.
    It includes functions to convert different types of content, such as paragraphs and chapters,
    into their corresponding HTML representations.
-}

type Counters = [Int]
type SummaryEntry = (String, Int, String)

toHtml :: Binah -> String
toHtml (Document (Preference title _) sections) =
    evalState (do
        sectionsHtml <- mapM sectionToHtml sections
        return [i|<html>
    <head>
    <title>#{title}</title>
    </head>
    <body>
    <h1>#{title}</h1>
    #{concat sectionsHtml}
    </body>
    </html>|]) []

sectionToHtml :: Section -> State Counters String
sectionToHtml (Section secTitle contents) = do
    let tocEntries = collectChapters contents
        contentToHtml' = contentToHtml tocEntries
    contentsHtml <- mapM contentToHtml' contents
    return [i|
        <section class="#{secTitle}">
        #{concat contentsHtml}
        </section>
        |]

contentToHtml :: [SummaryEntry] -> Content -> State Counters String
contentToHtml _ (Paragraph writings) = return [i|<p>#{concatMap writingToHtml writings}</p>|]
contentToHtml _ (Url f ws) = return [i|<p><!-- url placeholder: #{show f} -->#{concatMap writingToHtml ws}</p>|]
contentToHtml toc (Chapter level chTitle chContents) =
    blockToHtmlWithToc toc "chapter" level chTitle chContents
contentToHtml toc (ArrowList level title contents) =
    arrowListHtmlWithToc toc "arrow-list" level title contents
contentToHtml toc Summary =
    return $ tocHtml toc
contentToHtml _ (CodeBlock code) =
    return [i|<pre><code>#{escapeHtml code}</code></pre>|]
contentToHtml toc (BulletList level items) =
    bulletListHtml toc level items
contentToHtml _ Break = return "<br/>"

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

blockToHtmlWithToc :: [SummaryEntry] -> String -> Int -> String -> [Content] -> State Counters String
blockToHtmlWithToc toc className level title contents =
    let headingTag = headingTagName level
    in do
        incrementCounter level
        numbering <- getNumbering level
        let contentToHtml' = contentToHtml toc
        contentsHtml <- mapM contentToHtml' contents
        return [i|
        <div class="#{className} level-#{level}">
            <#{headingTag}>#{numbering} #{title}</#{headingTag}>
            #{concat contentsHtml}
        </div>
    |]

arrowListHtmlWithToc :: [SummaryEntry] -> String -> Int -> String -> [Content] -> State Counters String
arrowListHtmlWithToc toc className level title contents =
    do
        let contentToHtml' = contentToHtml toc
        contentsHtml <- mapM contentToHtml' contents
        return [i|
        <details class="#{className} level-#{level}">
            <summary>#{title}</summary>
            #{concat contentsHtml}
        </details>
    |]

bulletListHtml :: [SummaryEntry] -> Int -> [Content] -> State Counters String
bulletListHtml toc level items = do
    let contentToHtml' = contentToHtml toc
    itemsHtml <- mapM (\b -> case b of
                                                            Paragraph ws -> return ("<li>" ++ concatMap writingToHtml ws ++ "</li>")
                                                            _ -> do inner <- contentToHtml' b; return ("<li>" ++ inner ++ "</li>")) items
    return [i|<ul class="bullet-list level-#{level}">
        #{concat itemsHtml}
    </ul>|]

tocHtml :: [SummaryEntry] -> String
tocHtml entries =
    let rows = concatMap (\(num, _, t) -> "<p>" ++ num ++ " " ++ t ++ "</p>\n") entries
    in "<div class=\"toc\">\n" ++ rows ++ "</div>"

collectChapters :: [Content] -> [SummaryEntry]
collectChapters contents = fst $ collect [] contents
  where
    collect :: Counters -> [Content] -> ([SummaryEntry], Counters)
    collect counters [] = ([], counters)
    collect counters (Paragraph _ : rest) = collect counters rest
    collect counters (Url _ _ : rest) = collect counters rest
    collect counters (ArrowList lvl _ children : rest) =
        let (childEntries, countersAfterChildren) = collect counters children
            (restEntries, countersAfterRest) = collect countersAfterChildren rest
        in (childEntries ++ restEntries, countersAfterRest)
    collect counters (Chapter lvl title children : rest) =
        let (newCounters, numbering) = incrementOnCounters counters lvl
            entry = (numbering, lvl, title)
            (childEntries, countersAfterChildren) = collect newCounters children
            (restEntries, countersAfterRest) = collect newCounters rest
        in (entry : (childEntries ++ restEntries), countersAfterRest)
    collect counters (_:rest) = collect counters rest

    incrementOnCounters :: Counters -> Int -> (Counters, String)
    incrementOnCounters counts level =
        let idx = level - 1
            len = length counts
            countsExt = if len <= idx then counts ++ replicate (idx - len + 1) 0 else counts
            updated = [ if i == idx then (countsExt !! i) + 1
                        else if i > idx then 0
                        else countsExt !! i
                      | i <- [0 .. length countsExt - 1] ]
            numbering = intercalate "." (map show (take level updated))
        in (updated, numbering)

incrementCounter :: Int -> State Counters ()
incrementCounter level = do
    counters <- get
    let idx = level - 1
        len = length counters
        countersExt = if len <= idx then counters ++ replicate (idx - len + 1) 0 else counters
        updated = [ if i == idx then (countersExt !! i) + 1
                    else if i > idx then 0
                    else countersExt !! i
                  | i <- [0 .. length countersExt - 1] ]
    put updated

getNumbering :: Int -> State Counters String
getNumbering level = do
    counters <- gets (take level)
    return $ intercalate "." (map show counters)

escapeHtml :: String -> String
escapeHtml = concatMap escapeChar
    where
        escapeChar '&' = "&amp;"
        escapeChar '<' = "&lt;"
        escapeChar '>' = "&gt;"
        escapeChar c   = [c]