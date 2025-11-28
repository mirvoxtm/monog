<br>
<br>
<br>
<p align="center">
  <img width="50%" src="resources/img/logo.png" alt="Monog - The Monographic Notation Grammar" style="padding-bottom: 2px" />
</p>

<h3 align="center">Monog - The Monographic Notation Grammar</h3>
<br>
<p align="center">
  Monog is an easy - Markdown inspired markup language for academic purposes and uses. It is the follow-up to the previous <a href="https://markers.mirvox.xyz">Markers Markup Language</a>.
</p>

---

### What to expect from Monog?
Monog shall have *all of the features currently present in Markers*. This means automatic chapter counting, document conversion and all of the present tags of the previous version.

However, Monog aims for a completely new - and easy to learn experience. As such, differently from its predecessor, Monog does not have opening and closing tags - using a completely Markdown inspired syntax, thus making it a completely hands-on, hassle free experience.

Monog will also feature it's own custom stylesheet language, available in a separate file or inside the very own `.mon` document.

**The general direction is for Monog to be the best, easiest and most accessible academic paper formatting language**.

---

### What does Monog stand for? Why not keep Markers?
The name 'Markers' - although i'm very fond of it - makes it hard for people to find and try out the language. As such, Monog stands for ***Monographic Notation Grammar***. That's just a cool acronym, you can use it for other things apart from Monographies.

### I'm from Markers! Will I have a way to still use the previous tags?
Yes, but with an asterisks. This is not the current aim of the language, however, i can assure that in the future, via a _directive_, Markers tags will be available.

---
## Creating a Monog Document.
> Please note that currently, Monog does not do much in it's current state. This is for demonstration purposes only.

A **Monog** document has the following structure:
1. Title
2. Metadata
3. Content

The first line of text of a `.mon` document _will always be the title of the document_. Subsequent lines can be either Metadata or pure content.

Metadata is defined by the following keywords, in this style:
- By: (Your Name), (Another Name)
- At: (Your University/College)
- Mentor: (Your Mentor's Name)
- Location: (Your City or General Location)
- Date: (The Year, Month or Date of the Paper's Publication)
- Keywords: General, Paper, Keywords

At last, the Content is defined by text, chapters, arrow-lists, lists and every other tag that composes the whole body of the document.

As such, the most complete form of a Monog document can be:
```
A Monog Document
By: Person A, Person B
At: University of Open Source
Location: Brazil
Keywords: Monog, Markup Language

This is a general, informative text that is *not encapsulated* in any tag. It has recursive tag formatting properties.

# This is a Chapter
This is a chapter, is acts as a section that shall populate a summary or something similar.

# This is another Chapter.
In the future, more tags shall be available.
## Nested Chapters
Also, Chapters can be nested! Chapters are automatically closed when another one (or none) is found.
```
---
## Text Formatting
Monog inspires itself in Markdown for it's text formatting. It's usage is as follows:
- `*This is a bold text*` -> **This is a bold text**
- `_This is in italics_` -> *This is in italics*
- `~This is strikethrough~` -> <s>This is strikethrough</s>
- `__This is underlined__` -> <u>This is underlined</u>

There is also support for monospaced text with <code>`</code>.
Monog also supports colors for text with the following syntaxes:

- Hexadecimal: `{#FFFFFF}(This is some colored text)`
- RGB: `{255,255,255}(This is some colored text)`

---
### Roadmap
- [x] Text Formatting
- [x] Chapters and Arrow-Lists
- [x] Document Metadata
- [x] HTML Export
- [ ] Automatic Chapter and Summary
- [ ] Link, Image, Video and Audio support
- [ ] References, Quotes and Footnote support
- [ ] Page and Printing support
- [ ] PDF Export
- [ ] Exportação para Norma ABNT (ABNT Norm Export)
- [ ] Custom Stylesheets with TISS
- [ ] Euler (Monog's Simple Math Language) Implementation
