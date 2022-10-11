# The Snip

## Summary

Given a text file with demarcated regions, generate distinct output files containing the
content of those demarcated regions. These demarcated regions, called "snippets", may be 
interleaved, nested, or overlapping.

### Installation

```
which stack || ( curl -sSL https://get.haskellstack.org/ | sh )
stack install the-snip  # Ensure the output dir is on your $PATH
```

### Motivation

This program supports generating documentation. Often, a code snippet is worth thousands of words.  Documentation like technical
instruction that requires having code, but linters and type-checkers can't process inline code samples, and nothing ensures the
code stays up-to-date with other dependent samples.

This program addresses the problem by being able to generate the snippets directly from text files. Those files are then available
for processing by Pygments, LaTeX, or whatever else.

## Usage

### Simple

1. Create a snippet by adding two lines into the file.  One line must contain the literal string "`@snip>>>`" and may start with any other content. This is the "Start Marker". The other line must contain the literal string "`<<<@snip`" and may start with any other content. This is the "End Marker". The program discards all content before the literal strings, so feel free to start the line with a comment declaration and/or a description of the snippet.
1. Run `snip` and pass in the file name as the argument.
1. Read the snippet out of the newly-created output directory.  The snippet directory for an input file has the same as the input file but with `.snip.d` appended. The snippet file name will be `1.snip.<FILEEXT>`, where `<FILEEXT>` is the file extension of the input file. For example, if you're processing Haskell files with a `.hs` file extension, the snippet file name will be `1.snip.hs`.

### Directory

If an argument is a directory, then the input files will be the immediate contents of that directory.

### Recursive

If the program is running with the `-r`/`--recurse` flag and an argument is a directory, then the input files will be the contents of that directory and all its subdirectories.

### Named Snippets

By default, the name of a snippet is the 1-based order of the snippet's start in the file.  If you want to explicitly name the snippet file,
then pass in the snippet file name after the literal string of the start marker.  For instance, this start marker will start a snippet named "foo":

```
# This is the start of the fooing section. @snip>>> foo
```

This snippet will be in the output directory and named `foo.snip.<FILEEXT>`.

The snippet name must be parseable as a relative file path ([details here](https://hackage.haskell.org/package/path-0.9.2/docs/Path-Posix.html#v:parseRelFile)) and may not include whitespace. Whitespace or the end of the line terminates the snippet name. The parser ignores any content on the line after the snippet name, so this start marker will start the same snippet file:

```
# @snip>>> foo This is the start of the fooing section.
```

### Interleaved and Nested Snippets

By default, an end marker terminates all started snippets. If the start marker uses named snippets, though, the end marker may specify which
snippet to end.  For instance, this end marker will end the snippet named "foo":

```
# This is the end of the fooing section. <<<@snip foo
```

Analogous to start markers, the parser also ignores content after the snippet name, so this end marker will end the same snippet:

```
# <<<@snip foo This is the end of the fooing section.
```

Using specific end markers, snippets may start and end anywhere in the file. The parser will disregard lines that are markers for other snippets.

### Whitespace trimming

If the program is running with the `-t`/`--trim` flag, then each line of the snippet has its common leading whitespace trimmed.  
The common leading whitespace for a snippet is the longest sequence of tabs or spaces which start every line.  For instance, this code:

```
    This is my code.
        It is very good code.
    It runs very well and does useful things.
        Yay, my code!
```

will trim down to:

```
This is my code.
    It is very good code.
It runs very well and does useful things.
    Yay, my code!
```

This is nice for code samples extracted from nested sections of the code.

### Caveats

* The file system must be POSIX-compliant.
* Input files must be parseable as UTF8-encoded text files.
* The snippets are UTF8-encoded text files.
* The program must be able to read from all input files.
* The program must be able to freely create subdirectories, write files, and overwrite files in the output directory.
* Start and end markers may not be on the same line.
* There is an implicit catch-all end marker at EOF.
* Explicit snippet names must be unique within the file and may not be positive integers.
