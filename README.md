Latex Build system for Brown CS courses
=======================================

This program is meant to help make it easier to write latex documents for
homeworks, projects, course missives, and more. It lifts most repeated
logic out of your documents so that you can concentrate on writing the
assignment. It also provides some builtin macros for things like syntax
highlighting of code and allows running Scheme programs inside the document
to generate text. (See below for more.)


Installing for a course
=======================

Using the build system for your course is simple! You just need to place a
script in your tabin folder that looks like the following:

```shell
#!/bin/sh

export COURSE_DIR="/course/cs000"
export COURSE_NUM="00"
export COURSE_NAME="Introduction to using the Course Latex program"
export COURSE_PROF="Mello"
export FEEDBACK_URL="http://cs.brown.edu/courses/cs000/feedback"

exec /path/to/course-latex/scheme/build "$@"
```

There are some other useful environment variables that you may want to set:

- `BUILD_LIB`: The location of this repository. Defaults to
  `/contrib/projects/course-latex`.
- `SHADED_MODES`: A space separated list of modes whose content should be
  shaded. For example, a value of "cody sol" would make the modes "cody" and
  "sol" be shaded.
- `DEFAULT_MODES`: A space separated list of modes that are always on. For
  example, a value of "tips" would cause all content within a "tips"
  version section to always be compiled, without having to give "-m tips"
  on the command-line.
- `WEB_DIR`: The path to the web directory. This defaults to
  `$COURSE_DIR/web`. When the -w/--web flag is given, the PDF will be placed
  at `$WEB_DIR/content/<type>/<name>.pdf`.
- `COURSE_SEM_ONE`: The value to use for @nameSemOne{}. Defaults to
  `$COURSE_NUM`.
- `COURSE_SEM_TWO`: The value to use for @nameSemTwo{}. Defaults to
  `$COURSE_NUM`.
- `COURSE_SEM_OTHER`: The value to use for @nameSemOther{}. Defaults to
  `$COURSE_NUM`.
- `COURSE_SEM_ADVANCED`: The value to use for @nameSemAdvanced{}. Defaults to
  `$COURSE_NUM`.


Installing locally
==================

To run this, you should make sure that you have installed:

- rubber
- latex
- racket
- pygments (only needed if you're using syntax highlighting)
- pdftk (only needed if you're going to password protect documents)


Debian/Ubuntu
-------------

```shell
apt-get install rubber texlive-latex-base racket pdftk python-pygments
```

Gentoo
-------

```shell
emerge -av rubber texlive racket pdftk pygments
```

FreeBSD
-------

```shell
pkg install rubber texlive-base racket pdftk py27-pygments
```

Mac OS X (using [pkgsrc](http://pkgsrc.joyent.com/))
----------------------------------------------------

```shell
pkgin in py27-pygments rubber teTeX tex-pgfplots
```

Install [Racket](http://racket-lang.org/) separately, and add `/Applications/Racket vX.y/bin/` directory to your `PATH`.


Creating A Document
===================

The main.tex.mz file
--------------------

The main.tex.mz file describes what the entire document looks like. Normally
you'll want to split each problem in an assignment into different files and
include them, to make it easier for concurrent editing and sorting things out.

If you need to include a file, there are three ways to do so:

- @course{file}: Include a file shared between all courses
- @cs{file}: Include a file from `$COURSE_DIR/latex`.
- @localprob{file}: Include the file `file.tex` relative to the main.tex.mz file.

A document header and other information can be pulled in with:

- @missive{}: This is the course's missive.
- @doc{name}{date}: This is a document named `name` and due at `date`.
- @hw{num}{name}: This is homework number `num', with name `name`.
- @lec{num}{name}: This is lecture number `num`, with name `name`.
- @proj{num}{name}: This is project number `num`, with name `name`.
- @lab{num}{name}: This is lab number `num`, with name `name`.
- @exam{num}{name}: This is exam number `num`, with name `name`.
- @workshop{num}{name}: This is workshop number `num`, with name `name`.
- @topic{num}{name}: This is topic number `num`, with name `name`.
- @helpsession{name}: This is a helpsession for `name`.

Some other useful macros are:

- @practice: Start of practice section
- @problems: Start of problems section 
- @checkpoint: Include the checkpoint text (fetch a TA, etc.)
- @endoflab: Include the end of lab text.

Through the @course and @cs directives, you can include a variety of files. The
most common ones that you'll need to include are `coursehead` at the start of
the file, and `courseend` at the end of the file. Once you have declared the
type (using @hw, @proj, etc.), you will also want to include `coursebegin`,
which will take care of the due date and table of contents. These will provide
you with most of the commands you need, and format the page appropriately.

Working on a problem
--------------------

When working on a problem, there are many common things that you may find
yourself needing to do. For example, you may want to make a section only
show up in solution builds, or apply syntax highlighting to some code. The
following directives may prove useful:

- @ignore{body}: Don't print body
- @error{body}: Don't print body
- @version{vername}{body}: Only print the body when built with `-m vername`.
- @env{name}{text}: A more convenient way to wrap `text` with `\begin{name}`
  and `\end{name}`

For printing code with syntax highlighting:

- @includecode{file}: Include and apply syntax highlighting to the file `file'.
- @code{lang}{text}: Print the code `text` with `lang` syntax highlighting.
- @golang{text}: Print the Go code `text` with syntax highlighting.
- @java{text}: Print the Java code `text` with syntax highlighting.
- @ocaml{text}: Print the OCaml code `text` with syntax highlighting.
- @scala{text}: Print the Scala code `text` with syntax highlighting.
- @scheme{text}: Print the Scheme code `text` with syntax highlighting.
- @shell{text}: Print the shell code `text` with syntax highlighting.

For each of the languages above, there are sibling macros with
"inline", "include", or "solution" appended to the name. (For example,
@ocamlinline{text}, @ocamlinclude{file}, and @ocamlsolution{file}.) The
"inline" series of macros are more suitable for writing program fragments
inside a sentence; the "include" macros force Pygments to highlight a file
as a certain language; and the "solution" macros will only include the file
when built with `-m sol`.

The following directives are deprecated, and should be replaced when seen:

- @oopinclude{file}: Same as @includecode{text}
- @oopcode{text}: Same as @scala{text}

