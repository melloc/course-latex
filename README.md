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

BUILD_LIB:
  The location of this repository. Defaults to "/contrib/projects/course-latex".

SHADED_MODES:
  A space separated list of modes whose content should be shaded. For example,
  a value of "cody sol" would make the modes "cody" and "sol" be shaded.

DEFAULT_MODES:
  A space separated list of modes that are always on. For example, a value
  of "tips" would cause all content within a "tips" version section to
  always be compiled, without having to give "-m tips" on the command-line.

WEB_DIR:
  The path to the web directory. This defaults to "$COURSE_DIR/web". When
  the -w/--web flag is given, the PDF will be placed at
  "$WEB_DIR/content/<type>/<name>.pdf".

COURSE_SEM_ONE:
  The value to use for @nameSemOne{}. Defaults to $COURSE_NUM.

COURSE_SEM_TWO:
  The value to use for @nameSemTwo{}. Defaults to $COURSE_NUM.

COURSE_SEM_OTHER:
  The value to use for @nameSemOther{}. Defaults to $COURSE_NUM.

COURSE_SEM_ADVANCED:
  The value to use for @nameSemAdvanced{}. Defaults to $COURSE_NUM.

Built-in Macros
===============

TODO
