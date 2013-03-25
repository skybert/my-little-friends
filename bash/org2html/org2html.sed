#! /usr/bin/env sed

# headers
s~^#+TITLE: \(.*\)~<h1>\1</h1>~
s~^\* \(.*\)~<h2>\1</h2>~
s~^\*\* \(.*\)~<h3>\1</h3>~
s~^\*\*\* \(.*\)~<h4>\1</h4>~

# source code
/^#+BEGIN_SRC/,/#+END_SRC/ {
  s~^#+BEGIN_SRC .*~<pre class="prettyprint">~
  s~^#+END_SRC~</pre>~
  # TODO indent contents
}

# monotype
s~=\(.*\)=~<code>\1</code>~
