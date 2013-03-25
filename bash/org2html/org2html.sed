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

  /^[^<]/ {
    s/^/  /
  }
}

# monotype
s~=\(.*\)=~<code>\1</code>~

# lists
/^-/,/^-/ {
  s/^-/<li>/
  a </li>
}

/^$/,/^<li>/ {
  /^<li>/ {
    i <ul>
  }
}

/^<li>/,/^$/ {
  /^$/ {
    i </ul>
  }
}

# pictures
s~^\[\[\(.*\)\]\]~<div><img src="\1" alt="\1"/></div>~g

# paragraphs
/^$/,/^[A-Z]/ {
  /^[A-Z]/ {
    i <p>
  }
}

/^[^<][a-z]/,/^$/ {
  /^$/ {
    i </p>
  }
}

# fix entities
/^[ ]*[<]{1}/ {

}

# Lastly, remove empty lines
/^$/d
