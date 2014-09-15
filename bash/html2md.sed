# remove SSI directives
/[<][!]--[#].*/d

# main div
/[ ]*[<]div id="main"[>]/d

# div in general
/[ ]*[<]div[>]/d
/[ ]*[<][/]div[>]/d

# leading white space
s/^[ ]*//

# pre
s/[ ]*<pre[>]\(.*\)/    \1/
s/[ ]*<pre class=".*"[>]\(.*\)/    \1/
s/[<][/]pre[>]/\n/
s/^[$]/    $/
s/^[#]/    #/

# headings
s@[ ]*<h1>\([^<]*\)</h1>@title: \1@
s@[ ]*<h2>\([^<]*\)</h2>@## \1@
s@[ ]*<h3>\([^<]*\)</h3>@### \1@
s@[ ]*<h4>\([^<]*\)</h4>@#### \1@
s@[ ]*<h5>\([^<]*\)</h5>@##### \1@
s@[ ]*<h6>\([^<]*\)</h6>@###### \1@

# code
s@[ ]*<code>\([^<]*\)</code>@```\1```@
s@<code>@\n```@
s@</code>@```@

# p
s@[ ]*<[/]*p>[ ]*$@@

# lists
/<ol>/,/<\/ol>/{
  /<li>/{
    s@<li>@# @
  }
  s@<ol>@@
  s@</ol>@@
  s@</li>@@
}

/<ul>/,/<\/ul>/{
  /<li>/{
    s@<li>@- @
  }
  s@<ul>@@
  s@</ul>@@
  s@</li>@@
}
