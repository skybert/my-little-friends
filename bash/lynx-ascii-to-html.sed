s#…##g

/[[][0-9][0-9][0-9]?[]].* @.* .*(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Des)/,/.*BUTTON/{
  /[[][0-9][0-9][0-9]?[]].* @.* .*(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Des)/d
  /BUTTON/d
  /repl.*retweet.*favor.*/d

  /View (summary|conversation)/d
  /Embedded image permalink/d
  /View more photos and videos/d

  # hash tags
  s#[[][0-9][0-9][0-9]?[]][[]DEL: \# :DEL[]] #\##g

  # mentions
  s#[[][0-9][0-9][0-9]+[]][[]DEL: @ :DEL[]] ([^ ]*) #<a href="https://twitter.com/\1">@\1</a> #g
  s#[[]DEL: @ :DEL[]] (.*)$#<a href="https://twitter.com/\1">@\1</a> #g
  s#[.]\"#\"#g


  # remove any left over link references
  s#[[][0-9][0-9][0-9]+[]]##g

  # convert URLs to links
  s#[ ](https?://[^ ]*)[ ]?# <a href="\1">\1</a>#g

  # pictures
  s#(pic.twitter.com/.*)[ ]?#<a href="http://\1">\1</a>#g

  p
}
