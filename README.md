# Semantic UI

This repository is for trying things based on the
[Semantic UI](https://defanor.uberspace.net/notes/semantic-ui.html)
note.

The primary goal is to untie programs from UIs, simplifying the
programs and providing much more user control over UIs: the ability to
change colors, fonts, controls, or whole UI engines without
conflicting with application-provided ones, and for an application --
the ability to just do its job without dealing with UI.

## Prototype

In the `prototype` directory, there's one of the prototypes, which
includes two programs: `feed`, which loads an atom feed and packs it
into a structure, and `view`, which reads that structure, and renders
it using SDL. The viewer is designed to resemble an
[info](https://en.wikipedia.org/wiki/Info_%28Unix%29) reader, but with
graphics, and aiming to be more interactive (i.e., rendering not
static documents, but a basic and changing program UI, and passing
some events to a program).

Currently it depends, among other things, on a
[hs-sdl2-image fork](https://github.com/defanor/hs-sdl2-image), and
can be executed simply by piping `feed` output into `view` input:

```
feed http://xkcd.com/atom.xml https://feeds.feedburner.com/InvisibleBread | view | xargs -L 1 xdg-open
```

The current document structure:

```haskell
data Inline = IText String
            | ILink String String

data Block = BParagraph [Inline]
           | BImage String
           | BSection String [Block]
```

A screenshot:

![feeds in the viewer](http://paste.uberspace.net/feed-reader.png)

It uses s-expressions for serialization, looks like this:

```
(section "Feeds"
         (section "xkcd.com"
                  (section "Wrong"
                           (paragraph "Updated: 2016-09-09T00:00:00Z")
                           (image "/tmp/http:__imgs.xkcd.com_comics_wrong.png")
                           (paragraph "Hang on, I just remembered another thing I'm right about. See...")
                           (paragraph "URIs:" (link "http://xkcd.com/1731/" "http://xkcd.com/1731/")))
                  (section "Starshade"
                           (paragraph "Updated: 2016-09-07T00:00:00Z")
                           (image "/tmp/http:__imgs.xkcd.com_comics_starshade.png")
                           (paragraph "The New Worlds Mission is already trying to get funding for this, but NASA sponsored their proposal, so it will be hard to catch the telescope people by surprise with it.")
                           (paragraph "URIs:" (link "http://xkcd.com/1730/" "http://xkcd.com/1730/")))
                  (section "Migrating Geese"
                           (paragraph "Updated: 2016-09-05T00:00:00Z")
                           (image "/tmp/http:__imgs.xkcd.com_comics_migrating_geese.png")
                           (paragraph "\"Hey guys! I have a great idea for a migration!\" \"Dammit, Kevin.\"")
                           (paragraph "URIs:" (link "http://xkcd.com/1729/" "http://xkcd.com/1729/")))
                  (section "Cron Mail"
                           (paragraph "Updated: 2016-09-02T00:00:00Z")
                           (image "/tmp/http:__imgs.xkcd.com_comics_cron_mail.png")
                           (paragraph "Take THAT, piece of 1980s-era infrastructure I've inexplicably maintained on my systems for 15 years despite never really learning how it works.")
                           (paragraph "URIs:" (link "http://xkcd.com/1728/" "http://xkcd.com/1728/")))))
```
