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
includes three programs: `feed` and `doc`, which load feeds and
documents (anything pandoc can read), and pack them into a structure,
and `view`, which reads that structure, and renders it using SDL.

The viewer is designed to resemble an
[info](https://en.wikipedia.org/wiki/Info_%28Unix%29) reader, but with
graphics, and aiming to be more interactive in the future (more like
info reader inside of emacs, and with dynamic input).

### Controls

Controls are similar to those of `info`: `[`/`]`, `p`/`n`, `u` for
structured navigation; space, `f`/`b`, and mouse scroll for positional
navigation; mouse click on a link prints its target into stdout.

### Examples

#### Invocation

```
feed http://xkcd.com/atom.xml \
     https://feeds.feedburner.com/InvisibleBread \
     http://existentialcomics.com/rss.xml \
     http://www.smbc-comics.com/rss.php \
| view | xargs -L 1 xdg-open
```

#### Screenshots

![feeds in the viewer](http://paste.uberspace.net/feed-reader.png)

![readme in the viewer](http://paste.uberspace.net/doc-reader.png)

#### Document structure

```haskell
data Inline = IText String
            | ILink String String
            | ICode (Maybe String) String

data Block = BParagraph [Inline]
           | BImage String
           | BSection String [Block]
           | BCode (Maybe String) String
```

#### Serialization

```
(section "Entries"
         (section "Solar Spectrum"
                  (paragraph "Date: 2016-09-15 00:00:00 UTC")
                  (image "/home/defanor/.cache/feeds/http://imgs.xkcd.com/comics/solar_spectrum.png")
                  (paragraph "I still don't understand why the Sun paid the extra money for Transitions lenses.")
                  (paragraph "URI: " (link "http://xkcd.com/1733/" "http://xkcd.com/1733/")))
         (section "Earth Temperature Timeline"
                  (paragraph "Date: 2016-09-12 00:00:00 UTC")
                  (image "/home/defanor/.cache/feeds/http://imgs.xkcd.com/comics/earth_temperature_timeline.png")
                  (paragraph "[After setting your car on fire] Listen, your car's temperature has changed before.")
                  (paragraph "URI: " (link "http://xkcd.com/1732/" "http://xkcd.com/1732/")))
         (section "Wrong"
                  (paragraph "Date: 2016-09-09 00:00:00 UTC")
                  (image "/home/defanor/.cache/feeds/http://imgs.xkcd.com/comics/wrong.png")
                  (paragraph "Hang on, I just remembered another thing I'm right about. See...")
                  (paragraph "URI: " (link "http://xkcd.com/1731/" "http://xkcd.com/1731/")))
         (section "Starshade"
                  (paragraph "Date: 2016-09-07 00:00:00 UTC")
                  (image "/home/defanor/.cache/feeds/http://imgs.xkcd.com/comics/starshade.png")
                  (paragraph "The New Worlds Mission is already trying to get funding for this, but NASA sponsored their proposal, so it will be hard to catch the telescope people by surprise with it.")
                  (paragraph "URI: " (link "http://xkcd.com/1730/" "http://xkcd.com/1730/"))))
```

### Dependencies

Currently it depends, among other things, on a
[hs-sdl2-image fork](https://github.com/defanor/hs-sdl2-image).
