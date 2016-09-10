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
`feed | view | xargs -L 1 xdg-open`.

The current document structure:

```haskell
data Inline = IText String
            | ILink String String

data Block = BParagraph [Inline]
           | BImage String
           | BSection String [Block]
```

And how it looks like:

![xkcd feed in the viewer](http://paste.uberspace.net/feed-reader.png)
