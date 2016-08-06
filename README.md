# Type-safe &lt;style&gt; tags for elm-lang/html

When using `elm-reactor`, there's currently no way to (easily) include a CSS file for use by your view. The `elm-css` package allows you to code type-safe stylesheets in Elm and render them to your view like any other node.

What makes `elm-css` "type-safe" is that all the IDs and classes used by your HTML nodes are set by functions that are built when your stylesheets are created. This ensures that they will match up.

For example:

```elm
type Id = MyId
type Class = MyClass

-- import a font
imports = ["https://fonts.googleapis.com/css?family=Droid+Sans:400"]

-- create a rule
rule =
    { selectors = [Css.Class MyClass]
    , descriptor = [("font-family", "Droid Sans")]
    }
    
-- create the stylesheet
stylesheet = Css.stylesheet imports [rule]

-- render some HTML that uses it
render =
    Html.div []
        [ stylesheet.node
        , div [ stylesheet.class MyClass ] [ Html.text "Droid Sans!" ]
        ]
```

List of currently supported features:

* @import urls
* Type selectors (e.g. "div")
* Id selectors (e.g. "#content")
* Class selectors (e.g. ".menu-item")
* Descendant selectors (e.g. "div table")
* Immediate child selectors (e.g. "hr > p")
* Sibling selectors (e.g. "li ~ li")
* Adjacent selectors (e.g. "br + p")
* Pseudo classes and elements (e.g. "p:first-line:first-letter::after")
