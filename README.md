# Type-safe <style> tags for elm-lang/html

When using `elm-reactor`, there's currently no way to (easily) include a CSS file for use by your view. The `elm-css` package allows you to code type-safe stylesheets in Elm and render them to your view like any other node.

What makes `elm-css` "type-safe" is that all the IDs and classes used by your HTML nodes are set by functions that are built when your stylesheets are created. This ensures that they will match up.

For example:

```elm
    type Id = MyId
    type class = MyClass
    
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

Much of the [CSS3](https://www.w3.org/TR/css-cascade-3/) is supported: `@import`, type, id, class, descendants, children, siblings, adjacents, and pseudo-class/element selectors.
