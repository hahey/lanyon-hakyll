## Lanyon for Hakyll

This theme is a port of [Lanyon theme](https://github.com/poole/lanyon) originated by Mark Otto (mdo)
for Haskell-based static site generator [Hakyll](https://jaspervdj.be/hakyll/) originally written by Jasper Van der Jeugt.

The pagination code is a modified version of [the code by Mike Limansky](https://github.com/limansky/limansky_me/blob/master/src/site.hs).

The dynamic sidebar activation is explained in my [blog post](https://heuna-kim.net/posts/2020-11-19-Hakyll-Sidebar-Activation.html).

The current version does not support tags, related posts, and syntax highlight but it will be updated soon.

### Previews

![Preview](https://github.com/hahey/lanyon-hakyll/blob/main/previews/index-sidebar.png?raw=true)
![Preview](https://github.com/hahey/lanyon-hakyll/blob/main/previews/index.png?raw-true)

You can find an example website: [my blog](https://heuna-kim.net).

The original website in Jekyll: [Lanyon](https://lanyon.getpoole.com/).

### Building the site with stack
Assuming that you have `stack` installed ([install instruction](https://docs.haskellstack.org/en/stable/install_and_upgrade/)),
```
$ git clone https://github.com/hahey/lanyon-hakyll.git
$ cd lanyon-hakyll
$ stack build
$ stack exec site clean
$ stack exec site build
$ stack exec site watch
```
Then you can access the site at `http://localhost:8000` using your browser.

### Customization

#### Meta information
You can change the following website information in the file `site.hs`.

For the RSS feed:
```
feedConfig = FeedConfiguration
    { feedTitle       = "lanyon-hakyll: Lanyon Theme on Hakyll"
    , feedDescription = "A Fork of Lanyon based on Poole"
    , feedAuthorName  = "Heuna Kim"
    , feedAuthorEmail = "ai@heuna-kim.net"
    , feedRoot        = "https://github.com/hahey/lanyon-hakyll"
    }
```
For other informations:
```
siteCtx :: Context String
siteCtx =
    baseCtx `mappend`
    constField "site_description" "Lanyon Theme on Hakyll" `mappend`
    constField "site-url" "https://github.com/hahey/lanyon-hakyll" `mappend`
    constField "tagline" "A Fork of Lanyon based on Poole" `mappend`
    constField "site-title" "lanyon-hakyll" `mappend`
    constField "copy-year" "2020" `mappend`
    constField "github-repo" "https://github.com/hahey/lanyon-hakyll" `mappend`
    defaultContext
baseCtx =
    constField "baseurl" "http://localhost:8000"
```

You can keep the `baseurl` field for debugging purposes or viewing the site on your local computer but when you publish it, you will need to change it to the domain address of your website.

#### Pages and posts
All your files in the `pages` folder will be compiled, listed in the sidebar in an alphabetical order and displayed upon clicking them.
All your markdown files in the `posts` folder will be compiled, displayed in the home/index page with the pagination and also listed in the archive page.

#### Color setting
As in the original theme, add your favorite color theme to the `<body>` element in the `templates/default.html` file, for example:
```
<body class="theme-base-0d">
```
For the available color theme classes, please look at [the repository of Lanyon](https://github.com/poole/lanyon#themes).
