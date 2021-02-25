Unpack zip into folder
Create `static` and `templates`
CSS, JS, webfonts, images go into `static`
Create base.html importing the main index.html and index.html that `{% extends "base.html" %}`

You need to use `{{ SITEURL }}/{{ THEME_STATIC_DIR }}` to get to the `static` directory
Also replace links in the CSS file
