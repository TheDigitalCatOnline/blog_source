# The Digital Cat — Project Guide

Personal tech blog at **thedigitalcatonline.com**, built with Pelican (Python static site generator).

---

## Quick Reference

| Task | Command |
|------|---------|
| Start dev server | `./scripts/run.sh` |
| Deploy to production | `./scripts/deploy.sh` |
| Compile CSS only | `./scripts/compile_scss.sh` |
| Build production site | `cd pelican && make publish` |

---

## Directory Layout

```
thedigitalcat/
├── pelican/             # Pelican config and content
│   ├── pelicanconf.py   # Dev config (THEME, plugins, Mau, Markdown settings)
│   ├── publishconf.py   # Production overrides (URL, analytics, no debug)
│   ├── content/         # Blog content (see Content section)
│   └── output/          # Generated site (git-ignored, not deployed directly)
├── ragamuffin/          # Active theme (see Theme section)
├── mau/templates/       # Custom Mau HTML templates
├── bengal/              # OLD theme — do not use or edit
├── deploy/              # Copy of output/, synced to S3
├── scripts/             # Build and deploy shell scripts
├── requirements.txt     # Python dependencies
└── version.txt          # Version number managed by punch.py
```

---

## Theme: ragamuffin

The active theme. All theme work happens here.

```
ragamuffin/
├── compile.sh                  # Compiles SCSS → CSS (run this, not sass directly)
├── static/
│   ├── scss/
│   │   ├── _variables.scss     # Brand colours, fonts, layout dimensions — edit here
│   │   └── main.scss           # All site styles — edit here
│   ├── css/
│   │   ├── main.css            # COMPILED OUTPUT — never edit by hand
│   │   └── pygments/           # 30 syntax-highlighting themes (pre-generated)
│   └── js/main.js              # Banner + quote randomisation, sidebar toggle
└── templates/                  # Jinja2 templates
    ├── base.html
    ├── index.html
    ├── article.html
    ├── page.html
    ├── archives.html
    ├── tag.html
    ├── category.html
    ├── articles_list_page.html
    └── includes/
        ├── navbar.html
        ├── sidebar.html
        ├── banner.html
        ├── pagination.html
        ├── post_card.html
        ├── related_posts.html
        └── share.html
```

### CSS / SCSS — Critical Rule

**Never edit `ragamuffin/static/css/main.css` directly.** It is a compiled artifact and will be overwritten.

- Brand tokens (colours, fonts, spacing) → `ragamuffin/static/scss/_variables.scss`
- All other styles → `ragamuffin/static/scss/main.scss`
- After any SCSS change, run `./scripts/compile_scss.sh` to regenerate `main.css`

`scripts/compile_scss.sh` is the single entrypoint for SCSS compilation — it delegates to `ragamuffin/compile.sh`, which calls the `sass` CLI with `--no-source-map --style=expanded`. Both `run.sh` and `deploy.sh` go through this script. MDB UI Kit (Material Design Bootstrap 7.3.2) is loaded from CDN; `main.scss` overrides its CSS custom properties using brand tokens from `_variables.scss`.

Syntax highlighting CSS files in `ragamuffin/static/css/pygments/` are also pre-generated — do not edit them by hand.

### External CSS/JS Dependencies (CDN)

- **MDB UI Kit 7.3.2** (Bootstrap 5 wrapper)
- **Google Fonts**: Open Sans (body), Roboto Slab (headings)
- **Font Awesome 6.5.0** (icons)

---

## Content

All content lives in `pelican/content/`.

```
content/
├── *.mau          # Mau markup articles (newer, preferred format)
├── *.markdown     # Markdown articles (legacy format)
├── images/        # Per-article images (subdirectory per article)
├── code/          # Code samples referenced from articles
├── notebooks/     # Jupyter notebooks
├── files/         # Downloadable files
├── pages/         # Static pages (About, etc.)
└── extra/         # CNAME, robots.txt
```

### Two Content Formats

**Mau** (`.mau`) — newer, preferred for new articles:
```
:pelican.title:Article Title
:pelican.date:2024-01-15
:pelican.category:Programming
:pelican.tags:python, django
:pelican.image:images/article-slug/main.jpg
```

**Markdown** (`.markdown`) — legacy, ~76 existing articles:
```
Title: Article Title
Date: 2024-01-15
Category: Programming
Tags: python, django
Image: images/article-slug/main.jpg
```

New articles should use Mau format. Do not convert existing Markdown articles.

---

## Mau Syntax

Should you need to work on Mau posts or Mau templates, use `MAU_SYNTAX_REFERENCE.md` to learn Mau syntax, and check the official documentation at https://project-mau.github.io/pages/introduction.html for more details.

## Mau Templates

Custom rendering templates for Mau blocks live in `mau/templates/`. These are separate from the theme's Jinja2 templates.

| File | Purpose |
|------|---------|
| `block.admonition.html` | Admonition/callout boxes |
| `block.infobox.html` | Info boxes |
| `block.quote.html` | Pull quotes |
| `include-github.html` | GitHub embed |
| `include-image.html` | Image block |
| `macro.name__callout.html` | Inline callout macro |
| `macro.name__docs-rust.html` | Rust docs macro |
| `header.html`, `footnotes.html`, `source-marker.html` | Standard elements |

The `tip` alias in Mau maps to an admonition with class `tip` and a lightbulb icon (configured in `pelicanconf.py`).

---

## Pelican Configuration

**Dev** (`pelican/pelicanconf.py`): `SITEURL = ""`, `RELATIVE_URLS = True`, `DEBUG = True`

**Prod** (`pelican/publishconf.py`): imports dev config, then overrides `SITEURL`, disables debug, enables Google Analytics (`UA-74364524-1`) and Google Tag Manager (`G-FZ55XG1KGG`).

### Plugins

| Plugin | Purpose |
|--------|---------|
| pelican-related-posts | Related articles section (max 10) |
| pelican-series | Multi-part article series |
| pelican-share-post | Social sharing buttons |
| pelican-sitemap | XML sitemap |
| pelican-tag-cloud | Tag cloud in sidebar |
| pelican-webassets | Asset pipeline (CSS/JS minification at publish time) |

### URL Structure

- Articles: `/blog/{YYYY}/{MM}/{DD}/{slug}/`
- Categories: `/category/{slug}/`
- Tags: `/categories/{slug}/`
- Archives: `/archives/`
- Authors: `/authors/{slug}/`

### Banner Images & Quotes

`BANNER_IMAGES` (dict) and `QUOTES` (list) are defined in `pelicanconf.py` and injected into templates via `tojson`. `ragamuffin/static/js/main.js` picks one of each at random on page load.

---

## Build & Deploy

### Development

```bash
./scripts/run.sh
# Runs: ./scripts/compile_scss.sh && cd pelican && pelican -lr
# Live-reloads at http://localhost:8000
```

### Production Deploy

```bash
./scripts/deploy.sh
```

Steps:
1. `./scripts/compile_scss.sh` (compiles ragamuffin SCSS → CSS)
2. `cd pelican && make publish` (builds with `publishconf.py`)
3. Copies `pelican/output/` → `deploy/`
4. `aws s3 sync deploy/ s3://www.thedigitalcatonline.com --delete`
5. CloudFront invalidation on distribution `E1VK6I0BH0G8RZ`
6. AWS profile: `TDCAdministrator`

---

## Python Environment

Install dependencies:
```bash
pip install -r requirements.txt
```

Key packages: `pelican`, `Markdown==3.3.6`, `mau` (Mau parser), `webassets`, `cssmin`, `jsmin`, `mdx_video`, `punch.py`. (`boussole` appears in `requirements.txt` as a legacy entry but is no longer used in the build flow.)

---

## Versioning

Version is tracked in `version.txt` (currently `207`) and managed by `punch.py` via `punch_config.py` and `punch_version.py`. Use `punch` commands to bump versions, not manual edits.

---

## What NOT to Do

- **Never edit `ragamuffin/static/css/main.css`** — compiled from SCSS, will be overwritten
- **Never edit anything in `bengal/`** — old theme, not in use
- **Never edit files in `pelican/output/`** — generated, will be overwritten
- **Never edit files in `deploy/`** — copied from output, will be overwritten
- Do not convert existing `.markdown` articles to Mau — leave legacy content as-is
