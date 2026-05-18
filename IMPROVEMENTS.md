# Call conventions, ownership, and mutability in Rust

Source: pelican/content/call-conventions-ownership-and-mutability-in-rust.mau

## Top Priority — Major Issues

| # | Location | Issue | Suggestion |
|---|----------|-------|------------|
| 1 | Line 244 | Technical error: "2 bytes * 2^32^" — should be **2^20^** (or 1024 * 1024). 2 bytes × 2^20 = 2 MiB, which matches the stated size. 2^32 would give 8 GiB. | Change to `2 bytes * 2^20^` |
| 2 | Line 70 | Subject-verb agreement: "Why do computer provide" | Change to "Why do **computers** provide" |
| 3 | Line 280 | Typo: "a coupe of weeks" | Change to "a **couple** of weeks" |
| 4 | Line 168 | Grammar: "reads one of the field as if the variable was an `Item`" | Change to "reads one of the **fields** as if the variable **were** an `Item`" (subjunctive, more correct in BrE) |
| 5 | Line 217 | Compound number missing hyphen: "the 16 bits value" | Change to "the **16-bit** value" |
| 6 | Line 270 | Grammar: "another features of the language" | Change to "another **feature** of the language" |
| 7 | Line 290 | Article missing: "we make a copy of value" | Change to "we make a copy of **the** value" |
| 8 | Line 333 | Run-on sentence / comma splice: "I won't discuss it further in this article, make sure you read..." | Use a semicolon or split: "I won't discuss it further in this article**;** make sure you read..." |
| 9 | Line 503 | Awkward redundancy: "the function changed the value of the copied value and not that of the original variable" | Change to "the function changed the **copied value** and not the original variable" |
| 10 | Line 412 | "to derive the trait to implement it for the structure" — repetitive and slightly confusing | Consider "deriving the trait is enough for the structure to implement it" |

## Medium Priority — Clarity, Style, and British English

| # | Location | Issue | Suggestion |
|---|----------|-------|------------|
| 11 | Line 12 | "high-level language" — singular, but the context implies plural | Change to "high-level **languages**" |
| 12 | Line 22 | "highly recommend to eventually become familiar" — unidiomatic | Change to "highly recommend that you eventually become familiar" |
| 13 | Line 109 | "compiler will byte copy the value" — awkward; "byte-copy" is not common | Change to "the compiler will copy the value byte by byte" |
| 14 | Line 131 | "16 bit value 42" — hyphen missing | Change to "**16-bit** value 42" |
| 15 | Line 132 | Stray inconsistency: throughout the post you sometimes write `[class]("1","callout")` (no space after comma) and sometimes `[class]("1", "callout")` (with space). Mau accepts both, but pick one for consistency. | Standardise on one form (suggest the spaced form `[class]("1", "callout")`) |
| 16 | Lines 90, 174, 231 | Code style: missing space before `{` in `-> u16{` | Change to `-> u16 {` (idiomatic Rust) |
| 17 | Line 179 | Trailing whitespace at end of line after the period. | Remove trailing space |
| 18 | Line 208 | "Let's have a deep look:" — slightly unusual phrasing | Consider "Let's look at it in detail:" or "Let's have a closer look:" |
| 19 | Line 264 | Reference to callout `[class]("2", "callout")` appears in the explanation but no `|2|` marker is present in the Assembly snippet at lines 247–259. | Either add a `|2|` marker on `mov ax, word ptr [rdi]` (line 249) or remove the reference |
| 20 | Line 270 | "another features" (already noted) AND "we first need to discuss another features of the language: ownership." — also reads better as "another feature of the language: ownership". | (See item 6) |
| 21 | Line 327 | "implements the trait `Copy` out of the box" — fine, but "out of the box" is American English. | Consider "implements the `Copy` trait by default" or "natively" |
| 22 | Line 328 | "The Copy trait" header section uses "trait `Copy`" and "`Copy` trait" interchangeably. | Pick one order and stick to it (Rust idiom is "`Copy` trait") |
| 23 | Line 337 | "around 2014" is fine, but "It was changed around 2014" — minor: "was changed in 2014" sounds firmer if the year is known. | Optional |
| 24 | Line 414 | "for a struct that doesn't implement `Copy` and for one that does" — reads slightly awkwardly | Consider "for a struct that doesn't implement `Copy` and one that does" |
| 25 | Line 451 | "While the role of `mut` in front of variables is usually simple to grasp" | Possibly "**straightforward**" instead of "simple" |
| 26 | Line 528 | "This is paramount to understand" — "paramount" usually doesn't take "to + verb". | Change to "It is **crucial** to understand this" or "This is **essential** to understand" |
| 27 | Line 559 | "I am expecting them to vacate it after a while, and its management is my responsibility" — slightly stiff, but acceptable | Optional rewrite |
| 28 | Line 631 | Missing semicolon after `println!`: `println!("Value {}", i.value)` | Add `;` for consistency with other examples (Rust accepts both, but other snippets in the post use `;`) |
| 29 | Line 687 | "it definitely was to me to clarify" — slightly awkward | Consider "it definitely helped me clarify" |
| 30 | Line 691 | "I highly recommend to watch this video" — unidiomatic | Change to "I highly recommend **watching** this video" |

## Low Priority — Polish and Consistency

| # | Location | Issue | Suggestion |
|---|----------|-------|------------|
| 31 | Line 12 | "computers, and to use it proficiently requires" — comma before subordinate clause is OK, but the sentence structure is slightly clunky. | Consider: "Rust, like C and C++, is a low-level language that exposes details of the underlying architecture; using it proficiently requires…" |
| 32 | Line 39 | "[docs-rust](\"book/ch03-03-how-functions-work.html#parameters\", \"the Rust book\")" — verify the link works and URL is current. | Manual check |
| 33 | Line 44 | "an infinite amount of user-defined types" — "infinite" is hyperbolic; "any number of" or "countless" is more accurate. | Optional rewrite |
| 34 | Lines 56, 76 | "long-term storage like hard drives" — would Brits prefer "such as"? Both are accepted in BrE. | Optional |
| 35 | Line 70 | Heading "Why two different ways?" — the body answers it well, but the heading could read "Why two different ways exist". | Optional |
| 36 | Line 111 | "the amazing Compiler Explorer" — "amazing" is a touch American-flavoured; "excellent" reads more BrE. | Optional |
| 37 | Line 270 | "ownership" introduced here — flow is good, but you could foreshadow "mutability" too since the article structure introduces all three. | Optional |
| 38 | Line 282 | "I annotated the address of a shop on a piece of paper" — "wrote down" reads more naturally than "annotated". | Consider "I wrote down the address of a shop on a piece of paper" |
| 39 | Line 294 | "there is no such a thing" — should be "no such thing" (no "a"). | Change to "no **such thing**" |
| 40 | Line 389 | Section header "Implementing Copy for structs" — consistent with style; OK. | None |
| 41 | Line 432 | Code example missing the `:N:` markers (callouts). The narrative at line 439 doesn't reference any markers, so this is fine — just noting the inconsistency with surrounding examples. | None / optional |
| 42 | Line 587 | "everything is coherent here" — "consistent" is more idiomatic in BrE technical writing than "coherent" in this sense. | Consider "everything is **consistent** here" |
| 43 | Line 597 | Code: `*value += 1` — missing trailing semicolon (Rust allows it as a tail expression with unit type, but for clarity `*value += 1;` is better). | Consider adding `;` |
| 44 | Line 670 | "Use `llvm-objdump` options and `awk`" — slightly awkward; "Use `llvm-objdump` with `awk`" reads better. | Optional |
| 45 | Line 683 | "if you use something else than `proto`" | Change to "if you use something **other than** `proto`" |
| 46 | Lines 39, 63, 185, 333, 335 | The custom macro `[docs-rust]` is used consistently — good. | None |
| 47 | General | The recap section at line 637–646 is excellent and provides a clear decision tree. | None — well done |
| 48 | Line 441 | "passing arguments by value and by reference might produce identical results" — accurate caveat, well placed. | None |

## Internal Consistency Notes

- **No `TODO` markers found.** Good.
- **Promised content delivered:**
  - "We will see an example when we discuss mutability later" (line 322) — fulfilled in the Mutability section.
  - "Later, we will see an example of this" (line 139, regarding compiler optimisation overriding pass-by-value) — fulfilled in "The compiler's role" section.
  - "See the section 'Disassemble Rust' at the end of the post" (line 111) — section exists at line 648.
- **Callout numbering inconsistencies:**
  - In the first Assembly block (lines 117–127), callouts are 1, 2, 3, 5, 6, 7 — **callout 4 is skipped**. This appears intentional (since the second Assembly block uses 4 for `lea`), but a reader following the first example would notice the gap. Consider renumbering to 1–6 for the first block.
  - Lines 247–259 (third Assembly block): the explanation at line 264 references callout `[class]("2", "callout")` but no `|2|` marker exists in the snippet. (Also flagged in item 19.)
- **Structure:** The three-part split (call conventions → ownership → mutability) is clear, well signposted, and the recap ties them together effectively.

## Code Verification

The Rust snippets are syntactically correct and the Assembly explanations match what `rustc 1.83.0` produces for analogous code on Compiler Explorer. The one technical error found is the **2^32 vs 2^20** mistake (item 1) — important to fix because the maths doesn't work otherwise.

# Flask project setup: TDD, Docker, Postgres and more - Part 1

Source: pelican/content/flask-project-setup-tdd-docker-postgres-and-more-part-1.mau

## Top Priority — Major Issues

| # | Location | Issue | Suggestion |
|---|----------|-------|------------|
| 1 | Line 13 | Typo: "tutorials on Internet that **tech** you" | "tutorials on the Internet that **teach** you" |
| 2 | Line 27 | Grammar: "Run in production with no changes other **that** the static configuration" | "other **than** the static configuration" |
| 3 | Line 30 | Typo: "**Possible** simulate production" | "**Possibly** simulate production" (or "Be able to simulate") |
| 4 | Line 26 | Grammar: "Run **test** on an ephemeral database" | "Run **tests** on an ephemeral database" |
| 5 | Line 17 | Grammar: "Having seen too many **project suffer**" | "Having seen too many **projects suffer**" |
| 6 | Line 42 | Typo: "The configuration of **you** project" | "The configuration of **your** project" |
| 7 | Line 148 | Grammar: "the main difference is in **performances**" | "the main difference is in **performance**" (uncountable in this sense in BrE) |
| 8 | Line 162 | Stray double colon in code: `def create_app(config_name)::1:` — this is legal in Mau (callout marker), but worth confirming it renders correctly. Same pattern recurs (lines 257, 260, 278, 304, 308, 309). Looks deliberate; not an error, just flagging in case any are unintentional. | Verify rendering |
| 9 | Line 205 | Broken link macro: `[link]("http://127.0.0.1:5000/")` — `[link]` requires a URL as the first positional arg without quotes, or with quotes plus a label. As written it produces a link with the URL as text and may render oddly. Same on lines 325 and 459. | Use `[link](http://127.0.0.1:5000/, "http://127.0.0.1:5000/")` or just plain text |
| 10 | Line 220 | Grammar: "**included** Terraform" | "**including** Terraform" |
| 11 | Line 237 | Grammar/typo: "As in **flask** this is partially done" | "As in **Flask** this is partially done" (capitalisation, consistent with rest of post) |
| 12 | Line 549 | Typo: "whoever you **thing** might be interested" | "whoever you **think** might be interested" |
| 13 | Line 406 | Grammar: "Once the image has been **build**" | "Once the image has been **built**" |
| 14 | Line 331 | Stray colon in section heading: `=== Resources:` — every other "Resources" header in the post (lines 98, 211, 541) has no trailing colon. | Remove the colon for consistency |

## Medium Priority — Style, Clarity, and British English

| # | Location | Issue | Suggestion |
|---|----------|-------|------------|
| 1 | Line 15 | "I was very unsatisfied" — "dissatisfied" reads more naturally than "unsatisfied" in BrE for this sense | "I was very dissatisfied" |
| 2 | Line 32 | "I won't show here how to create the production infrastructure" — comma helps flow | "I won't show, here, how to…" or restructure |
| 3 | Line 36 | "frustrate the desire of 'see things happen'" — agreement issue | "frustrate the desire **to** 'see things happen'" |
| 4 | Line 34 | Heading: "A general advice" — "advice" is uncountable in BrE | "Some general advice" or "A general piece of advice" |
| 5 | Line 76 | "As you can see this allows me" — comma after "see" reads better | "As you can see, this allows me" |
| 6 | Line 76 | "speeds up the **deploy**" — noun "deploy" is American/jargon; BrE prefers **deployment** | "speeds up the deployment" |
| 7 | Line 78 | "as I'm using black to format the code I have to configure flake8 to accept what I'm doing" — sentence ends without a full stop before the file label | Add a full stop |
| 8 | Line 90 | "the editor for a programmer is like the violin for the violinist" — "for **the**/**a** programmer … for **a** violinist" feels mismatched. | "…is like a violin to a violinist." |
| 9 | Line 146 | "to avoid duplication (which is always good)" — the parenthetical is ambiguous (avoiding duplication is good, not duplication itself). | "to avoid duplication, which is always a good thing" |
| 10 | Line 148 | "As the documentation clearly states" — slightly preachy. | "As the documentation states" |
| 11 | Line 177 | "the standard 'Hello, world!' route" — earlier in the same code block (line 172) it's `"Hello, World!"` (capital W). Be consistent. | Use `"Hello, World!"` in both places |
| 12 | Line 179 | "any HTTP server we will use in production (for example Gunicorn or uWSGI) will be immediately working" | "will work immediately" |
| 13 | Line 179 | "since WSGI is a standard specification using that makes me sure" — "makes me sure" is awkward. | "since WSGI is a standard specification, using it ensures that any HTTP server…" |
| 14 | Line 191 | "tightly connected with" — BrE often prefers "tightly connected to" | minor; either works |
| 15 | Line 205 | "the **greetings** message" | "the **greeting** message" |
| 16 | Line 220 | "the choice of JSON comes from the fact that…" — slightly heavy | "I chose JSON because it is widespread…" |
| 17 | Line 237 | "a tradition initiated by Django" | "a tradition started by Django" reads more naturally |
| 18 | Line 302 | "I might use it to **customise subcommands of the flask main script**" | "I might use it to customise subcommands of Flask's main script" |
| 19 | Line 325 | "because of `FLASK_ENV` has been set" — extra "of" | "because `FLASK_ENV` has been set" |
| 20 | Line 334 | Trailing whitespace after the closing parenthesis on the click bullet | tidy up |
| 21 | Line 342 | "It's also a complex technology that sometimes requires a lot of work to get properly configured" | small clarity tweak |
| 22 | Line 344 | "interpolate environment variables. Once again through the environment variable…" — comma after "Once again" | "Once again, through the environment variable…" |
| 23 | Line 363 | "the requirements directory is copied into the image" — clear, but "is copied into the image at build time" would mirror "mounted live into the image at run time" later in the sentence | balance the prose |
| 24 | Line 365 | "This is not a complicated process, so I will keep it as a manual process for now." — "process" appears twice. | "This isn't complicated, so I'll keep it manual for now." |
| 25 | Line 406 | "We are explicitly passing environment variables here, as we have not wrapped docker-compose in the manage script yet." | optional |
| 26 | Line 428 | "you can `docker ps`" — verbing of `docker ps` is colloquial | "you can run `docker ps`" |
| 27 | Line 436 | "you can **login** directly" — "log in" is the verb; "login" is the noun | "you can log in directly" |
| 28 | Line 453 | "To tear down the containers, when running as daemon" | "when running as a daemon" |
| 29 | Line 459 | "you can head to either … or …" — "head over to" or "navigate to" reads more naturally | optional |
| 30 | Line 461 | "as their number is going to increase as soon as we add a database" | "since their number will grow as soon as we add a database" |
| 31 | Line 533 | "I resisted the temptation to refactor them because I know that the command `compose` will need some changes" | "will need changes" |

## Low Priority — Polish and Consistency

| # | Location | Issue | Suggestion |
|---|----------|-------|------------|
| 1 | Line 17 | "I consider this setup by no means _better_ than others" | "in no way better" is slightly punchier |
| 2 | Line 29 | "to create a sandbox where I can test queries" | "in which I can test queries" reads more formally |
| 3 | Line 36 | "spend tenfold later" — "tenfold more later" or "ten times as much later" reads more naturally | optional |
| 4 | Line 48 | "**3 files**" — BrE house style usually spells out small numbers ("three files"). | spell out |
| 5 | Line 53 | "## This file is currently empty" inside the `production.txt` snippet — `#` (single) is canonical for requirements files. | optional |
| 6 | Line 76 | Long sentence about production/testing/development; could be split | optional |
| 7 | Line 90 | "Take your time to properly configure the editor and the linter(s)." — split infinitive. | "to configure the editor and the linter(s) properly" |
| 8 | Line 104, 218, 336 | Heading numbering uses " - " (space hyphen space) — BrE often uses an en-dash " – ". | leave as is, but flagging |
| 9 | Line 116 | `[@source, sh]` vs other shell blocks tagged `[@source, bash]` (line 195) and `[@source]` with no language (line 297, 299). | optional consistency tidy |
| 10 | Line 215 | The link text is `WSGI` but the URL points to wsgi.readthedocs.io — fine | — |
| 11 | Line 216 | "My post [link]({{filename}}dissecting-a-web-stack.markdown,'Dissecting a web stack') includes a section on WSGI" — verify the target file actually has that extension on the site. | verify target |
| 12 | Line 555 | "I reviewed the whole tutorial and corrected several typos" — could end with a full stop for consistency | minor |
| 13 | Line 549 | Trailing whitespace at end of paragraph (after "interested. ") | tidy |
| 14 | Lines 96, 209, 329, 539 | "Git commit" sections use `[link](URL,"text")` — there's no space after the comma, while other link macros in the file (line 13, 15) sometimes have a space. | minor |

## Internal Consistency Notes

**Forward references that are honoured later:**

- Line 18 mentions "the deploy of an application in production… I want my setup to be 'deploy-friendly'" — Part 3 delivers this. ✓
- Line 28 "Have a command to initialise databases and manage migrations" — delivered in Part 2. ✓
- Line 29 "Have a way to spin up 'scenarios'" — delivered in Part 3. ✓
- Line 30 "Possible simulate production in the local environment" — delivered in Part 3. ✓
- Line 32 "I won't show here how to create the production infrastructure" — Part 3 delivers a "How to create the production infrastructure" section that gives only hints. ✓
- Line 342 "there is a major complication with using a database server that I will describe later" — delivered in Part 2 (file permissions discussion around `flask db init` / `flask db migrate`). ✓ Consider adding a brief pointer like "(I'll come to this in the next post when we add Postgres)".
- Line 344 "(at least the database)" — anticipates Part 2. ✓

**Backward references in later parts pointing to Part 1:** Accurate.

**No `TODO` markers found.** ✓

**Series intro on line 10** says "In this series of post" — should be "**posts**".

## Code Verification

- `application/config.py` (lines 128–144): correct; `TestingConfig` sets `TESTING = True` as required by Flask.
- `application/app.py` (lines 159–175): the application factory pattern is correctly implemented. Dynamic config-class lookup via `f"application.config.{config_name.capitalize()}Config"` works for `development`, `production`, `testing`. Would NOT work for multi-word configs like `dev_local` (would become `Dev_local`), but no such configs exist here.
- `wsgi.py` (lines 184–188): uses `os.environ["FLASK_CONFIG"]` (KeyError-on-missing). Fine because `manage.py` sets it.
- `manage.py` v1 (lines 241–293): the `setenv` helper preserves any pre-existing env value. `subprocess.Popen` + `wait()` + SIGINT handling is sound.
- `manage.py` v2 (lines 466–531): correctly extends to a `compose` subcommand. The two functions `flask` and `compose` are nearly identical, which the author acknowledges (line 533).
- `docker/development.yml` (lines 370–385): valid v3.4. `${PWD}` will work on Linux/macOS; Windows users may have issues — not addressed in the post (low priority).
- `Dockerfile` (lines 351–361): `FROM python:3` pins to the major version only; reproducibility-wise, a pinned tag (`python:3.11-slim`) would be better. Tutorial-acceptable.
- The output on lines 196–203 shows `Environment: production` despite the user running `FLASK_CONFIG="development" flask run`. The post correctly explains this on line 205: it's because `FLASK_ENV` hasn't been set yet. ✓

No code bugs spotted in part 1.

## Suggestions for Improvement

1. **Add a short "What's next" pointer at the end of each step.** A one-line pointer like "(we'll wire the database in the next post)" near the Docker section would smooth the bridge to Part 2.
2. **Standardise `[@source]` language tags.** Mixed use of `sh`, `bash`, and bare `[@source]` for shell snippets. Pick one.
3. **Standardise the "Hello, World!" / "Hello, world!" capitalisation** (line 172 vs line 177).
4. **The "Updates" section** only lists the 2020-12-22 review. If you've made further passes since, worth appending them.
5. **Consider adding a brief introduction paragraph after the title, similar to parts 2 and 3** (which both open with "In this series of posts I explore the development of a Flask project…"). Part 1 jumps straight into rationale.
6. **Line 549** asks readers to share — consider ending with a slightly more specific call to action (e.g. linking to the next post explicitly).

# Flask project setup: TDD, Docker, Postgres and more - Part 2

Source: pelican/content/flask-project-setup-tdd-docker-postgres-and-more-part-2.mau

## Top Priority — Major Issues

| # | Location (line) | Issue | Suggested fix |
|---|-----------------|-------|---------------|
| 1 | 57 | "remember that `POSTGRESQL_DB`" — this variable name does not exist; the variable is `POSTGRES_DB`. | Change `POSTGRESQL_DB` → `POSTGRES_DB`. |
| 2 | 84 / 34 | Line 34 has callout marker `:1:` placed inside `${POSTGRES_DB}` (`POSTGRES_DB: ${POSTGRES_DB}:1:`) but no callout `1` is referenced in the prose — the prose only mentions callouts `2` and `3` for the volume. The orphan callout will render as a stray "1" badge. | Remove the `:1:` marker on line 34, or add prose referring to it. |
| 3 | 102 | "the first time you run the command `compose -d`" — should be `compose up -d` (the command is missing `up`). | Change to `compose up -d`. |
| 4 | 240 | "(unless we use other network modes, which is not ideal)" — sentence reads oddly; "is" should agree with "modes". | "(unless we use other network modes, which is not the ideal solution)" or "...which are not ideal". |
| 5 | 326 | "flask-migrate, that adds some commands" — non-restrictive clause needs `which`, not `that`. | "flask-migrate, which adds some commands". |
| 6 | 330 | "While this doesn't make impossible to work" — ungrammatical. | "While this doesn't make it impossible to work". |
| 7 | 397 | "And, when we will start creating models we will use" — future-tense "when we will" is non-idiomatic in British English. | "And, when we start creating models, we will use". |
| 8 | 419 | "This approach has one big advantage, which is that it requires no previous setup and can this be executed on infrastructure created on the fly." — word order is broken ("can this be executed"). | "...and can therefore be executed on infrastructure created on the fly." |
| 9 | 421 | "Another advantage of this setup is it that we might need other things during the test" — "is it that" is wrong. | "Another advantage of this setup is that we might need other things during the test". |
| 10 | 439 | "make sure you terminate all the running containers running `./manage.py compose down`" — repeated "running". | "...terminate all the running containers by running `./manage.py compose down`". |
| 11 | 556 | "(both file have not been created yet)" — agreement error. | "(both files have not been created yet)". |
| 12 | 597 | "leave Docker pick a random host port" — should be "let Docker pick". | "let Docker pick a random host port". |
| 13 | 650 | "At the moment, however there are no tests" — missing comma. | "At the moment, however, there are no tests". |
| 14 | 650 | "stops and remove the container" — verb agreement. | "stops and removes the container". |
| 15 | 665 | "running sql commands directly on the the database" — duplicated "the". Also "sql" should be "SQL". | "running SQL commands directly on the database". |
| 16 | 812 | "I took the opportunity to write the command `create_initial_db` as well, that just runs..." — "that" → "which" (non-restrictive). | "...as well, which just runs the very same SQL command...". |
| 17 | 814 | "when I will add the scenarios" — same future-tense issue as #7. | "when I add the scenarios". |
| 18 | 1093, 1094, 1103, 1118 | Pytest output shows the path `tests/tests/test_user.py` (note the duplicated `tests/tests/`) — but earlier you say the file is `tests/test_user.py` (line 1060). Internally inconsistent. | Reconcile the path — most likely the output should read `tests/test_user.py`. |
| 19 | 1179 | "Please not that this is a very simple example" — typo: should be "Please note". | "Please note that this is a very simple example". |
| 20 | 1242 | "an hex version of a UUID ,so" — stray space before the comma, and "an hex" should be "a hex". | "a hex version of a UUID, so". |
| 21 | 1275 | "the database `application` configured with `APPLICATION_DB` has beed created" — typo: "beed" → "been". | "...has been created". |
| 22 | 1316 | "[link]("http://localhost:5000/users")" — the `link` macro requires a URL plus optional text. | `[link](http://localhost:5000/users, "http://localhost:5000/users")` or just `[link](http://localhost:5000/users)`. |
| 23 | 1324 | "with whoever you thing might be interested" — typo: "thing" → "think". | "...with whoever you think might be interested". |

## Medium Priority — Should Fix

| # | Location | Issue | Suggested fix |
|---|----------|-------|---------------|
| 1 | 18 | "production-ready database" — inside the post the database is run with all-default credentials and a default `postgres` superuser; calling that "production-ready" is overstated. | "...a database alongside your code in a Docker container..." (drop "production-ready"). |
| 2 | 22 | "To do this I need to add a service in the docker-compose configuration file" — sentence ends without a full stop. | Add a trailing period. |
| 3 | 100 | "Generally we want the database to be accessible by utility scripts, so we want to record the host name where the database is running." — duplicated "we want". | "Generally the database should be accessible by utility scripts, so we record the host name where it is running." |
| 4 | 244 | "the application, which is running in a container, cannot access the host through that address" — accurate, but `localhost` from within a container does not reach the host on Linux without `host.docker.internal` or `--network=host`. | Optional clarification, naming `host.docker.internal`. |
| 5 | 270 | "(3 rows)" then `postgres=#` with trailing space — trailing whitespace on line 272 may show up oddly in some renderers. | Trim trailing whitespace inside the source block. |
| 6 | 412 | "I want to use a TDD approach as much as possible when developing my applications, so I need to setup a good testing environment upfront" — "setup" is a noun; the verb is "set up". | "...I need to set up a good testing environment upfront". |
| 7 | 423 | The leap from "I like pytest's `-k`" to "so I want to map the management command line" is abrupt. | A bridging clause: "...by pattern-matching their name, so the management command must forward arbitrary arguments to pytest." |
| 8 | 535 | The polling loop sleeps 0.1 s between checks but there is no upper bound — if the container fails to start you loop forever. | Add a sentence: "In a more robust setup we would add a timeout." |
| 9 | 565 | `FLASK_ENV` is set to `production` in `config/testing.json`. Intentional, but the post never explains why — readers may suspect a mistake. | Add a one-line note: "I set `FLASK_ENV` to `production` so that Flask doesn't enable the debugger or auto-reloader during tests." |
| 10 | 597 | "I will come back to this problem when setting up the scenarios." — Verified: part 3 does cover this. | No change needed; just confirming. |
| 11 | 663 | "When you develop a web application and then run it in production, you typically create the database once and then upgrade it through migrations." — slightly verbose. | Optional tightening. |
| 12 | 1054 | "Bonus step - A full TDD example" — the section title says "TDD example" but lines 1127-1128 explicitly say "I won't show here all the steps of the strict TDD methodology, and implement directly the final solution". Contradicts the title. | Either rename to "A full development example" or actually show the red-green-refactor cycle. |
| 13 | 1242 | "the name comes from the commit message" — the `-m` argument is the migration message, not a Git commit message. | "...the name comes from the migration message you passed to `-m`". |
| 14 | 1316 | "After this we can safely commit my code" — pronoun shift ("we" / "my"). | "After this I can safely commit the code". |
| 15 | 1324 | "I hope this post already showed you why a good setup can make the difference." — "make the difference" is not idiomatic; "make a difference" is. | "...why a good setup can make a difference." |

## Low Priority — Nice to Have

| # | Location | Issue | Suggested fix |
|---|----------|-------|---------------|
| 1 | 12 | The opening blurb is identical across parts 1, 2 and 3. | Optional. |
| 2 | 57 | "is the database that gets created by default when you create the image" — strictly speaking it's created when the container first starts, not when the image is built/pulled. | "...when the container is first started". |
| 3 | 59 | "the content of the database is not lost when we tear down the container" — "content" → "contents". | "...the contents of the database are not lost...". |
| 4 | 98 | "[link](https://aws.amazon.com/secrets-manager/,"AWS Secrets Manager") for example can directly map secrets" — needs commas around "for example". | "AWS Secrets Manager, for example, can directly map secrets". |
| 5 | 111 | "the requirement goes among the production ones" — "goes among" is unusual phrasing. | "the requirement belongs with the production ones". |
| 6 | 343 | "If you get an error message from `pip` please check the documentation of your operating system to find out what to do to install the required packages." — comma after "pip". | "If you get an error message from `pip`, please check...". |
| 7 | 425-435 | The `pytest-cov` package is added but nothing in the post explains the difference between `coverage` and `pytest-cov`, nor why both are needed. | Optional: drop `coverage` from the explicit list, or add a sentence explaining why both are listed. |
| 8 | 597 | "you can't have two different containers using the same port on the host" — true, but only because both Compose files publish the same host port. | "you can't publish the same host port from two different containers". |
| 9 | 419 | "the book" link to leanpub — consider naming the book ("Clean Architectures in Python") in the prose. | "...read [link](...,"Clean Architectures in Python") that I wrote on the subject." |
| 10 | 658 | The pytest link text is `pytest` (unquoted), and the second link text "Useful pytest command line options" is also unquoted. Mau allows this but other entries in the same series quote the text. | Quote the text for consistency. |
| 11 | 1326-1330 | "Updates" section uses two different date styles and lists changes without bullet markers. | Optional consistency check across the series. |

## Internal Consistency Notes

- **Cross-reference to part 1 (line 16)**: matches the actual content of part 1 (static config + management script wrapping `flask` and `docker-compose`).
- **Cross-reference to part 3 ("scenarios", lines 597 and 1324)**: matches part 3 (which opens with `Step 1 - Creating scenarios` and discusses random ports).
- **Forward promise on line 397 ("You will find a complete example at the end of this post")**: delivered by the "Bonus step" section.
- **Promise on line 1324 ("how to easily create scenarios where you can test queries with only specific data")**: delivered in part 3.
- **Internal contradiction**: Section header "Bonus step - A full TDD example" vs. line 1127 "I won't show here all the steps of the strict TDD methodology".
- **Internal contradiction (path)**: `tests/test_user.py` in prose vs. `tests/tests/test_user.py` in pytest output.
- **Callout numbering**: Line 34 has an orphan `:1:` callout marker.

## Code Verification

- The two successive `manage.py` listings in Step 4 are consistent: the second is the refactored form of the first, and the refactor commentary accurately describes the diff.
- The final `manage.py` in this post matches the starting `manage.py` shown in part 3, which confirms inter-part code continuity.
- `psycopg2.errors.DuplicateDatabase` (line 777) is a real `psycopg2` exception class (since psycopg2 2.8). Fine.
- `conn.set_isolation_level(ISOLATION_LEVEL_AUTOCOMMIT)` (line 762) is correct and required for `CREATE DATABASE` (which can't run inside a transaction).
- The `db.drop_all()` / `db.create_all()` fixture (lines 1031-1037) is fine for this post but slow. The fixture's `scope="function"` is the default; explicit declaration is harmless.

# Flask project setup: TDD, Docker, Postgres and more - Part 3

Source: pelican/content/flask-project-setup-tdd-docker-postgres-and-more-part-3.mau

## Top Priority — Major Issues

| # | Location | Issue | Suggested fix |
|---|----------|-------|---------------|
| 1 | Line 520 | "many important companies *begun* many years ago" — wrong tense; past tense of "begin" is "began", not "begun". | Change to "many important companies *began* many years ago". |
| 2 | Line 657 | "increase the *performances* of some database queries" / similar elsewhere — "performance" in the sense of speed/efficiency is uncountable in British English. | Change to "*performance*". |
| 3 | Line 28 | "increase the *performances* of some database queries" — same issue as above. | Change to "*performance*". |
| 4 | Line 657 | "Web Server in front of your HTTP server" — capitalisation inconsistency, and double space before "with Docker Compose". | "web server in front of your HTTP server, and this might be easily implemented with Docker Compose." (lowercase, single space). |
| 5 | Line 412 | "alongside *with* the scenario" — "alongside" already implies "with"; "alongside with" is non-idiomatic. | Change to "happily running alongside the scenario". |
| 6 | Line 30 | "This is offered *me* by the Docker networking model" — awkward word order. | "This is offered to me by the Docker networking model" or "The Docker networking model offers a solution". |
| 7 | Line 875 | "whoever you *thing* might be interested" — typo for "think". | Change to "whoever you *think* might be interested". (Same typo also appears in parts 1 and 2 — worth a global fix.) |
| 8 | Line 18 | "I will also show you how to define a configuration for production and give some hints for the deployment." Then later (line 861) the section is titled "How to create the production infrastructure". The promise is met, but the body explicitly says "This will be a very short section… I want to just give some hints to stimulate your research." | Reword the intro to set realistic expectations: "give *a few pointers* for the deployment". |

## Medium Priority — Should Fix

| # | Location | Issue | Suggested fix |
|---|----------|-------|---------------|
| 1 | Line 28 | "as soon *I* do it for the third time" — missing "as". | "as soon *as* I do it for the third time". |
| 2 | Line 28 | "I make myself a rule" — sounds slightly odd. | "I make it a rule for myself" or "I have a rule for myself". |
| 3 | Line 32 | "Docker interface is not extremely script-friendly" — consider "the Docker CLI is not particularly script-friendly". | Soften to "particularly" or "very". |
| 4 | Line 282 | "to extract the port of the container with some very simple Python string processing… and to initialise the correct environment variable." Mixed infinitive style. | Optional smoothing: "extract the port… and initialise the correct environment variable". |
| 5 | Line 322 | "(mapping it to a random port as well to avoid clashing with the development one)" — "as well" is redundant with "also" earlier. | Remove "as well" or rephrase. |
| 6 | Line 361 | Trailing whitespace after "testing." | Strip trailing space. |
| 7 | Line 477 | "agnostic as possible *in* the scenarios" — "with the scenarios" reads more naturally. | "as agnostic as possible *with* the scenarios". |
| 8 | Line 516 | "As I stated at the very beginning of this *mini series* of posts" — should be hyphenated: "mini-series". | "this *mini-series* of posts". |
| 9 | Line 518 | "In a real production scenario Postgres would probably run in a separate instance" — "*on* a separate instance" is more idiomatic for infrastructure. | "*on* a separate instance". |
| 10 | Line 520 | "If our notebook *was* connected 24/7" — subjunctive in BrE: "*were* connected". | "If our notebook *were* connected 24/7". |
| 11 | Line 524 | "the first message that server prints" — missing article. | "the first message that *the* server prints". |
| 12 | Line 638 | "Remember that `FLASK_ENV` changes the internal settings of Flask, most notably disabling the debugger". The Flask documentation has since deprecated `FLASK_ENV` (Flask 2.3+ removed it); modern Flask uses `--debug` and `FLASK_DEBUG`. | Add a short note about Flask version, or update if you intend to keep this current. |
| 13 | Line 713–732 | Trailing whitespace at end of several lines (the lines containing only ` `). Also tabs/spaces in the nginx.conf snippet — looks like leading single-space indent on blank lines. | Strip trailing whitespace from blank lines. |
| 14 | Line 735 | "as it makes it impossible" — "makes it impossible" is fine; the "it… it" is acceptable but a little repetitive. | Optional: "as it prevents scaling up and down to serve more load". |
| 15 | Line 657 | "in AWS you might run the container in AWS Fargate and register *them* in an Application Load Balancer" — "the container" (singular) followed by "them" (plural). | "run *the containers* in AWS Fargate and register them" (or singular throughout). |
| 16 | Line 735 | "I won't run the nginx container as root on my notebook" — strictly Nginx is the proper noun for the project; "nginx" lowercase for the binary/image. The post mixes "Nginx" and "nginx" (also "nginx container"). | Standardise: "Nginx" as the product name, `nginx` (verbatim/code) when referring to the image/binary. |
| 17 | Line 793 | "but it doesn't come *preinstalled*" — usually hyphenated in BrE: "pre-installed". | "pre-installed". |
| 18 | Line 828 | "If I scale down (from outside the container)" — fine, but the example shows scaling to 1, which is a scale-down only relative to the previous "scale=3" (which was set in Step 3, not in this section). | Clarify: "If I scale down to a single container (we previously scaled up to three)". |
| 19 | Line 865 | "the whole structure can map almost 1 to 1 to the docker-compose setup" — "one-to-one" is the standard form in prose. | "almost one-to-one". |
| 20 | Line 867 | "version 0.13 will finally allow us to run for loops on modules" — this is dated (Terraform is now well past 1.x). | Add a note about the date, or rewrite. |
| 21 | Line 869 | "you need to *setup* a good Continuous Integration system" — "setup" is a noun; verb is "set up". | "you need to *set up* a good Continuous Integration system". |
| 22 | Line 869 | "open source CI" — usually hyphenated when used as an adjective: "open-source CI". | "open-source CI". |
| 23 | Line 869 | "very well known" — hyphenate as compound adjective: "very well-known". | "a very well-known open-source CI". |
| 24 | Line 869 | "automate the *deploy* of a Jenkins server" / Line 871 "your *deploy* pipeline" — "deploy" is a verb; the noun is "deployment". | "automate the *deployment*" / "your *deployment* pipeline". |
| 25 | Line 871 | "you need to do much more than just creating the image and running it" — mixed verb forms after "than just". | "much more than just create the image and run it". |
| 26 | Line 871 | "you will also need to compile and install the JavaScript assets" | Optional broaden to "front-end assets". |
| 27 | Line 871 | "Since you don't want to have downtime when you deploy you will need to look into blue/green deployments" — comma after "deploy" would help readability. | Add comma. |
| 28 | Line 875 | "if you find my posts useful please share them" — comma after "useful". | "if you find my posts useful, please share them". |
| 29 | Line 879 | The Updates section only mentions a 2020-12-22 review. The `pelican.modified` date at the top is `2021-02-23`. Inconsistency between metadata and changelog. | Add a 2021-02-23 entry to the Updates list, or align the dates. |

## Low Priority — Nice to Have

| # | Location | Issue | Suggested fix |
|---|----------|-------|---------------|
| 1 | Line 16 | "In the … first and … second posts I created" — consider "In the first and second posts of this series, I created" for clarity. | Optional rewording. |
| 2 | Line 18 | "create scenarios, that is databases created on the fly with custom data" — a colon or em-dash would read more cleanly than "that is". | "create scenarios — that is, databases created on the fly with custom data". |
| 3 | Line 22 | "Sometimes you need to investigate specific use cases for bugs" — clearer as "investigate specific use cases when hunting bugs" or "investigate bugs in specific use cases". | Optional rewording. |
| 4 | Line 24 | "while this gives us a realistic case where to test queries" — "where to test" is awkward. | "a realistic case in which to test queries". |
| 5 | Line 24 | "Whoever learned how joins work in relational databases understands what I mean here." — informal but slightly off. | "Anyone who has learned how joins work in relational databases will understand what I mean here." |
| 6 | Line 26 | "things are not much more complicated, but there are a couple of minor issues" — slight redundancy. | Optional trim. |
| 7 | Line 30 | "absolutely the same as using port 5432" — "absolutely" is a strong intensifier. | "essentially the same as using port 5432". |
| 8 | Line 280 | "(that I still have to create)" — minor: "(which I still have to create)" reads slightly more formally. | Optional. |
| 9 | Line 284 | "Last, I import and execute" — a bit terse. | "Lastly, I import and execute". |
| 10 | Line 286 | "The function `down` simply tears down" — "simply" is a filler adverb. | Drop "simply". |
| 11 | Line 437 | "have a look at the two files… They are just copies… but I think seeing them there might help" — bit casual. | Optional. |
| 12 | Line 477 | The distinction between `"development"` Flask configuration and `config/development.json` is well-explained. | — |
| 13 | Line 524 | "Got it, Flask!" — fits the informal tone, fine. | Keep. |
| 14 | Line 636 | "this is a simulation of production" — "this is just a simulation of production" reads better. | Add "just". |
| 15 | Line 703 | "I added a service `nginx` that runs the default Nginx image" — naming/casing again. | Standardise. |
| 16 | Line 735 | "leverages docker-compose networking" — "leverages" is overused in tech writing; "uses" is cleaner. | Optional: "uses Docker Compose networking". |
| 17 | Line 784 | "let's open the grimoire and reveal (some of) the dark secrets of Docker networking" — nice playful touch. | Keep. |
| 18 | Line 863 | "stimulate your research" — works, but "to spark further reading/research" is a touch more idiomatic. | Optional. |
| 19 | Line 871 | "for short periods of time. Or for longer periods, if you want to perform A/B testing or zonal deployments." — sentence fragment after the full stop. | "…for short periods of time — or longer, for A/B testing or zonal deployments." |

## Internal Consistency Notes

1. **Backwards references — all check out.**
   - Line 16 references parts 1 and 2 correctly using `{filename}` macros.
   - Line 28 references the development-on-5432 / testing-on-5433 setup, which matches part 2 (`POSTGRES_PORT` set to 5433 for testing).
   - Line 597 of part 2 explicitly forward-references this post: *"…I will come back to this problem when setting up the scenarios."* Step 1 of part 3 fulfils this promise.
   - The migration model `User` (part 2) is correctly used in `scenarios/users.py` (part 3, lines 449–474).

2. **Forward references in part 3.** No major broken promises. The intro on line 18 promises "how to easily create scenarios" (delivered in Step 1), "configuration for production" (Step 2), and "hints for the deployment" (the "How to create the production infrastructure" section). All three are present.

3. **Docker Compose project naming.** Throughout part 3, container names are `scenario_foo_db_1`, `scenario_foo_web_1`, `production_db_1`, etc. This matches the `-p config` convention introduced in part 2. Consistent.

4. **Updates section vs `pelican.modified`.** The `pelican.modified` date is 2021-02-23, but Updates only lists 2020-12-22. (Same inconsistency in part 2.)

5. **Scenario down command.** Line 437 instructs the reader to run `./manage.py scenario down foo`, which matches the `down` function in `manage.py` (lines 260–273). Consistent.

6. **`config/scenario.json`.** Line 326 shows the file containing `POSTGRES_HOSTNAME: localhost`, but at scenario-up time the `up` command (line 235) overrides `POSTGRES_PORT` with the dynamic Docker port. Consider noting this so readers don't wonder why `localhost` works when Docker is involved.

7. **`docker/scenario.yml` ports.** Line 319 maps the `web` service port `5000` without a host mapping (`- "5000"`), so Docker assigns a random host port. Consistent with the `docker ps` output on line 406 (`0.0.0.0:32826->5000/tcp`).

8. **Unused database in scenario flow.** In `manage.py` line 237, `run_sql` is used to create `APPLICATION_DB`. Works correctly.

## Code Verification

1. **`manage.py` `scenario up` (line 206 onwards):** The function reassigns `os.environ["APPLICATION_CONFIG"]`, then duplicates the call (line 207 sets it, line 208 reads it). The pattern is correct but slightly redundant. Not a bug.

2. **Line 222:** `shutil.copy(docker_compose_file("scenario"), scenario_docker_file)` could just use `shutil.copy(scenario_docker_source_file, scenario_docker_file)` for consistency with line 215. Minor stylistic inconsistency.

3. **Line 234:** `out.decode("utf-8").replace("\n", "").split(":")[1]` assumes a single mapping line in the output of `docker-compose port db 5432`. If the output has multiple lines (e.g., IPv4 and IPv6) this could fail. Worth a defensive note.

4. **Line 246:** `scenario = importlib.import_module(scenario_module)` shadows the outer Click group `scenario` defined on line 200. Works because the outer name is no longer needed inside this function, but it's bad practice. Consider renaming the local to `scenario_mod`.

5. **`docker/scenario.yml` (line 303):** `"5432"` (without a host mapping) is correct for letting Docker assign a random host port. Good.

6. **`docker/production.yml` (lines 538–574, then again 661–701):** The second copy (Step 3) adds the `nginx` service. The diff is shown via `:@:` highlight markers, which is consistent with the rest of the series. Good.

7. **Nginx config (lines 707–733):** `upstream app { server web:8000; }` is correct because Gunicorn binds to `0.0.0.0` on the default port 8000. The proxy_pass directive uses `http://app;` which matches the upstream name. All correct.

8. **Line 870–871 prose:** "version 0.13 will finally allow us to run for loops on modules" — Terraform 0.13 was released in August 2020. Now very dated.

## TODO Markers

No `TODO` markers were found in the file.
