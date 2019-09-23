# scala-mode2 — A new scala-mode for emacs

This is a new scala major mode for emacs 24. It is a complete rewrite
based on the *Scala Language Specification 2.9*.

The mode intends to provide the basic emacs support, including
- indenting of code, comments and multi-line strings
- motion commands
- highlighting

Currently the indenting of code has been finalized. Highlighting is
under work. No scala specific motion commands have been added, but
standard emacs motions work ofcourse.

## Setting the mode up for use

1. Make sure you have the latest version of **GNU Emacs** installed.
The mode has been developed on 24.2 and uses features not available
in emacs prior to version 24.

2. There are two mechanisms that can be used for the installation of
the mode into Emacs. The preferred manner is to use the built-in
package manager of Emacs 24 (i.e. `package.el`) and
the other is to manually clone the git repository, add the path to the mode
to the load-path and then to require it. For more information regarding
`package.el` please refer to the [EmacsWiki](http://emacswiki.org/emacs/ELPA).

    1. Package.el:
        Using the package.el within Emacs installation is the recommended
        manner to install scala-mode2 as it allows for continuous, easy
        updating from within Emacs itself. 
        
        Adding the MELPA repository to your emacs
        initialization will be required to locate the packages.
       
        Add the following to your emacs config (.emacs, init.el, etc), and
        if such a definition already exists, ensure that it contains
        the MELPA declaration, for example:

        ```lisp
        (require 'package)
        (add-to-list 'package-archives
                     '("melpa" . "http://melpa.milkbox.net/packages/") t)
        (package-initialize)
        (unless (package-installed-p 'scala-mode2)
          (package-refresh-contents) (package-install 'scala-mode2))
        ```
        
        or you could use ```customize``` to add a repository:
        
        ```
        M-x customize-variable [RET] package-archives
        ```
        
        and add MELPA, for example:
        
        ```
        melpa   http://melpa.milkbox.net/packages/
        ```
        
        and then use package install to install it:
        
        ```
        M-x package-install [RET] scala-mode2 [RET]
        ```
        

    2. Manual:
        Download the files to a local directory. You can use the *git clone*
        command, this will create a new directory called scala-mode2.

        ```
        git clone git://github.com/hvesalai/scala-mode2.git
        ```

        Include the following in your Emacs config file. If you have been
        using the old scala-mode, make sure it is no longer in *load-path*.

        ```lisp
        (add-to-list 'load-path "/path/to/scala-mode2/")
        (require 'scala-mode2)
        ```

3. That's it. Next you can start emacs and take a look at the
customization menu for scala-mode (use **M-x** *customize-mode* when
in scala-mode or use **M-x** *customize-variable* to customize one
variable). Also be sure to check the customization tips on various
keyboard commands and general emacs parameters which cannot be
modified from the scala-mode customization menu.

    For scala console (aka *REPL*) or **sbt** support, see
    [sbt-mode](https://github.com/hvesalai/sbt-mode).
    
    You might also be interested in these modules:
    - https://github.com/ancane/scala-outline-popup
    - https://github.com/ensime/ensime-server

## Indenting modes

*Where four developers meet, there are four opinions on how code should
be indented. Luckily scala-mode already supports 2^4 different ways of
indenting.*

### Run-on lines (scala-indent:default-run-on-strategy)

The indenting engine has three modes for handling run-on lines. The
**reluctant** (default) mode is geared toward a general style of coding
and the **eager** for strictly functional style. A third mode called
**operators** is between the two.

The difference between the modes is how they treat run-on lines. For
example, the *eager* mode will indent *map* in the following code

```scala
val x = List(1, 2, 3)
  map(x => x + 1)
```

The *operators* and *eager* modes will indent the second row in the
following code, as the first line ends with an operator character.

```scala
val x = 20 +
  21
```

The *reluctant* mode (default) will not indent the line in either
case. However, all three modes will indent the second line in these
examples as it is clear that the first line cannot terminate a statement
(see the *Scala Language Specification 2.9*, section 1.2).

```scala
val x = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).
  map (x => x + 1) // last token of previous line cannot terminate a statement

val y = (List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
           map (x => x + 1)) // inside 'newlines disabled' region
```

You can use empty lines in the *eager* mode to stop it from indenting a
line. For example

```scala
val x = foo("bar")
           ("zot", "kala") // indented as curry

val y = foo("bar")

("zot", "kala") // a tuple
```

However, in all three modes pressing the **tab** key repeatedly on a
line will toggle between the modes.

### Value expressions (scala-indent:indent-value-expression)

When this variable is set to *nil* (default), body of a value
expressions will be indented in the traditional way.

```scala
val x = try {
  some()
} catch {
  case e => other
} finally {
  clean-up()
}
```

However, when the variable is set to *t*, the body will be indented
one extra step to make the *val*, *var* or *def* stand out. For
example:

```scala
val x = try {
    some()
  } catch {
    case e => other
  } finally {
    clean-up()
  }
```

### Parameter lists (scala-indent:align-parameters)

When this variable is set to *nil* (default), parameters and run-on
lines in parameter lists will not align under or acording to the
first parameter.

```scala
val y = List( "Alpha", "Bravo",
  "Charlie" )

val x = equals(List(1,2,3) map (x =>
  x + 1))
```

When the variable is set to *t*, the same will be indented as:

```scala
val y = List( "Alpha", "Bravo",
              "Charlie" )

val x = equals(List(1,2,3) map (x =>
                 x + 1))
```

### Expression forms: if, for, try (scala-indent:align-forms)

When this variable is set to *nil* (default), *if*, *for* and *try*
forms are not aligned specially.

```scala
val x = if (kala)
  foo
else if (koira)
  bar
else
  zot

val x = try "1".toInt
catch { case e => 0}
finally { println("hello") }

val xs = for (i <- 1 to 10)
yield i
```

When the variable is set to *t*, the same will be indented as:

```scala
val x = if (kala)
          foo
        else if (koira)
          bar
        else
          zot

val x = try "1".toInt
        catch { case e => 0}
        finally { println("hello") }

val xs = for (i <- 1 to 10)
         yield i
```

## Indenting multi-line comments

The start of a multi-line comment is indented to the same level with
code.

By default, if a multi-line comment begins with `/**` it is considered
to be a Scaladoc comment. Scaladoc comments are indented according to
the Scaladoc style guide.

```
/** This is a Scaladoc comment.
  * 2nd line.
  */
```

Alternatively, if the configurable variable *scala-indent:use-javadoc-style*
is set to `t`, multi-line comments beginning with `/**` will be indented
according to the Javadoc style, wherein all following lines are indented
under the first asterisk.

```
/**
 * This is a Javadoc-style comment.
 * 2nd line.
 */
```

All other multi-line comments are indented under the first asterisk.

```
/****
 * Supercalifragilistic-
 * expialidocious!
 */

/*
 A comment
 */
```

Typing an asterisk in multi-line comment region, at the start of a
line, will trigger indent. Furthermore, if the configurable variable
*scala-indent:add-space-for-scaladoc-asterisk* is `t` (default) and the
asterisk was the last character on the line, a space will be inserted
after it. If you type a forward slash after the automatically inserted
space, the space is deleted again so that you can end the comment
without deleting the space manually.

## Filling (i.e. word wrap)

Paragraph *filling* (emacs jargon for word wrap) is supported for
comments and multi-line strings. Auto-fill is not supported yet.

To re-fill a paragraph, use the *fill-paragraph* command ( **M-q**
). As always, the column at which to wrap is controlled by the
*fill-column* variable, which you set it with the *set-fill-column*
command. To set the default, you use the *customize-variable* command
or a mode-hook.

## Joinin lines (delete indentation) and removing horizontal whitespace

Scala-mode defines its own *scala-indent:join-line* and
*scala-indent:fixup-whitespace* functions.

Unlike the normal *join-line* (aka *delete-indentation*),
*scala-indent:join-line* detects the comment fill-prefix and removes
it.

The *scala-indent:fixup-whitespace* first removes all horizontal
whitespace, then adds one space the context requires none to be
present (before semicolon, around dot, after '(' or '[', before ')' or
']', etc).

## Motion

Basic emacs motion will work as expected.

Text paragraph motion (i.e. *forward-paragraph*, *backward-paragraph*)
works inside comments and multi-line strings, and it respect Scaladoc's
wiki-style markup.

The commands *forward-sexp* and *backward-sexp* ( **M-C-f**, **M-C-b**
) motion commands will move over reserved words, literals, ids and
lists.

## Keymap and other commands

For the sake of customizability, scala-mode does not alter the default
emacs keymap beyond what is absolutely necessary. However, you can customize
the mode through the scala-mode-hook. Here are some suggested modification
you may want to try. Just copy-paste it to your `.emacs` file.

```lisp
(add-hook 'scala-mode-hook '(lambda ()

  ;; Bind the 'newline-and-indent' command to RET (aka 'enter'). This
  ;; is normally also available as C-j. The 'newline-and-indent'
  ;; command has the following functionality: 1) it removes trailing
  ;; whitespace from the current line, 2) it create a new line, and 3)
  ;; indents it.  An alternative is the
  ;; 'reindent-then-newline-and-indent' command.
  (local-set-key (kbd "RET") 'newline-and-indent)

  ;; Alternatively, bind the 'newline-and-indent' command and
  ;; 'scala-indent:insert-asterisk-on-multiline-comment' to RET in
  ;; order to get indentation and asterisk-insertion within multi-line
  ;; comments.
  ;; (local-set-key (kbd "RET") '(lambda ()
  ;;   (interactive)
  ;;   (newline-and-indent)
  ;;   (scala-indent:insert-asterisk-on-multiline-comment)))

  ;; Bind the backtab (shift tab) to
  ;; 'scala-indent:indent-with-reluctant-strategy command. This is usefull
  ;; when using the 'eager' mode by default and you want to "outdent" a
  ;; code line as a new statement.
  (local-set-key (kbd "<backtab>") 'scala-indent:indent-with-reluctant-strategy)

  ;; and other bindings here
))
```

## Whitespace

Emacs has a very nice minor mode for highlighting bad whitespace and
removing any unwanted whitespace when you save a file. To use it, add
this to your .emacs file.

```lisp
(add-hook 'scala-mode-hook '(lambda ()
  (require 'whitespace)

  ;; clean-up whitespace at save
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;; turn on highlight. To configure what is highlighted, customize
  ;; the *whitespace-style* variable. A sane set of things to
  ;; highlight is: face, tabs, trailing
  (whitespace-mode)
))
```

Since scala is indented with two spaces, scala-mode2 does not use tabs
at all for indents by default. If you want to turn on tabs mode, you
can do so in the mode hook (set *indent-tabs-mode* to t).

## Code highlighting

Highlighting code is still a work in progress. Feedback on how it
should work is welcomed as issues to this github project.

It may come as a surprise to some that Scaladoc comments (comments that
start with exactly `/** `) are highlighted in the same color as
strings. This is because Scaladoc comments get the font-lock-doc-face,
which is usually an alias for font-lock-string-face (a heritage from
lisp, the native language of emacs, where document comments are
strings). If this really bothers you, you may customize the face (use
**M-x** *customize-face*).

Free emacs tip: if you are using emacs from a text terminal with dark
background and you are having trouble with colors, try setting the
customization variable *frame-background-mode* to *dark* (use **M-x**
*customize-variable*).

### Highlighting of variable definitions
The highlighting of variable definitions, such as

```var test = "some mutable variable"```

now result in the variable name ("test" above) to be highlighted using the variable
scala-font-lock:var-face. Per default, the value of scala-font-lock:var-face
is 'font-lock-warning-face. You can always change the highlighting of vars
by changing scala-font-lock:var-face through the Emacs face customization
(use **M-x** *customize-face*).

Very complex scala files may need the following in your emacs init (.emacs, etc):

```lisp
;; For complex scala files
(setq max-lisp-eval-depth 50000)
(setq max-specpdl-size 5000)
```

## `beginning-of-defun` and `end-of-defun`

scala-mode2 defines `scala-syntax:beginning-of-definition` and
`scala-syntax:end-of-definition` which move the cursor forward and
backward over class, trait, object, def, val, var, and type definitions. These
functions are assigned to the buffer local variables
`beginning-of-defun-function` and `end-of-defun-function` which makes
it so that the `beginning-of-defun` and `end-of-defun` functions behave
in a way that is appropriate to scala. These functions are not currently able to
support some of the more advanced scala definition types.
Multiple assignment to variables e.g.

```scala
val a, b = (1, 2)
```

are among the assignment types that are not currently supported. In general
the only types of definition that these functions are likely to support
are ones that use only a single, simple (but possibly generic) identifier as 
its identifer.

## imenu

scala-mode2 supports imenu, a library for accessing locations in documents that
is included in emacs 24. The custom variable `scala-imenu:should-flatten-index`
controls whether or not the imenu index will be hierarchical or completely flat.
The current iMenu implementation only goes one level deep i.e. nested classes are
not traversed. scala-mode2's imenu support depends heavily on the
`scala-syntax:end-of-definition` and `scala-syntax:beginning-of-definition`
functions, and as such, it shares their limitations.

## Jump to the code

Some people find it useful to add the following function to their
`scala-mode-hook` as it will jump the package and import boilerplate
in most files:

```elisp
(scala-mode:goto-start-of-code)
```

## Auto parentheses formatting

We recommend using [smartparens](https://github.com/Fuco1/smartparens) to
automatically format parentheses on insertion, e.g. with

```elisp
  (sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
```

## Other features
- highlights only properly formatted string and character constants
- indenting a code line removes trailing whitespace

## Known issues

*do/while* is not always handled correctly. For example:

```scala
do
  foo
while (bar)
  foo
```

The indenter thinks the second occurrence of `foo` is the body of the while.
To work around this, terminate the while with a semicolon,
or put a blank line after it.

As mentioned above,
`scala-syntax:end-of-definition` `scala-syntax:beginning-of-definition`
don't properly handle any defintions that don't have a simple, single 
identifier.
Its likely that they will stumble when presented with some of the more advanced
and obscure scala definitions out there.

There also seems to be a strange bug with scala-modes2's `end-of-defun` integration
where two functions are skipped instead of just one. `scala-syntax:end-of-definition`
does not have this problem, so if you find this bug bothering you a lot
you can bind whatever you normally bind to `end-of-defun` to
`scala-syntax:end-of-definition` in scala mode to alleviate the issue.

## Future work

- syntax-begin-function for reliably fontifying elements which span
  multiple lines
- highlight headings and annotations inside Scaladoc specially (use
  underline for headings)
- highlight variables in string interpolation (scala 2.10)
- Improve `end-of-defun` and `beginning-of-defun`. In particular,
  figure out why `end-of-defun` sometimes skips defintions 
  even though `scala-syntax:end-of-definition` does not and add
  support for obscure types of val declarations.

All suggestions and especially pull requests are welcomed in github
https://github.com/hvesalai/scala-mode2

## Credits

Mode development: Heikki Vesalainen

Contributors and valuable feedback:
- Ray Racine
- Eiríkr Åsheim (aka Erik Osheim)
- Seth Tisue
- Gary Pamparà
- Evan Meagher
- Andrew Jones
- Vasya Novikov
- Hugh Giddens
- Nic Ferrier
- Tillmann Rendel
- Jim Powers
- Ivan Malison
- Sam Halliday
