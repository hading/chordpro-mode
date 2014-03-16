# Chordpro mode #

## Introduction ##

This is an emacs major mode, derived from text-mode, for editing files
in the Chordpro format (see, for example,
<http://www.vromans.org/johan/projects/Chordii/>).

It is currently somewhat crude, but it does simplify a few things and
I do intend to keep adding any refinements that I find useful in my
work with Chordpro files. The font-lock is based off a previously
existing chordpro mode, but this provides more operations.

I'm still developing and experimenting with this, so as I go along I
might decide that different mouse or keybindings work better and will
probably provide more functionality as I see things that will make the
work that I do easier. There are some fairly obvious things left to do
and I'll add them I need them. If you have any ideas feel free to
create an issue (or even better do it and send a pull request).

## Installation ##

Copy chordpro-mode.el into somewhere in your emacs load path (I put it
in ~/.emacs.d).

Then put something like this into your .emacs file (I use the .pro
extension for such files):

    (setq auto-mode-alist (cons '("\\.pro$" . chordpro-mode) auto-mode-alist))
    (autoload 'chordpro-mode "chordpro-mode")

Now when you visit a .pro file you should automatically get chordpro-mode.

## Use ##

### Keyboard ###

All of the keyboard commands use the Ctrl-c prefix.

* Ctrl-c i : Insert a chord at the point. You'll be prompted for the
  chord name in the minibuffer. The brackets will automatically be
  inserted, space trimmed, and the chord capitalized.
* Ctrl-c w : Kills the current chord. The current chord is one
  containing the point - because of the way emacs works this means
  that this command doesn't do what you want if the cursor is on the
  opening [ of a chord, only if it is between that and the closing ],
  inclusive. But there's another command for that.
* Ctrl-c z : Kills the next chord. Finds the next chord after the
  point and kills it. This one works if you are on the opening
  [, or if you are between chords.
* Ctrl-c c : Copy the current chord
* Ctrl-c x : Copy the next chord
* Ctrl-c h : Insert a chordpro comment
* Ctrl-c h : Insert a chordpro chorus
* Ctrl-c t : Insert a chordpro title
* Ctrl-c s : Insert a chordpro subtitle

### Mouse ###

Some of the commands can be invoked with the mouse. I still haven't
decided what I think the best way to go is, but these are some starting
experiments. All of them have corresponding keyboard commands (whether
specific to this mode or standard emacs commands).

* Ctrl-mouse-1 : Kills current chord
* Ctrl-mouse-2 : Yanks last kill (this is prone to change, as you can typically do this with a normal mouse-2)
* Ctrl-mouse-3 : Kills next chord
* Shift-mouse-1 : Copies current chord
* Shift-mouse-2 : Insert chord
* Shift-mouse-3 : Copies next chord


