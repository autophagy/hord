====
Hord
====

hord
  noun: hoard, treasure, an accumulation of valuable things hidden away


Hord is a tool for dotfile management/symlink using `Dhall`_. The idea being
to write dotfiles as Dhall configuration files, and then provide a definition
for what format those files should be compiled to (raw, text, json or yaml)
as well as where their symlinks should point. **Very much in toy development,
don't actually use this**.

Installation
============

Can be installed with ``stack install``, which should give you the ``hord`` command.

Configuration
=============

Hord, when invoked, should be given a target folder containing a ``hord.dhall``
configuration file, as well as dotfiles. The ``hord.dhall`` file should have
the following structure::

  { hord : List { src : Text, dest : Text, mode : < JSON | Raw | Text | YAML > } }

as an example::

    let Mode = < Raw | Text | YAML | JSON >

    in { hord = [ { src = "example.dhall"
          , dest = "/home/user/.config/example"
          , mode = Mode.Text
          }
          ,{ src = "example2.dhall"
          , dest = "/home/user/.config/example2.yaml"
          , mode = Mode.YAML
          }
          , { src = "example3.dhall"
          , dest = "/home/user/.config/example3.json"
          , mode = Mode.JSON
          }
        ] }

Values for ``src`` should be paths relative to ``hord.dhall``, whereas ``dest`` should
be the absolute path for where the symlink should end up.

.. _Dhall: https://dhall-lang.org/


