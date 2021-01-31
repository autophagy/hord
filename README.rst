.. image:: hord.png
   :alt: hord sigil
   :align: center

====
Hord
====

hord
  noun: hoard, treasure, an accumulation of valuable things hidden away


Hord is a tool for dotfile management/symlink using `Dhall`_. The idea being
to write dotfiles as Dhall configuration files, and then provide a definition
for what format those files should be compiled to (raw, text, json or yaml)
as well as where their symlinks should point. **Very much still in development,
be careful when using it! 😱**

Installation
============

Can be installed with ``stack install``, which should give you the ``hord`` command.

Configuration
=============

Hord, when invoked, should be given a target folder containing a ``hord.dhall``
configuration file, as well as dotfiles. The ``hord.dhall`` file should have
the following structure:

.. code-block:: dhall

  { hord : List { src : Text, dest : Text } }

Hord will then build the ``src`` file to a ``_build`` directory depending
on the following rules:

* If the ``src`` file is not a ``.dhall`` file, just copy the file (``Raw``).

* If the ``src`` file is a ``dhall`` file:

  * If ``dest`` is a ``.yaml`` or ``.yml`` file, compile to Dhall code to YAML,
    similar to ``dhall-to-yaml``.

  * If ``dest`` is a ``.json`` file, compile the Dhall code to JSON, similar to
    ``dhall-to-json``.

  * Otherwise, compile the Dhall code to a ``Text`` value and write that to the
    build file, similar to ``dhall text``.

As an example:

.. code-block:: dhall

    { hord =
      [ { src = "example.dhall"
        , dest = "/home/user/.config/example"
        }
      , { src = "example2.dhall"
        , dest = "/home/user/.config/example2.yaml"
        }
      , { src = "example3.dhall"
        , dest = "/home/user/.config/example3.json"
        }
      , { src = "example4.txt"
        , dest = "/home/user/.config/example4.txt"
        }
      ]
    }

Values for ``src`` should be paths relative to ``hord.dhall``, whereas ``dest`` should
be the absolute path for where the symlink should end up.

.. _Dhall: https://dhall-lang.org/


