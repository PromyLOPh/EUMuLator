EUMuLator
=========

EUMuLator is a reimplementation of the EUMEL0 machine in PureScript_.
Currently it is compatible with the EUMEL0 BIT-A-encoded instruction set as of
version 1.8. For an introduction to the EUMEL operating system, see `this
page <https://6xq.net/eumel/>`__.

.. Yes, this is my first PureScript project.

.. _PureScript: http://www.purescript.org/
.. _EUMuLator: https://github.com/promyloph/eumulator

Project status
--------------

Works:

- Disassembler

Work-in-progress:

- EUMEL0 virtual machine implementation

Future work:

- Browser intergation

Usage
-----

First `install PureScript`_. Then you’ll need a *Hintergrund* from EUMEL
version 1.8. Currently only `the base disk set’s Hintergrund version 1.8.7
<public/disks/grundpaket.zip>`__ is extractable by the script
``extractHintergrund.py``, found in the `tools repository`_. Extract the
dataspaces and run the disassembler:

.. _tools repository: https://github.com/PromyLOPh/eumel-tools
.. _install PureScript: https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md

.. code:: bash

   linearizeDisk.py 03_eumel0.img 03_eumel0.img.linear
   extractHintergrund.py 03_eumel0.img.linear
   pulp run -- 0002_0004.ds

Note that running the disassembler makes sense only for dataspace four, the
default dataspace, which contains each task’s code section.

