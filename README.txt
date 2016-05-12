*** Changes from DWSIM 3 code ***

Completed:

- Reorganized, cleaned code
- Isolated Flowsheet Drawing in a separate module
- Isolated Unit Operations
- Isolated Property Packages
- Internationalized code language (english class names, etc)
- Removed binary file support

- Flash algorithms are now configurable and separated from the property packages. Every flowsheet object can be associated with a different flash algo.
- New panel for inserting objects.
- Implemented object resizing directly on the flowsheet.
- New object appearance editing mode.
- Reworked property tables.

Work in progress:

- Object property editors are being rewritten from scratch. Currently done: Material Stream and Mixer.
- Utilities are now associated with flowsheet objects. They can be updated automatically with the owner object.

To do:

- Add an ironpython-based property package.
- Build a framework for adding user-defined Unit Operations and Property Packages.
- Implement an auto-update solution.


