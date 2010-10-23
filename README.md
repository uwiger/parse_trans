Parse Transform Utility Library
===============================

This application contains some utility functions that 
make it easier to write maintainable parse transforms
for erlang. The library uses Syntax Tools, which may seem
unwieldy at first. However, I recommend getting acquainted 
with Syntax Tools; it has many advantages in for parse
transforms.

Documentation
-------------
The EDoc is generated using the EDown extension, in order 
to make it easy to read online on Github. To generate
normal edoc, update `rebar.config` to remove the edown-
related dependencies and edoc options.
