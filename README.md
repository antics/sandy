sandy - simple ncurses text editor
==================================
sandy is a simple ncurses text editor.

Modyfications by antics
-----------------------
tabstop = 3
HIGHLIGHT_CURRENT = 0

Requirements
------------
In order to build sandy you need the ncurses header files.


Installation
------------
Edit config.mk to match your local setup (sandy is installed into the
/usr/local namespace by default). Optionally, create a config.h file to 
further configure the editor at compile time. An examples file is provided as 
config.def.h.

Afterwards enter the following command to build and install sandy (use root if
needed):

    make clean install


Running sandy
-------------
Use the following syntax:

	sandy [-r] [-S | -s SYNTAX] [-t TABSTOP] [File]

Where:
-a starts with autoindent
-r opens the file read-only
-S use no syntax colors at all.
-s SYNTAX  lets you specify the syntax colors for this file
-t TABSTOP sets the tabstop for this instance of sandy


Name
----
In case you are wondering, sandy was the smartest pet ferret. She died of
cancer though. We were sad and started coding this editor.

