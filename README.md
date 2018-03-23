# actr-stap

ACTR-STAP is an open-source ACT-R LISP library that enables ACT-R models to connect to task software developed in any programming language, running locally or remotely. 

Most importantly, the task software employed for ACT-R simulations would be the same software that is employed in human experiments and/or simulations with other modeling frameworks.

ACTR-STAP enables millisecond-precision model-controlled timing, even for faster-than-real-time simulations, and STAP message log files may be played back and analyzed on equal footing with those of humans and other computational participants. 

<img src="https://raw.githubusercontent.com/vdv7/stap/master/pres/stap-icon.png" width=250 align=right>
STAP (Simple Task-Actor Protocol) is a machine-readable format for specifying user-interface changes. 
STAP promotes model re-use across tasks and task re-use across models (including non ACT-R models).
STAP tasks may be developed in any language, and are rendered for human consumption via the a web template (e.g. stapjs; https://github.com/vdv7/stapjs).

More details on STAP may be found at https://github.com/vdv7/stap

Contents:

* actr-stap.lisp -- actr-stap library.
** should be loaded after ACT-R loads (tested with ACT-R v7)
** requires Quicklisp (https://www.quicklisp.org) to load additional LISP libraries (usocket, stjson, bordeaux-threads)

* button-clicker.lisp -- sample dumb model that looks for buttons on screen and clicks them
** try the following to get started: start the STAP task on localhost:9000, load ACT-R, load your model, then run:
         (run-tcp-task :host "localhost" :port 9000)
