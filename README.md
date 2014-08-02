PPCA2014 Scheme Interpreter Test Suite
=========================================

This repository contains the test suite for the scheme interpreter project of PPCA2014.

* gen.cpp: a arithmetic random test case generator (by TongLiang Liao)
* no_side_effect.scm: the test for syntaxes and procedures without side effect
* side_effect.scm: the test for syntaxes and procedures with side effect
* deep_tree.scm: a very long expression to test your stack management
* test.sh: script to run all tests. The first time to run the script will take longer for it will generates random test cases and build answers. Set the name, command, with_side_effect variables properly before running it. The output will be compared against the output of plt-r5rs.
