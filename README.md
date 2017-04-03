mod_link_check
========================

A Zotonic module for checking links within your content.

This module is part of [Ginger](http://github.com/driebit/ginger).

Features:

* Detect links and verify their current HTTP status

Configuration
-------------

These are the available configuration options you can set in the Zotonic configuration screen.

| Key            | Unit          | Description                              |
| ---------------|:-------------:| ----------------------------------------:|
| check_interval | milliseconds  | How often to initiate a check            |
| check_age      | milliseconds  | How long ago was last check before retry |
| ignore_internal| boolean       | Should internal links be ignored         |

Usage
-----

Currently there is only a single admin screen under Modules -> External Link Checker with a direct table display of the current data.

Status
-----

Very much a work in progress especially when it comes to the user interface.
