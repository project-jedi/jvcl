--TEST--
DB_driver::sequences
--SKIPIF--
<?php chdir(dirname(__FILE__)); require "skipif.inc"; ?>
--FILE--
<?php
require "connect.inc";
require "../sequences.inc";
?>
--EXPECT--
DB Error: no such table
DB Error: no such table <- good error catched
a=1
b=2
b-a=1
c=1
d=1
e=1
