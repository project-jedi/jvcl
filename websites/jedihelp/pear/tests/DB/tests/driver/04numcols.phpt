--TEST--
DB_driver::numCols test
--SKIPIF--
<?php chdir(dirname(__FILE__)); include("skipif.inc"); ?>
--FILE--
<?php
include("mktable.inc");
include("../numcols.inc");
?>
--EXPECT--
1
2
3
4
