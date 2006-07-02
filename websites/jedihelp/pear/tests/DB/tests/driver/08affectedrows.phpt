--TEST--
DB_driver::affectedRows test
--SKIPIF--
<?php chdir(dirname(__FILE__)); include("skipif.inc"); ?>
--FILE--
<?php
include("mktable.inc");
// Clean table
$dbh->query("DELETE FROM phptest");

// Affected rows by INSERT statement
$dbh->query("INSERT INTO phptest (a,b) VALUES(1, 'test')");
$dbh->query("INSERT INTO phptest (a,b) VALUES(2, 'test')");
printf("%d after insert\n", $dbh->affectedRows());

// Affected rows by SELECT statement
$dbh->query("SELECT * FROM phptest");
printf("%d after select\n", $dbh->affectedRows());
$dbh->query("DELETE FROM phptest WHERE b = 'test'");
printf("%d after delete\n", $dbh->affectedRows());

// Affected rows by DELETE statement
$dbh->query("INSERT INTO phptest (a,b) VALUES(1, 'test')");
$dbh->query("INSERT INTO phptest (a,b) VALUES(2, 'test')");
$dbh->setOption("optimize", "portability");
$dbh->query("DELETE FROM phptest");
printf("%d after delete all (optimize=%s)\n", $dbh->affectedRows(),
       $dbh->getOption("optimize"));
?>
--EXPECT--
1 after insert
0 after select
2 after delete
2 after delete all (optimize=portability)
