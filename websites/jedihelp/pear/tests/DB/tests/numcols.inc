<?php

$dbh->setErrorHandling(PEAR_ERROR_DIE);
$sth = $dbh->query("SELECT a FROM phptest");
printf("%d\n", $sth->numCols());
$sth = $dbh->query("SELECT a,b FROM phptest");
printf("%d\n", $sth->numCols());
$sth = $dbh->query("SELECT a,b,c FROM phptest");
printf("%d\n", $sth->numCols());
$sth = $dbh->query("SELECT * FROM phptest");
printf("%d\n", $sth->numCols());

?>
