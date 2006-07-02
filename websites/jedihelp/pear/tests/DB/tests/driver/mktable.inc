<?php
require_once "./connect.inc";

$dbh->setErrorHandling(PEAR_ERROR_RETURN);
$dbh->query("DROP TABLE phptest");

//$dbh->setErrorHandling(PEAR_ERROR_TRIGGER);
$dbh->setErrorHandling(PEAR_ERROR_CALLBACK, 'debug_die');
$test_mktable_query = "CREATE TABLE phptest (a INTEGER, b VARCHAR(40), c VARCHAR(255), d VARCHAR(20))";
$dbh->query($test_mktable_query);
$dbh->query("INSERT INTO phptest VALUES(42, 'bing', 'This is a test', '1999-11-21')");

$dbh->setErrorHandling(PEAR_ERROR_RETURN);

?>
