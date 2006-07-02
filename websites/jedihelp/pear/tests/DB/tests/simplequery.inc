<?php

$dbh->setErrorHandling(PEAR_ERROR_DIE);
$sth = $dbh->simpleQuery("SELECT * FROM phptest");
print gettype($sth)."\n";

?>
