<?php

if (!function_exists("errhandler"))
{
    function errhandler($obj)
    {
        printf("%s\n", $obj->getMessage());
    }
}

if (isset($test_error_mode)) {
    $dbh->setErrorHandling($test_error_mode);
} else {
    $dbh->setErrorHandling(PEAR_ERROR_DIE);
}
$sth = $dbh->query("SELECT a FROM phptest");
if (!DB::isError($rows = $sth->numRows())) {
    print "$rows\n";
} else {
    print "\n";
}
for ($i = 0; $i < 5; $i++) {
    $sth = $dbh->query("INSERT INTO phptest (a) VALUES($i)");
    $sth = $dbh->query("SELECT a FROM phptest");
    if (!DB::isError($rows = $sth->numRows())) {
        print "$rows\n";
    } else {
        print "\n";
    }
}
$dbh->query("DELETE FROM phptest WHERE a < 4");
$sth = $dbh->query("SELECT a FROM phptest");
if (!DB::isError($rows = $sth->numRows())) {
    print "$rows\n";
} else {
    print "\n";
}
$sth = $dbh->query("SELECT a FROM phptest where a < 0");
if (!DB::isError($rows = $sth->numRows())) {
    print "$rows\n";
} else {
    print "\n";
}
?>
