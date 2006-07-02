<?php
error_reporting(E_ALL);
function pe($obj) {
    die($obj->getMessage() . "\n" . $obj->getDebugInfo());
}

function print_obj(&$obj) {
    if (!is_object($obj)) {
        echo "ERROR: no object!\n";
    } else {
        echo get_class($obj) . " -> " . implode(" ", array_keys((array)$obj)) . "\n";
    }
}

$dbh->setErrorHandling(PEAR_ERROR_CALLBACK, 'pe');

echo "--- fetch with param DB_FETCHMODE_OBJECT ---\n";
$sth = $dbh->query("SELECT * FROM phptest");
$row = $sth->fetchRow(DB_FETCHMODE_OBJECT);
print_obj($row);
$sth = $dbh->query("SELECT * FROM phptest");
$sth->fetchInto($row, DB_FETCHMODE_OBJECT);
print_obj($row);

echo "--- fetch with default fetchmode DB_FETCHMODE_OBJECT ---\n";
$dbh->setFetchMode(DB_FETCHMODE_OBJECT);
$sth = $dbh->query("SELECT * FROM phptest");
$row = $sth->fetchRow();
print_obj($row);
$sth = $dbh->query("SELECT * FROM phptest");
$sth->fetchInto($row);
print_obj($row);

echo "--- fetch with default fetchmode DB_FETCHMODE_OBJECT and class DB_Row ---\n";
$dbh->setFetchMode(DB_FETCHMODE_OBJECT, 'DB_Row');
$sth = $dbh->query("SELECT * FROM phptest");
$row = $sth->fetchRow();
print_obj($row);
$sth = $dbh->query("SELECT * FROM phptest");
$sth->fetchInto($row);
print_obj($row);
?>