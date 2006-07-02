<?php

function errhandler($obj)
{
    if ($obj->getCode() == DB_ERROR) {
        print "  unknown: ".$obj->getUserInfo()."\n";
    } else {
        $msg = $obj->getMessage();
        print "  $msg";
        if (substr($msg, -1) != "\n") {
            print "\n";
        }
    }
}

$dbh->setErrorHandling(PEAR_ERROR_CALLBACK, "errhandler");

print "Trying to provoke DB_ERROR_NOSUCHTABLE\n";
$dbh->query("SELECT * FROM tableThatDoesNotExist");

print "Trying to provoke DB_ERROR_ALREADY_EXISTS\n";
$dbh->query($test_mktable_query);

print "Trying to provoke DB_ERROR_NOSUCHTABLE\n";
$dbh->query("DROP TABLE tableThatDoesNotExist");

print "Trying to provoke DB_ERROR_CONSTRAINT\n";
$dbh->setErrorHandling(PEAR_ERROR_RETURN);
$dbh->query("DROP TABLE b");
$dbh->query("DROP TABLE a");
$dbh->setErrorHandling(PEAR_ERROR_CALLBACK, "errhandler");
$dbh->query("CREATE TABLE a (a INTEGER NOT NULL, PRIMARY KEY(a))");
$dbh->query("CREATE TABLE b (b INTEGER REFERENCES a(a))");
$dbh->query("INSERT INTO a VALUES(1)");
$dbh->query("INSERT INTO b VALUES(2)");

print "Trying to provoke DB_ERROR_DIVZERO\n";
// Interbase detects the error on fetching
$dbh->getAll("SELECT 0/0 FROM phptest");

print "Trying to provoke DB_ERROR_INVALID_NUMBER\n";
$dbh->query("UPDATE phptest SET a = 'abc' WHERE a = 42");

print "Trying to provoke DB_ERROR_NOSUCHFIELD\n";
$dbh->query("SELECT e FROM phptest");

print "Trying to provoke DB_ERROR_SYNTAX\n";
$dbh->query("CREATE");

?>
