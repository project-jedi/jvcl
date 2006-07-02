<?php

$dbh->setErrorHandling(PEAR_ERROR_DIE);

print "testing fetchmodes: fetchrow default default\n";
$sth = $dbh->query("SELECT * FROM phptest");
$row = $sth->fetchRow();
print implode(" ", array_keys($row))."\n";

print "testing fetchmodes: fetchinto default default\n";
$sth = $dbh->query("SELECT * FROM phptest");
$row = array();
$sth->fetchInto($row);
print implode(" ", array_keys($row))."\n";

print "testing fetchmodes: fetchrow ordered default\n";
$dbh->setFetchMode(DB_FETCHMODE_ORDERED);
$sth = $dbh->query("SELECT * FROM phptest");
$row = $sth->fetchRow();
print implode(" ", array_keys($row))."\n";

print "testing fetchmodes: fetchrow assoc default\n";
$dbh->setFetchMode(DB_FETCHMODE_ASSOC);
$sth = $dbh->query("SELECT * FROM phptest");
$row = $sth->fetchRow();
print implode(" ", array_keys($row))."\n";

print "testing fetchmodes: fetchrow ordered default with assoc specified\n";
$dbh->setFetchMode(DB_FETCHMODE_ORDERED);
$sth = $dbh->query("SELECT * FROM phptest");
$row = $sth->fetchRow(DB_FETCHMODE_ASSOC);
print implode(" ", array_keys($row))."\n";

print "testing fetchmodes: fetchrow assoc default with ordered specified\n";
$dbh->setFetchMode(DB_FETCHMODE_ASSOC);
$sth = $dbh->query("SELECT * FROM phptest");
$row = $sth->fetchRow(DB_FETCHMODE_ORDERED);
print implode(" ", array_keys($row))."\n";

print "testing fetchmodes: fetchinto ordered default\n";
$dbh->setFetchMode(DB_FETCHMODE_ORDERED);
$sth = $dbh->query("SELECT * FROM phptest");
$row = array();
$sth->fetchInto($row);
print implode(" ", array_keys($row))."\n";

print "testing fetchmodes: fetchinto assoc default\n";
$dbh->setFetchMode(DB_FETCHMODE_ASSOC);
$sth = $dbh->query("SELECT * FROM phptest");
$row = array();
$sth->fetchInto($row);
print implode(" ", array_keys($row))."\n";

print "testing fetchmodes: fetchinto ordered default with assoc specified\n";
$dbh->setFetchMode(DB_FETCHMODE_ORDERED);
$sth = $dbh->query("SELECT * FROM phptest");
$row = array();
$sth->fetchInto($row, DB_FETCHMODE_ASSOC);
print implode(" ", array_keys($row))."\n";

print "testing fetchmodes: fetchinto assoc default with ordered specified\n";
$dbh->setFetchMode(DB_FETCHMODE_ASSOC);
$sth = $dbh->query("SELECT * FROM phptest");
$row = array();
$sth->fetchInto($row, DB_FETCHMODE_ORDERED);
print implode(" ", array_keys($row))."\n";

?>
