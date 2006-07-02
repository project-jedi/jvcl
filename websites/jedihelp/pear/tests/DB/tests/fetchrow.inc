<?php

$dbh->setErrorHandling(PEAR_ERROR_DIE);
$dbh->query("INSERT INTO phptest VALUES(1, 'one', 'One', '2001-02-16')");
$dbh->query("INSERT INTO phptest VALUES(2, 'two', 'Two', '2001-02-15')");
$dbh->query("INSERT INTO phptest VALUES(3, 'three', 'Three', '2001-02-14')");

print "testing fetchrow:\n";
$sth = $dbh->query("SELECT * FROM phptest");
for ($i = 1; $i <= 5; $i++) {
    print "row $i: ";
    $row = $sth->fetchRow();
    if (DB::isError($row)) {
        print $row->toString() . "\n";
        continue;
    }
    if (is_array($row)) {
        print implode(", ", $row) . "\n";
    } else {
        var_dump($row);
    }
}

$dbh->query("DELETE FROM phptest WHERE a <> 42");

?>
