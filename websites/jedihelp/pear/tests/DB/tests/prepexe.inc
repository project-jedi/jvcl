<?php

$dbh->setErrorHandling(PEAR_ERROR_DIE);

$tmpfile = tempnam("/tmp", "phptmp");
register_shutdown_function("my_shutdown");
$fp = fopen($tmpfile, "w");
$filedata = "opaque\nplaceholder\ntest";
fwrite($fp, $filedata);
fclose($fp);

// 1) Multiple prepare/exec INSERT queries
echo "------------1------------\n";

$sth1 = $dbh->prepare("INSERT INTO phptest (a, b) VALUES(?, 'a')");
$sth2 = $dbh->prepare("INSERT INTO phptest (a,b) VALUES(?,?)");
$sth3 = $dbh->prepare("INSERT INTO phptest (a,b,c) VALUES(?,?,&)");
print "sth1,sth2,sth3 created\n";

if (($res = $dbh->execute($sth1, array(72))) == DB_OK) {
    print "sth1 executed\n";
}
if (($res = $dbh->execute($sth2, array(72,'bing'))) == DB_OK) {
    print "sth2 executed\n";
}
if (($res = $dbh->execute($sth3, array(72,'gazonk',$tmpfile))) == DB_OK) {
    print "sth3 executed\n";
}
print_results();

// 2) One prepared, multiple time executed
echo "\n------------2------------\n";

$dbh->query('DELETE FROM phptest');
$sth = $dbh->prepare("INSERT INTO phptest (a,b,c) VALUES(?,?,&)");
$data = array(
    0 => array(72, 'set1', $tmpfile),
    1 => array(72, 'set2', $tmpfile),
    2 => array(72, 'set3', $tmpfile)
);
$dbh->executeMultiple($sth, $data);
print_results();

// 3) freePrepared() test
echo "\n------------3------------\n";

if ($dbh->freePrepared($sth)) {
    echo 'TRUE';
} else {
    echo 'FALSE';
}
echo "\n";
if ($dbh->freePrepared(666)) {
    echo 'TRUE';
} else {
    echo 'FALSE';
}
echo "\n";

// 4) SELECTs tests
echo "\n------------4------------\n";

$sth1 = $dbh->prepare("SELECT * FROM phptest WHERE a = ? ORDER BY b");
print_4($sth1, '72');
print_4($sth1, '71');
$sth2 = $dbh->prepare("SELECT * FROM phptest WHERE b = ? ORDER BY b");
print_4($sth2, 'set1');
$sth3 = $dbh->prepare("SELECT * FROM phptest WHERE c = & ORDER BY b");
print_4($sth3, $tmpfile);

function my_shutdown() {
    global $tmpfile;
    unlink($tmpfile);
}

function print_results() {
    global $dbh;
    print "results:\n";
    $sth = $dbh->query("SELECT * FROM phptest WHERE a = 72 ORDER BY b");
    while ($row = $sth->fetchRow(DB_FETCHMODE_ORDERED)) {
        print '|' . implode(" - ", $row) . "|\n";
    }
}

function print_4($sth, $bind) {
    global $dbh;
    $res = $dbh->execute($sth, $bind);
    while ($row = $res->fetchRow(DB_FETCHMODE_ORDERED)) {
        print '|' . implode(" - ", $row) . "|\n";
    }
    echo "~~\n";
}

?>