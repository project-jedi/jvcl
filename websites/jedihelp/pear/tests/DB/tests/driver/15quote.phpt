--TEST--
DB_driver::escape data test
--SKIPIF--
<?php chdir(dirname(__FILE__)); include("skipif.inc"); ?>
--FILE--
<?php
include './connect.inc';
$dbh->setErrorHandling(PEAR_ERROR_CALLBACK, 'debug_die');

$dbh->expectError(DB_ERROR_ALREADY_EXISTS);
$dbh->query("CREATE TABLE pearquote (n FLOAT, s VARCHAR(8))");
$dbh->popExpect();

$dbh->query("DELETE FROM pearquote");

$strings = array(
    "'",
    "\"",
    "\\",
    "%",
    "_",
    "''",
    "\"\"",
    "\\\\",
    "\\'\\'",
    "\\\"\\\""
);
$nums = array(
    156,
    12.3,
    '15',
    '12.3'
);

echo "String escape test: ";
foreach ($strings as $s) {
    $quoted = $dbh->quote($s);
    $dbh->query("INSERT INTO pearquote VALUES (1, $quoted)");
}
$diff = array_diff($strings, $res = $dbh->getCol("SELECT s FROM pearquote"));
if (count($diff) > 0) {
    echo "FAIL";
    print_r($strings);
    print_r($res);
} else {
    echo "OK";
}

$dbh->query("DELETE FROM pearquote");

echo "\nNumber escape test: ";
foreach ($nums as $n) {
    $quoted = $dbh->quote($n);
    $dbh->query("INSERT INTO pearquote VALUES ($quoted, 'foo')");
}
$diff = array_diff($nums, $res = $dbh->getCol("SELECT n FROM pearquote"));
if (count($diff) > 0) {
    echo "FAIL";
    print_r($nums);
    print_r($res);
} else {
    echo "OK\n";
}
?>
--EXPECT--
String escape test: OK
Number escape test: OK
