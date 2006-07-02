<?php
error_reporting(E_ALL);
function pe($obj) {
    die($obj->getMessage() . "\n" . $obj->getDebugInfo());
}
$dbh->setErrorHandling(PEAR_ERROR_RETURN);
$dbh->query("DROP TABLE php_limit");
$dbh->setErrorHandling(PEAR_ERROR_CALLBACK, 'pe');
$dbh->query("CREATE TABLE php_limit (a VARCHAR(20))");
$from    = 0;
$count   = 10;
$numrows = 30;

for ($i=0; $i<=$numrows+2; $i++) {
    $dbh->query("INSERT INTO php_limit VALUES('result $i')");
}
// Hack for upper case col returned by Oracle
if ($dbh->dsn['phptype'] == 'oci8' &&
    $dbh->options['optimize'] != 'portability') {
    $key = 'A';
} else {
    $key = 'a';
}
for ($i = 0; $i <= 3; $i++) {
    $from = 10 * $i;
    $res = $dbh->limitQuery("select * from php_limit", $from, $count);
    echo "======= From: $from || Number of rows to fetch: $count =======\n";
    while ($row = $res->fetchRow(DB_FETCHMODE_ASSOC)) {
        echo $res->getRowCounter() . '.- ' . $row[$key] . "\n";
    }
}
//$dbh->query("DROP TABLE php_limit");
?>