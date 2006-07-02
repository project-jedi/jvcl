--TEST--
DB configurable error handler test
--FILE--
<?php
include_once './include.inc';
require_once "DB.php";
error_reporting(4095);

function myfunc(&$obj) {
    print "myfunc here, obj=".$obj->toString()."\n";
}
function myfunc2(&$obj) {
    print "myfunc2 here, obj=".$obj->toString()."\n";
}
class myclass {
    function myfunc(&$obj) {
        print "myclass::myfunc here, obj=".$obj->toString()."\n";
    }
}
function test_error_handler($errno, $errmsg, $file, $line, $vars) {
        $errortype = array (
                1   =>  "Error",
                2   =>  "Warning",
                4   =>  "Parsing Error",
                8   =>  "Notice",
                16  =>  "Core Error",
                32  =>  "Core Warning",
                64  =>  "Compile Error",
                128 =>  "Compile Warning",
                256 =>  "User Error",
                512 =>  "User Warning",
                1024=>  "User Notice"
        );
        $prefix = $errortype[$errno];
        $file = basename($file);
        print "$prefix: $errmsg in $file on line XXX\n";
}

$obj = new myclass;

$dbh = DB::factory("mysql");

print "default: ";
$e = $dbh->raiseError("return testing error");
print $e->toString() . "\n";

print "global default: ";
PEAR::setErrorHandling(PEAR_ERROR_CALLBACK, "myfunc2");
$e = $dbh->raiseError("global default test");

$dbh->setErrorHandling(PEAR_ERROR_PRINT);
print "mode=print: ";
$e = $dbh->raiseError("print testing error");
print "\n";

$dbh->setErrorHandling(PEAR_ERROR_CALLBACK, "myfunc");
print "mode=function callback: ";
$e = $dbh->raiseError("function callback testing error");

$dbh->setErrorHandling(PEAR_ERROR_CALLBACK, array($obj, "myfunc"));
print "mode=object callback: ";
$e = $dbh->raiseError("object callback testing error");

set_error_handler("test_error_handler");
$dbh->setErrorHandling(PEAR_ERROR_TRIGGER);
print "mode=trigger: ";
$e = $dbh->raiseError("trigger testing error");

?>
--EXPECT--
default: [db_error: message="DB Error: return testing error" code=-1 mode=return level=notice prefix="" info=""]
global default: myfunc2 here, obj=[db_error: message="DB Error: global default test" code=-1 mode=callback callback=myfunc2 prefix="" info=""]
mode=print: DB Error: print testing error
mode=function callback: myfunc here, obj=[db_error: message="DB Error: function callback testing error" code=-1 mode=callback callback=myfunc prefix="" info=""]
mode=object callback: myclass::myfunc here, obj=[db_error: message="DB Error: object callback testing error" code=-1 mode=callback callback=myclass::myfunc prefix="" info=""]
mode=trigger: User Notice: DB Error: trigger testing error in PEAR.php on line XXX
