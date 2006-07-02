<?php

include_once('Auth/Auth.php');
include('TestAuthContainer.php');
include('FileContainer.php');
include('DBContainer.php');
include('MDBContainer.php');
include('POP3Container.php');
include('POP3aContainer.php');
include('IMAPContainer.php');
include_once('PHPUnit.php');


function error($err){
    print "Error\n";
    print "Code:".trim($err->getCode())."\n";
    print "Message:".trim($err->getMessage())."\n";
    #print "UserInfo:".trim($err->getUserInfo())."\n";
    #print "DebugInfo:".trim($err->getDebugInfo())."\n";

}

#error_reporting(0);
#PEAR::setErrorHandling(PEAR_ERROR_PRINT, "\nPear Error:%s \n");
PEAR::setErrorHandling(PEAR_ERROR_CALLBACK, "error");

set_time_limit(0);

$suite = new PHPUnit_TestSuite();

// File Container
$suite->addTest(new PHPUnit_TestSuite('IMAPContaner'));

$suite->addTest(new PHPUnit_TestSuite('FileContaner'));

// DB Container
//$suite->addTest(new PHPUnit_TestSuite('DBContainer'));
// MDB Container
//$suite->addTest(new PHPUnit_TestSuite('MDBContainer'));
// POP3 Container
//$suite->addTest(new PHPUnit_TestSuite('POP3Container'));


$result = PHPUnit::run($suite);
echo $result->toString();

?>