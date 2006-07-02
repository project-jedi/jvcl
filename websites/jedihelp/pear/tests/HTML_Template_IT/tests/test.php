<?php
$testcases = array(
    'ITX_api_testcase',
    'ITX_usage_testcase'
);
// use these if you want to check only base HTML_Template_IT functionality
if(false)
$testcases = array(
    'IT_api_testcase',
    'IT_usage_testcase'
);

require_once 'PHPUnit/TestSuite.php';
require_once 'HTML/Template/ITX.php';

$suite =& new PHPUnit_TestSuite();

foreach ($testcases as $testcase) {
    include_once $testcase . '.php';
    $methods = preg_grep('/^test/', get_class_methods($testcase));
    foreach ($methods as $method) {
        $suite->addTest(new $testcase($method));
    }
}

require_once 'Console_TestListener.php';
$result =& new PHPUnit_TestResult();
$result->addListener(new Console_TestListener);
$suite->run($result);
?>
