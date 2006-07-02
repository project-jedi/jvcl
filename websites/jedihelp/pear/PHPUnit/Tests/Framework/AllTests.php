<?php
//
// +------------------------------------------------------------------------+
// | PEAR :: PHPUnit                                                        |
// +------------------------------------------------------------------------+
// | Copyright (c) 2002-2003 Sebastian Bergmann <sb@sebastian-bergmann.de>. |
// +------------------------------------------------------------------------+
// | This source file is subject to version 3.00 of the PHP License,        |
// | that is available at http://www.php.net/license/3_0.txt.               |
// | If you did not receive a copy of the PHP license and are unable to     |
// | obtain it through the world-wide-web, please send a note to            |
// | license@php.net so we can mail you a copy immediately.                 |
// +------------------------------------------------------------------------+
//
// $Id: AllTests.php,v 1.4 2003/07/26 11:35:36 sebastian Exp $
//

if (!defined('PHPUnit_MAIN_METHOD')) {
    define('PHPUnit_MAIN_METHOD', 'PHPUnit_Tests_Framework_AllTests::main');
}

require_once 'PHPUnit/Framework/TestSuite.php';
require_once 'PHPUnit/TextUI/TestRunner.php';

require_once 'PHPUnit/Tests/Framework/AssertTest.php';
require_once 'PHPUnit/Tests/Framework/ComparisonFailureTest.php';
require_once 'PHPUnit/Tests/Framework/DoublePrecisionAssertTest.php';
require_once 'PHPUnit/Tests/Framework/NoArgTestCaseTest.php';
require_once 'PHPUnit/Tests/Framework/SuiteTest.php';
require_once 'PHPUnit/Tests/Framework/TestCaseTest.php';
/*
XXX: FIX ME: Disabled due to test implementation problems

require_once 'PHPUnit/Tests/Framework/TestImplementorTest.php';
require_once 'PHPUnit/Tests/Framework/TestListenerTest.php';
*/

class PHPUnit_Tests_Framework_AllTests {
    public static function main() {
        PHPUnit_TextUI_TestRunner::run(self::suite());
    }

    public static function suite() {
        $suite = new PHPUnit_Framework_TestSuite('PHPUnit Framework');

        $suite->addTestSuite(new Reflection_Class('PHPUnit_Tests_Framework_AssertTest'));
        $suite->addTestSuite(new Reflection_Class('PHPUnit_Tests_Framework_ComparisonFailureTest'));
        $suite->addTestSuite(new Reflection_Class('PHPUnit_Tests_Framework_DoublePrecisionAssertTest'));
        $suite->addTestSuite(new Reflection_Class('PHPUnit_Tests_Framework_NoArgTestCaseTest'));
        $suite->addTestSuite(new Reflection_Class('PHPUnit_Tests_Framework_SuiteTest'));
        $suite->addTestSuite(new Reflection_Class('PHPUnit_Tests_Framework_TestCaseTest'));
/*
XXX: FIX ME: Disabled due to test implementation problems

        $suite->addTestSuite('PHPUnit_Tests_Framework_TestImplementorTest');
        $suite->addTestSuite('PHPUnit_Tests_Framework_TestListenerTest');
*/

        return $suite;
    }
}

if (PHPUnit_MAIN_METHOD == 'PHPUnit_Tests_Framework_AllTests::main') {
    PHPUnit_Tests_Framework_AllTests::main();
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
