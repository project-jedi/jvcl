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
// $Id: AllTests.php,v 1.3 2003/07/26 11:35:36 sebastian Exp $
//

if (!defined('PHPUnit_MAIN_METHOD')) {
    define('PHPUnit_MAIN_METHOD', 'PHPUnit_Tests_Extensions_AllTests::main');
}

require_once 'PHPUnit/Framework/TestSuite.php';
require_once 'PHPUnit/TextUI/TestRunner.php';

require_once 'PHPUnit/Tests/Extensions/ExceptionTestCaseTest.php';
require_once 'PHPUnit/Tests/Extensions/ExtensionTest.php';
require_once 'PHPUnit/Tests/Extensions/PerformanceTestCaseTest.php';
require_once 'PHPUnit/Tests/Extensions/RepeatedTestTest.php';

class PHPUnit_Tests_Extensions_AllTests {
    public static function main() {
        PHPUnit_TextUI_TestRunner::run(self::suite());
    }

    public static function suite() {
        $suite = new PHPUnit_Framework_TestSuite('PHPUnit Extensions');

        $suite->addTestSuite(new Reflection_Class('PHPUnit_Tests_Extensions_ExceptionTestCaseTest'));
        $suite->addTestSuite(new Reflection_Class('PHPUnit_Tests_Extensions_ExtensionTest'));
        $suite->addTestSuite(new Reflection_Class('PHPUnit_Tests_Extensions_PerformanceTestCaseTest'));
        $suite->addTestSuite(new Reflection_Class('PHPUnit_Tests_Extensions_RepeatedTestTest'));

        return $suite;
    }
}

if (PHPUnit_MAIN_METHOD == 'PHPUnit_Tests_Extensions_AllTests::main') {
    PHPUnit_Tests_Extensions_AllTests::main();
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
