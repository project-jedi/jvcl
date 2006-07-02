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
// $Id: SuiteTest.php,v 1.2 2003/06/13 08:52:00 sebastian Exp $
//

require_once 'PHPUnit/Framework/TestCase.php';
require_once 'PHPUnit/Framework/TestResult.php';
require_once 'PHPUnit/Framework/TestSuite.php';

require_once 'PHPUnit/Tests/Framework/InheritedTestCase.php';
require_once 'PHPUnit/Tests/Framework/NotPublicTestCase.php';
require_once 'PHPUnit/Tests/Framework/NotVoidTestCase.php';
require_once 'PHPUnit/Tests/Framework/NoTestCaseClass.php';
require_once 'PHPUnit/Tests/Framework/NoTestCases.php';
require_once 'PHPUnit/Tests/Framework/OneTestCase.php';

class PHPUnit_Tests_Framework_SuiteTest extends PHPUnit_Framework_TestCase {
    protected $fResult;

    protected function setUp() {
        $this->fResult = new PHPUnit_Framework_TestResult;
    }

    public static function suite() {
        $suite = new PHPUnit_Framework_TestSuite;

        $suite->addTest(new PHPUnit_Tests_Framework_SuiteTest('testAddTestSuite'));
        $suite->addTest(new PHPUnit_Tests_Framework_SuiteTest('testInheritedTests'));
        $suite->addTest(new PHPUnit_Tests_Framework_SuiteTest('testNoTestCases'));
        $suite->addTest(new PHPUnit_Tests_Framework_SuiteTest('testNoTestCaseClass'));
        $suite->addTest(new PHPUnit_Tests_Framework_SuiteTest('testNotExistingTestCase'));
        $suite->addTest(new PHPUnit_Tests_Framework_SuiteTest('testNotPublicTestCase'));
        $suite->addTest(new PHPUnit_Tests_Framework_SuiteTest('testNotVoidTestCase'));
        $suite->addTest(new PHPUnit_Tests_Framework_SuiteTest('testOneTestCase'));
        $suite->addTest(new PHPUnit_Tests_Framework_SuiteTest('testShadowedTests'));

        return $suite;
    }

    public function testAddTestSuite() {
    }

    public function testInheritedTests() {
    }

    public function testNoTestCases() {
    }

    public function testNoTestCaseClass() {
    }

    public function testNotExistingTestCase() {
    }

    public function testNotPublicTestCase() {
    }

    public function testNotVoidTestCase() {
    }

    public function testOneTestCase() {
    }

    public function testShadowedTests() {
    }
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
