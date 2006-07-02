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
// $Id: TestSuite.php,v 1.8 2003/08/29 12:45:26 sebastian Exp $
//

require_once 'PHPUnit/Framework/Test.php';
require_once 'PHPUnit/Framework/TestCase.php';
require_once 'PHPUnit/Framework/Warning.php';

/**
 * A TestSuite is a Composite of Tests. It runs a collection of test cases.
 *
 * Here is an example using the dynamic test definition.
 *
 *   $suite = new PHPUnit_Framework_TestSuite();
 *   $suite->addTest(new MathTest('testPass'));
 *
 * Alternatively, a TestSuite can extract the tests to be run automatically.
 * To do so you pass the classname of your PHPUnit_Framework_TestCase class to
 * the PHPUnit_Framework_TestSuite constructor.
 *
 *   $suite = new PHPUnit_Framework_TestSuite('classname');
 *
 * This constructor creates a suite with all the methods starting with
 * "test" that take no arguments.
 *
 * @package phpunit.framework
 * @author  Sebastian Bergmann <sb@sebastian-bergmann.de>
 */
class PHPUnit_Framework_TestSuite implements PHPUnit_Framework_Test {
    // {{{ Members

    /**
    * The name of the test suite.
    *
    * @var    string
    * @access private
    */
    private $fName = '';

    /**
    * The tests in the test suite.
    *
    * @var    array
    * @access private
    */
    private $fTests = array();

    // }}}
    // {{{ public function __construct($theClass = '', $name = '')

    /**
    * Constructs a TestSuite.
    *
    * @param  optional mixed  $theClass
    * @param  optional string $name
    * @access public
    */
    public function __construct($theClass = '', $name = '') {
        if (is_string($theClass)) {
          $this->setName($theClass);

          return;
        }

        $constructor = $theClass->getConstructor();

        if (!$constructor ||
            !$constructor->isPublic()) {
            $this->addTest(
              self::warning(
                sprintf(
                  'Class %s has no public constructor',

                  $theClass->getName()
                )
              )
            );

            return;
        }

        $methods = $theClass->getMethods();
        $names   = array();

        foreach ($methods as $method) {
            $this->addTestMethod($method, $names, $theClass);
        }

        if (empty($this->fTests)) {
            $this->addTest(
              self::warning(
                sprintf(
                  'No tests found in %s',

                  $theClass->getName()
                )
              )
            );
        }
    }

    // }}}
    // {{{ public function addTest(PHPUnit_Framework_Test $test)

    /**
    * Adds a test to the suite.
    *
    * @param  PHPUnit_Framework_Test
    * @access public
    */
    public function addTest($test) {
        $this->fTests[] = $test;
    }

    // }}}
    // {{{ public function addTestSuite(Reflection_Class $testClass)

    /**
    * Adds the tests from the given class to the suite.
    *
    * @param  Reflection_Class $testClass
    * @access public
    */
    public function addTestSuite(Reflection_Class $testClass) {
        $this->addTest(new PHPUnit_Framework_TestSuite($testClass));
    }

    // }}}
    // {{{ public function countTestCases()

    /**
    * Counts the number of test cases that will be run by this test.
    *
    * @return integer
    * @access public
    */
    public function countTestCases() {
        $count = 0;

        foreach ($this->fTests as $test) {
            $count += $test->countTestCases();
        }

        return $count;
    }

    // }}}
    // {{{ public static function createTest(Reflection_Class $theClass, $name)

    /**
    * @param  Reflection_Class $theClass
    * @param  string           $name
    * @return PHPUnit_Framework_Test
    * @access public
    * @static
    */
    public static function createTest(Reflection_Class $theClass, $name) {
        $test = $theClass->newInstance();

        if ($test instanceof PHPUnit_Framework_TestCase) {
            $test->setName($name);
        } else {
            return self::warning(
              sprintf(
                'Cannot instantiate test case: %s',

                $name
              )
            );
        }

        return $test;
    }

    // }}}
    // {{{ public function getName()

    /**
    * Returns the name of the suite.
    *
    * @return string
    * @access public
    */
    public function getName() {
        return $this->fName;
    }

    // }}}
    // {{{ public function run(PHPUnit_Framework_TestResult $result)

    /**
    * Runs the tests and collects their result in a TestResult.
    *
    * @param  PHPUnit_Framework_TestResult $result
    * @access public
    */
    public function run(PHPUnit_Framework_TestResult $result) {
        foreach ($this->fTests as $test) {
            if ($result->shouldStop()) {
                break;
            }

            $this->runTest($test, $result);
        }
    }

    // }}}
    // {{{ public function runTest(PHPUnit_Framework_Test $test, PHPUnit_Framework_TestResult $result)

    /**
    * Runs a test.
    *
    * @param  PHPUnit_Framework_Test        $test
    * @param  PHPUnit_Framework_TestResult  $testResult
    * @access public
    */
    public function runTest(PHPUnit_Framework_Test $test, PHPUnit_Framework_TestResult $result) {
        $test->run($result);
    }

    // }}}
    // {{{ public function setName($name)

    /**
    * Sets the name of the suite.
    *
    * @param  string
    * @access public
    */
    public function setName($name) {
        $this->fName = $name;
    }

    // }}}
    // {{{ public function testAt($index)

    /**
    * Returns the test at the given index.
    *
    * @param  integer
    * @return PHPUnit_Framework_Test
    * @access public
    */
    public function testAt($index) {
        if (isset($this->fTests[$index])) {
            return $this->fTests[$index];
        } else {
            return false;
        }
    }

    // }}}
    // {{{ public function testCount()

    /**
    * Returns the number of tests in this suite.
    *
    * @return integer
    * @access public
    */
    public function testCount() {
        return sizeof($this->fTests);
    }

    // }}}
    // {{{ public function tests()

    /**
    * Returns the tests as an enumeration.
    *
    * @return array
    * @access public
    */
    public function tests() {
        return $this->fTests;
    }

    // }}}
    // {{{ public function toString()

    /**
    * Returns a string representation of the test suite.
    *
    * @return string
    * @access public
    */
    public function toString() {
        return $this->getName();
    }

    // }}}
    // {{{ public function addTestMethod(Reflection_Method $method, &$names, Reflection_Class $theClass)

    /**
    * @param  Reflection_Method $method
    * @param  array      $names
    * @param  Reflection_Class  $theClass
    * @access private
    */
    private function addTestMethod(Reflection_Method $method, &$names, Reflection_Class $theClass) {
        $name = $method->getName();

        if (in_array($name, $names)) {
            return;
        }

        if ($this->isPublicTestMethod($method)) {
            $names[] = $name;

            $this->addTest(
              self::createTest(
                $theClass,
                $name
              )
            );
        }

        else if ($this->isTestMethod($method)) {
            $this->addTest(
              self::warning(
                sprintf(
                  'Test method is not public: %s',

                  $name
                )
              )
            );
        }
    }

    // }}}
    // {{{ private function isPublicTestMethod(Reflection_Method $method)

    /**
    * @param  Reflection_Method $method
    * @return boolean
    * @access private
    */
    private function isPublicTestMethod(Reflection_Method $method) {
        return ($this->isTestMethod($method) &&
                $method->isPublic());
    }

    // }}}
    // {{{ private function isTestMethod(Reflection_Method $method)

    /**
    * @param  Reflection_Method $method
    * @return boolean
    * @access private
    */
    private function isTestMethod(Reflection_Method $method) {
        return (substr($method->name, 0, 4) == 'test');
    }

    // }}}
    // {{{ private static function warning($message)

    /**
    * @param  string  $message
    * @return PHPUnit_Framework_Warning
    * @access private
    */
    private static function warning($message) {
        return new PHPUnit_Framework_Warning($message);
    }

    // }}}
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
