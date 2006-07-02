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
// $Id: TestCase.php,v 1.5 2003/07/24 06:39:52 sebastian Exp $
//

require_once 'PHPUnit/Framework/Assert.php';
require_once 'PHPUnit/Framework/Test.php';
require_once 'PHPUnit/Framework/TestResult.php';

/**
 * A TestCase defines the fixture to run multiple tests.
 *
 * To define a TestCase
 *
 *   1) Implement a subclass of PEAR:PHPUnit::TestCase.
 *   2) Define instance variables that store the state of the fixture.
 *   3) Initialize the fixture state by overriding setUp().
 *   4) Clean-up after a test by overriding tearDown().
 *
 * Each test runs in its own fixture so there can be no side effects
 * among test runs.
 *
 * Here is an example:
 *
 *   class MathTest extends PHPUnit_Framework_TestCase {
 *     public $fValue1;
 *     public $fValue2;
 *
 *     public function __construct($name) {
 *       parent::__construct($name);
 *     }
 *
 *     public function setUp() {
 *       $this->fValue1 = 2;
 *       $this->fValue2 = 3;
 *     }
 *   }
 *
 * For each test implement a method which interacts with the fixture.
 * Verify the expected results with assertions specified by calling
 * assert with a boolean.
 *
 *   public function testPass() {
 *     $this->assertTrue($this->fValue1 + $this->fValue2 == 5);
 *   }
 *
 * @abstract
 * @package phpunit.framework
 * @author  Sebastian Bergmann <sb@sebastian-bergmann.de>
 */
abstract class PHPUnit_Framework_TestCase extends PHPUnit_Framework_Assert implements PHPUnit_Framework_Test {
    // {{{ Members

    /**
    * The name of the test case.
    *
    * @var    string
    * @access private
    */
    private $fName = null;

    // }}}
    // {{{ public function __construct($name = false)

    /**
    * Constructs a test case with the given name.
    *
    * @param  string
    * @access public
    */
    public function __construct($name = null) {
        if ($name != null) {
            $this->setName($name);
        }
    }

    // }}}
    // {{{ public function countTestCases()

    /**
    * Counts the number of test cases executed by run(TestResult result).
    *
    * @return integer
    * @access public
    */
    public function countTestCases() {
        return 1;
    }

    // }}}
    // {{{ public function getName()

    /**
    * Gets the name of a TestCase.
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
    * Runs the test case and collects the results in a given TestResult object.
    *
    * @param  PHPUnit_Framework_TestResult $result
    * @return PHPUnit_Framework_TestResult
    * @access public
    */
    public function run(PHPUnit_Framework_TestResult $result) {
        $result->run($this);
    }

    // }}}
    // {{{ public function runBare()

    /**
    * Runs the bare test sequence.
    *
    * @access public
    */
    public function runBare() {
        $catchedException = null;

        $this->setUp();

        try {
            $this->runTest();
        }

        catch (Exception $e) {
            $catchedException = $e;
        }

        $this->tearDown();

        // Workaround for missing "finally".
        if ($catchedException !== null) {
            throw $catchedException;
        }
    }

    // }}}
    // {{{ protected function runTest()

    /**
    * Override to run the test and assert its state.
    *
    * @access protected
    */
    protected function runTest() {
        self::assertNotNull($this->fName);

        try {
            $class  = new Reflection_Class($this);
            $method = $class->getMethod($this->fName);
        }

        catch (Exception $e) {
            self::fail($e->getMessage());
        }

        $method->invoke(new $class->name);
    }

    // }}}
    // {{{ public function setName($name)

    /**
    * Sets the name of a TestCase.
    *
    * @param  string
    * @access public
    */
    public function setName($name) {
        $this->fName = $name;
    }

    // }}}
    // {{{ public function toString()

    /**
    * Returns a string representation of the test case.
    *
    * @return string
    * @access public
    */
    public function toString() {
        $class = new Reflection_Class($this);

        return sprintf(
          '%s(%s)',

          $this->getName(),
          $class->name
        );
    }

    // }}}
    // {{{ protected function createResult()

    /**
    * Creates a default TestResult object.
    *
    * @return PHPUnit_Framework_TestResult
    * @access protected
    */
    protected function createResult() {
        return new PHPUnit_Framework_TestResult;
    }

    // }}}
    // {{{ protected function setUp()

    /**
    * Sets up the fixture, for example, open a network connection.
    * This method is called before a test is executed.
    *
    * @access protected
    */
    protected function setUp() {}

    // }}}
    // {{{ protected function tearDown()

    /**
    * Tears down the fixture, for example, close a network connection.
    * This method is called after a test is executed.
    *
    * @access protected
    */
    protected function tearDown() {}

    // }}}
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
