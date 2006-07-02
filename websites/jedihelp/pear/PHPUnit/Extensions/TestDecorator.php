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
// $Id: TestDecorator.php,v 1.3 2003/07/24 06:39:52 sebastian Exp $
//

require_once 'PHPUnit/Framework/Assert.php';
require_once 'PHPUnit/Framework/Test.php';

/**
 * A Decorator for Tests.
 *
 * Use TestDecorator as the base class for defining new
 * test decorators. Test decorator subclasses can be introduced
 * to add behaviour before or after a test is run.
 *
 * @package phpunit.extensions
 * @author  Sebastian Bergmann <sb@sebastian-bergmann.de>
 */
class PHPUnit_Extensions_TestDecorator extends PHPUnit_Framework_Assert implements PHPUnit_Framework_Test {
    // {{{ Members

    /**
    * The Test to be decorated.
    *
    * @var    object
    * @access protected
    */
    protected $test = null;

    // }}}
    // {{{ public function __construct(PHPUnit_Framework_Test $test)

    /**
    * Constructor.
    *
    * @param  PHPUnit_Framework_Test $test
    * @access public
    */
    public function __construct(PHPUnit_Framework_Test $test) {
        $this->test = $test;
    }

    // }}}
    // {{{ public function basicRun(PHPUnit_Framework_TestResult $result)

    /**
    * Runs the test and collects the
    * result in a TestResult.
    *
    * @param  PHPUnit_Framework_TestResult $result
    * @access public
    */
    public function basicRun(PHPUnit_Framework_TestResult $result) {
        $this->test->run($result);
    }

    // }}}
    // {{{ public function countTestCases()

    /**
    * Counts the number of test cases that
    * will be run by this test.
    *
    * @return integer
    * @access public
    */
    public function countTestCases() {
        return $this->test->countTestCases();
    }

    // }}}
    // {{{ public function getTest()

    /**
    * Returns the test to be run.
    *
    * @return PHPUnit_Framework_Test
    * @access public
    */
    public function getTest() {
        return $this->test;
    }

    // }}}
    // {{{ protected function run(PHPUnit_Framework_TestResult $result)

    /**
    * Runs the decorated test and collects the
    * result in a TestResult.
    *
    * @param  PHPUnit_Framework_TestResult $result
    * @access public
    */
    public function run(PHPUnit_Framework_TestResult $result) {
        $this->basicRun($result);
    }

    // }}}
    // {{{ public function toString()

    /**
    * Returns a string representation of the test.
    *
    * @return string
    * @access public
    */
    public function toString() {
        return $this->test->toString();
    }

    // }}}
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
