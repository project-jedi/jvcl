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
// $Id: BaseTestRunner.php,v 1.10 2003/09/01 21:30:27 sebastian Exp $
//

require_once 'PHPUnit/Framework/AssertionFailedError.php';
require_once 'PHPUnit/Framework/TestListener.php';
require_once 'PHPUnit/Framework/TestSuite.php';
require_once 'PHPUnit/Runner/StandardTestSuiteLoader.php';
require_once 'PHPUnit/Runner/TestRunListener.php';

/**
 * Base class for all test runners.
 *
 * @package phpunit.runner
 * @author  Sebastian Bergmann <sb@sebastian-bergmann.de>
 */
abstract class PHPUnit_Runner_BaseTestRunner implements PHPUnit_Framework_TestListener {
    // {{{ Constants

    const SUITE_METHODNAME = 'suite';

    // }}}

    // {{{ public function addError(PHPUnit_Framework_Test $test, Exception $e)

    /**
    * An error occurred.
    *
    * @param  PHPUnit_Framework_Test  $test
    * @param  Exception               $e
    * @access public
    */
    public function addError(PHPUnit_Framework_Test $test, Exception $e) {
        $this->testFailed(PHPUnit_Runner_TestRunListener::STATUS_ERROR, $test, $e);
    }

    // }}}
    // {{{ public function addFailure(PHPUnit_Framework_Test $test, PHPUnit_Framework_AssertionFailedError $e)

    /**
    * A failure occurred.
    *
    * @param  PHPUnit_Framework_Test                 $test
    * @param  PHPUnit_Framework_AssertionFailedError $e
    * @access public
    */
    public function addFailure(PHPUnit_Framework_Test $test, PHPUnit_Framework_AssertionFailedError $e) {
        $this->testFailed(PHPUnit_Runner_TestRunListener::STATUS_FAILURE, $test, $e);
    }

    // }}}
    // {{{ public function endTest(PHPUnit_Framework_Test $test)

    /**
    * A test ended.
    *
    * @param  PHPUnit_Framework_Test  $test
    * @access public
    */
    public function endTest(PHPUnit_Framework_Test $test) {
        $this->testEnded($test->getName());
    }

    // }}}
    // {{{ public function getLoader()

    /**
    * Returns the loader to be used.
    *
    * @return PHPUnit_Runner_TestSuiteLoader
    * @access protected
    */
    public function getLoader() {
        return new PHPUnit_Runner_StandardTestSuiteLoader;
    }

    // }}}
    // {{{ public function getTest($suiteClassName)

    /**
    * Returns the Test corresponding to the given suite.
    * This is a template method, subclasses override
    * the runFailed() and clearStatus() methods.
    *
    * @param  string  $suiteClassName
    * @return PHPUnit_Framework_Test
    * @access public
    */
    public function getTest($suiteClassName) {
        try {
            $testClass = $this->loadSuiteClass($suiteClassName);
        }

        catch (Exception $e) {
            $this->runFailed($e->getMessage());
            return null;
        }

        $suiteMethod = $testClass->getMethod(self::SUITE_METHODNAME);

        if ($suiteMethod) {
            if (!$suiteMethod->isStatic()) {
                self::runFailed(
                  'suite() method must be static.'
                );

                return null;
            }

            try {
                $test = $suiteMethod->invoke();
            }

            catch (Reflection_Exception $e) {
                self::runFailed(
                  sprintf(
                    "Failed to invoke suite() method.\n%s",

                    $e->getMessage()
                  )
                );

                return null;
            }
        } else {
            $test = new PHPUnit_Framework_TestSuite($testClass);
        }

        $this->clearStatus();

        return $test;
    }

    // }}}
    // {{{ public function startTest(PHPUnit_Framework_Test $test)

    /**
    * A test started.
    *
    * @param  PHPUnit_Framework_Test  $test
    * @access public
    */
    public function startTest(PHPUnit_Framework_Test $test) {
        $this->testStarted($test->getName());
    }

    // }}}
    // {{{ public abstract function testEnded($testName)

    /**
    * A test ended.
    *
    * @param  string  $testName
    * @access public
    * @abstract
    */
    public abstract function testEnded($testName);

    // }}}
    // {{{ public abstract function testFailed($status, PHPUnit_Framework_Test $test, PHPUnit_Framework_AssertionFailedError $e)

    /**
    * A test failed.
    *
    * @param  integer                                 $status
    * @param  PHPUnit_Framework_Test                  $test
    * @param  PHPUnit_Framework_AssertionFailedError  $e
    * @access public
    * @abstract
    */
    public abstract function testFailed($status, PHPUnit_Framework_Test $test, PHPUnit_Framework_AssertionFailedError $e);

    // }}}
    // {{{ public abstract function testStarted($testName)

    /**
    * A test started.
    *
    * @param  string  $testName
    * @access public
    * @abstract
    */
    public abstract function testStarted($testName);

    // }}}
    // {{{ protected abstract function runFailed($message)

    /**
    * Override to define how to handle a failed loading of
    * a test suite.
    *
    * @param  string  $message
    * @access protected
    * @abstract
    */
    protected abstract function runFailed($message);

    // }}}
    // {{{ protected function loadSuiteClass($suiteClassName)

    /**
    * Returns the loaded Reflection_Class for a suite name.
    *
    * @param  string  $suiteClassName
    * @return Reflection_Class
    * @access protected
    */
    protected function loadSuiteClass($suiteClassName) {
        return $this->getLoader()->load($suiteClassName);
    }

    // }}}
    // {{{ protected function clearStatus()

    /**
    * Clears the status message.
    *
    * @access protected
    */
    protected function clearStatus() {
    }

    // }}}
    // {{{ public static function getFilteredStack(Exception $e)

    /**
    * Filters stack frames from PHPUnit classes.
    *
    * @param  Exception $e
    * @return string
    * @access public
    * @static
    */
    public static function getFilteredStack(Exception $e) {
        $filter = array(
          'Assert.php',
          'TestCase.php',
          'TestResult.php',
          'TestRunner.php',
          'TestSuite.php'
        );

        $filteredStacktrace = '';
        $stacktrace         = $e->getTrace();

        foreach ($stacktrace as $frame) {
            $filtered = false;

            for ($i = 0; $i < sizeof($filter); $i++) {
                if (!isset($frame['file']) ||
                    $filter[$i] == substr($frame['file'], 0 - strlen($filter[$i]))) {
                    $filtered = true;
                    break;
                }
            }

            if (!$filtered) {
                $filteredStacktrace .= sprintf(
                  "%s:%s\n",

                  $frame['file'],
                  isset($frame['line']) ? $frame['line'] : '?'
                );
            }
        }

        return $filteredStacktrace;
    }

    // }}}
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
