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
// $Id: TestRunner.php,v 1.3 2003/07/24 06:39:53 sebastian Exp $
//

if (!defined('PHPUnit_MAIN_METHOD')) {
    define('PHPUnit_MAIN_METHOD', 'PHPUnit_TextUI_TestRunner::main');
}

require_once 'Benchmark/Timer.php';
require_once 'Console/Getopt.php';
require_once 'PHPUnit/Framework/Test.php';
require_once 'PHPUnit/Framework/TestResult.php';
require_once 'PHPUnit/Framework/Version.php';
require_once 'PHPUnit/Runner/BaseTestRunner.php';
require_once 'PHPUnit/TextUI/ResultPrinter.php';

/**
 * A TestRunner for the Command Line Interface (CLI)
 * PHP SAPI Module.
 *
 * @package phpunit.textui
 * @author  Sebastian Bergmann <sb@sebastian-bergmann.de>
 */
class PHPUnit_TextUI_TestRunner extends PHPUnit_Runner_BaseTestRunner {
    // {{{ Members

    /**
    * @var    PHPUnit_TextUI_ResultPrinter
    * @access private
    */
    private $fPrinter;

    // }}}
    // {{{ public function __construct()

    public function __construct() {
        $this->fPrinter = new PHPUnit_TextUI_ResultPrinter;
    }

    // }}}
    // {{{ public static function main()

    public static function main() {
        $options = Console_Getopt::getopt(
          $_SERVER['argv'],
          '',
          array('wait')
        );

        if (PEAR::isError($options)) {
            // ...
            exit;
        }

        $test = isset($options[1][0])    ? $options[1][0] : false;
        $wait = isset($options[0][0][0]) ? true           : false;

        if ($test) {
            $testRunner = new PHPUnit_TextUI_TestRunner;

            try {
                $result = $testRunner->doRun(
                  $testRunner->getTest($test),
                  $wait
                );
            }

            catch (Exception $e) {
                // ...
            }
        } else {
            // ...
        }
    }

    // }}}
    // {{{ public function doRun(PHPUnit_Framework_Test $suite, $wait = false)

    /**
    * Runs a test suite.
    *
    * @param           PHPUnit_Framework_Test $suite
    * @param  optional boolean                $wait
    * @return PHPUnit_Framework_TestResult
    * @access public
    */
    public function doRun(PHPUnit_Framework_Test $suite, $wait = false) {
        printf(
          "PHPUnit %s by Sebastian Bergmann.\n\n",

          PHPUnit_Framework_Version
        );

        $result = new PHPUnit_Framework_TestResult;
        $result->addListener($this->fPrinter);

        $timer = new Benchmark_Timer;

        $timer->start();
        $suite->run($result);
        $timer->stop();
        $this->pause($wait);

        $this->fPrinter->printResult($result, $timer->timeElapsed());

        return $result;
    }

    // }}}
    // {{{ public function run(PHPUnit_Framework_Test $test)

    /**
    * Runs a single test and collects its results.
    * This method can be used to start a test run
    * from your program.
    *
    * @param  PHPUnit_Framework_Test  $test
    * @return PHPUnit_Framework_TestResult
    * @access public
    */
    public static function run(PHPUnit_Framework_Test $test) {
        $runner = new PHPUnit_TextUI_TestRunner;

        return $runner->doRun($test);
    }

    // }}}
    // {{{ protected function pause($wait)

    /**
    * @param  boolean $wait
    * @access protected
    */
    protected function pause($wait) {
        if (!$wait) {
            return;
        }

        $this->fPrinter->printWaitPrompt();

        fgets(STDIN);
    }

    // }}}
    // {{{ public function testEnded($testName)

    /**
    * A test ended.
    *
    * @param  string  $testName
    * @access public
    */
    public function testEnded($testName) {}

    // }}}
    // {{{ public function testFailed($status, PHPUnit_Framework_Test $test, PHPUnit_Framework_AssertionFailedError $e)

    /**
    * A test failed.
    *
    * @param  integer                                $status
    * @param  PHPUnit_Framework_Test                 $test
    * @param  PHPUnit_Framework_AssertionFailedError $e
    * @access public
    */
    public function testFailed($status, PHPUnit_Framework_Test $test, PHPUnit_Framework_AssertionFailedError $e) {}

    // }}}
    // {{{ public function testStarted($testName)

    /**
    * A test started.
    *
    * @param  string  $testName
    * @access public
    */
    public function testStarted($testName) {}

    // }}}
    // {{{ protected function runFailed($message)

    /**
    * Handle a failed loading of a test suite.
    *
    * @param  string  $message
    * @access protected
    */
    protected function runFailed($message) {
        print $message;
    }

    // }}}
}

if (PHPUnit_MAIN_METHOD == 'PHPUnit_TextUI_TestRunner::main') {
    PHPUnit_TextUI_TestRunner::main();
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
