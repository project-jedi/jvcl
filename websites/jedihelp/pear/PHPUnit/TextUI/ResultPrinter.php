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
// $Id: ResultPrinter.php,v 1.7 2003/09/01 21:30:24 sebastian Exp $
//

require_once 'PHPUnit/Framework/TestFailure.php';
require_once 'PHPUnit/Framework/TestListener.php';
require_once 'PHPUnit/Framework/TestResult.php';

/**
 * @package phpunit.textui
 * @author  Sebastian Bergmann <sb@sebastian-bergmann.de>
 */
class PHPUnit_TextUI_ResultPrinter implements PHPUnit_Framework_TestListener {
    // {{{ Members

    /**
    * @var    integer
    * @access private
    */
    private $fColumn = 0;

    // }}}
    // {{{ public function printResult(PHPUnit_Framework_TestResult $result, $runTime)

    /**
    * @param  PHPUnit_Framework_TestResult  $result
    * @param  float                         $runTime
    * @access public
    */
    public function printResult(PHPUnit_Framework_TestResult $result, $runTime) {
        $this->printHeader($runTime);
        $this->printErrors($result);
        $this->printFailures($result);
        $this->printFooter($result);
    }

    // }}}
    // {{{ protected function printDefects($defects, $count, $type)

    /**
    * @param  array   $defects
    * @param  integer $count
    * @param  string  $type
    * @access protected
    */
    protected function printDefects($defects, $count, $type) {
        if ($count == 0) {
            return;
        }

        printf(
          "There %s %d %s%s:\n",

          ($count == 1) ? 'was' : 'were',
          $count,
          $type,
          ($count == 1) ? '' : 's'
        );

        $i = 1;

        foreach ($defects as $defect) {
            $this->printDefect($defect, $i++);
        }
    }

    // }}}
    // {{{ protected function printDefect(PHPUnit_Framework_TestFailure $defect, $count)

    /**
    * @param  PHPUnit_Framework_TestFailure $defect
    * @param  integer                       $count
    * @access protected
    */
    protected function printDefect(PHPUnit_Framework_TestFailure $defect, $count) {
        $this->printDefectHeader($defect, $count);
        $this->printDefectTrace($defect);
    }

    // }}}
    // {{{ protected function printDefectHeader(PHPUnit_Framework_TestFailure $defect, $count)

    /**
    * @param  PHPUnit_Framework_TestFailure $defect
    * @param  integer                       $count
    * @access protected
    */
    protected function printDefectHeader(PHPUnit_Framework_TestFailure $defect, $count) {
        $name = $defect->failedTest()->getName();

        if ($name == null) {
            $class = new Reflection_Class($defect->failedTest());
            $name  = $class->name;
        }

        printf(
          "%d) %s\n",

          $count,
          $name
        );
    }

    // }}}
    // {{{ protected function printDefectTrace(PHPUnit_Framework_TestFailure $defect)

    /**
    * @param  PHPUnit_Framework_TestFailure $defect
    * @access protected
    */
    protected function printDefectTrace(PHPUnit_Framework_TestFailure $defect) {
        print $defect->thrownException()->toString() . "\n";

        print PHPUnit_Runner_BaseTestRunner::getFilteredStack(
          $defect->thrownException()
        );
    }

    // }}}
    // {{{ protected function printErrors(PHPUnit_Framework_TestResult $result)

    /**
    * @param  PHPUnit_Framework_TestResult  $result
    * @access protected
    */
    protected function printErrors(PHPUnit_Framework_TestResult $result) {
        $this->printDefects($result->errors(), $result->errorCount(), 'error');
    }

    // }}}
    // {{{ protected function printFailures(PHPUnit_Framework_TestResult $result)

    /**
    * @param  PHPUnit_Framework_TestResult  $result
    * @access protected
    */
    protected function printFailures(PHPUnit_Framework_TestResult $result) {
        $this->printDefects($result->failures(), $result->failureCount(), 'failure');
    }

    // }}}
    // {{{ protected function printHeader($runTime)

    /**
    * @param  float   $runTime
    * @access protected
    */
    protected function printHeader($runTime) {
        printf(
          "\n\nTime: %s\n",

          $runTime
        );
    }

    // }}}
    // {{{ protected function printFooter(PHPUnit_Framework_TestResult $result)

    /**
    * @param  PHPUnit_Framework_TestResult  $result
    * @access protected
    */
    protected function printFooter(PHPUnit_Framework_TestResult $result) {
        if ($result->wasSuccessful()) {
            printf(
              "\nOK (%d test%s)\n",

              $result->runCount(),
              ($result->runCount() == 1) ? '' : 's'
            );
        } else {
            printf(
              "\nFAILURES!!!\nTests run: %d, Failures: %d, Errors: %d.\n",

              $result->runCount(),
              $result->failureCount(),
              $result->errorCount()
            );
        }
    }

    // }}}
    // {{{ public function printWaitPrompt()

    /**
    * @access public
    */
    public function printWaitPrompt() {
        print "\n<RETURN> to continue\n";
    }

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
        print 'E';
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
        print 'F';
    }

    // }}}
    // {{{ public function endTest(PHPUnit_Framework_Test $test)

    /**
    * A test ended.
    *
    * @param  PHPUnit_Framework_Test $test
    * @access public
    */
    public function endTest(PHPUnit_Framework_Test $test) {
    }

    // }}}
    // {{{ public function startTest(PHPUnit_Framework_Test $test)

    /**
    * A test started.
    *
    * @param  PHPUnit_Framework_Test $test
    * @access public
    */
    public function startTest(PHPUnit_Framework_Test $test) {
        print '.';

        if ($this->fColumn++ >= 40) {
            $this->fColumn = 0;
            print "\n";
        }
    }

    // }}}
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
