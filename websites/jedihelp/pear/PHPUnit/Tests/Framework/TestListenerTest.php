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
// $Id: TestListenerTest.php,v 1.2 2003/06/13 08:52:00 sebastian Exp $
//

require_once 'PHPUnit/Framework/TestCase.php';
require_once 'PHPUnit/Framework/TestListener.php';
require_once 'PHPUnit/Framework/TestResult.php';

require_once 'PHPUnit/Tests/Framework/Error.php';
require_once 'PHPUnit/Tests/Framework/Failure.php';
require_once 'PHPUnit/Tests/Framework/Success.php';

class PHPUnit_Tests_Framework_TestListenerTest extends PHPUnit_Framework_TestCase implements PHPUnit_Framework_TestListener {
    private $fResult;
    private $fStartCount;
    private $fEndCount;
    private $fFailureCount;
    private $fErrorCount;

    public function addError(PHPUnit_Framework_Test $test, Exception $e) {
        $this->fErrorCount++;
    }

    public function addFailure(PHPUnit_Framework_Test $test, PHPUnit_Framework_AssertionFailedError $e) {
        $this->fFailureCount++;
    }

    public function endTest(PHPUnit_Framework_Test $test) {
        $this->fEndCount++;
    }

    public function startTest(PHPUnit_Framework_Test $test) {
        $this->fStartCount++;
    }

    protected function setUp() {
        $this->fResult = new PHPUnit_Framework_TestResult;
        $this->fResult->addListener($this);

        $this->fFailureCount = 0;
        $this->fEndCount     = 0;
        $this->fStartCount   = 0;
    }

    public function testError() {
        $test = new PHPUnit_Tests_Framework_Error;
        $test->run($this->fResult);

        $this->assertEquals(1, $this->fErrorCount);
        $this->assertEquals(1, $this->fEndCount);
    }

    public function testFailure() {
        $test = new PHPUnit_Tests_Framework_Failure;
        $test->run($this->fResult);

        $this->assertEquals(1, $this->fFailureCount);
        $this->assertEquals(1, $this->fEndCount);
    }

    public function testStartStop() {
        $test = new PHPUnit_Tests_Framework_Success;
        $test->run($this->fResult);

        $this->assertEquals(1, $this->fStartCount);
        $this->assertEquals(1, $this->fEndCount);
    }
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
