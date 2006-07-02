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
// $Id: PerformanceTestCase.php,v 1.3 2003/07/24 06:39:52 sebastian Exp $
//

require_once 'Benchmark/Timer.php';
require_once 'PHPUnit/Framework/Assert.php';
require_once 'PHPUnit/Framework/TestCase.php';

/**
 * A TestCase that expects a TestCase to be executed
 * meeting a given time limit.
 *
 * @package phpunit.extensions
 * @author  Sebastian Bergmann <sb@sebastian-bergmann.de>
 */
class PHPUnit_Extensions_PerformanceTestCase extends PHPUnit_Framework_TestCase {
    // {{{ Members

    /**
    * @var    double
    * @access private
    */
    private $fMaxRunningTime = 0;

    // }}}
    // {{{ public function __construct($name, $maxRunningTime = 0)

    /**
    * @param  string $name
    * @param  double $maxRunningTime
    * @access public
    */
    public function __construct($name, $maxRunningTime = 0) {
        parent::__construct($name);
        $this->fMaxRunningTime = $maxRunningTime;
    }

    // }}}
    // {{{ protected function runTest()

    /**
    * @access public
    */
    protected function runTest() {
        $timer = new Benchmark_Timer;

        $timer->start();
        parent::runTest();
        $timer->stop();

        if ($this->fMaxRunningTime != 0 &&
            $timer->timeElapsed() > $this->fMaxRunningTime) {
            PHPUnit_Framework_Assert::fail(
              sprintf(
                'expected running time: <= %s but was: %s',

                $this->fMaxRunningTime,
                $timer->timeElapsed()
              )
            );
        }
    }

    // }}}
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
