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
// $Id: RepeatedTest.php,v 1.3 2003/07/24 06:39:52 sebastian Exp $
//

require_once 'PHPUnit/Framework/Test.php';
require_once 'PHPUnit/Framework/TestResult.php';
require_once 'PHPUnit/Extensions/TestDecorator.php';

/**
 * A Decorator that runs a test repeatedly.
 *
 * @package phpunit.extensions
 * @author  Sebastian Bergmann <sb@sebastian-bergmann.de>
 */
class PHPUnit_Extensions_RepeatedTest extends PHPUnit_Extensions_TestDecorator {
    // {{{ Members

    /**
    * @var    integer
    * @access private
    */
    private $timesRepeat = 1;

    // }}}
    // {{{ public function __construct(PHPUnit_Framework_Test $test, $timesRepeat = 1)

    /**
    * Constructor.
    *
    * @param  PHPUnit_Framework_Test  $test
    * @param  integer                 $timesRepeat
    * @access public
    */
    public function __construct(PHPUnit_Framework_Test $test, $timesRepeat = 1) {
        parent::__construct($test);
        $this->timesRepeat = $timesRepeat;
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
        return $this->timesRepeat * $this->test->countTestCases();
    }

    // }}}
    // {{{ public function run(PHPUnit_Framework_TestResult $result)

    /**
    * Runs the decorated test and collects the
    * result in a TestResult.
    *
    * @param  PHPUnit_Framework_TestResult $result
    * @access public
    */
    public function run(PHPUnit_Framework_TestResult $result) {
        for ($i = 0; $i < $this->timesRepeat && !$result->shouldStop(); $i++) {
            $this->test->run($result);
        }
    }

    // }}}
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
