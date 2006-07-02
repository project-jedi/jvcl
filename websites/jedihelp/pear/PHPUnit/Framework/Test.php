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
// $Id: Test.php,v 1.3 2003/07/24 06:39:52 sebastian Exp $
//

/**
 * A Test can be run and collect its results.
 *
 * @package phpunit.framework
 * @author  Sebastian Bergmann <sb@sebastian-bergmann.de>
 */
interface PHPUnit_Framework_Test {
    // {{{ public function countTestCases()

    /**
    * Counts the number of test cases that will be run by this test.
    *
    * @return integer
    * @access public
    */
    public function countTestCases();

    // }}}
    // {{{ public function run(PHPUnit_Framework_TestResult $result)

    /**
    * Runs a test and collects its result in a TestResult instance.
    *
    * @param  PHPUnit_Framework_TestResult $result
    * @access public
    */
    public function run(PHPUnit_Framework_TestResult $result);

    // }}}
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
