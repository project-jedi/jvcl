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
// $Id: TestSetup.php,v 1.3 2003/07/24 06:39:52 sebastian Exp $
//

require_once 'PHPUnit/Framework/TestResult.php';
require_once 'PHPUnit/Extensions/TestDecorator.php';

/**
 * A Decorator to set up and tear down additional fixture state.
 * Subclass TestSetup and insert it into your tests when you want
 * to set up additional state once before the tests are run.
 *
 * @package phpunit.extensions
 * @author  Sebastian Bergmann <sb@sebastian-bergmann.de>
 */
class PHPUnit_Extensions_TestSetup extends PHPUnit_Extensions_TestDecorator {
    // {{{ public function run(PHPUnit_TestResult $result)

    /**
    * Runs the decorated test and collects the
    * result in a TestResult.
    *
    * @param  PHPUnit_Framework_TestResult $result
    * @access public
    */
    public function run(PHPUnit_Framework_TestResult $result) {
        $this->setUp();
        $this->basicRun($result);
        $this->tearDown();
    }

    // }}}
    // {{{ protected function setUp()

    /**
    * Sets up the fixture. Override to set up additional fixture
    * state.
    *
    * @access protected
    */
    protected function setUp() {}

    // }}}
    // {{{ protected function tearDown()

    /**
    * Tears down the fixture. Override to tear down the additional
    * fixture state.
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
