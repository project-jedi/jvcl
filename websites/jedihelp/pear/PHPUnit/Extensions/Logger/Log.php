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
// $Id: Log.php,v 1.4 2003/07/24 06:39:52 sebastian Exp $
//

require_once 'PHPUnit/Framework/TestFailure.php';
require_once 'PHPUnit/Framework/TestListener.php';
require_once 'PHPUnit/Framework/TestResult.php';

require_once 'Log.php';

/**
 * @package phpunit.extensions.logger
 * @author  Sebastian Bergmann <sb@sebastian-bergmann.de>
 */
class PHPUnit_Extensions_Logger_Log implements PHPUnit_Framework_TestListener {
    // {{{ Members

    /**
    * Log.
    *
    * @var    Log
    * @access private
    */
    private $log;

    // }}}
    // {{{ public function __construct($type, $name = '', $ident = '', $conf = array(), $maxLevel = PEAR_LOG_DEBUG)

    /**
    * @param string $type      The type of concrete Log subclass to use.
    *                          Currently, valid values are 'console',
    *                          'syslog', 'sql', 'file', and 'mcal'.
    * @param string $name      The name of the actually log file, table, or
    *                          other specific store to use. Defaults to an
    *                          empty string, with which the subclass will
    *                          attempt to do something intelligent.
    * @param string $ident     The identity reported to the log system.
    * @param array  $conf      A hash containing any additional configuration
    *                          information that a subclass might need.
    * @param int $maxLevel     Maximum priority level at which to log.
    * @access public
    */
    public function __construct($type, $name = '', $ident = '', $conf = array(), $maxLevel = PEAR_LOG_DEBUG) {
        $this->log = Log::factory($type, $name, $ident, $conf, $maxLevel);
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
    }

    // }}}
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
