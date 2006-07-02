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
// $Id: IncludePathTestCollector.php,v 1.1 2003/07/25 05:01:45 sebastian Exp $
//

require_once 'PHPUnit/Runner/TestCollector.php';

/**
 * An implementation of a TestCollector that consults the
 * class path. It leaves it up to subclasses to decide
 * whether a class is a runnable Test.
 *
 * @package phpunit.runner
 * @author  Sebastian Bergmann <sb@sebastian-bergmann.de>
 */
abstract class PHPUnit_Runner_IncludePathTestCollector implements PHPUnit_Runner_TestCollector {
    // {{{ public function collectTests()

    /**
    * @return array
    * @access public
    */
    public function collectTests() {
    }

    // }}}
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
