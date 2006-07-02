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
// $Id: SimpleTestCollector.php,v 1.2 2003/08/29 07:47:02 sebastian Exp $
//

require_once 'PHPUnit/Runner/IncludePathTestCollector.php';

/**
 * An implementation of a TestCollector that considers
 * a class to be a test class when it contains the
 * pattern "Test" in its name.
 *
 * @package phpunit.runner
 * @author  Sebastian Bergmann <sb@sebastian-bergmann.de>
 */
class PHPUnit_Runner_SimpleTestCollector extends PHPUnit_Runner_IncludePathTestCollector {
    // {{{ protected function isTestClass($classFileName)

    /**
    * @param  string  $classFileName
    * @return boolean
    * @access public
    */
    protected function isTestClass($classFileName) {
    }

    // }}}
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
