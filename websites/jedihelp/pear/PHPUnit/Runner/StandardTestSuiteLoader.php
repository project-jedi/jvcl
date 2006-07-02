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
// $Id: StandardTestSuiteLoader.php,v 1.3 2003/08/28 11:58:18 sebastian Exp $
//

require_once 'PHPUnit/Runner/TestSuiteLoader.php';

/**
 * An interface to define how a test suite should be loaded.
 *
 * @package phpunit.runner
 * @author  Sebastian Bergmann <sb@sebastian-bergmann.de>
 */
class PHPUnit_Runner_StandardTestSuiteLoader extends PHPUnit_Runner_TestSuiteLoader {
    // {{{ public function load($suiteClassName)

    /**
    * @param  string  $testSuiteName
    * @return Reflection_Class
    * @access public
    */
    public function load($suiteClassName) {
        include_once $suiteClassName . '.php';

        return new Reflection_Class($suiteClassName);
    }

    // }}}
    // {{{ public function reload(Reflection_Class $aClass)

    /**
    * @param  Reflection_Class  $aClass
    * @return Reflection_Class
    * @access public
    */
    public function reload(Reflection_Class $aClass) {
        return $aClass;
    }

    // }}}
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
