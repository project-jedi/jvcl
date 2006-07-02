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
// $Id: DoublePrecisionAssertTest.php,v 1.2 2003/06/13 08:52:00 sebastian Exp $
//

require_once 'PHPUnit/Framework/TestCase.php';

class PHPUnit_Tests_Framework_DoublePrecisionAssertTest extends PHPUnit_Framework_TestCase {
    public function testAssertEqualsNaNFails() {
    }

    public function testAssertNaNEqualsFails() {
    }

    public function testAssertNaNEqualsNaNFails() {
    }

    public function testAssertPosInfinityNotEqualsNegInfinity() {
    }

    public function testAssertPosInfinityNotEquals() {
    }

    public function testAssertPosInfinityEqualsInfinity() {
    }

    public function testAssertNegInfinityEqualsInfinity() {
    }
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
