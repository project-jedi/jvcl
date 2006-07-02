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
// $Id: PerformanceTestCaseTest.php,v 1.3 2003/06/15 10:14:10 sebastian Exp $
//

require_once 'PHPUnit/Framework/AssertionFailedError.php';
require_once 'PHPUnit/Framework/TestCase.php';
require_once 'PHPUnit/Framework/TestResult.php';

require_once 'PHPUnit/Tests/Extensions/Sleep.php';

class PHPUnit_Tests_Extensions_PerformanceTestCaseTest extends PHPUnit_Framework_TestCase {
    public function testDoesNotExceedMaxRunningTime() {
    }

    public function testExceedsMaxRunningTime() {
    }
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
