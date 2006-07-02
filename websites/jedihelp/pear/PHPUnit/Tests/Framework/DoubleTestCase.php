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
// $Id: DoubleTestCase.php,v 1.2 2003/06/13 08:52:00 sebastian Exp $
//

require_once 'PHPUnit/Framework/Test.php';
require_once 'PHPUnit/Framework/TestCase.php';
require_once 'PHPUnit/Framework/TestResult.php';

class PHPUnit_Tests_Framework_DoubleTestCase implements PHPUnit_Framework_Test {
    private $fTestCase;

    public function __construct(PHPUnit_Framework_TestCase $testCase) {
        $this->fTestCase = $testCase;
    }

    public function countTestCases() {
        return 2;
    }

    public function run(PHPUnit_Framework_TestResult $result) {
        $result->startTest($this);

        $this->fTestCase->runBare();
        $this->fTestCase->runBare();

        $result->endTest($this);
    }
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
