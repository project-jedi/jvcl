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
// $Id: AssertTest.php,v 1.2 2003/06/13 08:52:00 sebastian Exp $
//

require_once 'PHPUnit/Framework/TestCase.php';

require_once 'PHPUnit/Tests/Framework/Object.php';

class PHPUnit_Tests_Framework_AssertTest extends PHPUnit_Framework_TestCase {
    public function testFail() {
        try {
            self::fail();
        }

        catch (PHPUnit_Framework_AssertionFailedError $e) {
            return;
        }

        self::fail();
    }

    public function testAssertEquals() {
        $o = new PHPUnit_Tests_Framework_Object;

        $this->assertEquals($o, $o);
    }

    public function testAssertEqualsNull() {
        $this->assertEquals(null, null);
    }

    public function testAssertStringEquals() {
        $this->assertEquals('a', 'a');
    }

    public function testAssertNullNotEqualsString() {
    }

    public function testAssertStringNotEqualsNull() {
    }

    public function testAssertNullNotEqualsNull() {
    }

    public function testAssertNull() {
        $this->assertNull(null);

        try {
            $this->assertNull(new PHPUnit_Tests_Framework_Object);
        }

        catch (PHPUnit_Framework_AssertionFailedError $e) {
            return;
        }

        self::fail();
    }

    public function testAssertNotNull() {
        $this->assertNotNull(new PHPUnit_Tests_Framework_Object);

        try {
            $this->assertNotNull(null);
        }

        catch (PHPUnit_Framework_AssertionFailedError $e) {
            return;
        }

        self::fail();
    }

    public function testAssertTrue() {
        $this->assertTrue(true);

        try {
            $this->assertTrue(false);
        }

        catch (PHPUnit_Framework_AssertionFailedError $e) {
            return;
        }

        self::fail();
    }

    public function testAssertFalse() {
        $this->assertFalse(false);

        try {
            $this->assertFalse(true);
        }

        catch (PHPUnit_Framework_AssertionFailedError $e) {
            return;
        }

        self::fail();
    }

    public function testAssertSame() {
        $o = new PHPUnit_Tests_Framework_Object;

        $this->assertSame($o, $o);

        try {
            $this->assertSame(
              new PHPUnit_Tests_Framework_Object,
              new PHPUnit_Tests_Framework_Object
            );
        }

        catch (PHPUnit_Framework_AssertionFailedError $e) {
            return;
        }

        self::fail();
    }

    public function testAssertNotSame() {
        $this->assertNotSame(
          new PHPUnit_Tests_Framework_Object,
          null
        );

        $this->assertNotSame(
          null,
          new PHPUnit_Tests_Framework_Object
        );

        $this->assertNotSame(
          new PHPUnit_Tests_Framework_Object,
          new PHPUnit_Tests_Framework_Object
        );

        $o = new PHPUnit_Tests_Framework_Object;

        try {
            $this->assertNotSame($o, $o);
        }

        catch (PHPUnit_Framework_AssertionFailedError $e) {
            return;
        }

        self::fail();
    }

    public function testAssertNotSameFailsNull() {
        try {
            $this->assertNotSame(null, null);
        }

        catch (PHPUnit_Framework_AssertionFailedError $e) {
            return;
        }

        self::fail();
    }
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
