<?php
/* vim: set expandtab tabstop=4 shiftwidth=4 softtabstop=4: */
//
// +----------------------------------------------------------------------+
// | PHP version 4.0                                                      |
// +----------------------------------------------------------------------+
// | Copyright (c) 2002 The PHP Group                                     |
// +----------------------------------------------------------------------+
// | This source file is subject to version 2.02 of the PHP license,      |
// | that is bundled with this package in the file LICENSE, and is        |
// | available at through the world-wide-web at                           |
// | http://www.php.net/license/2_02.txt.                                 |
// | If you did not receive a copy of the PHP license and are unable to   |
// | obtain it through the world-wide-web, please send a note to          |
// | license@php.net so we can mail you a copy immediately.               |
// +----------------------------------------------------------------------+
// | Authors: Jeroen Derks <jeroen@derks.it>                              |
// +----------------------------------------------------------------------+
//
// $Id: XteaTest.php,v 1.6 2002/09/03 11:04:08 jeroend Exp $

/** Xtea class */
require_once( 'Xtea.php' );
/** phpUnit classes */
require_once( 'PHPUnit/PHPUnit.php' );
/** Benchmarking */
require_once( 'Benchmark/Timer.php' );

/**
 *  Tester class for Xtea class.
 *
 *  @package    Crypt::Test
 *  @access     public
 *
 *  @version    $Revision: 1.6 $
 *  @since      2002/Aug/28
 *  @author     Jeroen Derks <jeroen@derks.it>
 */
class Crypt_XteaTest extends PHPUnit_TestCase
{
    var $obj;
    var $data;
    var $key;

    function Crypt_XteaTest($method) {
        $this->PHPUnit_TestCase($method);
    }

    function setUp() {
        $this->obj = new Crypt_Xtea;
        $this->key = '0123456789abcdeffedcba9876543210';

        //$this->data = '1'; return;
        //$this->data = '01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'; return;
        $this->data = '';
        for ($i = 0; $i < 256; ++$i) {
            $this->data .= chr($i & 0xff);
        }
        
    }

    function testIter() {
        $this->obj->setIter(36);
        $this->assertEquals(36, $this->obj->getIter());
    }

    function testCrypt() {
        $timer =& new Benchmark_Timer();
        $timer->start();
        for ( $i = 0; $i < strlen( $this->data ); ++$i )
        {
            $timer->setMarker('data');
            $data = substr( $this->data, 0, $i );
            $timer->setMarker('encryption');
            $encrypted = $this->obj->encrypt($data, $this->key);
            $timer->setMarker('decryption');
            $decrypted = $this->obj->decrypt($encrypted, $this->key);
            $timer->setMarker('assert');
            $this->assertEquals(strlen($data), strlen($decrypted));
            $this->assertEquals($data, $decrypted, "run $i failed: expected '***' (".strlen($data)."), actual '***' (".strlen($decrypted).")" );
        }
        /*
        set_time_limit(99999);
        $timer->setMarker('data');
        $data = '';
        for( $i = 0; $i < 1024 * 1024; ++$i )
            $data .= chr($i & 0xff);

        $timer->setMarker('encryption');
        $encrypted = $this->obj->encrypt($data, $this->key);
        $timer->setMarker('decryption');
        $decrypted = $this->obj->decrypt($encrypted, $this->key);
        $this->assertEquals(strlen($data), strlen($decrypted));
        $this->assertEquals($data, $decrypted, "run $i failed: expected '***' (".strlen($data)."), actual '***' (".strlen($decrypted).")" );
        */

        // make sure benchmarking output on destruction
        $timer->auto = true;
    }

    function tearDown() {
        $this->obj = NULL;
    }
}

?>
