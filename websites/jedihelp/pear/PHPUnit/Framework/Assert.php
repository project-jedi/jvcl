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
// $Id: Assert.php,v 1.6 2003/07/24 06:39:52 sebastian Exp $
//

require_once 'PHPUnit/Framework/AssertionFailedError.php';
require_once 'PHPUnit/Framework/ComparisonFailure.php';

/**
 * A set of assert methods.
 *
 * @package phpunit.framework
 * @author  Sebastian Bergmann <sb@sebastian-bergmann.de>
 */
class PHPUnit_Framework_Assert {
    // {{{ Static Members

    /**
    * @var    boolean
    * @access private
    * @static
    */
    private static $looselyTyped = false;

    // }}}
    // {{{ protected function __construct()

    /**
    * Protect constructor since it is a static only class.
    *
    * @access protected
    */
    protected function __construct() {}

    // }}}
    // {{{ public static function assertEquals($expected, $actual, $delta = 0, $message = '')

    /**
    * Asserts that two variables are equal.
    *
    * @param           mixed  $expected
    * @param           mixed  $actual
    * @param  optional mixed  $delta
    * @param  optional string $message
    * @access public
    * @static
    */
    public static function assertEquals($expected, $actual, $delta = 0, $message = '') {
        if (is_null($expected) && is_null($actual)) {
            return;
        }

        if (is_object($expected) && is_object($actual)) {
            if (serialize($expected) != serialize($actual)) {
                self::failNotEquals($expected, $actual, $message);
            }
        }

        if (is_array($expected) && is_array($actual)) {
            ksort($actual);
            ksort($expected);

            if (self::$looselyTyped) {
                $actual   = self::convertToString($actual);
                $expected = self::convertToString($expected);
            }

            self::assertEquals(serialize($expected), serialize($actual));
        }

        if (is_string($expected) && is_string($actual)) {
            if ($expected != $actual) {
                throw new PHPUnit_Framework_ComparisonFailure($expected, $actual, $message);
            }
        }

        if (is_double($expected) && is_double($actual)) {
            if (!(abs($expected - $actual) <= $delta)) {
                self::failNotEquals($expected, $actual, $message);
            }
        }

        if (is_float($expected) && is_float($actual)) {
            if (!(abs($expected - $actual) <= $delta)) {
                self::failNotEquals($expected, $actual, $message);
            }
        }

        if (is_long($expected) && is_long($actual)) {
            if ($expected != $actual) {
                self::failNotEquals($expected, $actual, $message);
            }
        }

        if (is_integer($expected) && is_integer($actual)) {
            if ($expected != $actual) {
                self::failNotEquals($expected, $actual, $message);
            }
        }

        if (is_bool($expected) && is_bool($actual)) {
            if ($expected != $actual) {
                self::failNotEquals($expected, $actual, $message);
            }
        }
    }

    // }}}
    // {{{ public static function assertTrue($condition, $message = '')

    /**
    * Asserts that a condition is true.
    *
    * @param  boolean         $condition
    * @param  optional string $message
    * @access public
    * @static
    */
    public static function assertTrue($condition, $message = '') {
        if (!$condition) {
            self::fail($message);
        }
    }

    // }}}
    // {{{ public static function assertFalse($condition, $message = '')

    /**
    * Asserts that a condition is false.
    *
    * @param  boolean         $condition
    * @param  optional string $message
    * @access public
    * @static
    */
    public static function assertFalse($condition, $message = '') {
        self::assertTrue(!$condition, $message);
    }

    // }}}
    // {{{ public static function assertNotNull($object, $message = '')

    /**
    * Asserts that an object isn't null.
    *
    * @param  object          $object
    * @param  optional string $message
    * @access public
    * @static
    */
    public static function assertNotNull($object, $message = '') {
        self::assertTrue($object !== null, $message);
    }

    // }}}
    // {{{ public static function assertNull($object, $message = '')

    /**
    * Asserts that an object is null.
    *
    * @param  object          $object
    * @param  optional string $message
    * @access public
    * @static
    */
    public static function assertNull($object, $message = '') {
        self::assertTrue($object === null, $message);
    }

    // }}}
    // {{{ public static function assertSame($expected, $actual, $message = '')

    /**
    * Asserts that two objects refer to the same object.
    *
    * @param  object          $object
    * @param  object          $object
    * @param  optional string $message
    * @access public
    * @static
    */
    public static function assertSame($expected, $actual, $message = '') {
        if ($expected !== $actual) {
            self::failNotSame($expected, $actual, $message);
        }
    }

    // }}}
    // {{{ public static function assertNotSame($expected, $actual, $message = '')

    /**
    * Asserts that two objects refer not to the same object.
    *
    * @param  object          $object
    * @param  object          $object
    * @param  optional string $message
    * @access public
    * @static
    */
    public static function assertNotSame($expected, $actual, $message = '') {
        if ($expected === $actual) {
            self::failSame($message);
        }
    }

    // }}}
    // {{{ public static function assertType($expected, $actual, $message = '')

    /**
    * Asserts that a variable is of a given type.
    *
    * @param  string          $expected
    * @param  mixed           $actual
    * @param  optional string $message
    * @access public
    * @static
    */

    public static function assertType($expected, $actual, $message = '') {
        $actual = gettype($actual);

        if ($expected != $actual) {
            self::failNotEquals(
              $expected,
              $actual,
              $message
            );
        }
    }

    // }}}
    // {{{ public static function assertRegExp($expected, $actual, $message = '')

    /**
    * Asserts that a string matches a given
    * regular expression.
    *
    * @param  string          $expected
    * @param  string          $actual
    * @param  optional string $message
    * @access public
    * @static
    */
    public static function assertRegExp($expected, $actual, $message = '') {
        if (!preg_match($expected, $actual)) {
            self::failNotEquals($expected, $actual, $message);
        }
    }

    // }}}
    // {{{ public static function fail($message = '')

    /**
    * Fails a test with the given message.
    *
    * @param  optional string $message
    * @throws PHPUnit_Framework_AssertionFailedError
    * @access public
    * @static
    */
    public static function fail($message = '') {
        throw new PHPUnit_Framework_AssertionFailedError($message);
    }

    // }}}
    // {{{ public static function format($expected, $actual, $message)

    /**
    * @param  mixed   $expected
    * @param  mixed   $actual
    * @param  string  $message
    * @access public
    * @static
    */
    public static function format($expected, $actual, $message) {
        return sprintf(
          '%s%sexpected: <%s> but was: <%s>',

          $message,
          ($message != '') ? ' ' : '',
          self::objectToString($expected),
          self::objectToString($actual)
        );
    }

    // }}}
    // {{{ public static function setLooselyTyped($looselyTyped)

    /**
    * @param  boolean $looselyTyped
    * @access public
    * @static
    */
    public static function setLooselyTyped($looselyTyped) {
        if (is_bool($looselyTyped)) {
            self::$looselyTyped = $looselyTyped;
        }
    }

    // }}}
    // {{{ private static function convertToString($value)

    /**
    * Converts a value to a string.
    *
    * @param  mixed   $value
    * @access private
    * @static
    */
    private static function convertToString($value) {
        foreach ($value as $k => $v) {
            if (is_array($v)) {
                $value[$k] = self::convertToString($value[$k]);
            } else {
                settype($value[$k], 'string');
            }
        }

        return $value;
    }

    // }}}
    // {{{ private static function failSame($message)

    /**
    * @param  string  $message
    * @throws PHPUnit_Framework_AssertionFailedError
    * @access private
    * @static
    */
    private static function failSame($message) {
        self::fail(
          sprintf(
            '%s%sexpected not same',

            $message,
            ($message != '') ? ' ' : ''
          )
        );
    }

    // }}}
    // {{{ private static function failNotSame($expected, $actual, $message)

    /**
    * @param  mixed   $expected
    * @param  mixed   $actual
    * @param  string  $message
    * @throws PHPUnit_Framework_AssertionFailedError
    * @access private
    * @static
    */
    private static function failNotSame($expected, $actual, $message) {
        self::fail(
          sprintf(
            '%s%sexpected same: <%s> was not: <%s>',

            $message,
            ($message != '') ? ' ' : '',
            self::objectToString($expected),
            self::objectToString($actual)
          )
        );
    }

    // }}}
    // {{{ private static function failNotEquals($expected, $actual, $message)

    /**
    * @param  mixed   $expected
    * @param  mixed   $actual
    * @param  string  $message
    * @throws PHPUnit_Framework_AssertionFailedError
    * @access private
    * @static
    */
    private static function failNotEquals($expected, $actual, $message) {
        self::fail(self::format($expected, $actual, $message));
    }

    // }}}
    // {{{ private static function objectToString($object)

    /**
    * @param  mixed   $object
    * @return string
    * @access private
    * @static
    */
    private static function objectToString($object) {
        if (is_array($object) || is_object($object)) {
            $object = serialize($object);
        }

        return $object;
    }

    // }}}
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
