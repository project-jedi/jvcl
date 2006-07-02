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
// $Id: TestFailure.php,v 1.3 2003/07/24 06:39:52 sebastian Exp $
//

require_once 'PHPUnit/Framework/Test.php';

/**
 * A TestFailure collects a failed test together with the caught exception.
 *
 * @package phpunit.framework
 * @author  Sebastian Bergmann <sb@sebastian-bergmann.de>
 */
class PHPUnit_Framework_TestFailure {
    // {{{ Members

    /**
    * @var    PHPUnit_Framework_Test
    * @access protected
    */
    protected $fFailedTest;

    /**
    * @var    Exception
    * @access protected
    */
    protected $fThrownException;

    // }}}
    // {{{ public function __construct(PHPUnit_Framework_Test $failedTest, Exception $thrownException)

    /**
    * Constructs a TestFailure with the given test and exception.
    *
    * @param  PHPUnit_Framework_Test  $failedTest
    * @param  Exception               $thrownException
    * @access public
    */
    public function __construct(PHPUnit_Framework_Test $failedTest, Exception $thrownException) {
        $this->fFailedTest      = $failedTest;
        $this->fThrownException = $thrownException;
    }

    // }}}
    // {{{ public function failedTest()

    /**
    * Gets the failed test.
    *
    * @return Test
    * @access public
    */
    public function failedTest() {
        return $this->fFailedTest;
    }

    // }}}
    // {{{ public function thrownException()

    /**
    * Gets the thrown exception.
    *
    * @return Exception
    * @access public
    */
    public function thrownException() {
        return $this->fThrownException;
    }

    // }}}
    // {{{ public function toString()

    /**
    * Returns a short description of the failure.
    *
    * @return string
    * @access public
    */
    public function toString() {
        return sprintf(
          '%s: %s',

          $this->fFailedTest->toString(),
          $this->fThrownException->getMessage()
        );
    }

    // }}}
    // {{{ public function exceptionMessage()

    /**
    * Returns the exception's message.
    *
    * @return string
    * @access public
    */
    public function exceptionMessage() {
        return $this->thrownException()->getMessage();
    }

    // }}}
    // {{{ public function isFailure()

    /**
    * Returns TRUE if the thrown exception
    * is of type AssertionFailedError.
    *
    * @return boolean
    * @access public
    */
    public function isFailure() {
        return ($this->thrownException() instanceof PHPUnit_Framework_AssertionFailedError);
    }

    // }}}
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
