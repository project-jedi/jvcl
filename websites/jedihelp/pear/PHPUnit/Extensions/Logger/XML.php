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
// $Id: XML.php,v 1.4 2003/07/24 06:39:52 sebastian Exp $
//

require_once 'PHPUnit/Framework/TestFailure.php';
require_once 'PHPUnit/Framework/TestListener.php';
require_once 'PHPUnit/Framework/TestResult.php';

require_once 'XML/Tree.php';

/**
 * A TestListener that generates an XML-based logfile
 * of the test execution.
 *
 * The XML markup is based upon the one used by the
 * Artima SuiteRunner, see http://www.artima.com/suiterunner/
 * for details.
 *
 * @package phpunit.extensions.logger
 * @author  Sebastian Bergmann <sb@sebastian-bergmann.de>
 */
class PHPUnit_Extensions_Logger_XML implements PHPUnit_Framework_TestListener {
    // {{{ Members

    /**
    * Filename.
    *
    * @var    string
    * @access private
    */
    private $file;

    /**
    * XML Tree.
    *
    * @var    XML_Tree
    * @access private
    */
    private $tree;

    /**
    * XML Tree Root Node.
    *
    * @var    XML_Tree_Node
    * @access private
    */
    private $xml;

    /**
    * @var    boolean
    * @access private
    */
    private $testFailed = false;

    // }}}
    // {{{ public function __construct($file = 'phpunit-log.xml')

    /**
    * @param  optional string $file
    * @access public
    */
    public function __construct($file = 'phpunit-log.xml') {
        $this->file = $file;
        $this->tree = new XML_Tree;
        $this->root = $this->tree->addRoot('run');
    }

    // }}}
    // {{{ public function __destruct()

    /**
    * @access public
    */
    public function __destruct() {
        if ($fp = @fopen($this->file, 'w')) {
            @fwrite($fp, $this->root->get());
            @fclose($fp);
        }
    }

    // }}}
    // {{{ public function addError(PHPUnit_Framework_Test $test, Exception $e)

    /**
    * An error occurred.
    *
    * @param  PHPUnit_Framework_Test  $test
    * @param  Exception               $e
    * @access public
    */
    public function addError(PHPUnit_Framework_Test $test, Exception $e) {
        $this->testFailed($test, $e);
    }

    // }}}
    // {{{ public function addFailure(PHPUnit_Framework_Test $test, PHPUnit_Framework_AssertionFailedError $e)

    /**
    * A failure occurred.
    *
    * @param  PHPUnit_Framework_Test                 $test
    * @param  PHPUnit_Framework_AssertionFailedError $e
    * @access public
    */
    public function addFailure(PHPUnit_Framework_Test $test, PHPUnit_Framework_AssertionFailedError $e) {
        $this->testFailed($test, $e);
    }

    // }}}
    // {{{ public function endTest(PHPUnit_Framework_Test $test)

    /**
    * A test ended.
    *
    * @param  PHPUnit_Framework_Test $test
    * @access public
    */
    public function endTest(PHPUnit_Framework_Test $test) {
        if (!$this->testFailed) {
            $testSucceeded = $this->root->addChild('testSucceeded');

            $testSucceeded->addChild('name', $test->getName());
            $this->addDate($testSucceeded);
        }
    }

    // }}}
    // {{{ public function startTest(PHPUnit_Framework_Test $test)

    /**
    * A test started.
    *
    * @param  PHPUnit_Framework_Test $test
    * @access public
    */
    public function startTest(PHPUnit_Framework_Test $test) {
        $testStarting = $this->root->addChild('testStarting');

        $testStarting->addChild('name', $test->getName());
        $this->addDate($testStarting);

        $this->testFailed = false;
    }

    // }}}
    // {{{ private function testFailed(PHPUnit_Framework_Test $test, Exception $e)

    /**
    * A test failed.
    *
    * @param  PHPUnit_Framework_Test  $test
    * @param  Exception               $e
    * @access private
    */
    private function testFailed(PHPUnit_Framework_Test $test, Exception $e) {
        $testFailed = $this->root->addChild('testFailed');

        $testFailed->addChild('name', $test->getName());
        $testFailed->addChild('message', $e->getMessage());
        $this->addDate($testFailed);

        $this->testFailed = true;
    }

    // }}}
    // {{{ private function addDate(XML_Tree_Node $node)

    /**
    * @param  XML_Tree_Node $node
    * @access private
    */
    private function addDate(XML_Tree_Node $node) {
        $node->addChild('date', date('D M m H:i:s T Y'));
    }

    // }}}
}

/*
 * vim600:  et sw=2 ts=2 fdm=marker
 * vim<600: et sw=2 ts=2
 */
?>
