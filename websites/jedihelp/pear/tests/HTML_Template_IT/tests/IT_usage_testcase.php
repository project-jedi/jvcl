<?php

class IT_Usage_TestCase extends PHPUnit_TestCase
{
   /**
    * An HTML_Template_IT object
    * @var object
    */
    var $tpl;

    function IT_Usage_TestCase($name)
    {
        $this->PHPUnit_TestCase($name);
    }

    function setUp()
    {
        $this->tpl =& new HTML_Template_IT('./templates');
    }

    function tearDown()
    {
        unset($this->tpl);
    }

    function _stripWhitespace($str)
    {
        return preg_replace('/\\s+/', '', $str);
    }

    function _methodExists($name) 
    {
        if (in_array(strtolower($name), get_class_methods($this->tpl))) {
            return true;
        }
        $this->assertTrue(false, 'method '. $name . ' not implemented in ' . get_class($this->tpl));
        return false;
    }


   /**
    * Tests iterations over two blocks
    *
    */
    function testBlockIteration()
    {
        $data = array(
            'a',
            array('b', array('1', '2', '3', '4')),
            'c',
            array('d', array('5', '6', '7'))
        );
        
        $result = $this->tpl->loadTemplateFile('blockiteration.html', true, true);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error loading template file: '. $result->getMessage());
        }
        foreach ($data as $value) {
            if (is_array($value)) {
                $this->tpl->setVariable('outer', $value[0]);
                foreach ($value[1] as $v) {
                    $this->tpl->setVariable('inner', $v);
                    $this->tpl->parse('inner_block');
                }
            } else {
                $this->tpl->setVariable('outer', $value);
            }
            $this->tpl->parse('outer_block');
        }
        $this->assertEquals('a#b|1|2|3|4#c#d|5|6|7#', $this->_stripWhitespace($this->tpl->get()));
    }

   /**
    * 
    *
    */
    function testTouchBlockIteration()
    {
        $data = array('a','b','c','d','e');
        $result = $this->tpl->loadTemplateFile('blockiteration.html', true, true);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error loading template file: '. $result->getMessage());
        }
        for ($i = 0; $i < count($data); $i++) {
            $this->tpl->setVariable('outer', $data[$i]);
            // the inner_block is empty and should be removed
            if (0 == $i % 2) {
                $this->tpl->touchBlock('inner_block');
            }
            $this->tpl->parse('outer_block');
        }
        $this->assertEquals('a|#b#c|#d#e|#', $this->_stripWhitespace($this->tpl->get()));
    }

    // Not available in stock version

   /**
    *
    */
	/*
    function testHideBlockIteration()
    {
        if (!$this->_methodExists('hideBlock')) {
            return;
        }
        $data = array('a','b','c','d','e');
        $result = $this->tpl->loadTemplateFile('blockiteration.html', true, true);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error loading template file: '. $result->getMessage());
        }
        for ($i = 0; $i < count($data); $i++) {
            $this->tpl->setVariable(array(
                'inner' => $i + 1,
                'outer' => $data[$i]
            ));
            // the inner_block is not empty, but should be removed
            if (0 == $i % 2) {
                $this->tpl->hideBlock('inner_block');
            }
            $this->tpl->parse('outer_block');
        }
        $this->assertEquals('a#b|2#c#d|4#e#', $this->_stripWhitespace($this->tpl->get()));
    }
	*/
}
?>
