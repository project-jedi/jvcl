<?php

class IT_api_TestCase extends PHPUnit_TestCase
{
   /**
    * An HTML_Template_IT object
    * @var object
    */
    var $tpl;

    function IT_api_TestCase($name)
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
    * Tests a setTemplate method 
    *
    */
    function testSetTemplate()
    {
        $result = $this->tpl->setTemplate('A template', false, false);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error setting template: '. $result->getMessage());
        }
        $this->assertEquals('A template', $this->tpl->get());
    }

   /**
    * Tests a loadTemplatefile method 
    *
    */
    function testLoadTemplatefile()
    {
        $result = $this->tpl->loadTemplatefile('loadtemplatefile.html', false, false);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error loading template file: '. $result->getMessage());
        }
        $this->assertEquals('A template', trim($this->tpl->get()));
    }

   /**
    * Tests a setVariable method
    *
    */
    function testSetVariable()
    {
        $result = $this->tpl->setTemplate('{placeholder1} {placeholder2} {placeholder3}', true, true);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error setting template: '. $result->getMessage());
        }
        // "scalar" call
        $this->tpl->setVariable('placeholder1', 'var1');
        // array call
        $this->tpl->setVariable(array(
            'placeholder2' => 'var2',
            'placeholder3' => 'var3'
        ));
        $this->assertEquals('var1 var2 var3', $this->tpl->get());
    }

   /**
    * Tests the <!-- INCLUDE --> functionality 
    *
    */
    function testInclude()
    {
        $result = $this->tpl->loadTemplateFile('include.html', false, false);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error loading template file: '. $result->getMessage());
        }
        $this->assertEquals('Master file; Included file', trim($this->tpl->get()));
    }

   /**
    *
    */
    function testCurrentBlock()
    {
        $result = $this->tpl->loadTemplateFile('blockiteration.html', true, true);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error loading template file: '. $result->getMessage());
        }
        $this->tpl->setVariable('outer', 'a');
        $this->tpl->setCurrentBlock('inner_block');
        for ($i = 0; $i < 5; $i++) {
            $this->tpl->setVariable('inner', $i + 1);
            $this->tpl->parseCurrentBlock();
        } // for
        $this->assertEquals('a|1|2|3|4|5#', $this->_stripWhitespace($this->tpl->get()));
    }

   /**
    *
    */
    function testRemovePlaceholders()
    {
        $result = $this->tpl->setTemplate('{placeholder1},{placeholder2},{placeholder3}', true, true);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error setting template: '. $result->getMessage());
        }
        // we do not set {placeholder3}
        $this->tpl->setVariable(array(
            'placeholder1' => 'var1',
            'placeholder2' => 'var2'
        ));
        $this->assertEquals('var1,var2,', $this->tpl->get());

        // Now, we should really add a switch for keeping {stuff} in
        // data supplied to setVariable() safe. Until then, removing it should
        // be expected behaviour
        $result = $this->tpl->setTemplate('{placeholder1},{placeholder2},{placeholder3}', true, true);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error setting template: '. $result->getMessage());
        }
        $this->tpl->setVariable(array(
            'placeholder1' => 'var1',
            'placeholder2' => 'var2',
            'placeholder3' => 'var3{stuff}'
        ));
        $this->assertEquals('var1,var2,var3', $this->tpl->get());
    }

   /**
    *
    */
    function testTouchBlock()
    {
        $result = $this->tpl->loadTemplateFile('blockiteration.html', false, true);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error loading template file: '. $result->getMessage());
        }
        $this->tpl->setVariable('outer', 'data');
        // inner_block should be preserved in output, even if empty
        $this->tpl->touchBlock('inner_block');
        $this->assertEquals('data|{inner}#', $this->_stripWhitespace($this->tpl->get()));
    }
   
    // Not available in stock class

   /**
    *
    */
    /*
    function testHideBlock()
    {
        if (!$this->_methodExists('hideBlock')) {
            return;
        }
        $result = $this->tpl->loadTemplateFile('blockiteration.html', false, true);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error loading template file: '. $result->getMessage());
        }
        $this->tpl->setVariable(array(
            'outer' => 'data',
            'inner' => 'stuff'
        ));
        // inner_block is not empty, but should be removed nonetheless
        $this->tpl->hideBlock('inner_block');
        $this->assertEquals('data#', $this->_stripWhitespace($this->tpl->get()));
    }
	*/
   /**
    *
    */
    /*
	function testSetGlobalVariable()
    {
        if (!$this->_methodExists('setGlobalVariable')) {
            return;
        }
        $result = $this->tpl->loadTemplateFile('globals.html', false, true);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error loading template file: '. $result->getMessage());
        }
        $this->tpl->setGlobalVariable('glob', 'glob');
        // {var2} is not, block_two should be removed
        $this->tpl->setVariable(array(
            'var1' => 'one',
            'var3' => 'three'
        ));
        for ($i = 0; $i < 3; $i++) {
            $this->tpl->setVariable('var4', $i + 1);
            $this->tpl->parse('block_four');
        } // for
        $this->assertEquals('glob:one#glob:three|glob:1|glob:2|glob:3#', $this->_stripWhitespace($this->tpl->get()));
    }
	*/
}

?>
