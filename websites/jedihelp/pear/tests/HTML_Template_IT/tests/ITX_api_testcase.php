<?php

require_once 'IT_api_testcase.php';

function _uppercaseCallback($ary)
{
    return strtoupper($ary[0]);
}


class ITX_api_TestCase extends IT_api_TestCase
{
    function ITX_api_TestCase($name)
    {
        $this->IT_api_TestCase($name);
    }

    function setUp()
    {
        $this->tpl =& new HTML_Template_ITX('./templates');
    }

    function testPlaceholderExists()
    {
        $this->tpl->setTemplate('{var}');
        $this->assertTrue($this->tpl->placeholderExists('var'), 'Existing placeholder \'var\' reported as nonexistant');
        $this->assertTrue(!$this->tpl->placeholderExists('foobar'), 'Nonexistant placeholder \'foobar\' reported as existing');
        $this->assertTrue($this->tpl->placeholderExists('var', '__global__'), 'Existing in block \'__global__\' placeholder \'var\' reported as nonexistant');
        $this->assertTrue(!$this->tpl->placeholderExists('foobar', '__global__'), 'Nonexistant in block \'__global__\' placeholder \'foobar\' reported as existing');
    }

    function testBlockExists()
    {
        $this->tpl->setTemplate('{var}');
        $this->assertTrue($this->tpl->blockExists('__global__'), 'Existing block \'__global__\' reported as nonexistant');
        $this->assertTrue(!$this->tpl->blockExists('foobar'), 'Nonexistant block \'foobar\' reported as existing');
    }

    function testAddBlock()
    {
        $result = $this->tpl->loadTemplatefile('blocks.html', true, true);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error loading template file: '. $result->getMessage());
        }
        $this->tpl->addBlock('var', 'added', 'added:{new_var}');
        $this->assertTrue($this->tpl->blockExists('added'), 'The new block seems to be missing');
        $this->assertTrue(!$this->tpl->placeholderExists('var'), 'The old variable seems to be still present in the template');
        $this->tpl->setVariable('new_var', 'new_value');
        $this->assertEquals('added:new_value', $this->_stripWhitespace($this->tpl->get()));
    }

    function testAddBlockfile()
    {
        $result = $this->tpl->loadTemplatefile('blocks.html', true, true);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error loading template file: '. $result->getMessage());
        }
        $result = $this->tpl->addBlockfile('var', 'added', 'addblock.html');
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error adding block from file: '. $result->getMessage());
        }
        $this->assertTrue($this->tpl->blockExists('added'), 'The new block seems to be missing');
        $this->assertTrue(!$this->tpl->placeholderExists('var'), 'The old variable seems to be still present in the template');
        $this->tpl->setVariable('new_var', 'new_value');
        $this->assertEquals('added:new_value', $this->_stripWhitespace($this->tpl->get()));
    }

    function testReplaceBlock()
    {
        $result = $this->tpl->loadTemplatefile('blocks.html', true, true);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error loading template file: '. $result->getMessage());
        }
        $this->tpl->setVariable('old_var', 'old_value');
        $this->tpl->parse('old_block');
        // old_block's contents should be discarded
        $this->tpl->replaceBlock('old_block', 'replaced:{replaced_var}#', false);
        $this->assertTrue(!$this->tpl->blockExists('old_inner_block') && !$this->tpl->placeholderExists('old_var'),
                          'The replaced block\'s contents seem to be still present');
        $this->tpl->setVariable('replaced_var', 'replaced_value');
        $this->tpl->parse('old_block');
        // this time old_block's contents should be preserved
        $this->tpl->replaceBlock('old_block', 'replaced_again:{brand_new_var}', true);
        $this->tpl->setVariable('brand_new_var', 'brand_new_value');
        $this->assertEquals('replaced:replaced_value#replaced_again:brand_new_value', $this->_stripWhitespace($this->tpl->get()));
    }

    function testReplaceBlockfile()
    {
        $result = $this->tpl->loadTemplatefile('blocks.html', true, true);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error loading template file: '. $result->getMessage());
        }
        $this->tpl->setVariable('old_var', 'old_value');
        $this->tpl->parse('old_block');
        // old_block's contents should be discarded
        $result = $this->tpl->replaceBlockfile('old_block', 'replaceblock.html', false);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error replacing block from file: '. $result->getMessage());
        }
        $this->assertTrue(!$this->tpl->blockExists('old_inner_block') && !$this->tpl->placeholderExists('old_var'),
                          'The replaced block\'s contents seem to be still present');
        $this->tpl->setVariable(array(
            'replaced_var'       => 'replaced_value',
            'replaced_inner_var' => 'inner_value'
        ));
        $this->tpl->parse('old_block');
        // this time old_block's contents should be preserved
        $result = $this->tpl->replaceBlockfile('old_block', 'addblock.html', true);
        if (PEAR::isError($result)) {
            $this->assertTrue(false, 'Error replacing block from file: '. $result->getMessage());
        }
        $this->tpl->setVariable('new_var', 'again');
        $this->assertEquals('replaced:replaced_value|inner_value#added:again', $this->_stripWhitespace($this->tpl->get()));
    }

    function testCallback()
    {
        $this->tpl->setTemplate('callback:func_uppercase(word)');
        $this->tpl->setCallbackFunction('uppercase', '_uppercaseCallback');
        $res = $this->tpl->performCallback();
        if (PEAR::isError($res)) {
            $this->assertTrue(false, 'Error performing callback: '. $res->getMessage());
        }
        $this->assertEquals('callback:WORD', $this->tpl->get());
    }
}

?>
