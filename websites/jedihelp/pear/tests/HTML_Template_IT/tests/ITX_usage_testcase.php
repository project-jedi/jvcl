<?php

require_once 'IT_usage_testcase.php';

class ITX_usage_testcase extends IT_usage_testcase
{
    function ITX_Usage_TestCase($name)
    {
        $this->IT_Usage_TestCase($name);
    }

    function setUp()
    {
        $this->tpl =& new HTML_Template_ITX('./templates');
    }
}
?>