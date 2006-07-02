<?php
  require_once "pear/HTML/Template/IT.php";
  
  $tpl = new HTML_Template_IT("./"); 
  $tpl->loadTemplatefile("test.tpl.html", true, true);
  
  $tpl->setCurrentBlock("not_logged");
  $tpl->touchBlock("not_logged");
  $tpl->parseCurrentBlock("not_logged");
  
  $tpl->Show();
?>