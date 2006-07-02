<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "page_blocks.php";
  
  function GetNotAllowedPage($reason)
  {
    $tpl = new HTML_Template_IT("./"); 
  
    $tpl->loadTemplatefile("not_allowed.tpl.html", true, false);
    
    SetCommonLoginStatus($tpl);
    SetAdminToolbar($tpl);
    SetCommonFooter($tpl); 
    
    $tpl->SetVariable("REASON", $reason);
    
    // print the output
    return $tpl->get();
  }
  
  if (realpath(__FILE__) == realpath($_SERVER["SCRIPT_FILENAME"]))
    header("Location: http://".$_SERVER["HTTP_HOST"].dirname($_SERVER["PHP_SELF"])."/index.php");

?>