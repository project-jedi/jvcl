<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "page_blocks.php";
  require_once "security_utils.php";
  
  function GetLoggedOutPage()
  {
    $tpl = new HTML_Template_IT("./"); 
  
    $tpl->loadTemplatefile("loggedout.tpl.html", true, true);
    
    SetCommonLoginStatus($tpl);
    SetCommonToolbar($tpl); 
    SetCommonFooter($tpl);
    
    // return the output
    return $tpl->get();
  }
  
  if (realpath(__FILE__) == realpath($_SERVER["SCRIPT_FILENAME"]))
    header("Location: http://".$_SERVER["HTTP_HOST"].dirname($_SERVER["PHP_SELF"])."/admin.php");

?>