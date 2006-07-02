<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "page_blocks.php";
  require_once "data_access.php";
  
  function GetNoAccessToProjectPage($projectId)
  {
    $tpl = new HTML_Template_IT("./"); 
  
    $tpl->loadTemplatefile("no_access_to_project.tpl.html", true, false);
    
    SetCommonLoginStatus($tpl);
    SetAdminToolbar($tpl);
    SetCommonFooter($tpl); 
    
    $projectInfos = GetProjectInfos($projectId);
    $tpl->setVariable("PROJECT_NAME", $projectInfos["Name"]);
    
    // print the output
    return $tpl->get();
  }
  
  if (realpath(__FILE__) == realpath($_SERVER["SCRIPT_FILENAME"]))
    header("Location: http://".$_SERVER["HTTP_HOST"].dirname($_SERVER["PHP_SELF"])."/admin.php");

?>