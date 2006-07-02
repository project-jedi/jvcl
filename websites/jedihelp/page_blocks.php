<?php
  if (realpath(__FILE__) == realpath($_SERVER["SCRIPT_FILENAME"]))
    header("Location: http://".$_SERVER["HTTP_HOST"].dirname($_SERVER["PHP_SELF"])."/index.php");

  require_once "pear/HTML/Template/IT.php";
  require_once "data_access.php";
  require_once "security_utils.php";
  require_once "utils.php";
  
  function SetCommonLoginStatus(&$tpl, $paramArray = array())
  {
    $toolbar_tpl = new HTML_Template_IT("./"); 
  
    $toolbar_tpl->loadTemplatefile("common_loginstatus.tpl.html", true, true);
    
    
    // parse the section depending on the current state of the user
    if (!IsLogged())
    {
      // not logged, calculate return page
      $returnPage = $_SERVER['PHP_SELF'];
      $getCount = count($_GET);
      if ($getCount > 0)
      {
        $returnPage .= "?";
        $i=0;
        foreach (array_keys($_GET) as $getParam)
        {
          $returnPage .= $getParam."=".$_GET[$getParam];
          if ($i < $getCount-1)
            $returnPage .= "&"; 
          $i++;
        }
      }       
      
      $toolbar_tpl->setCurrentBlock("not_logged");
      $toolbar_tpl->setVariable("RETURN_PAGE", urlencode($returnPage));
      $toolbar_tpl->parseCurrentBlock("not_logged");
    } 
    else
    {
      if (array_key_exists("ItemId", $paramArray))
        $projectId = GetProjectIdForItem($paramArray["ItemId"]);
      elseif (array_key_exists("UnitId", $paramArray))
        $projectId = GetProjectIdForUnit($paramArray["UnitId"]);
      elseif (array_key_exists("ProjectId", $paramArray))
        $projectId = $paramArray["ProjectId"];
      else
        $projectId = ""; 
      
      if ($projectId == "")
      {
        // no project Id, we simply indicate the logged in state
        $toolbar_tpl->setCurrentBlock("logged");
        $toolbar_tpl->setVariable("USERNAME", GetLoggedUserName());
        $toolbar_tpl->parseCurrentBlock("logged");
      }
      else
      {
        if (!IsWriter($projectId))
        {
          // no write access to project
          $toolbar_tpl->setCurrentBlock("not_allowed");
        $toolbar_tpl->setVariable("USERNAME", GetLoggedUserName());
          $toolbar_tpl->parseCurrentBlock("not_allowed");
        }
        else
        {
          // full write access
          $toolbar_tpl->setCurrentBlock("logged_and_write");
        $toolbar_tpl->setVariable("USERNAME", GetLoggedUserName());
          $toolbar_tpl->parseCurrentBlock("logged_and_write");
        }
      }
    }
    
    $tpl->setVariable("COMMON_LOGIN_STATUS", $toolbar_tpl->get());
  } 
  
  function SetCommonToolbar(&$tpl)
  {
    $toolbar_tpl = new HTML_Template_IT("./"); 
    $toolbar_tpl->loadTemplatefile("common_toolbar.tpl.html", true, false);
    $tpl->setVariable("COMMON_TOOLBAR", $toolbar_tpl->get());
  }
  
  function SetCommonFooter(&$tpl)
  {
    $footer_tpl = new HTML_Template_IT("./"); 
    $footer_tpl->loadTemplatefile("common_footer.tpl.html", true, false);
    $tpl->setVariable("COMMON_FOOTER", $footer_tpl->get());
  }
  
  function SetAdminToolbar(&$tpl)
  {
    $toolbar_tpl = new HTML_Template_IT("./"); 
    $toolbar_tpl->loadTemplatefile("admin_toolbar.tpl.html", true, false);
    $tpl->setVariable("ADMIN_TOOLBAR", $toolbar_tpl->get());
  }
  
  function SetLastChanged(&$tpl, $userId, $LastChange)
  {
    $last_changed_tpl = new HTML_Template_IT("./"); 
    $last_changed_tpl->loadTemplatefile("last_changed.tpl.html", true, false);
    if ($userId == -1)
      $userName = "Unknown";
    else
    {
      $userInfos = GetUserInfosById($userId);
      $userName = $userInfos["username"]; 
    }
    // format date
    $LastChange = date("Y-m-d H:i:s T", MySQLTimeStampToUnixTimeStamp($LastChange));
    
    // do replacements
    $last_changed_tpl->setVariable("USER_NAME", $userName);
    $last_changed_tpl->setVariable("LAST_CHANGE", $LastChange);
    $tpl->setVariable("LAST_CHANGED", $last_changed_tpl->get());
  }
  
  if (realpath(__FILE__) == realpath($_SERVER["SCRIPT_FILENAME"]))
    header("Location: http://".$_SERVER["HTTP_HOST"].dirname($_SERVER["PHP_SELF"])."/index.php");

?>