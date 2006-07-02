<?php

  require_once("data_access.php");
  
  $result = DeleteItems("WHERE Id=8");
  echo $result;

?>