<?php
  require_once('utils.php');
  
  $itemName = $_GET['Name'];
  $url = GetItemImageUrl($itemName);
  
  if (($itemName == '') || (! $im = ImageCreateFromBMP($url)))
  { 
    $im = imagecreatetruecolor(1,1);
    $white = imagecolorallocate($im, 255, 255, 255);
    imagesetpixel($im, 0, 0, $white);
  }

  header('Content-type: image/png');
  imagepng($im);
  imagedestroy($im);
?>
