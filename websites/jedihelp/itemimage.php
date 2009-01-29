<?php
  require_once('utils.php');
  
  $itemName = $_GET['Name'];
  $itemProjectId = $_GET['ProjectId'];
  $url = GetItemImageUrl($itemName, $itemProjectId);
  
  // For some reason, reading the bmp file on the homepages server does not
  // work, it only reads the first pixel. So we decided to upload PNG files
  // which also has the advantage to ensure fast and accurate response.
  // Note that should this ever be changed back, the GetItemImageUrl function
  // would have to be modified to return the approriate extension
  if (($itemName == '') || (($im = ImageCreateFromBMP($url)) === FALSE) )
  { 
    $im = imagecreate(1,1);
    $white = imagecolorallocate($im, 255, 255, 255);
    imagesetpixel($im, 0, 0, $white);
  }

//  $im = imagecreatefrompng($url);

  header('Content-type: image/png');
  imagepng($im);
  imagedestroy($im);
?>
