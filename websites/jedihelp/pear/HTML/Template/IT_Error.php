<?php
// +----------------------------------------------------------------------+
// | PHP Version 4                                                        |
// +----------------------------------------------------------------------+
// | Copyright (c) 1997, 1998, 1999, 2000, 2001 The PHP Group             |
// +----------------------------------------------------------------------+
// | This source file is subject to version 2.0 of the PHP license,       |
// | that is bundled with this package in the file LICENSE, and is        |
// | available at through the world-wide-web at                           |
// | http://www.php.net/license/2_02.txt.                                 |
// | If you did not receive a copy of the PHP license and are unable to   |
// | obtain it through the world-wide-web, please send a note to          |
// | license@php.net so we can mail you a copy immediately.               |
// +----------------------------------------------------------------------+
// | Author: Ulf Wendel <ulf.wendel@phpdoc.de>                            |
// +----------------------------------------------------------------------+
//
// $Id: IT_Error.php,v 1.1 2002/04/28 07:16:05 sebastian Exp $

require_once "pear/PEAR.php";

/**
* IT[X] Error class
* 
* @package IT[X]
*/
class IT_Error extends PEAR_Error {


  /**
  * Prefix of all error messages.
  * 
  * @var  string
  */
  var $error_message_prefix = "IntegratedTemplate Error: ";
  
  /**
  * Creates an cache error object.
  * 
  * @param  string  error message
  * @param  string  file where the error occured
  * @param  string  linenumber where the error occured
  */
  function IT_Error($msg, $file = __FILE__, $line = __LINE__) {
    
    $this->PEAR_Error(sprintf("%s [%s on line %d].", $msg, $file, $line));
    
  } // end func IT_Error
  
} // end class IT_Error
?>
