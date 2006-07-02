<?php
/* vim: set expandtab tabstop=4 shiftwidth=4: */
// +----------------------------------------------------------------------+
// | PHP Version 4                                                        |
// +----------------------------------------------------------------------+
// | Copyright (c) 1997-2003 The PHP Group                                |
// +----------------------------------------------------------------------+
// | This source file is subject to version 2.02 of the PHP license,      |
// | that is bundled with this package in the file LICENSE, and is        |
// | available at through the world-wide-web at                           |
// | http://www.php.net/license/2_02.txt.                                 |
// | If you did not receive a copy of the PHP license and are unable to   |
// | obtain it through the world-wide-web, please send a note to          |
// | license@php.net so we can mail you a copy immediately.               |
// +----------------------------------------------------------------------+
// | Author: Stijn de Reede <sjr@gmx.co.uk>                               |
// +----------------------------------------------------------------------+
//
// $Id$
//


/**
* @package  HTML_BBCodeParser
* @author   Stijn de Reede  <sjr@gmx.co.uk>
*/


require_once('HTML/BBCodeParser.php');




class HTML_BBCodeParser_Filter_Lists extends HTML_BBCodeParser
{

    /**
    * An array of tags parsed by the engine
    *
    * @access   private
    * @var      array
    */
    var $_definedTags = array(  'list'  => array(   'htmlopen'  => 'ol',
                                                    'htmlclose' => 'ol',
                                                    'allowed'   => 'none^li',
                                                    'attributes'=> array(   'list'  => 'type=%2$s%1$s%2$s',
                                                                            's'     => 'start=%2$s%1$d%2$s')
                                                    ),
                                'ulist' => array(   'htmlopen'  => 'ul',
                                                    'htmlclose' => 'ul',
                                                    'allowed'   => 'none^li',
                                                    'attributes'=> array()
                                                    ),
                                'li'    => array(   'htmlopen'  => 'li',
                                                    'htmlclose' => 'li',
                                                    'allowed'   => 'all',
                                                    'attributes'=> array(   'li'    => 'value=%2$s%1$d%2$s')
                                                    )
                                );


    /**
    * Executes statements before the actual array building starts
    *
    * This method should be overwritten in a filter if you want to do
    * something before the parsing process starts. This can be useful to
    * allow certain short alternative tags which then can be converted into
    * proper tags with preg_replace() calls.
    * The main class walks through all the filters and and calls this
    * method if it exists. The filters should modify their private $_text
    * variable.
    *
    * @return   none
    * @access   private
    * @see      $_text
    * @author   Stijn de Reede  <sjr@gmx.co.uk>
    */
    function _preparse()
    {
        $options = PEAR::getStaticProperty('HTML_BBCodeParser','_options');
        $o = $options['open'];
        $c = $options['close'];
        $oe = $options['open_esc'];
        $ce = $options['close_esc'];
        $pattern = array(   "!".$oe."\*".$ce."(.*)!i",
                            "!".$oe."list".$ce."(.+)".$oe."/list".$ce."!isU");
        $replace = array(   $o."li".$c."\\1".$o."/li".$c,
                            $o."ulist".$c."\\1".$o."/ulist".$c);
        $this->_preparsed = preg_replace($pattern, $replace, $this->_text);
    }


}


?>
