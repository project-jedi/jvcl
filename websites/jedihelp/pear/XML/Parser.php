<?php
//
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
// | Author: Stig Bakken <ssb@fast.no>                                    |
// +----------------------------------------------------------------------+
//
// $Id: Parser.php,v 1.5 2003/02/23 10:48:31 ssb Exp $

require_once 'PEAR.php';

/**
 * XML Parser class.  This is an XML parser based on PHP's "xml" extension,
 * based on the bundled expat library.
 *
 * @author  Stig Bakken <ssb@fast.no>
 * @todo    Tests that need to be made:
 *          - error class
 *          - mixing character encodings
 *          - a test using all expat handlers
 *          - options (folding, output charset)
 *          - different parsing modes
 *
 * @notes   - It requires PHP 4.0.4pl1 or greater
 *          - From revision 1.17, the function names used by the 'func' mode
 *            are in the format "xmltag_$elem", for example: use "xmltag_name"
 *            to handle the <name></name> tags of your xml file.
 */
class XML_Parser extends PEAR
{
    // {{{ properties

    /**
     * @var  resource  XML parser handle
     */
    var $parser;

    /**
     * @var  resource  File handle if parsing from a file
     */
    var $fp;

    /**
     * @var  boolean  Whether to do case folding
     */
    var $folding = true;

    /**
     * @var  string  Mode of operation, one of "event" or "func"
     */
    var $mode;

    /**
     * Mapping from expat handler function to class method.
     *
     * @var  array
     */
    var $handler = array(
        'character_data_handler'            => 'cdataHandler',
        'default_handler'                   => 'defaultHandler',
        'processing_instruction_handler'    => 'piHandler',
        'unparsed_entity_decl_handler'      => 'unparsedHandler',
        'notation_decl_handler'             => 'notationHandler',
        'external_entity_ref_handler'       => 'entityrefHandler'
    );

    /**
     * @var string source encoding
     */
    var $srcenc;

    /**
     * @var string target encoding
     */
    var $tgtenc;

    /*
     * Use call_user_func when php >= 4.0.7
     * @var boolean
     * @see setMode()
     */
    var $use_call_user_func = true;

    // }}}
    // {{{ constructor

    /**
     * Creates an XML parser.
     *
     * @param    string  source charset encoding, use NULL (default) to use
     *                   whatever the document specifies
     * @param    string  how this parser object should work, "event" for
     *                   startelement/endelement-type events, "func"
     *                   to have it call functions named after elements
     *
     * @see xml_parser_create
     */
    function XML_Parser($srcenc = null, $mode = "event", $tgtenc = null)
    {
        $this->PEAR('XML_Parser_Error');

        if ($srcenc === null) {
            $xp = @xml_parser_create();
        } else {
            $xp = @xml_parser_create($srcenc);
        }
        if (is_resource($xp)) {
            if ($tgtenc !== null) {
                if (!@xml_parser_set_option($xp, XML_OPTION_TARGET_ENCODING,
                                            $tgtenc)) {
                    return $this->raiseError("invalid target encoding");
                }
            }
            $this->parser = $xp;
            $this->setMode($mode);
            xml_parser_set_option($xp, XML_OPTION_CASE_FOLDING, $this->folding);
        }
        $this->srcenc = $srcenc;
        $this->tgtenc = $tgtenc;
    }
    // }}}

    // {{{ setMode()

    /**
     * Sets the mode and all handler.
     *
     * @param    string
     * @see      $handler
     */
    function setMode($mode)
    {

        $this->mode = $mode;

        xml_set_object($this->parser, $this);

        switch ($mode) {

            case "func":
                // use call_user_func() when php >= 4.0.7
                // or call_user_method() if not
                if (version_compare(phpversion(), '4.0.7', 'lt')) {
                    $this->use_call_user_func = false;
                } else {
                    $this->use_call_user_func = true;
                }

                xml_set_element_handler($this->parser, "funcStartHandler", "funcEndHandler");
                break;

            case "event":
                xml_set_element_handler($this->parser, "startHandler", "endHandler");
                break;
        }

        foreach ($this->handler as $xml_func => $method)
            if (method_exists($this, $method)) {
                $xml_func = "xml_set_" . $xml_func;
                $xml_func($this->parser, $method);
            }

    }

    // }}}
    // {{{ setInputFile()

    /**
     * Defines
     *
     * @param    string      Filename (full path)
     * @return   resource    fopen handle of the given file
     * @throws   XML_Parser_Error
     * @see      setInput(), parse()
     * @access   public
     */
    function setInputFile($file)
    {

        $fp = @fopen($file, "rb");
        if (is_resource($fp)) {
            $this->fp = $fp;
            return $fp;
        }

        return $this->raiseError($php_errormsg);
    }

    // }}}
    // {{{ setInput()

    /**
     * Sets the file handle to use with parse().
     *
     * @param    resource    fopen
     * @access   public
     * @see      parse(), setInputFile()
     */
    function setInput($fp)
    {
        if (is_resource($fp)) {
            $this->fp = $fp;
            return true;
        }

        return $this->raiseError("not a file resource");
    }

    // }}}
    // {{{ parse()

    /**
     * Central parsing function.
     *
     * @throws   XML_Parser_Error
     * @return   boolean true on success
     * @see      parseString()
     * @access   public
     */
    function parse()
    {
        if (!is_resource($this->fp)) {
            return $this->raiseError("no input");
        }

        while ($data = fread($this->fp, 2048)) {

            $err = $this->parseString($data, feof($this->fp));
            if (PEAR::isError($err)) {
                fclose($this->fp);
                return $err;
            }

        }

        fclose($this->fp);

        return true;
    }

    // }}}
    // {{{ parseString()

    /**
     * Parses a string.
     *
     * @param    string  XML data
     * @param    boolean ???
     * @throws   XML_Parser_Error
     * @return   mixed   true on success or a string with the xml parser error
     */
    function parseString($data, $eof = false)
    {
        if (!xml_parse($this->parser, $data, $eof)) {
            $err = $this->raiseError($this->parser);
            xml_parser_free($this->parser);
            return $err;
        }

        return true;
    }

    // }}}
    // {{{ funcStartHandler()

    function funcStartHandler($xp, $elem, $attribs)
    {
        $func = 'xmltag_' . $elem;
        if (method_exists($this, $func)) {
            if ($this->use_call_user_func) {
                call_user_func(array(&$this, $func), $xp, $elem, $attribs);
            } else {
                call_user_method($func, $this, $xp, $elem, $attribs);
            }
        }

    }

    // }}}
    // {{{ funcEndHandler()

    function funcEndHandler($xp, $elem)
    {
        $func = 'xmltag_' . $elem . '_';
        if (method_exists($this, $func)) {
            if ($this->use_call_user_func) {
                call_user_func(array(&$this, $func), $xp, $elem);
            } else {
                call_user_method($func, $this, $xp, $elem);
            }
        }
    }

    // }}}
    // {{{ startHandler()

    /**
     *
     * @abstract
     */
    function startHandler($xp, $elem, &$attribs)
    {
        return NULL;
    }

    // }}}
    // {{{ endHandler()

    /**
     *
     * @abstract
     */
    function endHandler($xp, $elem)
    {
        return NULL;
    }


    // }}}
}

class XML_Parser_Error extends PEAR_Error
{
    // {{{ properties

    var $error_message_prefix = 'XML_Parser: ';

    // }}}
    // {{{ constructor()

    function XML_Parser_Error($msgorparser = 'unknown error', $code = 0, $mode = PEAR_ERROR_RETURN, $level = E_USER_NOTICE)
    {
        if (is_resource($msgorparser)) {
            $code = xml_get_error_code($msgorparser);
            $msgorparser = sprintf("%s at XML input line %d",
                                   xml_error_string($code),
                                   xml_get_current_line_number($msgorparser));
        }
        $this->PEAR_Error($msgorparser, $code, $mode, $level);

    }

    // }}}
}
?>