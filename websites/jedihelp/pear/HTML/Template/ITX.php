<?php
//
// +----------------------------------------------------------------------+
// | PHP version 4.0                                                      |
// +----------------------------------------------------------------------+
// | Copyright (c) 1997-2001 The PHP Group                                |
// +----------------------------------------------------------------------+
// | This source file is subject to version 2.02 of the PHP license,      |
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
// $Id: ITX.php,v 1.8 2003/03/12 02:25:16 pajoye Exp $
//

require_once('pear/HTML/Template/IT.php');
require_once 'pear/HTML/Template/IT_Error.php';

/**
* Integrated Template Extension - ITX
*
* With this class you get the full power of the phplib template class.
* You may have one file with blocks in it but you have as well one main file
* and multiple files one for each block. This is quite usefull when you have
* user configurable websites. Using blocks not in the main template allows
* you to modify some parts of your layout easily.
*
* Note that you can replace an existing block and add new blocks at runtime.
* Adding new blocks means changing a variable placeholder to a block.
*
* @author   Ulf Wendel <uw@netuse.de>
* @access   public
* @version  $Id: ITX.php,v 1.8 2003/03/12 02:25:16 pajoye Exp $
* @package  IT[X]
*/
class HTML_Template_ITX extends HTML_Template_IT {

    /**
    * Array with all warnings.
    * @var       array
    * @access    public
    * @see       $printWarning, $haltOnWarning, warning()
    */
    var $warn = array();

    /**
    * Print warnings?
    * @var       array
    * @access    public
    * @see      $haltOnWarning, $warn, warning()
    */
    var $printWarning = false;

    /**
    * Call die() on warning?
    * @var         boolean
    * @access    public
    * @see       $warn, $printWarning, warning()
    */
    var $haltOnWarning = false;

    /**
    * RegExp used to test for a valid blockname.
    * @var    string
    */
    var $checkblocknameRegExp = '';

    /**
    * Functionnameprefix used when searching function calls in the template.
    * @var    string
    */
    var $functionPrefix = 'func_';

    /**
    * Functionname RegExp.
    * @var    string
    */
    var $functionnameRegExp = '[_a-zA-Z]+[A-Za-z_0-9]*';

    /**
    * RegExp used to grep function calls in the template.
    *
    * The variable gets set by the constructor.
    *
    * @var    string
    * @see    HTML_Template_IT()
    */
    var $functionRegExp = '';

    /**
    * List of functions found in the template.
    *
    * @var    array
    */
    var $functions         = array();

    /**
    * List of callback functions specified by the user.
    *
    * @var    array
    */
    var $callback         = array();

    /**
    * Builds some complex regexps and calls the constructor
    * of the parent class.
    *
    * Make sure that you call this constructor if you derive your own
    * template class from this one.
    *
    * @see    HTML_Template_IT()
    */
    function HTML_Template_ITX($root = '')
    {

        $this->checkblocknameRegExp = '@' . $this->blocknameRegExp . '@';
        $this->functionRegExp = '@' . $this->functionPrefix . '(' .
                                $this->functionnameRegExp . ')\s*\(@sm';

        $this->HTML_Template_IT($root);
    } // end func constructor

    function init()
    {
        $this->free();
        $this->buildFunctionlist();
        $this->findBlocks($this->template);
        // we don't need it any more
        $this->template = '';
        $this->buildBlockvariablelist();

    } // end func init

    /**
    * Replaces an existing block with new content.
    *
    * This function will replace a block of the template and all blocks
    * contained in the replaced block and add a new block insted, means
    * you can dynamically change your template.
    *
    * Note that changing the template structure violates one of the IT[X]
    * development goals. I've tried to write a simple to use template engine
    * supporting blocks. In contrast to other systems IT[X] analyses the way
    * you've nested blocks and knows which block belongs into another block.
    * The nesting information helps to make the API short and simple. Replacing
    * blocks does not only mean that IT[X] has to update the nesting
    * information (relatively time consumpting task) but you have to make sure
    * that you do not get confused due to the template change itself.
    *
    * @param    string      Blockname
    * @param    string      Blockcontent
    * @param    boolean     true if the new block inherits the content
    *                       of the old block
    * @return   boolean
    * @throws   IT_Error
    * @see      replaceBlockfile(), addBlock(), addBlockfile()
    * @access   public
    */
    function replaceBlock($block, $template, $keep_content = false)
    {
        if (!isset($this->blocklist[$block])) {
            return new IT_Error(
            "The block "."'$block'".
            " does not exist in the template and thus it can't be replaced.",
            __FILE__, __LINE__
            );
        }
        if ('' == $template) {
            return new IT_Error('No block content given.', __FILE__, __LINE__);
        }
        if ($keep_content) {
            $blockdata = $this->blockdata[$block];
        }

        // remove all kinds of links to the block / data of the block
        $this->removeBlockData($block);

        $template = "<!-- BEGIN $block -->" . $template . "<!-- END $block -->";
        $parents = $this->blockparents[$block];
        $this->findBlocks($template);
        $this->blockparents[$block] = $parents;

        // KLUDGE: rebuild the list for all block - could be done faster
        $this->buildBlockvariablelist();

        if ($keep_content) {
            $this->blockdata[$block] = $blockdata;
        }

        // old TODO - I'm not sure if we need this
        // update caches

        return true;
    } // end func replaceBlock

    /**
    * Replaces an existing block with new content from a file.
    *
    * @brother replaceBlock()
    * @param    string    Blockname
    * @param    string    Name of the file that contains the blockcontent
    * @param    boolean   true if the new block inherits the content of the old block
    */
    function replaceBlockfile($block, $filename, $keep_content = false)
    {
        return $this->replaceBlock($block, $this->getFile($filename), $keep_content);
    } // end func replaceBlockfile

    /**
    * Adds a block to the template changing a variable placeholder
    * to a block placeholder.
    *
    * Add means "replace a variable placeholder by a new block".
    * This is different to PHPLibs templates. The function loads a
    * block, creates a handle for it and assigns it to a certain
    * variable placeholder. To to the same with PHPLibs templates you would
    * call set_file() to create the handle and parse() to assign the
    * parsed block to a variable. By this PHPLibs templates assume
    * that you tend to assign a block to more than one one placeholder.
    * To assign a parsed block to more than only the placeholder you specify
    * in this function you have to use a combination of getBlock()
    * and setVariable().
    *
    * As no updates to cached data is necessary addBlock() and addBlockfile()
    * are rather "cheap" meaning quick operations.
    *
    * The block content must not start with <!-- BEGIN blockname -->
    * and end with <!-- END blockname --> this would cause overhead and
    * produce an error.
    *
    * @param    string    Name of the variable placeholder, the name must be unique
    *                     within the template.
    * @param    string    Name of the block to be added
    * @param    string    Content of the block
    * @return   boolean
    * @throws   IT_Error
    * @see      addBlockfile()
    * @access   public
    */
    function addBlock($placeholder, $blockname, $template)
    {

        // Don't trust any user even if it's a programmer or yourself...
        if ('' == $placeholder) {

            return new IT_Error('No variable placeholder given.',
                                __FILE__, __LINE__
                                );

        } else if ( '' == $blockname ||
                    !preg_match($this->checkblocknameRegExp, $blockname)
        ) {

            return new IT_Error("No or invalid blockname '$blockname' given.",
                    __FILE__, __LINE__
                    );

        } else if ('' == $template) {

            return new IT_Error('No block content given.', __FILE__, __LINE__);

        } else if (isset($this->blocklist[$blockname])) {

            return new IT_Error('The block already exists.',
                                __FILE__, __LINE__
                            );

        }

        // find out where to insert the new block
        $parents = $this->findPlaceholderBlocks($placeholder);
        if (0 == count($parents)) {

            return new IT_Error(
                "The variable placeholder".
                " '$placeholder' was not found in the template.",
                __FILE__, __LINE__
            );

        } else if ( count($parents) > 1 ) {

            reset($parents);
            while (list($k, $parent) = each($parents)) {
                $msg .= "$parent, ";
            }
            $msg = substr($parent, -2);

            return new IT_Error("The variable placeholder "."'$placeholder'".
                                " must be unique, found in multiple blocks '$msg'.",
                                __FILE__, __LINE__
                                );
        }

        $template = "<!-- BEGIN $blockname -->" . $template . "<!-- END $blockname -->";
        $this->findBlocks($template);
        if ($this->flagBlocktrouble) {
            return false;    // findBlocks() already throws an exception
        }
        $this->blockinner[$parents[0]][] = $blockname;
        $this->blocklist[$parents[0]] = preg_replace(
                    '@' . $this->openingDelimiter . $placeholder .
                    $this->closingDelimiter . '@',

                    $this->openingDelimiter . '__' . $blockname . '__' .
                    $this->closingDelimiter,

                    $this->blocklist[$parents[0]]
                );

        $this->deleteFromBlockvariablelist($parents[0], $placeholder);
        $this->updateBlockvariablelist($blockname);
    /*
    // check if any inner blocks were found
    if(is_array($this->blockinner[$blockname]) and count($this->blockinner[$blockname]) > 0) {
        // loop through inner blocks, registering the variable placeholders in each
        foreach($this->blockinner[$blockname] as $childBlock) {
            $this->updateBlockvariablelist($childBlock);
        }
    }
    */
        return true;
    } // end func addBlock

    /**
    * Adds a block taken from a file to the template changing a variable
    * placeholder to a block placeholder.
    *
    * @param      string    Name of the variable placeholder to be converted
    * @param      string    Name of the block to be added
    * @param      string    File that contains the block
    * @brother    addBlock()
    */
    function addBlockfile($placeholder, $blockname, $filename)
    {
        return $this->addBlock($placeholder, $blockname, $this->getFile($filename));
    } // end func addBlockfile

    /**
    * Returns the name of the (first) block that contains
    * the specified placeholder.
    *
    * @param    string  Name of the placeholder you're searching
    * @param    string  Name of the block to scan. If left out (default)
    *                   all blocks are scanned.
    * @return   string  Name of the (first) block that contains
    *                   the specified placeholder.
    *                   If the placeholder was not found or an error occured
    *                   an empty string is returned.
    * @throws   IT_Error
    * @access   public
    */
    function placeholderExists($placeholder, $block = '')
    {
        if ('' == $placeholder) {
            new IT_Error('No placeholder name given.', __FILE__, __LINE__);
            return '';
        }

        if ('' != $block && !isset($this->blocklist[$block])) {
            new IT_Error("Unknown block '$block'.", __FILE__, __LINE__);
            return '';
        }

        // name of the block where the given placeholder was found
        $found = '';

        if ('' != $block) {

            if (is_array($variables = $this->blockvariables[$block])) {

                // search the value in the list of blockvariables
                reset($variables);
                while (list($k, $variable) = each($variables)) {
                    if ($k == $placeholder) {
                        $found = $block;
                        break;
                    }
                }
            }

        } else {

            // search all blocks and return the name of the first block that
            // contains the placeholder
            reset($this->blockvariables);
            while (list($blockname, $variables) = each($this->blockvariables)){

                if (is_array($variables) && isset($variables[$placeholder])) {
                    $found = $blockname;
                    break;
                }
            }

        }

        return $found;
    } // end func placeholderExists

    /**
    * Checks the list of function calls in the template and
    * calls their callback function.
    *
    * @access    public
    */
    function performCallback()
    {

        reset($this->functions);
        while (list($func_id, $function) = each($this->functions)) {

            if (isset($this->callback[$function['name']])) {

                if ('' != $this->callback[$function['name']]['object']) {
                    $this->variableCache['__function' . $func_id . '__'] =
                        call_user_func(
                        array(
                        &$GLOBALS[$this->callback[$function['name']]['object']],
                        $this->callback[$function['name']]['function']),
                        $function['args']
                       );
                } else {
                    $this->variableCache['__function' . $func_id . '__'] =
                            call_user_func(
                            $this->callback[$function['name']]['function'],
                            $function['args']
                        );
                }

            }

        }

    } // end func performCallback

    /**
    * Returns a list of all function calls in the current template.
    *
    * @return   array
    * @access   public
    */
    function getFunctioncalls()
    {
        return $this->functions;
    } // end func getFunctioncalls

    /**
    * Replaces a function call with the given replacement.
    *
    * @param    int       Function ID
    * @param    string    Replacement
    * @deprec
    */
    function setFunctioncontent($functionID, $replacement)
    {
        $this->variableCache['__function' . $functionID . '__'] = $replacement;
    } // end func setFunctioncontent

    /**
    * Sets a callback function.
    *
    * IT[X] templates (note the X) can contain simple function calls.
    * "function call" means that the editor of the template can add
    * special placeholder to the template like 'func_h1("embedded in h1")'.
    * IT[X] will grab this function calls and allow you to define a callback
    * function for them.
    *
    * This is an absolutely evil feature. If your application makes heavy
    * use of such callbacks and you're even implementing if-then etc. on
    * the level of a template engine you're reiventing the wheel... - that's
    * actually how PHP came into life. Anyway, sometimes it's handy.
    *
    * Consider also using XML/XSLT or native PHP. And please do not push
    * IT[X] any further into this direction of adding logics to the template
    * engine.
    *
    * For those of you ready for the X in IT[X]:
    *
    * <?php
    * ...
    * function h_one($args) {
    *    return sprintf('<h1>%s</h1>', $args[0]);
    * }
    *
    * ...
    * $itx = new HTML_Template_ITX( ... );
    * ...
    * $itx->setCallbackFunction('h1', 'h_one');
    * $itx->performCallback();
    * ?>
    *
    * template:
    * func_h1('H1 Headline');
    *
    * @param    string    Function name in the template
    * @param    string    Name of the callback function
    * @param    string    Name of the callback object
    * @return   boolean   False on failure.
    * @throws   IT_Error
    * @access   public
    */
    function
    setCallbackFunction($tplfunction, $callbackfunction, $callbackobject = '')
    {

        if ('' == $tplfunction || '' == $callbackfunction) {
            return new IT_Error(
                "No template function "."('$tplfunction')".
                " and/or no callback function ('$callback') given.",
                    __FILE__, __LINE__
                );
        }
        $this->callback[$tplfunction] = array(
                                          "function"    => $callbackfunction,
                                          "object"        => $callbackobject
                                        );

        return true;
    } // end func setCallbackFunction

    /**
    * Sets the Callback function lookup table
    *
    * @param    array    function table
    *                    array[templatefunction] =
    *                       array(
    *                               "function" => userfunction,
    *                               "object" => userobject
    *                       )
    * @access    public
    */
    function setCallbackFuntiontable($functions)
    {
        $this->callback = $functions;
    } // end func setCallbackFunctiontable

    /**
    * Recursively removes all data assiciated with a block, including all inner blocks
    *
    * @param    string  block to be removed
    */
    function removeBlockData($block)
    {
        if (isset($this->blockinner[$block])) {
            foreach ($this->blockinner[$block] as $k => $inner) {
                $this->removeBlockData($inner);
            }

            unset($this->blockinner[$block]);
        }

        unset($this->blocklist[$block]);
        unset($this->blockdata[$block]);
        unset($this->blockvariables[$block]);
        unset($this->touchedBlocks[$block]);

    } // end func removeBlockinner

    /**
    * Returns a list of blocknames in the template.
    *
    * @return    array    [blockname => blockname]
    * @access    public
    * @see        blockExists()
    */
    function getBlocklist()
    {
        $blocklist = array();
        foreach ($this->blocklist as $block => $content) {
            $blocklist[$block] = $block;
        }

        return $blocklist;
    } // end func getBlocklist

    /**
    * Checks wheter a block exists.
    *
    * @param    string
    * @return    boolean
    * @access    public
    * @see        getBlocklist()
    */
    function blockExists($blockname)
    {
        return isset($this->blocklist[$blockname]);
    } // end func blockExists

    /**
    * Returns a list of variables of a block.
    *
    * @param    string    Blockname
    * @return    array    [varname => varname]
    * @access    public
    * @see        BlockvariableExists()
    */
    function getBlockvariables($block)
    {
        if (!isset($this->blockvariables[$block])) {
            return array();
        }

        $variables = array();
        foreach ($this->blockvariables[$block] as $variable => $v) {
            $variables[$variable] = $variable;
        }

        return $variables;
    } // end func getBlockvariables

    /**
    * Checks wheter a block variable exists.
    *
    * @param    string    Blockname
    * @param    string    Variablename
    * @return    boolean
    * @access    public
    * @see    getBlockvariables()
    */
    function BlockvariableExists($block, $variable)
    {
        return isset($this->blockvariables[$block][$variable]);
    } // end func BlockvariableExists

    /**
    * Builds a functionlist from the template.
    */
    function buildFunctionlist()
    {
        $this->functions = array();

        $template = $this->template;
        $num = 0;

        while (preg_match($this->functionRegExp, $template, $regs)) {

            $pos = strpos($template, $regs[0]);
            $template = substr($template, $pos + strlen($regs[0]));

            $head = $this->getValue($template, ')');
            $args = array();

            $this->template = str_replace($regs[0] . $head . ')',
                                '{__function' . $num . '__}', $this->template
                            );
            $template = str_replace($regs[0] . $head . ')',
                        '{__function' . $num . '__}', $template
                        );

            while ('' != $head && $args2 = $this->getValue($head, ',')) {
                $arg2 = trim($args2);
                $args[] = ('"' == $arg2{0} || "'" == $arg2{0}) ?
                                    substr($arg2, 1, -1) : $arg2;
                if ($arg2 == $head) {
                    break;
                }
                $head = substr($head, strlen($arg2) + 1);
            }

            $this->functions[$num++] = array(
                                                'name'    => $regs[1],
                                                'args'    => $args
                                            );
        }

    } // end func buildFunctionlist


    function getValue($code, $delimiter) {
        if ('' == $code) {
            return '';
        }

        if (!is_array($delimiter)) {
            $delimiter = array( $delimiter => true );
        }

        $len         = strlen($code);
        $enclosed    = false;
        $enclosed_by = '';

        if (isset($delimiter[$code[0]])) {

            $i = 1;

        } else {

            for ($i = 0; $i < $len; ++$i) {

                $char = $code[$i];

                if (
                        ('"' == $char || "'" == $char) &&
                        ($char == $enclosed_by || '' == $enclosed_by) &&
                        (0 == $i || ($i > 0 && '\\' != $code[$i - 1]))
                    ){

                    if (!$enclosed) {
                        $enclosed_by = $char;
                    } else {
                        $enclosed_by = "";
                    }
                    $enclosed = !$enclosed;

                }
                if (!$enclosed && isset($delimiter[$char])) {
                    break;
                }
            }

        }

        return substr($code, 0, $i);
    } // end func getValue


    /**
    * Deletes one or many variables from the block variable list.
    *
    * @param    string    Blockname
    * @param    mixed     Name of one variable or array of variables
    *                     ( array ( name => true ) ) to be stripped.
    */
    function deleteFromBlockvariablelist($block, $variables)
    {
        if (!is_array($variables)) {
            $variables = array($variables => true);
        }

        reset($this->blockvariables[$block]);
        while (list($varname, $val) = each($this->blockvariables[$block])) {
            if (isset($variables[$varname])) {
                unset($this->blockvariables[$block][$varname]);
            }
        }
    } // end deleteFromBlockvariablelist

    /**
    * Updates the variable list of a block.
    *
    * @param    string    Blockname
    */
    function updateBlockvariablelist($block)
    {
        preg_match_all( $this->variablesRegExp,
                        $this->blocklist[$block], $regs
                    );

        if (0 != count($regs[1])) {
            foreach ($regs[1] as $k => $var) {
                $this->blockvariables[$block][$var] = true;
            }
        } else {
            $this->blockvariables[$block] = array();
        }

        // check if any inner blocks were found
        if (isset($this->blockinner[$block]) &&
            is_array($this->blockinner[$block]) &&
            count($this->blockinner[$block]) > 0
        ) {
            /*
             * loop through inner blocks, registering the variable
             * placeholders in each
             */
            foreach($this->blockinner[$block] as $childBlock) {
                $this->updateBlockvariablelist($childBlock);
            }
        }

    } // end func updateBlockvariablelist

    /**
    * Returns an array of blocknames where the given variable
    * placeholder is used.
    *
    * @param    string    Variable placeholder
    * @return    array    $parents    parents[0..n] = blockname
    */
    function findPlaceholderBlocks($variable)
    {
        $parents = array();
        reset($this->blocklist);
        while (list($blockname, $content) = each($this->blocklist)) {
            reset($this->blockvariables[$blockname]);
            while (
                list($varname, $val) = each($this->blockvariables[$blockname]))
            {
                if ($variable == $varname) {
                    $parents[] = $blockname;
                }
            }
        }

        return $parents;
    } // end func findPlaceholderBlocks

    /**
    * Handles warnings, saves them to $warn and prints them or
    * calls die() depending on the flags
    *
    * @param    string    Warning
    * @param    string    File where the warning occured
    * @param    int       Linenumber where the warning occured
    * @see      $warn, $printWarning, $haltOnWarning
    */
    function warning($message, $file = '', $line = 0)
    {
        $message = sprintf(
                    'HTML_Template_ITX Warning: %s [File: %s, Line: %d]',
                    $message,
                    $file,
                    $line
                );

        $this->warn[] = $message;

        if ($this->printWarning) {
            print $message;
        }

        if ($this->haltOnError) {
            die($message);
        }
    } // end func warning

} // end class HTML_Template_ITX
?>