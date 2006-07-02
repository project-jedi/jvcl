<?php
/* vim: set expandtab tabstop=4 shiftwidth=4: */
// +----------------------------------------------------------------------+
// | PHP version 4.0                                                      |
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
// | Authors: Frederic Poeydomenge <frederic.poeydomenge@free.fr>         |
// +----------------------------------------------------------------------+
//
// $Id$

require_once 'PEAR.php';

// +----------------------------------------------------------------------+
// | SYNOPSIS                                                             |
// +----------------------------------------------------------------------+
// |   Var_Dump::display($vars);                                          |
// | or                                                                   |
// |   Var_Dump::display($vars,'',VAR_DUMP_DISPLAY_MODE_HTML_TABLE);      |
// | or                                                                   |
// |   $text = Var_Dump::r_display($vars);                                |
// | or                                                                   |
// |   $text = Var_Dump::r_display($vars,'',VAR_DUMP_DISPLAY_MODE_TEXT);  |
// | or                                                                   |
// |   $foo=new Var_Dump($confVD);                                        |
// |   $foo->setDisplayMode($confDM);                                     |
// |   $foo->display($vars);                                              |
// +----------------------------------------------------------------------+

if (!defined('VAR_DUMP_CONSTANTS')) {

    define('VAR_DUMP_MAGIC', 'fce75110d95bedd51264ce2a99cfc908');
    
    define('VAR_DUMP_ARRAY_TYPE',              1);
    define('VAR_DUMP_ARRAY_VALUE',             2);
    define('VAR_DUMP_ARRAY_MAGIC',             0);

    define('VAR_DUMP_DISPLAY_MODE_TEXT',       1);
    define('VAR_DUMP_DISPLAY_MODE_HTML_TEXT',  2);
    define('VAR_DUMP_DISPLAY_MODE_HTML_TABLE', 3);

    define('VAR_DUMP_CONSTANTS',            true);

    // 2003-08-20 Walt Boring <waboring at 3gstech dot com> : feature which
    // allows to turn off a few output options for dumping an object
    define('VAR_DUMP_NO_CLASS_VARS', 1);
    define('VAR_DUMP_NO_CLASS_METHODS', 2);
    //turns both class vars and class methods off
    define('VAR_DUMP_NO_CLASS_INFO', 3);


}

/**
* Displays informations about the values of variables on a graphical way
* - If given a simple variable (string, integer, double, ressource), the
* value itself is printed.
* - If given an array, it is explored recursively and values are presented
* in a format that shows keys and elements.
* - If given an object, informations about the object and the class
* are printed.
*/
class Var_Dump extends PEAR
{
        
    /**
    * Selected language for error messages
    * @var string $lang
    */
    var $lang = 'en';

    /**
    * Messages in various languages
    * @var string $messages
    */
    var $messages;

    /**
    * @var array $_VarDumpArray An array representing the structure of submitted variables
    */
    var $_VarDumpArray = array();

    /**
    * @var array $_VarDumpConfGlobal Global parameters for an instance of the class
    *                'showNotParsed' => Tell if we show 'not parsed' variables or not
    *                'displayMode'   => Default display mode : Text or Html
    *                'displaySkin'   => Default skin name
    */
    var $_VarDumpConfGlobal = array();

    /**
    * @var array $_VarDumpConfVisual Visual parameters for the rendering
    *                'bordercolor'   => border color
    *                'color1'        => cells color for odd rows
    *                'color2'        => cells color for even rows
    *                'bordersize'    => border size
    *                'cellpadding'   => padding of the table
    *                'cellspacing'   => spacing of the table
    *                'fontface'      => font face
    *                'fontsize'      => font size
    *                'fontcolor1'    => font color for odd rows
    *                'fontcolor2'    => font color for even rows
    */
    var $_VarDumpConfVisual = array();

    /**
    * @var string $_VarDumpDisplayFont1Start Text : Starting tag <font...> or empty
    * @var string $_VarDumpDisplayFont1End   Text : Ending tag </font> or empty
    * @var string $_VarDumpDisplayFont2Start Value : Starting tag <font...> or empty
    * @var string $_VarDumpDisplayFont2End   Value : Ending tag </font> or empty
    */
    var $_VarDumpDisplayFont1Start = '';
    var $_VarDumpDisplayFont1End   = '';
    var $_VarDumpDisplayFont2Start = '';
    var $_VarDumpDisplayFont2End   = '';


    // {{{ Var_Dump()
    /**
    * Var_Dump() - Constructor
    *
    * If an instance of the class is created, initialize the global parameters for this
    * instance : if we show 'not parsed' variables or not, the default display mode,
    * the default skin name..
    *
    * @param array $conf Global parameters for an instance of the class
    *                'showNotParsed' => Tell if we show 'not parsed' variables or not
    *                'displayMode'   => Default display mode : Text or Html
    *                'displaySkin'   => Default skin name
    */
    function Var_Dump($conf = array())
    {
        $conf2=array_merge(
            array(
                'language'      => 'en',
                'showNotParsed' => true,
                'displayMode'   => VAR_DUMP_DISPLAY_MODE_HTML_TEXT,
                'displaySkin'   => ''
            ),
            $conf
        );
        $this->_VarDumpConfGlobal = $conf2;
        $this->lang = $conf2['language'];
        $this->setSkin(
            $conf2['displayMode'],
            $conf2['displaySkin']
        );

        $this->messages = array(
            'TYPE_ARRAY' => array(
                'en' => 'Array',
                'fr' => 'Tableau'
            ),
            'TYPE_LONG' => array(
                'en' => 'Long',
                'fr' => 'Long'
            ),
            'TYPE_INT' => array(
                'en' => 'Int',
                'fr' => 'Entier'
            ),
            'TYPE_INTEGER' => array(
                'en' => 'Integer',
                'fr' => 'Entier'
            ),
            'TYPE_DOUBLE' => array(
                'en' => 'Double',
                'fr' => 'Double'
            ),
            'TYPE_FLOAT' => array(
                'en' => 'Float',
                'fr' => 'Flottant'
            ),
            'TYPE_REAL' => array(
                'en' => 'Real',
                'fr' => 'Réel'
            ),
            'TYPE_STRING' => array(
                'en' => 'String',
                'fr' => 'Chaine'
            ),
            'TYPE_BOOLEAN' => array(
                'en' => 'Boolean',
                'fr' => 'Booléen'
            ),
            'TYPE_NUMERIC' => array(
                'en' => 'Numeric',
                'fr' => 'Numérique'
            ),
            'TYPE_RESOURCE' => array(
                'en' => 'Resource',
                'fr' => 'Ressource'
            ),
            'TYPE_SCALAR' => array(
                'en' => 'Scalar',
                'fr' => 'Scalaire'
            ),
            'TYPE_NULL' => array(
                'en' => 'Null',
                'fr' => 'Null'
            ),
            'TYPE_UNKNOWN' => array(
                'en' => 'Unknown type',
                'fr' => 'Type inconnu'
            ),
            'BOOLEAN_TRUE' => array(
                'en' => 'TRUE',
                'fr' => 'VRAI'
            ),
            'BOOLEAN_FALSE' => array(
                'en' => 'FALSE',
                'fr' => 'FAUX'
            ),
            'OBJECT_CLASS_NAME' => array(
                'en' => 'Class name',
                'fr' => 'Nom de la classe'
            ),
            'OBJECT_PARENT' => array(
                'en' => 'Parent class',
                'fr' => 'Classe parente'
            ),
            'OBJECT_CLASS_VARS' => array(
                'en' => 'Class vars',
                'fr' => 'Variables de la classe'
            ),
            'OBJECT_CLASS_METHODS' => array(
                'en' => 'Class methods',
                'fr' => 'Méthodes de la classe'
            ),
            'OBJECT_OBJECT_VARS' => array(
                'en' => 'Object vars',
                'fr' => 'Variables d\'instance'
            ),
            'EMPTY_ARRAY' => array(
                'en' => 'Empty array',
                'fr' => 'Tableau vide'
            ),
            'NOT_PARSED' => array(
                'en' => 'Not parsed',
                'fr' => 'Non analysé'
            ),
            'NON_EXISTENT' => array(
                'en' => 'Non-existent variable',
                'fr' => 'Variable inexistente'
            )
        );

    }
    // }}}


    // {{{ display()
    /**
    * display() - Displays informations about a variable
    *
    * This method displays informations about the values of a variable on a
    * graphical way, arrays and objects being explored recursively
    *
    * @param $variable      A variable to explore
    * @param string $except If not empty, name of the key not to
    *                       explore, to avoid parsing references to itself
    */
    function display($variable = '', $except = '', $displayMode = false, $options=0)
    {
        if (!isset($this) or get_class($this) != 'var_dump') {
            $this = & new Var_Dump;
            if ($displayMode !== false) {
                $this->_VarDumpConfGlobal['displayMode'] = $displayMode;
                $this->setSkin($displayMode);
            }
        }

        echo $this->r_display($variable, $except, $displayMode, $options);
    }
    // }}}


    // {{{ r_display()
    /**
    * r_display() - Returns informations about a variable, ready to be printed
    *
    * This method returns informations about the values of a variable, ready
    * to be printed on a graphical way, arrays and objects being explored
    * recursively
    *
    * @param $variable      A variable to explore
    * @param string $except If not empty, name of the key not to
    *                       explore, to avoid parsing references to itself
    */
    function r_display($variable = '', $except = '', $displayMode = false, $options=0)
    {
        if (!isset($this) or get_class($this) != 'var_dump') {
            $this = & new Var_Dump;
            if ($displayMode !== false) {
                $this->_VarDumpConfGlobal['displayMode'] = $displayMode;
                $this->setSkin($displayMode);
            }
        }

        $this->_VarDumpConfGlobal['displayOptions'] = $options;

        $this->_populate($variable, $except);

        $this->_generateFontTags();

        switch($this->_VarDumpConfGlobal['displayMode']) {

            case VAR_DUMP_DISPLAY_MODE_TEXT:
                return $this->_formatTableTEXT($this->_VarDumpArray[0],false);
                break;
            
            case VAR_DUMP_DISPLAY_MODE_HTML_TEXT:
                return $this->_formatTableTEXT($this->_VarDumpArray[0],true);
                break;
                
            case VAR_DUMP_DISPLAY_MODE_HTML_TABLE:
            default:
                return $this->_formatTableHTML($this->_VarDumpArray);
                break;
                
        }
    }
    // }}}
            

    // {{{ _populate()
    /**
    * _populate() - Fills the informations concerning a single variable
    *
    * This method fills the local array $this->_VarDumpArray with the
    * informations concerning a single variable
    * When parsing $GLOBALS variable, avoid parsing recursively the
    * reference to itself.
    *
    * @param $variable      The variable to explore
    * @param string $except If not empty, name of the key not to
    *                       explore, to avoid parsing references to itself
    */
    function _populate($variable, $except = '')
    {
        $lang=$this->lang;
        $this->_VarDumpArray=array();
        if (empty($except)) {
            $w_except = '';
        } else {
            $w_except = ' '.trim($except);
        }
        if (//$variable == $GLOBALS does not work
            is_array($variable) and
            isset($variable['_GET']) and
            isset($variable['_POST']) and
            isset($variable['_COOKIE']) and
            isset($variable['_SERVER']) and
            isset($variable['_ENV'])
        ) {
            $this->_VarDumpArray[] = $this->_parseArray($GLOBALS, 'GLOBALS'.$w_except);
        } else {
            if (isset($variable)) {
                if (is_array($variable)) {
                    $this->_VarDumpArray[] = $this->_parseArray($variable, $except);
                } else if (is_object($variable)) {
                    $this->_VarDumpArray[] = $this->_parseObject($variable);
                } else {
                    $this->_VarDumpArray[] = $this->_parseVariable($variable);
                }
            } else {
                $this->_VarDumpArray[] =
                    array(
                        VAR_DUMP_ARRAY_MAGIC => VAR_DUMP_MAGIC,
                        VAR_DUMP_ARRAY_TYPE  => $this->messages['TYPE_NULL'][$lang],
                        VAR_DUMP_ARRAY_VALUE => $this->messages['NON_EXISTENT'][$lang]
                    );
            }
        }
    }
    // }}}


    // {{{ _parseVariable()
    /**
    * _parseVariable() - Parse (recursively) a variable
    *
    * This method parse a variable, either returning informations about
    * this variable, or in the case of an object or array, returning
    * recursive informations about this variable.
    *
    * @param $variable A variable to explore
    */
    function _parseVariable($variable)
    {
        $lang=$this->lang;
        if (is_object($variable)) {
            return $this->_parseObject($variable);
        } elseif (is_array($variable)) {
            return $this->_parseArray($variable);
        } elseif (is_long($variable)) {
            $type = $this->messages['TYPE_LONG'][$lang];
        } elseif (is_int($variable)) {
            $type = $this->messages['TYPE_INT'][$lang];
        } elseif (is_integer($variable)) {
            $type = $this->messages['TYPE_INTEGER'][$lang];
        } elseif (is_double($variable)) {
            $type = $this->messages['TYPE_DOUBLE'][$lang];
        } elseif (is_float($variable)) {
            $type = $this->messages['TYPE_FLOAT'][$lang];
        } elseif (is_real($variable)) {
            $type = $this->messages['TYPE_REAL'][$lang];
        } elseif (is_string($variable)) {
            $type = $this->messages['TYPE_STRING'][$lang].'['.strlen($variable).']';
        } elseif (is_bool($variable)) {
            $type = $this->messages['TYPE_BOOLEAN'][$lang];
            if ($variable==true) {
                $variable = $this->messages['BOOLEAN_TRUE'][$lang];
            } else {
                $variable = $this->messages['BOOLEAN_FALSE'][$lang];
            }
        } elseif (is_numeric($variable)) {
            $type = $this->messages['TYPE_NUMERIC'][$lang];
        } elseif (is_resource($variable)) {
            $type = $this->messages['TYPE_RESOURCE'][$lang].'['.get_resource_type($variable).']';
        } elseif (is_scalar($variable)) {
            $type = $this->messages['TYPE_SCALAR'][$lang];
        } elseif (is_null($variable)) {
            $type = $this->messages['TYPE_NULL'][$lang];
            $variable = 'Null';
        } else {
            $type = $this->messages['TYPE_UNKNOWN'][$lang].'['.gettype($variable).']';
        }
        return
            array(
                VAR_DUMP_ARRAY_MAGIC => VAR_DUMP_MAGIC,
                VAR_DUMP_ARRAY_TYPE  => $type,
                VAR_DUMP_ARRAY_VALUE => $variable
            );
    }
    // }}}


    // {{{ _parseArray()
    /**
    * _parseArray() - Parse recursively an array
    *
    * This method returns recursive informations on an array :
    * structure, keys and values
    *
    * @param array  $array  An array to explore
    * @param string $except If not empty, name of the key not to
    *                       explore, to avoid parsing references to itself
    */
    function _parseArray($array, $except = '')
    {
        $lang=$this->lang;
        if (is_string($except)) {
            $except_array = explode(' ',$except);
        } else {
            $except_array = array();
        }
        
        if (!is_array($array)) {	
            return $this->_parseVariable($array);
        } else {
            if (count($array)==0) {
                return 
                    array(
                        VAR_DUMP_ARRAY_MAGIC => VAR_DUMP_MAGIC,
                        VAR_DUMP_ARRAY_TYPE  => $this->messages['EMPTY_ARRAY'][$lang],
                        VAR_DUMP_ARRAY_VALUE => ''
                    );
            } else {
                $localArray = array();
                foreach($array as $key => $value) {
                    if (! (in_array($key, $except_array, TRUE))) {
                        $localArray[$key] = $this->_parseArray($value, $except);
                    } else {
                        $localArray[$key] = array(
                            VAR_DUMP_ARRAY_MAGIC => VAR_DUMP_MAGIC,
                            VAR_DUMP_ARRAY_TYPE  => $this->messages['NOT_PARSED'][$lang],
                            VAR_DUMP_ARRAY_VALUE => ''
                        );
                    }
                }
                return $localArray;
            }
        }
    }
    // }}}


    // {{{ _parseObject()
    /**
    * _parseObject() - Returns informations on an object
    *
    * This method returns informations on an object and its class :
    * default class variables and methods, current object state
    *
    * @param object $object An object to explore
    */
    function _parseObject($object)
    {
        $lang=$this->lang;
        if (!is_object($object)) {	
            return $this->_parseVariable($object);
        } else {
            $className=get_class($object);
            $arr = array(
                $this->messages['OBJECT_CLASS_NAME'][$lang]    => $this->_parseVariable($className),
                $this->messages['OBJECT_PARENT'][$lang]        => $this->_parseVariable(get_parent_class($object)),
                $this->messages['OBJECT_CLASS_VARS'][$lang]    => $this->_parseArray(get_class_vars($className)),
                $this->messages['OBJECT_CLASS_METHODS'][$lang] => $this->_parseArray(get_class_methods($className)),                
                $this->messages['OBJECT_OBJECT_VARS'][$lang]   => $this->_parseArray(get_object_vars($object))
            );

            switch ($this->_VarDumpConfGlobal['displayOptions'] & VAR_DUMP_NO_CLASS_INFO) {            
            case VAR_DUMP_NO_CLASS_VARS:                
                unset($arr[$this->messages['OBJECT_CLASS_VARS'][$lang]]);
                break;
            case VAR_DUMP_NO_CLASS_METHODS:
                unset($arr[$this->messages['OBJECT_CLASS_METHODS'][$lang]]);
                break;
            case VAR_DUMP_NO_CLASS_INFO:
                unset($arr[$this->messages['OBJECT_CLASS_VARS'][$lang]]);
                unset($arr[$this->messages['OBJECT_CLASS_METHODS'][$lang]]);
                break;
            default:
                break;
            }
            return $arr;
        }
    }
    // }}}


    // {{{ _isSingleVariable()
    /**
    * _isSingleVariable() - Tells if a variable is a single variable
    *
    * This method tells if a variable is a single variable (long,
    * string, double...) or a more complex one (array, object...)
    *
    * @param $variable The variable to check
    * @return          True if it's a single variable
    *                  False if it's a more complex variable
    */
    function _isSingleVariable($variable)
    {
        return (
            is_array($variable) and
            isset($variable[VAR_DUMP_ARRAY_MAGIC]) and
            $variable[VAR_DUMP_ARRAY_MAGIC]==VAR_DUMP_MAGIC
        );
    }
    // }}}


    // {{{ _generateFontTags()
    /**
    * _generateFontTags() - Generates the font tags <font...>
    *
    * This method generates the font tags <font...> with the
    * font size, color and face values choosen. If none of the
    * font parameters was modified, use the default font.
    */
    function _generateFontTags()
    {
        $font = '';
        if (!empty($this->_VarDumpConfVisual['fontface'])) {
            $font .= ' face="'.$this->_VarDumpConfVisual['fontface'].'"';
        }
        if (!empty($this->_VarDumpConfVisual['fontsize'])) {
            $font .= ' size="'.$this->_VarDumpConfVisual['fontsize'].'"';
        }
        for ($i = 1 ; $i <= 2 ; $i++) {
            $fontTag = $font;
            if (!empty($this->_VarDumpConfVisual['fontcolor'.$i])) {
                $fontTag .= ' color="'.$this->_VarDumpConfVisual['fontcolor'.$i].'"';
            }
            if (!empty($fontTag)) {
                $this->{'_VarDumpDisplayFont'.$i.'Start'} = '<font'.$fontTag.'>';
                $this->{'_VarDumpDisplayFont'.$i.'End'}   = '</font>';
            } else {
                $this->{'_VarDumpDisplayFont'.$i.'Start'} = '';
                $this->{'_VarDumpDisplayFont'.$i.'End'}   = '';
            }
        }
    }
    // }}}


    // {{{ _formatTableTEXT()
    /**
    * _formatTableTEXT() - Returns informations in text format
    *
    * This method returns all the informations collected on
    * the submitted variables, in a text format
    *
    * @param array $array  The _VarDumpArray structure to display
    * @param boolean $html Tell whether the output have to be html formated or not
    */
    function _formatTableTEXT($array,$html)
    {
        $tmp   = $this->_formatTableTEXTArray($array, $html);
        $tmp   = ereg_replace("(^([\n])*)|(^(<br>)*)|(([\n])*$)|((<br>)*$)", '', $tmp);
        if ($html) {
            $table =
                '<pre>'.
                $this->_VarDumpDisplayFont1Start.
                $tmp.
                $this->_VarDumpDisplayFont1End.
                '</pre>';
        } else {
            $table = $tmp;
        }
        return $table;
    }
    // }}}


    // {{{ _formatTableTEXTArray()
    /**
    * _formatTableTEXTArray() - Returns informations in text format
    *
    * This recursive method returns all the informations collected on
    * the submitted variables, in a text format
    *
    * @param array $array      The _VarDumpArray structure to display
    * @param boolean $html     Tell whether the output have to be html formated or not
    * @param array $complete   Contains the width of each column
    * @param int $level        Level of recursion
    */
    function _formatTableTEXTArray($array, $html = false, $complete = array(), $level = 0)
    {
        $lang=$this->lang;
        $table = '';
        if ($level > 0) {
            $table .= $this->_formatTableTEXTSeparator($complete, ' ');
        }
        if (is_array($array) and !$this->_isSingleVariable($array)) {
            $largeurCle  = 0;
            $largeurType = 0;
            foreach($array AS $key=>$val) {
                if (strlen($key) > $largeurCle) {
                    $largeurCle = strlen($key);
                }
                if ($this->_isSingleVariable($val)) {
                    if (strlen($val[VAR_DUMP_ARRAY_TYPE]) > $largeurType) {
                        $largeurType = strlen($val[VAR_DUMP_ARRAY_TYPE]);
                    }
                } else {
                    $str = $this->messages['TYPE_ARRAY'][$lang].'('.count($val).')';
                    $len = strlen($str);
                    if ($len > $largeurType) {
                        $largeurType = $len;
                    }
                }
            }
            $compteur = count($array);
            $k        = 0;
            foreach($array AS $key=>$val) {
                if ($k == 0) {  	
                    $table .= $this->_formatTableTEXTSeparator($complete, '+');
                } elseif ($k == $compteur-1) {
                    $table .= $this->_formatTableTEXTSeparator($complete, '+');
                } else {
                    $table .= $this->_formatTableTEXTSeparator($complete, '|');
                }
                if ($this->_isSingleVariable($val)) {
                    $table .=
                        sprintf('%-'.$largeurCle.'s', $key).
                        ' '.
                        sprintf('%-'.$largeurType.'s', $val[VAR_DUMP_ARRAY_TYPE]).
                        ' ('.
                        $this->_VarDumpDisplayFont2Start.
                        htmlspecialchars(rtrim($val[VAR_DUMP_ARRAY_VALUE])).
                        $this->_VarDumpDisplayFont2End.
                        ')';
                } else {
                    $table .=
                        sprintf('%-'.$largeurCle.'s', $key).
                        ' '.
                        sprintf('%-'.$largeurType.'s', $this->messages['TYPE_ARRAY'][$lang].'('.count($val).')');
                    array_push($complete, $largeurCle);
                    $table .= $this->_formatTableTEXTArray($val, $html, $complete, $level + 1);
                    array_pop($complete);
                }
                $k++;
            }
        } else {
            $table .=
                $array[VAR_DUMP_ARRAY_TYPE].
                ' ('.
                $this->_VarDumpDisplayFont2Start.
                htmlspecialchars(rtrim($array[VAR_DUMP_ARRAY_VALUE])).
                $this->_VarDumpDisplayFont2End.
                ')';
        }
        if ($level > 0) {
            $table .= $this->_formatTableTEXTSeparator($complete, ' ');
        }
        return $table;
    }
    // }}}


    // {{{ _formatTableTEXTSeparator()
    /**
    * _formatTableTEXTSeparator() - Returns the beginning of a line
    *
    * @param array $complete   Contains the width of each column
    * @param char $displayLast Last character to display on the line
    */
    function _formatTableTEXTSeparator($complete, $displayLast = '')
    {
        $text  = "\n";
        $count = count($complete);
        if ($count > 0) {
            $i = 0;
            foreach($complete AS $larg) {
                $sep = $displayLast;
                if ($i < $count - 1) {
                    $sep = '|';
                }
                $text .= str_repeat(' ', $larg + 1).$sep.' ';
                $i++;
            }
        }
        return $text;
    }
    // }}}


    // {{{ _formatTableHTML()
    /**
    * _formatTableHTML() - Returns informations in HTML format
    *
    * This method returns all the informations collected on
    * the submitted variables, in a graphical format
    *
    * @param array $array The _VarDumpArray structure to display
    * @param int   $level Level of recursion
    */      
    function _formatTableHTML($array, $level = 0)
    {
        $lang=$this->lang;
        $table        = '';
        $table_header = '';
        $table_footer = '';
        
        $table_footer .= '</table>';
        if ($this->_VarDumpConfVisual['bordersize'] > 0) {
            $table_header .= '<table border=0 cellpadding='.$this->_VarDumpConfVisual['bordersize'].' cellspacing=0 bgcolor='.$this->_VarDumpConfVisual['bordercolor'].'><tr><td>';
            $table_footer .= '</td></tr></table>';
        }
        $table_header .= '<table border=0 cellpadding='.$this->_VarDumpConfVisual['cellpadding'].' cellspacing='.$this->_VarDumpConfVisual['cellspacing'].'>';
        
        if (is_array($array)) {
            if ($this->_isSingleVariable($array)) {

                $table .= $this->_VarDumpDisplayFont1Start;
                $table .= $array[VAR_DUMP_ARRAY_TYPE];
                if (
                    $array[VAR_DUMP_ARRAY_TYPE] != $this->messages['EMPTY_ARRAY'][$lang] and
                    $array[VAR_DUMP_ARRAY_TYPE] != $this->messages['NOT_PARSED'][$lang]
                ) {
                    if (!empty($array[VAR_DUMP_ARRAY_VALUE])) {
                        $table .=
                            ' ('.
                            $this->_VarDumpDisplayFont2Start.
                            htmlspecialchars((string) $array[VAR_DUMP_ARRAY_VALUE]).
                            $this->_VarDumpDisplayFont2End.')';
                    } elseif (
                        (
                            is_integer($array[VAR_DUMP_ARRAY_VALUE]) and
                            $array[VAR_DUMP_ARRAY_VALUE] == 0
                        ) or (
                            is_string($array[VAR_DUMP_ARRAY_VALUE]) and
                            $array[VAR_DUMP_ARRAY_VALUE] == '0'
                        )
                    ) {
                        $table .=
                            ' ('.
                            $this->_VarDumpDisplayFont2Start.
                            '0'.
                            $this->_VarDumpDisplayFont2End.')';
                    } else {
                        $table .= ' ()';
                    }
                }
                $table .= $this->_VarDumpDisplayFont1End;

            } else {
                if ($level==0 and count($array)==1 and !$this->_isSingleVariable($array[0]) and is_array($array[0])) {
                    $display_border = false;
                } else {
                    $display_border = true;
                }
                if ($display_border) {
                    $table .= $table_header;
                }
                $c = 0;
                foreach($array as $key => $value) {
                    if (is_array($value)) {
                        if ($this->_isSingleVariable($value)) {
                            if ($value[VAR_DUMP_ARRAY_TYPE]==$this->messages['NOT_PARSED'][$lang]) {
                                continue;
                            } else {
                                $countValue = '';
                            }
                        } else {
                            $countValue = '&nbsp;('.count($value).')';
                        }
                    } else {
                        $countValue = '';
                    }
                    $bg='';
                    if ($c==0) {
                        if (!empty($this->_VarDumpConfVisual['color1'])) {
                            $bg = ' bgcolor='.$this->_VarDumpConfVisual['color1'];
                        }
                    } else {
                        if (!empty($this->_VarDumpConfVisual['color2'])) {
                            $bg = ' bgcolor='.$this->_VarDumpConfVisual['color2'];
                        }
                    }
                    if ($display_border) {
                        $table .= '<tr'.$bg.' valign=top>';
                        if ($level != 0 or count($array) != 1) {
                            $table .= '<td>'.$this->_VarDumpDisplayFont1Start.$key.$countValue.$this->_VarDumpDisplayFont1End.'</td>';
                        }
                        $table .= '<td>'.$this->_formatTableHTML($value, $level+1).'</td>';
                        $table .= '</tr>';
                    } else {
                        $table .= $this->_formatTableHTML($value, $level+1);
                    }
                    $c = 1-$c;
                }
                if ($display_border) {
                    $table .= $table_footer;
                }
            }
        } else {		
            $table .= $array;
        }
        return $table;
    }
    // }}}


    // {{{ setDisplayMode()
    /**
    * setDisplayMode() - Set visual parameters
    *
    * This method set visual parameters for the rendering : border color
    * and size, cells colors, padding and spacing of the table, font face,
    * size and color.
    *
    * @param array $conf Display configuration
    */
    function setDisplayMode($conf = array())
    {
        $conf2=array_merge(
            array(
                'bordercolor' => '#444444',
                'color1'      => '#dddddd',
                'color2'      => '#eeeeee',
                'bordersize'  => '1',
                'cellpadding' => '4',
                'cellspacing' => '0',
                'fontface'    => '',
                'fontsize'    => '',
                'fontcolor1'  => '',
                'fontcolor2'  => 'red'
            ),
            $conf
        );
        $this->_VarDumpConfVisual = $conf2;
    }
    // }}}
    

    // {{{ setSkin()
    /**
    * setSkin() - Set visual parameters using a skin model
    *
    * This method set visual parameters using a skin model
    *
    * @param int $mode    Default display mode : Text or Html
    * @param string $skin Name of the skin
    *                     for the moment : default, green, red or blue
    */
    function setSkin($mode = VAR_DUMP_DISPLAY_MODE_HTML_TEXT, $skin = 'default')
    {
        switch ($mode) {

            case VAR_DUMP_DISPLAY_MODE_TEXT:
                $this->setDisplayMode(
                    array(
                        'fontface'    => '',
                        'fontsize'    => '',
                        'fontcolor1'  => '',
                        'fontcolor2'  => ''
                    )
                );
                break;

            case VAR_DUMP_DISPLAY_MODE_HTML_TEXT:

                switch($skin) {
                    case 'green':
                        $this->setDisplayMode(array('fontcolor2' => '#55CC5F'));
                        break;
                    case 'red':
                        $this->setDisplayMode(array('fontcolor2' => '#CC555F'));
                        break;
                    case 'blue':
                        $this->setDisplayMode(array('fontcolor2' => '#5F55CC'));
                        break;
                    case 'default':
                    default:
                        $this->setDisplayMode();
                        break;
                }
                break;

            case VAR_DUMP_DISPLAY_MODE_HTML_TABLE:
            default:

                switch($skin) {
                    case 'green':
                        $this->setDisplayMode(
                            array(
                                'bordercolor' => '#2BB036',
                                'color1'      => '#55CC5F',
                                'color2'      => '#7FE888',
                                'bordersize'  => '1',
                                'cellpadding' => '4',
                                'cellspacing' => '0',
                                'fontface'    => '',
                                'fontsize'    => '',
                                'fontcolor1'  => '#FFFFFF',
                                'fontcolor2'  => '#FF00CC'
                            )
                        );
                        break;
                    case 'red':
                        $this->setDisplayMode(
                            array(
                                'bordercolor' => '#B02B36',
                                'color1'      => '#CC555F',
                                'color2'      => '#E87F88',
                                'bordersize'  => '1',
                                'cellpadding' => '4',
                                'cellspacing' => '0',
                                'fontface'    => '',
                                'fontsize'    => '',
                                'fontcolor1'  => '#FFFFFF',
                                'fontcolor2'  => '#FFCC00'
                            )
                        );
                        break;
                    case 'blue':
                        $this->setDisplayMode(
                            array(
                                'bordercolor' => '#362BB0',
                                'color1'      => '#5F55CC',
                                'color2'      => '#887FE8',
                                'bordersize'  => '1',
                                'cellpadding' => '4',
                                'cellspacing' => '0',
                                'fontface'    => '',
                                'fontsize'    => '',
                                'fontcolor1'  => '#FFFFFF',
                                'fontcolor2'  => '#CCFF00'
                            )
                        );
                        break;
                    case 'default':
                    default:
                        $this->setDisplayMode();
                        break;
                }
                break;
        }

    }
    // }}}

}

?>