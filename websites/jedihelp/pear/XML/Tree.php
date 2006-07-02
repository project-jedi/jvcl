<?php
//
// +----------------------------------------------------------------------+
// | PHP Version 4                                                        |
// +----------------------------------------------------------------------+
// | Copyright (c) 1997-2002 The PHP Group                                |
// +----------------------------------------------------------------------+
// | This source file is subject to version 2.02 of the PHP license,      |
// | that is bundled with this package in the file LICENSE, and is        |
// | available at through the world-wide-web at                           |
// | http://www.php.net/license/2_02.txt.                                 |
// | If you did not receive a copy of the PHP license and are unable to   |
// | obtain it through the world-wide-web, please send a note to          |
// | license@php.net so we can mail you a copy immediately.               |
// +----------------------------------------------------------------------+
// | Authors: Bernd Römer <berndr@bonn.edu>                               |
// |          Sebastian Bergmann <sb@sebastian-bergmann.de>               |
// |          Tomas V.V.Cox <cox@idecnet.com> (tree mapping from xml file)|
// +----------------------------------------------------------------------+
//
// $Id: Tree.php,v 1.19 2002/05/17 12:15:23 cox Exp $
//

require_once 'XML/Parser.php';
require_once 'XML/Tree/Node.php';

/**
* PEAR::XML_Tree
*
* Purpose
*
*    Allows for the building of XML data structures
*    using a tree representation, without the need
*    for an extension like DOMXML.
*
* Example
*
*    $tree  = new XML_Tree;
*    $root =& $tree->addRoot('root');
*    $foo  =& $root->addChild('foo');
*
*    header('Content-Type: text/xml');
*    $tree->dump();
*
* @author  Bernd Römer <berndr@bonn.edu>
* @package XML
* @version $Version$ - 1.0
*/
class XML_Tree extends XML_Parser
{
    /**
    * File Handle
    *
    * @var  ressource
    */
    var $file = NULL;

    /**
    * Filename
    *
    * @var  string
    */
    var $filename = '';

    /**
    * Namespace
    *
    * @var  array
    */
    var $namespace = array();

    /**
    * Root
    *
    * @var  object XML_Tree_Node
    */
    var $root = NULL;

    /**
    * XML Version
    *
    * @var  string
    */
    var $version = '1.0';

    /**
    * Constructor
    *
    * @param  string  Filename
    * @param  string  XML Version
    */
    function XML_Tree($filename = '', $version = '1.0') {
        $this->filename = $filename;
        $this->version  = $version;
    }

    /**
    * Add root node.
    *
    * @param  string  $name     name of root element
    * @return object XML_Tree_Node   reference to root node
    *
    * @access public
    */
    function &addRoot($name, $content = '', $attributes = array()) {
        $this->root = new XML_Tree_Node($name, $content, $attributes);
        return $this->root;
    }

    /**
    * @deprecated
    */
    function &add_root($name, $content = '', $attributes = array()) {
        return $this->addRoot($name, $content, $attributes);
    }

    /**
    * inserts a child/tree (child) into tree ($path,$pos) and
    * maintains namespace integrity
    *
    * @param array      $path           path to parent of child to remove
    * @param integer    $pos            position of child to be inserted in its parents children-list
    * @param mixed      $child          child-node (by XML_Tree,XML_Node or Name)
    * @param string     $content        content (text) for new node
    * @param array      $attributes     attribute-hash for new node
    *
    * @return object XML_Tree_Node inserted child (node)
    * @access public
    */
    function &insertChild($path,$pos,$child, $content = '', $attributes = array()) {
        // update namespace to maintain namespace integrity
        $count=count($path);
        foreach($this->namespace as $key => $val) {
            if ((array_slice($val,0,$count)==$path) && ($val[$count]>=$pos))
                $this->namespace[$key][$count]++;
        }

        $parent=&$this->get_node_by_path($path);
        return($parent->insert_child($pos,$child,$content,$attributes));
    }

    /**
    * @deprecated
    */
    function &insert_child($path,$pos,$child, $content = '', $attributes = array()) {
        return $this->insertChild($path, $child, $content, $attributes);
    }

    /*
    * removes a child ($path,$pos) from tree ($path,$pos) and
    * maintains namespace integrity
    *
    * @param array      $path   path to parent of child to remove
    * @param integer    $pos    position of child in parents children-list
    *
    * @return object XML_Tree_Node parent whichs child was removed
    * @access public
    */
    function &removeChild($path,$pos) {
        // update namespace to maintain namespace integrity
        $count=count($path);
        foreach($this->namespace as $key => $val) {
            if (array_slice($val,0,$count)==$path) {
                if ($val[$count]==$pos) { unset($this->namespace[$key]); break; }
                if ($val[$count]>$pos)
                    $this->namespace[$key][$count]--;
            }
        }

        $parent=&$this->get_node_by_path($path);
        return($parent->remove_child($pos));
    }

    /**
    * @deprecated
    */
    function &remove_child($path, $pos) {
        return $this->removeChild($path, $pos);
    }

    /*
    * Maps a xml file to a objects tree
    *
    * @return mixed The objects tree (XML_tree or an Pear error)
    * @access public
    */
    function &getTreeFromFile ()
    {
        $this->folding = false;
        $this->XML_Parser(null, 'event');
        $err = $this->setInputFile($this->filename);
        if (PEAR::isError($err)) {
            return $err;
        }
        $this->cdata = null;
        $err = $this->parse();
        if (PEAR::isError($err)) {
            return $err;
        }
        return $this->root;
    }

    function getTreeFromString($str)
    {
        $this->folding = false;
        $this->XML_Parser(null, 'event');
        $this->cdata = null;
        $err = $this->parseString($str);
        if (PEAR::isError($err)) {
            return $err;
        }
        return $this->root;
    }

    /**
    * Handler for the xml-data
    *
    * @param mixed  $xp         ignored
    * @param string $elem       name of the element
    * @param array  $attribs    attributes for the generated node
    *
    * @access private
    */
    function startHandler($xp, $elem, &$attribs)
    {
        // root elem
        if (!isset($this->i)) {
            $this->obj1 =& $this->add_root($elem, null, $attribs);
            $this->i = 2;
        } else {
            // mixed contents
            if (!empty($this->cdata)) {
                $parent_id = 'obj' . ($this->i - 1);
                $parent    =& $this->$parent_id;
                $parent->children[] = &new XML_Tree_Node(null, $this->cdata);
            }
            $obj_id = 'obj' . $this->i++;
            $this->$obj_id = &new XML_Tree_Node($elem, null, $attribs);
        }
        $this->cdata = null;
        return null;
    }

    /**
    * Handler for the xml-data
    *
    * @param mixed  $xp         ignored
    * @param string $elem       name of the element
    *
    * @access private
    */
    function endHandler($xp, $elem)
    {
        $this->i--;
        if ($this->i > 1) {
            $obj_id = 'obj' . $this->i;
            // recover the node created in StartHandler
            $node   =& $this->$obj_id;
            // mixed contents
            if (count($node->children) > 0) {
                if (trim($this->cdata)) {
                    $node->children[] = &new XML_Tree_Node(null, $this->cdata);
                }
            } else {
                $node->set_content($this->cdata);
            }
            $parent_id = 'obj' . ($this->i - 1);
            $parent    =& $this->$parent_id;
            // attach the node to its parent node children array
            $parent->children[] = $node;
        }
        $this->cdata = null;
        return null;
    }

    /*
    * The xml character data handler
    *
    * @param mixed  $xp         ignored
    * @param string $data       PCDATA between tags
    *
    * @access private
    */
    function cdataHandler($xp, $data)
    {
        if (trim($data)) {
            $this->cdata .= $data;
        }
    }

    /**
    * Get a copy of this tree.
    *
    * @return object XML_Tree
    * @access public
    */
    function clone() {
        $clone=new XML_Tree($this->filename,$this->version);
        $clone->root=$this->root->clone();

        // clone all other vars
        $temp=get_object_vars($this);
        foreach($temp as $varname => $value)
            if (!in_array($varname,array('filename','version','root')))
                $clone->$varname=$value;

        return($clone);
    }

    /**
    * Print text representation of XML tree.
    *
    * @access public
    */
    function dump() {
        echo $this->get();
    }

    /**
    * Get text representation of XML tree.
    *
    * @return  string  XML
    * @access public
    */
    function &get() {
        $out = '<?xml version="' . $this->version . "\"?>\n";
        $out .= $this->root->get();

        return $out;
    }

    /**
    * Get current namespace.
    *
    * @param  string  $name namespace
    * @return string
    *
    * @access public
    */
    function &getName($name) {
        return $this->root->get_element($this->namespace[$name]);
    }

    /**
    * @deprecated
    */
    function &get_name($name) {
        return $this->getName($name);
    }

    /**
    * Register a namespace.
    *
    * @param  string  $name namespace
    * @param  string  $path path
    *
    * @access public
    */
    function registerName($name, $path) {
        $this->namespace[$name] = $path;
    }

    /**
    * @deprecated
    */
    function register_name($name, $path) {
        return $this->registerName($name, $path);
    }
}
?>
