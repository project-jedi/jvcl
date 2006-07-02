<?php

include_once('TestAuthContainer.php');
include_once("Auth/Container/POP3.php");


class POP3Container extends TestAuthContainer {

    function POP3Container($name){
        $this->TestAuthContainer($name);
    }
    
    function &getContainer() {
        static $container;
        if(!isset($container)){
            include('./auth_container_pop3_options.php');
            $container = new Auth_Container_POP3($options);
        }
        return($container);
    }
    
    function &getExtraOptions() {
        include('./auth_container_pop3_options.php');
        return($extra_options);
    }
}




?>