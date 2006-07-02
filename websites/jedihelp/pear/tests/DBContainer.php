<?php

include_once('TestAuthContainer.php');
include_once("Auth/Container/DB.php");


class DBContainer extends TestAuthContainer {

    function DBContainer($name){
        $this->TestAuthContainer($name);
    }
    
    function &getContainer() {
        static $container;
        #print "In DBContainer::getContainer {$this->skip_tests}\n";
        if(!isset($container)){
            include('./auth_container_db_options.php');
            $container = new Auth_Container_DB($options);
            // Catch if DB connection cannot be made
            $res = $container->_prepare();
        }
        
        if(!DB::isConnection($container->db)){
            #print "In DBContainer::getContainer container->db is error \n";
            $this->skip_tests = true;
            $this->skip_tests_message = "SKIP TEST:DB is not a connection object, check dsn !!!";
        }
        return($container);
    }
    
    function &getExtraOptions() {
        include('./auth_container_db_options.php');
        return($extra_options);
    }
}




?>