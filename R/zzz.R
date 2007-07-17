.First.lib <- function(libname, pkgname) {
    	if(interactive() && .Platform$OS.type == "windows" && .Platform$GUI ==  "Rgui")
        	winMenuAddItem("Vignettes","logicFS","vignette('logicFS')")

}


