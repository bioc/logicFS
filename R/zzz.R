.onAttach <- function(libname, pkgname) {
    	if(.Platform$OS.type == "windows" && .Platform$GUI ==  "Rgui")
        	winMenuAddItem("Vignettes","logicFS","vignette('logicFS')")

}


