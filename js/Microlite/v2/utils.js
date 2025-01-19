/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*           Global variables and tools                               */
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
const VERBOSE = true;

const myconsole = {
    log : function(str) {
        if (VERBOSE)
            console.log(str);
    }
}
