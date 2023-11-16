
	TTL	COMMODORE AMIGA Z-LANGUAGE INTERPRETER

* ---------------------------------------------------------------------------
* ASSEMBLY FLAGS
* ---------------------------------------------------------------------------

EZIP	EQU	0	* ASSEMBLES EZIP CODE WHEN CLEAR
CZIP	EQU	-1	* ASSEMBLES CLASSIC ZIP CODE WHEN CLEAR
DEBUG	EQU	-1	* ASSEMBLES DEBUGGING CODE WHEN CLEAR

    INCLUDE	"ZIP1.ASM"	* 
    INCLUDE	"ZIP2.ASM"	* "OPADD ..."
    INCLUDE	"ZIP3.ASM"	* "ZIP MAIN ..."
    INCLUDE	"ZIP4.ASM"	* "MACHINE-DEP ..."

	END
