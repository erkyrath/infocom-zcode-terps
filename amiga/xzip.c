
#include "exec/types.h"
#include "exec/exec.h"
#include "exec/libraries.h"		/* for Library.lib_Version */
#include "libraries/dos.h"
#include "libraries/dosextens.h"

#include "intuition/intuition.h"	/* for AllocRemember */
#include "intuition/intuitionbase.h"

#include "workbench/icon.h"
#include "workbench/workbench.h"	/* for disk objects */

#include "graphics/text.h"		/* SetSoftStyle flags */
#include "graphics/rastport.h"		/* SetDrMode flags */
#include "graphics/gfxbase.h"		/* for screen size vars */

#include "hardware/intbits.h"		/* for interrupt flag */

#include "xzip1.c"
#include "xzip2.c"
#include "xzip3.c"
