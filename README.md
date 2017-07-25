# Linked registers

Draw a network graph of linked registers, using the API.

1. Download all phases of the register register.  If a register appears in more
   than one phase, keep only the superior one.
2. Download all fields in all phases and match them to the registers.
3. If a register uses a field that is the key of another register, then that is
   a link.
4. For registers with CURIE fields, download the whole register and examine the
   CURIEs to find out which registers they link to.

# Adoptions

Draw a network graph of adopted registers and their adoptors.  Registers are
from the API, but adoptions are hard-coded.

1. Do the steps of 'linked registers'.
2. Keep only the beta registers.
3. Read the adoptors from `data/adoption.tsv`, and forge the links.
