# Linked registers

A thing for drawing a network graph of linked registers.

1. Point to the [`registry-data`](https://github.com/openregister/registry-data)
   repo, `data` directory.
2. Start with the `beta` directory, then add any other registers in `alpha`,
   then `discovery`, reading the `register/[name-of-register].yaml` file in each
   one.
3. Ditto fields, choosing ones where the register is the same as the field
4. Registers link to one another via those common fields.
5. Think of something for CURIEs
