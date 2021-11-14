# Test Files

### Synthetic Data

These were created by hand, to match the schema of realistic files, but using entirely made up data.

Note that the files are not representative of real data beyond the schema.
For example, the branches data might references buses which are not present in the buses data, which should never happen in realistic files.

1. `_v30.raw`: v30 format data.
2. `_v29.raw`: v29 format data.
3. `_v33.raw`: v33 format data.

### Issues

These files were created to demonstrate issues that the parser should now be able to handle.

1. `spacezero.raw`: v30 format data with a space before the `0` bus number that indicates the end of the records.

---
_This is a work of fiction. Any resemblance to actual datas, living or dead, is purely coincidental._
