# Tell data.table that this package uses data.table syntax. Without this,
# `dt[i]` inside the package would silently fall back to `[.data.frame`
# semantics and break row-indexing. See ?datatable-importing.
.datatable.aware <- TRUE
