#define FAULT_ERROR(string) \
    Exn.error(string); return Factory.Impossible;

#define iftype(var, type, err) \
    if (!(var is type)) { \
        FAULT_ERROR(err); \
    } else
