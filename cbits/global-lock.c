// Based on the global-lock package.

// Atomic builtins were added in GCC 4.1.
#if  !defined(__GNUC__) \
  || (__GNUC__ < 4) \
  || (__GNUC__ == 4 && __GNUC_MINOR__ < 1)
#error global-lock requires GCC 4.1 or later.
#endif

static void* global = 0;

void* hs_propane_get_global(void) {
    return global;
}

int hs_propane_set_global(void* new_global) {
    void* old = __sync_val_compare_and_swap(&global, 0, new_global);
    return (old == 0);
}
