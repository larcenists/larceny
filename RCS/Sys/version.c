#ifdef __STDC__
  #ifndef USER
    #define USER "The Wizard of Oz"
  #endif
  char *version = "Larceny version 0.01, built by " USER " on " __DATE__;
#else
  char *version = "Larceny version 0.01";
#endif

