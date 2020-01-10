void *mempcpy(void *dest, const void *src, size_t n)
{
    return (void *) ((char*)memcpy(dest, src, n) + n);
}

char *
concat (const char *str, ...)
{
  va_list ap;
  size_t allocated = 100;
  char *result = (char *) malloc (allocated);

  if (result != NULL)
    {
      char *newp;

      va_start (ap, str);

      char *wp = result;
      const char *s;
      for (s = str; s != NULL; s = va_arg (ap, const char *))
        {
          size_t len = strlen (s);

          /* Resize the allocated memory if necessary.  */
          if (wp + len + 1 > result + allocated)
            {
              allocated = (allocated + len) * 2;
              newp = (char *) realloc (result, allocated);
              if (newp == NULL)
                {
                  free (result);
                  return NULL;
                }
              wp = newp + (wp - result);
              result = newp;
            }

          wp = mempcpy (wp, s, len);
        }

      /* Terminate the result string.  */
      *wp++ = '\0';

      /* Resize memory to the optimal size.  */
      newp = realloc (result, wp - result);
      if (newp != NULL)
        result = newp;

      va_end (ap);
    }

  return result;
}

int
main(int argc, char *argv[])
{
#ifdef WIN32
# define EXE_EXT ".exe"
#else
# define EXE_EXT ""
#endif
    const char file[] = "R" EXE_EXT;
        
    char * const extra_argv[] = { "--vanilla", "--slave",
                          "-e", R_CODE, "--args"};
    int extra_argc = sizeof(extra_argv) / sizeof(char *);
    char **cmd_args = calloc(extra_argc + argc + 1, sizeof(char *));
    cmd_args[0] = argv[0];
    for (int i = 0; i < extra_argc; i++)
        cmd_args[i+1] = extra_argv[i];
    for (int i = 1; i < argc; i++)
        cmd_args[extra_argc + i] = argv[i];
    
    return execvp(file, cmd_args);
}
