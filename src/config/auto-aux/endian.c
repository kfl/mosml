main()
{
  long n[2];
  char * p;

  n[0] = 0x41424344;
  n[1] = 0;
  p = (char *) n;
  if (strcmp(p, "ABCD") == 0)
    exit(0);
  if (strcmp(p, "DCBA") == 0)
    exit(1);
  exit(2);
}
