char * runtime_name = "BINDIR/camlrunm";
char * errmsg = "Cannot exec camlrunm.\n";

int main(argc, argv)
     int argc;
     char ** argv;
{
  execvp(runtime_name, argv);
  write(2, errmsg, sizeof(errmsg)-1);
  return 2;
}
