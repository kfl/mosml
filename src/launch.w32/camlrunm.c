__declspec(dllimport) int caml_main(int argc, char * argv[]);

int main(int argc, char * argv[])
{
  return caml_main(argc, argv);
}