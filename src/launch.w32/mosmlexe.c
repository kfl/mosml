#include <windows.h>

__declspec(dllimport) int caml_main(int argc, char * argv[]);

int main(int argc, char* argv[])
{
  HMODULE hModule;
  char szExePath[MAX_PATH];

  hModule = GetModuleHandle(NULL);
  GetModuleFileName(hModule, szExePath, sizeof(szExePath));

  argv[0] = szExePath;
  return caml_main(argc, argv);
}
