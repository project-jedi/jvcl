unit PrcUtils;

interface
uses Windows, Messages, Classes, SysUtils;

procedure GetProcessList( SList: TStrings );
procedure KillProcesByName( Name: string );

const

  PROCESS_TERMINATE         =  $0001;
  PROCESS_CREATE_THREAD     =  $0002;
  PROCESS_VM_OPERATION      =  $0008;
  PROCESS_VM_READ           =  $0010;
  PROCESS_VM_WRITE          =  $0020;
  PROCESS_DUP_HANDLE        =  $0040;
  PROCESS_CREATE_PROCESS    =  $0080;
  PROCESS_SET_QUOTA         =  $0100;
  PROCESS_SET_INFORMATION   =  $0200;
  PROCESS_QUERY_INFORMATION =  $0400;
  PROCESS_ALL_ACCESS        =
   STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $0FFF;

implementation
uses tlhelp32;

procedure GetProcessList( SList: TStrings );
var
 hProc, hSnapshot: THandle;
 pe32: TProcessEntry32;
begin
  SList.Clear;
  hSnapshot := CreateToolhelp32Snapshot( TH32CS_SNAPPROCESS, 0 );
  pe32.dwSize := SizeOf(TProcessEntry32);

  if Process32First(hSnapshot, pe32) then
    repeat
       SList.Add(Format('%x, %x: %s', [pe32.th32ProcessID, pe32.th32ParentProcessID, pe32.szExeFile]));

    until not Process32Next( hSnapshot, pe32 );

  CloseHandle(hSnapshot);
end;
//________
procedure KillProcesByName( Name: string );
var
 hProc, hSnapshot: THandle;
 pe32: TProcessEntry32;
begin
  hSnapshot := CreateToolhelp32Snapshot( TH32CS_SNAPPROCESS, 0 );
  pe32.dwSize := SizeOf(TProcessEntry32);

  if Process32First( hSnapshot, pe32 ) then
    repeat
       if CompareText( ExtractFileName(pe32.szExeFile), Name ) = 0 then
       begin
         hProc := OpenProcess( PROCESS_TERMINATE, false, pe32.th32ProcessID );
         TerminateProcess( hProc, 0 );
       end;
    until not Process32Next( hSnapshot, pe32 );

  CloseHandle(hSnapshot);
end;
//_______

end.
