{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W+,X+,Y+,Z1}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

program linkmapfile;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  JclDebug;

var
  Logo: Boolean;
  ExecutableFilename, MapFilename: string;
  JdbgFile: Boolean;
  Quiet: Boolean;

procedure ShowLogo;
begin
  if not Logo then
  begin
    Logo := True;
    if not Quiet then
    begin
      WriteLn('Link MAP file - command line tool');
      WriteLn;
    end;
  end;
end;

var
  LinkerBugUnit: string;
  MapFileSize, JclDebugDataSize: Integer;
  S: string;
  I: Integer;
begin
  try
    JdbgFile := False;
    Logo := False;
    for I := 1 to ParamCount do
    begin
      S := ParamStr(I);
      if S[1] = '-' then
      begin
        if S = '-j' then
          JdbgFile := True
        else
        if S = '-q' then
          Quiet := True;
      end
      else
      begin
        if ExecutableFilename = '' then
          ExecutableFilename := S
        else
        if MapFilename = '' then
          MapFilename := S
        else
        begin
          ShowLogo;
          WriteLn(ErrOutput, 'Unknown option: ' + S);
        end;
      end
    end;

    ShowLogo;
    if ExecutableFilename <> '' then
    begin
      if MapFilename = '' then
        MapFilename := ChangeFileExt(ExecutableFilename, '.map');

      if FileExists(ExecutableFilename) and FileExists(MapFilename) then
      begin
        if JdbgFile then
        begin
          ExitCode := Ord(not ConvertMapFileToJdbgFile(MapFilename));
          if not Quiet then
          begin
            if ExitCode = 0 then
              WriteLn('JDBG file created.')
            else
              WriteLn(ErrOutput, 'Failed to create JDBG file.');
          end;
        end
        else
        begin
          ExitCode := Ord(
            not InsertDebugDataIntoExecutableFile(ExecutableFilename, MapFilename,
                                              LinkerBugUnit, MapFileSize, JclDebugDataSize)
          );
          if not Quiet then
          begin
            if (ExitCode = 0) and (MapFileSize > 0) then
              WriteLn('MAP data inserted into ', ExtractFileName(ExecutableFilename), ' (', JclDebugDataSize * 100 div MapFileSize, '%)')
            else
              WriteLn(ErrOutput, 'Failed to insert MAP data into ', ExtractFileName(ExecutableFilename));
          end;
        end;
        Exit;
      end
      else
      begin
        WriteLn(ErrOutput, 'Executable/MAP file does not exist.');
        WriteLn;
      end;
    end;

    WriteLn('Usage:');
    WriteLn('  ', ChangeFileExt(ExtractFileName(ParamStr(0)), ''), ' [options] filename.bpl [filename.map]');
    WriteLn;
    WriteLn('Options:');
    WriteLn('  -j    create .jdbg file instead of linking the MAP data into the executable');
    WriteLn('  -q    quiet mode');
    WriteLn;
    ExitCode := 1;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, E.ClassName, ': ', E.Message);
      ExitCode := 2;
    end;
  end;
end.

