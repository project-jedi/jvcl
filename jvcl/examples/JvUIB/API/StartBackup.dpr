program StartBackup;

{$APPTYPE CONSOLE}

uses
  SysUtils, JvUIBase, JvUIBLib;

const
  user = 'SYSDBA';
  pass = 'masterkey';

var
  svc_handle: IscSvcHandle = nil;
  respbuf: string;
  spb, thd: string;
  Len: Word;

begin
  if (ParamCount <> 2) then
  begin
    writeln(ParamCount);
    writeln(format('Usage: %s dbfile backupfile', [ParamStr(0)]));
    readln;
    halt(1);
  end;

  spb := spb + isc_spb_version;
  spb := spb + isc_spb_current_version;

  spb := spb + isc_spb_user_name;
  spb := spb + char(Length(user));
  spb := spb + user;

  spb := spb + isc_spb_password;
  spb := spb + char(strlen (pass));
  spb := spb + pass;

  ServiceAttach('service_mgr', svc_handle, spb);
  try
    thd := thd + isc_action_svc_backup;

    thd := thd + isc_spb_dbname;
    thd := thd + Char(Length(ParamStr(1)));
    thd := thd + Char(Length(ParamStr(1))shr 8); // second part of smallint
    thd := thd + ParamStr(1);

    thd := thd + isc_spb_bkp_file;
    thd := thd + Char(Length(ParamStr(2)));
    thd := thd + Char(Length(ParamStr(2))shr 8);
    thd := thd + ParamStr(2);

    thd := thd + isc_spb_verbose;

    Writeln('Attach succeed');
    ServiceStart(svc_handle, thd);
    SetLength(respbuf, 1024);

    while true do
    begin
      ServiceQuery(svc_handle, '', isc_info_svc_line, respbuf);
      if (respbuf[1] <> isc_info_svc_line) then
      begin
        WriteLn('Invalid line.');
        Exit;
      end;
      Len := PWord(@respbuf[2])^;
      if len > 0 then
        Writeln(copy(respbuf, 4, len)) else
        Break;
    end;
  finally
    ServiceDetach(svc_handle);
    writeln('Press enter to continue.');
    Readln;
  end;

end.
